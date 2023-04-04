using System;
using System.Text;
using System.Diagnostics;
using System.IO.Pipes;
using System.IO;
using System.Threading.Tasks;
using System.Threading;
using System.Net;
using System.Net.Sockets;

namespace ElixirKit;

public delegate void ReadyHandler();

public delegate void ExitHandler(int ExitCode);

public delegate void EventHandler(string Name, string Data);

public static class API
{
    private static Release? release;
    private static Mutex? mutex;
    private static string? id;
    private static bool mainInstance = true;

    // On Windows we need to manually handle the app being launched multiple times.
    // It can be opened directly or via its associated file types and URL schemes.
    // This function checks if the current app is the "main instance".
    public static bool IsMainInstance(String id)
    {
        if (mutex != null)
        {
            throw new Exception("IsMainInstance can only be called once");
        }

        API.id = id;
        mutex = new Mutex(true, id, out mainInstance);
        return mainInstance;
    }

    public static bool HasExited {
        get {
            ensureMainInstance();
            return release!.HasExited;
        }
    }

    public static void Start(string name, ReadyHandler ready, ExitHandler? exited = null, string? logPath = null)
    {
        ensureMainInstance();

        release = new Release(name, ready, exited, logPath);

        if (mutex != null)
        {
            var t = new Task(() => {
                while (true) {
                    var line = PipeReadLine();

                    if (line != null)
                    {
                        release!.Send(line);
                    }
                }
            });

            t.Start();
        }
    }

    public static int Stop()
    {
        ensureMainInstance();
        return release!.Stop();
    }

    public static int WaitForExit()
    {
        ensureMainInstance();
        return release!.WaitForExit();
    }

    public static void Publish(string name, string data)
    {
        if (mainInstance)
        {
            release!.Send($"{name}:{data}");
        }
        else
        {
            PipeWriteLine($"{name}:{data}");
        }
    }

    public static void Subscribe(EventHandler handler)
    {
        ensureMainInstance();
        release!.Subscribe(handler);
    }

    static void ensureMainInstance()
    {
        if (!mainInstance)
        {
            throw new Exception("Not on main instance");
        }
    }

    private static string? PipeReadLine()
    {
        using var pipe = new NamedPipeServerStream(id!);
        pipe.WaitForConnection();
        using var reader = new StreamReader(pipe);
        var line = reader.ReadLine()!;
        pipe.Disconnect();
        return line;
    }

    private static void PipeWriteLine(string line)
    {
        using var pipe = new NamedPipeClientStream(id!);
        pipe.Connect();
        using var writer = new StreamWriter(pipe);
        writer.WriteLine(line);
    }
}

internal class Release
{
    Process process;
    Logger logger;
    Listener listener;
    Client client;

    internal bool HasExited {
        get {
            return process.HasExited;
        }
    }

    public Release(string name, ReadyHandler ready, ExitHandler? exited = null, string? logPath = null)
    {
        logger = new Logger(logPath);
        listener = new Listener();

        process = new Process()
        {
            StartInfo = new ProcessStartInfo()
            {
                FileName = relScript(name),
                UseShellExecute = false,
                CreateNoWindow = true,
                RedirectStandardInput = true,
                RedirectStandardOutput = true,
                RedirectStandardError = true,
            }
        };

        if (logPath != null)
        {
            logger.Capture(process);
        }

        process.StartInfo.Arguments = "start";
        process.StartInfo.EnvironmentVariables.Add("ELIXIRKIT_PORT", $"{listener.Port}");

        if (exited != null)
        {
            process.EnableRaisingEvents = true;
            process.Exited += (sender, args) =>
            {
                exited(process.ExitCode);
            };
        }

        process.OutputDataReceived += (sender, e) =>
        {
            if (!String.IsNullOrEmpty(e.Data)) { Console.WriteLine(e.Data); }
        };

        process.ErrorDataReceived += (sender, e) =>
        {
            if (!String.IsNullOrEmpty(e.Data)) { Console.Error.WriteLine(e.Data); }
        };

        logger.Capture(process);

        process.Start();
        process.BeginOutputReadLine();
        process.BeginErrorReadLine();

        Task.Run(() => {
            var tcpClient = listener.TcpListener.AcceptTcpClient();
            client = new Client(tcpClient);
            ready();
        });
    }

    public void Send(string message)
    {
        client.Send(message);
    }

    public void Subscribe(EventHandler handler)
    {
        client.Subscribe(handler);
    }

    public int Stop()
    {
        if (HasExited)
        {
            return process!.ExitCode;
        }

        client.TcpClient.Close();
        listener.TcpListener.Stop();
        return WaitForExit();
    }

    public int WaitForExit()
    {
        process!.WaitForExit();
        return process!.ExitCode;
    }

    private string relScript(string name)
    {
        var exe = Process.GetCurrentProcess().MainModule!.FileName;
        var dir = Path.GetDirectoryName(exe)!;

        if (Path.GetExtension(exe) == ".exe")
        {
            return Path.Combine(dir, "rel", "bin", name + ".bat");
        }
        else
        {
            return Path.Combine(dir, "rel", "bin", name);
        }
    }
}

internal class Logger {
    private StreamWriter? writer;

    public Logger(string? path)
    {
        if (path != null)
        {
            writer = File.AppendText(path);
            writer.AutoFlush = true;
        }
    }

    public void Capture(Process process)
    {
        if (writer != null)
        {
            process.OutputDataReceived += (sender, e) =>
            {
                if (!String.IsNullOrEmpty(e.Data)) { writer.WriteLine(e.Data); }
            };

            process.ErrorDataReceived += (sender, e) =>
            {
                if (!String.IsNullOrEmpty(e.Data)) { writer.WriteLine(e.Data); }
            };
        }
    }
}

internal class Listener
{
    public int Port;
    public TcpListener TcpListener;

    public Listener()
    {
        var endpoint = new IPEndPoint(IPAddress.Loopback, 0);
        TcpListener = new(endpoint);
        TcpListener.Start();
        Port = ((IPEndPoint)TcpListener.LocalEndpoint).Port;
    }
}

internal class Client
{
    public TcpClient TcpClient;
    private NetworkStream stream;

    public Client(TcpClient tcpClient)
    {
        TcpClient = tcpClient;
        stream = tcpClient.GetStream();
    }

    public void Send(string payload)
    {
        var payloadBytes = System.Text.Encoding.UTF8.GetBytes(payload);
        var size = System.Convert.ToUInt32(IPAddress.HostToNetworkOrder(payloadBytes.Length));
        var headerBytes = BitConverter.GetBytes(size);
        stream.Write(headerBytes, 0, headerBytes.Length);
        stream.Write(payloadBytes, 0, payloadBytes.Length);
    }

    public void Subscribe(EventHandler handler)
    {
        var t = new Task(() => {
            receiveMessage(handler);
        });

        t.Start();
    }

    private void receiveMessage(EventHandler handler)
    {
        var sizeBuf = new byte[4];
        var sizeBytesRead = stream.Read(sizeBuf, 0, sizeBuf.Length);

        // socket is closed
        if (sizeBytesRead == 0) { return; }

        var size = IPAddress.NetworkToHostOrder((int)BitConverter.ToUInt32(sizeBuf, 0));
        var payloadBuf = new byte[size];
        var payloadBytesRead = stream.Read(payloadBuf, 0, size);

        // socket is closed
        if (payloadBytesRead == 0) { return; }

        var payload = System.Text.Encoding.UTF8.GetString(payloadBuf);
        var parts = payload.Split(new char[] {':'}, 2);
        var name = parts[0];
        var data = parts[1];

        handler(name, data);
        receiveMessage(handler);
    }
}
