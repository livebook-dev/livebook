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

    static void ensureMainInstance()
    {
        if (!mainInstance)
        {
            throw new Exception("Not on main instance");
        }
    }

    public static bool HasExited {
        get {
            ensureMainInstance();
            return release!.HasExited;
        }
    }

    public static void Start(string name, ExitHandler? exited = null, string? logPath = null)
    {
        ensureMainInstance();

        release = new Release(name, exited, logPath);

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
            release!.Publish(name, data);
        }
        else
        {
            var message = Release.EncodeEventMessage(name, data);
            PipeWriteLine(message);
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

public delegate void ExitHandler(int ExitCode);

internal class Release
{
    Process startProcess;
    NetworkStream stream;
    TcpListener listener;
    TcpClient client;

    internal bool HasExited {
        get {
            return startProcess.HasExited;
        }
    }

    public Release(string name, ExitHandler? exited = null, string? logPath = null)
    {
        var endpoint = new IPEndPoint(IPAddress.Loopback, 0);
        listener = new(endpoint);
        listener.Start();
        var port = ((IPEndPoint)listener.LocalEndpoint).Port;

        startProcess = new Process()
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

        startProcess.StartInfo.Arguments = "start";
        startProcess.StartInfo.EnvironmentVariables.Add("ELIXIRKIT_PORT", $"{port}");

        if (exited != null)
        {
            startProcess.EnableRaisingEvents = true;
            startProcess.Exited += (sender, args) =>
            {
                exited(startProcess.ExitCode);
            };
        }

        startProcess.OutputDataReceived += (sender, e) =>
        {
            if (!String.IsNullOrEmpty(e.Data)) { Console.WriteLine(e.Data); }
        };

        startProcess.ErrorDataReceived += (sender, e) =>
        {
            if (!String.IsNullOrEmpty(e.Data)) { Console.Error.WriteLine(e.Data); }
        };

        if (logPath != null)
        {
            var logWriter = File.AppendText(logPath);
            logWriter.AutoFlush = true;

            startProcess.OutputDataReceived += (sender, e) =>
            {
                if (!String.IsNullOrEmpty(e.Data)) { logWriter.WriteLine(e.Data); }
            };

            startProcess.ErrorDataReceived += (sender, e) =>
            {
                if (!String.IsNullOrEmpty(e.Data)) { logWriter.WriteLine(e.Data); }
            };
        }

        startProcess.Start();
        startProcess.BeginOutputReadLine();
        startProcess.BeginErrorReadLine();

        client = listener.AcceptTcpClient();
        stream = client.GetStream();
    }

    internal static string EncodeEventMessage(string name, string data)
    {
        var bytes = System.Text.Encoding.UTF8.GetBytes(data);
        var encoded = System.Convert.ToBase64String(bytes);
        return $"event:{name}:{encoded}";
    }

    public void Publish(string name, string data) {
        Send(EncodeEventMessage(name, data));
    }

    internal void Send(string message)
    {
        var bytes = Encoding.UTF8.GetBytes(message + "\n");
        stream.Write(bytes, 0, bytes.Length);
    }

    public int Stop()
    {
        if (HasExited)
        {
            return startProcess!.ExitCode;
        }

        client.Close();
        listener.Stop();
        return WaitForExit();
    }

    public int WaitForExit()
    {
        startProcess!.WaitForExit();
        return startProcess!.ExitCode;
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
