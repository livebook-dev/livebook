using System;
using System.Drawing;
using System.IO;
using System.Windows.Forms;

#nullable enable

namespace Livebook;

static class LivebookMain
{
    [STAThread]
    static void Main(string[] args)
    {
        var prefix = "open:";
        var url = "";

        if (args.Length == 1 && args[0].StartsWith(prefix))
        {
            var uri = new System.Uri(args[0].Remove(0, prefix.Length));
            url = uri.AbsoluteUri;
        }

        if (ElixirKit.API.IsMainInstance("dev.livebook.Livebook"))
        {
            Application.EnableVisualStyles();
            Application.SetCompatibleTextRenderingDefault(false);
            Application.Run(new LivebookApp(url));

            if (!ElixirKit.API.HasExited)
            {
                ElixirKit.API.Publish("shutdown", "");
            }

            var code = ElixirKit.API.WaitForExit();
            if (code == 0) { return; }
            var message = $"Livebook exited with exit code {code}.\r\nLogs available at: {getLogPath()}";
            MessageBox.Show(new Form() { TopMost = true }, message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
        }
        else
        {
            ElixirKit.API.Publish("open", url);
        }
    }

    internal static string getLogPath()
    {
        return Path.Combine(
            Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData),
            "Livebook",
            "Logs",
            "Livebook.log"
        );
    }
}

class DummyForm : Form {}

class LivebookApp : ApplicationContext
{
    private NotifyIcon notifyIcon;

    public LivebookApp(string url)
    {
        ThreadExit += threadExit;

        ContextMenuStrip menu = new ContextMenuStrip();
        menu.Items.Add("Open", null, openClicked);
        menu.Items.Add("Quit", null, quitClicked);
        notifyIcon = new NotifyIcon()
        {
            Text = "Livebook",
            Visible = true,
            Icon = Icon.ExtractAssociatedIcon(Application.ExecutablePath)!,
            ContextMenuStrip = menu
        };
        notifyIcon.Click += notifyIconClicked;

        var logPath = LivebookMain.getLogPath();
        Directory.CreateDirectory(Path.GetDirectoryName(logPath)!);

        ElixirKit.API.Start(
            name: "app",
            logPath: logPath,
            exited: (exitCode) =>
            {
                Application.Exit();
            }
        );

        ElixirKit.API.Publish("open", url);
    }

    private void threadExit(object? sender, EventArgs e)
    {
        notifyIcon.Visible = false;
    }

    private void notifyIconClicked(object? sender, EventArgs e)
    {
        MouseEventArgs mouseEventArgs = (MouseEventArgs)e;

        if (mouseEventArgs.Button == MouseButtons.Left)
        {
            open();
        }
    }

    private void openClicked(object? sender, EventArgs e)
    {
        open();
    }

    private void quitClicked(object? sender, EventArgs e)
    {
        notifyIcon.Visible = false;
        Application.Exit();
    }

    private void open() {
        ElixirKit.API.Publish("open", "");
    }
}
