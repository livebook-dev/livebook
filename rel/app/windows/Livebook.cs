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

        var logPath = getLogPath();

        if (ElixirKit.API.IsMainInstance("dev.livebook.Livebook"))
        {
            Application.EnableVisualStyles();
            Application.SetCompatibleTextRenderingDefault(false);
            Application.Run(new LivebookApp(url, logPath));

            var code = ElixirKit.API.Stop();
            if (code == 0) { return; }
            var message = $"Livebook exited with exit code {code}.\r\nLogs available at: {logPath}";
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
    private string? url;
    private string logPath;

    public LivebookApp(string url, string logPath)
    {
        this.logPath = logPath;

        ThreadExit += threadExit;

        ContextMenuStrip menu = new ContextMenuStrip();
        menu.Items.Add("Open", null, openClicked);

        var copyURLButton = menu.Items.Add("Copy URL", null, copyURLClicked);
        copyURLButton.Enabled = false;

        menu.Items.Add("View Logs", null, viewLogsClicked);
        menu.Items.Add("Settings", null, openSettingsClicked);
        menu.Items.Add("Quit", null, quitClicked);
        notifyIcon = new NotifyIcon()
        {
            Text = "Livebook",
            Visible = true,
            Icon = Icon.ExtractAssociatedIcon(Application.ExecutablePath)!,
            ContextMenuStrip = menu
        };
        notifyIcon.Click += notifyIconClicked;

        Directory.CreateDirectory(Path.GetDirectoryName(logPath)!);

        ElixirKit.API.Start(
            name: "app",
            logPath: logPath,
            ready: () => {
                ElixirKit.API.Subscribe((name, data) =>
                {
                    switch (name)
                    {
                        case "url":
                            copyURLButton.Enabled = true;
                            this.url = data;
                            break;

                        default:
                            throw new Exception($"unknown event {name}");
                    }
                });

                ElixirKit.API.Publish("open", url);
            },
            exited: (exitCode) =>
            {
                Application.Exit();
            }
        );
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
            ElixirKit.API.Publish("open", "");
        }
    }

    private void openClicked(object? sender, EventArgs e)
    {
        ElixirKit.API.Publish("open", "");
    }

    private void copyURLClicked(object? sender, EventArgs e)
    {
        System.Windows.Forms.Clipboard.SetText(url!);
    }

    private void viewLogsClicked(object? sender, EventArgs e)
    {
        System.Diagnostics.Process.Start(logPath);
    }

    private void openSettingsClicked(object? sender, EventArgs e)
    {
        ElixirKit.API.Publish("open", "/settings");
    }

    private void quitClicked(object? sender, EventArgs e)
    {
        notifyIcon.Visible = false;
        Application.Exit();
    }
}
