namespace Demo;

static class DemoMain
{
    [STAThread]
    static void Main()
    {
        var api = new ElixirKit.API(id: "com.example.Demo");

        if (api.MainInstance)
        {
            api.Start(name: "demo", exited: (exitCode) =>
            {
                Application.Exit();
            });

            Application.ApplicationExit += (sender, args) =>
            {
                api.Stop();
            };

            api.Publish("log", "Hello from Windows Forms!");

            ApplicationConfiguration.Initialize();
            Application.Run(new DemoForm(api));
        }
        else
        {
            api.Publish("log", "Hello from another instance!");
        }
    }
}

class DemoForm : Form
{
    ElixirKit.API api;

    public DemoForm(ElixirKit.API api)
    {
        this.api = api;
        InitializeComponent();
    }

    private void form_Load(object? sender, EventArgs e)
    {
    }

    private void button_Click(object? sender, EventArgs e)
    {
        api.Publish("log", "button pressed!");
    }

    // WinForms boilerplate below.

    private System.ComponentModel.IContainer? components = null;

    protected override void Dispose(bool disposing)
    {
        if (disposing && (components != null))
        {
            components.Dispose();
        }
        base.Dispose(disposing);
    }

    #region Windows Form Designer generated code

    private void InitializeComponent()
    {
        this.components = new System.ComponentModel.Container();
        this.button = new System.Windows.Forms.Button();
        this.SuspendLayout();
        //
        // button
        //
        this.button.Location = new System.Drawing.Point(20, 20);
        this.button.Name = "button";
        this.button.Size = new System.Drawing.Size(200, 100);
        this.button.TabIndex = 0;
        this.button.Text = "Press me!";
        this.button.UseVisualStyleBackColor = true;
        this.button.Click += new System.EventHandler(this.button_Click);
        //
        // form
        //
        this.AutoScaleDimensions = new System.Drawing.SizeF(12F, 25F);
        this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
        this.ClientSize = new System.Drawing.Size(800, 450);
        this.Controls.Add(this.button);
        this.Name = "form";
        this.Text = "Demo";
        this.Load += new System.EventHandler(this.form_Load);
        this.ResumeLayout(false);
    }

    #endregion

    private System.Windows.Forms.Button button;
}
