using ElixirKit;

class Demo {
    public static void Main()
    {
        var api = new ElixirKit.API();
        api.Start(name: "demo");
        api.Publish("log", "Hello from C#!");
        api.WaitForExit();
    }
}
