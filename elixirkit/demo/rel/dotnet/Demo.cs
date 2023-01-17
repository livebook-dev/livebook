class Demo
{
    public static void Main()
    {
        ElixirKit.API.Start(name: "demo");
        ElixirKit.API.Publish("log", "Hello from C#!");
        ElixirKit.API.WaitForExit();
    }
}
