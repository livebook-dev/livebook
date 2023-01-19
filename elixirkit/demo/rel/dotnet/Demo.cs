class Demo
{
    public static void Main()
    {
        ElixirKit.API.Start(name: "demo");
        ElixirKit.API.Publish("log", "Hello from C#!");

        ElixirKit.API.Subscribe((name, data) =>
        {
            switch (name)
            {
                case "log":
                    Console.WriteLine($"[client] {data}");
                    break;

                default:
                    throw new Exception($"unknown event {name}");
            }
        });

        ElixirKit.API.WaitForExit();
    }
}
