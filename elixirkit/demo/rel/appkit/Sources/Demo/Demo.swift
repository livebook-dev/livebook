@main
public struct Demo {
    public private(set) var text = "Hello, World!"

    public static func main() {
        print(Demo().text)
    }
}
