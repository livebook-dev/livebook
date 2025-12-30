use elixirkit::Command;

fn main() {
    let root = std::env::current_dir().unwrap();
    let elixir_dir = root.join("src-elixir");

    let command = Command::new("mix", &["run", "--no-halt"]).current_dir(elixir_dir);

    let exit_code = command.start(|(name, data)| {
        match name {
            "ready" => {
                command.send(("ping", "ping"));
            }
            "echo" => {
                println!("[cli] {data}");
            }
            _ => panic!("unexpected event: {name}:{data}"),
        }
    });

    std::process::exit(exit_code);
}
