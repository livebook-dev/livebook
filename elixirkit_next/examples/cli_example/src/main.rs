use elixirkit::MixTask;

fn main() {
    let root = std::env::current_dir().unwrap();
    let elixir_dir = root.join("src-elixir");

    let exit_code = MixTask::start(elixir_dir, "run", &["--no-halt"], &[], |cmd, (name, data)| {
        match name {
            "ready" => {
                cmd.send(("ping", "ping"));
            }
            "echo" => {
                println!("[cli] {data}");
            }
            _ => panic!("unexpected event: {name}:{data}"),
        }
    });

    std::process::exit(exit_code);
}
