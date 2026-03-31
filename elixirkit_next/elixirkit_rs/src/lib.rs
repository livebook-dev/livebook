mod pubsub;

pub use pubsub::PubSub;

/// Returns a command for running `elixir` with [`PubSub`].
pub fn elixir(args: &[&str], pubsub: &PubSub) -> std::process::Command {
    let mut cmd = command(if cfg!(target_os = "windows") { "elixir.bat" } else { "elixir" }, pubsub);
    cmd.args(args);
    cmd
}

/// Returns a command for running a Mix task with [`PubSub`].
pub fn mix(task: &str, args: &[&str], pubsub: &PubSub) -> std::process::Command {
    let mut cmd = command(if cfg!(target_os = "windows") { "mix.bat" } else { "mix" }, pubsub);
    cmd.arg(task);
    cmd.args(args);
    cmd
}

/// Returns a command for launching an Elixir release with [`PubSub`].
pub fn release(dir: impl AsRef<std::path::Path>, name: &str, pubsub: &PubSub) -> std::process::Command {
    let dir = dir.as_ref();
    let mut script = dir.join("bin").join(name);
    if cfg!(target_os = "windows") {
        script.set_extension("bat");
    }
    let mut cmd = command(script.to_str().unwrap(), pubsub);
    cmd.args(["start"]);
    cmd
}

fn command(program: &str, pubsub: &PubSub) -> std::process::Command {
    let mut cmd = std::process::Command::new(program);
    cmd.env("ELIXIRKIT_PUBSUB", pubsub.url());
    cmd
}
