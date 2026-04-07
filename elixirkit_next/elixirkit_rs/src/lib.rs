mod pubsub;

pub use pubsub::PubSub;

/// Returns a command for running `elixir`.
pub fn elixir(args: &[&str]) -> std::process::Command {
    let mut cmd = std::process::Command::new(if cfg!(target_os = "windows") { "elixir.bat" } else { "elixir" });
    cmd.args(args);
    cmd
}

/// Returns a command for running a Mix task.
pub fn mix(task: &str, args: &[&str]) -> std::process::Command {
    let mut cmd = std::process::Command::new(if cfg!(target_os = "windows") { "mix.bat" } else { "mix" });
    cmd.arg(task);
    cmd.args(args);
    cmd
}

/// Returns a command for launching an Elixir release.
pub fn release(dir: impl AsRef<std::path::Path>, name: &str) -> std::process::Command {
    let dir = dir.as_ref();
    let mut script = dir.join("bin").join(name);
    if cfg!(target_os = "windows") {
        script.set_extension("bat");
    }
    let mut cmd = std::process::Command::new(script.to_str().unwrap());
    cmd.args(["start"]);
    cmd
}
