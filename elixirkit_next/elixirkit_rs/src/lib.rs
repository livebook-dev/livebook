use std::fs;
use std::io::{self, BufRead, BufReader, Read, Write};
use std::net::{TcpListener, TcpStream};
use std::path::{Path, PathBuf};
use std::process::{Child, Stdio};
use std::sync::{mpsc, Arc, Mutex};
use std::thread;
use std::time::{Duration, Instant};
use tracing::Level;

const CONNECT_TIMEOUT: Duration = Duration::from_secs(10);
const CONNECT_TIMEOUT_DEBUG: Duration = Duration::from_secs(300);
const CONNECT_POLL: Duration = Duration::from_millis(50);

#[derive(Debug)]
enum Message {
    Event(String, String), // (name, data)
    Exit(i32),
}

#[derive(Clone)]
pub struct Command {
    stream: Arc<TcpStream>,
}

impl Command {
    pub fn send(&self, (name, data): (&str, &str)) {
        let payload = format!("{name}:{data}");
        let _ = write_message(&self.stream, payload.as_bytes());
    }
}

pub struct MixTask;

impl MixTask {
    pub fn start<F>(
        path: impl Into<PathBuf>,
        task: impl Into<String>,
        args: &[&str],
        env: &[(&str, &str)],
        mut handler: F,
    ) -> i32
    where
        F: FnMut(&Command, (&str, &str)),
    {
        let path = path.into();
        let mut command = if cfg!(windows) {
            let mut cmd = std::process::Command::new("cmd");
            cmd.arg("/C").arg("mix.bat");
            cmd
        } else {
            std::process::Command::new("mix")
        };
        command.arg(task.into());
        command.args(args);
        command.current_dir(path);
        let env = env
            .iter()
            .map(|(key, value)| (key.to_string(), value.to_string()))
            .collect::<Vec<_>>();

        match start_elixir(command, env) {
            Ok((cmd, _child, events)) => run_event_loop(cmd, events, &mut handler),
            Err(e) => {
                eprintln!("Failed to start Elixir: {e}");
                1
            }
        }
    }
}

pub struct Release;

impl Release {
    pub fn start<F>(
        path: impl Into<PathBuf>,
        name: impl Into<String>,
        env: &[(&str, &str)],
        mut handler: F,
    ) -> i32
    where
        F: FnMut(&Command, (&str, &str)),
    {
        let path = path.into();
        let release_name = name.into();
        let mut script = path.join("bin").join(&release_name);
        if cfg!(target_os = "windows") && script.extension().is_none() {
            script.set_extension("bat");
        }

        let mut command = std::process::Command::new(script);
        command.arg("start").current_dir(&path);
        let env = build_release_env(&path, env);

        match start_elixir(command, env) {
            Ok((cmd, _child, events)) => run_event_loop(cmd, events, &mut handler),
            Err(e) => {
                eprintln!("Failed to start Elixir: {e}");
                1
            }
        }
    }
}

fn start_elixir(
    mut command: std::process::Command,
    env: Vec<(String, String)>,
) -> io::Result<(Command, Arc<Mutex<Child>>, mpsc::Receiver<Message>)> {
    let listener = TcpListener::bind("127.0.0.1:0")
        .map_err(|err| io::Error::new(err.kind(), format!("bind listener: {err}")))?;
    let port = listener
        .local_addr()
        .map_err(|err| io::Error::new(err.kind(), format!("read listener port: {err}")))?
        .port();

    command
        .env("ELIXIRKIT_PORT", port.to_string())
        .stdin(Stdio::null());
    for (key, value) in env {
        command.env(key, value);
    }
    command.stdout(Stdio::piped()).stderr(Stdio::piped());

    let mut child = command
        .spawn()
        .map_err(|err| io::Error::new(err.kind(), format!("spawn elixir: {err}")))?;

    if let Some(stdout) = child.stdout.take() {
        spawn_output_thread(stdout, false);
    }

    if let Some(stderr) = child.stderr.take() {
        spawn_output_thread(stderr, true);
    }

    listener
        .set_nonblocking(true)
        .map_err(|err| io::Error::new(err.kind(), format!("set nonblocking: {err}")))?;

    let start = Instant::now();
    let stream = loop {
        match listener.accept() {
            Ok((stream, _)) => break stream,
            Err(err) if err.kind() == io::ErrorKind::WouldBlock => {
                if let Some(status) = child
                    .try_wait()
                    .map_err(|err| io::Error::new(err.kind(), format!("wait child: {err}")))?
                {
                    return Err(io::Error::new(
                        io::ErrorKind::Other,
                        format!("elixir exited early: {status}"),
                    ));
                }

                let timeout = if cfg!(debug_assertions) {
                    CONNECT_TIMEOUT_DEBUG
                } else {
                    CONNECT_TIMEOUT
                };
                if start.elapsed() > timeout {
                    return Err(io::Error::new(
                        io::ErrorKind::TimedOut,
                        "timed out waiting for elixir connection",
                    ));
                }

                thread::sleep(CONNECT_POLL);
            }
            Err(err) => return Err(io::Error::new(err.kind(), format!("accept: {err}"))),
        }
    };

    stream
        .set_nonblocking(false)
        .map_err(|err| io::Error::new(err.kind(), format!("set blocking: {err}")))?;
    stream
        .set_nodelay(true)
        .map_err(|err| io::Error::new(err.kind(), format!("set nodelay: {err}")))?;

    let (msg_tx, msg_rx) = mpsc::channel();
    let mut read_stream = stream
        .try_clone()
        .map_err(|err| io::Error::new(err.kind(), format!("clone stream: {err}")))?;
    let read_tx = msg_tx.clone();
    thread::spawn(move || loop {
        match read_message(&mut read_stream) {
            Ok(Some(payload)) => {
                if let Some((name, data)) = parse_event(&payload) {
                    if read_tx.send(Message::Event(name, data)).is_err() {
                        break;
                    }
                }
            }
            Ok(None) => {
                thread::sleep(CONNECT_POLL);
            }
            Err(_) => break,
        }
    });

    let child = Arc::new(Mutex::new(child));
    let watch_child = Arc::clone(&child);
    let watch_tx = msg_tx;
    thread::spawn(move || loop {
        let status = {
            let mut child = match watch_child.lock() {
                Ok(child) => child,
                Err(_) => break,
            };
            match child.try_wait() {
                Ok(Some(status)) => Some(status),
                Ok(None) => None,
                Err(_) => break,
            }
        };

        if let Some(status) = status {
            let code = status.code().unwrap_or(1);
            let _ = watch_tx.send(Message::Exit(code));
            break;
        }

        thread::sleep(CONNECT_POLL);
    });

    let command = Command {
        stream: Arc::new(stream),
    };

    Ok((command, child, msg_rx))
}

fn run_event_loop<F>(
    command: Command,
    events: mpsc::Receiver<Message>,
    handler: &mut F,
) -> i32
where
    F: FnMut(&Command, (&str, &str)),
{
    loop {
        match events.recv() {
            Ok(Message::Event(name, data)) => {
                handler(&command, (&name, &data));
            }
            Ok(Message::Exit(code)) => {
                return code;
            }
            Err(_) => {
                // Channel closed, treat as exit
                return 1;
            }
        }
    }
}

fn build_release_env(path: &Path, env: &[(&str, &str)]) -> Vec<(String, String)> {
    let mut env_vec = env
        .iter()
        .map(|(key, value)| (key.to_string(), value.to_string()))
        .collect::<Vec<_>>();

    if cfg!(target_os = "windows") {
        let extra_paths = windows_release_paths(path);
        if !extra_paths.is_empty() {
            let mut base_path = None;
            for (key, value) in &env_vec {
                if key == "PATH" {
                    base_path = Some(value.clone());
                    break;
                }
            }

            let base_path = base_path
                .or_else(|| std::env::var("PATH").ok())
                .unwrap_or_default();
            let mut paths = Vec::with_capacity(extra_paths.len() + 1);
            paths.extend(extra_paths);
            paths.extend(std::env::split_paths(&base_path));

            let joined = std::env::join_paths(paths)
                .map(|value| value.to_string_lossy().to_string())
                .unwrap_or(base_path);

            let mut replaced = false;
            for (key, value) in &mut env_vec {
                if key == "PATH" {
                    *value = joined.clone();
                    replaced = true;
                    break;
                }
            }
            if !replaced {
                env_vec.push(("PATH".to_string(), joined));
            }
        }
    }

    env_vec
}

fn windows_release_paths(release_root: &Path) -> Vec<PathBuf> {
    let mut paths = Vec::new();
    let vendor = release_root.join("vendor");
    let vendor_entries = match fs::read_dir(&vendor) {
        Ok(entries) => entries,
        Err(_) => return paths,
    };

    for entry in vendor_entries.flatten() {
        let otp_root = entry.path().join("otp");
        if !otp_root.is_dir() {
            continue;
        }

        let otp_bin = otp_root.join("bin");
        if otp_bin.is_dir() {
            paths.push(otp_bin);
        }

        if let Ok(erts_entries) = fs::read_dir(&otp_root) {
            for erts_entry in erts_entries.flatten() {
                let name = erts_entry.file_name();
                let name = name.to_string_lossy();
                if name.starts_with("erts-") {
                    let bin = erts_entry.path().join("bin");
                    if bin.is_dir() {
                        paths.push(bin);
                    }
                }
            }
        }
    }

    paths
}

fn read_message(stream: &mut TcpStream) -> io::Result<Option<String>> {
    let mut size_buf = [0u8; 4];
    if let Err(err) = stream.read_exact(&mut size_buf) {
        if err.kind() == io::ErrorKind::WouldBlock {
            return Ok(None);
        }
        return Err(err);
    }

    let size = u32::from_be_bytes(size_buf) as usize;
    let mut payload = vec![0u8; size];
    if let Err(err) = stream.read_exact(&mut payload) {
        if err.kind() == io::ErrorKind::WouldBlock {
            return Ok(None);
        }
        return Err(err);
    }

    let payload = String::from_utf8(payload)
        .map_err(|err| io::Error::new(io::ErrorKind::InvalidData, err))?;
    Ok(Some(payload))
}

fn parse_event(payload: &str) -> Option<(String, String)> {
    let mut parts = payload.splitn(2, ':');
    let name = parts.next()?.to_string();
    let data = parts.next()?.to_string();
    Some((name, data))
}

fn write_message(stream: &TcpStream, payload: &[u8]) -> io::Result<()> {
    let size = u32::try_from(payload.len())
        .map_err(|_| io::Error::new(io::ErrorKind::InvalidInput, "payload too large"))?;
    let mut stream = stream;
    stream.write_all(&size.to_be_bytes())?;
    stream.write_all(payload)?;
    stream.flush()
}

fn spawn_output_thread(reader: impl Read + Send + 'static, is_stderr: bool) {
    thread::spawn(move || {
        let mut reader = BufReader::new(reader);
        let mut line = String::new();
        loop {
            line.clear();
            let bytes = reader.read_line(&mut line).unwrap_or(0);
            if bytes == 0 {
                break;
            }

            let line = line.trim_end_matches(&['\r', '\n'][..]);
            if is_stderr {
                if tracing::enabled!(Level::ERROR) {
                    tracing::error!(target: "elixir", "{line}");
                } else {
                    eprintln!("{line}");
                }
            } else if tracing::enabled!(Level::INFO) {
                tracing::info!(target: "elixir", "{line}");
            } else {
                println!("{line}");
            }
        }
    });
}
