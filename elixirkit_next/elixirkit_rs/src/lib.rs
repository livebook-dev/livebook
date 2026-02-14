use std::io::{self, BufRead, BufReader, Read, Write};
use std::net::{TcpListener, TcpStream};
use std::path::PathBuf;
use std::process::{Child, Stdio};
use std::sync::{mpsc, Arc, Mutex};
use std::thread;
use std::time::{Duration, Instant};
use tracing::Level;

const CONNECT_TIMEOUT: Duration = Duration::from_secs(10);
const CONNECT_TIMEOUT_DEBUG: Duration = Duration::from_secs(300);
const CONNECT_POLL: Duration = Duration::from_millis(500);

#[derive(Debug)]
enum Message {
    Event(String, String), // (name, data)
    Exit(i32),
}

#[derive(Clone)]
pub struct Command {
    stream: Arc<Mutex<Option<Arc<TcpStream>>>>,
    program: String,
    args: Vec<String>,
    cwd: Option<PathBuf>,
    env: Vec<(String, String)>,
}

impl Command {
    pub fn new(program: impl Into<String>, args: &[impl AsRef<str>]) -> Self {
        Self {
            stream: Arc::new(Mutex::new(None)),
            program: program.into(),
            args: args.iter().map(|s| s.as_ref().to_string()).collect(),
            cwd: None,
            env: Vec::new(),
        }
    }

    pub fn current_dir(mut self, path: impl Into<PathBuf>) -> Self {
        self.cwd = Some(path.into());
        self
    }

    pub fn env(mut self, env: &[(&str, &str)]) -> Self {
        self.env = env
            .iter()
            .map(|(k, v)| (k.to_string(), v.to_string()))
            .collect();
        self
    }

    pub fn send(&self, (name, data): (&str, &str)) {
        if let Some(stream) = self.stream.lock().unwrap().as_ref() {
            let payload = format!("{name}:{data}");
            let _ = write_message(stream, payload.as_bytes());
        } else {
            panic!("Cannot send before start");
        }
    }

    pub fn start<F>(&self, mut handler: F) -> i32
    where
        F: FnMut((&str, &str)),
    {
        let mut command = std::process::Command::new(&self.program);
        command.args(&self.args);
        if let Some(cwd) = &self.cwd {
            command.current_dir(cwd);
        }

        let env_refs: Vec<(&str, &str)> = self
            .env
            .iter()
            .map(|(k, v)| (k.as_str(), v.as_str()))
            .collect();

        match start_elixir(command, &env_refs) {
            Ok((stream, _child, events)) => {
                *self.stream.lock().unwrap() = Some(stream);
                run_event_loop(events, &mut handler)
            }
            Err(e) => {
                eprintln!("Failed to start Elixir: {e}");
                1
            }
        }
    }
}

pub fn release(path: impl Into<PathBuf>, name: impl Into<String>) -> Command {
    let path = path.into();
    let name = name.into();
    let mut script = path.join("bin").join(&name);
    if cfg!(target_os = "windows") && script.extension().is_none() {
        script.set_extension("bat");
    }

    Command {
        stream: Arc::new(Mutex::new(None)),
        program: script.to_string_lossy().to_string(),
        args: vec!["start".to_string()],
        cwd: Some(path),
        env: Vec::new(),
    }
}

fn start_elixir(
    mut command: std::process::Command,
    env: &[(&str, &str)],
) -> io::Result<(Arc<TcpStream>, Arc<Mutex<Child>>, mpsc::Receiver<Message>)> {
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

    Ok((Arc::new(stream), child, msg_rx))
}

fn run_event_loop<F>(events: mpsc::Receiver<Message>, handler: &mut F) -> i32
where
    F: FnMut((&str, &str)),
{
    loop {
        match events.recv() {
            Ok(Message::Event(name, data)) => {
                handler((&name, &data));
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
