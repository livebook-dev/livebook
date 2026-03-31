use std::collections::HashMap;
use std::io::{self, Read, Write};
use std::net::{TcpListener, TcpStream};
use std::sync::{Arc, Condvar, Mutex};
use std::thread;

type Callback = Box<dyn Fn(&[u8]) + Send>;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ConnectionState {
    WaitingForConnection,
    Connected,
    Closed,
}

struct Inner {
    port: u16,
    stream: Mutex<Option<TcpStream>>,
    subscribers: Mutex<HashMap<String, Vec<Callback>>>,
    connection_state: Mutex<ConnectionState>,
    connection_state_changed: Condvar,
}

/// A handle to the PubSub connection.
#[derive(Clone)]
pub struct PubSub {
    inner: Arc<Inner>,
}

impl PubSub {
    /// Listens on the given URL and returns a [`PubSub`] handle.
    ///
    /// The URL must be in the format `tcp://127.0.0.1:{port}`, where port
    /// can be `0` to let the OS assign an available port.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// let pubsub = elixirkit::PubSub::listen("tcp://127.0.0.1:0")
    ///     .expect("failed to listen");
    /// ```
    pub fn listen(url: &str) -> Result<Self, io::Error> {
        let port = parse_url(url)?;
        let listener = TcpListener::bind(("127.0.0.1", port))?;
        let actual_port = listener.local_addr()?.port();

        let pubsub = PubSub {
            inner: Arc::new(Inner {
                port: actual_port,
                stream: Mutex::new(None),
                subscribers: Mutex::new(HashMap::new()),
                connection_state: Mutex::new(ConnectionState::WaitingForConnection),
                connection_state_changed: Condvar::new(),
            }),
        };

        let inner = pubsub.inner.clone();
        thread::Builder::new()
            .name("elixirkit-pubsub".into())
            .spawn(move || {
                let Ok((tcp_stream, _)) = listener.accept() else {
                    set_connection_state(&inner, ConnectionState::Closed);
                    return;
                };
                let _ = tcp_stream.set_nodelay(true);

                let reader = match tcp_stream.try_clone() {
                    Ok(r) => r,
                    Err(_) => {
                        set_connection_state(&inner, ConnectionState::Closed);
                        return;
                    }
                };
                *inner.stream.lock().unwrap() = Some(tcp_stream);
                set_connection_state(&inner, ConnectionState::Connected);

                read_loop(&inner, reader);
                *inner.stream.lock().unwrap() = None;
                set_connection_state(&inner, ConnectionState::Closed);
            })?;

        Ok(pubsub)
    }

    // TODO: not documented, used just for testing for now.
    #[doc(hidden)]
    pub fn connect(url: &str) -> Result<Self, io::Error> {
        let port = parse_url(url)?;
        let tcp_stream = TcpStream::connect(("127.0.0.1", port))?;
        let _ = tcp_stream.set_nodelay(true);

        let reader = tcp_stream.try_clone()?;

        let pubsub = PubSub {
            inner: Arc::new(Inner {
                port,
                stream: Mutex::new(Some(tcp_stream)),
                subscribers: Mutex::new(HashMap::new()),
                connection_state: Mutex::new(ConnectionState::Connected),
                connection_state_changed: Condvar::new(),
            }),
        };

        let inner = pubsub.inner.clone();
        thread::Builder::new()
            .name("elixirkit-pubsub".into())
            .spawn(move || {
                read_loop(&inner, reader);
                *inner.stream.lock().unwrap() = None;
                set_connection_state(&inner, ConnectionState::Closed);
            })?;

        Ok(pubsub)
    }

    /// Returns the URL for this PubSub connection.
    pub fn url(&self) -> String {
        format!("tcp://127.0.0.1:{}", self.inner.port)
    }

    /// Subscribes to messages on the given topic from the Elixir side.
    pub fn subscribe<F>(&self, topic: &str, callback: F)
    where
        F: Fn(&[u8]) + Send + 'static,
    {
        self.inner
            .subscribers
            .lock()
            .unwrap()
            .entry(topic.to_string())
            .or_default()
            .push(Box::new(callback));
    }

    /// Broadcasts a message on the given topic to the Elixir side.
    pub fn broadcast(&self, topic: &str, message: &[u8]) -> io::Result<()> {
        if topic.len() > 255 {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "topic must be at most 255 bytes",
            ));
        }
        let guard = self.inner.stream.lock().unwrap();
        match guard.as_ref() {
            Some(stream) => write_message(stream, topic.as_bytes(), message),
            None => Err(io::Error::new(io::ErrorKind::NotConnected, "not connected")),
        }
    }

    // TODO: not documented, used just for testing for now.
    #[doc(hidden)]
    pub fn wait(&self) {
        let mut state = self.inner.connection_state.lock().unwrap();
        while *state != ConnectionState::Closed {
            state = self.inner.connection_state_changed.wait(state).unwrap();
        }
    }
}

fn read_loop(inner: &Inner, mut reader: TcpStream) {
    loop {
        let mut len_buf = [0u8; 4];
        if reader.read_exact(&mut len_buf).is_err() {
            break;
        }
        let frame_len = u32::from_be_bytes(len_buf) as usize;

        let mut frame = vec![0u8; frame_len];
        if reader.read_exact(&mut frame).is_err() {
            break;
        }

        if frame.is_empty() {
            continue;
        }
        let topic_len = frame[0] as usize;
        if frame.len() < 1 + topic_len {
            continue;
        }
        let topic = &frame[1..1 + topic_len];
        let payload = &frame[1 + topic_len..];

        let topic_str = String::from_utf8_lossy(topic);

        let subscribers = inner.subscribers.lock().unwrap();
        if let Some(callbacks) = subscribers.get(topic_str.as_ref()) {
            for cb in callbacks {
                cb(payload);
            }
        }
    }
}

fn parse_url(url: &str) -> Result<u16, io::Error> {
    url.strip_prefix("tcp://127.0.0.1:")
        .and_then(|port| port.parse::<u16>().ok())
        .ok_or_else(|| {
            io::Error::new(
                io::ErrorKind::InvalidInput,
                format!("expected tcp://127.0.0.1:{{port}}, got: {:?}", url),
            )
        })
}

fn set_connection_state(inner: &Inner, state: ConnectionState) {
    *inner.connection_state.lock().unwrap() = state;
    inner.connection_state_changed.notify_all();
}

fn write_message(mut stream: &TcpStream, topic: &[u8], payload: &[u8]) -> io::Result<()> {
    let inner_len = 1 + topic.len() + payload.len();
    let frame_len = u32::try_from(inner_len)
        .map_err(|_| io::Error::new(io::ErrorKind::InvalidInput, "frame too large"))?;

    stream.write_all(&frame_len.to_be_bytes())?;
    stream.write_all(&[topic.len() as u8])?;
    stream.write_all(topic)?;
    stream.write_all(payload)?;
    stream.flush()
}
