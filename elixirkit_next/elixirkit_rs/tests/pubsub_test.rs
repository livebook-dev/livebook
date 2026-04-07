use std::sync::mpsc;
use std::time::Duration;

#[test]
fn bidirectional_messaging() {
    let pubsub1 = elixirkit::PubSub::listen("tcp://127.0.0.1:0").unwrap();
    let pubsub2 = elixirkit::PubSub::connect(&pubsub1.url()).unwrap();

    let (tx1, rx1) = mpsc::channel();
    let (tx2, rx2) = mpsc::channel();

    pubsub1.subscribe("topic", move |msg| {
        tx1.send(msg.to_vec()).unwrap();
    });
    pubsub2.subscribe("topic", move |msg| {
        tx2.send(msg.to_vec()).unwrap();
    });

    pubsub1.broadcast("topic", b"message1").unwrap();
    pubsub2.broadcast("topic", b"message2").unwrap();

    let msg = rx2.recv_timeout(Duration::from_secs(5)).unwrap();
    assert_eq!(msg, b"message1");

    let msg = rx1.recv_timeout(Duration::from_secs(5)).unwrap();
    assert_eq!(msg, b"message2");
}
