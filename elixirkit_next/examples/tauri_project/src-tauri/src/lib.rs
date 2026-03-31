use elixirkit::PubSub;
use std::path::PathBuf;
use std::thread;
#[cfg(not(debug_assertions))]
use tauri::Manager;

#[cfg(not(debug_assertions))]
const EXAMPLE_RELEASE_NAME: &str = "example";
#[cfg(not(debug_assertions))]
const EXAMPLE_SECRET_KEY_BASE: &str =
    "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef";

pub fn run() {
    let pubsub = PubSub::listen("tcp://127.0.0.1:0").expect("failed to listen");

    tauri::Builder::default()
        .plugin(tauri_plugin_opener::init())
        .setup(move |app| {
            use tauri::Manager;

            let app_handle = app.handle().clone();
            pubsub.subscribe("messages", move |msg| {
                if msg == b"ready" {
                    if let Some(window) = app_handle.get_webview_window("main") {
                        let _ = window.navigate("http://localhost:4000".parse().unwrap());
                        let _ = window.show();
                    }
                } else if msg == b"toggle_devtools" {
                    if let Some(window) = app_handle.get_webview_window("main") {
                        toggle_devtools(window);
                    }
                } else {
                    println!("[rust] {}", String::from_utf8_lossy(msg));
                }
            });

            let app_handle = app.handle().clone();
            let elixir_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("..");
            thread::spawn(move || {
                let status = elixir_command(&app_handle, &elixir_dir, &pubsub)
                    .status()
                    .expect("failed to start Elixir");

                app_handle.exit(status.code().unwrap_or(1));
            });

            Ok(())
        })
        .build(tauri::generate_context!())
        .expect("error while building tauri application")
        .run(|_app_handle, _event| {});
}

#[cfg(debug_assertions)]
fn elixir_command(
    _app_handle: &tauri::AppHandle,
    elixir_dir: &PathBuf,
    pubsub: &PubSub,
) -> std::process::Command {
    let mut cmd = elixirkit::mix("phx.server", &[], pubsub);
    cmd.current_dir(elixir_dir);
    cmd
}

#[cfg(not(debug_assertions))]
fn elixir_command(
    app_handle: &tauri::AppHandle,
    _elixir_dir: &PathBuf,
    pubsub: &PubSub,
) -> std::process::Command {
    let release_dir = app_handle
        .path()
        .resource_dir()
        .expect("failed to resolve Tauri resources directory")
        .join("rel");

    let mut cmd = elixirkit::release(&release_dir, EXAMPLE_RELEASE_NAME, pubsub);
    cmd.env("PHX_SERVER", "true");
    cmd.env("PHX_HOST", "127.0.0.1");
    cmd.env("PORT", "4000");
    cmd.env("SECRET_KEY_BASE", EXAMPLE_SECRET_KEY_BASE);
    cmd
}

#[cfg(debug_assertions)]
fn toggle_devtools(window: tauri::WebviewWindow) {
    if window.is_devtools_open() {
        window.close_devtools();
    } else {
        window.open_devtools();
    }
}

#[cfg(not(debug_assertions))]
fn toggle_devtools(_window: tauri::WebviewWindow) {}
