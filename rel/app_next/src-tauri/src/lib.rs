use elixirkit::Command;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};
use tauri::menu::{Menu, MenuItem, PredefinedMenuItem};
use tauri::tray::TrayIconBuilder;
use tauri::{AppHandle, Manager, Wry};
use tauri_plugin_clipboard_manager::ClipboardExt;
use tauri_plugin_dialog::{DialogExt, MessageDialogButtons, MessageDialogKind};
use tauri_plugin_opener::OpenerExt;
use tauri_plugin_updater::UpdaterExt;
use tracing_subscriber::{
    filter::LevelFilter, layer::SubscriberExt, util::SubscriberInitExt, Layer,
};

const TRAY_ID: &str = "livebook-tray";

pub fn run() {
    let builder = tauri::Builder::default()
        .plugin(tauri_plugin_deep_link::init())
        .plugin(tauri_plugin_single_instance::init(|app, argv, _cwd| {
            let urls = extract_open_urls(argv);
            if let Some(state) = app.try_state::<AppState>() {
                for url in urls {
                    state.publish_open(&url);
                }
            }
        }))
        .plugin(tauri_plugin_opener::init())
        .plugin(tauri_plugin_dialog::init())
        .plugin(tauri_plugin_clipboard_manager::init())
        .plugin(tauri_plugin_updater::Builder::new().build())
        .setup(|app| {
            let log_path = log_path()?;
            ensure_parent_dir(&log_path)?;
            let log_guard = init_tracing(&log_path);

            let app_handle = app.handle();
            let open_item = menu_item(app_handle, "open", "Open", true, "cmd+o");
            let new_item = menu_item(app_handle, "new", "New Notebook", true, "cmd+n");
            let copy_url_item = menu_item(app_handle, "copy-url", "Copy URL", false, "cmd+c");
            let logs_item = menu_item(app_handle, "view-logs", "View Logs", true, "cmd+l");
            let boot_item = menu_item(app_handle, "boot-script", boot_script_label(), true, "");
            let settings_item = menu_item(app_handle, "settings", "Settings", true, "cmd+,");
            let check_updates_item = menu_item(app_handle, "check-updates", "Check for Updates…", true, "");
            let quit_item = menu_item(app_handle, "quit", "Quit", true, "cmd+q");
            let tray_menu = Menu::with_items(
                app_handle,
                &[
                    &open_item,
                    &new_item,
                    &PredefinedMenuItem::separator(app_handle)?,
                    &copy_url_item,
                    &logs_item,
                    &boot_item,
                    &PredefinedMenuItem::separator(app_handle)?,
                    &settings_item,
                    &check_updates_item,
                    &quit_item,
                ],
            )?;

            let tray = TrayIconBuilder::with_id(TRAY_ID)
                .tooltip("Livebook")
                .icon(app_handle.default_window_icon().unwrap().clone())
                .show_menu_on_left_click(false)
                .on_menu_event(move |app, event| match event.id.as_ref() {
                    "open" => app.state::<AppState>().publish_open(""),
                    "new" => app.state::<AppState>().publish_open("/new"),
                    "settings" => app.state::<AppState>().publish_open("/settings"),
                    "copy-url" => {
                        if let Some(state) = app.try_state::<AppState>() {
                            if let Some(url) = state.get_url() {
                                let _ = app.clipboard().write_text(url);
                            }
                        }
                    }
                    "view-logs" => {
                        if let Some(state) = app.try_state::<AppState>() {
                            let _ = app
                                .opener()
                                .open_path(state.log_path.display().to_string(), None::<&str>);
                        }
                    }
                    "boot-script" => {
                        if let Ok(path) = ensure_boot_script() {
                            let _ = app.opener().open_path(path.display().to_string(), None::<&str>);
                        }
                    }
                    "check-updates" => {
                        check_for_updates(app.clone());
                    }
                    "quit" => app.exit(0),
                    _ => {}
                })
                .build(app_handle)?;

            let state = AppState::new(
                log_path.clone(),
                log_guard,
                tray_menu,
                copy_url_item,
                tray,
            );
            app.manage(state);

            let initial_urls = extract_open_urls(std::env::args().skip(1).collect());
            if initial_urls.is_empty() {
                if let Some(state) = app.try_state::<AppState>() {
                    state.publish_open("");
                }
            } else {
                for url in initial_urls {
                    if let Some(state) = app.try_state::<AppState>() {
                        state.publish_open(&url);
                    }
                }
            }

            // Check for updates on boot
            let app_handle_for_updates = app.handle().clone();
            tauri::async_runtime::spawn(async move {
                let _ = check_for_updates_on_boot(app_handle_for_updates).await;
            });

            let handle = app.handle().clone();
            std::thread::spawn(move || {
                let command = if cfg!(debug_assertions) {
                    let mix_root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../..");
                    Command::new("mix", &["phx.server"])
                        .current_dir(mix_root)
                        .env(&[("MIX_TARGET", "app_next")])
                } else {
                    let release_dir = handle.path().resource_dir().unwrap().join("rel");
                    elixirkit::release(release_dir, "app")
                };

                let status = command.start(|(name, data)| {
                    if name == "ready" {
                        let state = handle.state::<AppState>();
                        state.set_elixir_command(&command);

                        state.set_url(data.to_string());
                        state.enable_tray_menu();
                        if state.should_open_url() {
                            let _ = handle.opener().open_url(data, None::<&str>);
                        }
                    } else {
                        tracing::error!("unexpected event: {name}:{data}");
                        handle.exit(1);
                    }
                });

                if status != 0 {
                    show_exit_dialog(&handle, status);
                }
                handle.exit(status);
            });

            Ok(())
        });

    let app = builder
        .build(tauri::generate_context!())
        .expect("error while building tauri application");

    app.run(|app_handle, event| {
        #[cfg(not(target_os = "macos"))]
        let _ = app_handle;

        match event {
            #[cfg(target_os = "macos")]
            tauri::RunEvent::Opened { urls } => {
                if let Some(state) = app_handle.try_state::<AppState>() {
                    for url in normalize_urls(urls) {
                        state.publish_open(&url);
                    }
                }
            }

            #[cfg(target_os = "macos")]
            tauri::RunEvent::Reopen { .. } => {
                if let Some(state) = app_handle.try_state::<AppState>() {
                    state.publish_open("");
                }
            }

            _ => {}
        }
    });
}

struct AppState {
    command: Arc<Mutex<Option<Arc<Command>>>>,
    pending_open: Arc<Mutex<Vec<String>>>,
    current_url: Arc<Mutex<Option<String>>>,
    log_path: PathBuf,
    did_open_url: Arc<Mutex<bool>>,
    _log_guard: Option<tracing_appender::non_blocking::WorkerGuard>,
    tray_menu: Menu<Wry>,
    copy_url_item: MenuItem<Wry>,
    tray: tauri::tray::TrayIcon<Wry>,
    tray_menu_ready: Arc<Mutex<bool>>,
}

impl AppState {
    fn new(
        log_path: PathBuf,
        log_guard: Option<tracing_appender::non_blocking::WorkerGuard>,
        tray_menu: Menu<Wry>,
        copy_url_item: MenuItem<Wry>,
        tray: tauri::tray::TrayIcon<Wry>,
    ) -> Self {
        Self {
            command: Arc::new(Mutex::new(None)),
            pending_open: Arc::new(Mutex::new(Vec::new())),
            current_url: Arc::new(Mutex::new(None)),
            log_path,
            did_open_url: Arc::new(Mutex::new(false)),
            _log_guard: log_guard,
            tray_menu,
            copy_url_item,
            tray,
            tray_menu_ready: Arc::new(Mutex::new(false)),
        }
    }

    fn publish_open(&self, url: &str) {
        if let Some(command) = self.command.lock().ok().and_then(|guard| guard.clone()) {
            command.send(("open", url));
            return;
        }

        if let Ok(mut pending) = self.pending_open.lock() {
            pending.push(url.to_string());
        }
    }

    fn set_url(&self, url: String) {
        if let Ok(mut guard) = self.current_url.lock() {
            *guard = Some(url);
        }
    }

    fn get_url(&self) -> Option<String> {
        self.current_url
            .lock()
            .ok()
            .and_then(|value| value.clone())
    }

    fn should_open_url(&self) -> bool {
        if let Ok(mut guard) = self.did_open_url.lock() {
            if !*guard {
                *guard = true;
                return true;
            }
        }
        false
    }

    fn enable_tray_menu(&self) {
        let should_set = self
            .tray_menu_ready
            .lock()
            .map(|mut ready| {
                if *ready {
                    false
                } else {
                    *ready = true;
                    true
                }
            })
            .unwrap_or(false);

        if !should_set {
            return;
        }

        let _ = self.tray.set_menu(Some(self.tray_menu.clone()));
        let _ = self.tray.set_show_menu_on_left_click(true);
        let _ = self.copy_url_item.set_enabled(true);
    }

    fn set_elixir_command(&self, command: &Command) {
        if let Ok(mut guard) = self.command.lock() {
            *guard = Some(Arc::new(command.clone()));
        }

        if let Ok(mut pending) = self.pending_open.lock() {
            if let Some(cmd) = self.command.lock().ok().and_then(|guard| guard.clone()) {
                for url in pending.drain(..) {
                    cmd.send(("open", &url));
                }
            }
        }
    }
}

fn menu_item(
    app_handle: &AppHandle,
    id: &str,
    label: &str,
    is_enabled: bool,
    accelerator: &str,
) -> MenuItem<Wry> {
    let accel = if cfg!(target_os = "macos") && !accelerator.is_empty() {
        Some(accelerator)
    } else {
        None
    };

    MenuItem::with_id(app_handle, id, label, is_enabled, accel)
        .expect("failed to create menu item")
}

fn show_exit_dialog(handle: &AppHandle, code: i32) {
    let log_path = handle
        .try_state::<AppState>()
        .map(|state| state.log_path.clone());
    let message = match log_path {
        Some(path) => format!(
            "Livebook exited with exit code {}.\nLogs available at: {}",
            code,
            path.display()
        ),
        None => format!("Livebook exited with exit code {code}."),
    };
    handle.dialog().message(message).show(|_| {});
}

fn extract_open_urls(args: Vec<String>) -> Vec<String> {
    let mut out = Vec::new();
    for arg in args {
        if let Some(url) = normalize_open_url(&arg) {
            out.push(url);
        }
    }
    out
}

#[cfg(target_os = "macos")]
fn normalize_urls(urls: Vec<url::Url>) -> Vec<String> {
    urls.into_iter()
        .filter_map(|url| normalize_open_url(url.as_str()))
        .collect()
}

fn normalize_open_url(input: &str) -> Option<String> {
    let parsed = if cfg!(windows) && input.len() >= 2 && input.chars().nth(1) == Some(':') {
        // Convert Windows paths like C:\foo.livemd to file://c:\foo.livemd
        let path = PathBuf::from(input);
        url::Url::from_file_path(path).ok()?
    } else {
        url::Url::parse(input).ok()?
    };

    match parsed.scheme() {
        "livebook" => Some(parsed.to_string()),
        "file" => {
            let path = parsed.to_file_path().ok()?;
            Some(format!("file://{}", path.display()))
        }
        _ => None,
    }
}

fn log_path() -> tauri::Result<PathBuf> {
    let path = if cfg!(target_os = "macos") {
        let home = home_dir().ok_or_else(|| tauri::Error::AssetNotFound("HOME".into()))?;
        home.join("Library").join("Logs").join("Livebook.log")
    } else if cfg!(target_os = "windows") {
        let base = if let Ok(value) = std::env::var("LOCALAPPDATA") {
            PathBuf::from(value)
        } else if let Some(home) = home_dir() {
            home.join("AppData").join("Local")
        } else {
            return Err(tauri::Error::AssetNotFound("LOCALAPPDATA".into()));
        };
        base.join("Livebook").join("Logs").join("Livebook.log")
    } else {
        let home = home_dir().ok_or_else(|| tauri::Error::AssetNotFound("HOME".into()))?;
        home.join(".local").join("share").join("Livebook").join("Logs").join("Livebook.log")
    };
    Ok(path)
}

fn home_dir() -> Option<PathBuf> {
    std::env::var_os("HOME")
        .or_else(|| std::env::var_os("USERPROFILE"))
        .map(PathBuf::from)
}

fn ensure_parent_dir(path: &Path) -> tauri::Result<()> {
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent)?;
    }
    Ok(())
}

fn init_tracing(
    log_path: &Path,
) -> Option<tracing_appender::non_blocking::WorkerGuard> {
    let dir = log_path.parent()?;
    let file = log_path.file_name()?;

    let file_appender = tracing_appender::rolling::never(dir, file);
    let (file_writer, guard) = tracing_appender::non_blocking(file_appender);

    let console_layer = tracing_subscriber::fmt::layer()
        .with_writer(std::io::stdout)
        .with_filter(LevelFilter::TRACE);
    let file_layer = tracing_subscriber::fmt::layer()
        .with_writer(file_writer)
        .with_ansi(false)
        .with_target(false)
        .with_level(true)
        .with_filter(LevelFilter::INFO);

    let _ = tracing_subscriber::registry()
        .with(console_layer)
        .with(file_layer)
        .try_init();

    Some(guard)
}

fn boot_script_label() -> &'static str {
    if cfg!(target_os = "windows") {
        "Open .livebookdesktop.bat"
    } else {
        "Open .livebookdesktop.sh"
    }
}

fn ensure_boot_script() -> tauri::Result<PathBuf> {
    let path = boot_script_path()?;
    if !path.exists() {
        let contents = boot_script_template();
        std::fs::write(&path, contents)?;
    }
    Ok(path)
}

fn boot_script_path() -> tauri::Result<PathBuf> {
    let home = home_dir().ok_or_else(|| tauri::Error::AssetNotFound("HOME".into()))?;
    if cfg!(target_os = "windows") {
        Ok(home.join(".livebookdesktop.bat"))
    } else {
        Ok(home.join(".livebookdesktop.sh"))
    }
}

fn boot_script_template() -> &'static str {
    if cfg!(target_os = "windows") {
        "@echo off\r\n\
rem This file is used to configure Livebook before booting.\r\n\
rem If you change this file, you must restart Livebook for your changes to take place.\r\n\
rem See https://hexdocs.pm/livebook/readme.html#environment-variables for all available environment variables.\r\n\
\r\n\
rem Allow Livebook to connect to remote machines over IPv6\r\n\
rem set ERL_AFLAGS=-proto_dist inet6_tcp\r\n\
\r\n\
rem Add directory to PATH\r\n\
rem set PATH=C:\\bin;%PATH%\r\n"
    } else {
        "# This file is used to configure Livebook before booting.\n\
# If you change this file, you must restart Livebook for your changes to take place.\n\
# See https://hexdocs.pm/livebook/readme.html#environment-variables for all available environment variables.\n\
\n\
# Allow Livebook to connect to remote machines over IPv6\n\
# export ERL_AFLAGS=\"-proto_dist inet6_tcp\"\n\
\n\
# Add Homebrew to PATH\n\
# export PATH=/opt/homebrew/bin:$PATH\n"
    }
}

async fn check_for_updates_on_boot(app: AppHandle) -> tauri_plugin_updater::Result<()> {
    if let Some(update) = app.updater()?.check().await? {
        let should_install = app
            .dialog()
            .message(format!(
                "Version {} is available!\n\nWould you like to download and install it now?",
                update.version
            ))
            .kind(MessageDialogKind::Info)
            .title("Update Available")
            .buttons(MessageDialogButtons::OkCancel)
            .blocking_show();

        if should_install {
            match update.download_and_install(|_, _| {}, || {}).await {
                Ok(()) => {
                    app.restart();
                }
                Err(e) => {
                    tracing::error!("Failed to install update: {}", e);
                    show_error_dialog(
                        &app,
                        "Update Failed",
                        format!("Failed to install update: {}", e),
                    );
                }
            }
        }
    }

    Ok(())
}

fn check_for_updates(app: AppHandle) {
    tauri::async_runtime::spawn(async move {
        let _ = check_for_updates_async(app).await;
    });
}

async fn check_for_updates_async(app: AppHandle) -> tauri_plugin_updater::Result<()> {
    if let Some(update) = app.updater()?.check().await? {
        let should_install = app
            .dialog()
            .message(format!(
                "Version {} is available!\n\nWould you like to download and install it now?",
                update.version
            ))
            .kind(MessageDialogKind::Info)
            .title("Update Available")
            .buttons(MessageDialogButtons::OkCancel)
            .blocking_show();

        if should_install {
            match update.download_and_install(|_, _| {}, || {}).await {
                Ok(()) => {
                    app.restart();
                }
                Err(e) => {
                    show_error_dialog(
                        &app,
                        "Update Failed",
                        format!("Failed to install update: {}", e),
                    );
                }
            }
        }
    } else {
        app.dialog()
            .message(format!(
                "You're running the latest version:\n\nv{}",
                app.package_info().version
            ))
            .kind(MessageDialogKind::Info)
            .title("No Updates Available")
            .blocking_show();
    }

    Ok(())
}

fn show_error_dialog(app: &AppHandle, title: impl Into<String>, message: impl Into<String>) {
    app.dialog()
        .message(message)
        .kind(MessageDialogKind::Error)
        .title(title)
        .blocking_show();
}
