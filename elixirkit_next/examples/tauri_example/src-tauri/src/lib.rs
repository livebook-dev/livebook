use elixirkit::MixTask;
use std::thread;
use tauri_plugin_dialog::DialogExt;

pub fn run() {
    tauri::Builder::default()
    .plugin(tauri_plugin_opener::init())
    .plugin(tauri_plugin_dialog::init())
    .setup(|app| {
        let elixir_dir = std::env::current_dir()?.join("..").join("src-elixir");
        let handle = app.handle().clone();

        thread::spawn(move || {
            let handle_clone = handle.clone();
            let exit_code = MixTask::start(elixir_dir, "run", &["--no-halt"], &[], move |cmd, (name, data)| {
                match name {
                    "ready" => {
                        cmd.send(("ping", "ping"));
                    }
                    "echo" => {
                        println!("[tauri] {data}");
                        let handle_dialog = handle_clone.clone();
                        handle_clone.dialog().message(format!("{data}")).show(move |_| {
                            handle_dialog.exit(0);
                        });
                    }
                    _ => panic!("unexpected event: {name}:{data}"),
                }
            });
            handle.exit(exit_code);
        });

        Ok(())
    })
    .build(tauri::generate_context!())
    .unwrap()
    .run(|_app_handle, _event| {});
}
