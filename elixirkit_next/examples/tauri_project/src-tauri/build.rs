use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

fn main() {
    let tauri_dir =
        PathBuf::from(env::var("CARGO_MANIFEST_DIR").expect("missing CARGO_MANIFEST_DIR"));
    let app_dir = tauri_dir.join("..");
    let release_dir = tauri_dir.join("resources/rel");

    println!("cargo:rerun-if-env-changed=PROFILE");
    println!("cargo:rerun-if-changed=../mix.exs");
    println!("cargo:rerun-if-changed=../mix.lock");
    println!("cargo:rerun-if-changed=../config");
    println!("cargo:rerun-if-changed=../lib");
    println!("cargo:rerun-if-changed=../priv");
    println!("cargo:rerun-if-changed=../assets");

    fs::create_dir_all(&release_dir).expect("failed to create release directory");

    if env::var("PROFILE").as_deref() == Ok("release") {
        mix(&["setup"], &app_dir);
        mix(&["assets.deploy"], &app_dir);
        mix(&["release", "--overwrite", "--path", release_dir.to_str().unwrap()], &app_dir);
    }

    tauri_build::build()
}

fn mix(args: &[&str], dir: &Path) {
    let status = Command::new("mix")
        .current_dir(dir)
        .env("MIX_ENV", "prod")
        .args(args)
        .status()
        .unwrap_or_else(|e| panic!("mix {}: {e}", args.join(" ")));

    assert!(status.success(), "mix {} failed", args.join(" "));
}
