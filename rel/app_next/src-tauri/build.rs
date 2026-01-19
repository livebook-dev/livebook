fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=../../mix.exs");
    println!("cargo:rerun-if-changed=../../mix.lock");
    println!("cargo:rerun-if-changed=../../lib");
    println!("cargo:rerun-if-changed=../../config");
    println!("cargo:rerun-if-changed=../../versions");

    tauri_build::build()
}
