fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=../../mix.exs");
    println!("cargo:rerun-if-changed=../../mix.lock");
    println!("cargo:rerun-if-changed=../../lib");
    println!("cargo:rerun-if-changed=../../config");

    // TODO:
    println!("cargo:rerun-if-changed=../../rel/app");
    // TODO:
    println!("cargo:rerun-if-changed=../../rel/app/standalone.exs");
    println!("cargo:rerun-if-changed=../../versions");

    tauri_build::build()
}
