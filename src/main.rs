#[cfg(not(target_os = "wasi"))]
mod dxc;
mod naga;
mod glslang;
mod shader_error;
mod common;
mod server;

pub fn main() {
    
    let _res = std::panic::catch_unwind(|| {
        server::run();
    });
}
