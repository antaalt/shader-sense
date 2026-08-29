use log::{error, warn};
use shader_renderer::server;

use crate::server::{ServerConfig, ServerTransport};

fn get_version() -> &'static str {
    static VERSION: &str = env!("CARGO_PKG_VERSION");
    return VERSION;
}

fn print_version() {
    println!("shader-renderer v{}", get_version());
}

fn usage() {
    print_version();
    println!("This is a headless shader renderer using JSON RPC for communication.");
    println!("Usage: shader-renderer [OPTIONS] [TRANSPORT]");
    println!();
    println!("Options:");
    println!("  --version | -v            Print server version.");
    println!("  --help | -h               Print this helper.");
    println!("  --width                   Initial width of renderer (default to 1280).");
    println!("  --height                  Initial height of renderer (default to 720).");
    println!("Transport:");
    println!("  --stdio                   Use the stdio transport. Default transport.");
    println!("  --tcp-listen <IP addr>    Listen for a connection on given address and port.");
    println!("  --tcp-connect <IP addr>   Connect to a tcp stream at given address and port.");
}

fn main() {
    env_logger::init();
    let mut args = std::env::args().into_iter();
    let _exe = args.next().unwrap();
    let mut config = ServerConfig::default();
    let mut transport = ServerTransport::default();

    // TODO: args
    // --hlsl, --wgsl, --glsl (or --spirv, --dxil --wgsl to select correct backend).
    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--version" | "-v" => return print_version(),
            "--help" | "-h" => return usage(),
            "--width" => {
                if let Some(width) = args.next() {
                    if let Ok(width) = width.parse() {
                        config.width = width;
                    } else {
                        error!("Missing width for argument --width");
                        return usage();
                    }
                } else {
                    error!("Missing width for argument --width");
                    return usage();
                }
            }
            "--height" => {
                if let Some(height) = args.next() {
                    if let Ok(height) = height.parse() {
                        config.height = height;
                    } else {
                        error!("Missing height for argument --height");
                        return usage();
                    }
                } else {
                    error!("Missing height for argument --height");
                    return usage();
                }
            }
            "--stdio" => transport = ServerTransport::Stdio,
            "--tcp-listen" => {
                if let Some(address) = args.next() {
                    if let Ok(address) = address.parse() {
                        transport = ServerTransport::TcpListen(address);
                    } else {
                        error!("Failed to parse IP for --tcp-listen: {}", address);
                        return usage();
                    }
                } else {
                    error!("Missing file path for argument --tcp-listen");
                    return usage();
                }
            }
            "--tcp-connect" => {
                if let Some(address) = args.next() {
                    if let Ok(address) = address.parse() {
                        transport = ServerTransport::TcpConnect(address);
                    } else {
                        error!("Failed to parse IP for --tcp-connect: {}", address);
                        return usage();
                    }
                } else {
                    error!("Missing file path for argument --tcp-connect");
                    return usage();
                }
            }
            arg => {
                warn!("Argument {} unknown", arg);
            }
        }
    }

    let mut server = match server::Server::new(config, transport) {
        Ok(server) => server,
        Err(err) => {
            error!("Failed to init server: {}", err.to_string());
            return;
        }
    };
    match server.run() {
        Ok(_) => {}
        Err(err) => {
            error!("Failed to run server: {}", err.to_string());
            return;
        }
    }
}
