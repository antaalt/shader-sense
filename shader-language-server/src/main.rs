use std::collections::HashSet;

use log::{error, info, warn};
use shader_language_server::server::{
    self,
    server_config::{ServerConfig, ServerSerializedConfig},
    Transport,
};
use shader_sense::shader::ShadingLanguage;

fn get_version() -> &'static str {
    static VERSION: &str = env!("CARGO_PKG_VERSION");
    return VERSION;
}

fn print_version() {
    println!("shader-language-server v{}", get_version());
}

fn run_server(
    config: ServerConfig,
    transport: Transport,
    shading_language: HashSet<ShadingLanguage>,
) {
    info!(
        "shader-language-server v{} ({}-{}-{})",
        get_version(),
        std::env::consts::ARCH,
        std::env::consts::OS,
        std::env::consts::FAMILY,
    );
    if let Ok(current_exe) = std::env::current_exe() {
        info!("Server running from {}", current_exe.display());
    }
    if let Ok(current_dir) = std::env::current_dir() {
        info!(
            "Server current working directory is {}",
            current_dir.display()
        );
    }
    server::run(config, transport, shading_language);
}

fn usage() {
    print_version();
    println!("Overview: This is a shader language server following lsp protocol.");
    println!("Usage: shader-language-server [OPTIONS]");
    println!();
    println!("Options:");
    println!("  --config                  Pass a custom config as a JSON string for server.");
    println!("  --config-file             Pass a custom config as a file for server.");
    println!("  --cwd                     Set current working directory of server. If not set, will be the server executable path.");
    println!("  --version | -v            Print server version.");
    println!("  --help | -h               Print this helper.");
    println!("Language:");
    println!("  By default, all of them are enabled. If you specify a single one, you will need to pick every language you need.");
    println!("  --hlsl                    Add support for hlsl language id.");
    println!("  --glsl                    Add support for glsl language id.");
    println!("  --wgsl                    Add support for wgsl language id.");
    println!("  --slang                   Add support for slang language id.");
    println!("Transport:");
    println!("  --stdio                   Use the stdio transport. Default transport.");
    println!("  --tcp                     Use tcp transport. Not implemented yet.");
    println!("  --memory                  Use memory transport. Not implemented yet.");
}

pub fn main() {
    env_logger::init();
    let mut args = std::env::args().into_iter();
    let _exe = args.next().unwrap();
    let mut transport = Transport::default();
    let mut config = ServerConfig::default();
    let mut shading_language = HashSet::new();
    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--version" => return print_version(),
            "-v" => return print_version(),
            "--help" => return usage(),
            "-h" => return usage(),
            "--config" => {
                if let Some(config_str) = args.next() {
                    match serde_json::from_str::<ServerSerializedConfig>(&config_str) {
                        Ok(config_parsed) => {
                            info!("Parsed config {:?}", config_parsed);
                            config = config_parsed.compute_engine_config();
                        }
                        Err(err) => {
                            error!("Failed to parse config {}: {}", config_str, err);
                            return usage();
                        }
                    }
                } else {
                    error!("Missing JSON for argument --config");
                    return usage();
                }
            }
            "--config-file" => {
                if let Some(config_file) = args.next() {
                    match std::fs::read_to_string(&config_file) {
                        Ok(config_str) => {
                            match serde_json::from_str::<ServerSerializedConfig>(&config_str) {
                                Ok(config_parsed) => {
                                    info!("Parsed config {:?}", config_parsed);
                                    config = config_parsed.compute_engine_config();
                                }
                                Err(err) => {
                                    error!("Failed to parse config file {}: {}", config_str, err);
                                    return usage();
                                }
                            }
                        }
                        Err(err) => {
                            error!("Failed to open config file {}: {}", config_file, err);
                            return usage();
                        }
                    }
                } else {
                    error!("Missing file path for argument --config-file");
                    return usage();
                }
            }
            "--wgsl" => {
                shading_language.insert(ShadingLanguage::Wgsl);
            }
            "--hlsl" => {
                shading_language.insert(ShadingLanguage::Hlsl);
            }
            "--glsl" => {
                shading_language.insert(ShadingLanguage::Glsl);
            }
            "--slang" => {
                shading_language.insert(ShadingLanguage::Slang);
            }
            "--stdio" => transport = Transport::Stdio,
            "--tcp" => transport = Transport::Tcp,
            "--memory" => transport = Transport::Memory,
            "--cwd" => {
                if let Some(cwd) = args.next() {
                    match std::env::set_current_dir(cwd) {
                        Ok(_) => {}
                        Err(err) => error!("Failed to set cwd: {}", err),
                    }
                } else {
                    error!("Missing directory for --cwd");
                    return usage();
                }
            }
            arg => {
                warn!("Argument {} unknown", arg);
            }
        }
    }
    // If no langugages specified, add them all for backward compatibilty
    if shading_language.is_empty() {
        shading_language.insert(ShadingLanguage::Glsl);
        shading_language.insert(ShadingLanguage::Hlsl);
        shading_language.insert(ShadingLanguage::Wgsl);
    }
    run_server(config, transport, shading_language);
}
