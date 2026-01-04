//! Command line interface to validate and inspect shaders using [`shader-sense`].
//!
//! It can be launched using the following options:
//! ```bash
//! --hlsl                    Use HLSL shading language (default)
//! --glsl                    Use GLSL shading language
//! --wgsl                    Use WGSL shading language
//! -D, --define <DEF>        Define a macro
//! -I, --include <PATH>      Add an include directory
//! -E, --entry-point <NAME>  Specify the shader entry point
//! -S, --stage <STAGE>       Specify shader stage (vertex, fragment, compute, mesh, task, control, evaluation, geometry)
//! --validate                Validate the shader
//! --functions               List functions
//! --includes                List includes
//! --macros                  List macros
//! --variables               List variables
//! --constants               List constants
//! --keywords                List keywords
//! --types                   List types
//! --version, -v             Print version information
//! --help, -h                Print this message
//! ```
//!
//! Example:
//! ```cli
//! shader-sense-cli --hlsl -E main -S vertex shader.hlsl
//! ```
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    path::{Path, PathBuf},
    rc::Rc,
};

use colored::Colorize;
use shader_sense::{
    shader::{
        GlslCompilationParams, GlslSpirvVersion, GlslTargetClient, HlslCompilationParams,
        HlslShaderModel, HlslVersion, ShaderCompilationParams, ShaderContextParams, ShaderParams,
        ShaderStage, ShadingLanguage, WgslCompilationParams,
    },
    shader_error::ShaderDiagnosticSeverity,
    symbols::{
        shader_module_parser::ShaderModuleParser,
        symbol_provider::SymbolProvider,
        symbols::{ShaderSymbolMode, ShaderSymbolType},
    },
    validator::validator::Validator,
};

fn get_version() -> &'static str {
    static VERSION: &str = env!("CARGO_PKG_VERSION");
    return VERSION;
}

fn print_version() {
    println!("shader-sense-cli v{}", get_version());
}

pub fn usage() {
    print_version();
    println!("Overview: Command line to validate shaders & inspect symbols.");
    println!("Usage: shader-sense-cli [OPTIONS] <FILE>");
    println!();
    println!("Options:");
    println!("  --hlsl                    Use HLSL shading language (default)");
    println!("  --glsl                    Use GLSL shading language");
    println!("  --wgsl                    Use WGSL shading language");
    println!("  -D, --define <DEF>        Define a macro");
    println!("  -I, --include <PATH>      Add an include directory");
    println!("  -E, --entry-point <NAME>  Specify the shader entry point");
    println!("  -S, --stage <STAGE>       Specify shader stage (vertex, fragment, compute, mesh, task, control, evaluation, geometry)");
    println!("  --validate                Validate the shader");
    println!("  --functions               List functions");
    println!("  --includes                List includes");
    println!("  --macros                  List macros");
    println!("  --variables               List variables");
    println!("  --constants               List constants");
    println!("  --keywords                List keywords");
    println!("  --types                   List types");
    println!("  --version, -v             Print version information");
    println!("  --help, -h                Print this message");
    println!();
    println!("Example:");
    println!("  shader-sense-cli --hlsl -E main -S vertex shader.hlsl");
}

pub fn main() {
    let mut args = std::env::args().into_iter();

    let mut file_name: Option<String> = None;
    let mut should_validate = false;
    let mut symbol_type_to_print: HashSet<ShaderSymbolType> = HashSet::new();
    let mut shading_language = ShadingLanguage::Hlsl;
    let mut preamble: Option<String> = None;
    let mut defines = Vec::new();
    let mut includes = Vec::new();
    let mut entry_point = None;
    let mut shader_stage = None;
    let _exe = args.next().unwrap();
    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--hlsl" => {
                shading_language = ShadingLanguage::Hlsl;
            }
            "--glsl" => {
                shading_language = ShadingLanguage::Glsl;
            }
            "--wgsl" => {
                shading_language = ShadingLanguage::Wgsl;
            }
            "-D" | "--define" => match args.next() {
                Some(define) => defines.push(define),
                None => {
                    println!("Missing define value");
                    usage();
                }
            },
            "-I" | "--include" => match args.next() {
                Some(include) => includes.push(include),
                None => {
                    println!("Missing include value");
                    usage();
                }
            },
            "-E" | "--entry-point" => match args.next() {
                Some(value_entry_point) => entry_point = Some(value_entry_point),
                None => {
                    println!("Missing entry point value");
                    usage();
                }
            },
            "-S" | "--stage" => match args.next() {
                Some(stage) => match stage.as_str() {
                    "vertex" => shader_stage = Some(ShaderStage::Vertex),
                    "fragment" | "pixel" => shader_stage = Some(ShaderStage::Fragment),
                    "compute" => shader_stage = Some(ShaderStage::Compute),
                    "mesh" => shader_stage = Some(ShaderStage::Mesh),
                    "task" | "amplification" => shader_stage = Some(ShaderStage::Task),
                    "control" | "hull" => shader_stage = Some(ShaderStage::TesselationControl),
                    "evaluation" | "domain" => {
                        shader_stage = Some(ShaderStage::TesselationEvaluation)
                    }
                    "geometry" => shader_stage = Some(ShaderStage::Geometry),
                    stage => println!("Unknown shader stage {}", stage),
                },
                None => {
                    println!("Missing stage value");
                    usage();
                }
            },
            "-P" | "--preamble" => match args.next() {
                Some(arg) => preamble = Some(arg),
                None => {
                    println!("Missing preamble value");
                    usage();
                }
            },
            "--validate" => {
                should_validate = true;
            }
            "--functions" => {
                symbol_type_to_print.insert(ShaderSymbolType::Functions);
            }
            "--includes" => {
                symbol_type_to_print.insert(ShaderSymbolType::Include);
            }
            "--macros" => {
                symbol_type_to_print.insert(ShaderSymbolType::Macros);
            }
            "--variables" => {
                symbol_type_to_print.insert(ShaderSymbolType::Variables);
            }
            "--constants" => {
                symbol_type_to_print.insert(ShaderSymbolType::Constants);
            }
            "--keywords" => {
                symbol_type_to_print.insert(ShaderSymbolType::Keyword);
            }
            "--types" => {
                symbol_type_to_print.insert(ShaderSymbolType::Types);
            }
            "--version" | "-v" => {
                print_version();
            }
            "--help" | "-h" => {
                usage();
            }
            parsed_file_name => match &mut file_name {
                Some(_) => usage(),
                None => {
                    file_name = Some(parsed_file_name.into());
                }
            },
        }
    }
    match file_name {
        Some(file_name) => {
            let shader_params = ShaderParams {
                context: ShaderContextParams {
                    includes: includes.into_iter().map(|i| PathBuf::from(i)).collect(),
                    defines: defines.into_iter().map(|d| (d, "1".to_owned())).collect(),
                    path_remapping: HashMap::new(),
                },
                compilation: ShaderCompilationParams {
                    entry_point: entry_point,
                    shader_stage: shader_stage,
                    hlsl: HlslCompilationParams {
                        shader_model: HlslShaderModel::ShaderModel6_8,
                        version: HlslVersion::V2018,
                        enable16bit_types: false,
                        spirv: false,
                    },
                    glsl: GlslCompilationParams {
                        client: GlslTargetClient::Vulkan1_3,
                        spirv: GlslSpirvVersion::SPIRV1_6,
                        preamble: preamble.unwrap_or_default(),
                    },
                    wgsl: WgslCompilationParams {},
                },
            };
            let shader_path = Path::new(&file_name);
            let shader_content = std::fs::read_to_string(shader_path).unwrap();
            // By default validate (if we dont parse symbols)
            if should_validate || symbol_type_to_print.is_empty() {
                // Validator intended to validate a file using standard API.
                let validator = Validator::from_shading_language(shading_language);
                match validator.validate_shader(
                    &shader_content,
                    shader_path,
                    &shader_params,
                    &mut |path: &Path| Some(std::fs::read_to_string(path).unwrap()),
                ) {
                    Ok(diagnostic_list) => {
                        if diagnostic_list.is_empty() {
                            println!(
                                "{}",
                                "✅ Success ! Shader validation found no error.".green()
                            );
                        } else {
                            // Pretty print errors
                            for diagnostic in diagnostic_list.diagnostics {
                                let filename = diagnostic.range.file_path.file_name().unwrap();
                                let formatted_path = format!(
                                    "{}:{}:{}",
                                    filename.display(),
                                    diagnostic.range.start().line,
                                    diagnostic.range.start().pos
                                );
                                let header = match diagnostic.severity {
                                    ShaderDiagnosticSeverity::Error => {
                                        format!("❌ Error at {}", formatted_path).red().bold()
                                    }
                                    ShaderDiagnosticSeverity::Warning => {
                                        format!("⚠️  Warning at {}", formatted_path).yellow().bold()
                                    }
                                    ShaderDiagnosticSeverity::Information => {
                                        format!("ℹ️️  Information at {}", formatted_path)
                                            .blue()
                                            .bold()
                                    }
                                    ShaderDiagnosticSeverity::Hint => {
                                        format!("💡 Hint at {}", formatted_path).blue().bold()
                                    }
                                };
                                println!("{}\n{}", header, diagnostic.error.italic());
                            }
                        }
                    }
                    Err(err) => println!("Failed to validate file: {:#?}", err),
                }
            }
            if !symbol_type_to_print.is_empty() {
                // SymbolProvider intended to gather file symbol at runtime by inspecting the AST.
                let mut shader_module_parser =
                    ShaderModuleParser::from_shading_language(shading_language);
                let symbol_provider = SymbolProvider::from_shading_language(shading_language);
                match shader_module_parser.create_module(shader_path, &shader_content) {
                    Ok(shader_module) => {
                        let symbols = symbol_provider
                            .query_symbols(
                                &shader_module,
                                shader_params,
                                &mut |include| {
                                    let include_module = shader_module_parser.create_module(
                                        &include.get_absolute_path(),
                                        std::fs::read_to_string(&include.get_absolute_path())
                                            .unwrap()
                                            .as_str(),
                                    )?;
                                    Ok(Some(Rc::new(RefCell::new(include_module))))
                                },
                                None,
                            )
                            .unwrap();
                        let symbol_list = symbols.get_all_symbols();
                        let mut found_some_symbols = false;
                        for symbol in symbol_list.iter() {
                            let header = match &symbol.mode {
                                ShaderSymbolMode::Runtime(runtime) => format!(
                                    "{}:{}:{}",
                                    runtime.file_path.file_name().unwrap().display(),
                                    runtime.range.start.line,
                                    runtime.range.start.pos
                                ),
                                _ => symbol.format(),
                            };
                            let icon = match &symbol.get_type() {
                                Some(ty) => {
                                    if !symbol_type_to_print.contains(ty) {
                                        continue;
                                    }
                                    match ty {
                                        ShaderSymbolType::Types => {
                                            format!("{} {}", "{}".white().bold(), "Type").yellow()
                                        }
                                        ShaderSymbolType::Constants => "♾️ Constant".yellow(),
                                        ShaderSymbolType::Functions => "⚙️  Function".yellow(),
                                        ShaderSymbolType::Keyword => {
                                            format!("{} {}", "</>".white().bold(), "Keyword")
                                                .yellow()
                                        }
                                        ShaderSymbolType::Variables => "🔡 Variable".yellow(),
                                        ShaderSymbolType::CallExpression => continue,
                                        ShaderSymbolType::Include => "🔗 Include".yellow(),
                                        ShaderSymbolType::Macros => "✏️  Macro".yellow(),
                                    }
                                }
                                None => continue,
                            };
                            found_some_symbols = true;
                            println!("{} {} {}", icon, header.blue(), symbol.format().italic());
                        }
                        if !found_some_symbols {
                            fn get_type_string(ty: &ShaderSymbolType) -> &'static str {
                                match ty {
                                    ShaderSymbolType::Types => "types",
                                    ShaderSymbolType::Constants => "constants",
                                    ShaderSymbolType::Variables => "variables",
                                    ShaderSymbolType::CallExpression => "callExpression",
                                    ShaderSymbolType::Functions => "functions",
                                    ShaderSymbolType::Keyword => "keyword",
                                    ShaderSymbolType::Macros => "macros",
                                    ShaderSymbolType::Include => "include",
                                }
                            }
                            println!(
                                "{}",
                                format!(
                                    "⚠️  Couldn't find any symbol of type {}",
                                    symbol_type_to_print
                                        .iter()
                                        .map(|t| get_type_string(t))
                                        .collect::<Vec<&str>>()
                                        .join(", ")
                                )
                                .yellow()
                            )
                        }
                    }
                    Err(err) => println!("Failed to create ast: {:#?}", err),
                }
            }
        }
        None => {
            println!("Missing a filename.");
            usage();
        }
    }
}
