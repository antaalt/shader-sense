use std::{borrow::Borrow, collections::HashMap};

use regex::Regex;
use shader_sense::symbols::symbols::{
    ShaderBuiltinSymbol, ShaderParameter, ShaderSignature, ShaderSymbol, ShaderSymbolData,
};
use xmltree::Element;

use crate::glsl::{get_childs, merge_text};

use super::GlslIntrinsicParser;

impl GlslIntrinsicParser {
    pub fn add_methods(&self, symbols: &mut ShaderBuiltinSymbol, cache_path: &str) {
        let paths = std::fs::read_dir(cache_path).expect("Failed to read dir");
        for path_dir in paths {
            let path = path_dir.expect("Failed to parse path").path();
            println!("Reading {}", path.display());

            let mut link_symbol = Vec::new();
            let filename = path
                .file_name()
                .expect("Invalid filename")
                .to_string_lossy()
                .to_string();
            let link = format!(
                "https://registry.khronos.org/OpenGL-Refpages/gl4/html/{}",
                filename.replace("xml", "xhtml")
            );
            let resp = std::fs::read_to_string(path).expect("Failed to read cached file");

            let elements = Element::parse(resp.as_bytes()).expect("Failed to parse xml");
            let is_function = elements
                .get_child("refsynopsisdiv")
                .unwrap()
                .get_child("funcsynopsis")
                .is_some();
            #[allow(unused_assignments)]
            let mut is_variable = false;
            if is_function {
                let refsynopsis = elements.get_child("refsynopsisdiv").unwrap();
                let synopsiss = get_childs(refsynopsis, "funcsynopsis");
                let mut signatures = HashMap::<String, Vec<ShaderSignature>>::new();
                for synopsis in synopsiss {
                    for prototype in get_childs(&synopsis, "funcprototype") {
                        let funcdef = prototype.get_child("funcdef").unwrap();
                        let paramdef = get_childs(&prototype, "paramdef");
                        let return_type = funcdef.get_text().unwrap();
                        let function_name =
                            funcdef.get_child("function").unwrap().get_text().unwrap();
                        let parameters = paramdef
                            .iter()
                            .filter_map(|p| {
                                let param_type = p.get_text().unwrap();
                                // Skip void parameter.
                                match p.get_child("parameter") {
                                    Some(value) => {
                                        let param_name = value.get_text().unwrap();
                                        Some(ShaderParameter {
                                            ty: param_type.trim().into(),
                                            label: param_name.trim().into(),
                                            count: None,
                                            description: "".into(),
                                        })
                                    }
                                    None => None,
                                }
                            })
                            .collect::<Vec<ShaderParameter>>();
                        let key: &str = function_name.trim();
                        match signatures.get_mut(key) {
                            Some(signature) => signature.push(ShaderSignature {
                                returnType: return_type.trim().into(),
                                description: "".into(),
                                parameters: parameters,
                            }),
                            None => {
                                signatures.insert(
                                    key.into(),
                                    vec![ShaderSignature {
                                        returnType: return_type.trim().into(),
                                        description: "".into(),
                                        parameters: parameters,
                                    }],
                                );
                            }
                        };
                    }
                }
                for signature in signatures {
                    // function
                    link_symbol.push(ShaderSymbol {
                        label: signature.0,
                        description: "".to_string(),
                        version: "".to_string(),
                        stages: Vec::new(),
                        link: Some(link.clone()),
                        data: ShaderSymbolData::Functions {
                            signatures: signature.1,
                        },
                        range: None,
                        content: None,
                    });
                }
            } else {
                // Variable
                let refsynopsisdiv = elements.get_child("refsynopsisdiv").unwrap();
                // Skip values with gl_PerVertex block
                let (var_name, ty, got_is_variable) = match refsynopsisdiv
                    .get_child("fieldsynopsis")
                {
                    Some(declaration) => {
                        let ty = declaration.get_child("type").unwrap().get_text().unwrap();
                        let modifier = declaration
                            .get_child("modifier")
                            .unwrap()
                            .get_text()
                            .unwrap();
                        let var_name = declaration
                            .get_child("varname")
                            .unwrap()
                            .get_text()
                            .unwrap();
                        is_variable = modifier.contains("out");
                        (var_name, ty, is_variable)
                    }
                    None => {
                        match refsynopsisdiv.children.iter().nth(2) {
                            Some(v) => {
                                let declaration =
                                    v.as_element().unwrap().get_child("fieldsynopsis").unwrap();
                                let ty = declaration.get_child("type").unwrap().get_text().unwrap();
                                let modifier = declaration
                                    .get_child("modifier")
                                    .unwrap()
                                    .get_text()
                                    .unwrap();
                                let var_name = declaration
                                    .get_child("varname")
                                    .unwrap()
                                    .get_text()
                                    .unwrap();
                                is_variable = modifier.contains("out");
                                (var_name, ty, is_variable)
                            }
                            None => {
                                let varname = elements
                                    .get_child("refnamediv")
                                    .unwrap()
                                    .get_child("refname")
                                    .unwrap()
                                    .get_text()
                                    .unwrap();

                                let ty = match varname.borrow() {
                                    "gl_PointSize" => "vec4",
                                    "gl_Position" => "vec4",
                                    _ => panic!("Variable {} type could not be parsed, need to be set manually", varname)
                                };
                                (varname, ty.into(), false)
                            }
                        }
                    }
                };
                is_variable = got_is_variable;

                link_symbol.push(ShaderSymbol {
                    label: var_name.trim().into(),
                    description: "".to_string(),
                    version: "".to_string(),
                    stages: Vec::new(),
                    link: Some(link.clone()),
                    data: ShaderSymbolData::Variables {
                        ty: ty.into(),
                        count: None,
                    },
                    range: None,
                    content: None,
                });
            }
            let refsect = get_childs(&elements, "refsect1");
            for refs in refsect {
                match refs.attributes["id"].as_str() {
                    "parameters" => {
                        // Could retrieve parameters info aswell.
                        let descs = get_childs(&refs, "varlistentry");
                        for _desc in descs {}
                    }
                    "description" => {
                        let desc_para = get_childs(&refs, "para");
                        let mut description = String::new();
                        desc_para.iter().for_each(|e| {
                            let desc = merge_text(&e);
                            description.push_str(desc.as_str());
                        });
                        // All symbols shares same description...
                        for symbol in &mut link_symbol {
                            symbol.description = description.clone();
                        }
                    }
                    "versions" => {
                        let version_nodes = get_childs(
                            refs.get_child("informaltable")
                                .unwrap()
                                .get_child("tgroup")
                                .unwrap()
                                .get_child("tbody")
                                .unwrap(),
                            "row",
                        );
                        for version_node in version_nodes {
                            // H@CKER
                            let mut version_key =
                                match version_node.get_child("entry").unwrap().get_text() {
                                    Some(value) => value.to_string(),
                                    None => version_node
                                        .get_child("entry")
                                        .unwrap()
                                        .get_child("varname")
                                        .unwrap()
                                        .get_text()
                                        .unwrap()
                                        .to_string(),
                                };
                            // Fix doc typos
                            version_key = match version_key.as_str() {
                                "bitfieldInsert" => {
                                    if filename == "bitfieldExtract.xml" {
                                        "bitfieldExtract".to_string()
                                    } else {
                                        version_key
                                    }
                                }
                                "floatBitsToUInt" => "floatBitsToUint".to_string(),
                                "interpolateAtoOffset" => "interpolateAtOffset".to_string(),
                                _ => version_key,
                            };
                            // TODO: handle {} correctly and , aswell
                            // TODO: handle variant version aswell...
                            let reg = Regex::new("\\(([a-zA-Z0-9\\s\\,\\{\\}]*)\\)")
                                .expect("failed to create regex");

                            let variants = reg
                                .captures_iter(&version_key)
                                .map(|f| f.get(1).unwrap().as_str().to_string())
                                .collect::<Vec<String>>();

                            for variant in &variants {
                                version_key =
                                    version_key.replace(format!("({})", variant).as_str(), "");
                            }
                            let keys: Vec<String> = version_key
                                .replace(",", "")
                                .split_whitespace()
                                .map(|w| String::from(w))
                                .collect();

                            let version_tag = version_node.get_child("include").unwrap();
                            let regex = Regex::new(r"@role='(\d+)'")
                                .expect("failed to create regex for version");
                            let version_parsed =
                                regex.captures(&version_tag.attributes["xpointer"]).unwrap();
                            let glsl_version = version_parsed
                                .get(1)
                                .map(|e| e.as_str())
                                .unwrap()
                                .parse::<u32>()
                                .unwrap()
                                * 10;
                            for key in keys {
                                let mut found = false;
                                for symbol in &mut link_symbol {
                                    let compare_symbol = symbol
                                        .label
                                        .replace("[]", "")
                                        .replace("[2]", "")
                                        .replace("[4]", "");
                                    if compare_symbol == key {
                                        found = true;
                                        symbol.version = glsl_version.to_string();
                                    }
                                }
                                if !found {
                                    panic!(
                                        "Could not set version for {:?} :\n{:#?}",
                                        key, link_symbol
                                    )
                                }
                            }
                        }
                    }
                    _ => {} // seealso & Copyright ignored
                }
            }
            // TODO: clean genType / genDType / genIType / genUType / genBType / mat / dmat -> type, vec2, vec3, vec4
            // TODO: parse description latex aswell...
            // TODO: get parameters description
            // TODO: get variant version correctly
            // TODO: retrieve vk extensions (might need to be manual...).
            // TODO: rgba32f types from imageLoad

            // TODO: retrieve stage aswell. might need to do this manually, with a list of all func for all stages.
            for symbol in link_symbol {
                symbols.push(symbol);
            }
        }
    }
}
