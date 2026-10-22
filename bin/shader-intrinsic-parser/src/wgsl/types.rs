use std::collections::HashMap;

use scraper::{Html, Selector};
use shader_sense::symbols::{
    symbol_list::ShaderSymbolList,
    symbols::{
        ShaderSignature, ShaderSymbol, ShaderSymbolData, ShaderSymbolIntrinsic, ShaderSymbolMode,
    },
};

use super::{get_spec_link, get_table_rows, get_text, visit_spec, WgslIntrinsicParser};

/// Description & spec section of a predeclared type.
fn get_type_info(label: &str) -> (&'static str, &'static str) {
    match label {
        "bool" => ("Boolean type, with values true and false.", "bool-type"),
        "i32" => ("32-bit signed integer type.", "integer-types"),
        "u32" => ("32-bit unsigned integer type.", "integer-types"),
        "f32" => (
            "32-bit IEEE-754 floating point type.",
            "floating-point-types",
        ),
        "f16" => (
            "16-bit IEEE-754 floating point type. Requires `enable f16;`.",
            "floating-point-types",
        ),
        "sampler" => (
            "Sampler, mediating access to a sampled texture.",
            "sampler-type",
        ),
        "sampler_comparison" => (
            "Comparison sampler, mediating access to a depth texture.",
            "sampler-type",
        ),
        "texture_external" => (
            "External texture, an opaque 2D float-sampled texture.",
            "external-texture-type",
        ),
        "texture_multisampled_2d" => ("Multisampled texture.", "multisampled-texture-type"),
        "array" => (
            "Array type, fixed size `array<E, N>` or runtime sized `array<E>`.",
            "array-types",
        ),
        "atomic" => (
            "Atomic type `atomic<T>`, where T is i32 or u32.",
            "atomic-types",
        ),
        "ptr" => (
            "Pointer type `ptr<AS, T, AM>` in address space AS with access mode AM.",
            "ref-ptr-types",
        ),
        label if label.starts_with("vec") => ("Vector type `vecN<T>`.", "vector-types"),
        label if label.starts_with("mat") => (
            "Column-major matrix type `matCxR<T>`, with C columns and R rows.",
            "matrix-types",
        ),
        label if label.starts_with("texture_depth") => ("Depth texture.", "texture-depth"),
        label if label.starts_with("texture_storage") => (
            "Storage texture `texture_storage_*<Format, Access>`.",
            "texture-storage",
        ),
        label if label.starts_with("texture_") => (
            "Sampled texture `texture_*<T>`, where T is f32, i32 or u32.",
            "sampled-texture-type",
        ),
        _ => ("", "predeclared-types"),
    }
}

fn new_wgsl_type(
    label: &str,
    description: String,
    link: String,
    constructors: Option<&Vec<ShaderSignature>>,
) -> ShaderSymbol {
    ShaderSymbol {
        label: label.into(),
        requirement: None,
        data: ShaderSymbolData::Types {
            constructors: constructors.cloned().unwrap_or_default(),
        },
        mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(description, Some(link))),
    }
}

impl WgslIntrinsicParser {
    pub fn add_types(
        &self,
        symbols: &mut ShaderSymbolList,
        document: &Html,
        constructors: HashMap<String, Vec<ShaderSignature>>,
    ) {
        // Predeclared types are listed in a single section, with type generators in a table.
        let mut labels = Vec::new();
        let li_selector = Selector::parse("li").unwrap();
        visit_spec(document, |heading, element| {
            if heading
                .map(|heading| heading.id != "predeclared-types")
                .unwrap_or(true)
            {
                return;
            }
            match element.value().name() {
                "ul" => labels.extend(element.select(&li_selector).map(|li| get_text(&li))),
                "table" => labels.extend(
                    get_table_rows(&element)
                        .iter()
                        .skip(1) // Header
                        .filter_map(|cells| cells.first().map(|cell| get_text(cell))),
                ),
                _ => {}
            }
        });
        assert!(!labels.is_empty(), "Failed to find WGSL predeclared types.");
        for label in &labels {
            let (description, id) = get_type_info(label);
            symbols.types.push(new_wgsl_type(
                label,
                description.into(),
                get_spec_link(id),
                constructors.get(label),
            ));
        }
        // Predeclared aliases
        for size in 2..=4 {
            for (suffix, ty) in [("i", "i32"), ("u", "u32"), ("f", "f32"), ("h", "f16")] {
                let base = format!("vec{}", size);
                symbols.types.push(new_wgsl_type(
                    &format!("{}{}", base, suffix),
                    format!("Predeclared alias for `{}<{}>`.", base, ty),
                    get_spec_link("vector-types"),
                    constructors.get(&base),
                ));
            }
        }
        for columns in 2..=4 {
            for rows in 2..=4 {
                for (suffix, ty) in [("f", "f32"), ("h", "f16")] {
                    let base = format!("mat{}x{}", columns, rows);
                    symbols.types.push(new_wgsl_type(
                        &format!("{}{}", base, suffix),
                        format!("Predeclared alias for `{}<{}>`.", base, ty),
                        get_spec_link("matrix-types"),
                        constructors.get(&base),
                    ));
                }
            }
        }
    }
}
