use std::collections::HashSet;

use scraper::{Html, Selector};
use shader_sense::symbols::{
    symbol_list::ShaderSymbolList,
    symbols::{ShaderSymbol, ShaderSymbolData, ShaderSymbolIntrinsic, ShaderSymbolMode},
};

use super::{
    get_spec_link, get_table_rows, get_text, visit_spec, WgslIntrinsicParser, WESL_SPEC_URL,
};

fn new_wgsl_keyword(label: &str, description: String, link: String) -> ShaderSymbol {
    ShaderSymbol {
        label: label.into(),
        requirement: None,
        data: ShaderSymbolData::Keyword {},
        mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(description, Some(link))),
    }
}

impl WgslIntrinsicParser {
    pub fn add_keywords(&self, symbols: &mut ShaderSymbolList, document: &Html) {
        let mut labels = HashSet::new();
        let mut push_keyword =
            |symbols: &mut ShaderSymbolList, label: &str, description: String, link: String| {
                if labels.insert(label.to_string()) {
                    symbols
                        .keywords
                        .push(new_wgsl_keyword(label, description, link));
                }
            };
        // Keywords
        let keyword_selector = Selector::parse(r#"dfn[data-dfn-for="syntax_kw"]"#).unwrap();
        for keyword in document.select(&keyword_selector) {
            push_keyword(
                symbols,
                &get_text(&keyword),
                "WGSL keyword.".into(),
                get_spec_link(keyword.value().attr("id").unwrap_or("keyword-summary")),
            );
        }
        // WESL keywords. See https://github.com/wgsl-tooling-wg/wesl-spec/blob/main/Imports.md
        for (label, description) in [
            (
                "import",
                "WESL keyword. Import declarations from another module.",
            ),
            (
                "package",
                "WESL keyword. Import path relative to the root of the current package.",
            ),
            (
                "super",
                "WESL keyword. Import path relative to the parent of the current module.",
            ),
            (
                "self",
                "WESL keyword. Refer to the current module in an import path.",
            ),
            ("as", "WESL keyword. Rename an imported item."),
            ("public", "WESL keyword."),
        ] {
            push_keyword(symbols, label, description.into(), WESL_SPEC_URL.into());
        }
        let code_selector = Selector::parse("code").unwrap();
        visit_spec(document, |heading, element| {
            let heading = match heading {
                Some(heading) => heading,
                None => return,
            };
            match (heading.id.as_str(), element.value().name()) {
                // Builtin values used in @builtin attribute.
                ("builtin-value-names", "ul") => {
                    for code in element.select(&code_selector) {
                        let label = get_text(&code).trim_matches('\'').to_string();
                        push_keyword(
                            symbols,
                            &label,
                            "Built-in value, used with the `@builtin` attribute.".into(),
                            get_spec_link("builtin-inputs-outputs"),
                        );
                    }
                }
                // Enumerants such as address spaces, access modes and texel formats.
                // Enumeration & extension cells span over multiple rows.
                ("predeclared-enumerants", "table") => {
                    let mut enumeration = String::new();
                    let mut remaining_rows = 0;
                    for cells in get_table_rows(&element).iter().skip(1) {
                        let enumerant_cell = if remaining_rows == 0 {
                            let enumeration_cell = match cells.first() {
                                Some(cell) => cell,
                                None => continue,
                            };
                            // Only take first text, skip notes.
                            enumeration = enumeration_cell
                                .text()
                                .map(|text| text.trim())
                                .find(|text| !text.is_empty())
                                .unwrap_or("")
                                .to_string();
                            remaining_rows = enumeration_cell
                                .value()
                                .attr("rowspan")
                                .and_then(|rowspan| rowspan.parse::<u32>().ok())
                                .unwrap_or(1);
                            cells.get(1)
                        } else {
                            cells.first()
                        };
                        remaining_rows -= 1;
                        if let Some(enumerant_cell) = enumerant_cell {
                            push_keyword(
                                symbols,
                                &get_text(enumerant_cell),
                                format!("Predeclared enumerant of {}.", enumeration),
                                get_spec_link("predeclared-enumerants"),
                            );
                        }
                    }
                }
                _ => {}
            }
        });
    }
}
