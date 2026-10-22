use std::collections::HashMap;

use scraper::{ElementRef, Html, Selector};
use shader_sense::symbols::{
    symbol_list::ShaderSymbolList,
    symbols::{
        ShaderSignature, ShaderSymbol, ShaderSymbolData, ShaderSymbolIntrinsic, ShaderSymbolMode,
    },
};

use super::{
    get_markdown, get_spec_link, get_table_rows, get_text, has_class, parse_signatures, visit_spec,
    SpecSignature, WgslIntrinsicParser,
};

/// Content of a single section of builtin functions.
#[derive(Default)]
struct BuiltinSection {
    id: String,
    is_constructor: bool,
    // Signature with its parameterization.
    overloads: Vec<(SpecSignature, String)>,
    // Description from builtin table if any.
    description: Option<String>,
    paragraphs: Vec<String>,
    parameter_descriptions: HashMap<String, String>,
}

impl BuiltinSection {
    fn visit(&mut self, element: ElementRef) {
        match element.value().name() {
            "table" if has_class(&element, "builtin") => self.visit_builtin_table(element),
            "table" => self.visit_data_table(element),
            "pre" => {
                for signature in parse_signatures(&element.text().collect::<String>()) {
                    self.overloads.push((signature, "".into()));
                }
            }
            "p" => {
                let text = get_markdown(&element);
                if !text.is_empty() && text != "Parameters:" {
                    self.paragraphs.push(text);
                }
            }
            _ => {}
        }
    }
    // Table with a row per field, such as Overload, Parameterization & Description.
    fn visit_builtin_table(&mut self, table: ElementRef) {
        let mut signatures = Vec::new();
        let mut conditions = Vec::new();
        for cells in get_table_rows(&table) {
            if cells.len() < 2 {
                continue;
            }
            match get_text(&cells[0]).as_str() {
                "Overload" | "Overloads" => {
                    signatures.extend(parse_signatures(&cells[1].text().collect::<String>()))
                }
                "Parameterization" | "Preconditions" => conditions.push(get_markdown(&cells[1])),
                "Description" => {
                    if self.description.is_none() {
                        self.description = Some(get_markdown(&cells[1]));
                    }
                }
                _ => {}
            }
        }
        let parameterization = conditions.join("\n\n");
        for signature in signatures {
            self.overloads.push((signature, parameterization.clone()));
        }
    }
    // Either a table of overloads with their parameterization, or a table of parameters.
    fn visit_data_table(&mut self, table: ElementRef) {
        let rows = get_table_rows(&table);
        let is_overload_table = rows
            .first()
            .map(|header| header.iter().any(|cell| get_text(cell) == "Overload"))
            .unwrap_or(false);
        let pre_selector = Selector::parse("pre").unwrap();
        let code_selector = Selector::parse("code").unwrap();
        for cells in rows.iter().skip(if is_overload_table { 1 } else { 0 }) {
            if cells.len() != 2 {
                continue;
            }
            if is_overload_table {
                let parameterization = get_markdown(&cells[0]);
                for pre in cells[1].select(&pre_selector) {
                    for signature in parse_signatures(&pre.text().collect::<String>()) {
                        self.overloads.push((signature, parameterization.clone()));
                    }
                }
            } else if let Some(code) = cells[0].select(&code_selector).next() {
                self.parameter_descriptions
                    .insert(get_text(&code), get_markdown(&cells[1]));
            }
        }
    }
    fn get_description(&self) -> String {
        self.description
            .clone()
            .unwrap_or_else(|| self.paragraphs.join("\n\n"))
    }
    fn get_signatures(&self) -> Vec<(String, ShaderSignature)> {
        self.overloads
            .iter()
            .map(|(signature, parameterization)| {
                let description = match &signature.template {
                    Some(template) if parameterization.is_empty() => {
                        format!("Template parameters: `{}`", template)
                    }
                    Some(template) => {
                        format!(
                            "Template parameters: `{}`\n\n{}",
                            template, parameterization
                        )
                    }
                    None => parameterization.clone(),
                };
                let mut shader_signature = signature.into_signature(description);
                for parameter in &mut shader_signature.parameters {
                    if let Some(description) = self.parameter_descriptions.get(&parameter.label) {
                        parameter.description = description.clone();
                    }
                }
                (signature.label.clone(), shader_signature)
            })
            .collect()
    }
}

impl WgslIntrinsicParser {
    /// Add all builtin functions of the spec, and return constructors signatures of types.
    pub fn add_functions(
        &self,
        symbols: &mut ShaderSymbolList,
        document: &Html,
    ) -> HashMap<String, Vec<ShaderSignature>> {
        // Gather builtin function sections first.
        let mut sections: Vec<BuiltinSection> = Vec::new();
        visit_spec(document, |heading, element| {
            let heading = match heading {
                Some(heading) if heading.level == "17" || heading.level.starts_with("17.") => {
                    heading
                }
                _ => return,
            };
            if sections
                .last()
                .map(|section| section.id != heading.id)
                .unwrap_or(true)
            {
                sections.push(BuiltinSection {
                    id: heading.id.clone(),
                    is_constructor: heading.level.starts_with("17.1."),
                    ..Default::default()
                });
            }
            sections.last_mut().unwrap().visit(element);
        });

        let mut constructors: HashMap<String, Vec<ShaderSignature>> = HashMap::new();
        let mut function_indices: HashMap<String, usize> = HashMap::new();
        for section in &sections {
            let description = section.get_description();
            let link = get_spec_link(&section.id);
            for (label, signature) in section.get_signatures() {
                if section.is_constructor {
                    constructors.entry(label).or_default().push(signature);
                } else if let Some(index) = function_indices.get(&label) {
                    if let ShaderSymbolData::Functions { signatures } =
                        &mut symbols.functions[*index].data
                    {
                        signatures.push(signature);
                    }
                } else {
                    function_indices.insert(label.clone(), symbols.functions.len());
                    symbols.functions.push(ShaderSymbol {
                        label,
                        requirement: None,
                        data: ShaderSymbolData::Functions {
                            signatures: vec![signature],
                        },
                        mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                            description.clone(),
                            Some(link.clone()),
                        )),
                    });
                }
            }
        }
        constructors
    }
}
