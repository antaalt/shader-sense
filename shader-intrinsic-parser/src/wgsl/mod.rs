use scraper::{ElementRef, Html, Node, Selector};
use shader_sense::symbols::{
    symbol_list::ShaderSymbolList,
    symbols::{ShaderParameter, ShaderSignature},
};

use crate::common::{download_file, IntrinsicParser};

mod functions;
mod keywords;
mod types;

/// WGSL specification, which hold every builtin in a parsable format.
pub const SPEC_URL: &str = "https://www.w3.org/TR/WGSL/";
pub const SPEC_FILE: &str = "wgsl.html";
/// WESL specification, which only add keywords on top of WGSL.
pub const WESL_SPEC_URL: &str = "https://wesl-lang.dev/spec";

pub fn get_spec_link(id: &str) -> String {
    format!("{}#{}", SPEC_URL, id)
}

/// Merge all text of an element on a single line.
pub fn get_text(element: &ElementRef) -> String {
    element
        .text()
        .collect::<String>()
        .split_whitespace()
        .collect::<Vec<&str>>()
        .join(" ")
}

/// Convert an element to markdown, keeping line breaks & inline code as it is displayed as such.
pub fn get_markdown(element: &ElementRef) -> String {
    fn visit(node: ego_tree::NodeRef<Node>, markdown: &mut String) {
        match node.value() {
            // Escape '<' so that generics such as vecN<S> are not read as html.
            // Source line breaks are only formatting, real ones come from elements.
            Node::Text(text) => markdown.push_str(&text.replace('<', r"\<").replace('\n', " ")),
            Node::Element(element) => match element.name() {
                "br" => markdown.push('\n'),
                "code" => {
                    let code = ElementRef::wrap(node)
                        .map(|code| get_text(&code))
                        .unwrap_or_default();
                    markdown.push_str(&format!("`{}`", code));
                }
                "p" | "div" | "ul" | "ol" | "li" | "blockquote" | "tr" => {
                    markdown.push('\n');
                    if element.name() == "li" {
                        markdown.push_str("- ");
                    }
                    node.children().for_each(|child| visit(child, markdown));
                    markdown.push('\n');
                }
                _ => node.children().for_each(|child| visit(child, markdown)),
            },
            _ => {}
        }
    }
    let mut markdown = String::new();
    element
        .children()
        .for_each(|child| visit(child, &mut markdown));
    // Collapse whitespaces and use paragraphs for every line break.
    let mut lines: Vec<String> = Vec::new();
    for line in markdown.lines() {
        let line = line.split_whitespace().collect::<Vec<&str>>().join(" ");
        match lines.last_mut() {
            // List item content might be in a nested paragraph.
            Some(last) if last == "-" => *last = format!("- {}", line),
            _ if line.is_empty() => {}
            _ => lines.push(line),
        }
    }
    lines.retain(|line| line != "-");
    lines.join("\n\n")
}

pub fn get_child_elements<'a>(element: &ElementRef<'a>) -> Vec<ElementRef<'a>> {
    element.children().filter_map(ElementRef::wrap).collect()
}

pub fn has_class(element: &ElementRef, class: &str) -> bool {
    element.value().classes().any(|c| c == class)
}

/// Get the cells of each row of a table.
pub fn get_table_rows<'a>(table: &ElementRef<'a>) -> Vec<Vec<ElementRef<'a>>> {
    let row_selector = Selector::parse("tr").unwrap();
    let cell_selector = Selector::parse("td, th").unwrap();
    table
        .select(&row_selector)
        .map(|row| row.select(&cell_selector).collect())
        .collect()
}

/// A heading of the spec, delimiting a section.
pub struct SpecHeading {
    pub level: String, // Section number such as 17.5.1
    pub id: String,
}

/// The spec is flat, every heading & its content are siblings in main.
/// Iterate over them while keeping track of the current heading.
pub fn visit_spec<'a>(
    document: &'a Html,
    mut visitor: impl FnMut(Option<&SpecHeading>, ElementRef<'a>),
) {
    let main_selector = Selector::parse("main").unwrap();
    let main = document
        .select(&main_selector)
        .next()
        .expect("No main element in WGSL spec.");
    let mut heading = None;
    for element in get_child_elements(&main) {
        let name = element.value().name();
        if matches!(name, "h2" | "h3" | "h4" | "h5" | "h6") {
            heading = Some(SpecHeading {
                level: element.value().attr("data-level").unwrap_or("").into(),
                id: element.value().attr("id").unwrap_or("").into(),
            });
        }
        visitor(heading.as_ref(), element);
    }
}

/// A function declaration as written in the spec.
#[derive(Debug, Clone)]
pub struct SpecSignature {
    pub label: String,
    pub template: Option<String>,
    pub parameters: Vec<(String, String)>, // (label, type)
    pub return_type: String,
}

impl SpecSignature {
    pub fn into_signature(&self, description: String) -> ShaderSignature {
        ShaderSignature {
            returnType: self.return_type.clone(),
            description,
            parameters: self
                .parameters
                .iter()
                .map(|(label, ty)| ShaderParameter {
                    ty: ty.clone(),
                    label: label.clone(),
                    count: None,
                    description: "".into(),
                    range: None,
                })
                .collect(),
        }
    }
}

/// Find the end of a block starting with an opening character, handling nesting.
fn find_closing(text: &str, open: char, close: char) -> Option<usize> {
    let mut depth = 0;
    for (index, c) in text.char_indices() {
        if c == open {
            depth += 1;
        } else if c == close {
            depth -= 1;
            if depth == 0 {
                return Some(index);
            }
        }
    }
    None
}

/// Split at top level commas, ignoring the one in templates or parenthesis.
fn split_top_level(text: &str) -> Vec<&str> {
    let mut parts = Vec::new();
    let mut depth = 0;
    let mut start = 0;
    for (index, c) in text.char_indices() {
        match c {
            '<' | '(' => depth += 1,
            '>' | ')' => depth -= 1,
            ',' if depth == 0 => {
                parts.push(&text[start..index]);
                start = index + 1;
            }
            _ => {}
        }
    }
    parts.push(&text[start..]);
    parts
        .into_iter()
        .map(|part| part.trim())
        .filter(|part| !part.is_empty())
        .collect()
}

/// Parse every function declarations from a code block such as
/// `@const @must_use fn clamp(e: T, low: T, high: T) -> T`
pub fn parse_signatures(code: &str) -> Vec<SpecSignature> {
    let code = code.split_whitespace().collect::<Vec<&str>>().join(" ");
    let fn_regex = regex::Regex::new(r"\bfn ([A-Za-z_][A-Za-z0-9_]*)\s*").unwrap();
    let matches: Vec<_> = fn_regex.captures_iter(&code).collect();
    let mut signatures = Vec::new();
    for (index, capture) in matches.iter().enumerate() {
        let declaration_end = matches
            .get(index + 1)
            .map(|next| next.get(0).unwrap().start())
            .unwrap_or(code.len());
        let label = capture.get(1).unwrap().as_str().to_string();
        let mut rest = &code[capture.get(0).unwrap().end()..declaration_end];
        let template = if rest.starts_with('<') {
            let end = match find_closing(rest, '<', '>') {
                Some(end) => end,
                None => continue,
            };
            let template = rest[..=end].to_string();
            rest = rest[end + 1..].trim_start();
            Some(template)
        } else {
            None
        };
        if !rest.starts_with('(') {
            continue;
        }
        let parameters_end = match find_closing(rest, '(', ')') {
            Some(end) => end,
            None => continue,
        };
        let parameters = split_top_level(&rest[1..parameters_end])
            .into_iter()
            .map(|parameter| match parameter.split_once(':') {
                Some((label, ty)) => (label.trim().to_string(), ty.trim().to_string()),
                None => (parameter.to_string(), "".into()), // Variadic such as '...'
            })
            .collect();
        let rest = rest[parameters_end + 1..].trim_start();
        let return_type = match rest.strip_prefix("->") {
            // Next declaration attributes are part of this block, remove them.
            Some(return_type) => return_type
                .split(" @")
                .next()
                .unwrap()
                .trim()
                .trim_end_matches(';')
                .to_string(),
            None => "void".into(),
        };
        signatures.push(SpecSignature {
            label,
            template,
            parameters,
            return_type,
        });
    }
    signatures
}

pub struct WgslIntrinsicParser {}

impl IntrinsicParser for WgslIntrinsicParser {
    fn cache(&self, cache_path: &str) {
        std::fs::create_dir_all(cache_path).expect("Failed to create dir.");
        println!(
            "Caching file from {} to {}{}",
            SPEC_URL, cache_path, SPEC_FILE
        );
        let spec = download_file(SPEC_URL);
        std::fs::write(format!("{}{}", cache_path, SPEC_FILE), spec).expect("Failed to write file");
    }
    fn parse(&self, cache_path: &str) -> ShaderSymbolList {
        let spec = std::fs::read_to_string(format!("{}{}", cache_path, SPEC_FILE))
            .expect("Failed to read WGSL spec from cache.");
        let document = Html::parse_document(&spec);

        let mut symbols = ShaderSymbolList::default();
        let constructors = self.add_functions(&mut symbols, &document);
        self.add_types(&mut symbols, &document, constructors);
        self.add_keywords(&mut symbols, &document);
        symbols
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn get_markdown_ok() {
        let html = Html::parse_fragment(
            "<div>Result is\n<code>e</code> when <ul><li><p>vecN&lt;S></p></li></ul>a<br>b</div>",
        );
        let div = html
            .select(&Selector::parse("div").unwrap())
            .next()
            .unwrap();
        assert_eq!(
            get_markdown(&div),
            "Result is `e` when\n\n- vecN\\<S>\n\na\n\nb"
        );
    }

    #[test]
    fn parse_signatures_ok() {
        let signatures = parse_signatures(
            "@const @must_use fn mat2x2<T>(e : mat2x2<S>) -> mat2x2<T>\n\
             @const @must_use fn clamp(e: T,\n low: T, high: T) -> T",
        );
        assert_eq!(signatures.len(), 2);
        assert_eq!(signatures[0].label, "mat2x2");
        assert_eq!(signatures[0].template.as_deref(), Some("<T>"));
        assert_eq!(
            signatures[0].parameters,
            vec![("e".into(), "mat2x2<S>".into())]
        );
        assert_eq!(signatures[0].return_type, "mat2x2<T>");
        assert_eq!(signatures[1].label, "clamp");
        assert_eq!(signatures[1].parameters.len(), 3);
        assert_eq!(signatures[1].return_type, "T");
    }

    #[test]
    fn parse_signatures_nested_template() {
        let signatures =
            parse_signatures("fn atomicLoad(atomic_ptr: ptr<AS, atomic<T>, read_write>) -> T");
        assert_eq!(signatures.len(), 1);
        assert_eq!(
            signatures[0].parameters,
            vec![("atomic_ptr".into(), "ptr<AS, atomic<T>, read_write>".into())]
        );
        let signatures = parse_signatures("fn workgroupBarrier()");
        assert_eq!(signatures[0].return_type, "void");
        assert!(signatures[0].parameters.is_empty());
    }
}
