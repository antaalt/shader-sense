use scraper::{Html, Selector};
use shader_sense::symbols::{
    symbol_list::ShaderSymbolList,
    symbols::{ShaderSymbol, ShaderSymbolData, ShaderSymbolIntrinsic, ShaderSymbolMode},
};

use super::{HlslIntrinsicParser, SEMANTIC_FILE};

impl HlslIntrinsicParser {
    pub fn add_semantic(&self, symbols: &mut ShaderSymbolList, cache_path: &str) {
        let semantics_file = std::fs::read_to_string(format!("{}{}", cache_path, SEMANTIC_FILE))
            .expect("Failed to read file");
        {
            let semantics = Html::parse_document(&semantics_file);
            let content = Html::parse_fragment(
                &semantics
                    .select(&Selector::parse(".content").unwrap())
                    .next()
                    .unwrap()
                    .html(),
            );
            for table in content.select(&Selector::parse("table>tbody").unwrap()) {
                for tr_node in table.child_elements() {
                    if tr_node.child_elements().count() != 3 {
                        continue;
                    }
                    let label = tr_node
                        .child_elements()
                        .nth(0)
                        .map(|f| f.text().collect::<String>())
                        .unwrap()
                        .replace(", where 0 <= n <= 7", ""); // Clean
                    let description = tr_node
                        .child_elements()
                        .nth(1)
                        .map(|f| f.text().collect::<String>())
                        .unwrap();
                    let ty = tr_node
                        .child_elements()
                        .nth(2)
                        .map(|f| f.text().collect::<String>())
                        .unwrap();

                    println!("Reading semantic {}", label);

                    symbols.constants.push(ShaderSymbol {
                        label: label.into(),
                        mode: ShaderSymbolMode::Intrinsic(ShaderSymbolIntrinsic::new(
                            description.into(),
                            Some("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-semantics".into())
                        )),
                        requirement: None,
                        data: ShaderSymbolData::Variables { ty, count: None },
                    });
                }
            }
        }
    }
}
