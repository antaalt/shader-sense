use shader_sense::symbols::symbols::ShaderBuiltinSymbol;

use crate::common::{download_file, IntrinsicParser};
use std::collections::HashSet;

use regex::Regex;
use scraper::{Html, Selector};
use xmltree::XMLNode;

mod extensions;
mod keywords;
mod methods;
mod types;

pub fn get_links(url: &str) -> HashSet<String> {
    let resp = download_file(url);

    let mut unique_links = HashSet::new();
    let fragment = Html::parse_fragment(&resp);
    let selector = Selector::parse(".Level3 a").unwrap();
    let capi_regex = Regex::new(r"\bgl[A-Z]").expect("Failed to create regex");
    for element in fragment.select(&selector) {
        match element.attr("href") {
            Some(link) => {
                // Skip C api functions
                if capi_regex.find(&link).is_none() {
                    unique_links.insert(link.into());
                }
            }
            None => panic!("No elements found"),
        };
    }
    unique_links
}

pub fn get_childs(element: &xmltree::Element, name: &str) -> Vec<xmltree::Element> {
    let childs: Vec<xmltree::Element> = element
        .children
        .iter()
        .filter_map(|e| match e {
            XMLNode::Element(elem) => {
                if elem.name == name {
                    Some(elem)
                } else {
                    None
                }
            }
            _ => None,
        })
        .map(|f| f.clone())
        .collect();
    return childs;
}

pub fn merge_text(element: &xmltree::Element) -> String {
    let text_nodes: Vec<String> = element
        .children
        .iter()
        .filter_map(|node| match node {
            XMLNode::Element(elem) => Some(merge_text(&elem)),
            XMLNode::Text(elem) => Some(String::from(elem)),
            _ => None,
        })
        .map(|f| {
            f.clone().pop();
            f
        })
        .collect();
    if text_nodes.is_empty() {
        "".into()
    } else if text_nodes.len() == 1 {
        text_nodes[0].clone()
    } else {
        let mut full_text = String::new();
        for text in text_nodes {
            text.split_whitespace().for_each(|w| {
                if !full_text.is_empty() {
                    full_text.push(' ');
                }
                full_text.push_str(w);
            });
        }
        full_text
    }
}
pub struct GlslIntrinsicParser {}

impl IntrinsicParser for GlslIntrinsicParser {
    fn cache(&self, cache_path: &str) {
        // TODO: dont parse only gl4, missing gl2.1, es3 (3.2) / es3.1 / es3.0 / es2.0 / es1.1
        let unique_links =
            get_links("https://registry.khronos.org/OpenGL-Refpages/gl4/html/indexflat.php");
        std::fs::create_dir_all(cache_path).expect("Failed to create dir.");
        for link in unique_links {
            let filename = link.replace("xhtml", "xml");
            if filename == "removedTypes.xml" {
                continue; // Unvalid file.
            }
            let url = format!(
                "https://registry.khronos.org/OpenGL-Refpages/gl4/{}",
                filename
            );
            println!("Caching file from {} to {}{}", url, cache_path, filename);
            let mut asset = download_file(url.as_str());
            // Somehow these characters fail parsing
            asset = asset.replace("&plus;", "+");
            asset = asset.replace("&minus;", "-");
            asset = asset.replace("&dot;", ".");
            asset = asset.replace("&sdot;", "•");
            asset = asset.replace("&ge;", "&gt;="); // These dont seems to work fine... So hack
            asset = asset.replace("&le;", "&lt;="); // These dont seems to work fine... So hack
            asset = asset.replace("&af;", "()");
            asset = asset.replace("&delta;", "Δ");
            asset = asset.replace("&nbsp;", "");
            asset = asset.replace("&lambda;", "Λ");
            std::fs::write(format!("{}{}", cache_path, filename).as_str(), asset)
                .expect("Failed to write file");
        }
    }
    fn parse(&self, cache_path: &str) -> ShaderBuiltinSymbol {
        let mut symbols = ShaderBuiltinSymbol::default();

        self.add_methods(&mut symbols, cache_path);
        self.add_types(&mut symbols);
        self.add_keywords(&mut symbols);

        symbols
    }
}
