use std::collections::HashSet;

use scraper::{Html, Selector};
use shader_sense::symbols::symbols::ShaderBuiltinSymbol;

use crate::common::{download_file, IntrinsicParser};

mod functions;
mod semantics;
mod types;

pub const SEMANTIC_FILE: &str = "semantics.html";

pub fn get_links(url: &str) -> HashSet<String> {
    let intrinsics_file = download_file(url);

    let mut unique_links = HashSet::new();

    let intrincics = Html::parse_document(&intrinsics_file);
    let table = Selector::parse(".content table").unwrap();
    let intrinsic_table = intrincics.select(&table).next().unwrap();
    let fragment = Html::parse_fragment(&intrinsic_table.html());
    let rows = Selector::parse("tr").unwrap();
    for row in fragment.select(&rows) {
        let label_node = row.child_elements().nth(0).unwrap();
        let link = match label_node.child_elements().nth(0) {
            Some(link) => link,
            None => continue,
        };
        //let label = label_node.text().collect::<Vec<_>>()[0];
        //let description = desc_node.text().collect::<Vec<_>>()[0];
        //let version = version_node.text().collect::<Vec<_>>()[0];
        let name = link.attr("href").unwrap();
        unique_links.insert(name.into());
    }
    unique_links
}

enum GenericTypeTemplate {
    Scalar,
    Vector,
    Matrix,
}
pub struct GenericType {
    ty: String,
    template: GenericTypeTemplate,
}

impl GenericType {
    pub fn scalar(ty: &str) -> Self {
        Self {
            ty: ty.into(),
            template: GenericTypeTemplate::Scalar,
        }
    }
    pub fn matrix(ty: &str) -> Self {
        Self {
            ty: ty.into(),
            template: GenericTypeTemplate::Matrix,
        }
    }
    pub fn vector(ty: &str) -> Self {
        Self {
            ty: ty.into(),
            template: GenericTypeTemplate::Vector,
        }
    }
    pub fn format(&self) -> String {
        match self.template {
            GenericTypeTemplate::Scalar => format!("{}", self.ty),
            GenericTypeTemplate::Vector => format!("{}n", self.ty),
            GenericTypeTemplate::Matrix => format!("{}nxn", self.ty),
        }
    }
    pub fn format_with_type(&self, ty: &str) -> String {
        match self.template {
            GenericTypeTemplate::Scalar => format!("{}", ty),
            GenericTypeTemplate::Vector => format!("{}n", ty),
            GenericTypeTemplate::Matrix => format!("{}nxn", ty),
        }
    }
    pub fn format_as_scalar(&self) -> String {
        self.ty.clone()
    }
}

pub fn type_size_iter(
    types: &[&str],
    scalar: bool,
    vector: bool,
    matrix: bool,
) -> Vec<GenericType> {
    let mut iter = Vec::new();
    if scalar {
        for ty in types {
            iter.push(GenericType::scalar(ty));
        }
    }
    if vector {
        for ty in types {
            iter.push(GenericType::vector(ty));
        }
    }
    if matrix {
        for ty in types {
            iter.push(GenericType::matrix(ty));
        }
    }
    iter
}

pub struct HlslIntrinsicParser {}

impl IntrinsicParser for HlslIntrinsicParser {
    fn cache(&self, cache_path: &str) {
        let unique_links = get_links("https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-intrinsic-functions");
        std::fs::create_dir_all(cache_path).expect("Failed to create dir.");
        for link in unique_links {
            let url = format!(
                "https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/{}",
                link
            );
            println!("Caching file from {} to {}{}", url, cache_path, link);
            let asset = download_file(url.as_str());
            std::fs::write(format!("{}{}", cache_path, link).as_str(), asset)
                .expect("Failed to write file");
        }
        let semantic_link = "https://learn.microsoft.com/en-us/windows/win32/direct3dhlsl/dx-graphics-hlsl-semantics";
        let content = download_file(&semantic_link);
        std::fs::write(format!("{}{}", cache_path, SEMANTIC_FILE).as_str(), content)
            .expect("Failed to write semantic file");
    }

    fn parse(&self, cache_path: &str) -> ShaderBuiltinSymbol {
        let mut symbols = ShaderBuiltinSymbol::default();
        // Doc is so bad its totally unscrappable. Do it manually.
        self.add_functions(&mut symbols);
        self.add_types(&mut symbols);
        self.add_semantic(&mut symbols, cache_path);

        symbols
    }
}
