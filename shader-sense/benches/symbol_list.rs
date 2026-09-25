//! Benchmarks for the symbol list paths walked on every LSP request.
//!
//! `get_all_symbols` followed by a scope filter runs on each completion, hover,
//! signature & semantic token request, over the module symbols plus the whole
//! intrinsic database (~590 symbols for HLSL), so the allocation behaviour of
//! `filter` / `retain` dominates the request cost on large files.
use std::path::{Path, PathBuf};

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use shader_sense::{
    include::canonicalize,
    position::{ShaderFilePosition, ShaderPosition},
    shader::{
        HlslShadingLanguageTag, ShaderCompilationParams, ShaderParams, ShadingLanguage,
        ShadingLanguageTag,
    },
    symbols::{
        intrinsics::ShaderIntrinsics,
        shader_module::ShaderSymbols,
        shader_module_parser::ShaderModuleParser,
        symbol_provider::{default_include_callback, SymbolProvider},
    },
};

const SHADER_PATH: &str = "test/bench.hlsl";
const SHADER: &str = r#"
struct Material {
    float3 albedo;
    float roughness;
    float3 get_albedo() { return albedo; }
};
struct Frame {
    Material material;
    float4x4 view;
};
#if defined(UNDEFINED_MACRO)
float inactive_region_function(float x) { return x; }
static const float inactive_region_constant = 1.0;
#endif
cbuffer Constants { Frame frame; };
float4 shade(Material material, float3 light) {
    float3 albedo = material.get_albedo();
    return float4(albedo * light, material.roughness);
}
float4 main(float4 position : SV_Position) : SV_Target {
    Material material = frame.material;
    float3 light = float3(1.0, 1.0, 1.0);
    return shade(material, light) + material.albedo.x;
}
"#;

// Symbols hold a canonicalized path, and scope resolution compares paths verbatim,
// so every query has to use the same form or nothing local resolves.
fn shader_path() -> PathBuf {
    canonicalize(Path::new(SHADER_PATH)).unwrap()
}

fn query_symbols() -> ShaderSymbols {
    let shader_path = shader_path();
    let mut parser =
        ShaderModuleParser::from_shading_language(HlslShadingLanguageTag::get_language());
    let provider = SymbolProvider::from_shading_language(HlslShadingLanguageTag::get_language());
    let shader_module = parser.create_module(&shader_path, SHADER).unwrap();
    provider
        .query_symbols(
            &shader_module,
            ShaderParams::default(),
            &mut default_include_callback::<HlslShadingLanguageTag>,
            None,
        )
        .unwrap()
}

// Cursor inside main, on the `albedo` member of the `material.albedo.x` chain.
const CURSOR_WORD: &str = "material.albedo.x";
fn cursor_position() -> ShaderFilePosition {
    let offset = SHADER.find(CURSOR_WORD).unwrap() + "material.al".len();
    let position = ShaderPosition::from_byte_offset(SHADER, offset).unwrap();
    ShaderFilePosition::from(shader_path(), position)
}

fn bench_symbol_list(c: &mut Criterion) {
    let symbols = query_symbols();
    let intrinsics = ShaderIntrinsics::get(ShadingLanguage::Hlsl);
    let compilation_params = ShaderCompilationParams::default();
    let position = cursor_position();

    // `ShaderSymbolList::filter` over the whole HLSL intrinsic database.
    c.bench_function("intrinsics_filter", |b| {
        b.iter(|| black_box(intrinsics.get_intrinsics_symbol(&compilation_params)))
    });

    // Preprocessor filter + include tree walk, as done per request.
    c.bench_function("get_all_symbols", |b| {
        b.iter(|| black_box(symbols.get_all_symbols()))
    });

    // The completion pipeline: all symbols + intrinsics, then scope filtering.
    // Compare allocating a second list against filtering the first in place.
    let mut group = c.benchmark_group("scope_filter");
    group.bench_function("filter_scoped_symbol", |b| {
        b.iter(|| {
            let mut list = symbols.get_all_symbols();
            list.append(intrinsics.get_intrinsics_symbol(&compilation_params));
            black_box(list.filter_scoped_symbol(&position));
        })
    });
    group.bench_function("retain_scoped_symbol", |b| {
        b.iter(|| {
            let mut list = symbols.get_all_symbols();
            list.append(intrinsics.get_intrinsics_symbol(&compilation_params));
            list.retain_scoped_symbol(&position);
            black_box(list);
        })
    });
    group.finish();
}

fn bench_symbol_lookup(c: &mut Criterion) {
    let symbols = query_symbols();
    let intrinsics = ShaderIntrinsics::get(ShadingLanguage::Hlsl);
    let compilation_params = ShaderCompilationParams::default();
    let position = cursor_position();
    let mut symbol_list = symbols.get_all_symbols();
    symbol_list.append(intrinsics.get_intrinsics_symbol(&compilation_params));

    let parser_provider =
        SymbolProvider::from_shading_language(HlslShadingLanguageTag::get_language());
    let mut parser =
        ShaderModuleParser::from_shading_language(HlslShadingLanguageTag::get_language());
    let shader_module = parser.create_module(&shader_path(), SHADER).unwrap();
    let word = parser_provider
        .get_word_range_at_position(&shader_module, &position.position)
        .unwrap();
    assert!(
        word.is_field(),
        "Benchmark expects a chained field access, got {:?}",
        word
    );

    let resolved = word.find_symbol_from_parent(shader_path(), &symbol_list);
    assert_eq!(
        resolved
            .iter()
            .map(|s| s.label.as_str())
            .collect::<Vec<_>>(),
        ["albedo"],
        "Benchmark expects the field access to resolve to the struct member"
    );

    // Chained field resolution, which used to build a whole scoped list per lookup.
    c.bench_function("find_symbol_from_parent", |b| {
        b.iter(|| black_box(word.find_symbol_from_parent(shader_path(), &symbol_list)))
    });
}

criterion_group!(benches, bench_symbol_list, bench_symbol_lookup);
criterion_main!(benches);
