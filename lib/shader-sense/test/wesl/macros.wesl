// global variables and bindings...
@if(textured)
@group(0) @binding(0) var my_texture: texture_2d<f32>;

// module-scope declarations can be grouped in blocks.
// @if-decorated blocks do not introduce a scope.
@if(debug)
{
    const MAX_DEBUG_OUTPUT: u32 = 1024;
    const DEBUG_GROUP: u32 = 1;

    @group(DEBUG_GROUP) @binding(0)
    var<storage, read_write> debug_buffer: array<u32, MAX_DEBUG_OUTPUT>;
    @group(DEBUG_GROUP) @binding(1)
    var<storage, read_write> debug_buffer_counter: atomic<u32>;

    fn debug_write(debug_value: u32) {
      let index = min(atomicAdd(&debug_buffer_counter, 1), MAX_DEBUG_OUTPUT - 1);
      debug_buffer[index] = debug_value;
    }
}

// structs declarations and struct members...
struct Ray {
  position: vec4f,
  direction: vec4f,
  @if(debug_mode && raytracing_enabled)
  ray_steps: u32,
}

// function declarations, parameters and statements...
fn main() -> vec4f {
  @if(legacy_implementation || (is_web_version && xyz_not_supported))
  let result = legacy_impl();
  @if(!legacy_implementation && !(is_web_version && xyz_not_supported))
  let result = modern_impl();
}

// quirky example
// attribute order does not matter.
@compute @if(feature) fn main() { }
// ... is equivalent to 
@if(feature) @compute fn main() { }

// feature names live in their own namespace, i.e. they cannot shadow, or be shadowed by declarations.
const feature1 = 10;
@if(feature1) fn main() -> u32 { // 'feature1' in @if does not refer to the const-declaration.
    return feature1*2;           // 'feature1' in return statement does not refer to the feature flag.
}

// @elif & @else chains.
@if(textured) const quality = 2;
@elif(debug) const quality = 1;
@else const quality = 0;
