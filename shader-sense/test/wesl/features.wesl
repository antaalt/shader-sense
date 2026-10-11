// Valid only if debug feature is enabled, as main uses debug_only unconditionally.
@if(debug)
fn debug_only() -> f32 {
    return 1.0;
}

@if(textured) const quality = 2.0;
@else const quality = 0.0;

@fragment
fn main() -> @location(0) vec4f {
    return vec4f(debug_only() * quality);
}
