@fragment
fn main() -> @location(0) vec4<f32> {
    let preamble = PreambleStruct(PREAMBLE_VALUE);
    return vec4<f32>(preamble.value, 0.0, 0.0, 1.0);
}
