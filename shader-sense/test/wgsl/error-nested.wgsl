const SCALE: f32 = 2.0;

fn compute(value: f32) -> f32 {
    var result = value;
    if (value > 0.0) {
        for (var i = 0; i < 4; i++) {
            result = result * SCALE;
        }
    } else {
        let invalid: u32 = result; // Type mismatch
        result = 0.0;
    }
    return result;
}

@fragment
fn fs_main() -> @location(0) vec4<f32> {
    return vec4<f32>(compute(1.0));
}
