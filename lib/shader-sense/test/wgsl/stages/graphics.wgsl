

@vertex
fn VSMain(@builtin(vertex_index) vertex_index: u32) -> @builtin(position) vec4<f32> {
    let x = f32(i32(vertex_index) - 1);
    let y = f32(i32(vertex_index & 1u) * 2);
    return vec4<f32>(x, y, 0.0, 1.0);
}

@fragment
fn PSMain() {
    
}