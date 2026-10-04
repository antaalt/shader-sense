const PI: f32 = 3.14159;

fn saturate_color(color: vec3f) -> vec3f {
    return clamp(color, vec3f(0.0), vec3f(1.0));
}
