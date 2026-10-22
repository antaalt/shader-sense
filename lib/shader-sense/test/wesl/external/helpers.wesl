public fn to_srgb(color: vec3f) -> vec3f {
    return pow(color, vec3f(1.0 / 2.2));
}
