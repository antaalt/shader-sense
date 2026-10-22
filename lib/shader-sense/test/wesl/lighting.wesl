import super::util::math::PI;

struct Light {
    direction: vec3f,
    color: vec3f,
}

fn shade(light: Light, normal: vec3f) -> vec3f {
    return light.color * max(dot(normal, light.direction), 0.0) / PI;
}
