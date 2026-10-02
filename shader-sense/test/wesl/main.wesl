import package::lighting::{ Light, shade };
import package::util::math::saturate_color;
import external::helpers::to_srgb;

@fragment
fn fs_main() -> @location(0) vec4f {
    let light = Light(vec3f(0.0, 1.0, 0.0), vec3f(1.0));
    let color = shade(light, vec3f(0.0, 0.0, 1.0));
    return vec4f(to_srgb(saturate_color(color)), 1.0);
}
