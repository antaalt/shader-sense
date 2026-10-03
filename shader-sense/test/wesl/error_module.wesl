import package::broken::broken_value;

@fragment
fn fs_main() -> @location(0) vec4f {
    return vec4f(broken_value());
}
