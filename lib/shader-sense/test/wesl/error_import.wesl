// Import a module that does not exist.
import package::missing::foo;

@fragment
fn fs_main() -> @location(0) vec4f {
    return vec4f(foo(), 1.0);
}
