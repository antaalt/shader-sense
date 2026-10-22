@group(0) @binding(0)
var<storage, read_write> data: array<u32>;

@compute @workgroup_size(64)
fn CSMain(@builtin(global_invocation_id) global_id: vec3<u32>) {
    let idx = global_id.x;
    if (idx < arrayLength(&data)) {
        data[idx] = data[idx] + 1;
    }
}