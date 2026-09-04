struct VertexOutput {
    @builtin(position) position: vec4f,
};

@vertex
fn main(@builtin(vertex_index) vertexIndex: u32) -> VertexOutput {
    // Generate a triangle that covers the whole screen using vertex indices 0, 1, and 2
    var pos = array<vec2f, 3>(
        vec2f(-1.0, -3.0),
        vec2f( 3.0,  1.0),
        vec2f(-1.0,  1.0)
    );

    var output: VertexOutput;
    output.position = vec4f(pos[vertexIndex], 0.0, 1.0);
    return output;
}