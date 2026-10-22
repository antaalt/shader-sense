
#version 450

layout(quads, equal_spacing, ccw) in;

layout(location = 0) in vec3 te_position[];
layout(location = 1) in vec3 te_normal[];

layout(location = 0) out vec3 te_out_position;
layout(location = 1) out vec3 te_out_normal;

layout(binding = 0) uniform Camera {
    mat4 u_model;
    mat4 u_view;
    mat4 u_projection;
};

void TESMain() {
    // Barycentric coordinates for quad
    float u = gl_TessCoord.x;
    float v = gl_TessCoord.y;

    // Bilinear interpolation of positions and normals
    vec3 p0 = te_position[0];
    vec3 p1 = te_position[1];
    vec3 p2 = te_position[2];
    vec3 p3 = te_position[3];

    vec3 n0 = te_normal[0];
    vec3 n1 = te_normal[1];
    vec3 n2 = te_normal[2];
    vec3 n3 = te_normal[3];

    vec3 position = mix(mix(p0, p1, u), mix(p3, p2, u), v);
    vec3 normal = normalize(mix(mix(n0, n1, u), mix(n3, n2, u), v));

    te_out_position = position;
    te_out_normal = normal;

    gl_Position = u_projection * u_view * u_model * vec4(position, 1.0);
}