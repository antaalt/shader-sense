#version 460

#extension GL_EXT_mesh_shader: require

layout(local_size_x = 1, local_size_y = 1, local_size_z = 1) in;

layout(triangles, max_vertices = 3, max_primitives = 1) out;

layout(location = 0) in vec3 inPosition[3];
layout(location = 3) in vec4 inColor[3];

layout(location = 0) out VertexOutput {
	vec4 color;
} vertexOutput[];

void MSMain()
{
	SetMeshOutputsEXT(3, 1);
	vec4 offset = vec4(gl_GlobalInvocationID.xyz, 0);
    gl_MeshVerticesEXT[0].gl_Position = offset +  vec4(inPosition[0], 1.0);
    gl_MeshVerticesEXT[1].gl_Position = offset +  vec4(inPosition[1], 1.0);
    gl_MeshVerticesEXT[2].gl_Position = offset +  vec4(inPosition[2], 1.0);
    vertexOutput[0].color = inColor[0];
    vertexOutput[1].color = inColor[2];
    vertexOutput[2].color = inColor[2];
	gl_PrimitiveTriangleIndicesEXT[gl_LocalInvocationIndex] =  uvec3(0, 1, 2);

}