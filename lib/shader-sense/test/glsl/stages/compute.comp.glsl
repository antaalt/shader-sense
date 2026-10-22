#version 450

layout(local_size_x = 8, local_size_y = 8, local_size_z = 1) in;

layout(std430, binding = 0) buffer OutputBuffer {
    uint outputBuffer[];
};

void CSMain()
{
    uint index = gl_GlobalInvocationID.x + gl_GlobalInvocationID.y * 8;
    outputBuffer[index] = index;
}