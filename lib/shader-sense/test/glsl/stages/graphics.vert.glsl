#version 450

layout(location = 0) in vec3 inPos;
layout(location = 1) in vec4 inColor;
layout(location = 2) in vec3 inNormal;

layout(location = 0) out vec3 outWorldPos;
layout(location = 1) out vec3 outNormal;
layout(location = 2) out vec4 outColor;

void VSMain() {
    outWorldPos = inPos;
    gl_Position = vec4(inPos, 1.0);
    outColor = inColor;
    outNormal = inNormal;
}