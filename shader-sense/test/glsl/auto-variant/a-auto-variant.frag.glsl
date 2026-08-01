#version 450

#extension GL_GOOGLE_include_directive : require

#include "./workgroup-layout.glsl"

layout(location = 0) out vec4 outColor;

void main() {
    outColor = vec4(1.0);
}