#version 450

#extension GL_GOOGLE_include_directive : require

// Invalid path but accessible via custom includes.
#include "inc1/level1.glsl"

void main() {
    level1;
}