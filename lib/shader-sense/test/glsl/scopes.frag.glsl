#version 450

#extension GL_GOOGLE_include_directive : require

#include "./inc0/inc1/level1.glsl"

const int scopeGlobal = 15;

void main() {
    int scopeRoot = 4;
    {
        int scope1 = 5;
        {
            int scope2 = 42;
            scope2;
        }
        scope1;
    }
    scopeRoot;
}