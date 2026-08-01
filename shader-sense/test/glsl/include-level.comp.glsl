#version 450

#extension GL_GOOGLE_include_directive : require

#include "./inc0/level0.glsl"

#ifdef GL_COMPUTE_SHADER
uint fibonacci(uint nthNumber) {
    int prevprevNumber, prevNumber = 0, currentNumber = 1;
    for (int i = 1; i < nthNumber ; i++) {
        prevprevNumber = prevNumber;
        prevNumber = currentNumber;
        currentNumber = prevprevNumber + prevNumber;
    }
    return currentNumber;
}

void compute() {
    Test test;
    float level = level0;
    //vec4 frags = gl_FragCoord; // Error
    uvec3 o = gl_GlobalInvocationID;
    uint i = gl_LocalInvocationIndex;
    uint root = fibonacci(1);
    uint level0 = fibonacciLevel0(2);
    uint level1 = fibonacciLevel1(3);
}
#endif