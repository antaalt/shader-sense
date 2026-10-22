#include "./inc0/inc1/level1.hlsl"
#include "./inc0/level0.hlsl"

[numthreads(1,1,1)]
void compute() {
    float level = level0;
    float level0 = methodLevel0();
    float level1 = methodLevel1();
}