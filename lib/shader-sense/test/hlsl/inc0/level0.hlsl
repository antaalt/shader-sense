#ifndef __INCLUDE_LEVEL_0
#define __INCLUDE_LEVEL_0 1

#include "./inc1/level1.hlsl"

static const float level0 = level1 + 4.0;

float methodLevel0() {
    return methodLevel1();
}

#endif