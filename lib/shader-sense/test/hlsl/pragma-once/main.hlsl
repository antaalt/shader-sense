// Here we have two different path even if same file is targeted
// so DXC interpret them as different and pragma once is not working.
#include "include.hlsl"
#include "./include.hlsl"
[numthreads(1,1,1)]
void main() {
    shouldNotBeRedefined();
}