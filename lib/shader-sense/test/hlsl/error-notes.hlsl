
#include "inc0/level0.hlsl"

float g_float1;

cbuffer g_cbuffer {
    float4 cbuffer_float4;
    int3x4 cbuffer_int3x4;
}

tbuffer g_tbuffer {
    float tbuffer_float;
}

int loop_before_assignment() {
  // fxc warning X3554: unknown attribute loop, or attribute invalid for this statement
  [loop] // expected-warning {{attribute 'loop' can only be applied to 'for', 'while' and 'do' loop statements}} fxc-pass {{}}
  int val = 2;
  return val;
}

[numthreads(1, 1, 1)]
void cs_main(uint3 dtid : SV_DispatchThreadID) {
    tbuffer_float *= 2; // Error + note
    cbuffer_float4 += 52; // Error + note
}