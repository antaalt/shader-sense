RWStructuredBuffer<uint> outputBuffer : register(u0);

[numthreads(8, 8, 1)]
void CSMain(uint3 dispatchThreadID : SV_DispatchThreadID)
{
    uint index = dispatchThreadID.x + dispatchThreadID.y * 8;
    outputBuffer[index] = index;
}