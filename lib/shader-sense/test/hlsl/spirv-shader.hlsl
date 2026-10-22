// Constant buffer
[[vk::binding(0, 0)]]
cbuffer Transform : register(b0)
{
    float4x4 model;
    float4x4 view;
    float4x4 proj;
};

// Texture and sampler with explicit binding
[[vk::binding(1, 0)]] Texture2D tex : register(t1);
[[vk::binding(2, 0)]] SamplerState samp : register(s1);

// Vertex Input Structure
struct VSInput {
    float3 pos : POSITION;
    float3 color : COLOR0;
};

// Vertex Output / Fragment Input
struct VSOutput {
    [[vk::location(0)]] float4 position : SV_POSITION;
    [[vk::location(1)]] float3 color : COLOR0;
};

// Vertex Shader
VSOutput mainVS(VSInput input)
{
    VSOutput output;
    float4 worldPos = mul(float4(input.pos, 1.0), model);
    float4 viewPos = mul(worldPos, view);
    output.position = mul(viewPos, proj);
    output.color = input.color;
    return output;
}

// Fragment Shader Output
[[vk::location(0)]] float4 mainPS([[vk::location(1)]] float3 color : COLOR0) : SV_Target
{
    return float4(color, 1.0);
}