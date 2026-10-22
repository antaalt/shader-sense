
struct ShaderOutput {
    float4 position : SV_Position;
};

ShaderOutput vs_main(uint3 dtid : SV_DispatchThreadID) {
    ShaderOutput output = (ShaderOutput)0;
    return output;
}