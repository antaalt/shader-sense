struct VS_INPUT {
    float3 pos : POSITION;
    float4 color : COLOR;
    float3 normal : NORMAL;
};

struct VS_OUTPUT {
    float4 pos : SV_POSITION;
    float3 worldPos : POSITION;
    float3 normal : NORMAL;
    float4 color : COLOR;
};

// Vertex shader
VS_OUTPUT VSMain(VS_INPUT input) {
    VS_OUTPUT output;
    output.worldPos = input.pos;
    output.pos = float4(input.pos, 1.0);
    output.color = input.color;
    output.normal = input.normal;
    return output;
}


struct HS_CONTROL_POINT
{
    float3 pos : POSITION;
    float3 normal   : NORMAL;
};

struct HS_CONSTANT_DATA_OUTPUT
{
    float Edges[3] : SV_TessFactor;
    float Inside   : SV_InsideTessFactor;
};

[domain("tri")]
[partitioning("integer")]
[outputtopology("triangle_cw")]
[outputcontrolpoints(3)]
[patchconstantfunc("HSConstantFunction")]
HS_CONTROL_POINT HSMain(InputPatch<VS_OUTPUT, 3> patch, uint i : SV_OutputControlPointID)
{
    HS_CONTROL_POINT cp;
    cp.pos = patch[i].worldPos;
    cp.normal = patch[i].normal;
    return cp;
}

HS_CONSTANT_DATA_OUTPUT HSConstantFunction(InputPatch<VS_OUTPUT, 3> patch)
{
    HS_CONSTANT_DATA_OUTPUT output;
    output.Edges[0] = 4;
    output.Edges[1] = 4;
    output.Edges[2] = 4;
    output.Inside   = 4;
    return output;
}

struct DS_OUTPUT
{
    float4 Position : SV_POSITION;
    float3 Normal   : NORMAL;
};

[domain("tri")]
DS_OUTPUT DSMain(HS_CONSTANT_DATA_OUTPUT input, const OutputPatch<HS_CONTROL_POINT, 3> patch, float3 bary : SV_DomainLocation)
{
    DS_OUTPUT output;
    output.Position = float4(
        patch[0].pos * bary.x +
        patch[1].pos * bary.y +
        patch[2].pos * bary.z, 1.0f
    );
    output.Normal = normalize(
        patch[0].normal * bary.x +
        patch[1].normal * bary.y +
        patch[2].normal * bary.z
    );
    return output;
}

[maxvertexcount(3)]
void GSMain(triangle DS_OUTPUT input[3], inout TriangleStream<DS_OUTPUT> triStream) {
    for (int i = 0; i < 3; ++i) {
        triStream.Append(input[i]);
    }
}

float4 PSMain(VS_OUTPUT input) : SV_TARGET {
    return input.color;
}