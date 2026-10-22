struct MSOutput {
    float4 position : SV_Position;
    float3 normal   : NORMAL;
    float2 uv       : TEXCOORD0;
};

struct AmplificationPayload
{
    uint meshletID;
};

struct Meshlet
{
    float3 positions[32];
    float3 normals[32];
    float2 uvs[32];
    uint indices[16];
};

StructuredBuffer<Meshlet> Meshlets;

[numthreads(8, 8, 1)]
[shader("amplification")]
void ASMain(in uint groupID : SV_GroupID)
{
    // Pass meshletID to mesh shader
    AmplificationPayload payload;
    payload.meshletID = groupID;
    DispatchMesh(1, 1, 1, payload);
}

[numthreads(8, 8, 1)]
[shader("mesh")]
[outputtopology("triangle")]
void MSMain(
    uint gtid : SV_GroupThreadID, 
    uint gid : SV_GroupID, 
    in payload AmplificationPayload payload,
    out indices uint3 indices[32],
    out vertices MSOutput vertex[64])
{
    Meshlet m = Meshlets.Load(gid); 
    SetMeshOutputCounts(3, 1); 

    [unroll]
    for (uint iVert = 0; iVert < 32; ++iVert)
    {
        vertex[iVert].position = float4(m.positions[iVert], 1.0f);
        vertex[iVert].normal = m.normals[iVert];
        vertex[iVert].uv = m.uvs[iVert];
    }
    for (uint iTri = 0; iTri < 16; ++iTri)
    {
        indices[iTri] = m.indices[iTri];
    }
}