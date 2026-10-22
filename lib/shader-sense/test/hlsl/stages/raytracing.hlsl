// Resources
RaytracingAccelerationStructure SceneBVH : register(t0);
RWTexture2D<float4> OutputTexture : register(u0);

// Ray payload structure
struct [raypayload] RayPayload
{
    float3 color: read(caller) : write(caller, closesthit, anyhit, miss);
    uint   hit: read(caller) : write(caller, closesthit, anyhit, miss);
};

// Ray generation shader
[shader("raygeneration")]
void RayGenMain()
{
    // Get dispatch index
    uint3 launchIndex = DispatchRaysIndex();
    uint3 launchDims  = DispatchRaysDimensions();

    // Compute normalized screen coordinates
    float2 uv = (float2(launchIndex.xy) + 0.5f) / float2(launchDims.xy);

    // Set up ray origin and direction (simple camera)
    float3 origin = float3(0.f, 0.f, -5.f);
    float3 dir = normalize(float3(uv * 2.f - 1.f, 1.f));

    // Initialize payload
    RayPayload payload;
    payload.color = float3(0.f, 0.f, 0.f);
    payload.hit = 0;
    
    RayDesc ray = {
        origin,
        0.f,
        dir,
        1000.f
    };

    // Trace ray
    TraceRay(
        SceneBVH,       // Acceleration structure
        RAY_FLAG_NONE,  // Ray flags
        0xFF,           // Instance mask
        0,              // Ray contribution to hit group index
        1,              // Multiplier for geometry contribution
        0,              // Miss shader index
        ray,            // Ray
        payload         // Payload
    );

    // Write result to output
    if (payload.hit) {
        OutputTexture[launchIndex.xy] = float4(payload.color, 1.f);
    } else {
        OutputTexture[launchIndex.xy] = float4(1.f, 0.f, 1.f, 1.f);
    }
}

// Closest hit shader
[shader("closesthit")]
void ClosestHitMain(inout RayPayload payload, in BuiltInTriangleIntersectionAttributes attr)
{
    // Could CallShader or TraceRay for multi bounce.
    payload.color = float3(1.f, 0.f, 0.f); // Red for hit
    payload.hit = 1;
}

// Any hit shader
[shader("anyhit")]
void AnyHitMain(inout RayPayload payload, in BuiltInTriangleIntersectionAttributes attr)
{
    payload.color = float3(1.f, 0.f, 0.f); // Red for hit
    payload.hit = 1;
}

// Miss shader
[shader("miss")]
void MissMain(inout RayPayload payload)
{
    // Use ray system values to compute contributions of background, sky, etc...
    // Combine contributions into ray payload
    //CallShader( ... ); // if desired
    //TraceRay( ... );   // if desired
    payload.color = float3(0.f, 0.f, 1.f); // Blue for miss
    payload.hit = 0;
}

[shader("callable")]
void callable_main(inout RayPayload params)
{
    // Perform some common operations and update params
    // CallShader(); // maybe
}

struct CustomPrimitiveDef {  };
struct MyAttributes {  };
struct CustomIntersectionIterator {};
void InitCustomIntersectionIterator(CustomIntersectionIterator it) {}
StructuredBuffer<CustomPrimitiveDef> CustomPrimitiveDefinitions;
bool IntersectCustomPrimitiveFrontToBack(
    CustomPrimitiveDef prim,
    inout CustomIntersectionIterator it,
    float3 origin, float3 dir,
    float rayTMin, inout float curT,
    out MyAttributes attr
) { 
    return true;
}

[shader("intersection")]
void IntersectionMain()
{
    float THit = RayTCurrent();
    MyAttributes attr;
    CustomIntersectionIterator it;
    InitCustomIntersectionIterator(it); 
    while(IntersectCustomPrimitiveFrontToBack(
            CustomPrimitiveDefinitions.Load(0),
            it, ObjectRayOrigin(), ObjectRayDirection(), 
            RayTMin(), THit, attr))
    {
        // Exit on the first hit.  Note that if the ray has
        // RAY_FLAG_ACCEPT_FIRST_HIT_AND_END_SEARCH or an
        // anyhit shader is used and calls AcceptHitAndEndSearch(),
        // that would also fully exit this intersection shader (making
        // the “break” below moot in that case).        
        if (ReportHit(THit, /*hitKind*/ 0, attr) && (RayFlags() &  RAY_FLAG_FORCE_OPAQUE))
            break;
    }
}