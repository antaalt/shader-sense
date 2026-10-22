#version 460 core
#extension GL_EXT_ray_tracing : enable

layout(binding = 0) uniform accelerationStructureEXT acceleration;
layout(binding = 1, rgba8) uniform image2D renderTarget;

layout(location = 0) rayPayloadEXT vec3 color;

void RayGenMain() 
{
    // Get dispatch index
    uvec3 launchIndex = gl_LaunchIDEXT;
    uvec3 launchDims  = gl_LaunchSizeEXT;

    // Compute normalized screen coordinates
    vec2 uv = (vec2(launchIndex.xy) + 0.5f) / vec2(launchDims.xy);

    // Set up ray origin and direction (simple camera)
    vec3 origin = vec3(0.f, 0.f, -5.f);
    vec3 dir = normalize(vec3(uv * 2.f - 1.f, 1.f));

	float tmin = 0.001;
	float tmax = 10000.0;

    color = vec3(0.0);

    traceRayEXT(acceleration, gl_RayFlagsOpaqueEXT, 0xff, 0, 0, 0, origin.xyz, tmin, dir.xyz, tmax, 0);

	imageStore(renderTarget, ivec2(gl_LaunchIDEXT.xy), vec4(color, 0.0));
}