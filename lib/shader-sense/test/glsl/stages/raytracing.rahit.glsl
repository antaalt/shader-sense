#version 460
#extension GL_EXT_ray_tracing : require

hitAttributeEXT vec3 attribs;

layout(location = 0) rayPayloadEXT vec4 payload;

void main()
{
    vec3 surfaceColor = attribs;
    float alpha = surfaceColor.g;

    if (alpha < 0.5)
    {
        // Ignore this hit, continue tracing
        ignoreIntersectionEXT;
    }
}