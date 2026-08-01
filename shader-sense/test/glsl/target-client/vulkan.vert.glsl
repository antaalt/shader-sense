#version 450 core

layout(location = 0) in vec3 inPosition;
layout(location = 1) in vec3 inColor;
layout(location = 2) in vec2 inTexCoord;

layout(location = 0) out vec3 outColor;

layout(binding = 0, set = 0) uniform UBO {
    mat4 model;
    mat4 view;
    mat4 projection;
} ubo;
layout(binding = 1, set = 0) uniform sampler2D texSampler;

layout(push_constant) uniform PushConstants {
    vec4 offset;
} push;

void main() {
    gl_Position = ubo.projection * ubo.view * ubo.model * vec4(inPosition + push.offset.xyz, 1.0);
    vec4 texColor = texture(texSampler, inTexCoord);
    outColor = inColor * texColor.rgb;
}

