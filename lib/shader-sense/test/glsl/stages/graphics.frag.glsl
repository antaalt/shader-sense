#version 450

layout(location = 0) out vec4 renderTarget;

void PSMain()
{
    renderTarget = vec4(1.0, 0.0, 1.0, 1.0);
}