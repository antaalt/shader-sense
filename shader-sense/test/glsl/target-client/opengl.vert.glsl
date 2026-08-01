#version 330 core

in vec3 inColor;
out vec4 outFragColor;

void main() {
    outFragColor = vec4(inColor, 1.0); // RGBA (alpha = 1.0)
}