#version 450

layout(binding=0) uniform MatrixGlobal {
    mat4 u_modelviewGlobal;
    mat4 u_projectionGlobal;
};
layout(binding=1) uniform MatrixHidden {
    mat4 u_modelviewHidden;
    mat4 u_projectionHidden;
} u_accessor;

void main() {
}