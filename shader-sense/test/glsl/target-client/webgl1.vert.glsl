attribute vec3 aPosition;
attribute vec3 aColor;

uniform mat4 uMVP;

varying vec3 vColor;

void main() {
    vColor = aColor;
    gl_Position = uMVP * vec4(aPosition, 1.0);
}