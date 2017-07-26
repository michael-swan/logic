#version 300 es
precision mediump float;
in vec2 position;
uniform mat4 transform;
void main()
{
    gl_Position = transform * vec4(position, 1.0, 1.0);
}
