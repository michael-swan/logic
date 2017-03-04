#version 430 core
in vec2 position;
in vec2 texcoord;
out vec2 Texcoord;
uniform mat4 transform;
void main()
{
    Texcoord = texcoord;
    gl_Position = transform * vec4(position, 0.0, 1.0);
}
