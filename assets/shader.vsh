#version 150
precision highp float;

uniform mat4 projection;
uniform mat4 modelView;

in vec3 a_vertex;
in vec3 a_normal;
in vec2 a_texCoord;

void main(void)
{
	gl_Position = u_projection * u_modelView * vec4(a_vertex, 1.0);
}
