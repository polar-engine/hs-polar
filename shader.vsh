#version 150
#extension GL_ARB_explicit_attrib_location: enable

precision highp float;

layout(location=0) in vec2 a_vertex;

void main() {
	gl_Position = vec4(a_vertex, 0.0, 1.0);
}
