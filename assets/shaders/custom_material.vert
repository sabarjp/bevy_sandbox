#version 450

layout(location = 0) in vec3 Vertex_Position;
layout(location = 1) in vec3 Vertex_Normal;
layout(location = 2) in vec2 Vertex_Uv;

layout(location = 0) out vec2 texCoordV;
layout(location = 1) out vec4 position;
// note: glPosition is built-in output

layout(set = 0, binding = 0) uniform CameraViewProj {
    mat4 ViewProj;
    mat4 View;
    mat4 InverseView;
    mat4 Projection;
    vec3 WorldPosition;
    float near;
    float far;
    float width;
    float height;
};

layout(set = 2, binding = 0) uniform Mesh {
    mat4 Model;
    mat4 InverseTransposeModel;
    uint flags;
};

void main() {
    texCoordV = Vertex_Uv;
    position = Model * vec4(Vertex_Position, 1.0);
    gl_Position = ViewProj * Model * vec4(Vertex_Position, 1.0);
}