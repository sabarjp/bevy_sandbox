#version 450

// note: glPosition is built-in input but will not be bound
layout(location=0) in vec2 texCoordV;
layout(location=1) in vec4 position;
layout(location=0) out vec4 fragColor;

layout(set=1, binding = 0) uniform CustomMaterial {
    vec4 color;
    vec3 origin;
    float enabled;
};

void main() {
    vec4 fogColor = vec4(0.0, 1.0, 1.0, 1.0);
    float fogMin = 0.00;
    float fogMax = 0.98;

    if (enabled != 1) { fragColor = vec4(0); return; }

    float near = 20.0;
    float far  = 100.0;

    float d = distance(vec4(origin, 1.0), position);

    float intensity = clamp((1 - ((far - d) / (far - near))), fogMin, fogMax);

    fragColor = mix(color, fogColor, intensity);
}
