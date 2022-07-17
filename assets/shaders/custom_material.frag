#version 450

// note: glPosition is built-in input but will not be bound
layout(location=0) in vec2 texCoordV;
layout(location=1) in vec4 position;
layout(location=0) out vec4 fragColor;

layout(set=1, binding = 0) uniform CustomMaterial {
    vec4 color;
    vec2 pi;
    vec2 gamma;
    vec2 sunPosition;
    vec2 origin;
    vec2 nearFar;
    vec2 enabled;
    vec4 backgroundColor0;
    vec4 backgroundColor1;
};

//layout(set=1, binding = 1) uniform texture2D pos0Texture2D;
//layout(set=1, binding = 2) uniform sampler pos0Sampler;
//layout(set=1, binding = 3) uniform texture2D pos1Texture2D;
//layout(set=1, binding = 4) uniform sampler pos1Sampler;
//layout(set=1, binding = 5) uniform texture2D smokeTexture2D;
//layout(set=1, binding = 6) uniform sampler smokeSampler;



void main() {
  float fogMin = 0.00;
  float fogMax = 0.97;

  if (enabled.x != 1) { fragColor = vec4(0); return; }

/*
  vec2 texSize  = textureSize(sampler2D(pos0Texture2D, pos0Sampler), 0).xy;
  vec2 texCoord = gl_FragCoord.xy / texSize;
  vec4 smokeMask    = texture(sampler2D(smokeTexture2D, smokeSampler), texCoord);
  vec4 position0    = texture(sampler2D(pos0Texture2D, pos0Sampler), texCoord);
       position0.y -= origin.y;
*/

  float near = nearFar.x;
  float far  = nearFar.y;

/*
  vec4 position1    = texture(sampler2D(pos1Texture2D, pos1Sampler), texCoord);
       position1.y -= origin.y;
  if (position1.a <= 0) { position1.y = far; }

  vec4 position = position1;
*/
/*
  if (position0.a <= 0 && smokeMask.r > 0) {
    position.y   = mix(far, position1.y, smokeMask.r);
  } else if (position0.a >  0 && smokeMask.r > 0) {
    position.xyz = mix(position0.xyz, position1.xyz, smokeMask.r);
  }

  float random =
    fract
      ( 10000
      * sin
          (
            ( gl_FragCoord.x
            * 104729
            + gl_FragCoord.y
            * 7639
            )
          * pi.y
          )
      );

  vec4 backgroundColor0     = backgroundColor0;
  vec4 backgroundColor1     = backgroundColor1;
       backgroundColor0.rgb = pow(backgroundColor0.rgb, vec3(gamma.x));
       backgroundColor1.rgb = pow(backgroundColor1.rgb, vec3(gamma.x));

  vec4 color =
    mix
      ( backgroundColor0
      , backgroundColor1
      , 1.0 - clamp(random * 0.1 + texCoord.y, 0.0, 1.0)
      );

  float sunPosition = max(0.2, -1 * sin(sunPosition.x * pi.y));

  color.rgb *= sunPosition;
  color.b    = mix(color.b + 0.05, color.b, sunPosition);
*/

  float intensity =
    clamp
      ((position.y - near)
        / (far        - near)
      , fogMin
      , fogMax
      );


  //fragColor = vec4(color.rgb, intensity);
    fragColor = gl_FragCoord;
}
