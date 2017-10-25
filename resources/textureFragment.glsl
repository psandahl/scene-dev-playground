#version 330 core

in vec2 vTexCoord;

uniform sampler2D transparentImage;
uniform float alpha;

out vec4 color;

void main()
{
  color = vec4(texture2D(transparentImage, vTexCoord).rgb, alpha);
}
