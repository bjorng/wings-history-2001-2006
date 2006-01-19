%%
%%  wood.fs --
%%
%%     Simple wood shader stolen from RenderMonkey
%%
%%  Copyright (c) 2006 Dan Gudmundsson
%%
%%  See the file "license.terms" for information on usage and redistribution
%%  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
%%     $Id: wood.fs,v 1.1 2006/01/19 23:20:11 dgud Exp $
%%

uniform vec4  darkWood;
uniform vec4  liteWood;
uniform float frequency;
uniform float noiseScale;
uniform float scale;

varying vec3 auv_pos3d;
uniform sampler3D auv_noise;

float auv_noise(float P, vec3 pos)
{
    float temp = P, total;
    vec4 per = vec4(1.0,temp,temp*temp,temp*temp*temp*temp);
    total = 1.0/dot(per, vec4(1.0));
    per  *= total;
    vec4 noise = texture3D(auv_noise, pos);
    return dot(per,noise);
}

void main(void)
{
    // Signed noise
    float snoise = 2.0 * auv_noise(0.5,scale*auv_pos3d+0.5) - 1.0;

    // Stretch along y axis
    vec2 adjustedScaledPos = vec2(auv_pos3d.x, auv_pos3d.y*.25);
    // Rings are defined by distance to z axis and wobbled along it
    // and perturbed with some noise
    float ring = 0.5*(1.0+sin(5.0*sin(frequency*auv_pos3d.z)+
			      frequency*(noiseScale*snoise+
					 6.28*length(adjustedScaledPos.xy))));    
    // Add some noise and get base color
    float lrp = ring + snoise;
    gl_FragColor = mix(darkWood, liteWood, lrp);
}
