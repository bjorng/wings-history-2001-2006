// 
// noise.fs
//
//      Noise shader
//
// Copyright (c) 2006 Dan Gudmundsson
//
//  See the file "license.terms" for information on usage and redistribution
//  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
//
//     $Id: noise.fs,v 1.3 2006/01/19 23:20:11 dgud Exp $
//


uniform sampler3D auv_noise;

// T = fun(P) -> T=1+1/(P)+1/(P*P)+1/(P*P*P*P),All=[1/T,1/(P*T),1/(P*P*T),1/(P*P*P*P*T)], {T,list_to_tuple(All), lists:sum(All)} end.

float auv_noise(float P, vec3 pos)
{
    float temp = P, total;
    vec4 per = vec4(1.0,temp,temp*temp,temp*temp*temp*temp);
    total = 1.0/dot(per, vec4(1.0));
    per  *= total;
    vec4 noise = texture3D(auv_noise, pos);
    return dot(per,noise);
}
varying vec3 auv_pos2d;
varying vec3 auv_pos3d;
//varying vec3 auv_normal;
uniform float persistance, blend, scale;
uniform vec4 color1, color2;
uniform sampler2D auv_bg;

void main(void)
{   
    float noise;
    vec4 fColor = vec4(1.0); 
    noise = auv_noise(persistance, (scale*auv_pos3d)+0.5); // *0.159155
    vec4 bg = texture2D(auv_bg, auv_pos2d.xy);
    fColor = mix(color2, color1, noise);
    float alpha = fColor.w;
    if(blend > 0.5) alpha *= abs(noise*2.0-1.0);
    // I make my own blending I want keep alpha to be max 
    fColor = mix(bg,fColor,alpha);
    fColor.w = max(alpha,bg.w);
    // Return the calculated color
    gl_FragColor = fColor;
} 

