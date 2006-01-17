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
//     $Id: noise.fs,v 1.1 2006/01/17 23:22:02 dgud Exp $
//

uniform sampler3D auv_noise;
varying vec3 auv_pos2d;
varying vec3 auv_pos3d;
varying vec3 auv_normal;

// T = fun(P) -> T=1+1/(P)+1/(P*P)+1/(P*P*P*P),All=[1/T,1/(P*T),1/(P*P*T),
// 1/(P*P*P*P*T)], {T,list_to_tuple(All), lists:sum(All)} end.

void main(void)
{   
    vec4 fColor = vec4(1.0,0.0,0.0,1.0); // {0.3,0.4,0.8,0.5};
    // P=2 => [0.551724,0.275862,0.137931,3.44828e-2]
    vec4 s2 = vec4(0.551724,0.275862,0.137931,3.44828e-2);
    vec4 temp = texture3D(auv_noise, auv_pos3d);
    fColor.xyz = vec3(temp.x*s2.x+temp.y*s2.y+temp.z*s2.z+temp.w*s2.w);

    // fColor.xyz = abs(auv_pos3d.xyz);
    // fColor = vec4(1.0,0.0,0.0,1.0);
    // Return the calculated color
    gl_FragColor = fColor;
    
}
