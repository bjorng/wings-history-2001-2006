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
//     $Id: noise.fs,v 1.2 2006/01/18 15:21:22 dgud Exp $
//

varying vec3 auv_pos2d;
varying vec3 auv_pos3d;
varying vec3 auv_normal;
uniform sampler3D auv_noise;

// T = fun(P) -> T=1+1/(P)+1/(P*P)+1/(P*P*P*P),All=[1/T,1/(P*T),1/(P*P*T),1/(P*P*P*P*T)], {T,list_to_tuple(All), lists:sum(All)} end.

float auv_noise(float P, vec3 pos)
{
  float temp = 1.0/P, total;
  vec4 per = vec4(1.0,temp,temp*temp,temp*temp*temp*temp);
  total = 1.0/dot(per, vec4(1.0));
  per  *= total;
  vec4 noise = texture3D(auv_noise, auv_pos3d);
  return dot(per,noise);
}

void main(void)
{   
    vec4 fColor = vec4(1.0,0.0,0.0,0.8); // {0.3,0.4,0.8,0.5};
    // P=2 => [0.551724,0.275862,0.137931,3.44828e-2]
//      vec4 s2 = vec4(0.551724,0.275862,0.137931,3.44828e-2);
//      vec4 temp = texture3D(auv_noise, auv_pos3d);
     //     fColor.xyz = vec3(dot(s2,temp));
     fColor.xyz = vec3(auv_noise(0.5, auv_pos3d));
    // Return the calculated color
    gl_FragColor = fColor;  
}

