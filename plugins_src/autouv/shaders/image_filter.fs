//
//  image_filter.fs --
//
//     Image filters 
//
//  Copyright (c) 2006 Dan Gudmundsson
//
//  See the file "license.terms" for information on usage and redistribution
//  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
//
//     $Id: image_filter.fs,v 1.1 2006/01/20 15:40:27 dgud Exp $
//
// Grabbed from tutorial By Jérôme Guinot jegx [at] ozone3d [dot] net
//

#define KERNEL_SIZE 9

uniform sampler2D auv_bg;
uniform float kernel[KERNEL_SIZE];
uniform vec2 auv_texsz;
varying vec3 auv_pos2d;

void main(void)
{
  float step_w = 1.0/(auv_texsz.x);
  float step_h = 1.0/(auv_texsz.y);

  vec4 sum = vec4(0.0), tmp;
  float scale = kernel[0]+kernel[1]+kernel[2]+kernel[4]+kernel[5]+
    kernel[6]+kernel[7]+kernel[8];

  tmp = texture2D(auv_bg, auv_pos2d.st + vec2(-step_w, -step_h));
  sum += tmp * kernel[0];
  tmp = texture2D(auv_bg, auv_pos2d.st + vec2(0.0, -step_h));
  sum += tmp * kernel[1];
  tmp = texture2D(auv_bg, auv_pos2d.st + vec2(step_w, -step_h));
  sum += tmp * kernel[2];
  tmp = texture2D(auv_bg, auv_pos2d.st + vec2(-step_w, 0.0));
  sum += tmp * kernel[3];
  tmp = texture2D(auv_bg, auv_pos2d.st + vec2(0.0, 0.0));
  sum += tmp * kernel[4];
  tmp = texture2D(auv_bg, auv_pos2d.st + vec2(step_w, 0.0));
  sum += tmp * kernel[5];
  tmp = texture2D(auv_bg, auv_pos2d.st + vec2(-step_w, step_h));
  sum += tmp * kernel[6];
  tmp = texture2D(auv_bg, auv_pos2d.st + vec2(0.0, step_h));
  sum += tmp * kernel[7];
  tmp = texture2D(auv_bg, auv_pos2d.st + vec2(step_w, step_h));
  sum += tmp * kernel[8];
  
  sum = sum/scale;
  gl_FragColor = sum;
}
