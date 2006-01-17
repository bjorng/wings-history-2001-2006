// 
// standard.vs
//
//      Pass through vertex shader.
//
// Copyright (c) 2006 Dan Gudmundsson
//
//  See the file "license.terms" for information on usage and redistribution
//  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
//
//     $Id: standard.vs,v 1.1 2006/01/17 23:22:02 dgud Exp $
//
 
varying vec3 auv_pos2d;
varying vec3 auv_pos3d;
varying vec3 auv_normal;

void main(void)
{
    // UV coords comes here since we are actually drawing on a texture    
    auv_pos2d    = gl_Vertex.xyz;
    // The vertex positions comes here in world space
    auv_pos3d    = gl_MultiTexCoord1.xyz;
    // The normals comes here in world space
    auv_normal   = gl_Normal.xyz;
    
    vec4 Position = gl_Vertex;
    gl_Position   = gl_ModelViewProjectionMatrix * Position;
}
