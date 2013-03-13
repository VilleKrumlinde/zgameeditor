//ZOpenGL är ZzDC mapping mot open gl
//Anpassat nedladdad dll
//Hämta senaste från http://cvs.sourceforge.net/viewcvs.py/jedi-sdl/JEDI-SDLv1.0/

unit ZOpenGL;

//unit GL;
{
  $Id: GL.pas,v 1.6 2003/06/02 12:32:12 savage Exp $

  Adaption of the delphi3d.net OpenGL units to FreePascal
  Sebastian Guenther (sg@freepascal.org) in 2002
  These units are free to use
}

(*++ BUILD Version: 0004    // Increment this if a change has global effects

Copyright (c) 1985-96, Microsoft Corporation

Module Name:

    gl.h

Abstract:

    Procedure declarations, constant definitions and macros for the OpenGL
    component.

--*)

(*
** Copyright 1996 Silicon Graphics, Inc.
** All Rights Reserved.
**
** This is UNPUBLISHED PROPRIETARY SOURCE CODE of Silicon Graphics, Inc.;
** the contents of this file may not be disclosed to third parties, copied or
** duplicated in any form, in whole or in part, without the prior written
** permission of Silicon Graphics, Inc.
**
** RESTRICTED RIGHTS LEGEND:
** Use, duplication or disclosure by the Government is subject to restrictions
** as set forth in subdivision (c)(1)(ii) of the Rights in Technical Data
** and Computer Software clause at DFARS 252.227-7013, and/or in similar or
** successor clauses in the FAR, DOD or NASA FAR Supplement. Unpublished -
** rights reserved under the Copyright Laws of the United States.
*)

{******************************************************************************}
{                                                                              }
{ Converted to Delphi by Tom Nuydens (tom@delphi3d.net)                        }
{ For the latest updates, visit Delphi3D: http://www.delphi3d.net              }
{                                                                              }
{ Modified for Delphi/Kylix and FreePascal                                     }
{    by Dominique Louis ( Dominique@Savagesoftware.com.au)                     }
{ For the latest updates, visit JEDI-SDL : http://www.sf.net/projects/jedi-sdl }
{                                                                              }
{******************************************************************************}

{
   Rev 1.0    2005-01-14 18:10:04  supervisor
  Initial Revision
  Revision 1.6  2003/06/02 12:32:12  savage
  Modified Sources to avoid warnings with Delphi by moving CVS Logging to the top of the header files. Hopefully CVS Logging still works.
}

interface

type
  GLenum     = Cardinal;      PGLenum     = ^GLenum;
  GLboolean  = Byte;          PGLboolean  = ^GLboolean;
  GLbitfield = Cardinal;      PGLbitfield = ^GLbitfield;
  GLbyte     = ShortInt;      PGLbyte     = ^GLbyte;
  GLshort    = SmallInt;      PGLshort    = ^GLshort;
  GLint      = Integer;       PGLint      = ^GLint;
  GLsizei    = Integer;       PGLsizei    = ^GLsizei;
  GLubyte    = Byte;          PGLubyte    = ^GLubyte;
  GLushort   = Word;          PGLushort   = ^GLushort;
  GLuint     = Cardinal;      PGLuint     = ^GLuint;
  GLfloat    = Single;        PGLfloat    = ^GLfloat;
  GLclampf   = Single;        PGLclampf   = ^GLclampf;
  GLdouble   = Double;        PGLdouble   = ^GLdouble;
  GLclampd   = Double;        PGLclampd   = ^GLclampd;
{ GLvoid     = void; }        PGLvoid     = Pointer;


{******************************************************************************}

const
  // Version
  GL_VERSION_1_1                    = 1;

  // AccumOp
  GL_ACCUM                          = $0100;
  GL_LOAD                           = $0101;
  GL_RETURN                         = $0102;
  GL_MULT                           = $0103;
  GL_ADD                            = $0104;

  // AlphaFunction
  GL_NEVER                          = $0200;
  GL_LESS                           = $0201;
  GL_EQUAL                          = $0202;
  GL_LEQUAL                         = $0203;
  GL_GREATER                        = $0204;
  GL_NOTEQUAL                       = $0205;
  GL_GEQUAL                         = $0206;
  GL_ALWAYS                         = $0207;

  // AttribMask
  GL_CURRENT_BIT                    = $00000001;
  GL_POINT_BIT                      = $00000002;
  GL_LINE_BIT                       = $00000004;
  GL_POLYGON_BIT                    = $00000008;
  GL_POLYGON_STIPPLE_BIT            = $00000010;
  GL_PIXEL_MODE_BIT                 = $00000020;
  GL_LIGHTING_BIT                   = $00000040;
  GL_FOG_BIT                        = $00000080;
  GL_DEPTH_BUFFER_BIT               = $00000100;
  GL_ACCUM_BUFFER_BIT               = $00000200;
  GL_STENCIL_BUFFER_BIT             = $00000400;
  GL_VIEWPORT_BIT                   = $00000800;
  GL_TRANSFORM_BIT                  = $00001000;
  GL_ENABLE_BIT                     = $00002000;
  GL_COLOR_BUFFER_BIT               = $00004000;
  GL_HINT_BIT                       = $00008000;
  GL_EVAL_BIT                       = $00010000;
  GL_LIST_BIT                       = $00020000;
  GL_TEXTURE_BIT                    = $00040000;
  GL_SCISSOR_BIT                    = $00080000;
  GL_ALL_ATTRIB_BITS                = $000FFFFF;

  // BeginMode
  GL_POINTS                         = $0000;
  GL_LINES                          = $0001;
  GL_LINE_LOOP                      = $0002;
  GL_LINE_STRIP                     = $0003;
  GL_TRIANGLES                      = $0004;
  GL_TRIANGLE_STRIP                 = $0005;
  GL_TRIANGLE_FAN                   = $0006;
  GL_QUADS                          = $0007;
  GL_QUAD_STRIP                     = $0008;
  GL_POLYGON                        = $0009;

  // BlendingFactorDest
  GL_ZERO                           = 0;
  GL_ONE                            = 1;
  GL_SRC_COLOR                      = $0300;
  GL_ONE_MINUS_SRC_COLOR            = $0301;
  GL_SRC_ALPHA                      = $0302;
  GL_ONE_MINUS_SRC_ALPHA            = $0303;
  GL_DST_ALPHA                      = $0304;
  GL_ONE_MINUS_DST_ALPHA            = $0305;

  // BlendingFactorSrc
  //      GL_ZERO
  //      GL_ONE
  GL_DST_COLOR                      = $0306;
  GL_ONE_MINUS_DST_COLOR            = $0307;
  GL_SRC_ALPHA_SATURATE             = $0308;
  //      GL_SRC_ALPHA
  //      GL_ONE_MINUS_SRC_ALPHA
  //      GL_DST_ALPHA
  //      GL_ONE_MINUS_DST_ALPHA

  // Boolean
  GL_TRUE                           = 1;
  GL_FALSE                          = 0;

  // ClearBufferMask
  //      GL_COLOR_BUFFER_BIT
  //      GL_ACCUM_BUFFER_BIT
  //      GL_STENCIL_BUFFER_BIT
  //      GL_DEPTH_BUFFER_BIT

  // ClientArrayType
  //      GL_VERTEX_ARRAY
  //      GL_NORMAL_ARRAY
  //      GL_COLOR_ARRAY
  //      GL_INDEX_ARRAY
  //      GL_TEXTURE_COORD_ARRAY
  //      GL_EDGE_FLAG_ARRAY

  // ClipPlaneName
  GL_CLIP_PLANE0                    = $3000;
  GL_CLIP_PLANE1                    = $3001;
  GL_CLIP_PLANE2                    = $3002;
  GL_CLIP_PLANE3                    = $3003;
  GL_CLIP_PLANE4                    = $3004;
  GL_CLIP_PLANE5                    = $3005;

  // ColorMaterialFace
  //      GL_FRONT
  //      GL_BACK
  //      GL_FRONT_AND_BACK

  // ColorMaterialParameter
  //      GL_AMBIENT
  //      GL_DIFFUSE
  //      GL_SPECULAR
  //      GL_EMISSION
  //      GL_AMBIENT_AND_DIFFUSE

  // ColorPointerType
  //      GL_BYTE
  //      GL_UNSIGNED_BYTE
  //      GL_SHORT
  //      GL_UNSIGNED_SHORT
  //      GL_INT
  //      GL_UNSIGNED_INT
  //      GL_FLOAT
  //      GL_DOUBLE

  // CullFaceMode
  //      GL_FRONT
  //      GL_BACK
  //      GL_FRONT_AND_BACK

  // DataType
  GL_BYTE                           = $1400;
  GL_UNSIGNED_BYTE                  = $1401;
  GL_SHORT                          = $1402;
  GL_UNSIGNED_SHORT                 = $1403;
  GL_INT                            = $1404;
  GL_UNSIGNED_INT                   = $1405;
  GL_FLOAT                          = $1406;
  GL_2_BYTES                        = $1407;
  GL_3_BYTES                        = $1408;
  GL_4_BYTES                        = $1409;
  GL_DOUBLE                         = $140A;

  // DepthFunction
  //      GL_NEVER
  //      GL_LESS
  //      GL_EQUAL
  //      GL_LEQUAL
  //      GL_GREATER
  //      GL_NOTEQUAL
  //      GL_GEQUAL
  //      GL_ALWAYS

  // DrawBufferMode
  GL_NONE                           = 0;
  GL_FRONT_LEFT                     = $0400;
  GL_FRONT_RIGHT                    = $0401;
  GL_BACK_LEFT                      = $0402;
  GL_BACK_RIGHT                     = $0403;
  GL_FRONT                          = $0404;
  GL_BACK                           = $0405;
  GL_LEFT                           = $0406;
  GL_RIGHT                          = $0407;
  GL_FRONT_AND_BACK                 = $0408;
  GL_AUX0                           = $0409;
  GL_AUX1                           = $040A;
  GL_AUX2                           = $040B;
  GL_AUX3                           = $040C;

  // Enable
  //      GL_FOG
  //      GL_LIGHTING
  //      GL_TEXTURE_1D
  //      GL_TEXTURE_2D
  //      GL_LINE_STIPPLE
  //      GL_POLYGON_STIPPLE
  //      GL_CULL_FACE
  //      GL_ALPHA_TEST
  //      GL_BLEND
  //      GL_INDEX_LOGIC_OP
  //      GL_COLOR_LOGIC_OP
  //      GL_DITHER
  //      GL_STENCIL_TEST
  //      GL_DEPTH_TEST
  //      GL_CLIP_PLANE0
  //      GL_CLIP_PLANE1
  //      GL_CLIP_PLANE2
  //      GL_CLIP_PLANE3
  //      GL_CLIP_PLANE4
  //      GL_CLIP_PLANE5
  //      GL_LIGHT0
  //      GL_LIGHT1
  //      GL_LIGHT2
  //      GL_LIGHT3
  //      GL_LIGHT4
  //      GL_LIGHT5
  //      GL_LIGHT6
  //      GL_LIGHT7
  //      GL_TEXTURE_GEN_S
  //      GL_TEXTURE_GEN_T
  //      GL_TEXTURE_GEN_R
  //      GL_TEXTURE_GEN_Q
  //      GL_MAP1_VERTEX_3
  //      GL_MAP1_VERTEX_4
  //      GL_MAP1_COLOR_4
  //      GL_MAP1_INDEX
  //      GL_MAP1_NORMAL
  //      GL_MAP1_TEXTURE_COORD_1
  //      GL_MAP1_TEXTURE_COORD_2
  //      GL_MAP1_TEXTURE_COORD_3
  //      GL_MAP1_TEXTURE_COORD_4
  //      GL_MAP2_VERTEX_3
  //      GL_MAP2_VERTEX_4
  //      GL_MAP2_COLOR_4
  //      GL_MAP2_INDEX
  //      GL_MAP2_NORMAL
  //      GL_MAP2_TEXTURE_COORD_1
  //      GL_MAP2_TEXTURE_COORD_2
  //      GL_MAP2_TEXTURE_COORD_3
  //      GL_MAP2_TEXTURE_COORD_4
  //      GL_POINT_SMOOTH
  //      GL_LINE_SMOOTH
  //      GL_POLYGON_SMOOTH
  //      GL_SCISSOR_TEST
  //      GL_COLOR_MATERIAL
  //      GL_NORMALIZE
  //      GL_AUTO_NORMAL
  //      GL_VERTEX_ARRAY
  //      GL_NORMAL_ARRAY
  //      GL_COLOR_ARRAY
  //      GL_INDEX_ARRAY
  //      GL_TEXTURE_COORD_ARRAY
  //      GL_EDGE_FLAG_ARRAY
  //      GL_POLYGON_OFFSET_POINT
  //      GL_POLYGON_OFFSET_LINE
  //      GL_POLYGON_OFFSET_FILL

  // ErrorCode
  GL_NO_ERROR                       = 0;
  GL_INVALID_ENUM                   = $0500;
  GL_INVALID_VALUE                  = $0501;
  GL_INVALID_OPERATION              = $0502;
  GL_STACK_OVERFLOW                 = $0503;
  GL_STACK_UNDERFLOW                = $0504;
  GL_OUT_OF_MEMORY                  = $0505;

  // FeedBackMode
  GL_2D                             = $0600;
  GL_3D                             = $0601;
  GL_3D_COLOR                       = $0602;
  GL_3D_COLOR_TEXTURE               = $0603;
  GL_4D_COLOR_TEXTURE               = $0604;

  // FeedBackToken
  GL_PASS_THROUGH_TOKEN             = $0700;
  GL_POINT_TOKEN                    = $0701;
  GL_LINE_TOKEN                     = $0702;
  GL_POLYGON_TOKEN                  = $0703;
  GL_BITMAP_TOKEN                   = $0704;
  GL_DRAW_PIXEL_TOKEN               = $0705;
  GL_COPY_PIXEL_TOKEN               = $0706;
  GL_LINE_RESET_TOKEN               = $0707;

  // FogMode
  //      GL_LINEAR
  GL_EXP                            = $0800;
  GL_EXP2                           = $0801;

  // FogParameter
  //      GL_FOG_COLOR
  //      GL_FOG_DENSITY
  //      GL_FOG_END
  //      GL_FOG_INDEX
  //      GL_FOG_MODE
  //      GL_FOG_START

  // FrontFaceDirection
  GL_CW                             = $0900;
  GL_CCW                            = $0901;

  // GetMapTarget
  GL_COEFF                          = $0A00;
  GL_ORDER                          = $0A01;
  GL_DOMAIN                         = $0A02;

  // GetPixelMap
  //      GL_PIXEL_MAP_I_TO_I 
  //      GL_PIXEL_MAP_S_TO_S
  //      GL_PIXEL_MAP_I_TO_R
  //      GL_PIXEL_MAP_I_TO_G
  //      GL_PIXEL_MAP_I_TO_B 
  //      GL_PIXEL_MAP_I_TO_A
  //      GL_PIXEL_MAP_R_TO_R
  //      GL_PIXEL_MAP_G_TO_G 
  //      GL_PIXEL_MAP_B_TO_B
  //      GL_PIXEL_MAP_A_TO_A 

  // GetPointerTarget 
  //      GL_VERTEX_ARRAY_POINTER
  //      GL_NORMAL_ARRAY_POINTER 
  //      GL_COLOR_ARRAY_POINTER
  //      GL_INDEX_ARRAY_POINTER
  //      GL_TEXTURE_COORD_ARRAY_POINTER 
  //      GL_EDGE_FLAG_ARRAY_POINTER 

  // GetTarget 
  GL_CURRENT_COLOR                  = $0B00;
  GL_CURRENT_INDEX                  = $0B01;
  GL_CURRENT_NORMAL                 = $0B02;
  GL_CURRENT_TEXTURE_COORDS         = $0B03;
  GL_CURRENT_RASTER_COLOR           = $0B04;
  GL_CURRENT_RASTER_INDEX           = $0B05;
  GL_CURRENT_RASTER_TEXTURE_COORDS  = $0B06;
  GL_CURRENT_RASTER_POSITION        = $0B07;
  GL_CURRENT_RASTER_POSITION_VALID  = $0B08;
  GL_CURRENT_RASTER_DISTANCE        = $0B09;
  GL_POINT_SMOOTH                   = $0B10;
  GL_POINT_SIZE                     = $0B11;
  GL_POINT_SIZE_RANGE               = $0B12;
  GL_POINT_SIZE_GRANULARITY         = $0B13;
  GL_LINE_SMOOTH                    = $0B20;
  GL_LINE_WIDTH                     = $0B21;
  GL_LINE_WIDTH_RANGE               = $0B22;
  GL_LINE_WIDTH_GRANULARITY         = $0B23;
  GL_LINE_STIPPLE                   = $0B24;
  GL_LINE_STIPPLE_PATTERN           = $0B25;
  GL_LINE_STIPPLE_REPEAT            = $0B26;
  GL_LIST_MODE                      = $0B30;
  GL_MAX_LIST_NESTING               = $0B31;
  GL_LIST_BASE                      = $0B32;
  GL_LIST_INDEX                     = $0B33;
  GL_POLYGON_MODE                   = $0B40;
  GL_POLYGON_SMOOTH                 = $0B41;
  GL_POLYGON_STIPPLE                = $0B42;
  GL_EDGE_FLAG                      = $0B43;
  GL_CULL_FACE                      = $0B44;
  GL_CULL_FACE_MODE                 = $0B45;
  GL_FRONT_FACE                     = $0B46;
  GL_LIGHTING                       = $0B50;
  GL_LIGHT_MODEL_LOCAL_VIEWER       = $0B51;
  GL_LIGHT_MODEL_TWO_SIDE           = $0B52;
  GL_LIGHT_MODEL_AMBIENT            = $0B53;
  GL_SHADE_MODEL                    = $0B54;
  GL_COLOR_MATERIAL_FACE            = $0B55;
  GL_COLOR_MATERIAL_PARAMETER       = $0B56;
  GL_COLOR_MATERIAL                 = $0B57;
  GL_FOG                            = $0B60;
  GL_FOG_INDEX                      = $0B61;
  GL_FOG_DENSITY                    = $0B62;
  GL_FOG_START                      = $0B63;
  GL_FOG_END                        = $0B64;
  GL_FOG_MODE                       = $0B65;
  GL_FOG_COLOR                      = $0B66;
  GL_DEPTH_RANGE                    = $0B70;
  GL_DEPTH_TEST                     = $0B71;
  GL_DEPTH_WRITEMASK                = $0B72;
  GL_DEPTH_CLEAR_VALUE              = $0B73;
  GL_DEPTH_FUNC                     = $0B74;
  GL_ACCUM_CLEAR_VALUE              = $0B80;
  GL_STENCIL_TEST                   = $0B90;
  GL_STENCIL_CLEAR_VALUE            = $0B91;
  GL_STENCIL_FUNC                   = $0B92;
  GL_STENCIL_VALUE_MASK             = $0B93;
  GL_STENCIL_FAIL                   = $0B94;
  GL_STENCIL_PASS_DEPTH_FAIL        = $0B95;
  GL_STENCIL_PASS_DEPTH_PASS        = $0B96;
  GL_STENCIL_REF                    = $0B97;
  GL_STENCIL_WRITEMASK              = $0B98;
  GL_MATRIX_MODE                    = $0BA0;
  GL_NORMALIZE                      = $0BA1;
  GL_VIEWPORT                       = $0BA2;
  GL_MODELVIEW_STACK_DEPTH          = $0BA3;
  GL_PROJECTION_STACK_DEPTH         = $0BA4;
  GL_TEXTURE_STACK_DEPTH            = $0BA5;
  GL_MODELVIEW_MATRIX               = $0BA6;
  GL_PROJECTION_MATRIX              = $0BA7;
  GL_TEXTURE_MATRIX                 = $0BA8;
  GL_ATTRIB_STACK_DEPTH             = $0BB0;
  GL_CLIENT_ATTRIB_STACK_DEPTH      = $0BB1;
  GL_ALPHA_TEST                     = $0BC0;
  GL_ALPHA_TEST_FUNC                = $0BC1;
  GL_ALPHA_TEST_REF                 = $0BC2;
  GL_DITHER                         = $0BD0;
  GL_BLEND_DST                      = $0BE0;
  GL_BLEND_SRC                      = $0BE1;
  GL_BLEND                          = $0BE2;
  GL_LOGIC_OP_MODE                  = $0BF0;
  GL_INDEX_LOGIC_OP                 = $0BF1;
  GL_COLOR_LOGIC_OP                 = $0BF2;
  GL_AUX_BUFFERS                    = $0C00;
  GL_DRAW_BUFFER                    = $0C01;
  GL_READ_BUFFER                    = $0C02;
  GL_SCISSOR_BOX                    = $0C10;
  GL_SCISSOR_TEST                   = $0C11;
  GL_INDEX_CLEAR_VALUE              = $0C20;
  GL_INDEX_WRITEMASK                = $0C21;
  GL_COLOR_CLEAR_VALUE              = $0C22;
  GL_COLOR_WRITEMASK                = $0C23;
  GL_INDEX_MODE                     = $0C30;
  GL_RGBA_MODE                      = $0C31;
  GL_DOUBLEBUFFER                   = $0C32;
  GL_STEREO                         = $0C33;
  GL_RENDER_MODE                    = $0C40;
  GL_PERSPECTIVE_CORRECTION_HINT    = $0C50;
  GL_POINT_SMOOTH_HINT              = $0C51;
  GL_LINE_SMOOTH_HINT               = $0C52;
  GL_POLYGON_SMOOTH_HINT            = $0C53;
  GL_FOG_HINT                       = $0C54;
  GL_TEXTURE_GEN_S                  = $0C60;
  GL_TEXTURE_GEN_T                  = $0C61;
  GL_TEXTURE_GEN_R                  = $0C62;
  GL_TEXTURE_GEN_Q                  = $0C63;
  GL_PIXEL_MAP_I_TO_I               = $0C70;
  GL_PIXEL_MAP_S_TO_S               = $0C71;
  GL_PIXEL_MAP_I_TO_R               = $0C72;
  GL_PIXEL_MAP_I_TO_G               = $0C73;
  GL_PIXEL_MAP_I_TO_B               = $0C74;
  GL_PIXEL_MAP_I_TO_A               = $0C75;
  GL_PIXEL_MAP_R_TO_R               = $0C76;
  GL_PIXEL_MAP_G_TO_G               = $0C77;
  GL_PIXEL_MAP_B_TO_B               = $0C78;
  GL_PIXEL_MAP_A_TO_A               = $0C79;
  GL_PIXEL_MAP_I_TO_I_SIZE          = $0CB0;
  GL_PIXEL_MAP_S_TO_S_SIZE          = $0CB1;
  GL_PIXEL_MAP_I_TO_R_SIZE          = $0CB2;
  GL_PIXEL_MAP_I_TO_G_SIZE          = $0CB3;
  GL_PIXEL_MAP_I_TO_B_SIZE          = $0CB4;
  GL_PIXEL_MAP_I_TO_A_SIZE          = $0CB5;
  GL_PIXEL_MAP_R_TO_R_SIZE          = $0CB6;
  GL_PIXEL_MAP_G_TO_G_SIZE          = $0CB7;
  GL_PIXEL_MAP_B_TO_B_SIZE          = $0CB8;
  GL_PIXEL_MAP_A_TO_A_SIZE          = $0CB9;
  GL_UNPACK_SWAP_BYTES              = $0CF0;
  GL_UNPACK_LSB_FIRST               = $0CF1;
  GL_UNPACK_ROW_LENGTH              = $0CF2;
  GL_UNPACK_SKIP_ROWS               = $0CF3;
  GL_UNPACK_SKIP_PIXELS             = $0CF4;
  GL_UNPACK_ALIGNMENT               = $0CF5;
  GL_PACK_SWAP_BYTES                = $0D00;
  GL_PACK_LSB_FIRST                 = $0D01;
  GL_PACK_ROW_LENGTH                = $0D02;
  GL_PACK_SKIP_ROWS                 = $0D03;
  GL_PACK_SKIP_PIXELS               = $0D04;
  GL_PACK_ALIGNMENT                 = $0D05;
  GL_MAP_COLOR                      = $0D10;
  GL_MAP_STENCIL                    = $0D11;
  GL_INDEX_SHIFT                    = $0D12;
  GL_INDEX_OFFSET                   = $0D13;
  GL_RED_SCALE                      = $0D14;
  GL_RED_BIAS                       = $0D15;
  GL_ZOOM_X                         = $0D16;
  GL_ZOOM_Y                         = $0D17;
  GL_GREEN_SCALE                    = $0D18;
  GL_GREEN_BIAS                     = $0D19;
  GL_BLUE_SCALE                     = $0D1A;
  GL_BLUE_BIAS                      = $0D1B;
  GL_ALPHA_SCALE                    = $0D1C;
  GL_ALPHA_BIAS                     = $0D1D;
  GL_DEPTH_SCALE                    = $0D1E;
  GL_DEPTH_BIAS                     = $0D1F;
  GL_MAX_EVAL_ORDER                 = $0D30;
  GL_MAX_LIGHTS                     = $0D31;
  GL_MAX_CLIP_PLANES                = $0D32;
  GL_MAX_TEXTURE_SIZE               = $0D33;
  GL_MAX_PIXEL_MAP_TABLE            = $0D34;
  GL_MAX_ATTRIB_STACK_DEPTH         = $0D35;
  GL_MAX_MODELVIEW_STACK_DEPTH      = $0D36;
  GL_MAX_NAME_STACK_DEPTH           = $0D37;
  GL_MAX_PROJECTION_STACK_DEPTH     = $0D38;
  GL_MAX_TEXTURE_STACK_DEPTH        = $0D39;
  GL_MAX_VIEWPORT_DIMS              = $0D3A;
  GL_MAX_CLIENT_ATTRIB_STACK_DEPTH  = $0D3B;
  GL_SUBPIXEL_BITS                  = $0D50;
  GL_INDEX_BITS                     = $0D51;
  GL_RED_BITS                       = $0D52;
  GL_GREEN_BITS                     = $0D53;
  GL_BLUE_BITS                      = $0D54;
  GL_ALPHA_BITS                     = $0D55;
  GL_DEPTH_BITS                     = $0D56;
  GL_STENCIL_BITS                   = $0D57;
  GL_ACCUM_RED_BITS                 = $0D58;
  GL_ACCUM_GREEN_BITS               = $0D59;
  GL_ACCUM_BLUE_BITS                = $0D5A;
  GL_ACCUM_ALPHA_BITS               = $0D5B;
  GL_NAME_STACK_DEPTH               = $0D70;
  GL_AUTO_NORMAL                    = $0D80;
  GL_MAP1_COLOR_4                   = $0D90;
  GL_MAP1_INDEX                     = $0D91;
  GL_MAP1_NORMAL                    = $0D92;
  GL_MAP1_TEXTURE_COORD_1           = $0D93;
  GL_MAP1_TEXTURE_COORD_2           = $0D94;
  GL_MAP1_TEXTURE_COORD_3           = $0D95;
  GL_MAP1_TEXTURE_COORD_4           = $0D96;
  GL_MAP1_VERTEX_3                  = $0D97;
  GL_MAP1_VERTEX_4                  = $0D98;
  GL_MAP2_COLOR_4                   = $0DB0;
  GL_MAP2_INDEX                     = $0DB1;
  GL_MAP2_NORMAL                    = $0DB2;
  GL_MAP2_TEXTURE_COORD_1           = $0DB3;
  GL_MAP2_TEXTURE_COORD_2           = $0DB4;
  GL_MAP2_TEXTURE_COORD_3           = $0DB5;
  GL_MAP2_TEXTURE_COORD_4           = $0DB6;
  GL_MAP2_VERTEX_3                  = $0DB7;
  GL_MAP2_VERTEX_4                  = $0DB8;
  GL_MAP1_GRID_DOMAIN               = $0DD0;
  GL_MAP1_GRID_SEGMENTS             = $0DD1;
  GL_MAP2_GRID_DOMAIN               = $0DD2;
  GL_MAP2_GRID_SEGMENTS             = $0DD3;
  GL_TEXTURE_1D                     = $0DE0;
  GL_TEXTURE_2D                     = $0DE1;
  GL_FEEDBACK_BUFFER_POINTER        = $0DF0;
  GL_FEEDBACK_BUFFER_SIZE           = $0DF1;
  GL_FEEDBACK_BUFFER_TYPE           = $0DF2;
  GL_SELECTION_BUFFER_POINTER       = $0DF3;
  GL_SELECTION_BUFFER_SIZE          = $0DF4;
  //      GL_TEXTURE_BINDING_1D
  //      GL_TEXTURE_BINDING_2D
  //      GL_VERTEX_ARRAY
  //      GL_NORMAL_ARRAY 
  //      GL_COLOR_ARRAY 
  //      GL_INDEX_ARRAY
  //      GL_TEXTURE_COORD_ARRAY 
  //      GL_EDGE_FLAG_ARRAY
  //      GL_VERTEX_ARRAY_SIZE
  //      GL_VERTEX_ARRAY_TYPE 
  //      GL_VERTEX_ARRAY_STRIDE 
  //      GL_NORMAL_ARRAY_TYPE 
  //      GL_NORMAL_ARRAY_STRIDE
  //      GL_COLOR_ARRAY_SIZE 
  //      GL_COLOR_ARRAY_TYPE
  //      GL_COLOR_ARRAY_STRIDE 
  //      GL_INDEX_ARRAY_TYPE 
  //      GL_INDEX_ARRAY_STRIDE 
  //      GL_TEXTURE_COORD_ARRAY_SIZE 
  //      GL_TEXTURE_COORD_ARRAY_TYPE 
  //      GL_TEXTURE_COORD_ARRAY_STRIDE
  //      GL_EDGE_FLAG_ARRAY_STRIDE 
  //      GL_POLYGON_OFFSET_FACTOR 
  //      GL_POLYGON_OFFSET_UNITS 

  // GetTextureParameter 
  //      GL_TEXTURE_MAG_FILTER
  //      GL_TEXTURE_MIN_FILTER
  //      GL_TEXTURE_WRAP_S
  //      GL_TEXTURE_WRAP_T
  GL_TEXTURE_WIDTH                  = $1000;
  GL_TEXTURE_HEIGHT                 = $1001;
  GL_TEXTURE_INTERNAL_FORMAT        = $1003;
  GL_TEXTURE_BORDER_COLOR           = $1004;
  GL_TEXTURE_BORDER                 = $1005;
  //      GL_TEXTURE_RED_SIZE
  //      GL_TEXTURE_GREEN_SIZE
  //      GL_TEXTURE_BLUE_SIZE
  //      GL_TEXTURE_ALPHA_SIZE
  //      GL_TEXTURE_LUMINANCE_SIZE
  //      GL_TEXTURE_INTENSITY_SIZE
  //      GL_TEXTURE_PRIORITY
  //      GL_TEXTURE_RESIDENT

  // HintMode
  GL_DONT_CARE                      = $1100;
  GL_FASTEST                        = $1101;
  GL_NICEST                         = $1102;

  // HintTarget
  //      GL_PERSPECTIVE_CORRECTION_HINT
  //      GL_POINT_SMOOTH_HINT
  //      GL_LINE_SMOOTH_HINT
  //      GL_POLYGON_SMOOTH_HINT
  //      GL_FOG_HINT

  // IndexPointerType
  //      GL_SHORT
  //      GL_INT
  //      GL_FLOAT
  //      GL_DOUBLE

  // LightModelParameter
  //      GL_LIGHT_MODEL_AMBIENT
  //      GL_LIGHT_MODEL_LOCAL_VIEWER
  //      GL_LIGHT_MODEL_TWO_SIDE

  // LightName
  GL_LIGHT0                         = $4000;
  GL_LIGHT1                         = $4001;
  GL_LIGHT2                         = $4002;
  GL_LIGHT3                         = $4003;
  GL_LIGHT4                         = $4004;
  GL_LIGHT5                         = $4005;
  GL_LIGHT6                         = $4006;
  GL_LIGHT7                         = $4007;

  // LightParameter
  GL_AMBIENT                        = $1200;
  GL_DIFFUSE                        = $1201;
  GL_SPECULAR                       = $1202;
  GL_POSITION                       = $1203;
  GL_SPOT_DIRECTION                 = $1204;
  GL_SPOT_EXPONENT                  = $1205;
  GL_SPOT_CUTOFF                    = $1206;
  GL_CONSTANT_ATTENUATION           = $1207;
  GL_LINEAR_ATTENUATION             = $1208;
  GL_QUADRATIC_ATTENUATION          = $1209;

  // InterleavedArrays
  //      GL_V2F
  //      GL_V3F
  //      GL_C4UB_V2F
  //      GL_C4UB_V3F
  //      GL_C3F_V3F
  //      GL_N3F_V3F
  //      GL_C4F_N3F_V3F
  //      GL_T2F_V3F
  //      GL_T4F_V4F
  //      GL_T2F_C4UB_V3F
  //      GL_T2F_C3F_V3F
  //      GL_T2F_N3F_V3F
  //      GL_T2F_C4F_N3F_V3F
  //      GL_T4F_C4F_N3F_V4F

  // ListMode
  GL_COMPILE                        = $1300;
  GL_COMPILE_AND_EXECUTE            = $1301;

  // ListNameType
  //      GL_BYTE
  //      GL_UNSIGNED_BYTE
  //      GL_SHORT
  //      GL_UNSIGNED_SHORT
  //      GL_INT
  //      GL_UNSIGNED_INT
  //      GL_FLOAT
  //      GL_2_BYTES
  //      GL_3_BYTES
  //      GL_4_BYTES

  // LogicOp
  GL_CLEAR                          = $1500;
  GL_AND                            = $1501;
  GL_AND_REVERSE                    = $1502;
  GL_COPY                           = $1503;
  GL_AND_INVERTED                   = $1504;
  GL_NOOP                           = $1505;
  GL_XOR                            = $1506;
  GL_OR                             = $1507;
  GL_NOR                            = $1508;
  GL_EQUIV                          = $1509;
  GL_INVERT                         = $150A;
  GL_OR_REVERSE                     = $150B;
  GL_COPY_INVERTED                  = $150C;
  GL_OR_INVERTED                    = $150D;
  GL_NAND                           = $150E;
  GL_SET                            = $150F;

  // MapTarget
  //      GL_MAP1_COLOR_4
  //      GL_MAP1_INDEX
  //      GL_MAP1_NORMAL
  //      GL_MAP1_TEXTURE_COORD_1
  //      GL_MAP1_TEXTURE_COORD_2
  //      GL_MAP1_TEXTURE_COORD_3
  //      GL_MAP1_TEXTURE_COORD_4
  //      GL_MAP1_VERTEX_3
  //      GL_MAP1_VERTEX_4
  //      GL_MAP2_COLOR_4
  //      GL_MAP2_INDEX
  //      GL_MAP2_NORMAL
  //      GL_MAP2_TEXTURE_COORD_1
  //      GL_MAP2_TEXTURE_COORD_2
  //      GL_MAP2_TEXTURE_COORD_3
  //      GL_MAP2_TEXTURE_COORD_4
  //      GL_MAP2_VERTEX_3
  //      GL_MAP2_VERTEX_4

  // MaterialFace
  //      GL_FRONT
  //      GL_BACK
  //      GL_FRONT_AND_BACK

  // MaterialParameter
  GL_EMISSION                       = $1600;
  GL_SHININESS                      = $1601;
  GL_AMBIENT_AND_DIFFUSE            = $1602;
  GL_COLOR_INDEXES                  = $1603;
  //      GL_AMBIENT
  //      GL_DIFFUSE
  //      GL_SPECULAR

  // MatrixMode
  GL_MODELVIEW                      = $1700;
  GL_PROJECTION                     = $1701;
  GL_TEXTURE                        = $1702;

  // MeshMode1
  //      GL_POINT
  //      GL_LINE

  // MeshMode2
  //      GL_POINT
  //      GL_LINE
  //      GL_FILL

  // NormalPointerType
  //      GL_BYTE
  //      GL_SHORT
  //      GL_INT
  //      GL_FLOAT
  //      GL_DOUBLE

  // PixelCopyType
  GL_COLOR                          = $1800;
  GL_DEPTH                          = $1801;
  GL_STENCIL                        = $1802;

  // PixelFormat
  GL_COLOR_INDEX                    = $1900;
  GL_STENCIL_INDEX                  = $1901;
  GL_DEPTH_COMPONENT                = $1902;
  GL_RED                            = $1903;
  GL_GREEN                          = $1904;
  GL_BLUE                           = $1905;
  GL_ALPHA                          = $1906;
  GL_RGB                            = $1907;
  GL_BGR = $80E0; //opengl 1.2
  GL_RGBA                           = $1908;
  GL_LUMINANCE                      = $1909;
  GL_LUMINANCE_ALPHA                = $190A;

  GL_R32F = $822E; //OpenGL 3+

  // PixelMap
  //      GL_PIXEL_MAP_I_TO_I
  //      GL_PIXEL_MAP_S_TO_S
  //      GL_PIXEL_MAP_I_TO_R
  //      GL_PIXEL_MAP_I_TO_G
  //      GL_PIXEL_MAP_I_TO_B
  //      GL_PIXEL_MAP_I_TO_A
  //      GL_PIXEL_MAP_R_TO_R
  //      GL_PIXEL_MAP_G_TO_G
  //      GL_PIXEL_MAP_B_TO_B
  //      GL_PIXEL_MAP_A_TO_A

  // PixelStore
  //      GL_UNPACK_SWAP_BYTES
  //      GL_UNPACK_LSB_FIRST
  //      GL_UNPACK_ROW_LENGTH
  //      GL_UNPACK_SKIP_ROWS
  //      GL_UNPACK_SKIP_PIXELS
  //      GL_UNPACK_ALIGNMENT
  //      GL_PACK_SWAP_BYTES
  //      GL_PACK_LSB_FIRST
  //      GL_PACK_ROW_LENGTH
  //      GL_PACK_SKIP_ROWS
  //      GL_PACK_SKIP_PIXELS
  //      GL_PACK_ALIGNMENT

  // PixelTransfer
  //      GL_MAP_COLOR
  //      GL_MAP_STENCIL
  //      GL_INDEX_SHIFT
  //      GL_INDEX_OFFSET
  //      GL_RED_SCALE
  //      GL_RED_BIAS
  //      GL_GREEN_SCALE
  //      GL_GREEN_BIAS
  //      GL_BLUE_SCALE
  //      GL_BLUE_BIAS
  //      GL_ALPHA_SCALE
  //      GL_ALPHA_BIAS
  //      GL_DEPTH_SCALE
  //      GL_DEPTH_BIAS

  // PixelType
  GL_BITMAP                         = $1A00;
  //      GL_BYTE
  //      GL_UNSIGNED_BYTE
  //      GL_SHORT
  //      GL_UNSIGNED_SHORT
  //      GL_INT
  //      GL_UNSIGNED_INT
  //      GL_FLOAT

  // PolygonMode
  GL_POINT                          = $1B00;
  GL_LINE                           = $1B01;
  GL_FILL                           = $1B02;

  // ReadBufferMode
  //      GL_FRONT_LEFT
  //      GL_FRONT_RIGHT
  //      GL_BACK_LEFT
  //      GL_BACK_RIGHT
  //      GL_FRONT
  //      GL_BACK
  //      GL_LEFT
  //      GL_RIGHT
  //      GL_AUX0
  //      GL_AUX1
  //      GL_AUX2
  //      GL_AUX3

  // RenderingMode
  GL_RENDER                         = $1C00;
  GL_FEEDBACK                       = $1C01;
  GL_SELECT                         = $1C02;

  // ShadingModel
  GL_FLAT                           = $1D00;
  GL_SMOOTH                         = $1D01;

  // StencilFunction
  //      GL_NEVER
  //      GL_LESS
  //      GL_EQUAL
  //      GL_LEQUAL
  //      GL_GREATER
  //      GL_NOTEQUAL
  //      GL_GEQUAL
  //      GL_ALWAYS

  // StencilOp
  //      GL_ZERO
  GL_KEEP                           = $1E00;
  GL_REPLACE                        = $1E01;
  GL_INCR                           = $1E02;
  GL_DECR                           = $1E03;
  //      GL_INVERT

  // StringName
  GL_VENDOR                         = $1F00;
  GL_RENDERER                       = $1F01;
  GL_VERSION                        = $1F02;
  GL_EXTENSIONS                     = $1F03;

  // TextureCoordName
  GL_S                              = $2000;
  GL_T                              = $2001;
  GL_R                              = $2002;
  GL_Q                              = $2003;

  // TexCoordPointerType
  //      GL_SHORT
  //      GL_INT
  //      GL_FLOAT
  //      GL_DOUBLE

  // TextureEnvMode
  GL_MODULATE                       = $2100;
  GL_DECAL                          = $2101;
  //      GL_BLEND
  //      GL_REPLACE

  // TextureEnvParameter
  GL_TEXTURE_ENV_MODE               = $2200;
  GL_TEXTURE_ENV_COLOR              = $2201;

  // TextureEnvTarget
  GL_TEXTURE_ENV                    = $2300;

  // TextureGenMode
  GL_EYE_LINEAR                     = $2400;
  GL_OBJECT_LINEAR                  = $2401;
  GL_SPHERE_MAP                     = $2402;

  // TextureGenParameter
  GL_TEXTURE_GEN_MODE               = $2500;
  GL_OBJECT_PLANE                   = $2501;
  GL_EYE_PLANE                      = $2502;

  // TextureMagFilter
  GL_NEAREST                        = $2600;
  GL_LINEAR                         = $2601;

  // TextureMinFilter
  //      GL_NEAREST
  //      GL_LINEAR
  GL_NEAREST_MIPMAP_NEAREST         = $2700;
  GL_LINEAR_MIPMAP_NEAREST          = $2701;
  GL_NEAREST_MIPMAP_LINEAR          = $2702;
  GL_LINEAR_MIPMAP_LINEAR           = $2703;

  // TextureParameterName
  GL_TEXTURE_MAG_FILTER             = $2800;
  GL_TEXTURE_MIN_FILTER             = $2801;
  GL_TEXTURE_WRAP_S                 = $2802;
  GL_TEXTURE_WRAP_T                 = $2803;
  //      GL_TEXTURE_BORDER_COLOR
  //      GL_TEXTURE_PRIORITY

  //Ville: Finns i opengl 1.4
  GL_GENERATE_MIPMAP                = $8191;
  GL_CONVOLUTION_2D                 = $8011;
  GL_MAX_TEXTURE_UNITS = 34018;

  // TextureTarget
  //      GL_TEXTURE_1D
  //      GL_TEXTURE_2D
  //      GL_PROXY_TEXTURE_1D
  //      GL_PROXY_TEXTURE_2D

  // TextureWrapMode
  GL_CLAMP                          = $2900;
  GL_REPEAT                         = $2901;

  // VertexPointerType
  //      GL_SHORT
  //      GL_INT
  //      GL_FLOAT
  //      GL_DOUBLE

  // ClientAttribMask
  GL_CLIENT_PIXEL_STORE_BIT         = $00000001;
  GL_CLIENT_VERTEX_ARRAY_BIT        = $00000002;
  GL_CLIENT_ALL_ATTRIB_BITS         = $FFFFFFFF;

  // polygon_offset
  GL_POLYGON_OFFSET_FACTOR          = $8038;
  GL_POLYGON_OFFSET_UNITS           = $2A00;
  GL_POLYGON_OFFSET_POINT           = $2A01;
  GL_POLYGON_OFFSET_LINE            = $2A02;
  GL_POLYGON_OFFSET_FILL            = $8037;

  // texture
  GL_ALPHA4                         = $803B;
  GL_ALPHA8                         = $803C;
  GL_ALPHA12                        = $803D;
  GL_ALPHA16                        = $803E;
  GL_LUMINANCE4                     = $803F;
  GL_LUMINANCE8                     = $8040;
  GL_LUMINANCE12                    = $8041;
  GL_LUMINANCE16                    = $8042;
  GL_LUMINANCE4_ALPHA4              = $8043;
  GL_LUMINANCE6_ALPHA2              = $8044;
  GL_LUMINANCE8_ALPHA8              = $8045;
  GL_LUMINANCE12_ALPHA4             = $8046;
  GL_LUMINANCE12_ALPHA12            = $8047;
  GL_LUMINANCE16_ALPHA16            = $8048;
  GL_INTENSITY                      = $8049;
  GL_INTENSITY4                     = $804A;
  GL_INTENSITY8                     = $804B;
  GL_INTENSITY12                    = $804C;
  GL_INTENSITY16                    = $804D;
  GL_R3_G3_B2                       = $2A10;
  GL_RGB4                           = $804F;
  GL_RGB5                           = $8050;
  GL_RGB8                           = $8051;
  GL_RGB10                          = $8052;
  GL_RGB12                          = $8053;
  GL_RGB16                          = $8054;
  GL_RGBA2                          = $8055;
  GL_RGBA4                          = $8056;
  GL_RGB5_A1                        = $8057;
  GL_RGBA8                          = $8058;
  GL_RGB10_A2                       = $8059;
  GL_RGBA12                         = $805A;
  GL_RGBA16                         = $805B;
  GL_TEXTURE_RED_SIZE               = $805C;
  GL_TEXTURE_GREEN_SIZE             = $805D;
  GL_TEXTURE_BLUE_SIZE              = $805E;
  GL_TEXTURE_ALPHA_SIZE             = $805F;
  GL_TEXTURE_LUMINANCE_SIZE         = $8060;
  GL_TEXTURE_INTENSITY_SIZE         = $8061;
  GL_PROXY_TEXTURE_1D               = $8063;
  GL_PROXY_TEXTURE_2D               = $8064;

  // texture_object
  GL_TEXTURE_PRIORITY               = $8066;
  GL_TEXTURE_RESIDENT               = $8067;
  GL_TEXTURE_BINDING_1D             = $8068;
  GL_TEXTURE_BINDING_2D             = $8069;

  // vertex_array
  GL_VERTEX_ARRAY                   = $8074;
  GL_NORMAL_ARRAY                   = $8075;
  GL_COLOR_ARRAY                    = $8076;
  GL_INDEX_ARRAY                    = $8077;
  GL_TEXTURE_COORD_ARRAY            = $8078;
  GL_EDGE_FLAG_ARRAY                = $8079;
  GL_VERTEX_ARRAY_SIZE              = $807A;
  GL_VERTEX_ARRAY_TYPE              = $807B;
  GL_VERTEX_ARRAY_STRIDE            = $807C;
  GL_NORMAL_ARRAY_TYPE              = $807E;
  GL_NORMAL_ARRAY_STRIDE            = $807F;
  GL_COLOR_ARRAY_SIZE               = $8081;
  GL_COLOR_ARRAY_TYPE               = $8082;
  GL_COLOR_ARRAY_STRIDE             = $8083;
  GL_INDEX_ARRAY_TYPE               = $8085;
  GL_INDEX_ARRAY_STRIDE             = $8086;
  GL_TEXTURE_COORD_ARRAY_SIZE       = $8088;
  GL_TEXTURE_COORD_ARRAY_TYPE       = $8089;
  GL_TEXTURE_COORD_ARRAY_STRIDE     = $808A;
  GL_EDGE_FLAG_ARRAY_STRIDE         = $808C;
  GL_VERTEX_ARRAY_POINTER           = $808E;
  GL_NORMAL_ARRAY_POINTER           = $808F;
  GL_COLOR_ARRAY_POINTER            = $8090;
  GL_INDEX_ARRAY_POINTER            = $8091;
  GL_TEXTURE_COORD_ARRAY_POINTER    = $8092;
  GL_EDGE_FLAG_ARRAY_POINTER        = $8093;
  GL_V2F                            = $2A20;
  GL_V3F                            = $2A21;
  GL_C4UB_V2F                       = $2A22;
  GL_C4UB_V3F                       = $2A23;
  GL_C3F_V3F                        = $2A24;
  GL_N3F_V3F                        = $2A25;
  GL_C4F_N3F_V3F                    = $2A26;
  GL_T2F_V3F                        = $2A27;
  GL_T4F_V4F                        = $2A28;
  GL_T2F_C4UB_V3F                   = $2A29;
  GL_T2F_C3F_V3F                    = $2A2A;
  GL_T2F_N3F_V3F                    = $2A2B;
  GL_T2F_C4F_N3F_V3F                = $2A2C;
  GL_T4F_C4F_N3F_V4F                = $2A2D;

  // Extensions
  GL_EXT_vertex_array               = 1;
  GL_WIN_swap_hint                  = 1;
  GL_EXT_bgra                       = 1;
  GL_EXT_paletted_texture           = 1;

  // EXT_vertex_array
  GL_VERTEX_ARRAY_EXT               = $8074;
  GL_NORMAL_ARRAY_EXT               = $8075;
  GL_COLOR_ARRAY_EXT                = $8076;
  GL_INDEX_ARRAY_EXT                = $8077;
  GL_TEXTURE_COORD_ARRAY_EXT        = $8078;
  GL_EDGE_FLAG_ARRAY_EXT            = $8079;
  GL_VERTEX_ARRAY_SIZE_EXT          = $807A;
  GL_VERTEX_ARRAY_TYPE_EXT          = $807B;
  GL_VERTEX_ARRAY_STRIDE_EXT        = $807C;
  GL_VERTEX_ARRAY_COUNT_EXT         = $807D;
  GL_NORMAL_ARRAY_TYPE_EXT          = $807E;
  GL_NORMAL_ARRAY_STRIDE_EXT        = $807F;
  GL_NORMAL_ARRAY_COUNT_EXT         = $8080;
  GL_COLOR_ARRAY_SIZE_EXT           = $8081;
  GL_COLOR_ARRAY_TYPE_EXT           = $8082;
  GL_COLOR_ARRAY_STRIDE_EXT         = $8083;
  GL_COLOR_ARRAY_COUNT_EXT          = $8084;
  GL_INDEX_ARRAY_TYPE_EXT           = $8085;
  GL_INDEX_ARRAY_STRIDE_EXT         = $8086;
  GL_INDEX_ARRAY_COUNT_EXT          = $8087;
  GL_TEXTURE_COORD_ARRAY_SIZE_EXT   = $8088;
  GL_TEXTURE_COORD_ARRAY_TYPE_EXT   = $8089;
  GL_TEXTURE_COORD_ARRAY_STRIDE_EXT = $808A;
  GL_TEXTURE_COORD_ARRAY_COUNT_EXT  = $808B;
  GL_EDGE_FLAG_ARRAY_STRIDE_EXT     = $808C;
  GL_EDGE_FLAG_ARRAY_COUNT_EXT      = $808D;
  GL_VERTEX_ARRAY_POINTER_EXT       = $808E;
  GL_NORMAL_ARRAY_POINTER_EXT       = $808F;
  GL_COLOR_ARRAY_POINTER_EXT        = $8090;
  GL_INDEX_ARRAY_POINTER_EXT        = $8091;
  GL_TEXTURE_COORD_ARRAY_POINTER_EXT = $8092;
  GL_EDGE_FLAG_ARRAY_POINTER_EXT    = $8093;
  GL_DOUBLE_EXT                     = GL_DOUBLE;

  // EXT_bgra
  GL_BGR_EXT                        = $80E0;
  GL_BGRA_EXT                       = $80E1;

  // EXT_paletted_texture

  // These must match the GL_COLOR_TABLE_*_SGI enumerants
  GL_COLOR_TABLE_FORMAT_EXT         = $80D8;
  GL_COLOR_TABLE_WIDTH_EXT          = $80D9;
  GL_COLOR_TABLE_RED_SIZE_EXT       = $80DA;
  GL_COLOR_TABLE_GREEN_SIZE_EXT     = $80DB;
  GL_COLOR_TABLE_BLUE_SIZE_EXT      = $80DC;
  GL_COLOR_TABLE_ALPHA_SIZE_EXT     = $80DD;
  GL_COLOR_TABLE_LUMINANCE_SIZE_EXT = $80DE;
  GL_COLOR_TABLE_INTENSITY_SIZE_EXT = $80DF;

  GL_COLOR_INDEX1_EXT               = $80E2;
  GL_COLOR_INDEX2_EXT               = $80E3;
  GL_COLOR_INDEX4_EXT               = $80E4;
  GL_COLOR_INDEX8_EXT               = $80E5;
  GL_COLOR_INDEX12_EXT              = $80E6;
  GL_COLOR_INDEX16_EXT              = $80E7;

  // For compatibility with OpenGL v1.0
  GL_LOGIC_OP                       = GL_INDEX_LOGIC_OP;
  GL_TEXTURE_COMPONENTS             = GL_TEXTURE_INTERNAL_FORMAT;

  //OpenGL 2.0
  GL_FRAGMENT_SHADER = $8B30;
  GL_VERTEX_SHADER = $8B31;
  GL_COMPILE_STATUS = $8B81;
  GL_LINK_STATUS = $8B82;
  GL_VALIDATE_STATUS = $8B83;

  GL_SHADING_LANGUAGE_VERSION = 35724;


  //VBOs
  GL_ARRAY_BUFFER = $8892;
  GL_ELEMENT_ARRAY_BUFFER = $8893;

  STATIC_DRAW = $88E4;

  //***** GL_EXT_framebuffer_object *****//
  GL_FRAMEBUFFER_EXT = $8D40;
  GL_RENDERBUFFER_EXT = $8D41;
  GL_STENCIL_INDEX_EXT = $8D45;
  GL_STENCIL_INDEX1_EXT = $8D46;
  GL_STENCIL_INDEX4_EXT = $8D47;
  GL_STENCIL_INDEX8_EXT = $8D48;
  GL_STENCIL_INDEX16_EXT = $8D49;
  GL_RENDERBUFFER_WIDTH_EXT = $8D42;
  GL_RENDERBUFFER_HEIGHT_EXT = $8D43;
  GL_RENDERBUFFER_INTERNAL_FORMAT_EXT = $8D44;
  GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE_EXT = $8CD0;
  GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME_EXT = $8CD1;
  GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL_EXT = $8CD2;
  GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE_EXT = $8CD3;
  GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_3D_ZOFFSET_EXT = $8CD4;
  GL_COLOR_ATTACHMENT0_EXT = $8CE0;
  GL_COLOR_ATTACHMENT1_EXT = $8CE1;
  GL_COLOR_ATTACHMENT2_EXT = $8CE2;
  GL_COLOR_ATTACHMENT3_EXT = $8CE3;
  GL_COLOR_ATTACHMENT4_EXT = $8CE4;
  GL_COLOR_ATTACHMENT5_EXT = $8CE5;
  GL_COLOR_ATTACHMENT6_EXT = $8CE6;
  GL_COLOR_ATTACHMENT7_EXT = $8CE7;
  GL_COLOR_ATTACHMENT8_EXT = $8CE8;
  GL_COLOR_ATTACHMENT9_EXT = $8CE9;
  GL_COLOR_ATTACHMENT10_EXT = $8CEA;
  GL_COLOR_ATTACHMENT11_EXT = $8CEB;
  GL_COLOR_ATTACHMENT12_EXT = $8CEC;
  GL_COLOR_ATTACHMENT13_EXT = $8CED;
  GL_COLOR_ATTACHMENT14_EXT = $8CEE;
  GL_COLOR_ATTACHMENT15_EXT = $8CEF;
  GL_DEPTH_ATTACHMENT_EXT = $8D00;
  GL_STENCIL_ATTACHMENT_EXT = $8D20;
  GL_FRAMEBUFFER_COMPLETE_EXT = $8CD5;
  GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT_EXT = $8CD6;
  GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT_EXT = $8CD7;
  GL_FRAMEBUFFER_INCOMPLETE_DUPLICATE_ATTACHMENT_EXT = $8CD8;
  GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS_EXT = $8CD9;
  GL_FRAMEBUFFER_INCOMPLETE_FORMATS_EXT = $8CDA;
  GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER_EXT = $8CDB;
  GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER_EXT = $8CDC;
  GL_FRAMEBUFFER_UNSUPPORTED_EXT = $8CDD;
  GL_FRAMEBUFFER_STATUS_ERROR_EXT = $8CDE;
  GL_FRAMEBUFFER_BINDING_EXT = $8CA6;
  GL_RENDERBUFFER_BINDING_EXT = $8CA7;
  GL_MAX_COLOR_ATTACHMENTS_EXT = $8CDF;
  GL_MAX_RENDERBUFFER_SIZE_EXT = $84E8;
  GL_INVALID_FRAMEBUFFER_OPERATION_EXT = $0506;


{******************************************************************************}

procedure gluPerspective(fovy,aspect,zNear,zFar : GLDouble);


var
  glBindTexture : procedure (target: GLenum; texture: GLuint); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glBlendFunc : procedure (sfactor, dfactor: GLenum); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glClear : procedure (mask: GLbitfield); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glClearColor : procedure (red, green, blue, alpha: GLclampf); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glColorPointer : procedure (size: GLint; atype: GLenum; stride: GLsizei; const pointer: Pointer); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glCopyTexImage2D : procedure (target: GLenum; level: GLint; internalFormat: GLenum; x, y: GLint; width, height: GLsizei; border: GLint); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glCopyTexSubImage2D : procedure (target: GLenum; level, xoffset, yoffset, x, y: GLint; width, height: GLsizei); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glCullFace : procedure (mode: GLenum); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glDeleteTextures : procedure (n: GLsizei; const textures: PGLuint); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glDepthFunc : procedure (func: GLenum); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glDepthMask : procedure (flag: GLboolean); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glDepthRange : procedure (zNear, zFar: GLclampd); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glDisable : procedure (cap: GLenum); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glDisableClientState : procedure (aarray: GLenum); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glDrawArrays : procedure (mode: GLenum; first: GLint; count: GLsizei); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glDrawElements : procedure (mode: GLenum; count: GLsizei; atype: GLenum; const indices: Pointer); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glEnable : procedure (cap: GLenum); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glEnableClientState : procedure (aarray: GLenum); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glFinish : procedure ; {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glFlush : procedure ; {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glFrontFace : procedure (mode: GLenum); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glGenLists : function (range: GLsizei): GLuint; {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glGenTextures : procedure (n: GLsizei; textures: PGLuint); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glGetError : function : GLenum; {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glGetFloatv : procedure (pname: GLenum; params: PGLfloat); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glGetIntegerv : procedure (pname: GLenum; params: PGLint); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glMateriali : procedure (face, pname: GLenum; param: GLint); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glTexParameteri : procedure (target: GLenum; pname: GLenum; param: GLint); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glGetString : function (name: GLenum): PAnsiChar; {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glHint : procedure (target, mode: GLenum); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glIndexMask : procedure (mask: GLuint); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glLightModelf : procedure (pname: GLenum; param: GLfloat); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glLightModelfv : procedure (pname: GLenum; const params: PGLfloat); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glLightfv : procedure (light, pname: GLenum; const params: PGLfloat); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glLineWidth : procedure (width: GLfloat); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glListBase : procedure (base: GLuint); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glLoadIdentity : procedure ; {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glLoadMatrixf : procedure (const m: PGLfloat); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glMaterialf : procedure (face, pname: GLenum; param: GLfloat); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glMaterialfv : procedure (face, pname: GLenum; const params: PGLfloat); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glMatrixMode : procedure (mode: GLenum); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glNormal3f : procedure (nx, ny, nz: GLfloat); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glNormalPointer : procedure (atype: GLenum; stride: GLsizei; const pointer: Pointer); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}

  glOrtho : procedure (left, right, bottom, top, zNear, zFar: GLdouble); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glFrustum : procedure (left, right, bottom, top, zNear, zFar: GLdouble); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glBegin : procedure (mode: GLenum); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glCallList : procedure (list: GLuint); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glTexImage1D : procedure (target: GLEnum; level, internalformat: GLint; width: GLsizei; border: GLint; format, atype: GLEnum; pixels: Pointer); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glTexSubImage1D : procedure (target: GLEnum; level, xoffset: GLint; width: GLsizei; format, atype: GLEnum; pixels: Pointer); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glColor3f : procedure (red, green, blue: GLfloat); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glColor3fv : procedure (const v: PGLfloat); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glColor4fv : procedure (const v: PGLfloat); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glEnd : procedure ; {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glColorMaterial : procedure (face, mode: GLenum); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glDeleteLists : procedure (list: GLuint; range: GLsizei); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glDrawBuffer : procedure (mode: GLenum); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glGetTexImage : procedure (target: GLenum; level: GLint; format: GLenum; atype: GLenum; pixels: Pointer); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glPolygonMode : procedure (face, mode: GLenum); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glPopAttrib : procedure ; {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glPushAttrib : procedure (mask: GLbitfield); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glRasterPos2f : procedure (x, y: GLfloat); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glTexCoord2f : procedure (s, t: GLfloat); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glVertex2f : procedure (x, y: GLfloat); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glVertex3f : procedure (x, y, z: GLfloat); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}

  glPointSize : procedure (size: GLfloat); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glPolygonOffset : procedure (factor, units: GLfloat); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glPopMatrix : procedure ; {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glPushMatrix : procedure ; {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glReadPixels : procedure (x, y: GLint; width, height: GLsizei; format, atype: GLenum; pixels: Pointer); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glRotatef : procedure (angle, x, y, z: GLfloat); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glScalef : procedure (x, y, z: GLfloat); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glScissor : procedure (x, y: GLint; width, height: GLsizei); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glShadeModel : procedure (mode: GLenum); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glTexCoordPointer : procedure (size: GLint; atype: GLenum; stride: GLsizei; const pointer: Pointer); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glTexGeni : procedure (coord: GLenum; pname: GLenum; param: GLint); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glTexImage2D : procedure (target: GLenum; level, internalformat: GLint; width, height: GLsizei; border: GLint; format, atype: GLenum; const pixels: Pointer); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glTexParameterf : procedure (target: GLenum; pname: GLenum; param: GLfloat); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glTranslatef : procedure (x, y, z: GLfloat); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glVertexPointer : procedure (size: GLint; atype: GLenum; stride: GLsizei; const pointer: Pointer); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glViewport : procedure (x, y: GLint; width, height: GLsizei); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}

  {$ifdef android}
  glColor4f : procedure(red, green, blue, alpha: GLfloat); cdecl;
  {$endif}


{$if defined(WIN32) or defined(WIN64)}
function wglGetProcAddress (P : pansichar) : pointer; stdcall; external 'opengl32.dll';
{$ifend}

var
   glActiveTexture : procedure(target: GLenum); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  //OpenGL 2.0
//  glBlendEquationSeparate: procedure(modeRGB: GLenum; modeAlpha: GLenum); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
//  glDrawBuffers: procedure(n: GLsizei; const bufs: PGLenum); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
//  glStencilOpSeparate: procedure(face: GLenum; sfail: GLenum; dpfail: GLenum; dppass: GLenum); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
//  glStencilFuncSeparate: procedure(frontfunc: GLenum; backfunc: GLenum; ref: GLint; mask: GLuint); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
//  glStencilMaskSeparate: procedure(face: GLenum; mask: GLuint); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glAttachShader: procedure(_program: GLuint; shader: GLuint); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glBindAttribLocation: procedure(_program: GLuint; index: GLuint; const name: PAnsiChar); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glCompileShader: procedure(shader: GLuint); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glCreateProgram: function(): GLuint; {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glCreateShader: function(_type: GLenum): GLuint; {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glDeleteProgram: procedure(_program: GLuint); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glDeleteShader: procedure(shader: GLuint); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glDetachShader: procedure(_program: GLuint; shader: GLuint); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
//  glDisableVertexAttribArray: procedure(index: GLuint); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
//  glEnableVertexAttribArray: procedure(index: GLuint); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
//  glGetActiveAttrib: procedure(_program: GLuint; index: GLuint; bufSize: GLsizei; length: PGLsizei; size: PGLint; _type: PGLenum; name: PGLchar); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
//  glGetActiveUniform: procedure(_program: GLuint; index: GLuint; bufSize: GLsizei; length: PGLsizei; size: PGLint; _type: PGLenum; name: PGLchar); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
//  glGetAttachedShaders: procedure(_program: GLuint; maxCount: GLsizei; count: PGLsizei; obj: PGLuint); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
//  glGetAttribLocation: function(_program: GLuint; const name: PGLchar): GLint; {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glGetProgramiv: procedure(_program: GLuint; pname: GLenum; params: PGLint); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
//  glGetProgramInfoLog: procedure(_program: GLuint; bufSize: GLsizei; length: PGLsizei; infoLog: PGLchar); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glGetShaderiv: procedure(shader: GLuint; pname: GLenum; params: PGLint); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glGetShaderInfoLog: procedure(shader: GLuint; bufSize: GLsizei; length: PGLsizei; infoLog: pointer); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glGetProgramInfoLog: procedure(prog: GLuint; bufSize: GLsizei; length: PGLsizei; infoLog: pointer); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
//  glGetShaderSource: procedure(shader: GLuint; bufSize: GLsizei; length: PGLsizei; source: PGLchar); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glGetUniformLocation: function(_program: GLuint; name: pointer): GLint; {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
//  glGetUniformfv: procedure(_program: GLuint; location: GLint; params: PGLfloat); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
//  glGetUniformiv: procedure(_program: GLuint; location: GLint; params: PGLint); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
//  glGetVertexAttribdv: procedure(index: GLuint; pname: GLenum; params: PGLdouble); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
//  glGetVertexAttribfv: procedure(index: GLuint; pname: GLenum; params: PGLfloat); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
//  glGetVertexAttribiv: procedure(index: GLuint; pname: GLenum; params: PGLint); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
//  glGetVertexAttribPointerv: procedure(index: GLuint; pname: GLenum; pointer: PGLvoid); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
//  glIsProgram: function(_program: GLuint): GLboolean; {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
//  glIsShader: function(shader: GLuint): GLboolean; {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glLinkProgram: procedure(_program: GLuint); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glShaderSource: procedure(shader: GLuint; count: GLsizei; s : pointer; const length: PGLint); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glUseProgram: procedure(_program: GLuint); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glUniform1f: procedure(location: GLint; v0: GLfloat); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
//  glUniform2f: procedure(location: GLint; v0: GLfloat; v1: GLfloat); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
//  glUniform3f: procedure(location: GLint; v0: GLfloat; v1: GLfloat; v2: GLfloat); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
//  glUniform4f: procedure(location: GLint; v0: GLfloat; v1: GLfloat; v2: GLfloat; v3: GLfloat); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glUniform1i: procedure(location: GLint; v0: GLint); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
//  glUniform2i: procedure(location: GLint; v0: GLint; v1: GLint); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
//  glUniform3i: procedure(location: GLint; v0: GLint; v1: GLint; v2: GLint); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
//  glUniform4i: procedure(location: GLint; v0: GLint; v1: GLint; v2: GLint; v3: GLint); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glUniform1fv: procedure(location: GLint; count: GLsizei; const value: PGLfloat); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
//  glUniform2fv: procedure(location: GLint; count: GLsizei; const value: PGLfloat); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
//  glUniform3fv: procedure(location: GLint; count: GLsizei; const value: PGLfloat); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
//  glUniform4fv: procedure(location: GLint; count: GLsizei; const value: PGLfloat); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
//  glUniform1iv: procedure(location: GLint; count: GLsizei; const value: PGLint); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
//  glUniform2iv: procedure(location: GLint; count: GLsizei; const value: PGLint); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
//  glUniform3iv: procedure(location: GLint; count: GLsizei; const value: PGLint); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
//  glUniform4iv: procedure(location: GLint; count: GLsizei; const value: PGLint); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
//  glUniformMatrix2fv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLfloat); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
//  glUniformMatrix3fv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLfloat); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glUniformMatrix4fv: procedure(location: GLint; count: GLsizei; transpose: GLboolean; const value: PGLfloat); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glValidateProgram: procedure(_program: GLuint); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
//  glVertexAttrib1d: procedure(index: GLuint; x: GLdouble); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
//  glVertexAttrib1dv: procedure(index: GLuint; const v: PGLdouble); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
//  glVertexAttrib1f: procedure(index: GLuint; x: GLfloat); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
//  glVertexAttrib1fv: procedure(index: GLuint; const v: PGLfloat); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
//  glVertexAttrib1s: procedure(index: GLuint; x: GLshort); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
//  glVertexAttrib1sv: procedure(index: GLuint; const v: PGLshort); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
//  glVertexAttrib2d: procedure(index: GLuint; x: GLdouble; y: GLdouble); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
//  glVertexAttrib2dv: procedure(index: GLuint; const v: PGLdouble); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
//  glVertexAttrib2f: procedure(index: GLuint; x: GLfloat; y: GLfloat); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
//  glVertexAttrib2fv: procedure(index: GLuint; const v: PGLfloat); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
//  glVertexAttrib2s: procedure(index: GLuint; x: GLshort; y: GLshort); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
//  glVertexAttrib2sv: procedure(index: GLuint; const v: PGLshort); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
//  glVertexAttrib3d: procedure(index: GLuint; x: GLdouble; y: GLdouble; z: GLdouble); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
//  glVertexAttrib3dv: procedure(index: GLuint; const v: PGLdouble); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
//  glVertexAttrib3f: procedure(index: GLuint; x: GLfloat; y: GLfloat; z: GLfloat); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
//  glVertexAttrib3fv: procedure(index: GLuint; const v: PGLfloat); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
//  glVertexAttrib3s: procedure(index: GLuint; x: GLshort; y: GLshort; z: GLshort); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
//  glVertexAttrib3sv: procedure(index: GLuint; const v: PGLshort); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
//  glVertexAttrib4Nbv: procedure(index: GLuint; const v: PGLbyte); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
//  glVertexAttrib4Niv: procedure(index: GLuint; const v: PGLint); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
//  glVertexAttrib4Nsv: procedure(index: GLuint; const v: PGLshort); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
//  glVertexAttrib4Nub: procedure(index: GLuint; x: GLubyte; y: GLubyte; z: GLubyte; w: GLubyte); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
//  glVertexAttrib4Nubv: procedure(index: GLuint; const v: PGLubyte); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
//  glVertexAttrib4Nuiv: procedure(index: GLuint; const v: PGLuint); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
//  glVertexAttrib4Nusv: procedure(index: GLuint; const v: PGLushort); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
//  glVertexAttrib4bv: procedure(index: GLuint; const v: PGLbyte); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
//  glVertexAttrib4d: procedure(index: GLuint; x: GLdouble; y: GLdouble; z: GLdouble; w: GLdouble); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
//  glVertexAttrib4dv: procedure(index: GLuint; const v: PGLdouble); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
//  glVertexAttrib4f: procedure(index: GLuint; x: GLfloat; y: GLfloat; z: GLfloat; w: GLfloat); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
//  glVertexAttrib4fv: procedure(index: GLuint; const v: PGLfloat); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
//  glVertexAttrib4iv: procedure(index: GLuint; const v: PGLint); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
//  glVertexAttrib4s: procedure(index: GLuint; x: GLshort; y: GLshort; z: GLshort; w: GLshort); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
//  glVertexAttrib4sv: procedure(index: GLuint; const v: PGLshort); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
//  glVertexAttrib4ubv: procedure(index: GLuint; const v: PGLubyte); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
//  glVertexAttrib4uiv: procedure(index: GLuint; const v: PGLuint); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
//  glVertexAttrib4usv: procedure(index: GLuint; const v: PGLushort); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
//  glVertexAttribPointer: procedure(index: GLuint; size: GLint; _type: GLenum; normalized: GLboolean; stride: GLsizei; const pointer: PGLvoid); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
//  glVertexAttribPointer: procedure(index: GLuint; size: GLint; _type: GLenum; normalized: GLboolean; stride: GLsizei; const pointer: PGLvoid); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}


  //Vertex buffer objects
  glBindBuffer : procedure(target: GLenum; buffer: GLuint); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glDeleteBuffers : procedure(n :GLsizei; const buffers: PGLuint); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glGenBuffers : procedure(n :GLsizei; buffers: PGLuint); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glBufferData : procedure(target: GLenum; size: integer; const data: pointer; usage: GLenum); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glBufferSubData : procedure(target: GLenum; offset: integer; size: integer; const data: pointer); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}

  //***** GL_EXT_framebuffer_object *****//
  glIsRenderbufferEXT: function(renderbuffer: GLuint): GLboolean; {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glBindRenderbufferEXT: procedure(target: GLenum; renderbuffer: GLuint); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glDeleteRenderbuffersEXT: procedure(n: GLsizei; const renderbuffers: PGLuint); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glGenRenderbuffersEXT: procedure(n: GLsizei; renderbuffers: PGLuint); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glRenderbufferStorageEXT: procedure(target: GLenum; internalformat: GLenum; width: GLsizei; height: GLsizei); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glGetRenderbufferParameterivEXT: procedure(target: GLenum; pname: GLenum; params: PGLint); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glIsFramebufferEXT: function(framebuffer: GLuint): GLboolean; {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glBindFramebufferEXT: procedure(target: GLenum; framebuffer: GLuint); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glDeleteFramebuffersEXT: procedure(n: GLsizei; const framebuffers: PGLuint); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glGenFramebuffersEXT: procedure(n: GLsizei; framebuffers: PGLuint); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glCheckFramebufferStatusEXT: function(target: GLenum): GLenum; {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glFramebufferTexture2DEXT: procedure(target: GLenum; attachment: GLenum; textarget: GLenum; texture: GLuint; level: GLint); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glFramebufferRenderbufferEXT: procedure(target: GLenum; attachment: GLenum; renderbuffertarget: GLenum; renderbuffer: GLuint); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glGetFramebufferAttachmentParameterivEXT: procedure(target: GLenum; attachment: GLenum; pname: GLenum; params: PGLint); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glGenerateMipmapEXT: procedure(target: GLenum); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}

  glDisableVertexAttribArray : procedure(index : GLuint); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glEnableVertexAttribArray : procedure(index : GLuint); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}
  glVertexAttribPointer : procedure(index : GLuint; size : GLint; _type : GLenum; normalized : GLboolean; stride : GLsizei; ptr : pointer); {$IFDEF WIN32}stdcall;{$ELSE}cdecl;{$ENDIF}


  ShadersSupported,MultiTextureSupported,VbosSupported,FbosSupported : boolean;



procedure LoadOpenGLExtensions(const Mode : integer);
procedure LoadOpenGL(const Mode : integer);

{$ifndef minimal}
procedure CheckGLError;
{$endif}

var
  LibGL : NativeUInt;

implementation


uses ZMath,ZLog,ZPlatform
  {$ifndef minimal}
  ,SysUtils
  {$endif}
  {$ifdef android}
  ,Math
  {$endif}
  ;

{$ifndef minimal}
procedure CheckGLError;
var
  Error : GLenum;
begin
  Error := glGetError;
  if Error<>0 then
    ZLog.GetLog('GL').Warning( 'GL ERROR: ' + IntToStr(Error) );
end;
{$endif}

//OpenGL 2.0
//Must be loaded as extensions on Windows because Opengl32.dll does not export 2.0 procs
const ExtFuncArray : packed array[0..36{$if (not defined(minimal)) or defined(android)}+5{$ifend}] of
  packed record
    Name : pansichar;
    Ptr : ^pointer;
  end =
(
(Name : 'glAttachShader'; Ptr : @@glAttachShader),
(Name : 'glBindAttribLocation'; Ptr : @@glBindAttribLocation),
//(Name : 'glBlendEquationSeparate'; Ptr : @@glBlendEquationSeparate),
(Name : 'glCompileShader'; Ptr : @@glCompileShader),
(Name : 'glCreateShader'; Ptr : @@glCreateShader),
(Name : 'glCreateProgram'; Ptr : @@glCreateProgram),
(Name : 'glDeleteShader'; Ptr : @@glDeleteShader),
(Name : 'glDeleteProgram'; Ptr : @@glDeleteProgram),
(Name : 'glDetachShader'; Ptr : @@glDetachShader),
//(Name : 'glDisableVertexAttribArray'; Ptr : @@glDisableVertexAttribArray),
//(Name : 'glDrawBuffers'; Ptr : @@glDrawBuffers),
//(Name : 'glEnableVertexAttribArray'; Ptr : @@glEnableVertexAttribArray),
//(Name : 'glGetAttribLocation'; Ptr : @@glGetAttribLocation),
//(Name : 'glGetActiveAttrib'; Ptr : @@glGetActiveAttrib),
//(Name : 'glGetAttachedShaders'; Ptr : @@glGetAttachedShaders),
//(Name : 'glGetActiveUniform'; Ptr : @@glGetActiveUniform),
//(Name : 'glGetProgramInfoLog'; Ptr : @@glGetProgramInfoLog),

{$if (not defined(minimal)) or defined(android)}
(Name : 'glGetProgramiv'; Ptr : @@glGetProgramiv),
(Name : 'glGetShaderiv'; Ptr : @@glGetShaderiv),
(Name : 'glGetShaderInfoLog'; Ptr : @@glGetShaderInfoLog),
(Name : 'glGetProgramInfoLog'; Ptr : @@glGetProgramInfoLog),
(Name : 'glValidateProgram'; Ptr : @@glValidateProgram),
{$ifend}

//(Name : 'glGetShaderSource'; Ptr : @@glGetShaderSource),
//(Name : 'glGetUniformiv'; Ptr : @@glGetUniformiv),
(Name : 'glGetUniformLocation'; Ptr : @@glGetUniformLocation),
//(Name : 'glGetUniformfv'; Ptr : @@glGetUniformfv),
//(Name : 'glGetVertexAttribPointerv'; Ptr : @@glGetVertexAttribPointerv),
//(Name : 'glGetVertexAttribiv'; Ptr : @@glGetVertexAttribiv),
//(Name : 'glGetVertexAttribfv'; Ptr : @@glGetVertexAttribfv),
//(Name : 'glGetVertexAttribdv'; Ptr : @@glGetVertexAttribdv),
//(Name : 'glIsProgram'; Ptr : @@glIsProgram),
//(Name : 'glIsShader'; Ptr : @@glIsShader),
(Name : 'glLinkProgram'; Ptr : @@glLinkProgram),
(Name : 'glShaderSource'; Ptr : @@glShaderSource),
//(Name : 'glStencilOpSeparate'; Ptr : @@glStencilOpSeparate),
//(Name : 'glStencilMaskSeparate'; Ptr : @@glStencilMaskSeparate),
//(Name : 'glStencilFuncSeparate'; Ptr : @@glStencilFuncSeparate),
//(Name : 'glUniform2f'; Ptr : @@glUniform2f),
//(Name : 'glUniform3fv'; Ptr : @@glUniform3fv),
(Name : 'glUniform1f'; Ptr : @@glUniform1f),
//(Name : 'glUniform4fv'; Ptr : @@glUniform4fv),
//(Name : 'glUniform4f'; Ptr : @@glUniform4f),
//(Name : 'glUniform2fv'; Ptr : @@glUniform2fv),
//(Name : 'glUniform3i'; Ptr : @@glUniform3i),
//(Name : 'glUniform2iv'; Ptr : @@glUniform2iv),
//(Name : 'glUniform4i'; Ptr : @@glUniform4i),
//(Name : 'glUniform3iv'; Ptr : @@glUniform3iv),
(Name : 'glUniform1i'; Ptr : @@glUniform1i),
//(Name : 'glUniform3f'; Ptr : @@glUniform3f),
//(Name : 'glUniform4iv'; Ptr : @@glUniform4iv),
//(Name : 'glUniform1iv'; Ptr : @@glUniform1iv),
//(Name : 'glUniformMatrix2fv'; Ptr : @@glUniformMatrix2fv),
(Name : 'glUniform1fv'; Ptr : @@glUniform1fv),
//(Name : 'glUniformMatrix3fv'; Ptr : @@glUniformMatrix3fv),
(Name : 'glUniformMatrix4fv'; Ptr : @@glUniformMatrix4fv),
//(Name : 'glUniform2i'; Ptr : @@glUniform2i),
(Name : 'glUseProgram'; Ptr : @@glUseProgram),
 //Multitexture
(Name : 'glActiveTexture'; Ptr : @@glActiveTexture),
 //VBO
(Name : 'glBindBuffer'; Ptr : @@glBindBuffer),
(Name : 'glDeleteBuffers'; Ptr : @@glDeleteBuffers),
(Name : 'glGenBuffers'; Ptr : @@glGenBuffers),
(Name : 'glBufferData'; Ptr : @@glBufferData),
(Name : 'glBufferSubData'; Ptr : @@glBufferSubData),
//FBO
(Name : 'glIsRenderbufferEXT'; Ptr : @@glIsRenderbufferEXT),
(Name : 'glBindRenderbufferEXT'; Ptr : @@glBindRenderbufferEXT),
(Name : 'glDeleteRenderbuffersEXT'; Ptr : @@glDeleteRenderbuffersEXT),
(Name : 'glGenRenderbuffersEXT'; Ptr : @@glGenRenderbuffersEXT),
(Name : 'glRenderbufferStorageEXT'; Ptr : @@glRenderbufferStorageEXT),
(Name : 'glBindFramebufferEXT'; Ptr : @@glBindFramebufferEXT),
(Name : 'glDeleteFramebuffersEXT'; Ptr : @@glDeleteFramebuffersEXT),
(Name : 'glGenFramebuffersEXT'; Ptr : @@glGenFramebuffersEXT),
(Name : 'glCheckFramebufferStatusEXT'; Ptr : @@glCheckFramebufferStatusEXT),
(Name : 'glFramebufferTexture2DEXT'; Ptr : @@glFramebufferTexture2DEXT),
(Name : 'glFramebufferRenderbufferEXT'; Ptr : @@glFramebufferRenderbufferEXT),
(Name : 'glGenerateMipmapEXT'; Ptr : @@glGenerateMipmapEXT),

(Name : 'glDisableVertexAttribArray'; Ptr : @@glDisableVertexAttribArray),
(Name : 'glEnableVertexAttribArray'; Ptr : @@glEnableVertexAttribArray),
(Name : 'glVertexAttribPointer'; Ptr : @@glVertexAttribPointer)
);

procedure LoadOpenGLExtensions;
var
  I : integer;
begin
  //Opengl 2.0 extensions
  //These must be loaded after a rendering context is created, otherwise
  //wglGetProcAddress will return nil.

  for I := 0 to High(ExtFuncArray) do
    ExtFuncArray[I].Ptr^ := Platform_GLLoadProc(ExtFuncArray[I].Name);

  ShadersSupported := @glUseProgram<>nil;
  MultiTextureSupported := @glActiveTexture<>nil;
  VbosSupported := @glBindBuffer<>nil;
  {$ifdef linux}
  //VBOs crash on my ubuntu-linux setup.
  VbosSupported := False;
  {$endif}
  FbosSupported := @glIsRenderbufferEXT<>nil;
end;

{$ifdef Android} //Define dummys for functions not supported in GL ES 1.1
procedure fakeTexImage1D(target: GLEnum; level, internalformat: GLint; width: GLsizei; border: GLint; format, atype: GLEnum; pixels: Pointer);
begin end;

procedure fakeTexSubImage1D(target: GLEnum; level, xoffset: GLint; width: GLsizei; format, atype: GLEnum; pixels: Pointer);
begin end;

procedure fakeColor3f(red, green, blue: GLfloat);
begin
  glColor4f(red,green,blue,1);
end;

procedure fakeColor3fv(const v: PGLfloat);
var
  P : PGLFloat;
  r,g,b : single;
begin
  p := v;
  r := p^; inc(p);
  g := p^; inc(p);
  b := p^;
  glColor4f(r,g,b,1);
end;

procedure fakeColor4fv(const v: PGLfloat);
var
  P : PGLFloat;
  r,g,b,a : single;
begin
  p := v;
  r := p^; inc(p);
  g := p^; inc(p);
  b := p^; inc(p);
  a := p^;
  glColor4f(r,g,b,a);
end;

type
  PSaveAttribs = ^TSaveAttribs;
  TSaveAttribs =
  record
    Viewport : array[0..3] of integer;
    Lighting,Cullface,Texture2d : integer;
  end;

const
  SavedAttrib : PSaveAttribs = nil;

procedure fakePushAttrib(mask: GLbitfield);
var
  A : PSaveAttribs;
begin
  if SavedAttrib<>nil then
    Platform_Error('Nested attrib push');
  New(A);

  glGetIntegerv(GL_VIEWPORT, @A^.Viewport);
  glGetIntegerv(GL_LIGHTING, @A^.Lighting);
  glGetIntegerv(GL_CULL_FACE, @A^.Cullface);
  glGetIntegerv(GL_TEXTURE_2D, @A^.Texture2d);

  SavedAttrib := A;
end;

procedure fakePopAttrib;
var
  A : PSaveAttribs;
begin
  A := SavedAttrib;
  if A=nil then
    Platform_Error('Pop attrib without push');

  glViewport(A.Viewport[0],A.Viewport[1],A.Viewport[2],A.Viewport[3]);
  if A.Lighting=0 then
    glDisable(GL_LIGHTING)
  else
    glEnable(GL_LIGHTING);

  if A.Cullface=0 then
    glDisable(GL_CULL_FACE)
  else
    glEnable(GL_CULL_FACE);

  if A.Texture2d=0 then
    glDisable(GL_TEXTURE_2D)
  else
    glEnable(GL_TEXTURE_2D);

  Dispose(A);
  SavedAttrib := nil;
end;

procedure fakeBegin(mode: GLenum);
begin end;
procedure fakeCallList(list: GLuint);
begin end;
procedure fakeEnd;
begin end;
procedure fakeColorMaterial(face, mode: GLenum);
begin end;
procedure fakeDeleteLists(list: GLuint; range: GLsizei);
begin end;
procedure fakeDrawBuffer(mode: GLenum);
begin end;
procedure fakeGetTexImage(target: GLenum; level: GLint; format: GLenum; atype: GLenum; pixels: Pointer);
begin end;
procedure fakePolygonMode(face, mode: GLenum);
begin end;

procedure fakeRasterPos2f(x, y: GLfloat);
begin end;
procedure fakeTexCoord2f(s, t: GLfloat);
begin end;
procedure fakeVertex2f(x, y: GLfloat);
begin end;
procedure fakeVertex3f(x, y, z: GLfloat);
begin end;

procedure GlNotImp;
begin
  Platform_Error('Non-implemented GL function called');
end;
{$endif} //Android

const FuncArray : packed array[0..79{$ifdef android}+1{$endif}] of
  packed record
    Name : pansichar;
    Ptr : ^pointer;
  end =
(
(Name : 'glBindTexture'; Ptr : @@glBindTexture),
(Name : 'glBlendFunc'; Ptr : @@glBlendFunc),
(Name : 'glClear'; Ptr : @@glClear),
(Name : 'glClearColor'; Ptr : @@glClearColor),
(Name : 'glColorPointer'; Ptr : @@glColorPointer),
(Name : 'glCopyTexImage2D'; Ptr : @@glCopyTexImage2D),
(Name : 'glCopyTexSubImage2D'; Ptr : @@glCopyTexSubImage2D),
(Name : 'glCullFace'; Ptr : @@glCullFace),
(Name : 'glDeleteTextures'; Ptr : @@glDeleteTextures),
(Name : 'glDepthFunc'; Ptr : @@glDepthFunc),
(Name : 'glDepthMask'; Ptr : @@glDepthMask),
(Name : 'glDepthRange'; Ptr : @@glDepthRange),
(Name : 'glDisable'; Ptr : @@glDisable),
(Name : 'glDisableClientState'; Ptr : @@glDisableClientState),
(Name : 'glDrawArrays'; Ptr : @@glDrawArrays),
(Name : 'glDrawElements'; Ptr : @@glDrawElements),
(Name : 'glEnable'; Ptr : @@glEnable),
(Name : 'glEnableClientState'; Ptr : @@glEnableClientState),
(Name : 'glFinish'; Ptr : @@glFinish),
(Name : 'glFlush'; Ptr : @@glFlush),
(Name : 'glFrontFace'; Ptr : @@glFrontFace),
(Name : 'glGenLists'; Ptr : @@glGenLists),
(Name : 'glGenTextures'; Ptr : @@glGenTextures),
(Name : 'glGetError'; Ptr : @@glGetError),
(Name : 'glGetFloatv'; Ptr : @@glGetFloatv),
(Name : 'glGetIntegerv'; Ptr : @@glGetIntegerv),
(Name : 'glMateriali'; Ptr : @@glMateriali),
(Name : 'glTexParameteri'; Ptr : @@glTexParameteri),
(Name : 'glGetString'; Ptr : @@glGetString),
(Name : 'glHint'; Ptr : @@glHint),
(Name : 'glIndexMask'; Ptr : @@glIndexMask),
(Name : 'glLightModelf'; Ptr : @@glLightModelf),
(Name : 'glLightModelfv'; Ptr : @@glLightModelfv),
(Name : 'glLightfv'; Ptr : @@glLightfv),
(Name : 'glLineWidth'; Ptr : @@glLineWidth),
(Name : 'glListBase'; Ptr : @@glListBase),
(Name : 'glLoadIdentity'; Ptr : @@glLoadIdentity),
(Name : 'glLoadMatrixf'; Ptr : @@glLoadMatrixf),
(Name : 'glMaterialf'; Ptr : @@glMaterialf),
(Name : 'glMaterialfv'; Ptr : @@glMaterialfv),
(Name : 'glMatrixMode'; Ptr : @@glMatrixMode),
(Name : 'glNormal3f'; Ptr : @@glNormal3f),
(Name : 'glNormalPointer'; Ptr : @@glNormalPointer),
(Name : {$ifdef android}'glOrthof'{$else}'glOrtho'{$endif}; Ptr : @@glOrtho),
(Name : {$ifdef android}'glFrustumf'{$else}'glFrustum'{$endif}; Ptr : @@glFrustum),
(Name : 'glBegin'; Ptr : @@glBegin),
(Name : 'glCallList'; Ptr : @@glCallList),
(Name : 'glTexImage1D'; Ptr : @@glTexImage1D),
(Name : 'glTexSubImage1D'; Ptr : @@glTexSubImage1D),
(Name : 'glColor3f'; Ptr : @@glColor3f),
(Name : 'glColor3fv'; Ptr : @@glColor3fv),
(Name : 'glColor4fv'; Ptr : @@glColor4fv),
(Name : 'glEnd'; Ptr : @@glEnd),
(Name : 'glColorMaterial'; Ptr : @@glColorMaterial),
(Name : 'glDeleteLists'; Ptr : @@glDeleteLists),
(Name : 'glDrawBuffer'; Ptr : @@glDrawBuffer),
(Name : 'glGetTexImage'; Ptr : @@glGetTexImage),
(Name : 'glPolygonMode'; Ptr : @@glPolygonMode),
(Name : 'glPopAttrib'; Ptr : @@glPopAttrib),
(Name : 'glPushAttrib'; Ptr : @@glPushAttrib),
(Name : 'glRasterPos2f'; Ptr : @@glRasterPos2f),
(Name : 'glTexCoord2f'; Ptr : @@glTexCoord2f),
(Name : 'glVertex2f'; Ptr : @@glVertex2f),
(Name : 'glVertex3f'; Ptr : @@glVertex3f),
(Name : 'glPointSize'; Ptr : @@glPointSize),
(Name : 'glPolygonOffset'; Ptr : @@glPolygonOffset),
(Name : 'glPopMatrix'; Ptr : @@glPopMatrix),
(Name : 'glPushMatrix'; Ptr : @@glPushMatrix),
(Name : 'glReadPixels'; Ptr : @@glReadPixels),
(Name : 'glRotatef'; Ptr : @@glRotatef),
(Name : 'glScalef'; Ptr : @@glScalef),
(Name : 'glScissor'; Ptr : @@glScissor),
(Name : 'glShadeModel'; Ptr : @@glShadeModel),
(Name : 'glTexCoordPointer'; Ptr : @@glTexCoordPointer),
(Name : 'glTexGeni'; Ptr : @@glTexGeni),
(Name : 'glTexImage2D'; Ptr : @@glTexImage2D),
(Name : 'glTexParameterf'; Ptr : @@glTexParameterf),
(Name : 'glTranslatef'; Ptr : @@glTranslatef),
(Name : 'glVertexPointer'; Ptr : @@glVertexPointer),
(Name : 'glViewport'; Ptr : @@glViewport)
{$ifdef android}
,(Name : 'glColor4f'; Ptr : @@glColor4f)
{$endif}
);

procedure LoadOpenGL(const Mode : integer);
var
  I : integer;
  S : PAnsiChar;
begin
  {$IFDEF LINUX}
    {$IFDEF ANDROID}
    if Mode=0 then //Different modules depending on GLES version
      S := 'libGLESv1_CM.so'
    else
      S := 'libGLESv2.so';
    {$else}
    S := 'libGL.so';
    {$endif}
  {$ENDIF}
  {$IFDEF DARWIN}
  ///System/Library/Frameworks/OpenGL.framework/Libraries/
  S := 'libGL.dylib';
  {$ENDIF}
  {$if defined(WIN32) or defined(WIN64)}
  S := 'opengl32.dll';
  {$ifend}

  LibGL := Platform_LoadModule(S);
  if LibGL=0 then
    ZHalt('OpenGL could not be loaded');

  for I := 0 to High(FuncArray) do
    FuncArray[I].Ptr^ := Platform_GetModuleProc(LibGL, FuncArray[I].Name);

  {$ifdef android}
  for I := 0 to High(FuncArray) do
    if FuncArray[I].Ptr^=nil then
      FuncArray[I].Ptr^ := @GlNotImp;
  glTexImage1D := @fakeTexImage1D;
  glTexSubImage1D := @fakeTexSubImage1D;
  glColor3f := @fakeColor3f;
  glColor3fv := @fakeColor3fv;
  glColor4fv := @fakeColor4fv;
  glPushAttrib := @fakePushAttrib;
  glPopAttrib := @fakePopAttrib;
  glBegin := @fakeBegin;
  glCallList := @fakeCallList;
  glEnd := @fakeEnd;
  glColorMaterial := @fakeColorMaterial;
  glDeleteLists := @fakeDeleteLists;
  glDrawBuffer := @fakeDrawBuffer;
  glGetTexImage := @fakeGetTexImage;
  glPolygonMode := @fakePolygonMode;
  glRasterPos2f := @fakeRasterPos2f;
  glTexCoord2f := @fakeTexCoord2f;
  glVertex2f := @fakeVertex2f;
  glVertex3f := @fakeVertex3f;
  {$endif}
end;

//Ville:
//http://steinsoft.net/index.php?site=Programming/Code%20Snippets/OpenGL/gluperspective
procedure gluPerspective(fovy,aspect,zNear,zFar : GLDouble);
var
   xmin, xmax, ymin, ymax : GLdouble;
begin
  ymax := zNear * tan(fovy * PI / 360.0);
  ymin := -ymax;
  xmin := ymin * aspect;
  xmax := ymax * aspect;
  glFrustum(xmin, xmax, ymin, ymax, zNear, zFar);
end;






initialization

  {$IFNDEF FPC}
  {$IFNDEF __GPC__}
  Set8087CW($133F);
  {$ENDIF}
  {$ENDIF}

  {$ifdef android}
//  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
  {$endif}

end.

