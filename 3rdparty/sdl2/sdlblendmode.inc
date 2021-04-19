//from "sdl_blendmode.h"

{**
 *  The blend mode used in SDL_RenderCopy() and drawing operations.
 *}

type
  PSDL_BlendMode = ^TSDL_BlendMode;
  TSDL_BlendMode = DWord;

const
  SDL_BLENDMODE_NONE  = $00000000;    {**< No blending *}
  SDL_BLENDMODE_BLEND = $00000001;    {**< dst = (src * A) + (dst * (1-A)) *}
  SDL_BLENDMODE_ADD   = $00000002;    {**< dst = (src * A) + dst *}
  SDL_BLENDMODE_MOD   = $00000004;    {**< dst = src * dst *}

type
  PSDL_BlendOperation = ^TSDL_BlendOperation;
  TSDL_BlendOperation = DWord;

const
  SDL_BLENDOPERATION_ADD              = $1;  {**< dst + src: supported by all renderers *}
  SDL_BLENDOPERATION_SUBTRACT         = $2;  {**< dst - src : supported by D3D9, D3D11, OpenGL, OpenGLES}
  SDL_BLENDOPERATION_REV_SUBTRACT     = $3;  {**< src - dst : supported by D3D9, D3D11, OpenGL, OpenGLES}
  SDL_BLENDOPERATION_MINIMUM          = $4;  {**< min(dst, src) : supported by D3D11 *}
  SDL_BLENDOPERATION_MAXIMUM          = $5;  {**< max(dst, src) : supported by D3D11 *}

type
  PSDL_BlendFactor = ^TSDL_BlendFactor;
  TSDL_BlendFactor = DWord;

const
  SDL_BLENDFACTOR_ZERO                = $1;  {**< 0, 0, 0, 0 *}
  SDL_BLENDFACTOR_ONE                 = $2;  {**< 1, 1, 1, 1 *}
  SDL_BLENDFACTOR_SRC_COLOR           = $3;  {**< srcR, srcG, srcB, srcA *}
  SDL_BLENDFACTOR_ONE_MINUS_SRC_COLOR = $4;  {**< 1-srcR, 1-srcG, 1-srcB, 1-srcA *}
  SDL_BLENDFACTOR_SRC_ALPHA           = $5;  {**< srcA, srcA, srcA, srcA *}
  SDL_BLENDFACTOR_ONE_MINUS_SRC_ALPHA = $6;  {**< 1-srcA, 1-srcA, 1-srcA, 1-srcA *}
  SDL_BLENDFACTOR_DST_COLOR           = $7;  {**< dstR, dstG, dstB, dstA *}
  SDL_BLENDFACTOR_ONE_MINUS_DST_COLOR = $8;  {**< 1-dstR, 1-dstG, 1-dstB, 1-dstA *}
  SDL_BLENDFACTOR_DST_ALPHA           = $9;  {**< dstA, dstA, dstA, dstA *}
  SDL_BLENDFACTOR_ONE_MINUS_DST_ALPHA = $A;  {**< 1-dstA, 1-dstA, 1-dstA, 1-dstA *}

{**
 *  \brief Create a custom blend mode, which may or may not be supported by a given renderer
 *
 *  \param srcColorFactor source color factor
 *  \param dstColorFactor destination color factor
 *  \param colorOperation color operation
 *  \param srcAlphaFactor source alpha factor
 *  \param dstAlphaFactor destination alpha factor
 *  \param alphaOperation alpha operation
 *
 *  The result of the blend mode operation will be:
 *      dstRGB = dstRGB * dstColorFactor colorOperation srcRGB * srcColorFactor
 *  and
 *      dstA = dstA * dstAlphaFactor alphaOperation srcA * srcAlphaFactor
 *}
Function SDL_ComposeCustomBlendMode(srcColorFactor, dstColorFactor : TSDL_BlendFactor; colorOperation : TSDL_BlendOperation; srcAlphaFactor, dstAlphaFactor : TSDL_BlendFactor; alphaOperation : TSDL_BlendOperation) : TSDL_BlendMode cdecl;
  external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_ComposeCustomBlendMode' {$ENDIF} {$ENDIF};
