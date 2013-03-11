{Copyright (c) 2008 Ville Krumlinde

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.}

program ZzDC;

{$WEAKLINKRTTI ON}

{$if defined(CPUX64)}
  {$EXCESSPRECISION OFF} //Needed for fast single-precision math
{$ifend}

{$if defined(Win32) or defined(Win64)}
  {$ifdef fpc}
    {$R Data.res}
  {$else}
    {$R Data.res Data.rc}
    {$SETPEFLAGS 1} // IMAGE_FILE_RELOCS_STRIPPED
  {$endif}
{$ifend}

//Zzap Dynamic Content engine

{
  Defines
    ZDEBUG    Extra debug code
    ZZDC_SDL  Use platform SDL

    MINIMAL              No debugcode and no editor support
    zzdc_screensaver     Win32 platform screensaver

    ZLOG      Logging active (large binary)
}

uses
  ZClasses in 'ZClasses.pas',
  ZOpenGL in 'ZOpenGL.pas',
  ZBitmap in 'ZBitmap.pas',
  BitmapProducers in 'BitmapProducers.pas',
  ZPlatform in 'ZPlatform.pas',
  ZApplication in 'ZApplication.pas',
  ZLog in 'ZLog.pas',
  Animators in 'Animators.pas',
  Meshes in 'Meshes.pas',
  Renderer in 'Renderer.pas',
  ZMath in 'ZMath.pas',
  Commands in 'Commands.pas',
  ZExpressions in 'ZExpressions.pas',
  Collision in 'Collision.pas',
  Steering in 'Steering.pas',
  AudioPlayer in 'AudioPlayer.pas',
  AudioComponents in 'AudioComponents.pas',
  ImplicitMeshes in 'ImplicitMeshes.pas',
  ZFile in 'ZFile.pas',
  NanoJpeg in 'NanoJpeg.pas',
  GLDrivers in 'GLDrivers.pas';

var
  ZApp : TZApplication;
begin

  ZApp := ZApplication.LoadApplicationComponent;
  ZApp.Run;
  ZApp.Free;

end.
