{Copyright (c) 2020 Ville Krumlinde

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

unit ZPlatform;

{$include zzdc_globalopt.inc}

interface

uses ZClasses;

type
  TRunCallback = function : boolean of object;

procedure Platform_InitGlobals;

//W and H is desired rez, returns actual rez + window handle.
function Platform_InitScreen(var Width, Height : integer; UseFullScreen : boolean; Title : PAnsiChar; ZApp : pointer) : integer;
function Platform_GetDisplayRefreshRate : integer;
procedure Platform_SetWindowCaption(Title : PAnsiChar);

procedure Platform_ShutdownScreen;
procedure Platform_SwapBuffers;
procedure Platform_Run(Callback : TRunCallback);
function Platform_GetExeFileName : PAnsiChar;

//Return time in seconds since program start
function Platform_GetTime : single;
function Platform_GetSystemTime : integer;

function Platform_IsKeyPressed(C : AnsiChar) : boolean;

function Platform_GetMousePos : TZPointi;
procedure Platform_SetMousePos(const X,Y : integer);
procedure Platform_ShowMouse(Visible : boolean);

function Platform_CommandLine(Switch : PAnsiChar) : boolean;

procedure Platform_Error(ErrorMessage : PAnsiChar);
//procedure Platform_SimpleText(Scale,X,Y : single; Msg : PAnsiChar);

procedure Platform_ReadFile(FileName : PAnsiChar; var Memory : pointer; var Size : integer; IsRelative : Boolean);
procedure Platform_WriteFile(FileName : PAnsiChar; Memory : pointer; Size : integer; Append : Boolean);

procedure Platform_InitAudio;
procedure Platform_ShutdownAudio;

function Platform_CreateMutex : pointer;
procedure Platform_FreeMutex(P : pointer);
procedure Platform_EnterMutex(P : pointer);
procedure Platform_LeaveMutex(P : pointer);
procedure Platform_Sleep(time : integer);

function Platform_CreateThread(ZThread : TZThread) : pointer;
procedure Platform_FreeThread(T : pointer);

function Platform_CreateEvent : pointer;
procedure Platform_WaitEvent(E : pointer);
procedure Platform_SignalEvent(E : pointer);
procedure Platform_FreeEvent(E : pointer);

function Platform_GetCpuCount : integer;

function Platform_GenerateFontDisplayLists(Size : integer; FirstChar,LastChar : integer) : integer;
//function Platform_GenerateFontTexture(Char : integer) : integer;

function Platform_LoadLinkedResource : TZInputStream;
function Platform_GLLoadProc(const P : PAnsiChar) : pointer;

function Platform_ShowOptionDialog(App : pointer) : boolean;

function Platform_GetJoystickAxis(JoyId : integer; Axis : integer) : single;
function Platform_GetJoystickButton(JoyId : integer; Button : integer) : boolean;
function Platform_GetJoystickPOV(JoyId : integer) : single;

procedure Platform_NetOpen(Url : PAnsiChar; InBrowser : boolean; WebOpen : pointer);
function Platform_NetRead(Handle,Buffer : pointer; Size : integer) : integer;

function Platform_LoadModule(const Name : PAnsiChar) : NativeUInt;
function Platform_GetModuleProc(Module : NativeUInt; const Name : PAnsiChar) : pointer;

function Platform_TouchGetCount : integer;
function Platform_TouchGetPos(const TouchIndex : integer) : TZPointi;
function Platform_TouchGetId(const TouchIndex : integer) : integer;

{$ifndef minimal}
type
  TDesignerAudioCallback = procedure(P : pointer; FrameCount : integer);
procedure Platform_DesignerSetAudioCallback(F : TDesignerAudioCallback);
procedure Platform_DesignerSetFilePath(const P : string);
procedure Platform_FreeModule(Handle : NativeUInt);
{$endif}

const
  UpKey = '^';
  DownKey = '_';
  LeftKey = '<';
  RightKey = '>';


var
  //Contains keyup/down events since last frame
  KeyDownList,KeyUpList : TZArrayList;
  Platform_SyncWithMonitor : procedure(const Delay : integer);

type
  TScreenMode =
    packed record
      W,H : smallint;
    end;
const
  ScreenModes : packed array[0..5] of TScreenMode = (
(W:-1; H:-1),  //Fullscreen with desktop resolution
(W:640; H:480),
(W:800; H:600),
(W:1024; H:768),
(W:1280; H:800),
(W:1280; H:1024)
);

{$if Defined(ZZDC_SDL)}
  {$INCLUDE ZPlatform_SDL.inc}
{$elseif Defined(ANDROID)}
  {$INCLUDE ZPlatform_Android.inc}
{$elseif Defined(ZZDC_MacZgeViz)}
  {$INCLUDE ZPlatform_MacZgeViz.inc}
{$else}
  {$INCLUDE ZPlatform_Win32.inc}
{$endif}
