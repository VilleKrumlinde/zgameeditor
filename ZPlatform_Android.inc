{Copyright (c) 2012 Ville Krumlinde

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

//This file is INCLUDEd in ZPlatform.pas

var
  AndroidCurrentMouse : TZPointi;
  AndroidKeys : array[ansichar] of boolean;
  AndroidPath : PAnsiChar;

implementation

uses ZOpenGL, ZLog, UnixType;

function __android_log_write(prio : integer; tag, text : PAnsiChar) : integer; cdecl; external 'liblog.so';
function gettimeofday( val : PTimeVal; tzp : Pointer ) : Integer; cdecl; external 'libc';

var
  TimerStart : single;
  StartTimeVal : TimeVal;
  KeyMap : array[AnsiChar] of AnsiChar;

procedure Platform_ShutdownScreen;
begin
end;

function Platform_InitScreen(var Width, Height : integer; UseFullScreen : boolean; Title : PAnsiChar; ZApp : pointer) : integer;
begin
  Result := 0;
end;

procedure Platform_SetWindowCaption(Title : PAnsiChar);
begin
end;

procedure Platform_ShowMouse(Visible : boolean);
begin
end;

procedure Platform_SwapBuffers;
begin
end;

procedure Platform_Run(Callback : TRunCallback);
begin
end;


function Platform_IsKeyPressed(C : AnsiChar) : boolean;
begin
  Result := AndroidKeys[ KeyMap[C] ];
end;

function Platform_CommandLine(Switch : PAnsiChar) : boolean;
begin
  if Switch='s' then
    Result := True
  else
    Result := False;
end;

procedure Platform_Error(ErrorMessage : PAnsiChar);
begin
  __android_log_write(6,'ZgeAndroid',ErrorMessage);
end;

function Platform_GetMousePos : TZPointi;
begin
  Result := AndroidCurrentMouse;
end;

procedure Platform_SetMousePos(const X,Y : integer);
begin
end;

function Platform_GetExeFileName : PAnsiChar;
begin
  Result := '';
end;

procedure Platform_WriteFile(FileName : PAnsiChar; Memory : pointer; Size : integer; Append : Boolean);
begin
end;

procedure Platform_ReadFile(FileName : PAnsiChar; var Memory : pointer; var Size : integer; IsRelative : Boolean);
begin
  Memory := nil;
  Size := 0;
end;

function Platform_GenerateFontDisplayLists(Size : integer; FirstChar,LastChar : integer) : integer;
begin
  Result := 0;
end;

function Platform_GetTime : single;
var
  timerTimeVal : TimeVal;
begin
  gettimeofday( @timerTimeVal, nil );
  timerTimeVal.tv_sec := timerTimeVal.tv_sec - StartTimeVal.tv_sec;
  Result := timerTimeVal.tv_sec + (timerTimeVal.tv_usec * 0.000001);
end;

function Platform_GetSystemTime : integer;
var
  timerTimeVal : TimeVal;
begin
  gettimeofday( @timerTimeVal, nil );
  Result := timerTimeVal.tv_sec;
end;

procedure Platform_DesignerSetDC(DC : longword; HWnd : longword);
begin
end;

{$i-}
function Platform_LoadLinkedResource : TZInputStream;
var
  F : file;
  Buf : array[0..511] of AnsiChar;
  Memory : pointer;
  Size : integer;
begin
  Result := nil;
  System.FileMode := 0;

  ZStrCopy(PAnsiChar(@Buf), AndroidPath);
  ZStrCat(PAnsiChar(@Buf), '/zzdc.dat');
  System.Assign(F,PAnsiChar(@Buf));

  System.Reset(F,1);
  if IOResult<>0 then
  begin
    Platform_Error('File missing zzdc.dat');
    Exit;
  end;

  Size := System.FileSize(F);

  GetMem(Memory,Size);
  System.BlockRead(F,Memory^,Size);

  System.Close(F);
  Result := TZInputStream.CreateFromMemory(Memory, Size);
end;


procedure Platform_InitAudio;
begin
end;


procedure Platform_ShutdownAudio;
begin
end;

function Platform_CreateMutex : pointer;
begin
  Result := nil;
end;

procedure Platform_FreeMutex(P : pointer);
begin
end;

procedure Platform_EnterMutex(P : pointer);
begin
end;

procedure Platform_LeaveMutex(P : pointer);
begin
end;

function Platform_GLLoadProc(const P : PAnsiChar) : pointer;
begin
  Result := nil;
end;

procedure Platform_InitGlobals;
var
  I : integer;
begin
  gettimeofday( @StartTimeVal, nil );

  AndroidCurrentMouse.X := 0;
  AndroidCurrentMouse.Y := 0;

  for I := 0 to 255 do
    KeyMap[chr(I)] := Chr(I);
  for I := ord('A') to ord('Z') do
    KeyMap[chr(I)] := Chr(I + (ord('a')-ord('A')));
end;

function Platform_ShowOptionDialog(App : pointer) : boolean;
begin
  Result := False;
end;

function Platform_GetDisplayRefreshRate : integer;
begin
  Result := 0;
end;

procedure Platform_Sleep(time : integer);
begin
end;

function Platform_GetJoystickAxis(JoyId : integer; Axis : integer) : single;
begin
  Result := 0;
end;

function Platform_GetJoystickButton(JoyId : integer; Button : integer) : boolean;
begin
  Result := False;
end;

function Platform_GetJoystickPOV(JoyId : integer) : single;
begin
  Result := -1;
end;


procedure Platform_NetOpen(Url : PAnsiChar; InBrowser : boolean; WebOpen : pointer);
begin
end;

procedure Platform_NetRead(Handle,Buffer : pointer; Size : integer);
begin
end;

function Platform_LoadModule(const Name : PAnsiChar) : integer;
begin
  Result := 0;
end;

function Platform_GetModuleProc(Module : integer; const Name : PAnsiChar) : pointer;
begin
  Result := nil;
end;

end.
