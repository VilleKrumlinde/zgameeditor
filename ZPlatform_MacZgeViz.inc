//This file is INCLUDEd in ZPlatform.pas

//Mac version to be used with ZGameEditor Visualizer


var
  //Current mouse pos, written to in eventhandling
  MacLastMouse : array[0..1] of integer;
  MacKeys : array[0..511] of boolean;


implementation

{$i-}  //No io-test on files, code tests ioresult

uses AudioPlayer, ZApplication, SyncObjs, Classes,
  {$ifdef fpc}dl,CocoaAll{$else}Posix.Dlfcn{$endif}, ZOpenGL,
  {$ifndef minimal}Sysutils{$endif};

var
  StartTime : double;

{$ifndef minimal}
var
  DesignerFilePath : string;

procedure Platform_DesignerSetFilePath(const P : string);
begin
  DesignerFilePath := P;
end;
{$endif}

procedure Platform_ShowMouse(Visible : boolean);
const vals : array[boolean] of integer = (0,1);
begin

end;

function Platform_InitScreen(var Width, Height : integer; UseFullScreen : boolean; Title : PAnsiChar; ZApp : pointer) : integer;
begin
  Result := 0;
end;

procedure Platform_SetWindowCaption(Title : PAnsiChar);
begin
end;

procedure Platform_ShutdownScreen;
begin
end;

procedure Platform_SwapBuffers;
begin
end;

procedure Platform_Run(Callback : TRunCallback);
begin
end;

{$ifndef fpc}
const
  libCoreAnimation = '/System/Library/Frameworks/QuartzCore.framework/QuartzCore';

function CACurrentMediaTime: double; cdecl; external libCoreAnimation name '_CACurrentMediaTime';
{$endif}

function Platform_GetTime : single;
var
  D : double;
begin
  D := CACurrentMediaTime - StartTime; //take care not to overflow
  Result := D;
end;

function Platform_IsKeyPressed(C : AnsiChar) : boolean;
begin
  Result := MacKeys[ Ord(C) ];
end;

function Platform_CommandLine(Switch : PAnsiChar) : boolean;
begin
end;

procedure Platform_Error(ErrorMessage : PAnsiChar);
begin
  WriteLn(ErrorMessage);
end;

function Platform_GetMousePos : TZPointi;
begin
  Result.x := MacLastMouse[0];
  Result.y := MacLastMouse[1];
end;

procedure Platform_SetMousePos(const X,Y : integer);
begin
end;

function Platform_GetExeFileName : PAnsiChar;
begin
  Result := '';
end;

procedure MakeFullPath(FileName : PAnsiChar; out Buf : array of AnsiChar);
begin
  Assert(Length(DesignerFilePath) + Length(FileName) < Length(Buf));
  if (Length(FileName)>0) and (FileName[0]='/') then
    ZStrCopy(PAnsiChar(@Buf),FileName)  //Absolute path specified
  else
  begin
    ZStrCopy(PAnsiChar(@Buf),PAnsiChar(UTF8String(DesignerFilePath)));
    ZStrCat(PAnsiChar(@Buf),FileName);
  end;
end;

procedure Platform_WriteFile(FileName : PAnsiChar; Memory : pointer; Size : integer; Append : Boolean);
var
  Buf : array[0..511] of AnsiChar;
  Written : longint;
  F : file;
begin
  MakeFullPath(FileName,Buf);
  FileName := @Buf;

  System.FileMode := 2;
  System.Assign(F,FileName);

  if Append then
  begin
    System.Reset(F,1);
    System.Seek(F,System.FileSize(F));
  end
  else
    System.Rewrite(F,1);

  if IOResult<>0 then
  begin
    Exit;
  end;

  System.BlockWrite(F,Memory^,Size,Written);

  System.Close(F);
end;

procedure Platform_ReadFile(FileName : PAnsiChar; var Memory : pointer; var Size : integer; IsRelative : Boolean);
var
  ms : TMemoryStream;
  name : string;
begin
  //Open file relative to path of exe-file
  if IsRelative then
    name := DesignerFilePath + string(FileName)
  else
    name := string(FileName);
    
  if FileExists(name) then
  begin
    ms := TMemoryStream.Create;
    ms.LoadFromFile(name);
    Size := ms.Size;
    GetMem(Memory, Size);
    Move(ms.Memory^, Memory^, Size);
    ms.Free;
  end;
end;

procedure Platform_InitGlobals;
begin
  
end;

procedure Platform_InitAudio;
begin
end;

procedure Platform_ShutdownAudio;
begin
end;

function Platform_CreateMutex : pointer;
begin
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

function Platform_GenerateFontDisplayLists(Size : integer; FirstChar,LastChar : integer) : integer;
begin
  Result := 0;
end;

function Platform_LoadLinkedResource : TZInputStream;
begin
  Result := nil;
end;

function Platform_GLLoadProc(const P : PAnsiChar) : pointer;
begin
  Result := Platform_GetModuleProc(ZOpenGL.LibGL, P);
end;

function Platform_ShowOptionDialog(App : pointer) : boolean;
begin
  Result := True;
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

function Platform_LoadModule(const Name : PAnsiChar) : NativeUInt;
{$ifndef minimal}
var
  s : string;
{$endif}
begin
  //Open file relative to path of exe-file
  {$ifndef minimal}
  //Inside designer we must provide full path to zgeproj-file
  s := ChangeFileExt(DesignerFilePath + String(Name),'.dylib');
  if FileExists(s) then
    Result := NativeUInt( dlopen(PAnsiChar(UTF8String(s)), RTLD_NOW) )
  else
    //Not found in zgeproj-path, assume system32-dll such as user32 and load without path
    Result := NativeUInt( dlopen(Name, RTLD_NOW) );
  {$else}
  //In engine always let the OS find the dll without path
  Result := NativeUInt( dlopen(Name, RTLD_NOW) );
  {$endif}
end;

{$ifndef minimal}
procedure Platform_FreeModule(Handle : NativeUInt);
begin
  // implement when needed
end;
{$endif}

function Platform_GetModuleProc(Module : NativeUInt; const Name : PAnsiChar) : pointer;
begin
  Result := dlsym(Module, Name );
end;

function Platform_GetSystemTime : integer;
begin
  Result := 0;
end;

procedure Platform_NetOpen(Url : PAnsiChar; InBrowser : boolean; WebOpen : pointer);
begin
end;

function Platform_NetRead(Handle,Buffer : pointer; Size : integer) : integer;
begin
  Result := 0;
end;

function Platform_TouchGetCount : integer;
begin
  Result := 0;
end;

function Platform_TouchGetPos(const TouchIndex : integer) : TZPointi;
begin
  Result.X := 0;
  Result.Y := 0;
end;

function Platform_TouchGetId(const TouchIndex : integer) : integer;
begin
  Result := 0;
end;

function Platform_CreateThread(ZThread : TZThread) : pointer;
begin
  Result := nil;
end;

procedure Platform_FreeThread(T : pointer);
begin

end;

function Platform_CreateEvent : pointer;
begin
  Result := TEvent.Create(nil,False,False,'');
end;

procedure Platform_WaitEvent(E : pointer);
begin
  TEvent(E).WaitFor(INFINITE);
end;

procedure Platform_SignalEvent(E : pointer);
begin
  TEvent(E).SetEvent;
end;

procedure Platform_FreeEvent(E : pointer);
begin
  TEvent(E).Free;
end;

function Platform_GetCpuCount : integer;
begin
  Result := 1;
end;

procedure Platform_DesignerSetAudioCallback(F : TDesignerAudioCallback);
begin

end;

initialization

  StartTime := CACurrentMediaTime;

end.
