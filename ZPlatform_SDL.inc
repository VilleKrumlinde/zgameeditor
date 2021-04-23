//This file is INCLUDEd in ZPlatform.pas

implementation

{$if defined(darwin) and defined(fpc)}
  //Must link with hacked main-function for sdl osx
  {$linklib SDLMain.o}
{$ifend}

{$i-}  //No io-test on files, code tests ioresult

uses sdl2, ZLog, AudioPlayer, ZApplication, SyncObjs
  {$ifdef MSWINDOWS},Windows{$endif}
  {$ifdef fpc}
    {$if defined(darwin) or defined(linux)},dl{$endif}
  {$endif}
  ;

var
  //Current mouse pos, written to in eventhandling
  SdlMousePos : TZPointi;
  Keys : array[0..511] of boolean;
  KeyMap : array[AnsiChar] of integer;
  gZApp: TZApplication;

procedure Platform_ShowMouse(Visible : boolean);
const vals : array[boolean] of integer = (0,1);
begin
  SDL_ShowCursor(vals[Visible]);
end;

{$ifdef macos}
//Need to init cocoa before calling SDL_SetVideoMode
//http://stackoverflow.com/questions/12641755/sdl-video-init-causes-exception-on-mac-os-x-10-8
procedure OsxInit;
var
  cocoa_lib : nativeuint;
  nsappload : procedure; cdecl;
begin
  cocoa_lib := dlopen( '/System/Library/Frameworks/Cocoa.framework/Cocoa', RTLD_LAZY );
  nsappload := dlsym( cocoa_lib, 'NSApplicationLoad');
  nsappload();
end;
{$endif}
var
  SdlScreen : PSDL_Window;

function Platform_InitScreen(var Width, Height : integer; UseFullScreen : boolean; Title : PAnsiChar; ZApp : pointer) : integer;
var
  Flags : SDL2.UINT32;
begin
  {$ifdef macos}
  OsxInit;
  {$endif}

  //Save a global handle to the app
  gZApp := TZApplication(ZApp);

  if UseFullScreen then
    flags := SDL_WINDOW_FULLSCREEN or SDL_WINDOW_OPENGL
  else
    flags := SDL_WINDOW_OPENGL or SDL_WINDOW_RESIZABLE;

  if Width=-1 then
  begin
    //Todo: support "current desktop resolution" on sdl
    Width := 800;
    Height := 600;
  end;

//  SDL_WM_SetCaption(Title, nil);

  SDL_GL_SetAttribute( SDL_GL_RED_SIZE, 8 );
  SDL_GL_SetAttribute( SDL_GL_GREEN_SIZE, 8 );
  SDL_GL_SetAttribute( SDL_GL_BLUE_SIZE, 8 );
  SDL_GL_SetAttribute( SDL_GL_DEPTH_SIZE, 16 );
  SDL_GL_SetAttribute( SDL_GL_DOUBLEBUFFER, 1 );


  SdlScreen := SDL_CreateWindow(Title,
                          SDL_WINDOWPOS_UNDEFINED,
                          SDL_WINDOWPOS_UNDEFINED,
                          Width, Height,
                          Flags);

  if (SdlScreen = nil) then
    ZHalt(SDL_GetError);

  SDL_GL_CreateContext(SdlScreen);

  //Returnera actual res
  SDL_GetWindowSize(SdlScreen,@Width,@Height);

  Result := 0;
end;

procedure Platform_SetWindowCaption(Title : PAnsiChar);
begin
  SDL_SetWindowTitle(SdlScreen,Title);
end;

procedure Platform_ShutdownScreen;
begin
  SDL_Quit;
end;

procedure Platform_SwapBuffers;
begin
  SDL_GL_SwapWindow(SdlScreen);
end;


procedure Platform_Run(Callback : TRunCallback);
//WinMain som behandlar meddelanden och anropar callback
var
  E : TSDL_Event;
  finished : Boolean;
begin
  Finished := False;
  while not Finished do
  begin
    while (SDL_PollEvent(@E)>0) do
    begin
      case E.type_ of
        SDL_QUITEV : Finished := True;
        SDL_KEYDOWN :
          begin
            Keys[ E.Key.keysym.sym and 511 ] := True;
          end;
        SDL_KEYUP :
          begin
            {$ifdef minimal}
            if (E.Key.keysym.sym=SDLK_ESCAPE) and gZApp.EscapeToQuit then
              Finished := True;
            {$endif}
            Keys[ E.Key.keysym.sym and 511 ] := False;
          end;
        SDL_MOUSEMOTION :
          begin
            SdlMousePos.X := E.motion.X;
            SdlMousePos.Y := E.motion.Y;
          end;
        SDL_MOUSEBUTTONDOWN,SDL_MOUSEBUTTONUP :
          begin
            //Button 1: Left mouse button
            //Button 2: Middle mouse button
            //Button 3: Right mouse button
            //4=wheel up, 5=wheel down
            case E.Button.button of
              1 : Keys[ Ord('{') ] := E.type_=SDL_MOUSEBUTTONDOWN;
              2 : Keys[ Ord('|') ] := E.type_=SDL_MOUSEBUTTONDOWN;
              3 : Keys[ Ord('}') ] := E.type_=SDL_MOUSEBUTTONDOWN;
              4 : gZApp.MouseWheelDelta := 1;
              5 : gZApp.MouseWheelDelta := -1;
            end;
          end;
      end;
    end;
    if not (Callback()) then
      Finished := True;
  end;
end;

function Platform_GetTime : single;
begin
  //todo from beginning of time
  Result := SDL_GetTicks/1000.0;
end;

function Platform_IsKeyPressed(C : AnsiChar) : boolean;
begin
  Result := Keys[ KeyMap[C] ];
end;

function Platform_CommandLine(Switch : PAnsiChar) : boolean;
var
  P : AnsiString;
  I : integer;
begin
  Result := False;
  I := 1;
  while I<=System.ParamCount do
  begin
    P := System.ParamStr(I);
    if (P[1]='-') and (P[2]=Switch[0]) then
    begin
      Result := True;
      Break;
    end;
    Inc(I);
  end;
end;

procedure Platform_Error(ErrorMessage : PAnsiChar);
begin
  WriteLn(ErrorMessage);
end;

function Platform_GetMousePos : TZPointi;
begin
  Result := SdlMousePos;
end;

procedure Platform_SetMousePos(const X,Y : integer);
begin
  SDL_WarpMouseInWindow(SdlScreen,X,Y);
end;

function Platform_GetExeFileName : PAnsiChar;
{$ifdef fpc}
var
  S : string;
  I : integer;
begin
  //In Fpc paramstr returns shortstring, must manually copy to PAnsiChar
  S := System.ParamStr(0);
  I := Length(S);
  if I=0 then
  begin
    //Upx-compressed binaries on Linux must read '   '-environ to get the name of executable
//todo: SDL_GetEnv missing in pascal headers?
    Result := ''; //SDL_GetEnv('   ');
  end
  else
  begin
    GetMem(Result,I+1);
    System.Move(S[1],Result^,I);
    Result[I] := #0;
  end;
{$else}
var
  AppName: array [0..1024] of Char;
  ResModuleName : string;
begin
  GetModuleFileName(0, AppName, Length(AppName));
  Result := PAnsiChar( AnsiString(PChar(@AppName)) );
  {$endif}
end;

procedure MakeFullPath(FileName : PAnsiChar; var Buf : array of AnsiChar);
var
  I : integer;
  Tmp : PAnsiChar;
begin
  Tmp := @Buf;
  ZStrCopy(Tmp,Platform_GetExeFileName);
  I := ZStrLength(Tmp);
  while (I>0) and (not (Tmp[I] in ['\','/'])) do
    Dec(I);
  if I>0 then
    Tmp[I+1] := #0;
  //Append filename
  ZStrCat(Tmp,FileName);
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
    {$ifdef zlog}
    ZLog.GetLog('Platform').Write('File could not be created: ' + FileName);
    {$endif}
    Exit;
  end;

  System.BlockWrite(F,Memory^,Size,Written);

  System.Close(F);
end;

procedure Platform_ReadFile(FileName : PAnsiChar; var Memory : pointer; var Size : integer; IsRelative : Boolean);
var
  F : file;
  Buf : array[0..511] of AnsiChar;
begin
  //Open file relative to path of exe-file
  if IsRelative then
  begin
    MakeFullPath(FileName,Buf);
    FileName := @Buf;
  end;

  System.FileMode := 0;
  System.Assign(F,FileName);

  System.Reset(F,1);
  if IOResult<>0 then
  begin
    {$ifdef zlog}
    ZLog.GetLog('Platform').Write('File missing: ' + FileName);
    {$endif}
    Exit;
  end;

  Size := System.FileSize(F);

  GetMem(Memory,Size);
  System.BlockRead(F,Memory^,Size);

  System.Close(F);
end;

procedure InitKeyMap;
var
  I : integer;
begin
  for I := 0 to 255 do
    KeyMap[AnsiChar(I)] := I;
  for I := ord('A') to ord('Z') do
    KeyMap[AnsiChar(I)] := I + (ord('a')-ord('A'));;

  KeyMap[UpKey] := SDL_SCANCODE_UP;
  KeyMap[DownKey] := SDL_SCANCODE_DOWN;
  KeyMap[LeftKey] := SDL_SCANCODE_LEFT;
  KeyMap[RightKey] := SDL_SCANCODE_RIGHT;
end;

procedure Platform_InitGlobals;
begin
  InitKeyMap;
end;


//Audio
var
  MixBuffer : array[0..SoundBufferFramesSize-1] of TSoundMixUnit;

procedure ClipAndCopy(Source : PSoundMixUnit; Dest : PFloat; FrameCount : integer);
begin
  Move(Source^,Dest^,FrameCount * SizeOf(TSoundMixUnit));
end;

procedure SdlAudioCallback(userdata: Pointer; stream: PUInt8; len: Integer ); cdecl;
var
  BytesToWrite,FramesToWrite : integer;
begin
  BytesToWrite := Len;

  FramesToWrite := BytesToWrite  div SizeOf(TSoundOutputUnit);

  FillChar(MixBuffer,FramesToWrite * SizeOf(TSoundMixUnit),0);

  Platform_EnterMutex(VoicesMutex);
    //Skriv SamplesToWrite samples från LastWritePosition
    //dela upp i två anrop ifall buffer-wrap
    AudioPlayer.RenderToMixBuffer(@MixBuffer[0],FramesToWrite);
  Platform_LeaveMutex(VoicesMutex);

  //Flytta paint buffer till dma-soundbuffer
  ClipAndCopy(@MixBuffer[0],Pointer(Stream),FramesToWrite);
end;

procedure Platform_InitAudio;
var
  Desired, Obtained: TSDL_AudioSpec;
begin
  FillChar(Desired,SizeOf(Desired),0);
  desired.freq := AudioPlayer.AudioRate;
  desired.format:=AUDIO_F32LSB;
  desired.channels:=AudioPlayer.StereoChannels;
  desired.samples:=AudioPlayer.SoundBufferFramesSize;
  desired.callback:=@SdlAudioCallback;
//  desired->userdata=NULL;

  if SDL_OpenAudio(@Desired,@Obtained)<0 then
    ZHalt('Could not initialize audio. Start with -s for no sound.');

  if (Obtained.Freq<>Desired.Freq) or
    (Obtained.Format<>Desired.Format) or
    (Obtained.Channels<>Desired.Channels) then
    ZHalt('Could not initialize audio. Start with -s for no sound.');

  //Start playing
  SDL_PauseAudio(0);
end;

procedure Platform_ShutdownAudio;
begin
  SDL_CloseAudio;
end;

function Platform_CreateMutex : pointer;
begin
  Result := SDL_CreateMutex();
end;

procedure Platform_FreeMutex(P : pointer);
begin
  SDL_DestroyMutex(PSDL_mutex(P));
end;

procedure Platform_EnterMutex(P : pointer);
begin
  SDL_LockMutex(PSDL_mutex(P));
end;

procedure Platform_LeaveMutex(P : pointer);
begin
  SDL_UnlockMutex(PSDL_mutex(P));
end;

function Platform_GenerateFontDisplayLists(Size : integer; FirstChar,LastChar : integer) : integer;
begin
  //todo
  Result := 0;
end;

function Platform_LoadLinkedResource : TZInputStream;
begin
  //todo
  Result := nil;
end;

function Platform_GLLoadProc(const P : PAnsiChar) : pointer;
begin
  Result := SDL_GL_GetProcAddress(P);
end;

{$IFDEF LINUX}
function Platform_ShowOptionDialog(App : pointer) : boolean;
const
  xstr = 'x'#0;
  xdef = 'xmessage "Choose screen resolution:" -center -buttons '#0;
  xcom = ','#0;
var
  I : integer;
  Tmp1 : array[0..10] of AnsiChar;
  Str : array[0..511] of AnsiChar;
begin
  Str[0]:=#0;
  ZStrCat(Str,Xdef);

  for I := 0 to High(ScreenModes) do
  begin
    //inttofloat conversion
    ZStrConvertInt(ScreenModes[I].W, PAnsiChar(@Tmp1));
    ZStrCat(Str,Tmp1);
    ZStrCat(Str,xstr);
    ZStrConvertInt(ScreenModes[I].H, PAnsiChar(@Tmp1));
    ZStrCat(Str,Tmp1);
    if I<High(ScreenModes) then
      ZStrCat(Str,xcom);
  end;

//TODO: import "System" call from libc

  //Get exitvalue by rightshift 8
//  I := libc.system(Str) shr 8;
  //First button is 101
//  if I>100 then
//    byte(TZApplication(App).ScreenMode) := I-101;

  Result := True;
end;
{$else}
function Platform_ShowOptionDialog(App : pointer) : boolean;
begin
  Result := True;
end;
{$endif}


function Platform_GetDisplayRefreshRate : integer;
begin
  //Not supported via SDL?
  Result := 0;
end;

procedure Platform_Sleep(time : integer);
begin
  SDL_Delay(time);
end;

function Platform_GetJoystickAxis(JoyId : integer; Axis : integer) : single;
begin
  //TODO: joystick support
  Result := 0;
end;

function Platform_GetJoystickButton(JoyId : integer; Button : integer) : boolean;
begin
  //TODO: joystick support
  Result := False;
end;

function Platform_GetJoystickPOV(JoyId : integer) : single;
begin
  //TODO: joystick support
  Result := -1;
end;

function Platform_LoadModule(const Name : PAnsiChar) : NativeUInt;
begin
  {$if Defined(Linux) or Defined(Darwin)}
  Result := NativeUInt(dlopen( Name, RTLD_NOW));
  {$else}
  Result := LoadLibraryA(Name);
  {$endif}
end;

function Platform_GetModuleProc(Module : NativeUInt; const Name : PAnsiChar) : pointer;
begin
  {$if Defined(Linux) or Defined(Darwin)}
  Result := dlsym({$ifdef fpc}Pointer(Module){$else}Module{$endif}, Name );
  {$else}
  Result := GetProcAddress(Module,Name);
  {$endif}
end;

function Platform_GetSystemTime : integer;
begin
  //TODO: System time support
  Result := 0;
end;

procedure Platform_NetOpen(Url : PAnsiChar; InBrowser : boolean; WebOpen : pointer);
begin
  //TODO: Net support
end;

function Platform_NetRead(Handle,Buffer : pointer; Size : integer) : integer;
begin
  //TODO: Net support
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
  Result := SDL_CreateSemaphore(0);
end;

procedure Platform_WaitEvent(E : pointer);
begin
  SDL_SemWait(PSDL_sem(E));
end;

procedure Platform_SignalEvent(E : pointer);
begin
  SDL_SemPost(PSDL_sem(E));
end;

procedure Platform_FreeEvent(E : pointer);
begin
  SDL_DestroySemaphore(PSDL_sem(E));
end;

function Platform_GetCpuCount : integer;
begin
  Result := 1;
end;

{$ifndef minimal}
procedure Platform_DesignerSetAudioCallback(F : TDesignerAudioCallback);
begin

end;

procedure Platform_DesignerSetFilePath(const P : AnsiString);
begin

end;
{$endif}

initialization

  // Initialize SDL
  if (SDL_Init(SDL_INIT_VIDEO) < 0) then
    ZHalt(SDL_GetError);

end.
