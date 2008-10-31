//This file is INCLUDEd in ZPlatform.pas

implementation

{$ifdef darwin}
  //Must link with hacked main-function for sdl osx
  {$linklib SDLMain.o}
{$endif}

uses SDL, ZLog, AudioPlayer,ZApplication
  {$IFDEF LINUX},libc{$ENDIF}
  ;

var
  //Current mouse pos, written to in eventhandling
  SdlMousePos : TZPointi;
  Keys : array[0..511] of boolean;
  KeyMap : array[char] of word;

procedure Platform_InitScreen(var Width, Height : integer; UseFullScreen : boolean; Title : PChar);
var
  Flags : SDL.UINT32;
  Mode : PSDL_Surface;
begin
  if UseFullScreen then
  begin
    flags := SDL_OPENGL or SDL_FULLSCREEN;
    //Stäng av mus
    SDL_ShowCursor(0);
  end
  else
    flags := SDL_OPENGL or SDL_RESIZABLE;

  SDL_WM_SetCaption(Title, nil);

  SDL_GL_SetAttribute( SDL_GL_RED_SIZE, 8 );
  SDL_GL_SetAttribute( SDL_GL_GREEN_SIZE, 8 );
  SDL_GL_SetAttribute( SDL_GL_BLUE_SIZE, 8 );
  SDL_GL_SetAttribute( SDL_GL_DEPTH_SIZE, 16 );
  SDL_GL_SetAttribute( SDL_GL_DOUBLEBUFFER, 1 );

  Mode := SDL_SetVideoMode(Width, Height, 0, flags);
  if (Mode = nil) then
    ZHalt(SDL_GetError);

  //Returnera actual res
  Width := Mode.W;
  Height := Mode.H;
end;

procedure Platform_ShutdownScreen;
begin
  SDL_Quit;
end;

procedure Platform_SwapBuffers;
begin
  SDL_GL_SwapBuffers;
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
            Keys[ E.Key.keysym.sym mod 512 ] := True;
          end;
        SDL_KEYUP :
          begin
            {$ifdef minimal}
            if E.Key.keysym.sym=SDLK_ESCAPE then
              Finished := True;
            {$endif}
            Keys[ E.Key.keysym.sym mod 512 ] := False;
          end;
        SDL_MOUSEMOTION :
          begin
            SdlMousePos.X := E.motion.X;
            SdlMousePos.Y := E.motion.Y;
          end;
        SDL_MOUSEBUTTONDOWN,SDL_MOUSEBUTTONUP :
          begin
            //Use '{' as key for left mouse button
            if E.Button.button=1 then
              Keys[ Ord('{') ] := E.type_=SDL_MOUSEBUTTONDOWN
            else
              Keys[ Ord('}') ] := E.type_=SDL_MOUSEBUTTONDOWN;
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

function Platform_IsKeyPressed(C : char) : boolean;
begin
  Result := Keys[ KeyMap[C] ];
end;

function Platform_CommandLine(Switch : PChar) : boolean;
var
  P : string;
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

procedure Platform_Error(ErrorMessage : PChar);
begin
  WriteLn(ErrorMessage);
end;

function Platform_GetMousePos : TZPointi;
begin
  Result := SdlMousePos;
end;

procedure Platform_SetMousePos(const X,Y : integer);
begin
  SDL_WarpMouse(X,Y);
end;

function Platform_GetExeFileName : PChar;
{$ifdef fpc}
var
  S : string;
  I : integer;
begin
  //In Fpc paramstr returns shortstring, must manually copy to pchar
  S := System.ParamStr(0);
  I := Length(S);
  if I=0 then
  begin
    //Upx-compressed binaries on Linux must read '   '-environ to get the name of executable
    Result := SDL_GetEnv('   ');
  end
  else
  begin
    GetMem(Result,I+1);
    System.Move(S[1],Result^,I);
    Result[I] := #0;
  end;
{$else}
begin
  Result := PChar(System.ParamStr(0));
{$endif}
end;

procedure MakeFullPath(FileName : PChar; var Buf : array of char);
var
  I : integer;
  Tmp : PChar;
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

procedure Platform_WriteFile(FileName : PChar; Memory : pointer; Size : integer; Append : Boolean);
var
  Buf : array[0..511] of char;
  Written : longint;
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

  System.BlockWrite(F,Memory^,Size,Written);

  System.Close(F);
end;

procedure Platform_ReadFile(FileName : PChar; var Memory : pointer; var Size : integer; IsRelative : Boolean);
var
  F : file;
  Buf : array[0..511] of char;
begin
  //Open file relative to path of exe-file
  if IsRelative then
  begin
    MakeFullPath(FileName,Buf);
    FileName := @Buf;
  end;

  System.FileMode := 0;
  System.Assign(F,FileName);
  if IOResult<>0 then
  begin
    {$ifdef zlog}
    ZLog.GetLog('Platform').Write('File missing: ' + FileName);
    {$endif}
    Exit;
  end;

  System.Reset(F,1);

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
    KeyMap[chr(I)] := I;
  for I := ord('A') to ord('Z') do
    KeyMap[chr(I)] := I + (ord('a')-ord('A'));;

  KeyMap[UpKey] := SDLK_UP;
  KeyMap[DownKey] := SDLK_DOWN;
  KeyMap[LeftKey] := SDLK_LEFT;
  KeyMap[RightKey] := SDLK_RIGHT;
end;

procedure Platform_InitGlobals;
begin
  InitKeyMap;
end;


//Audio
const
  MixBufferSamplesSize = SoundBufferSamplesSize;  //Måste vara lika stor som dma-buffern
var
  MixBuffer : array[0..(MixBufferSamplesSize*StereoChannels)-1] of TSoundMixUnit;

procedure ClipAndCopy(Source : PSoundMixUnit; Dest : PSoundOutputUnit; SampleCount : integer);
var
  I : integer;
  Val : TSoundMixUnit;
begin
  for I := 0 to (SampleCount * StereoChannels)-1 do
  begin
    Val := Source^ div (1 shl (MixPBits-OutputBits));
    if Val>High(TSoundOutputUnit) then
      Dest^ := High(TSoundOutputUnit)
    else if Val<Low(TSoundOutputUnit) then
      Dest^ := Low(TSoundOutputUnit)
    else
      Dest^ := Val;
    Inc(Source);
    Inc(Dest);
  end;
end;

procedure SdlAudioCallback(userdata: Pointer; stream: PUInt8; len: Integer ); cdecl;
var
  BytesToWrite,SamplesToWrite : integer;
begin
  BytesToWrite := Len;
  SamplesToWrite := BytesToWrite div (SizeOf(TSoundOutputUnit) * StereoChannels);
  FillChar(MixBuffer,SamplesToWrite * SizeOf(TSoundMixUnit) * StereoChannels,0);

  Platform_EnterMutex(VoicesMutex);
    //Skriv SamplesToWrite samples från LastWritePosition
    //dela upp i två anrop ifall buffer-wrap
    AudioPlayer.RenderToMixBuffer(@MixBuffer[0],SamplesToWrite);
  Platform_LeaveMutex(VoicesMutex);

  //Flytta paint buffer till dma-soundbuffer
  ClipAndCopy(@MixBuffer[0],Pointer(Stream),SamplesToWrite);
end;

procedure Platform_InitAudio;
var
  Desired, Obtained: TSDL_AudioSpec;
begin
  FillChar(Desired,SizeOf(Desired),0);
  desired.freq := AudioPlayer.AudioRate;
  if AudioPlayer.OutputBits=16 then
    desired.format:=AUDIO_S16LSB
  else
    ZHalt('only 16 bit audio supported');
  desired.channels:=AudioPlayer.StereoChannels;
  desired.samples:=AudioPlayer.SoundBufferSamplesSize;
  desired.callback:=@SdlAudioCallback;
//  desired->userdata=NULL;

  if SDL_OpenAudio(@Desired,@Obtained)<0 then
    ZHalt('Could not initialize audio. Start with -s for no sound.');

  if (Obtained.Freq<>Desired.Freq) or
    (Obtained.Format<>Desired.Format) or
    (Obtained.Samples<>Desired.Samples) or
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
  SDL_mutexP(PSDL_mutex(P));
end;

procedure Platform_LeaveMutex(P : pointer);
begin
  SDL_mutexV(PSDL_mutex(P));
end;

function Platform_GenerateFontDisplayLists(Size : integer; FirstChar,LastChar : integer) : integer;
begin
  //todo
  result := 0;
end;

function Platform_LoadLinkedResource : TZInputStream;
begin
  //todo
  result := nil;
end;

function Platform_GLLoadProc(const P : PChar) : pointer;
begin
  Result := SDL_GL_GetProcAddress(P);
end;

{$IFDEF LINUX}
function Platform_ShowOptionDialog : boolean;
const
  xstr = 'x'#0;
  xdef = 'xmessage "Choose screen resolution:" -center -buttons '#0;
  xcom = ','#0;
var
  I : integer;
  Tmp1 : array[0..10] of char;
  Str : array[0..511] of char;
begin
  Str[0]:=#0;
  ZStrCat(Str,Xdef);

  for I := 0 to High(ScreenModes) do
  begin
    //inttofloat conversion
    ZStrConvertFloat(ScreenModes[I].W, PChar(@Tmp1));
    ZStrCat(Str,Tmp1);
    ZStrCat(Str,xstr);
    ZStrConvertFloat(ScreenModes[I].H, PChar(@Tmp1));
    ZStrCat(Str,Tmp1);
    if I<High(ScreenModes) then
      ZStrCat(Str,xcom);
  end;

  //Get exitvalue by rightshift 8
  I := libc.system(Str) shr 8;
  //First button is 101
  if I>100 then
    byte(ZApp.ScreenMode) := I-101;

  Result := True;
end;
{$else}
function Platform_ShowOptionDialog : boolean;
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

initialization

  // Initialize SDL
  if (SDL_Init(SDL_INIT_VIDEO) < 0) then
    ZHalt(SDL_GetError);

end.
