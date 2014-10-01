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

procedure AndroidLog(const P : PAnsiChar);

const
  KeyPaused = AnsiChar(255);
  KeyResumed = AnsiChar(254);
  KeyBack = AnsiChar(253);

var
  AndroidCurrentMouse : TZPointi;
  AndroidKeys : array[ansichar] of boolean;
  AndroidPath,AndroidDataPath,AndroidLibraryPath : PAnsiChar;
  AndroidThis : pointer;

  AndroidContentPtr : pointer = nil;
  AndroidContentSize : integer;

  AndroidTouchCount : integer = 0;
  AndroidTouch : array[0..10] of record
    Id,X,Y : integer;
  end;


const
  MAX_JOYSTICKS = 16;
  MAX_AXES = 6;

type
  PJoystick = ^TJoystick;
  TJoystick =
    record
      Values : array[0..MAX_AXES-1] of single;
      Buttons : DWORD;
      POV : single;
    end;

var
  Joysticks : array[0..MAX_JOYSTICKS-1] of TJoystick;

implementation

uses ZOpenGL, ZLog, UnixType, cthreads, jni, AudioPlayer;

{$i-}  //No io-test on files, code tests ioresult

function __android_log_write(prio : integer; tag, text : PAnsiChar) : integer; cdecl; external 'liblog.so';
function gettimeofday( val : PTimeVal; tzp : Pointer ) : Integer; cdecl; external 'libc';
function select( n : longint; readfds, writefds, exceptfds : Pointer; var timeout : timeVal ) : longint; cdecl; external 'libc';
function usleep(__useconds:integer):longint;cdecl;external 'libc' name 'usleep';

function dlopen ( Name : PAnsiChar; Flags : longint) : Pointer; cdecl; external 'dl';
function dlclose( Lib : Pointer) : Longint; cdecl; external 'dl';
function dlsym  ( Lib : Pointer; Name : PAnsiChar) : Pointer; cdecl; external 'dl';


var
  StartTimeVal : TimeVal;
  KeyMap : array[AnsiChar] of AnsiChar;

procedure AndroidLog(const P : PAnsiChar);
begin
  __android_log_write(6,'ZgeAndroid',P);
end;

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
  if Result then
  begin
    if C in ['{', KeyPaused, KeyResumed, KeyBack] then
      //Actions that we auto reset
      AndroidKeys[ C ] := False;
  end;
end;

function Platform_CommandLine(Switch : PAnsiChar) : boolean;
begin
{  if Switch='s' then
    Result := True  //Disable audio
  else}
    Result := False;
end;

procedure Platform_Error(ErrorMessage : PAnsiChar);
begin
  AndroidLog(ErrorMessage);
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

procedure MakeFullPath(FileName,Path : PAnsiChar; var Buf : array of AnsiChar);
var
  Tmp : PAnsiChar;
begin
  Tmp := @Buf;
  if FileName^='/' then
    //If filename is absolute then use it directly
    ZStrCopy(Tmp,FileName)
  else
  begin
    //Otherwise use path + filename
    ZStrCopy(Tmp,Path);
    ZStrCat(Tmp,FileName);
  end;
end;

procedure Platform_WriteFile(FileName : PAnsiChar; Memory : pointer; Size : integer; Append : Boolean);
var
  Buf,MkBuf : array[0..511] of AnsiChar;
  Written : longint;
  I : integer;
  F : file;

  function TryCreate : boolean;
  begin
    System.Assign(F,FileName);

    if Append then
    begin
      System.Reset(F,1);
      System.Seek(F,System.FileSize(F));
    end
    else
      System.Rewrite(F,1);

    Result := IOResult=0;
    if not Result  then
    begin
      Platform_Error( PAnsiChar('File could not be created: ' + FileName) );
      System.Close(F);
    end;
  end;

begin
  MakeFullPath(FileName,AndroidDataPath,Buf);
  FileName := @Buf;

  System.FileMode := 2;
  if not TryCreate then
  begin
    ZStrCopy(PAnsiChar(@MkBuf),FileName);
    I := ZStrLength(PAnsiChar(@MkBuf))-1;
    while (I>0) and (MkBuf[I]<>'/') do
      Dec(I);
    if I>0 then
    begin
      MkBuf[I]:=#0;
      AndroidLog( PAnsiChar('Trying to create folder: ' + PAnsiChar(MkBuf)) );
      MkDir(PAnsiChar(MkBuf));
      if not TryCreate then
        Exit;
    end;
  end;

  System.BlockWrite(F,Memory^,Size,Written);

  System.Close(F);
end;

procedure ReadAssetFile(Filename : PAnsiChar; var Memory : pointer; var Size : integer);
var
  Env : PJNIEnv;
  AppClass : JClass;
  Mid : JMethodID;
  Params : array[0..10] of integer;
  Buffer: jarray;
  PBuffer : pointer;
  IsCopy : JBoolean;
  BufferSize : JSize;

  function C(const args : array of const) : pointer;
  var
    I : integer;
  begin
     for I := 0 to High(args) do
       Params[I] := args[I].vinteger;
     Result := @Params;
  end;

begin
  CurVM^.GetEnv(CurVM,@Env,JNI_VERSION_1_6);

  AndroidLog( PAnsiChar('Opening: ' + Filename) );
  AppClass := Env^.GetObjectClass(Env, Jobject(AndroidThis));
  Mid := Env^.GetMethodID(Env, AppClass , 'openAssetFile', '(Ljava/lang/String;)[B');
  Buffer := Env^.CallObjectMethodV(Env, Jobject(AndroidThis), Mid, C([Env^.NewStringUTF(Env,FileName)]) );

  BufferSize := Env^.GetArrayLength(Env,Buffer);

  PBuffer := Env^.GetByteArrayElements(Env, Buffer, IsCopy);
  if PBuffer=nil then
    AndroidLog('could not get buffer')
  else
  begin
    GetMem(Memory, BufferSize);
    Move(PBuffer^, Memory^, BufferSize);
    Size := BufferSize;
    Env^.ReleaseByteArrayElements(Env, Buffer, PBuffer, 0);
  end;
end;

procedure Platform_ReadFile(FileName : PAnsiChar; var Memory : pointer; var Size : integer; IsRelative : Boolean);
var
  F : file;
  Buf : array[0..511] of AnsiChar;
begin
  if Copy(FileName,1,8)='/assets/' then
  begin
    ReadAssetFile(FileName, Memory, Size);
  end else
  begin
    //Open file relative to path of app data dir
    if IsRelative then
    begin
      MakeFullPath(FileName,AndroidDataPath,Buf);
      FileName := @Buf;
    end;

    System.FileMode := 0;
    System.Assign(F,FileName);

    System.Reset(F,1);
    if IOResult<>0 then
    begin
      Platform_Error( PAnsiChar('File missing: ' + FileName) );
      Exit;
    end;

    Size := System.FileSize(F);

    GetMem(Memory,Size);
    System.BlockRead(F,Memory^,Size);

    System.Close(F);
  end;
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

function Platform_LoadLinkedResource : TZInputStream;
var
  F : file;
  Buf : array[0..511] of AnsiChar;
  Memory : pointer;
  Size : integer;
begin
  //Prioritize embedded content
  if AndroidContentPtr<>nil then
    Exit( TZInputStream.CreateFromMemory(AndroidContentPtr, AndroidContentSize) );

  //If no embedded content then try to load from external storage
  Result := nil;
  System.FileMode := 0;

  ZStrCopy(PAnsiChar(@Buf), AndroidPath);
  ZStrCat(PAnsiChar(@Buf), '/zzdc.dat');
  System.Assign(F,PAnsiChar(@Buf));

  System.Reset(F,1);
  if IOResult<>0 then
  begin
    AndroidLog(PAnsiChar(@Buf));
    Platform_Error('Platform_LoadLinkedResource: No data found');
    Exit;
  end;

  Size := System.FileSize(F);

  GetMem(Memory,Size);
  System.BlockRead(F,Memory^,Size);

  System.Close(F);
  Result := TZInputStream.CreateFromMemory(Memory, Size);
end;


const
  MixBufferSamplesSize = SoundBufferSamplesSize;
var
  MixBuffer : packed array[0..(MixBufferSamplesSize*StereoChannels)-1] of TSoundMixUnit;
  AudioTerminated : boolean;

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

{procedure LogValue(const S : AnsiString; const Value : integer);
begin
  platform_error(pansichar(S + ': ' + ansistring(inttostr(Value))));
end;}

procedure MixAndCopyData(Destination : pointer; len: Integer );
var
  BytesToWrite,SamplesToWrite : integer;
begin
  BytesToWrite := Len;

  SamplesToWrite := BytesToWrite div (SizeOf(TSoundOutputUnit) * StereoChannels);

  if SamplesToWrite*StereoChannels>High(MixBuffer) then
    //test for high limit on buffer
    Exit;

  FillChar(MixBuffer,SamplesToWrite * StereoChannels * SizeOf(TSoundMixUnit),0);

  Platform_EnterMutex(VoicesMutex);
    AudioPlayer.RenderToMixBuffer(@MixBuffer[0],SamplesToWrite);
  Platform_LeaveMutex(VoicesMutex);

  //Flytta paint buffer till dma-soundbuffer
  ClipAndCopy(@MixBuffer[0],Destination,SamplesToWrite);
end;


function AudioCallback(Data : pointer) : longint;
const
  OnePassSize = 512;
var
  Env : PJNIEnv;
  cAudioTrack : jclass;
  mGetMinBufferSize,mAudioTrack,mPlay,mStop,mRelease,mWrite : JMethodID;
  bufferSizeInBytes : integer;
  track : jobject;
  Buffer : jarray;
  Params : array[0..10] of integer;
  PBuffer : pointer;
  IsCopy : JBoolean;

  function C(const args : array of const) : pointer;
  var
    I : integer;
  begin
     for I := 0 to High(args) do
       Params[I] := args[I].vinteger;
     Result := @Params;
  end;

begin
  //Write to Java AudioTrack.
  //Concept from OpenAL: http://repo.or.cz/w/openal-soft/android.git/blob/HEAD:/Alc/backends/android.c

  Result := 0;

  Platform_error('audiocallback started');

  if CurVM^.AttachCurrentThread(CurVM,@Env,nil)<>0 then
  begin
    Platform_error('error attach thread');
    Exit;
  end;

  env^.PushLocalFrame(env, 2);

  cAudioTrack := env^.FindClass(env, 'android/media/AudioTrack');
  if cAudioTrack=nil then
  begin
    Platform_error('no audiotrack');
    Exit;
  end;

  mGetMinBufferSize := env^.GetStaticMethodID(env, cAudioTrack, 'getMinBufferSize', '(III)I');
  mAudioTrack := env^.GetMethodID(env, cAudioTrack, '<init>', '(IIIIII)V');
  mPlay := env^.GetMethodID(env, cAudioTrack, 'play', '()V');
  mStop := env^.GetMethodID(env, cAudioTrack, 'stop', '()V');
  mRelease := env^.GetMethodID(env, cAudioTrack, 'release', '()V');
  mWrite := env^.GetMethodID(env, cAudioTrack, 'write', '([BII)I');

  bufferSizeInBytes := Env^.CallStaticIntMethodV(env, cAudioTrack, mGetMinBufferSize, C([AudioPlayer.AudioRate, 3, 2]));
  //LogValue('BufferSize',bufferSizeInBytes);

  track := env^.NewObjectV(env, cAudioTrack, mAudioTrack, C([3, AudioPlayer.AudioRate, 3, 2, bufferSizeInBytes, 1]));
  if track=nil then
    Platform_error('could not create track')
  else
  begin
    env^.CallNonvirtualVoidMethod(env, track, cAudioTrack, mPlay);

    buffer := env^.NewByteArray(env, bufferSizeInBytes);

    while not AudioTerminated do
    begin
      //Write in small steps to avoid having to lock VoicesMutex for long
      PBuffer := env^.GetPrimitiveArrayCritical(env, buffer, IsCopy);
      MixAndCopyData(PBuffer,OnePassSize);
      env^.ReleasePrimitiveArrayCritical(env, buffer, pBuffer, 0);
      env^.CallNonvirtualIntMethodV(env, track, cAudioTrack, mWrite, C([buffer, 0, OnePassSize]));

      Platform_Sleep( Round(((SoundBufferSamplesSize/AudioRate)/8) * 1000) );
    end;

    env^.CallNonvirtualVoidMethod(env, track, cAudioTrack, mStop);
    env^.CallNonvirtualVoidMethod(env, track, cAudioTrack, mRelease);
  end;

  Env^.PopLocalFrame(Env, nil);
  CurVM^.DetachCurrentThread(CurVM);
end;

procedure Platform_InitAudio;
var
  id : LongWord;
begin
  AudioTerminated := False;
  BeginThread( AudioCallback, nil, id);
end;

procedure Platform_ShutdownAudio;
begin
  AudioTerminated := True;
end;

function Platform_CreateMutex : pointer;
var
  P : PRTLCriticalSection;
begin
  P := GetMem(SizeOf(Pointer));
  InitCriticalSection(P^);
  Result := P;
end;

procedure Platform_FreeMutex(P : pointer);
begin
  DoneCriticalSection(PRTLCriticalSection(P)^);
  FreeMem(P);
end;

procedure Platform_EnterMutex(P : pointer);
begin
  EnterCriticalSection(PRTLCriticalSection(P)^);
end;

procedure Platform_LeaveMutex(P : pointer);
begin
  LeaveCriticalSection(PRTLCriticalSection(P)^);
end;

function Platform_GLLoadProc(const P : PAnsiChar) : pointer;
begin
  Result := Platform_GetModuleProc(ZOpenGL.LibGL, P);
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
//  for I := ord('A') to ord('Z') do
//    KeyMap[chr(I)] := Chr(I + (ord('a')-ord('A')));
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
  usleep(time);
end;

function Platform_GetJoystickAxis(JoyId : integer; Axis : integer) : single;
begin
  Result := 0;
  if JoyId<MAX_JOYSTICKS then
  begin
    if Axis<MAX_AXES then
      Result := Joysticks[JoyId].Values[Axis];
  end;
end;

function Platform_GetJoystickButton(JoyId : integer; Button : integer) : boolean;
begin
  Result := False;
  if JoyId<MAX_JOYSTICKS then
    Result := (Joysticks[JoyId].Buttons and (1 shl Button))>0;
end;

function Platform_GetJoystickPOV(JoyId : integer) : single;
begin
  Result := -1;
end;


procedure Platform_NetOpen(Url : PAnsiChar; InBrowser : boolean; WebOpen : pointer);
var
  Env : PJNIEnv;
  AppClass : JClass;
  Mid : JMethodID;
  Params : array[0..10] of integer;

  function C(const args : array of const) : pointer;
  var
    I : integer;
  begin
     for I := 0 to High(args) do
       Params[I] := args[I].vinteger;
     Result := @Params;
  end;

begin
  if InBrowser then
  begin
    CurVM^.GetEnv(CurVM,@Env,JNI_VERSION_1_6);
    AndroidLog( PAnsiChar('Opening: ' + Url) );
    AppClass := Env^.GetObjectClass(Env, Jobject(AndroidThis));
    Mid := Env^.GetMethodID(Env, AppClass , 'openURL', '(Ljava/lang/String;)V');
    Env^.CallVoidMethodV(Env, Jobject(AndroidThis), Mid, C([Env^.NewStringUTF(Env,Url)]) );
  end;
end;

function Platform_NetRead(Handle,Buffer : pointer; Size : integer) : integer;
begin
  Result := 0;
end;


function Platform_LoadModule(const Name : PAnsiChar) : NativeUInt;
var
  Buf : array[0..511] of AnsiChar;
  TheName : PAnsiChar;
  InitFunc : function(vm:PJavaVM;reserved:pointer):jint; cdecl;
begin
  if Name^='.' then
  begin
    MakeFullPath(Name,AndroidLibraryPath,Buf);
    TheName := @Buf;
  end
  else
    TheName := Name;
  Result := Integer( dlopen(TheName,1) );
  if Result=0 then
  begin
    Platform_Error( PAnsiChar('failed to load: ' + TheName) );
  end;

  InitFunc := Platform_GetModuleProc(Result,'JNI_OnLoad');
  if @InitFunc<>nil then
  begin
    AndroidLog('Found JNI_OnLoad function, calling now...');
    InitFunc(curVM,nil);
  end;

end;

function Platform_GetModuleProc(Module : NativeUInt; const Name : PAnsiChar) : pointer;
begin
  Result := dlsym(Pointer(Module), Name);
  if Result=nil then
    Platform_Error(Name);
end;

function Platform_TouchGetCount : integer;
begin
  Result := AndroidTouchCount;
end;

function Platform_TouchGetPos(const TouchIndex : integer) : TZPointi;
begin
  if TouchIndex>=AndroidTouchCount then
  begin
    Result.X := 0;
    Result.Y := 0;
  end
  else
  begin
    Result.X := AndroidTouch[TouchIndex].X;
    Result.Y := AndroidTouch[TouchIndex].Y;
  end;
end;

function Platform_TouchGetId(const TouchIndex : integer) : integer;
begin
  Result := 0;
  if TouchIndex<AndroidTouchCount then
    Result := AndroidTouch[TouchIndex].Id;
end;

end.
