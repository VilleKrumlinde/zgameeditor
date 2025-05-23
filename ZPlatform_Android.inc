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
  MAX_AXES = 10;

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

uses ZOpenGL, ZLog, UnixType, cthreads, jni, AudioPlayer, SyncObjs, SysUtils, Math;

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

procedure ReadAssetFile(Filename: PAnsiChar; var Memory: pointer; var Size: integer);
var
  Env: PJNIEnv;
  AppClass: JClass;
  Mid: JMethodID;
  jniParams: array[0..0] of JValue; // For the (String) parameter
  JStr: jstring;
  Buffer: jbyteArray;
  PBuffer: pointer;
  IsCopy: JBoolean;
  BufferSize: JSize;
begin
  // Get the JNI environment
  CurVM^.GetEnv(CurVM, @Env, JNI_VERSION_1_6);

  // Log filename
  AndroidLog(PAnsiChar('Opening: ' + Filename));

  // Get class and method ID
  AppClass := Env^.GetObjectClass(Env, jobject(AndroidThis));
  Mid := Env^.GetMethodID(Env, AppClass, 'openAssetFile', '(Ljava/lang/String;)[B');

  // Create Java string and assign it to the parameter array
  JStr := Env^.NewStringUTF(Env, Filename);
  jniParams[0].l := JStr;

  // Call Java method using CallObjectMethodA
  Buffer := Env^.CallObjectMethodA(Env, jobject(AndroidThis), Mid, @jniParams[0]);

  // Clean up local ref for the string
  Env^.DeleteLocalRef(Env, JStr);

  // If Buffer is valid, extract data
  if Buffer <> nil then
  begin
    BufferSize := Env^.GetArrayLength(Env, Buffer);
    PBuffer := Env^.GetByteArrayElements(Env, Buffer, IsCopy);

    if PBuffer = nil then
      AndroidLog('Could not get buffer')
    else
    begin
      GetMem(Memory, BufferSize);
      Move(PBuffer^, Memory^, BufferSize);
      Size := BufferSize;

      // Release Java array
      Env^.ReleaseByteArrayElements(Env, Buffer, PBuffer, 0);
    end;
  end
  else
    AndroidLog('CallObjectMethodA returned null');
end;

procedure Platform_ReadFile(FileName : PAnsiChar; var Memory : pointer; var Size : integer; IsRelative : Boolean);
var
  F : file;
  Buf : array[0..511] of AnsiChar;
begin
  if Copy(FileName,1,7)='assets/' then
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
  MixBufferSamplesSize = SoundBufferFramesSize * 2; //todo skip *2 when float output
var
  MixBuffer : packed array[0..MixBufferSamplesSize-1] of TSoundMixUnit;
  AudioTerminated : boolean;

procedure ClipAndCopy(Source : PSoundMixUnit; Dest : PSmallInt; FrameCount : integer);
var
  I : integer;
  Val : integer;
begin
  for I := 0 to FrameCount-1 do
  begin //Convert from float to smallint. Todo: on Lollipop floats are accepted directly.

    //Fast version: no clipping
//    Val := Trunc(Source^ * High(Smallint));
//    Dest^ := Val;

    //Slow version
    Val := Round(Source^.Left * High(Smallint));
    if Val>High(Smallint) then
      Dest^ := High(Smallint)
    else if Val<Low(SmallInt) then
      Dest^ := Low(SmallInt)
    else
      Dest^ := Val;
    Inc(Dest);

    Val := Round(Source^.Right * High(Smallint));
    if Val>High(Smallint) then
      Dest^ := High(Smallint)
    else if Val<Low(SmallInt) then
      Dest^ := Low(SmallInt)
    else
      Dest^ := Val;
    Inc(Dest);

    Inc(Source);

  end;
end;

{procedure LogValue(const S : AnsiString; const Value : integer);
begin
  platform_error(pansichar(S + ': ' + ansistring(inttostr(Value))));
end;}

procedure MixAndCopyData(Destination : pointer; len: Integer);
var
  BytesToWrite,FramesToWrite : integer;
begin
  BytesToWrite := Len;

//  SamplesToWrite := BytesToWrite div (SizeOf(TSoundOutputUnit) * StereoChannels);
  FramesToWrite := BytesToWrite div (SizeOf(SmallInt) * StereoChannels); //todo change to above line when float output

  if FramesToWrite>High(MixBuffer) then
    //test for high limit on buffer
    Exit;

  FillChar(MixBuffer,FramesToWrite * SizeOf(TSoundMixUnit),0);

  Platform_EnterMutex(VoicesMutex);
    AudioPlayer.RenderToMixBuffer(@MixBuffer[0],FramesToWrite);
  Platform_LeaveMutex(VoicesMutex);

  //Flytta paint buffer till dma-soundbuffer
  ClipAndCopy(@MixBuffer[0],Destination,FramesToWrite);
end;


{$IFDEF CPU64}
function AudioCallback(P: pointer): Int64;
{$ELSE}
function AudioCallback(P: Pointer): LongInt;
{$ENDIF}
const
  OnePassSize = 512;
var
  Env : PJNIEnv;
  cAudioTrack : jclass;
  mGetMinBufferSize,mAudioTrack,mPlay,mStop,mRelease,mWrite : JMethodID;
  bufferSizeInBytes : integer;
  track : jobject;
  Buffer : jarray;
  Params : array[0..10] of JValue;
  PBuffer : pointer;
  IsCopy : JBoolean;

  function C(const args : array of const) : PJValue;
  var
    I : integer;
  begin
    Fillchar(Params, SizeOf(Params), 0);
     for I := 0 to High(args) do
      begin
        case args[I].VType of
          vtInteger: Params[I].i := args[I].VInteger; // Assign integer value
          vtPointer: Params[I].l := args[I].VPointer; // Assign pointer value
          else
            raise Exception.Create('Unsupported argument type');
        end;
      end;
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

  bufferSizeInBytes := Env^.CallStaticIntMethodA(env, cAudioTrack, mGetMinBufferSize, C([AudioPlayer.AudioRate, 3, 2]));
  //LogValue('BufferSize',bufferSizeInBytes);

  track := env^.NewObjectA(env, cAudioTrack, mAudioTrack, C([3, AudioPlayer.AudioRate, 3, 2, bufferSizeInBytes, 1]));
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
      MixAndCopyData(PBuffer, Min(OnePassSize, bufferSizeInBytes));
      env^.ReleasePrimitiveArrayCritical(env, buffer, pBuffer, 0);
      env^.CallNonvirtualIntMethodA(env, track, cAudioTrack, mWrite, C([buffer, 0, Min(OnePassSize, bufferSizeInBytes)]));

      //Can only sleep a very short while here, otherwise sound breaks up
      //Round(((SoundBufferSamplesSize/AudioRate)/16) * 1000) );
      Platform_Sleep( 1 );
    end;

    env^.CallNonvirtualVoidMethod(env, track, cAudioTrack, mStop);
    env^.CallNonvirtualVoidMethod(env, track, cAudioTrack, mRelease);
  end;

  Env^.PopLocalFrame(Env, nil);
  CurVM^.DetachCurrentThread(CurVM);
end;

procedure Platform_InitAudio;
var
  {$IFDEF CPU64}
  id: QWord;
  {$ELSE}
  id: LongWord;
  {$ENDIF}
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
  P := GetMem(SizeOf(TRTLCriticalSection));
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
  Math.SetExceptionMask([exZeroDivide,exOverflow,exUnderflow,exPrecision,exDenormalized,exInvalidOp]);

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
  //usleep takes param in 1/1e6 seconds.
  usleep(time * 1000);
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

procedure Platform_NetOpen(Url: PAnsiChar; InBrowser: boolean; WebOpen: pointer);
var
  Env: PJNIEnv;
  AppClass: JClass;
  Mid: JMethodID;
  jniParams: array[0..0] of JValue;
  JStr: jstring;

begin
  if InBrowser then
  begin
    // Get JNI environment
    CurVM^.GetEnv(CurVM, @Env, JNI_VERSION_1_6);

    AndroidLog(PAnsiChar('Opening: ' + Url));

    // Get class and method ID
    AppClass := Env^.GetObjectClass(Env, jobject(AndroidThis));
    Mid := Env^.GetMethodID(Env, AppClass, 'openURL', '(Ljava/lang/String;)V');

    // Create Java string and set as argument
    JStr := Env^.NewStringUTF(Env, Url);
    jniParams[0].l := JStr;

    // Call Java method using CallVoidMethodA
    Env^.CallVoidMethodA(Env, jobject(AndroidThis), Mid, @jniParams[0]);

    // Clean up local reference
    Env^.DeleteLocalRef(Env, JStr);
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
  Result := NativeUInt( dlopen(TheName,1) );
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

{$ifndef minimal}
procedure Platform_FreeModule(Handle : NativeUInt);
begin
end;
{$endif}

function Platform_GetModuleProc(Module : NativeUInt; const Name : PAnsiChar) : pointer;
begin
  Result := dlsym(Pointer(Module), Name);
  if Result=nil then
    Platform_Error(PChar('Function not found: ' + Name));
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

{$IFDEF CPU64}
function ThreadWrapper(Data : pointer) : Int64;
{$ELSE}
function ThreadWrapper(Data : pointer) : longint;
{$ENDIF}
begin
  TZThread(Data).Execute;
  EndThread( 0 );
  Result := 0;
end;

function Platform_CreateThread(ZThread : TZThread) : pointer;
var
  {$IFDEF CPU64}
  id: QWord;
  {$ELSE}
  id: LongWord;
  {$ENDIF}
begin
  BeginThread(ThreadWrapper, ZThread, id);
  Result := pointer(id);
end;

procedure Platform_FreeThread(T : pointer);
begin

end;

function Platform_CreateEvent : pointer;
begin
  Result := TEventObject.Create(nil,False,False,'');
end;

procedure Platform_WaitEvent(E : pointer);
begin
  TEventObject(E).WaitFor(INFINITE);
end;

procedure Platform_SignalEvent(E : pointer);
begin
  TEventObject(E).SetEvent;
end;

procedure Platform_FreeEvent(E : pointer);
begin
  TEventObject(E).Free;
end;

function Platform_GetCpuCount : integer;
var
  Env : PJNIEnv;
  RunClass : JClass;
  Mid : JMethodID;
  RunObj : JObject;
begin
  CurVM^.GetEnv(CurVM,@Env,JNI_VERSION_1_6);

  RunClass := Env^.FindClass(Env, 'java/lang/Runtime');

  Mid := Env^.GetStaticMethodID(Env, RunClass , 'getRuntime', '()Ljava/lang/Runtime;');
  RunObj := Env^.CallStaticObjectMethod(Env, RunClass, Mid);

  Mid := Env^.GetMethodID(Env, RunClass , 'availableProcessors', '()I');
  Result := Env^.CallIntMethod(Env,RunObj,Mid);
  AndroidLog( PChar('CPUs: ' + IntToStr(Result)) );
end;

end.
