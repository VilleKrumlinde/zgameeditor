//This file is INCLUDEd in ZPlatform.pas

//Mac version to be used with ZGameEditor Visualizer


var
  //Current mouse pos, written to in eventhandling
  MacLastMouse : array[0..1] of integer;
  MacKeys : array[0..511] of boolean;

{$modeswitch objectivec1}

implementation

{$i-}  //No io-test on files, code tests ioresult

uses AudioPlayer, ZApplication, ZOpenGL,
  SyncObjs, Classes,
  dl, CocoaAll, MacOSAll, Sysutils;

var
  StartTime : double;
  TheApp : TZApplication;

{$ifndef minimal}
var
  DesignerFilePath : AnsiString;

procedure Platform_DesignerSetFilePath(const P : AnsiString);
begin
  DesignerFilePath := P;
end;
{$endif}

procedure Platform_ShowMouse(Visible : boolean);
const vals : array[boolean] of integer = (0,1);
begin

end;

type
  {$if FPC_FULlVERSION<30200}
  TObjCBool = boolean;
  {$else}
  TObjCBool = ObjCBOOL;
  {$endif}

  TZgeApplicationDelegate = objcclass(NSObject, NSApplicationDelegateProtocol)
    function applicationShouldTerminateAfterLastWindowClosed(sender: NSApplication): TObjCBool;
  end;

  TZgeWindow = objcclass(NSWindow)
    procedure sendEvent(theEvent: NSEvent); override;
    procedure keyDown (theEvent: NSEvent); override;
    function canBecomeKeyWindow : TObjCBool; override;
    function canBecomeMainWindow : TObjCBool; override;
  end;

  TZgeView = objcclass(NSOpenGLView)
    function acceptsFirstResponder : TObjCBool; override;
    function becomeFirstResponder : TObjCBool; override;
    function canBecomeKeyView : TObjCBool; override;
    procedure mouseMoved(theEvent: NSEvent); override;
    procedure updateTrackingAreas; override;
  end;

function NSStrToStr(const AStr: NSString): string;
var
  R: NSRange;
begin
  if AStr <> nil then
  begin
    SetLength(Result, AStr.length);
    R.location := 0;
    R.length := AStr.length;
    AStr.getCharacters_range({$if FPC_FULlVERSION<30200}PAnsiChar{$ELSE}unicharPtr{$ENDIF}(Result), R);
  end
  else
    Result := '';
end;

function TranslateKey(const Event : NSEvent) : byte;
var
  S : string;
begin
  case Event.KeyCode of
    $7E : Result := ord('^');
    $7D : Result := ord('_');
    $7B : Result := ord('<');
    $7C : Result := ord('>');
  else 
    begin
      S := NSStrToStr(Event.charactersIgnoringModifiers());
      if Length(S)>0 then
        Result := byte(UpCase(S[1]))
      else
        Result := 0;
    end;
  end;
end;

function TZgeWindow.canBecomeKeyWindow : TObjCBool; 
begin
  Result := True;
end;

function TZgeWindow.canBecomeMainWindow : TObjCBool; 
begin
  Result := True;
end;

procedure TZgeWindow.keyDown (theEvent: NSEvent); 
begin
  //Empty method needed to get rid of keyboard beep
end;

procedure Platform_DesignerHandleMacEvent(Event,View : pointer);
var
  theEvent: NSEvent;
  theView : NSView;
  Nsp : NSPoint;
begin
  theEvent := NSEvent(Event);
  theView := NSView(View);

  Nsp := theEvent.locationInWindow;
  Nsp := theView.convertPoint_fromView(Nsp,nil);
  Nsp.y := theView.bounds.size.height-Nsp.y;
  MacLastMouse[0] := Trunc(Nsp.x);
  MacLastMouse[1] := abs(Trunc(Nsp.y));

  case theEvent.type_ of
    NSLeftMouseDown,NSLeftMouseUp : 
      begin
        MacKeys[ Ord('{') ] := theEvent.type_=NSLeftMouseDown;
      end;
    NSRightMouseDown,NSRightMouseUp : 
      begin
        MacKeys[ Ord('}') ] := theEvent.type_=NSRightMouseDown;
      end;
    NSKeyUp : 
      begin
        MacKeys[ TranslateKey(theEvent) ] := False;
        if (theEvent.KeyCode=kVK_Escape) and TheApp.EscapeToQuit then
          Halt;
      end;
    NSKeyDown : MacKeys[ TranslateKey(theEvent) ] := True;
  end;
end;

procedure TZgeWindow.sendEvent(theEvent: NSEvent);
var
  View : NSView;
begin
  View := Self.contentView;
  Platform_DesignerHandleMacEvent(theEvent,View);
  inherited sendEvent(theEvent);
end;

function TZgeApplicationDelegate.applicationShouldTerminateAfterLastWindowClosed(sender: NSApplication): TObjCBool;
begin
  Result := True;
end;


function TZgeView.acceptsFirstResponder : TObjCBool; 
begin
  Result := True;
end;

function TZgeView.becomeFirstResponder : TObjCBool; 
begin
  Result := True;
end;

function TZgeView.canBecomeKeyView : TObjCBool;
begin
  Result := True;
end;

procedure TZgeView.updateTrackingAreas;
var
  track: NSTrackingArea;
begin
  track:=NSTrackingArea(NSTrackingArea.alloc).initWithRect_options_owner_userInfo(
    self.bounds
    , NSTrackingMouseEnteredAndExited or NSTrackingMouseMoved or NSTrackingActiveAlways
    , self, nil);
  self.addTrackingArea(track);
  track.release;
end;

procedure TZgeView.mouseMoved(theEvent: NSEvent);
var
  Nsp : NSPoint;
begin
  Nsp := theEvent.locationInWindow;
  Nsp := convertPoint_fromView(Nsp,nil);
  Nsp.y := bounds.size.height-Nsp.y;
  MacLastMouse[0] := Trunc(Nsp.x);
  MacLastMouse[1] := abs(Trunc(Nsp.y));
end;

var
  MainWindow: TZgeWindow;
  Ctx: NSOpenGLContext;

function Platform_InitScreen(var Width, Height : integer; UseFullScreen : boolean; Title : PAnsiChar; ZApp : pointer) : integer;
const
  Attr: array[0..15] of NSOpenGLPixelFormatAttribute =
    (NSOpenGLPFADepthSize, 16,
     NSOpenGLPFAColorSize, 32,
     NSOpenGLPFAAlphaSize, 8,
     NSOpenGLPFADoubleBuffer,
     NSOpenGLPFAStencilSize, 8,
     //Request multisample
     NSOpenGLPFAMultisample,
     NSOpenGLPFASampleBuffers, 1,
     NSOpenGLPFASamples, 8,
     NSOpenGLPFANoRecovery,
     0);
var
  Fmt: NSOpenGLPixelFormat;
  P : Pointer;
  View: TZgeView;
  MainWindowRect : NSRect;
  Del : TZgeApplicationDelegate;
begin
  Del := TZgeApplicationDelegate.alloc;
  NSApplication.sharedApplication.setDelegate(Del);

  Fmt := NSOpenGLPixelFormat(NSOpenGLPixelFormat.alloc).initWithAttributes(@Attr[0]);
  P := NSOpenGLContext(NSOpenGLContext.alloc).initWithFormat_shareContext(Fmt, nil);
  Ctx := NSOpenGLContext(P);

  if Width=-1 then
    Width := 800;
  if Height=-1 then
    Height := 600;

  MainWindowRect.origin.x := 600.0;
  MainWindowRect.origin.y := 600.0;
  MainWindowRect.size.width := Width;
  MainWindowRect.size.height := Height;

  MainWindow := TZgeWindow.alloc.initWithContentRect_stylemask_backing_defer(MainWindowRect,
    NSTitledWindowMask or NSClosableWindowMask or NSMiniaturizableWindowMask,
    NSBackingStoreBuffered, False);

  View := TZgeView.alloc.initWithFrame(MainWindowRect);
  View.setWantsBestResolutionOpenGLSurface(False);
  MainWindow.setContentView(View);
  MainWindow.makeFirstResponder(View);

  MainWindow.makeKeyAndOrderFront(NSApplication.sharedApplication);

  Platform_SetWindowCaption(Title);

  Ctx.setView( View );
  Ctx.Update;

  Ctx.makeCurrentContext;
  
  TheApp := TZApplication(ZApp);
end;

procedure Platform_SetWindowCaption(Title : PAnsiChar);
begin
  MainWindow.setTitle( NSSTR(Title) );
end;

procedure Platform_ShutdownScreen;
begin
  MainWindow.close;
  Ctx.release;
end;

procedure Platform_SwapBuffers;
begin
  Ctx.flushBuffer;
end;

type
  TTimerHandler = objcclass(NSObject)
    Callback : TRunCallback;
    procedure TimerEvent(sender : id); message 'TimerEvent:';
  end;

var
  TimerHandler: TTimerHandler;
  DisplayTimer : NSTimer;

procedure TTimerHandler.TimerEvent(sender : id);
begin
  Callback();
end;

procedure Platform_Run(Callback : TRunCallback);
begin
  TimerHandler := TTimerHandler.alloc;
  TimerHandler.Callback := Callback;

  DisplayTimer := NSTimer.timerWithTimeInterval_target_selector_userInfo_repeats(1.0/1000.0, 
    TimerHandler, ObjCSelector('TimerEvent:'), nil, True);

  NSRunLoop.currentRunLoop.addTimer_forMode(DisplayTimer , NSDefaultRunLoopMode);

  NSApplication.sharedApplication.run();
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

var
  ExeName : string;

function Platform_GetExeFileName : PAnsiChar;
begin
  ExeName := ParamStr(0);
  Result := PAnsiChar(ExeName);
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
    Exit;
  end;

  Size := System.FileSize(F);

  GetMem(Memory,Size);
  System.BlockRead(F,Memory^,Size);

  System.Close(F);
end;

procedure Platform_InitGlobals;
begin
  
end;


var
  audioFormat: AudioStreamBasicDescription;
  outputUnit: AudioUnit;
  MixBuffer : array[0..SoundBufferFramesSize-1] of TSoundMixUnit;

function AudioOutputCallback(inRefCon: UnivPtr; var ioActionFlags: AudioUnitRenderActionFlags; const (*var*) inTimeStamp: AudioTimeStamp; inBusNumber: UInt32; inNumberFrames: UInt32; ioData: AudioBufferListPtr ): OSStatus;
var
  BytesToWrite, FramesToWrite : integer;
begin
  BytesToWrite := inNumberFrames * 8;

  FramesToWrite := BytesToWrite  div SizeOf(TSoundOutputUnit);

  FillChar(MixBuffer,FramesToWrite * SizeOf(TSoundMixUnit),0);

  Platform_EnterMutex(VoicesMutex);
    AudioPlayer.RenderToMixBuffer(@MixBuffer[0],FramesToWrite);
  Platform_LeaveMutex(VoicesMutex);

  // Copy audio samples to output buffer
  Move(MixBuffer[0], ioData.mBuffers[0].mData^, BytesToWrite);

  Result := noErr;
end;

procedure Platform_InitAudio;
var
  outputDesc: AudioComponentDescription;
  outputComp: AudioComponent;
  outCallback : AURenderCallbackStruct;
begin
  audioFormat.mSampleRate := 44100.0;
  audioFormat.mFormatID := kAudioFormatLinearPCM;
  audioFormat.mFormatFlags := kAudioFormatFlagIsFloat or kAudioFormatFlagIsPacked;
  audioFormat.mFramesPerPacket := 1;
  audioFormat.mChannelsPerFrame := 2;
  audioFormat.mBitsPerChannel := 32;
  audioFormat.mBytesPerFrame := (audioFormat.mBitsPerChannel * audioFormat.mChannelsPerFrame) div 8;
  audioFormat.mBytesPerPacket := audioFormat.mBytesPerFrame * audioFormat.mFramesPerPacket;
  
  outputDesc.componentType := kAudioUnitType_Output;
  outputDesc.componentSubType := kAudioUnitSubType_DefaultOutput;
  outputDesc.componentManufacturer := kAudioUnitManufacturer_Apple;
  outputDesc.componentFlags := 0;
  outputDesc.componentFlagsMask := 0;

  outputComp := AudioComponentFindNext(nil, outputDesc);
  AudioComponentInstanceNew(outputComp, outputUnit);

  AudioUnitSetProperty(outputUnit, kAudioUnitProperty_StreamFormat, kAudioUnitScope_Input, 0, @audioFormat, SizeOf(audioFormat));

  outCallback.inputProc := @AudioOutputCallback;
  AudioUnitSetProperty(outputUnit, kAudioUnitProperty_SetRenderCallback, kAudioUnitScope_Input, 0, @outCallback, SizeOf(outCallback));

  AudioUnitInitialize(outputUnit);
  AudioOutputUnitStart(outputUnit);
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
  Sleep(time);
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
  S : AnsiString;
{$endif}
begin
  //Open file relative to path of exe-file
  {$ifndef minimal}
  //Inside designer we must provide full path to zgeproj-file
  S := AnsiString(ChangeFileExt(String(DesignerFilePath) + String(Name),'.dylib'));
  if FileExists(string(S)) then
    Result := NativeUInt( dlopen(PAnsiChar(S), RTLD_NOW) )
  else
    //Not found in zgeproj-path, assume system32-dll such as user32 and load without path
    Result := NativeUInt( dlopen(Name, RTLD_NOW) );
  {$else}
  //In engine always let the OS find the dll without path
  Result := NativeUInt( dlopen(Name, RTLD_NOW) );
  {$endif}
end;

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

{$ifndef minimal}
procedure Platform_DesignerSetAudioCallback(F : TDesignerAudioCallback);
begin

end;
{$endif}

initialization

  StartTime := CACurrentMediaTime;

end.
