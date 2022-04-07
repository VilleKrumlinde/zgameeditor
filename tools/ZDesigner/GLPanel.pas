unit GLPanel;

//Based on https://gitlab.com/freepascal.org/lazarus/lazarus/-/blob/main/components/opengl/glcocoanscontext.pas

interface

{$if defined(fpc) and defined(macos)}
{$modeswitch objectivec1}
{$endif}

uses
  {$ifdef MSWINDOWS}
  Windows, Messages,
  {$endif}
  {$ifdef ZgeLazarus}
  LCLType, LCLIntf, LMessages, WSLCLClasses, WSControls,
  {$endif}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  dglOpenGL, ExtCtrls, ZApplication;

type 
  {$ifdef MACOS}
  HGLRC = pointer;
  {$endif}

  TGLPanel = class(TWinControl)
  private
    Hrc: HGLRC;
    InitCalled: Boolean;
    FOnGLDraw: TNotifyEvent;
    FOnGLInit: TNotifyEvent;
    {$ifdef MSWINDOWS}
    procedure SetDCPixelFormat(const DC: HDC);
    {$endif}
  protected
    {$ifndef fpc}
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WMEraseBackground( var msg:TWMEraseBkgnd ); message WM_ERASEBKGND;
    procedure WMGETDLGCODE( var msg:TMessage); message WM_GETDLGCODE;
    {$endif}
    procedure Paint; virtual;
    procedure SetParent(AParent: TWinControl); override;
    procedure DestroyHandle; override;
    procedure CreateHandle; override;
    property Color default clBlack;
    {$ifdef ZgeLazarus}
    procedure WMPaint(var Message: TLMPaint); message LM_PAINT;
    class procedure WSRegisterClass; override;
    {$endif}
  public
    SharedHrc: HGLRC;
    function GetHrc : HGLRC;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$ifdef ZgeLazarus}
    procedure Invalidate; override;
    procedure EraseBackground(DC: HDC); override;
    procedure RealizeBounds; override;
    {$endif}
    property MouseCapture;
//    property Canvas;
    property OnGLDraw: TNotifyEvent read FOnGLDraw write FOnGLDraw;
    property OnGLInit: TNotifyEvent read FOnGLInit write FOnGLInit;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnMouseWheel;
    procedure ForceInitGL;
  end;

  TGLPanelZGE = class(TGLPanel)
  strict private
    RenderTimer : TTimer;
    OldGlWindowProc : TWndMethod;
    procedure RenderTimerTimer(Sender: TObject);
    {$ifndef fpc}
    procedure GlWindowProc(var Message: TMessage);
    {$endif}
  protected
    procedure Paint; override;
  public
    App : TZApplication;
    OnBindData: TNotifyEvent;
    OnUpdateData: TNotifyEvent;
    procedure LoadApp(const FileName : string);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TWSOpenGLPanel = class(TWSWinControl)
  published
    class function CreateHandle(const AWinControl: TWinControl;
                                const AParams: TCreateParams): HWND; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
    class function GetDoubleBuffered(const AWinControl: TWinControl): Boolean; override;
  end;



implementation

uses
  {$ifdef MACOS}
  CocoaUtils, CocoaAll, CocoaPrivate, MacOSAll, CocoaWSCommon, types, LCLMessageGlue,
  {$endif}
  ZLog, ZPlatform, ZClasses, frmEditor;

type
  TCocoaOpenGLView = objcclass(NSOpenGLView)
  public
    Owner: TWinControl;
    nsGL: NSOpenGLContext;
    callback: TLCLCommonCallback;
    backingScaleFactor: Single;
    function acceptsFirstResponder: LCLObjCBoolean; override;
    function becomeFirstResponder: LCLObjCBoolean; override;
    function resignFirstResponder: LCLObjCBoolean; override;
    procedure drawRect(dirtyRect: NSRect); override;
    procedure dealloc; override;
    function lclGetCallback: ICommonCallback; override;
    procedure lclClearCallback; override;
    function lclIsEnabled: Boolean; override;
    // mouse
    procedure mouseDown(event: NSEvent); override;
    procedure mouseUp(event: NSEvent); override;
    procedure rightMouseDown(event: NSEvent); override;
    procedure rightMouseUp(event: NSEvent); override;
    procedure rightMouseDragged(event: NSEvent); override;
    procedure otherMouseDown(event: NSEvent); override;
    procedure otherMouseUp(event: NSEvent); override;
    procedure otherMouseDragged(event: NSEvent); override;
    procedure mouseDragged(event: NSEvent); override;
    procedure mouseEntered(event: NSEvent); override;
    procedure mouseExited(event: NSEvent); override;
    procedure mouseMoved(event: NSEvent); override;
    procedure scrollWheel(event: NSEvent); override;
    // other
    procedure resetCursorRects; override;
  end;

{$ifndef fpc}
procedure TGLPanel.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.WindowClass.style := Params.WindowClass.style or CS_OWNDC;
end;
{$endif}

{$ifdef MSWINDOWS}
procedure TGLPanel.SetDCPixelFormat(const DC: HDC);
var
  i: Integer;
  nPixelFormat: Integer;
  pfd: TPixelFormatDescriptor;
  TmpDC : HDC;
begin
  nPixelFormat := GetPixelFormat(DC);
  if nPixelFormat<>0 then
    Exit;

  FillChar(pfd, SizeOf(pfd), 0);
  with pfd do
  begin
    nSize     := sizeof(pfd);   // Size of this structure
    nVersion  := 1;             // Version number
    dwFlags   := PFD_DRAW_TO_WINDOW    // Buffer supports drawing to window
                         or PFD_SUPPORT_OPENGL // Buffer supports OpenGL drawing
                         or PFD_DOUBLEBUFFER;
    iPixelType:= PFD_TYPE_RGBA;    // RGBA pixel values
    cColorBits:= 32;
    cDepthBits:= 16;
    iLayerType:= PFD_MAIN_PLANE;    // Layer type
    cAlphaBits := 8;
    cStencilBits := 8; //NanoVG requires stencil buffer
  end;

  //Use the same pixeldepth as the desktop
  TmpDC := GetDC(0);
  I := GetDeviceCaps(TmpDC, BITSPIXEL);
  ReleaseDC(0, TmpDC);
  if I in [16,24,32] then
    pfd.cColorBits := I;
  if pfd.cColorBits<>32 then
    pfd.cAlphaBits := 0;
  nPixelFormat := ChoosePixelFormat(DC, @pfd);
  if not SetPixelFormat(DC, nPixelFormat, @pfd) then
    raise Exception.Create('Error SetPixelFormat Nr:'+IntToStr(GetLastError)+' nPFrmt:'+IntToStr(nPixelFormat));
end;
{$endif}


function GetCGLContextObj(OpenGLControlHandle: HWND): CGLContextObj;
var
  View: NSOpenGLView;
begin
  Result:=nil;
  if OpenGLControlHandle=0 then exit;
  View:=TCocoaOpenGLView(OpenGLControlHandle);
  Result:=CGLContextObj(View.openGLContext.CGLContextObj);
//  NSScreen.mainScreen.colorSpace;
end;

function LOpenGLMakeCurrent(Handle: HWND): boolean;
var
  CGLContext: CGLContextObj;
begin
  if Handle=0 then exit;
  CGLContext := GetCGLContextObj(Handle);
  Result:=CGLSetCurrentContext(CGLContext)=kCGLNoError;
end;

procedure TGLPanel.Paint;
begin
LOpenGLMakeCurrent(Handle);

  // Draw the scene.
  if (not InitCalled) and (Self.Handle<>0) then
  begin
    InitCalled := True;
    if Assigned(OnGLInit) then
      OnGlInit(Self);
  end;

  {$ifdef MACOS}
//  NSOpenGLContext(hrc).makeCurrentContext;
//  NSOpenGLContext(hrc).update;
  {$endif}

  {$ifdef MSWINDOWS}
  wglMakeCurrent(Canvas.Handle,hrc);
  Platform_DesignerSetDC(Self.Canvas.Handle, Self.Handle);
  {$endif}

  if Assigned(OnGLDraw) then
    OnGLDraw(Self);

  {$ifdef MACOS}
//  NSOpenGLContext(hrc).flushBuffer;
  {$endif}

  {$ifdef MSWINDOWS}
  SwapBuffers(Canvas.Handle);
  {$endif}
end;

{$ifdef ZgeLazarus}
procedure TGLPanel.WMPaint(var Message: TLMPaint);
begin
TCocoaOpenGLView(Handle).nsGL.makeCurrentContext;
  Include(FControlState, csCustomPaint);
  inherited WMPaint(Message);
  Paint;
  glFlush();
//NSOpenGLContext(Handle).flushBuffer;
TCocoaOpenGLView(Handle).nsGL.flushBuffer;
  Exclude(FControlState, csCustomPaint);
end;
{$endif}

constructor TGLPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle:=ControlStyle-[csSetCaption];
  {$ifdef fpc}
  FCompStyle:=csNonLCL;  
  {$else}
  Locked := True;
  {$endif}
  Color := clBlack;
end;

destructor TGLPanel.Destroy;
begin
  // Clean up and terminate.
  {$ifdef MSWINDOWS}
  wglMakeCurrent(0,0);
  inherited;
  if (hrc <> 0) and (SharedHrc=0) then
    //Delete context if we owned it
    wglDeleteContext(HRC)
  {$endif}

  {$ifdef MACOS}
(*  if (hrc <> nil) then
    NSView(NSOpenGLContext(HRC).View).window.close;
  if (hrc <> nil) and (SharedHrc=nil) then
    //Delete context if we owned it
    NSOpenGLContext(HRC).release; *)
  inherited;
  {$endif}
end;

procedure TGLPanel.ForceInitGL;
begin
  HandleNeeded;
  Paint;
end;

{$ifdef ZgeLazarus}
procedure TGLPanel.Invalidate; 
begin
  {$ifdef MACOS}
  if csCustomPaint in FControlState then 
    Exit;
  {$endif}
  inherited;
end;

procedure TGLPanel.EraseBackground(DC: HDC); 
begin
end;

procedure TGLPanel.RealizeBounds;
begin
  if IsVisible and HandleAllocated
  and ([csDestroying]*ComponentState=[]) then
  begin
    if LOpenGLMakeCurrent(Self.Handle) then
      ;//LOpenGLViewport(Handle,0,0,Width,Height);
  end;
  inherited RealizeBounds;
end;

{$endif}

function TGLPanel.GetHrc: HGLRC;
begin
  Result := Self.Hrc;
end;

{$ifndef fpc}
procedure TGLPanel.WMEraseBackground(var Msg:TWMEraseBkgnd);
begin
  Msg.Result := 1;
end;

procedure TGLPanel.WMGETDLGCODE(var Msg: TMessage);
begin
  //Needed to detect arrow keys in zzdc-runtime
  Msg.Result := DLGC_WANTARROWS;
end;
{$endif}

procedure TGLPanel.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  if AParent<>nil then
    ForceInitGL;
end;

procedure TGLPanel.DestroyHandle;
begin
  {$ifdef MSWINDOWS}
  glFinish();
  wglMakeCurrent(0,0);
  {$endif}
  inherited;
end;

procedure TGLPanel.CreateHandle;
begin
  inherited;
  Self.Hrc := Pointer(Self.Handle);
end;

class procedure TGLPanel.WSRegisterClass;
const
  Registered : Boolean = False;
begin
  if Registered then
    Exit;
  inherited WSRegisterClass;
  RegisterWSComponent(TGLPanel,TWSOpenGLPanel);
  Registered := True;
end;

{ TGLPanelZGE }

constructor TGLPanelZGE.Create(AOwner: TComponent);
begin
  inherited;
  Self.RenderTimer := TTimer.Create(AOwner);
  Self.RenderTimer.OnTimer := RenderTimerTimer;
  Self.TabStop := True;

  OldGlWindowProc := Self.WindowProc;
  {$ifndef ZgeLazarus}
  Self.WindowProc := GlWindowProc;
  {$endif}
end;

destructor TGLPanelZGE.Destroy;
begin
  FreeAndNil(App);
  inherited;
end;

{$ifndef ZgeLazarus}
procedure TGLPanelZGE.GlWindowProc(var Message: TMessage);
begin
  SetWindowLongPtr(Self.Handle,GWL_USERDATA, NativeInt(Self.App) );
  Platform_DesignerWindowProc( pointer(@Message) );
  OldGlWindowProc(Message);
end;
{$endif}

procedure TGLPanelZGE.LoadApp(const FileName: string);
var
  A : TZApplication;
begin
  A := ComponentManager.LoadXmlFromFile( FileName ) as TZApplication;
  A.OnGetLibraryPath := EditorForm.OnGetLibraryPath;
  A.Compile;

  Self.App := A;
  if Assigned(OnBindData) then
    OnBindData(Self);

  A.DesignerReset;
  A.DesignerStart(Self.Width,Self.Height, 0);

  RenderTimer.Interval := 25;
  RenderTimer.Enabled := True;
end;

procedure TGLPanelZGE.Paint;
begin
  inherited;

  if Self.App<>nil then
  begin
    if Assigned(OnUpdateData) then
      Self.OnUpdateData(Self);

    Self.App.ScreenWidth := Self.Width;
    Self.App.ScreenHeight := Self.Height;
    Self.App.UpdateViewport;
    Self.App.WindowHandle := Self.Handle;

    try
      App.Main;
    except
      on E : Exception do
      begin
        FreeAndNil(App);
        raise;
      end;
    end;
  end;
end;

procedure TGLPanelZGE.RenderTimerTimer(Sender: TObject);
begin
  if Self.Tag=0 then
  begin
    //Must have focus for mousewheel to work
    Self.SetFocus;
    Self.Tag := 1;
  end;
  Invalidate;
end;


{ TWSOpenGLPanel }


{ TCocoaOpenGLView }

function TCocoaOpenGLView.acceptsFirstResponder: LCLObjCBoolean;
begin
  Result := True;
end;

function TCocoaOpenGLView.becomeFirstResponder: LCLObjCBoolean;
begin
  Result:=inherited becomeFirstResponder;
  callback.BecomeFirstResponder;
end;

function TCocoaOpenGLView.resignFirstResponder: LCLObjCBoolean;
begin
  Result:=inherited resignFirstResponder;
  callback.ResignFirstResponder;
end;

procedure TCocoaOpenGLView.dealloc;
begin
  inherited dealloc;
end;

function TCocoaOpenGLView.lclGetCallback: ICommonCallback;
begin
  Result := callback;
end;

procedure TCocoaOpenGLView.lclClearCallback;
begin
  callback := nil;
end;

function TCocoaOpenGLView.lclIsEnabled: Boolean;
begin
  Result := Owner.Enabled;
end;

procedure TCocoaOpenGLView.mouseDown(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
  begin
    // do not pass mouseDown below or it will pass it to the parent control
    // causing double events
    //inherited mouseDown(event);
  end;
end;

procedure TCocoaOpenGLView.mouseUp(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited mouseUp(event);
end;

procedure TCocoaOpenGLView.rightMouseDown(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited rightMouseDown(event);
end;

procedure TCocoaOpenGLView.rightMouseUp(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited rightMouseUp(event);
end;

procedure TCocoaOpenGLView.rightMouseDragged(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseMove(event) then
    inherited rightMouseDragged(event);
end;

procedure TCocoaOpenGLView.otherMouseDown(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited otherMouseDown(event);
end;

procedure TCocoaOpenGLView.otherMouseUp(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseUpDownEvent(event) then
    inherited otherMouseUp(event);
end;

procedure TCocoaOpenGLView.otherMouseDragged(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseMove(event) then
    inherited otherMouseDragged(event);
end;

procedure TCocoaOpenGLView.mouseDragged(event: NSEvent);
begin
  if Assigned(callback)
    then callback.MouseMove(event)
    else inherited mouseDragged(event);
end;

procedure TCocoaOpenGLView.mouseEntered(event: NSEvent);
begin
  inherited mouseEntered(event);
end;

procedure TCocoaOpenGLView.mouseExited(event: NSEvent);
begin
  inherited mouseExited(event);
end;

procedure TCocoaOpenGLView.mouseMoved(event: NSEvent);
begin
  if not Assigned(callback) or not callback.MouseMove(event) then
    inherited mouseMoved(event);
end;

procedure TCocoaOpenGLView.scrollWheel(event: NSEvent);
begin
  if Assigned(callback)
    then callback.scrollWheel(event)
    else inherited scrollWheel(event);
end;

procedure TCocoaOpenGLView.resetCursorRects;
begin
  if not Assigned(callback) or not callback.resetCursorRects then
    inherited resetCursorRects;
end;

procedure TCocoaOpenGLView.drawRect(dirtyRect: NSRect);
var
  ctx : NSGraphicsContext;
  PS  : TPaintStruct;
  r   : NSRect;
begin
  ctx := NSGraphicsContext.currentContext;
  inherited drawRect(dirtyRect);
  if CheckMainThread and Assigned(callback) then
  begin
    if ctx = nil then
    begin
      // In macOS 10.14 (mojave) current context is nil
      // we still can paint anything releated to OpenGL!
      // todo: consider creating a dummy context (for a bitmap)
      FillChar(PS, SizeOf(TPaintStruct), 0);
      r := frame;
      r.origin.x:=0;
      r.origin.y:=0;
      PS.hdc := HDC(0);
      PS.rcPaint := NSRectToRect(r);
      LCLSendPaintMsg(Owner, HDC(0), @PS);
    end
    else
      callback.Draw(ctx, bounds, dirtyRect);
  end;
end;

type
  TTestContext = objcclass(NSOpenGLContext)
  public
    procedure dealloc; override;
  end;

procedure TTestContext.dealloc;
begin
end;

class function TWSOpenGLPanel.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
const
  Attr: array[0..14] of NSOpenGLPixelFormatAttribute =
    (NSOpenGLPFADepthSize, 16,
     NSOpenGLPFAColorSize, 32,
     NSOpenGLPFAAlphaSize, 8,
//     NSOpenGLPFADoubleBuffer,
     NSOpenGLPFAStencilSize, 8,
     //Request multisample
     NSOpenGLPFAMultisample,
     NSOpenGLPFASampleBuffers, 1,
     NSOpenGLPFASamples, 8,
     NSOpenGLPFANoRecovery,
     0);
var
  View: TCocoaOpenGLView;
  Attrs: NSOpenGLPixelFormatAttributePtr;
  PixFmt: NSOpenGLPixelFormat;
  p: NSView;
  ns: NSRect;
  aNSOpenGLContext,sharedCtx: NSOpenGLContext;
  CGLContext: CGLContextObj;
  i : integer;
begin
  Result:=0;

  if TGLPanel(AWinControl).SharedHrc=nil then
    sharedCtx := nil
  else
    sharedCtx := TCocoaOpenGLView(TGLPanel(AWinControl).SharedHrc).nsGL;

  p := nil;
  if (AParams.WndParent <> 0) then
    p := NSObject(AParams.WndParent).lclContentView;
  if Assigned(p) then
    LCLToNSRect(types.Bounds(AParams.X, AParams.Y, AParams.Width, AParams.Height),
      p.frame.size.height, ns)
  else
    ns := GetNSRect(AParams.X, AParams.Y, AParams.Width, AParams.Height);

  PixFmt := NSOpenGLPixelFormat(NSOpenGLPixelFormat.alloc).initWithAttributes(@Attr[0]);
  aNSOpenGLContext := TTestContext(TTestContext.alloc).initWithFormat_shareContext(PixFmt, sharedCtx);

  View := TCocoaOpenGLView(TCocoaOpenGLView.alloc).initWithFrame_pixelFormat(ns,PixFmt);
  View.setHidden(AParams.Style and WS_VISIBLE = 0);
  if Assigned(p) then
    p.addSubview(View);
  SetViewDefaults(View);
  View.Owner:=AWinControl;
  View.nsGL := aNSOpenGLContext;
  View.callback:=TLCLCommonCallback.Create(View, AWinControl);
//    aNSOpenGLContext.setView(view);
  Result:=TLCLIntfHandle(View);

  //https://stackoverflow.com/a/44183191/43673
  view.setOpenGLContext(aNSOpenGLContext);

{  if Assigned(sharedCtx) then
  begin
    aNSOpenGLContext.makeCurrentContext;
    i := -1;
    glGenTextures(1, @i);
    writeln(inttostr(i));
  end;}
end;

class procedure TWSOpenGLPanel.DestroyHandle(const AWinControl: TWinControl);
begin
//  LOpenGLDestroyContextInfo(AWinControl);
  // do not use "inherited DestroyHandle", because the LCL changes the hierarchy at run time
//  TWSWinControlClass(ClassParent).DestroyHandle(AWinControl);
end;

class function TWSOpenGLPanel.GetDoubleBuffered(const AWinControl: TWinControl): Boolean;
begin
  Result := False;
  if AWinControl=nil then ;
end;

initialization
  InitOpenGL;
finalization
// CloseOpenGL;
end.
