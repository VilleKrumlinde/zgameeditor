unit GLPanel;

interface

{$if defined(fpc) and defined(macos)}
{$modeswitch objectivec1}
{$endif}

uses
  {$ifdef MSWINDOWS}
  Windows, Messages,
  {$endif}
  {$ifdef ZgeLazarus}
  LCLType, LCLIntf, LMessages, 
  {$endif}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  dglOpenGL, ExtCtrls, ZApplication;

type 
  {$ifdef MACOS}
  HGLRC = pointer;
  {$endif}

  TGLPanel = class(TCustomPanel)
  private
    Hrc: HGLRC;
    InitCalled: Boolean;
    FOnGLDraw: TNotifyEvent;
    FOnGLInit: TNotifyEvent;
    {$ifdef MSWINDOWS}
    procedure SetDCPixelFormat(const DC: HDC);
    {$endif}
    procedure CreateRenderContext;
  protected
    {$ifndef fpc}
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WMEraseBackground( var msg:TWMEraseBkgnd ); message WM_ERASEBKGND;
    procedure WMGETDLGCODE( var msg:TMessage); message WM_GETDLGCODE;
    {$endif}
    procedure Paint; override;
    procedure SetParent(AParent: TWinControl); override;
    procedure DestroyHandle; override;
    procedure CreateHandle; override;
    property Color default clBlack;
    {$ifdef ZgeLazarus}
    procedure WMPaint(var Message: TLMPaint); message LM_PAINT;
    {$endif}
  public
    SharedHrc: HGLRC;
    function GetHrc : HGLRC;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$ifdef ZgeLazarus}
    procedure Invalidate; override;
    procedure EraseBackground(DC: HDC); override;
    {$endif}
    property MouseCapture;
    property Canvas;
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

implementation

uses
  {$ifdef MACOS}
  CocoaUtils, CocoaAll, CocoaPrivate,
  {$endif}
  ZLog, ZPlatform, ZClasses, frmEditor;

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


procedure TGLPanel.CreateRenderContext;
{$ifdef MACOS}
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
  Ctx: NSOpenGLContext;
  Fmt: NSOpenGLPixelFormat;
  P : Pointer;
  View: pointer;

  MainWindow: NSWindow;
  MainWindowRect : NSRect;

{$endif}
begin
  {$ifdef MACOS}
  Fmt := NSOpenGLPixelFormat(NSOpenGLPixelFormat.alloc).initWithAttributes(@Attr[0]);
  P := NSOpenGLContext(NSOpenGLContext.alloc).initWithFormat_shareContext(Fmt, nil);
  Ctx := NSOpenGLContext(P);

  MainWindowRect.origin.x := 300.0;
  MainWindowRect.origin.y := 300.0;
  MainWindowRect.size.width := 300.0;
  MainWindowRect.size.height := 500.0;
  MainWindow := NSWindow.alloc.initWithContentRect_stylemask_backing_defer(MainWindowRect,
    NSTitledWindowMask or NSClosableWindowMask or NSMiniaturizableWindowMask or NSResizableWindowMask,
    NSBackingStoreBuffered, False);
  MainWindow.makeKeyAndOrderFront(NSapp);

//  View := NSObject(Handle).lclContentView;
  View := MainWindow.contentView;
  Ctx.setView( NSView(View) );
  Ctx.Update;

  Ctx.makeCurrentContext;
  Self.Hrc := Ctx;

  dglOpenGL.ReadExtensions;
  glViewport(0,0,ClientWidth,ClientHeight);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  {$endif}

  {$ifdef MSWINDOWS}
  // Create a rendering context.
  SetDCPixelFormat(Canvas.Handle);

  if SharedHrc<>0 then
    //Use existing hrc
    //http://stackoverflow.com/questions/13581303/opengl-share-existing-textures-with-future-contexts
    Hrc := SharedHrc;

  if hrc=0 then
    hrc := wglCreateContext(Canvas.Handle);
  if hrc <> 0 then
  begin
    wglMakeCurrent(Canvas.Handle,hrc);
    dglOpenGL.ReadExtensions;
    glViewport(0,0,ClientWidth,ClientHeight);
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  end;
  {$endif}
end;


procedure TGLPanel.Paint;
begin
  // Draw the scene.
  if (not InitCalled) and (Self.Handle<>0) then
  begin
    InitCalled := True;
    if Assigned(OnGLInit) then
      OnGlInit(Self);
  end;

  {$ifdef MACOS}
  NSOpenGLContext(hrc).update;
  {$endif}

  {$ifdef MSWINDOWS}
  wglMakeCurrent(Canvas.Handle,hrc);
  Platform_DesignerSetDC(Self.Canvas.Handle, Self.Handle);
  {$endif}

  if Assigned(OnGLDraw) then
    OnGLDraw(Self);

  {$ifdef MACOS}
  NSOpenGLContext(hrc).flushBuffer;
  {$endif}

  {$ifdef MSWINDOWS}
  SwapBuffers(Canvas.Handle);
  {$endif}
end;

{$ifdef ZgeLazarus}
procedure TGLPanel.WMPaint(var Message: TLMPaint);
begin
  Include(FControlState, csCustomPaint);
  Paint;
  Exclude(FControlState, csCustomPaint);
end;
{$endif}

constructor TGLPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$ifndef fpc}
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
  if (hrc <> nil) and (SharedHrc=nil) then
    //Delete context if we owned it
    NSOpenGLContext(HRC).release;
  inherited;
  {$endif}
end;

procedure TGLPanel.ForceInitGL;
begin
  HandleNeeded;
  Paint;
  {$ifdef MACOS}
  TCocoaCustomControl(Handle).lclClearCallback;
  {$endif}
end;

{$ifdef ZgeLazarus}
procedure TGLPanel.Invalidate; 
begin
  {$ifdef MACOS}
  if csCustomPaint in FControlState then 
    Exit;
  if Parent<>nil then
    TCocoaCustomControl(Handle).lclClearCallback;
  {$endif}
  inherited;
end;

procedure TGLPanel.EraseBackground(DC: HDC); 
begin
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
  if Self.Parent<>nil then
  begin
    CreateRenderContext;
  end;
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

initialization
  InitOpenGL;
finalization
// CloseOpenGL;
end.
