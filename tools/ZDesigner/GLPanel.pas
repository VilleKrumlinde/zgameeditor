unit GLPanel;

interface

uses
  Windows, Messages, SysUtils, Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  OpenGL12, Vcl.ExtCtrls, ZApplication;

type
  TGLPanel = class(TCustomPanel)
  private
    Hrc: HGLRC;
    InitCalled: Boolean;
    FOnGLDraw: TNotifyEvent;
    FOnGLInit: TNotifyEvent;
    procedure SetDCPixelFormat(const DC: HDC);
    procedure CreateRenderContext;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WMEraseBackground( var msg:TWMEraseBkgnd ); message WM_ERASEBKGND;
    procedure WMGETDLGCODE( var msg:TMessage); message WM_GETDLGCODE;
    procedure Paint; override;
    procedure SetParent(AParent: TWinControl); override;
    procedure DestroyHandle; override;
    procedure CreateHandle; override;
    property Color default clBlack;
  public
    SharedHrc: HGLRC;
    function GetHrc : HGLRC;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
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
    procedure GlWindowProc(var Message: TMessage);
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

uses ZLog, ZPlatform, ZClasses, frmEditor;

procedure TGLPanel.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.WindowClass.style := Params.WindowClass.style or CS_OWNDC;
end;

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
    cDepthBits:= 24;
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


procedure TGLPanel.CreateRenderContext;
begin
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
    glViewport(0,0,ClientWidth,ClientHeight);
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  end;
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

  wglMakeCurrent(Canvas.Handle,hrc);

  Platform_DesignerSetDC(Self.Canvas.Handle, Self.Handle);

  if Assigned(OnGLDraw) then
    OnGLDraw(Self);

  SwapBuffers(Canvas.Handle);
end;


constructor TGLPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Locked := True;
  Color := clBlack;
end;

destructor TGLPanel.Destroy;
begin
  // Clean up and terminate.
  wglMakeCurrent(0,0);
  inherited;
  if (hrc <> 0) and (SharedHrc=0) then
    //Delete context if we owned it
    wglDeleteContext(HRC)
end;

procedure TGLPanel.ForceInitGL;
begin
  HandleNeeded;
  Paint;
end;

function TGLPanel.GetHrc: HGLRC;
begin
  Result := Self.Hrc;
end;

procedure TGLPanel.WMEraseBackground(var Msg:TWMEraseBkgnd);
begin
  Msg.Result := 1;
end;

procedure TGLPanel.WMGETDLGCODE(var Msg: TMessage);
begin
  //Needed to detect arrow keys in zzdc-runtime
  Msg.Result := DLGC_WANTARROWS;
end;

procedure TGLPanel.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  if AParent<>nil then
    ForceInitGL;
end;

procedure TGLPanel.DestroyHandle;
begin
  glFinish();
  wglMakeCurrent(0,0);
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
  Self.WindowProc := GlWindowProc;
end;

destructor TGLPanelZGE.Destroy;
begin
  FreeAndNil(App);
  inherited;
end;

procedure TGLPanelZGE.GlWindowProc(var Message: TMessage);
begin
  SetWindowLongPtr(Self.Handle,GWL_USERDATA, NativeInt(Self.App) );
  Platform_DesignerWindowProc( pointer(@Message) );
  OldGlWindowProc(Message);
end;

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
    //Must have focus for mousehwheel to work
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
