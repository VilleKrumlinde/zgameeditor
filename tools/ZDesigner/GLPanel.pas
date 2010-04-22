unit GLPanel;
/////////////////////////////////////////////////////////////////////////
// Industrial Software Solutions
// 4205 Hideaway
// Arlington, Texas 76017
// Mitchell E. James
// May 18, 1996
// mjames@cyberhighway.net
// http://www.cyberhighway.net/~mjames/

// note: When running under the Delphi 2.0 debugger the Windows GL subsystem errors out randomly.
// note: The Windows GL subsystem seems to work fine running Delphi GL executables.
// note: I haven't been running with a palette. Not sure if that works.


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  OpenGL12, ExtCtrls;

type
  TCustomGLPanel = class(TCustomPanel)
  private
    hrc: HGLRC;
    InitCalled: Boolean;
    FOnGLDraw: TNotifyEvent;
    FOnGLInit: TNotifyEvent;
    function SetDCPixelFormat(DC: HDC):Boolean;
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
  published
  end;

  TGLPanel = class(TCustomGLPanel)
  private
  protected
  public
  published
    property Align;
//    property Alignment;
//    property BevelInner;
//    property BevelOuter;
//    property BevelWidth;
//    property BorderWidth;
//    property BorderStyle;
    property DragCursor;
    property DragMode;
    property Enabled;
//    property Caption;
//    property Color;
//    property Ctl3D;
//    property Font;
//    property ParentColor;
//    property ParentCtl3D;
//    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;
    property OnGLDraw;
    property OnGLInit;
  end;

implementation

uses ZLog, ZPlatform;

procedure TCustomGLPanel.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.WindowClass.style := Params.WindowClass.style or CS_OWNDC;
end;

function TCustomGLPanel.SetDCPixelFormat( DC: HDC):Boolean;
var
  i: Integer;
  nPixelFormat: Integer;
  pfd: TPixelFormatDescriptor;
  TmpDC : HDC;
begin
  Result := True;
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
  end;
  nPixelFormat := GetPixelFormat(DC);
  if nPixelFormat = 0 then
  begin
    //Ville: Kopiera pixeldepth från skrivbord
    TmpDC := GetDC(0);
    I := GetDeviceCaps(TmpDC, BITSPIXEL);
    ReleaseDC(0, TmpDC);
    if I in [16,24,32] then
      pfd.cColorBits := I;
    if pfd.cColorBits<>32 then
      pfd.cAlphaBits := 0;
    nPixelFormat := ChoosePixelFormat(DC, @pfd);
    if not SetPixelFormat(DC, nPixelFormat, @pfd) then
    begin
      Result := False;
      ZLog.GetLog(Self.ClassName).Write('Error SetPixelFormat Nr:'+IntToStr(GetLastError)+' nPFrmt:'+IntToStr(nPixelFormat));
    end;
  end;
end;


procedure TCustomGLPanel.CreateRenderContext;
begin
  // Create a rendering context.
  SetDCPixelFormat(Canvas.Handle);
  if hrc=0 then
    hrc := wglCreateContext(Canvas.Handle);
  if hrc <> 0 then
  begin
    ActivateRenderingContext(Canvas.Handle,hrc);
    glViewport(0,0,ClientWidth,ClientHeight);
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  end;
end;


procedure TCustomGLPanel.Paint;
begin
  // Draw the scene.
  if (not InitCalled) and (Self.Handle<>0) then
  begin
    InitCalled := True;
    if Assigned(OnGLInit) then
      OnGlInit(Self);
  end;

//      BeginPaint(Handle, ps);
  if (wglGetCurrentContext <> hrc) and (hrc<>0) then
    ActivateRenderingContext(Canvas.Handle,hrc);

  if Assigned(OnGLDraw) then
    OnGLDraw(Self);

  SwapBuffers(Canvas.Handle);
//      EndPaint(Handle, ps);
end;


constructor TCustomGLPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Locked := True;
  Color := clBlack;
end;

destructor TCustomGLPanel.Destroy;
begin
  // Clean up and terminate.
  inherited;
  if HasActiveContext then
    DeactivateRenderingContext;
  if hrc <> 0 then
    DestroyRenderingContext(hrc);
end;

procedure TCustomGLPanel.ForceInitGL;
begin
  Paint;
end;

procedure TCustomGLPanel.WMEraseBackground(var Msg:TWMEraseBkgnd);
begin
  Msg.Result := 0
end;

procedure TCustomGLPanel.WMGETDLGCODE(var Msg: TMessage);
begin
  //Needed to detect arrow keys in zzdc-runtime
  Msg.Result := DLGC_WANTARROWS;
end;

procedure TCustomGLPanel.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
end;

procedure TCustomGLPanel.DestroyHandle;
begin
  if HasActiveContext then
    DeactivateRenderingContext;
  inherited;
end;

procedure TCustomGLPanel.CreateHandle;
begin
  inherited;
  if Self.Parent<>nil then
  begin
    Platform_DesignerSetDC(Self.Canvas.Handle, Self.Handle);
    CreateRenderContext;
  end;
end;


initialization
  InitOpenGL;
finalization
// CloseOpenGL;
end.
