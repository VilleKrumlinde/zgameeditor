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
    TPFDPixelType = (GLp_TYPE_RGBA, GLp_TYPE_COLORINDEX);
    TPFDLayerType = (GLL_MAIN_PLANE, GLL_OVERLAY_PLANE, GLL_UNDERLAY_PLANE);
    // PIXELFORMATDESCRIPTOR flags
    TPFDFlag = (GLf_DOUBLEBUFFER, GLf_STEREO, GLf_DRAW_TO_WINDOW,
                GLf_DRAW_TO_BITMAP, GLf_SUPPORT_GDI, GLf_SUPPORT_OPENGL,
                GLf_GENERIC_FORMAT, GLf_NEED_PALETTE, GLf_NEED_SYSTEM_PALETTE,
                GLf_SWAP_EXCHANGE, GLf_SWAP_COPY);
    TPFDPixelTypes = set of TPFDPixelType;
    TPFDLayerTypes = set of TPFDLayerType;
    TPFDFlags      = set of TPFDFlag;

  TCustomGLPanel = class(TCustomPanel)
  private
//    DC: HDC;
    hrc: HGLRC;
    Palette: HPALETTE;
    FFirstTimeInFlag,InitCalled: Boolean;
    FNotifyRedraw: Boolean;
    FPFDChanged: Boolean;
    FPixelType: TPFDPixelTypes;
    FLayerType: TPFDLayerTypes;
    FFlags: TPFDFlags;
    GPixelType: Word;
    GLayerType: Longword;
    GFlags: Word;
    FColorBits: Cardinal;
    FDepthBits: Cardinal;
    FOnGLDraw: TNotifyEvent; // pointer to users routine of GL draw commands
    FOnAfterDraw: TNotifyEvent; // Event after all gl drawing and swapbuffer
    FOnGLInit: TNotifyEvent; // pointer to users routine for GL initialization
    FOnGLPrep: TNotifyEvent;
    FOnGLDestroying: TNotifyEvent; // pointer to users routine for static setup
    procedure ResetFlags (Value: TPFDFlags);
    procedure ResetPixelType (Value: TPFDPixelTypes);
    procedure ResetLayerType (Value: TPFDLayerTypes);
    function SetDCPixelFormat(DC: HDC):Boolean;
    procedure NewPaint;
  protected
   procedure CreateParams(var Params: TCreateParams); override;
   procedure WMEraseBackground( var msg:TWMEraseBkgnd ); message WM_ERASEBKGND;
   procedure WMGETDLGCODE( var msg:TMessage); message WM_GETDLGCODE;
    procedure SetFlags (Value: TPFDFlags);
    procedure SetPixelType (Value: TPFDPixelTypes);
    procedure SetLayerType (Value: TPFDLayerTypes);
    procedure SetColorBits (Value: Cardinal);
    procedure SetDepthBits (Value: Cardinal);
    function GetFlags : TPFDFlags;
    function GetPixelType: TPFDPixelTypes;
    function GetLayerType: TPFDLayerTypes;
    function GetColorBits: Cardinal;
    function GetDepthBits: Cardinal;
    procedure Paint; override;
    procedure Resize; override;
    property Locked default True;
    property Color default clBlack;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeforeDestruction; override;

    procedure GLReDraw;
    procedure NewGLPrep;
    property MouseCapture;
    property Canvas;
    property NotifyRedraw: Boolean read FNotifyRedraw write FNotifyRedraw default True;
    property GLColorBits: Cardinal read GetColorBits write SetColorBits default 16;
    property GLDepthBits: Cardinal read GetDepthBits write SetDepthBits default 16;
    property GLFlags: TPFDFlags read Getflags write SetFlags
      default [GLf_DRAW_TO_WINDOW , GLf_SUPPORT_OPENGL, GLf_DOUBLEBUFFER];
    property GLLayerType: TPFDLayerTypes read GetLayerType write SetLayerType
      default [GLL_MAIN_PLANE];
    property GLPixelType: TPFDPixelTypes read GetPixelType write SetPixelType
      default [GLp_TYPE_RGBA];
    property OnGLDraw: TNotifyEvent read FOnGLDraw write FOnGLDraw;
    property OnAfterDraw: TNotifyEvent read FOnAfterDraw write FOnAfterDraw;
    property OnGLInit: TNotifyEvent read FOnGLInit write FOnGLInit;
    property OnGLPrep: TNotifyEvent read FOnGLPrep write FOnGLPrep;
    property OnGLDestroying: TNotifyEvent read FOnGLDestroying write FOnGLDestroying;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnMouseWheel;
    procedure ForceInitGL;
    procedure ParentChanged;
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
    property GLColorBits;
    property GLDepthBits;
    property GLFlags;
    property GLLayerType;
//    property ParentColor;
//    property ParentCtl3D;
//    property ParentFont;
    property ParentShowHint;
    property GLPixelType;
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
    property OnGLPrep;
  end;

procedure Register;

implementation

uses ZLog, ZPlatform;

var
   errmess: String;

procedure TCustomGLPanel.Resize;
   begin
   NewGLPrep;
   inherited Resize;
//   if Assigned(OnResize) then OnResize(self);
   end;

procedure TCustomGLPanel.CreateParams(var Params: TCreateParams);
   begin
   inherited;
   Params.WindowClass.style := Params.WindowClass.style or CS_OWNDC;
   end;

function TCustomGLPanel.SetDCPixelFormat( DC: HDC):Boolean;
   var
      hHeap: THandle;
      nColors, i: Integer;
      lpPalette: PLogPalette;
      byRedMask, byGreenMask, byBlueMask: Byte;
      nPixelFormat: Integer;
      pfd: TPixelFormatDescriptor;
      TmpDC : HDC;
   begin
   result := True;
   FillChar(pfd, SizeOf(pfd), 0);

   with pfd do
      begin
      nSize     := sizeof(pfd);   // Size of this structure
      nVersion  := 1;             // Version number
      dwFlags   := GFlags;        // Flags
      iPixelType:= GPixelType;    // RGBA pixel values
      cColorBits:= 32; //FColorBits;    // 24-bit color
      cDepthBits:= FDepthBits;    // 32-bit depth buffer
      iLayerType:= GLayerType;    // Layer type
      cAlphaBits := 8;
      end;
   nPixelFormat := GetPixelFormat(DC);
   if  nPixelFormat = 0 then
   begin
     //Ville: Kopiera pixeldepth från skrivbord
     TmpDC := GetDC(0);
     I := GetDeviceCaps(TmpDC, BITSPIXEL);
     ReleaseDC(0, TmpDC);
     if I in [16,24,32] then
       pfd.cColorBits := I;

     nPixelFormat := ChoosePixelFormat(DC, @pfd);
     if not SetPixelFormat(DC, nPixelFormat, @pfd) then
     begin
       result := False;
       errmess := 'Error SetPixelFormat Nr:'+IntToStr(GetLastError)+' nPFrmt:'+IntToStr(nPixelFormat);
       ZLog.GetLog(Self.ClassName).Write(errmess);
       //Application.MAinForm.Caption := errmess;
     end;
   end;

   DescribePixelFormat(DC, nPixelFormat, sizeof(TPixelFormatDescriptor), pfd);

   if ((pfd.dwFlags and PFD_NEED_PALETTE) <> 0) then
      begin
      nColors   := 1 shl pfd.cColorBits;
      hHeap     := GetProcessHeap;
      lpPalette := HeapAlloc(hHeap, 0, sizeof(TLogPalette) + (nColors * sizeof(TPaletteEntry)));

      lpPalette^.palVersion := $300;
      lpPalette^.palNumEntries := nColors;

      byRedMask   := (1 shl pfd.cRedBits) - 1;
      byGreenMask := (1 shl pfd.cGreenBits) - 1;
      byBlueMask  := (1 shl pfd.cBlueBits) - 1;

      for i := 0 to nColors - 1 do
         begin
         lpPalette^.palPalEntry[i].peRed   := (((i shr pfd.cRedShift)   and byRedMask)   * 255) DIV byRedMask;
         lpPalette^.palPalEntry[i].peGreen := (((i shr pfd.cGreenShift) and byGreenMask) * 255) DIV byGreenMask;
         lpPalette^.palPalEntry[i].peBlue  := (((i shr pfd.cBlueShift)  and byBlueMask)  * 255) DIV byBlueMask;
         lpPalette^.palPalEntry[i].peFlags := 0;
         end;

      Palette := CreatePalette(lpPalette^);
      HeapFree(hHeap, 0, lpPalette);

      if (Palette <> 0) then
         begin
         SelectPalette(DC, Palette, False);
         RealizePalette(DC);
         end;
      end;
   end;

procedure TCustomGLPanel.ResetFlags (Value: TPFDFlags);
   begin
   GFlags := 0;
   if GLf_DOUBLEBUFFER in Value then GFlags := GFlags or PFD_DOUBLEBUFFER;
   if GLf_STEREO in Value then GFlags := GFlags or PFD_STEREO;
   if GLf_DRAW_TO_WINDOW in Value then GFlags := GFlags or PFD_DRAW_TO_WINDOW;
   if GLf_DRAW_TO_BITMAP in Value then GFlags := GFlags or PFD_DRAW_TO_BITMAP;
   if GLf_SUPPORT_GDI in Value then GFlags := GFlags or PFD_SUPPORT_GDI;
   if GLf_SUPPORT_OPENGL in Value then GFlags := GFlags or PFD_SUPPORT_OPENGL;
   if GLf_GENERIC_FORMAT in Value then GFlags := GFlags or PFD_GENERIC_FORMAT;
   if GLf_NEED_PALETTE in Value then GFlags := GFlags or PFD_NEED_PALETTE;
   if GLf_NEED_SYSTEM_PALETTE in Value then GFlags := GFlags or PFD_NEED_SYSTEM_PALETTE;
   if GLf_SWAP_EXCHANGE in Value then GFlags := GFlags or PFD_SWAP_EXCHANGE;
   if GLf_SWAP_COPY in Value then GFlags := GFlags or PFD_SWAP_COPY;
   end;

procedure TCustomGLPanel.ResetPixelType (Value: TPFDPixelTypes);
   begin
   if GLp_TYPE_RGBA in Value then GPixelType := PFD_TYPE_RGBA;
   if GLp_TYPE_COLORINDEX in Value then GPixelType := PFD_TYPE_COLORINDEX;
   end;

procedure TCustomGLPanel.ResetLayerType (Value: TPFDLayerTypes);
   begin
   if GLL_MAIN_PLANE in Value then GLayerType := PFD_MAIN_PLANE;
   if GLL_OVERLAY_PLANE in Value then GLayerType := PFD_OVERLAY_PLANE;
   if GLL_UNDERLAY_PLANE in Value then GLayerType := PFD_UNDERLAY_PLANE;
   end;

procedure TCustomGLPanel.SetFlags(Value: TPFDFlags);
   begin
   if FFlags <> Value then
      begin
      FFlags := Value;
      if not (csDesigning in ComponentState) then
         begin
         ResetFlags (Value);
         FPFDChanged := True;
         end;
      end;
   end;

procedure TCustomGLPanel.SetPixelType (Value: TPFDPixelTypes);
   begin
   if FPixelType <> Value then
      begin
      FPixelType := Value;
      if not (csDesigning in ComponentState) then
         begin
         ResetPixelType (Value);
         FPFDChanged := True;
         end;
      end;
   end;

procedure TCustomGLPanel.SetLayerType (Value: TPFDLayerTypes);
   begin
   if FLayerType <> Value then
      begin
      FLayerType := Value;
      if not (csDesigning in ComponentState) then
         begin
         ResetLayerType (Value);
         FPFDChanged := True;
         end;
      end;
   end;

procedure TCustomGLPanel.SetColorBits (Value: Cardinal);
   begin
   FColorBits := Value;
   end;

procedure TCustomGLPanel.SetDepthBits (Value: Cardinal);
   begin
   FDepthBits := Value;
   end;

function TCustomGLPanel.GetFlags : TPFDFlags;
   begin
   GetFlags := FFlags;
   end;

function TCustomGLPanel.GetPixelType: TPFDPixelTypes;
   begin
   GetPixelType := FPixelType;
   end;

function TCustomGLPanel.GetLayerType: TPFDLayerTypes;
   begin
   GetLayerType := FLayerType;
   end;

function TCustomGLPanel.GetColorBits: Cardinal;
   begin
   GetColorBits := FColorBits;
   end;

function TCustomGLPanel.GetDepthBits: Cardinal;
   begin
   GetDepthBits := FDepthBits;
   end;

procedure TCustomGLPanel.Paint;
   var
      r: TRect;
   begin
   if (csDesigning in ComponentState) then
      begin
      r.Left := 0;
      r.Top := 0;
      r.Bottom := Height;
      r.Right := Width;
      Canvas.FillRect(r);
      Canvas.TextOut(0,0,ClassName);
      end
    else
      NewPaint;
   end;

procedure TCustomGLPanel.NewGLPrep;
   begin
   if not FFirstTimeInFlag then
      if Assigned(OnGLPrep) then OnGLPrep(self);
//   if Assigned(OnResize) then OnResize(self);
   end;

procedure TCustomGLPanel.NewPaint;
//   var
//      ps : TPaintStruct;
   begin
   inherited;
   if not (csDesigning in ComponentState) then
      begin
      // Draw the scene.
      if (FPFDChanged and (not FFirstTimeInFlag)) then
         begin
         SetDCPixelFormat(Canvas.Handle);
         FPFDChanged := False;
         end;

      if FFirstTimeInFlag then
      begin
        FFirstTimeInFlag := False;
        // Create a rendering context.
        SetDCPixelFormat(Canvas.Handle);
        if hrc=0 then
          hrc := wglCreateContext(Canvas.Handle);
        if hrc = 0 then
          FFirstTimeInFlag := True
        else
          ActivateRenderingContext(Canvas.Handle,hrc);
      end;

      if not InitCalled then
      begin
        InitCalled := True;
        if Assigned(OnGLInit) then OnGlInit(self);
        if Assigned(OnGLPrep) then OnGLPrep(self);
        if Assigned(OnResize) then OnResize(self);
      end;

//      BeginPaint(Handle, ps);
      if (wglGetCurrentContext <> hrc) and (hrc<>0) then
        ActivateRenderingContext(Canvas.Handle,hrc);
      if Assigned(OnGLDraw) then OnGLDraw(self);
      if GLf_DOUBLEBUFFER in FFlags then
         SwapBuffers(Canvas.Handle);
//      EndPaint(Handle, ps);
      if Assigned(OnAfterDraw) then OnAfterDraw(self);


      end;
   end;

constructor TCustomGLPanel.Create(AOwner: TComponent);
   begin
   inherited Create(AOwner);
   FPFDChanged := False;
   FPixelType := [GLp_TYPE_RGBA];
   FFlags := [GLf_DRAW_TO_WINDOW, GLf_SUPPORT_OPENGL, GLf_DOUBLEBUFFER];
   FLayerType := [GLL_MAIN_PLANE];
   FColorBits := 16;
   FDepthBits := 16;
   ResetFlags(FFlags);
   ResetPixelType(FPixelType);
   ResetLayerType(FLayerType);
   FFirstTimeInFlag := True;
   FNotifyRedraw := True;
   Locked := True;
   Color := clBlack;
   end;

destructor TCustomGLPanel.Destroy;
   begin
   if not (csDesigning in ComponentState) then
      begin
      // Clean up and terminate.
      if HasActiveContext then DeactivateRenderingContext;
      if hrc <> 0 then DestroyRenderingContext(hrc);
      if (Palette <> 0) then
         DeleteObject(Palette);
      end;
   inherited;
   end;

procedure TCustomGLPanel.ParentChanged;
begin
  if HasActiveContext then
    DeactivateRenderingContext;
  FFirstTimeInFlag := True;
  Platform_DesignerSetDC(Self.Canvas.Handle, Self.Handle);
end;

procedure TCustomGLPanel.ForceInitGL;
begin
  NewPaint;
end;

procedure TCustomGLPanel.GLReDraw;
   begin
//   NewPaint;
   if FNotifyRedraw then
      begin
      Canvas.Brush.Style := bsClear;
      Canvas.Pen.Style := psSolid;
      Canvas.TextRect(ClientRect,Width div 2, 0,'Redrawing');
      Canvas.Brush.Style := bsSolid;
      end;
   Invalidate;
   end;

procedure TCustomGLPanel.WMEraseBackground( var msg:TWMEraseBkgnd );
   begin
   msg.Result := 0
   end;

procedure TCustomGLPanel.WMGETDLGCODE(var msg: TMessage);
begin
  //Needed to detect arrow keys in zzdc-runtime
  msg.result := DLGC_WANTARROWS;
end;

procedure Register;
   begin
   RegisterComponents('OpenGL', [TGLPanel]);
   end;

procedure TCustomGLPanel.BeforeDestruction;
begin
  inherited;

  if Assigned(FOnGLDestroying) then
    FOnGLDestroying(Self);
end;

initialization
InitOpenGL;
finalization
// CloseOpenGL;
end.
