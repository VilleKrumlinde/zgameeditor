{Copyright (c) 2008 Ville Krumlinde

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

{$ifndef minimal}
procedure Platform_DesignerSetDC(DC : longword; HWnd : longword);
procedure Platform_DesignerWindowProc(P : pointer);
{$endif}

{$if defined(zzdc_screensaver) or defined(zzdc_activex)}
//Defined in interface so it can be assigned from activex-form
var
  //Screensaver example
  //http://www.christiancoders.com/cgi-bin/articles/show_article.pl?f=briant05292003004836.html
  PreviewParentHWnd : longword; //HWND;
  {$if defined(zzdc_activex)}
  ZgeActiveXFinished : boolean;
  {$ifend}
{$ifend}

implementation

uses ZOpenGL, ZLog, {$ifdef FPC}Windows{$else}Winapi.Windows{$endif}, ZApplication, AudioPlayer,
  Commands
 {$ifndef minimal}
  ,SysUtils
 {$endif}
 {$IFDEF FPC}
  ,WinEvent
 {$ENDIF}
 ;


//gl code based on, http://www.sulaco.co.za

var
  h_Wnd  : HWND;                     // Global window handle
  h_DC   : HDC;                      // Global device context
  h_RC   : HGLRC;                    // OpenGL rendering context

  Keys : array[0..255] of boolean;
  KeyMap : array[AnsiChar] of byte;
  LastMouse : TPoint;


procedure UpdateJoysticks; forward;
procedure InitJoysticks; forward;


//Copied from Messages.pas
const
  WM_CREATE           = $0001;
  WM_DESTROY          = $0002;
  WM_CLOSE            = $0010;
  WM_KEYDOWN          = $0100;
  WM_KEYUP            = $0101;
  WM_MOUSEMOVE        = $0200;
  WM_LBUTTONDOWN      = $0201;
  WM_LBUTTONUP        = $0202;
  WM_RBUTTONDOWN      = $0204;
  WM_RBUTTONUP        = $0205;
  WM_MBUTTONDOWN      = $0207;
  WM_MBUTTONUP        = $0208;
  WM_QUIT             = $0012;
  WM_INITDIALOG       = $0110;
  WM_COMMAND          = $0111;
  WM_ACTIVATE         = $0006;
  WM_ACTIVATEAPP      = $001C;
  WM_NCACTIVATE       = $0086;
  WM_MOUSEWHEEL       = $020A;
  WM_LBUTTONDBLCLK    = $0203;

  BM_SETCHECK = $00F1;
  CBN_SELCHANGE  = 1;

  CB_ADDSTRING             = $0143;
  CB_SETCURSEL             = $014E;
  CB_GETCURSEL             = $0147;

  {$ifdef fpc}
  ID_OK = 1;
  ID_CANCEL = 2;
  {$endif}


type
  //From Messages.pas
  TWordFiller = record
  {$IFDEF CPUX86}
    Filler: array[1..2] of Byte; // Pad word make it 4 Bytes (2+2)
  {$ENDIF}
  {$IFDEF CPUX64}
    Filler: array[1..6] of Byte; // Pad word to make it 8 Bytes (2+6)
  {$ENDIF}
  end;

  TDWordFiller = record
  {$IFDEF CPUX64}
    Filler: array[1..4] of Byte; // Pad DWORD to make it 8 bytes (4+4) [x64 only]
  {$ENDIF}
  end;

  PMessage = ^TMessage;
  TMessage = record
    Msg: Cardinal;
    case Integer of
      0: (
        WParam: WPARAM;
        LParam: LPARAM;
        Result: LRESULT);
      1: (
        WParamLo: Word;
        WParamHi: Word;
        WParamFiller: TDWordFiller;
        LParamLo: Word;
        LParamHi: Word;
        LParamFiller: TDWordFiller;
        ResultLo: Word;
        ResultHi: Word;
        ResultFiller: TDWordFiller);
  end;

{$POINTERMATH ON}
function WndProc(hWnd: HWND; Msg: UINT;  wParam: WPARAM;  lParam: LPARAM): LRESULT; stdcall;
var
  ZApp : TZApplication;
begin
  Result := 0;
  case Msg of
    WM_CREATE:
      begin
        // Insert stuff you want executed when the program starts
        {$ifdef minimal}
        SetWindowLongPtr(hWnd,GWL_USERDATA, NativeInt(PCreateStruct(lParam).lpCreateParams) );
        {$endif}
      end;
    WM_CLOSE,WM_DESTROY:
      begin
        //In screensaver mode with ParentWindow: parent window sends destroy to
        //all child windows, important to quit otherwise process will keep on living.
        {$ifdef minimal}
        PostQuitMessage(0);
        {$endif}
      end;
    WM_KEYDOWN:       // Set the pressed key (wparam) to equal true so we can check if its pressed
      begin
        {$ifdef cpux64}
        Keys[ PByte(@wParam)[4] ] := True;
        {$else}
        Keys[Lo(wParam)] := True;
        {$endif}
      end;
    WM_KEYUP:         // Set the released key (wparam) to equal false so we can check if its pressed
      begin
        {$ifdef minimal}
        if (wParam=VK_ESCAPE) then
        begin
          ZApp := TZApplication(GetWindowLongPtr(hWnd,GWL_USERDATA));
          if ZApp.EscapeToQuit then
            PostMessage(hWnd,WM_QUIT,0,0);
        end;
        {$endif}
        {$ifdef cpux64}
        Keys[ PByte(@wParam)[4] ] := False;
        {$else}
        Keys[Lo(wParam)] := False;
        {$endif}
      end;
    WM_MOUSEMOVE :
      begin
        LastMouse.X := LoWord(lParam);
        LastMouse.Y := HiWord(lParam);
      end;
    WM_LBUTTONDOWN,WM_LBUTTONUP :
      //Use '{' as key for left mouse button
      Keys[ Ord('{') ] := Msg=WM_LBUTTONDOWN;
    WM_RBUTTONDOWN,WM_RBUTTONUP :
      //Use '}' as key for right mouse button
      Keys[ Ord('}') ] := Msg=WM_RBUTTONDOWN;
    WM_MBUTTONDOWN,WM_MBUTTONUP :
      //Use '|' as key for middle mouse button
      Keys[ Ord('|') ] := Msg=WM_MBUTTONDOWN;
    WM_LBUTTONDBLCLK :
      Keys[ Ord('[') ] := True;
    WM_MOUSEWHEEL :
      begin
        ZApp := TZApplication(GetWindowLongPtr(hWnd,GWL_USERDATA));
        if ZApp<>nil then
          ZApp.MouseWheelDelta := shortint(wParam shr 16) div 120;
      end;
{$ifdef minimal}
    else
      Result := DefWindowProc(hWnd, Msg, wParam, lParam);    // Default result if nothing happens
{$endif}
  end;

  {$ifdef zgeviz}
  //Always pass on the message to avoid zgeviz stealing keypresses from FL-studio
  if (Msg=WM_KEYDOWN) or (Msg=WM_KEYUP) then
    Result := DefWindowProc(hWnd, Msg, wParam, lParam);
  {$endif}
end;

{$if (not defined(minimal))}
procedure Platform_DesignerWindowProc(P : pointer);
var
  M : PMessage;
begin
  //Brygga mellan vcl-tmessage och win-api wndproc.
  M := PMessage(P);
  WndProc(h_Wnd,M.Msg,M.WParam,M.LParam);
  UpdateJoysticks;
end;
{$ifend}


procedure KillWnd(Fullscreen : Boolean);
begin
  if Fullscreen then             // Change back to non fullscreen
  begin
    {$IFDEF FPC}
       ChangeDisplaySettings(nil, 0);
    {$ELSE}
       ChangeDisplaySettings( devmode(nil^), 0);
    {$ENDIF}
    ShowCursor(True);
  end;

  // Makes current rendering context not current, and releases the device
  // context that is used by the rendering context.
  if (not wglMakeCurrent(h_DC, 0)) then
    {$ifdef zdebug}
    MessageBox(0, 'Release of DC and RC failed!', 'Error', MB_OK or MB_ICONERROR)
    {$endif}
    ;

  // Attempts to delete the rendering context
  if (not wglDeleteContext(h_RC)) then
  begin
    {$ifdef zdebug}
    MessageBox(0, 'Release of rendering context failed!', 'Error', MB_OK or MB_ICONERROR);
    {$endif}
    h_RC := 0;
  end;

  // Attemps to release the device context
  if ((h_DC > 0) and (ReleaseDC(h_Wnd, h_DC) = 0)) then
  begin
    {$ifdef zdebug}
    MessageBox(0, 'Release of device context failed!', 'Error', MB_OK or MB_ICONERROR);
    {$endif}
    h_DC := 0;
  end;

  // Attempts to destroy the window
  if ((h_Wnd <> 0) and (not DestroyWindow(h_Wnd))) then
  begin
    {$ifdef zdebug}
    MessageBox(0, 'Unable to destroy window!', 'Error', MB_OK or MB_ICONERROR);
    {$endif}
    h_Wnd := 0;
  end;

  // Attempts to unregister the window class
  if (not UnRegisterClass('OpenGL', hInstance)) then
  begin
    //Skip errormessage if activex, otherwise it will hang IE
    {$ifdef zdebug}
    MessageBox(0, 'Unable to unregister window class!', 'Error', MB_OK or MB_ICONERROR);
    {$endif}
//    hInstance := 0;
  end;
end;




var
  Fullscreen : Boolean;

procedure Platform_ShutdownScreen;
begin
  KillWnd(FullScreen);
end;

function Platform_InitScreen(var Width, Height : integer; UseFullScreen : boolean; Title : PAnsiChar; ZApp : pointer) : integer;
var
  wndClass : TWndClassA;         // Window class
  dwStyle : DWORD;              // Window styles
  dwExStyle : DWORD;            // Extended window styles
  dmScreenSettings : DEVMODE;   // Screen settings (fullscreen, etc...)
  PixelFormat : GLuint;         // Settings for the OpenGL rendering
  pfd : TPIXELFORMATDESCRIPTOR;  // Settings for the OpenGL window
  ScreenDC : HDC;

  PixelDepth : Integer;

  R : TRect;
  DesktopSize : TRect;
  WinX,WinY,DesktopPixelDepth : integer;
  UseFullScreenWindow : boolean;
begin
  GetClientRect(GetDesktopWindow,DesktopSize);

  FullScreen := UseFullScreen;

  //FullscreenWindow means use full desktop size without making a mode-switch
  UseFullScreenWindow := Width=-1;
  if UseFullScreenWindow then
  begin
    FullScreen := False;
    Width := DesktopSize.Right;
    Height := DesktopSize.Bottom;
  end;

  {$if defined(zzdc_screensaver) or defined(zzdc_activex)}
  if PreviewParentHWnd<>0 then
    FullScreen := False;
  {$ifend}

  PixelDepth := 32;

  if not Fullscreen then
  begin
    //If windowed, do not ask for more pixeldepth than supported
    //otherwise hw-acceleration will be disabled
    ScreenDC := GetDC(0);
    DesktopPixelDepth := GetDeviceCaps(ScreenDC, BITSPIXEL);
    if PixelDepth>DesktopPixelDepth then
      PixelDepth := DesktopPixelDepth;
    ReleaseDC(0, ScreenDC);
  end;

  FillChar(wndClass, SizeOf(wndClass),0);  // Clear the window class structure

  // Set up the window class
//  wndClass.cbSize := SizeOf(wndClass);
  wndClass.style  := CS_HREDRAW or    // Redraws entire window if length changes
                     CS_VREDRAW or    // Redraws entire window if height changes
                     CS_OWNDC or      // Unique device context for the window
                     CS_DBLCLKS;      // Receive doubleclick messages
  wndClass.lpfnWndProc   := @WndProc;        // Set the window procedure to our func WndProc
  wndClass.hInstance     := HInstance;  //Delphi rtl gets the hinstance (SysInit.Unit)
  wndClass.hCursor       := LoadCursor(0, IDC_ARROW);
  wndClass.lpszClassName := 'OpenGL';
//  wndClass.hIconSm := LoadIcon(0, IDI_APPLICATION);
  wndClass.hIcon := LoadIcon(0, IDI_APPLICATION);

//if LoadImageA(HInstance, '1', IMAGE_ICON, 32, 32, 0)=0 then
//  zhalt('crash2');
//if LoadIconA(hInstance,'HEJ')=0 then
//  zhalt('crash');

  if (RegisterClassA(wndClass) = 0) then  // Attemp to register the window class
    {$if not defined(zzdc_activex)}
    ZHalt('Failed to register window class')
    {$ifend};

  // Change to fullscreen if so desired
  if Fullscreen then
  begin
    FillChar(dmScreenSettings, SizeOf(dmScreenSettings), 0);
    with dmScreenSettings do begin              // Set parameters for the screen setting
      dmSize       := SizeOf(dmScreenSettings);
      dmPelsWidth  := Width;                    // Window width
      dmPelsHeight := Height;                   // Window height
      dmBitsPerPel := PixelDepth;               // Window color depth
      dmFields     := DM_PELSWIDTH or DM_PELSHEIGHT or DM_BITSPERPEL;
    end;

    // Try to change screen mode to fullscreen
    if (ChangeDisplaySettings(dmScreenSettings, CDS_FULLSCREEN) = DISP_CHANGE_FAILED) then
      ZHalt('Unable to switch to fullscreen.');
  end;

  if Fullscreen or UseFullScreenWindow then
  begin
    dwStyle := WS_POPUP or                // Creates a popup window
               WS_CLIPCHILDREN            // Doesn't draw within child windows
               or WS_CLIPSIBLINGS;        // Doesn't draw within sibling windows
    dwExStyle := WS_EX_APPWINDOW;         // Top level window
  end
  else
  begin
    dwStyle :=
               (WS_OVERLAPPED or WS_CAPTION or WS_SYSMENU) or
               //WS_OVERLAPPEDWINDOW or     // Creates an overlapping window
               WS_CLIPCHILDREN or         // Doesn't draw within child windows
               WS_CLIPSIBLINGS;           // Doesn't draw within sibling windows
    dwExStyle := WS_EX_APPWINDOW or       // Top level window
                 WS_EX_WINDOWEDGE;        // Border with a raised edge
  end;


  {$if defined(zzdc_screensaver) or defined(zzdc_activex)}
  if PreviewParentHWnd<>0 then
  begin
    //We are a child window
    GetClientRect(PreviewParentHWnd, R);
    Height := R.Bottom;
    Width := R.Right;
    dwStyle := WS_VISIBLE or WS_CHILD;
    dwExStyle := 0;
  end;
  {$ifend}

  {$ifndef minimal}
  try
  {$endif}
    R.Top := 0;
    R.Left := 0;
    R.Bottom := Height;
    R.Right := Width;
//    ViewportH := Height;
//    ViewportW := Width;

    if (not Fullscreen) and (not UseFullScreenWindow)
      {$if defined(zzdc_screensaver) or defined(zzdc_activex)}
      and (PreviewParentHWnd=0)
      {$ifend}
    then
    begin
      //Önskad fönsterstorlek måste justeras så att titlebar får plats
      AdjustWindowRectEx(R,dwStyle,False,dwExStyle);
      WinX := (DesktopSize.Right - R.Right) div 2;
      WinY := (DesktopSize.Bottom - R.Bottom) div 2;
    end else
    begin
      WinX := 0;
      WinY := 0;
    end;

    // Attempt to create the actual window
    h_Wnd := CreateWindowExA(dwExStyle,      // Extended window styles
                            'OpenGL',       // Class name
                            Title,   // Window title (caption)
                            dwStyle,        // Window styles
                            WinX, WinY,     // Window position
                            R.Right - R.Left, R.Bottom - R.Top,  // Size of window
                              {$if defined(zzdc_screensaver) or defined(zzdc_activex)}
                              PreviewParentHWnd
                              {$else}
                              0
                              {$ifend}, //Parent window
                            0,              // No menu
                            hInstance,     // Instance
                            ZApp);         // Pass to WM_CREATE
    if h_Wnd = 0 then
      ZHalt('Unable to create window.');

    // Try to get a device context
    h_DC := GetDC(h_Wnd);
    if (h_DC = 0) then
      ZHalt('Unable to get a device context.');

    // Settings for the OpenGL window
    FillChar(pfd, SizeOf(pfd), 0);
    with pfd do
    begin
      nSize           := SizeOf(TPIXELFORMATDESCRIPTOR); // Size Of This Pixel Format Descriptor
      nVersion        := 1;                    // The version of this data structure
      dwFlags         := PFD_DRAW_TO_WINDOW    // Buffer supports drawing to window
                         or PFD_SUPPORT_OPENGL // Buffer supports OpenGL drawing
                         or PFD_DOUBLEBUFFER;  // Supports double buffering
      iPixelType      := PFD_TYPE_RGBA;        // RGBA color format
      cColorBits      := PixelDepth;           // OpenGL color depth
      //cRedBits        := 8;                    // Number of red bitplanes
      //cRedShift       := 0;                    // Shift count for red bitplanes
      //cGreenBits      := 8;                    // Number of green bitplanes
      //cGreenShift     := 0;                    // Shift count for green bitplanes
      //cBlueBits       := 8;                    // Number of blue bitplanes
      //cBlueShift      := 0;                    // Shift count for blue bitplanes

      //Only ask for alpha bits in 32 bit mode
      //Otherwise hw-acceleration might be disabled
      if PixelDepth=32 then
        cAlphaBits      := 8;                    // Not supported

      //cAlphaShift     := 0;                    // Not supported
      //cAccumBits      := 0;                    // No accumulation buffer
      //cAccumRedBits   := 0;                    // Number of red bits in a-buffer
      //cAccumGreenBits := 0;                    // Number of green bits in a-buffer
      //cAccumBlueBits  := 0;                    // Number of blue bits in a-buffer
      //cAccumAlphaBits := 0;                    // Number of alpha bits in a-buffer
      cDepthBits      := 16;                   // Specifies the depth of the depth buffer
      //cStencilBits    := 0;                    // Turn off stencil buffer
      //cAuxBuffers     := 0;                    // Not supported
      iLayerType      := PFD_MAIN_PLANE;       // Ignored
      //bReserved       := 0;                    // Number of overlay and underlay planes
      //dwLayerMask     := 0;                    // Ignored
      //dwVisibleMask   := 0;                    // Transparent color of underlay plane
      //dwDamageMask    := 0;                     // Ignored
    end;

    // Attempts to find the pixel format supported by a device context that is the best match to a given pixel format specification.
    PixelFormat := ChoosePixelFormat(h_DC, {$IFNDEF FPC}@{$ENDIF}pfd);
    if (PixelFormat = 0) then
      ZHalt('Unable to find a suitable pixel format');

    // Sets the specified device context's pixel format to the format specified by the PixelFormat.
    if (not SetPixelFormat(h_DC, PixelFormat, @pfd)) then
      ZHalt('Unable to set the pixel format');

    // Create a OpenGL rendering context
    h_RC := wglCreateContext(h_DC);
    if (h_RC = 0) then
      ZHalt('Unable to create an OpenGL rendering context');

    // Makes the specified OpenGL rendering context the calling thread's current rendering context
    if (not wglMakeCurrent(h_DC, h_RC)) then
      ZHalt('Unable to activate OpenGL rendering context');

 {$ifndef minimal}
  except
    KillWnd(Fullscreen);
    raise;
  end;
 {$endif}

  // Settings to ensure that the window is the topmost window
  ShowWindow(h_Wnd, SW_SHOW);
  SetForegroundWindow(h_Wnd);
  SetFocus(h_Wnd);

  if FullScreen then
    //Try to workaround corrupt font problem in fullscreen
    Sleep(500);

  // Ensure the OpenGL window is resized properly
//  ResizeWndGL(Width, Height);

  {$ifdef zzdc_screensaver}
  //Must have initial mouseposition in screensaver mode
  GetCursorPos(LastMouse);
  {$endif}

  Result := h_Wnd;
end;

procedure Platform_SetWindowCaption(Title : PAnsiChar);
begin
  SetWindowTextA(h_Wnd,Title);
end;

procedure Platform_ShowMouse(Visible : boolean);
begin
  //Hide mouse, except in preview mode
  {$if defined(zzdc_screensaver) or defined(zzdc_activex)}
  if PreviewParentHWnd<>0 then
    Exit;
  {$ifend}
  ShowCursor(Visible);
end;

procedure Platform_SwapBuffers;
begin
  SwapBuffers(h_DC);
end;


procedure Platform_Run(Callback : TRunCallback);
//WinMain som behandlar meddelanden och anropar callback
var
  Msg : TMsg;
  Finished : Boolean;
  {$ifdef zzdc_screensaver}
  EndTime : single;
  {$endif}
begin
  Finished := False;

  {$ifdef zzdc_screensaver}
  if PreviewParentHWnd<>0 then
  begin
    //The control panel preview window does not show until app is idle
    //This half-second wait-loop will make the preview window appear
    EndTime := Platform_GetTime + 0.5;
    while Platform_GetTime<EndTime do
      if (PeekMessage(msg, 0, 0, 0, PM_REMOVE)) then // Check if there is a message for this window
      begin
        TranslateMessage(msg);
        DispatchMessage(msg);
      end;
  end;
  {$endif}

  // Main message loop:
  while not Finished do
  begin
    if (PeekMessage(msg, 0, 0, 0, PM_REMOVE)) then // Check if there is a message for this window
    begin
      {$ifdef zzdc_screensaver}
      if PreviewParentHWnd=0 then
        case Msg.Message of
          WM_MOUSEMOVE :
            Finished := (Abs(LOWORD(Msg.lParam) - LastMouse.X) > 5) or
              (Abs(HIWORD(Msg.lParam) - LastMouse.Y) > 5);
          WM_KEYDOWN,WM_ACTIVATE,WM_ACTIVATEAPP,WM_NCACTIVATE : Finished := True;
        end;
      {$endif}
      if (msg.message = WM_QUIT) then     // If WM_QUIT message received then we are done
        finished := True
      else
      begin                               // Else translate and dispatch the message to this window
        TranslateMessage(msg);
        DispatchMessage(msg);
      end;
    end
    else
    begin
      if not (Callback()) then
        Finished := True;

      UpdateJoysticks;

      {$ifdef zzdc_activex}
      if ZgeActiveXFinished then
        Finished := True;
      {$endif}

      {$if defined(zzdc_screensaver) or defined(zzdc_activex)}
      //Sleep(5);  //Avoid using all cpu in screensaver mode
      {$ifend}
    end;
  end;
end;


// Keyboard input

procedure InitKeyMap;
var
  I : integer;
begin
  for I := 0 to 255 do
    KeyMap[AnsiChar(I)] := I;
  KeyMap[UpKey] := VK_UP;
  KeyMap[DownKey] := VK_DOWN;
  KeyMap[LeftKey] := VK_LEFT;
  KeyMap[RightKey] := VK_RIGHT;
end;


function Platform_IsKeyPressed(C : AnsiChar) : boolean;
begin
  Result := Keys[ KeyMap[C] ];
  if Result and (C='[') then
    //Buffer doubleclick event until read
    Keys[ Ord('[') ] := False;
end;

function Platform_CommandLine(Switch : PAnsiChar) : boolean;
var
  P : PAnsiChar;
begin
  {$ifdef zzdc_screensaver}
  if Switch[0] in ['f','s'] then
  begin //screensaver always fullscreen no sound
    Result := True;
    Exit;
  end;
  {$endif}
  P := GetCommandLineA;
  Result := False;
  while(P[0]<>#0) do
  begin
    if (P[0] in ['-','/']) and (P[1]=Switch[0]) then
    begin
      Result := True;
      Break;
    end;
    Inc(P);
  end;
end;

procedure Platform_Error(ErrorMessage : PAnsiChar);
begin
  MessageBoxA(H_Wnd, ErrorMessage, 'ZZDC Error', MB_OK);
end;

function Platform_GetMousePos : TZPointi;
begin
  if Fullscreen then
    Result := TZPointi(LastMouse)
  else
  begin
    //I fönster-läge så behöver muskoord klippas till fönsterstorlek
    //Och getcursorpos används pga wm_mousemove enbart skickas till aktuellt fönster
    GetCursorPos(TPoint(Result));
    ScreenToClient(h_Wnd,TPoint(Result));
  end;
end;

procedure Platform_SetMousePos(const X,Y : integer);
var
  Tmp : TPoint;
begin
  Tmp.X := X;
  Tmp.Y := Y;
  if not Fullscreen then
    ClientToScreen(h_Wnd,Tmp);
  SetCursorPos(Tmp.X,Tmp.Y);
end;

function Platform_GetExeFileName : PAnsiChar;
begin
  GetMem(Result,256);
  GetModuleFileNameA(0, Result, 256)
end;


{$ifndef minimal}
var
  DesignerFilePath : AnsiString;

procedure Platform_DesignerSetFilePath(const P : AnsiString);
begin
  DesignerFilePath := P;
end;
{$endif}

procedure MakeFullPath(FileName : PAnsiChar; var Buf : array of ansichar);
{$ifdef minimal}
var
  I : integer;
{$endif}
begin
  {$ifdef minimal}
  I := GetModuleFileNameA(0,Buf,255);
  while (I>0) and (Buf[I]<>'\') do
    Dec(I);
  if I>0 then
  begin
    Buf[I+1]:=#0;
    //lstrcat(Buf,FileName);
    ZStrCat(PAnsiChar(@Buf),FileName);
  end;
  {$else}
  Assert(Length(DesignerFilePath) + Length(FileName) < Length(Buf));
  ZStrCopy(PAnsiChar(@Buf),PAnsiChar(DesignerFilePath));
  ZStrCat(PAnsiChar(@Buf),FileName);
  {$endif}
end;

procedure Platform_WriteFile(FileName : PAnsiChar; Memory : pointer; Size : integer; Append : Boolean);
var
  Handle : THandle;
  Dummy : longword;
  Buf : array[0..254] of AnsiChar;
  OpenFlag : LongWord;
begin
  MakeFullPath(FileName,Buf);
  FileName := @Buf;

  if Append then
    OpenFlag := OPEN_ALWAYS
  else
    OpenFlag := CREATE_ALWAYS;

  Handle := CreateFileA(FileName, GENERIC_WRITE,
    0, nil, OpenFlag,
    FILE_ATTRIBUTE_NORMAL, 0);

  if Handle=INVALID_HANDLE_VALUE then
  begin
    {$ifdef zlog}
    ZLog.GetLog('Platform').Write('Could not open file: ' + FileName);
    {$endif}
    Exit;
  end;

  SetFilePointer(Handle, 0, nil, FILE_END);
  WriteFile(Handle, Memory^, Size, Dummy, nil);

  CloseHandle(Handle);
end;

procedure Platform_ReadFile(FileName : PAnsiChar; var Memory : pointer; var Size : integer; IsRelative : Boolean);
var
  Handle : THandle;
  Dummy : longword;
  Buf : array[0..254] of AnsiChar;
begin

  //Open file relative to path of exe-file
  if IsRelative then
  begin
    MakeFullPath(FileName,Buf);
    FileName := @Buf;
  end;

  Handle := CreateFileA(FileName, GENERIC_READ,
    0, nil, OPEN_EXISTING,
    FILE_ATTRIBUTE_NORMAL, 0);

  if Handle=INVALID_HANDLE_VALUE then
  begin
    {$ifdef zlog}
    ZLog.GetLog('Platform').Write('File missing: ' + FileName);
    {$endif}
    Exit;
  end;

  Size := GetFileSize(Handle,nil);
  GetMem(Memory,Size);
  ReadFile(Handle, Memory^, Size, Dummy, nil);
  CloseHandle(Handle);
end;



function Platform_GenerateFontDisplayLists(Size : integer; FirstChar,LastChar : integer) : integer;
var
  Quantity : integer;
  Start : integer;
  OldFont,Font : HFONT;
  Tries : integer;
begin
  Quantity := LastChar - FirstChar;
  Start := glGenLists(Quantity);
  Font := CreateFont( (Size-2),             // Height Of Font
                          Size-2,             // Width Of Font
                          0,              // Angle Of Escapement
                          0,              // Orientation Angle
                          1000,             // Font Weight
                          0,              // Italic
                          0,              // Underline
                          0,              // Strikeout
                          ANSI_CHARSET,           // Character Set Identifier
                          OUT_TT_PRECIS ,         // Output Precision
                          CLIP_DEFAULT_PRECIS,          // Clipping Precision
                          ANTIALIASED_QUALITY,          // Output Quality
                          FF_DONTCARE or DEFAULT_PITCH, // Family And Pitch
                          'Lucida Console'); //'Verdana'); //'Arial Black'); //'Courier New');
  oldfont := HFONT(SelectObject(h_DC, font));           // Selects The Font We Want

  //In some cases (16 bit color) wglUseFontBitmaps failse the first time
  Tries:=2;
  while not wglUseFontBitmaps(h_DC,FirstChar,Quantity,Start) and (Tries>0) do
    Dec(Tries);

  //  wglUseFontOutlines(h_DC, FirstChar, Quantity, Start, 0.0, 0.1, WGL_FONT_POLYGONS, nil);
  //alt. med getglyphoutline så kan texturer skapas direkt
  SelectObject(h_DC, oldfont);
  DeleteObject(font);
  //todo delete lists on finalization
  Result := Start;
end;


var
  TimerFreq,TimerStart : int64;

function Platform_GetTime : single;
var
  Count : int64;
begin
  //http://www.devsource.com/article2/0,1895,2063854,00.asp
  QueryPerformanceCounter(Count);

  //Om man inte drar ifrån timerstart så blir värdet för högt
  //och det blir frameskips.
  Count := (Count-TimerStart);
//  Count := Count * 1000;

//QueryPerformanceFrequency(TimerFreq);
  Result := (Count/TimerFreq);

//  Result := Count div TimerFreq;
//  Result := GetTickCount;
end;

function Platform_GetSystemTime : integer;
var
  T : TSystemTime;
begin
  GetLocalTime(T);
  Result := T.wHour * 3600 + T.wMinute * 60 + T.wSecond;
end;

procedure Platform_DesignerSetDC(DC : longword; HWnd : longword);
begin
  H_DC := DC;
  H_Wnd := HWnd;
end;

function Platform_LoadLinkedResource : TZInputStream;
var
  HResInfo: THandle;
  HGlobal: THandle;
  Size : integer;
begin
  Result := nil;
  HResInfo := FindResource(HInstance,'data_file', RT_RCDATA);
  //if HResInfo = 0 then Error;
  HGlobal := LoadResource(HInstance, HResInfo);
  //if HGlobal = 0 then Error;
  Size := SizeOfResource(HInstance, HResInfo);
  if Size<=4 then
    //Only dummy data present, return nil
    Exit;
  Result := TZInputStream.CreateFromMemory(LockResource(HGlobal), Size);
end;



////////////////////////////////
// AUDIO

//From mmsystem.pas
type
  PWaveFormatEx = ^TWaveFormatEx;
  tWAVEFORMATEX = packed record
    wFormatTag: Word;         { format type }
    nChannels: Word;          { number of channels (i.e. mono, stereo, etc.) }
    nSamplesPerSec: DWORD;  { sample rate }
    nAvgBytesPerSec: DWORD; { for buffer estimation }
    nBlockAlign: Word;      { block size of data }
    wBitsPerSample: Word;   { number of bits per sample of mono data }
    cbSize: Word;           { the count in bytes of the size of }
  end;
  MMRESULT = UINT;

const
  WAVE_FORMAT_PCM     = 1;
  mmsyst = 'winmm.dll';

function timeBeginPeriod(uPeriod: UINT): MMRESULT; stdcall; external mmsyst;
function timeEndPeriod(uPeriod: UINT): MMRESULT; stdcall; external mmsyst;

function PlaybackThread_Execute(Parameter: Pointer): Integer; stdcall; forward;

type
  //DirectSound
  //C-style references

  PDSBufferDesc = ^TDSBufferDesc;
  TDSBufferDesc = packed record
    dwSize: DWORD;
    dwFlags: DWORD;
    dwBufferBytes: DWORD;
    dwReserved: DWORD;
    lpwfxFormat: PWaveFormatEx;
    guid3DAlgorithm: TGUID;
  end;

  PDSCcaps = ^TDSCcaps;
  TDSCcaps = packed record
    dwSize: DWORD;
    dwFlags: DWORD;
    dwFormats: DWORD;
    dwChannels: DWORD;
  end;

  PDSCaps = ^TDSCaps;
  TDSCaps = packed record
    dwSize                         : DWORD;
    dwFlags                        : DWORD;
    dwMinSecondarySampleRate       : DWORD;
    dwMaxSecondarySampleRate       : DWORD;
    dwPrimaryBuffers               : DWORD;
    dwMaxHwMixingAllBuffers        : DWORD;
    dwMaxHwMixingStaticBuffers     : DWORD;
    dwMaxHwMixingStreamingBuffers  : DWORD;
    dwFreeHwMixingAllBuffers       : DWORD;
    dwFreeHwMixingStaticBuffers    : DWORD;
    dwFreeHwMixingStreamingBuffers : DWORD;
    dwMaxHw3DAllBuffers            : DWORD;
    dwMaxHw3DStaticBuffers         : DWORD;
    dwMaxHw3DStreamingBuffers      : DWORD;
    dwFreeHw3DAllBuffers           : DWORD;
    dwFreeHw3DStaticBuffers        : DWORD;
    dwFreeHw3DStreamingBuffers     : DWORD;
    dwTotalHwMemBytes              : DWORD;
    dwFreeHwMemBytes               : DWORD;
    dwMaxContigFreeHwMemBytes      : DWORD;
    dwUnlockTransferRateHwBuffers  : DWORD;
    dwPlayCpuOverheadSwBuffers     : DWORD;
    dwReserved1                    : DWORD;
    dwReserved2                    : DWORD;
  end;

  PDSBcaps = ^TDSBcaps;
  TDSBcaps = packed record
    dwSize                : DWORD;
    dwFlags               : DWORD;
    dwBufferBytes         : DWORD;
    dwUnlockTransferRate  : DWORD;
    dwPlayCpuOverhead     : DWORD;
  end;

  PDirectSound = ^TDirectSound;
  TDirectSound_Vtbl =
  packed record
    //IInterface
    QueryInterface : function (This : PDirectSound; const IID: TGUID; out Obj): HResult; stdcall;
    _AddRef : function(This : PDirectSound) : Integer; stdcall;
    _Release : function(This : PDirectSound) : Integer; stdcall;
    // IDirectSound methods
    CreateSoundBuffer : function(This : PDirectSound;const pcDSBufferDesc: TDSBufferDesc; ppDSBuffer: pointer; pUnkOuter: pointer): HResult; stdcall;
    GetCaps : function (This : PDirectSound; out pDSCaps: TDSCaps): HResult; stdcall;
    DuplicateSoundBuffer : function (This : PDirectSound; pDSBufferOriginal: pointer; out ppDSBufferDuplicate: pointer): HResult; stdcall;
    SetCooperativeLevel : function (This : PDirectSound; hwnd: HWND; dwLevel: DWORD): HResult; stdcall;
    Compact : function(This : PDirectSound) : HResult; stdcall;
    GetSpeakerConfig : function (This : PDirectSound; out pdwSpeakerConfig: DWORD): HResult; stdcall;
    SetSpeakerConfig : function (This : PDirectSound; dwSpeakerConfig: DWORD): HResult; stdcall;
    Initialize : function (This : PDirectSound; pcGuidDevice: PGUID): HResult; stdcall;
  end;
  TDirectSound =
  packed record
    Vtbl : ^TDirectSound_Vtbl;
  end;

  PDirectSoundBuffer = ^TDirectSoundBuffer;
  TDirectSoundBuffer_Vtbl =
  packed record
    //IInterface
    QueryInterface : function (This : PDirectSoundBuffer; const IID: TGUID; out Obj): HResult; stdcall;
    _AddRef : function(This : PDirectSoundBuffer) : Integer; stdcall;
    _Release : function(This : PDirectSoundBuffer) : Integer; stdcall;
    // IDirectSoundBuffer methods
    GetCaps : function (This : PDirectSoundBuffer; var pDSBufferCaps: TDSBcaps): HResult; stdcall;
    GetCurrentPosition : function (This : PDirectSoundBuffer; pdwCurrentPlayCursor, pdwCurrentWriteCursor: PDWORD): HResult; stdcall;
    GetFormat : function (This : PDirectSoundBuffer; pwfxFormat: PWaveFormatEx; dwSizeAllocated: DWORD; pdwSizeWritten: PDWORD): HResult; stdcall;
    GetVolume : function (This : PDirectSoundBuffer; out plVolume: Longint): HResult; stdcall;
    GetPan : function (This : PDirectSoundBuffer; out plPan: Longint): HResult; stdcall;
    GetFrequency : function (This : PDirectSoundBuffer; out pdwFrequency: DWORD): HResult; stdcall;
    GetStatus : function (This : PDirectSoundBuffer; out pdwStatus: DWORD): HResult; stdcall;
    Initialize : function (This : PDirectSoundBuffer; pDirectSound: PDirectSound; const pcDSBufferDesc: TDSBufferDesc): HResult; stdcall;
    Lock : function (This : PDirectSoundBuffer; dwOffset, dwBytes: DWORD; ppvAudioPtr1: PPointer; pdwAudioBytes1: PDWORD;
      ppvAudioPtr2: PPointer; pdwAudioBytes2: PDWORD; dwFlags: DWORD): HResult; stdcall;
    Play : function (This : PDirectSoundBuffer; dwReserved1, dwPriority, dwFlags: DWORD): HResult; stdcall;
    SetCurrentPosition : function (This : PDirectSoundBuffer; dwNewPosition: DWORD): HResult; stdcall;
    SetFormat : function (This : PDirectSoundBuffer; pcfxFormat: PWaveFormatEx): HResult; stdcall;
    SetVolume : function (This : PDirectSoundBuffer; lVolume: Longint): HResult; stdcall;
    SetPan : function (This : PDirectSoundBuffer; lPan: Longint): HResult; stdcall;
    SetFrequency : function (This : PDirectSoundBuffer; dwFrequency: DWORD): HResult; stdcall;
    Stop : function(This : PDirectSoundBuffer) : HResult; stdcall;
    Unlock : function (This : PDirectSoundBuffer; pvAudioPtr1: Pointer; dwAudioBytes1: DWORD; pvAudioPtr2: Pointer; dwAudioBytes2: DWORD): HResult; stdcall;
    Restore : function(This : PDirectSoundBuffer) : HResult; stdcall;
  end;
  TDirectSoundBuffer =
  packed record
    Vtbl : ^TDirectSoundBuffer_Vtbl;
  end;

const
  DSSCL_NORMAL                = $00000001;
  DSSCL_PRIORITY              = $00000002;
  DSSCL_EXCLUSIVE             = $00000003;
  DSSCL_WRITEPRIMARY          = $00000004;

  DSBCAPS_PRIMARYBUFFER       = $00000001;
  DSBCAPS_STATIC              = $00000002;
  DSBCAPS_LOCHARDWARE         = $00000004;
  DSBCAPS_LOCSOFTWARE         = $00000008;
  DSBCAPS_CTRL3D              = $00000010;
  DSBCAPS_CTRLFREQUENCY       = $00000020;
  DSBCAPS_CTRLPAN             = $00000040;
  DSBCAPS_CTRLVOLUME          = $00000080;
  DSBCAPS_CTRLPOSITIONNOTIFY  = $00000100;
  DSBCAPS_CTRLFX              = $00000200;
  DSBCAPS_STICKYFOCUS         = $00004000;
  DSBCAPS_GLOBALFOCUS         = $00008000;
  DSBCAPS_GETCURRENTPOSITION2 = $00010000;
  DSBCAPS_MUTE3DATMAXDISTANCE = $00020000;
  DSBCAPS_LOCDEFER            = $00040000;

  DSBPLAY_LOOPING             = $00000001;
  DSBPLAY_LOCHARDWARE         = $00000002;
  DSBPLAY_LOCSOFTWARE         = $00000004;
  DSBPLAY_TERMINATEBY_TIME    = $00000008;
  DSBPLAY_TERMINATEBY_DISTANCE    = $000000010;
  DSBPLAY_TERMINATEBY_PRIORITY    = $000000020;

var
  DirectSoundLib: THandle;
  DirectSoundCreate: function(lpGuid: PGUID; ppDS: pointer; pUnkOuter: pointer): HResult; stdcall;
  DS : PDirectSound;
  DSBuffer : PDirectSoundBuffer;
  DSPrimary : PDirectSoundBuffer;
  PlaybackThreadHandle : THandle;
  PlaybackThreadId : DWORD;
  PlaybackThread_Terminated : boolean;
  LastWritePosition : integer;


procedure Platform_InitAudio;
var
  Buf : TDSBUFFERDESC;
  Fmt : tWAVEFORMATEX;
  Err : HRESULT;
begin
  DirectSoundLib:= LoadLibrary('dsound.dll');
  if DirectSoundLib=0 then
    ZHalt('could not load dsound.dll');
  DirectSoundCreate:= GetProcAddress(DirectSoundLib, 'DirectSoundCreate');

  if DirectSoundCreate(nil, @DS ,nil)<>0 then
    ZHalt('dsound failed');
  Err:=DS.Vtbl.SetCooperativeLevel(DS,H_Wnd,DSSCL_PRIORITY);
  if Err<>0 then
    ZHalt('dsound.setcooplevel failed');

  Fmt.wFormatTag := wave_Format_PCM;         // it's PCM data
  Fmt.nChannels := AudioPlayer.StereoChannels;  // mono/stereo
  Fmt.nSamplesPerSec := AudioPlayer.AudioRate;  // set the 44.1KHz rate
  Fmt.wBitsPerSample := AudioPlayer.OutputBits;              // 16-bit audio
  Fmt.nBlockAlign := Fmt.nChannels * SizeOf(TSoundOutputUnit);
  Fmt.nAvgBytesPerSec := Fmt.nSamplesPerSec * Fmt.nBlockAlign;
  Fmt.cbSize := 0;

  //Create primary buffer
  //Detta behövs för att försäkra sig om att den primära mix-buffern har samma format
  //som den sekundära.
  //**quake3 gör inte detta, men quake2?
  FillChar(Buf,SizeOf(Buf),0);
  Buf.dwSize := SizeOf(Buf);
  Buf.dwFlags := DSBCAPS_PRIMARYBUFFER;
  Err:=DS.Vtbl.CreateSoundBuffer(DS,Buf,@DSPrimary,nil);
  if Err<>0 then
    ZHalt('dsound.createSoundBuffer failed');
  Err:=DSPrimary.Vtbl.SetFormat(DSPrimary,@Fmt);
  if Err<>0 then
    ZHalt('setprimary format failed');

  //Create secondary buffer
  FillChar(Buf,SizeOf(Buf),0);
  Buf.dwSize := SizeOf(Buf);
  Buf.dwBufferBytes := SoundBufferByteSize;
  Buf.lpwfxFormat := @Fmt;
  Buf.dwFlags := DSBCAPS_GETCURRENTPOSITION2 or DSBCAPS_GLOBALFOCUS;
  Err:=DS.Vtbl.CreateSoundBuffer(DS,Buf,@DSBuffer,nil);
  if Err<>0 then
    ZHalt('dsound.createSoundBuffer failed');

  //todo: lås och töm hela bufferten

  //Start playback
  DSBuffer.Vtbl.Play(DSBuffer,0,0,DSBPLAY_LOOPING);

  //Öka precisionen för Sleep()-anrop, behövs för thread
  TimeBeginPeriod(1);

  {$if defined(zzdc_activex)}
  PlaybackThread_Terminated := False;  //todo: synchronize
  {$ifend}

  //Create and start thread
  PlaybackThreadHandle := CreateThread(nil, 0, @PlaybackThread_Execute, nil, 0, PlaybackThreadID);
  SetThreadPriority(PlaybackThreadHandle,THREAD_PRIORITY_ABOVE_NORMAL);
end;


procedure Platform_ShutdownAudio;
begin
  if PlaybackThreadHandle<>0 then
  begin
    PlaybackThread_Terminated := True;  //todo: synchronize
    WaitForSingleObject(PlaybackThreadHandle, INFINITE);
    CloseHandle(PlaybackThreadHandle);
  end;

  TimeEndPeriod(1);

  if DSBuffer<>nil then
  begin
    DSBuffer.Vtbl.Stop(DSBuffer);
    DSBuffer.Vtbl._Release(DSBuffer);
  end;

  if DSPrimary<>nil then
    DSPrimary.Vtbl._Release(DSPrimary);

//This code crashes when used with bass.dll
//  if DS<>nil then
//    DS.Vtbl._Release(DS);

  if DirectSoundLib<>0 then
    FreeLibrary(DirectSoundLib);
end;

{$ifndef minimal}
var
  FAudioCallBack : TDesignerAudioCallback;
procedure Platform_DesignerSetAudioCallback(F : TDesignerAudioCallback);
begin
  FAudioCallBack := F;
end;
{$endif}

{ TPlaybackThread }
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

  {$ifndef minimal}
  //Kopiera mixad data tillbaka till gui för att visa display
  if Assigned(FAudioCallBack) then
  begin
    Dec(Dest,SampleCount*StereoChannels);
    FAudioCallBack(Dest,SampleCount);
  end;
  {$endif}
end;


function PlaybackThread_Execute(Parameter: Pointer): Integer; stdcall;
var
  PlayCursorPos,WriteCursorPos : integer;
  LockSize1,LockSize2 : LongWord;
  LockSamples1 : integer;
  LockPtr1,LockPtr2 : pointer;
  BytesToWrite,SamplesToWrite : integer;
begin
  while not PlaybackThread_Terminated do
  begin
    //Sleep a quarter of the buffer size.
    //Ändrat till en åttondel.
    //Om för lång sleep så blir det brus.
    //Obs, behöver anropa TimeBeginPeriod i init för att sleep ska bete sig rätt?
    Sleep( Round(((SoundBufferSamplesSize/AudioRate)/8) * 1000) );

    DSBuffer.Vtbl.GetCurrentPosition(DSBuffer,@PlayCursorPos,@WriteCursorPos);

    if LastWritePosition>PlayCursorPos then
      BytesToWrite := (SoundBufferByteSize-LastWritePosition) + PlayCursorPos
    else
      BytesToWrite := PlayCursorPos - LastWritePosition;

    {$ifndef minimal}
    if BytesToWrite=0 then
    begin
      //Restore is needed if focus switched back from another program (madtracker)
      DSBuffer.Vtbl.Restore(DSBuffer);
      DSBuffer.Vtbl.Play(DSBuffer,0,0,DSBPLAY_LOOPING);
    end;
    {$endif}

    SamplesToWrite := BytesToWrite div (SizeOf(TSoundOutputUnit) * StereoChannels);
    if (SamplesToWrite<AudioRate div 1000) then
      //Loop om för få samples
      Continue;
    if SamplesToWrite>MixBufferSamplesSize then
      //Safety mot för många samples
      SamplesToWrite := MixBufferSamplesSize;
    FillChar(MixBuffer,SamplesToWrite * SizeOf(TSoundMixUnit) * StereoChannels,0);

    Platform_EnterMutex(VoicesMutex);
      //Skriv SamplesToWrite samples från LastWritePosition
      //dela upp i två anrop ifall buffer-wrap
      AudioPlayer.RenderToMixBuffer(@MixBuffer[0],SamplesToWrite);
    Platform_LeaveMutex(VoicesMutex);

    DSBuffer.Vtbl.Lock(DSBuffer,LastWritePosition,BytesToWrite,@LockPtr1,@LockSize1, @LockPtr2,@LockSize2,0);

    //Flytta paint buffer till dsoundbuffer
    //Sker i två anrop ifall buffer har wrappats
    //Locksize är i bytes, räkna om till samples
    LockSamples1 := LockSize1 div (SizeOf(TSoundOutputUnit)*StereoChannels);
    ClipAndCopy(@MixBuffer[0],LockPtr1,LockSamples1);
    if LockPtr2<>nil then
    begin
      ClipAndCopy(@MixBuffer[LockSamples1 * StereoChannels],
        LockPtr2,
        LockSize2 div (SizeOf(TSoundOutputUnit)*StereoChannels) );
    end;

    DSBuffer.Vtbl.Unlock(DSBuffer,LockPtr1,BytesToWrite,LockPtr2,0);

    LastWritePosition := (LastWritePosition + BytesToWrite) mod SoundBufferByteSize;
  end;
  Result := 0;
end;


function Platform_CreateMutex : pointer;
begin
  GetMem(Result,SizeOf(TRTLCriticalSection));
  InitializeCriticalSection( PRTLCriticalSection(Result)^ );
end;

procedure Platform_FreeMutex(P : pointer);
begin
  DeleteCriticalSection( PRTLCriticalSection(P)^ );
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

{$ifdef zzdc_screensaver}
procedure InitScreenSaver;
var
  I,E : integer;
  S : string;
begin
//  MessageBox(0, 'pausing so I can debug', GetCommandLine, MB_OK);
  if Platform_CommandLine('p') then
  begin
    I := 1;
    repeat
      S := ParamStr(I);
      if (Length(S)>0) and (ansichar(S[1]) in ['0'..'9']) then
      begin
        Val(S, PreviewParentHWnd, E);
        Break;
      end;
      Inc(I);
    until Length(S)=0;
  end;

  if Platform_CommandLine('c') or Platform_CommandLine('a') then
    Halt; //no configuration or password support

 { //Not sure if this is neccessary, seems to work better without forcing single instance
  if (CreateMutex(nil, False, 'ZzdcScreensaver') = 0) or
    //Must be single instance
    (GetLastError = ERROR_ALREADY_EXISTS) then
    Halt(1);   }
end;
{$endif}

function Platform_GLLoadProc(const P : PAnsiChar) : pointer;
begin
  Result := wglGetProcAddress(P);
end;

procedure Platform_InitGlobals;
begin
  {$ifdef zzdc_screensaver}
  InitScreenSaver;
  {$endif}

  //Init timer
  QueryPerformanceFrequency(TimerFreq);
  QueryPerformanceCounter(TimerStart);

  InitKeyMap;

  InitJoysticks;
end;


/////////////////////////////
//Option dialog code

{$ifdef fpc}
  {$R OptionsDialog.res}
{$else}
  {$R OptionsDialog.res OptionsDialog.rc}
{$endif}

const
  // OptionDialog ID numbers
  ID_COMBOBOX = 100;  // ID number of the sample combobox.
  ID_CHECKBOX = 101;  // ID number of the checkbox.


function Options(Dialog: HWnd; Message, WParam: Longint; LParam: Longint): Bool; stdcall;
const
  xstr = 'x'#0;
  DesktopStr = 'Use Desktop resolution'#0;
  App : TZApplication = nil;
var
  I : integer;
  Tmp1 : array[0..10] of AnsiChar;
  Str : array[0..100] of AnsiChar;
  R : TRect;
begin
//  Result := True;
  case Message of
    wm_InitDialog:
    begin
      App := TZApplication(LParam);
      for I := 0 to High(ScreenModes) do
      begin
        if I=0 then
          ZStrCopy(Str,DesktopStr)
        else
        begin
          ZStrConvertInt(ScreenModes[I].W, PAnsiChar(@Str));
          ZStrCat(Str,xstr);
          ZStrConvertInt(ScreenModes[I].H, PAnsiChar(@Tmp1));
          ZStrCat(Str,Tmp1);
        end;
        SendDlgItemMessage(Dialog, ID_COMBOBOX, CB_ADDSTRING, 0, Integer(@Str));
      end;
      SendDlgItemMessage(Dialog,ID_COMBOBOX,CB_SETCURSEL,byte(App.ScreenMode),0);

      //Center on screen
      GetWindowRect(GetDesktopWindow, R);
      SetWindowPos(Dialog,
                 HWND_TOP,
                 (R.right div 2) - 104,
                 (R.bottom div 2) - 60,
                 0, 0,          // Ignores size arguments.
                 SWP_NOSIZE);

    end;

    wm_Command:
    begin
      case LoWord(WParam) of
        ID_OK,ID_CANCEL:
        begin
          EndDialog(Dialog, LoWord(WParam));
          //Exit;
        end;

        ID_CHECKBOX:
        begin
          App.Fullscreen := not App.Fullscreen;
          SendMessage(LParam, BM_SETCHECK, Ord(App.Fullscreen), 0);
        end;

        ID_COMBOBOX:
        case HiWord(WParam) of
          CBN_SELCHANGE:
            byte(App.ScreenMode) := SendMessage(LParam,CB_GETCURSEL,0,0);
        end;

      end;
    end;
  end;
  Result := False;
end;

function Platform_ShowOptionDialog(App : pointer) : boolean;
begin
  Result := DialogBoxParamA(hInstance, PAnsiChar('IDDLG_OPTIONSDIALOG'), 0,@Options,NativeInt(App))=ID_OK;
end;

function Platform_GetDisplayRefreshRate : integer;
var
  dmScreenSettings : DEVMODE;
begin
  FillChar(dmScreenSettings, SizeOf(dmScreenSettings), 0);
  dmScreenSettings.dmSize       := SizeOf(dmScreenSettings);
  EnumDisplaySettings(nil,DWORD(-1),dmScreenSettings);
  Result := dmScreenSettings.dmDisplayFrequency;
end;

procedure Platform_Sleep(time : integer);
begin
  Sleep(time);
end;


//-------------------------------------------
//JOYSTICK support
//Inspired and borrowed from SDL_mmjoystick.c

const
  MAX_JOYSTICKS = 16;
  MAX_AXES = 6;

type
  //From mmsystem.pas and slightly modified
  TZ_JOYCAPS = record
    wMid: Word;
    wPid: Word;
    szPname: array[0..32-1] of byte;
    wXmin: UINT;
    wXmax: UINT;
    wYmin: UINT;
    wYmax: UINT;
    wZmin: UINT;
    wZmax: UINT;
    wNumButtons: UINT;
    wPeriodMin: UINT;
    wPeriodMax: UINT;
    wRmin: UINT;
    wRmax: UINT;
    wUmin: UINT;
    wUmax: UINT;
    wVmin: UINT;
    wVmax: UINT;
    wCaps: UINT;
    wMaxAxes: UINT;
    wNumAxes: integer;
    wMaxButtons: UINT;
    szRegKey: array[0..32 - 1] of byte;
    szOEMVxD: array[0..260 - 1] of byte;
  end;

  TZ_JOYINFOEX = record
    dwSize: DWORD;
    dwFlags: DWORD;
    wXpos: UINT;
    wYpos: UINT;
    wZpos: UINT;
    dwRpos: DWORD;
    dwUpos: DWORD;
    dwVpos: DWORD;
    dwButtons: DWORD;
    dwButtonNumber: DWORD;
    wPOV: word;
    filler1 : word;
    dwReserved1: DWORD;
    dwReserved2: DWORD;
  end;

  PJoystick = ^TJoystick;
  TJoystick =
    record
      Caps : TZ_JOYCAPS;
      TransAxis : array[0..MAX_AXES-1] of
        record
          Offset : single;
          Scale : single;
        end;
      Values : array[0..MAX_AXES-1] of single;
      Buttons : DWORD;
      POV : single;
    end;

var
  Joysticks : array[0..MAX_JOYSTICKS-1] of TJoystick;
  JoyCount : integer;

function joyGetNumDevs: UINT; stdcall; external mmsyst;
function joyGetPosEx(uJoyID: UINT; lpInfo: pointer): MMRESULT; stdcall; external mmsyst;
function joyGetDevCapsA(uJoyID: UINT; lpCaps: pointer; uSize: UINT): MMRESULT; stdcall; external mmsyst;



procedure InitJoysticks;
type
  PMinMax = ^TMinMax;
  TMinMax = packed record min,max : LongWord; end;
var
  I,J,MaxDevs : integer;
  MinMax : PMinMax;
  Joy : PJoystick;
begin
  MaxDevs := joyGetNumDevs();
  if MaxDevs>MAX_JOYSTICKS then
    MaxDevs := MAX_JOYSTICKS;

  for I := 0 to MaxDevs - 1 do
  begin
    Joy := @Joysticks[I];
    if joyGetDevCapsA(I,@Joy.Caps,SizeOf(TZ_JOYCAPS))<>0 then
      Break;

    MinMax := @Joy.Caps.wXmin;
    for J := 0 to MAX_AXES-1 do
    begin
      if (MinMax.Min<>0) or (MinMax.Max<>0) then
      begin
        Joy.TransAxis[J].Offset := MinMax.Min;
        Joy.TransAxis[J].Scale := (2.0) / (MinMax.Max-MinMax.Min);
      end;
      if J=2 then
        MinMax := @Joy.Caps.wRmin
      else
        Inc(MinMax);
    end;
  end;
  JoyCount := I;
  {$ifndef minimal}
  ZLog.GetLog('Platform').Write('Gamepads detected: ' + IntToStr(JoyCount));
  {$endif}
end;

procedure UpdateJoystick(JoyId : integer; var Joy : TJoystick);
const
  Mask : LongWord = $FFFFFFF0;
var
  Info : TZ_JOYINFOEX;
  Flag : LongWord;
  I : integer;
  Pos : ^LongWord;
begin
  Info.dwSize := SizeOf(Info);
  Info.dwFlags := $BF or $200;

  if joyGetPosEx(JoyId, @Info)<>0 then
    Exit;

  Pos := @Info.wXpos;
  Flag := 1;
  for I := 0 to MAX_AXES - 1 do
  begin
    if (Info.dwFlags and Flag)>0 then
      Joy.Values[I] := (((Pos^ and Mask) - Joy.TransAxis[I].Offset) * Joy.TransAxis[I].Scale)-1.0;
    Flag := Flag shl 1;
    Inc(Pos);
  end;

  Joy.Buttons := Info.dwButtons;

  if Info.wPOV=$FFFF then
    Joy.POV := -1
  else
    Joy.POV := Info.wPOV*0.01;
end;

procedure UpdateJoysticks;
var
  I : integer;
begin
  for I := 0 to JoyCount - 1 do
    UpdateJoystick(I,Joysticks[I]);
end;


function Platform_GetJoystickAxis(JoyId : integer; Axis : integer) : single;
var
  Joy : PJoystick;
begin
  Result := 0;
  if JoyId<JoyCount then
  begin
    Joy := @Joysticks[JoyId];
    if Axis<Joy.Caps.wNumAxes then
      Result := Joysticks[JoyId].Values[Axis];
  end;
end;

function Platform_GetJoystickButton(JoyId : integer; Button : integer) : boolean;
begin
  Result := False;
  if JoyId<JoyCount then
    Result := (Joysticks[JoyId].Buttons and (1 shl Button))>0;
end;

function Platform_GetJoystickPOV(JoyId : integer) : single;
begin
  Result := -1;
  if JoyId<JoyCount then
    Result := (Joysticks[JoyId]).POV;
end;

/////////////
//Net-support

//From WinInet.pas
type
  HINTERNET = Pointer;
  PInternetAsyncResult = ^TInternetAsyncResult;
  TInternetAsyncResult = packed record
    dwResult: DWORD; { the HINTERNET, DWORD or BOOL return code from an async API }
    dwError: DWORD; { dwError - the error code if the API failed }
  end;

const
  INTERNET_STATUS_HANDLE_CREATED              = 60;
  INTERNET_STATUS_REQUEST_COMPLETE            = 100;
  INTERNET_STATUS_RESPONSE_RECEIVED           = 41;
  INTERNET_OPEN_TYPE_PRECONFIG        = 0;  { use registry configuration }
  INTERNET_FLAG_ASYNC = $10000000;                  { this request is asynchronous (where supported) }
  winetdll = 'wininet.dll';

function InternetOpenA(lpszAgent: PAnsiChar; dwAccessType: DWORD;
  lpszProxy, lpszProxyBypass: PAnsiChar; dwFlags: DWORD): HINTERNET; stdcall; external winetdll;
function InternetSetStatusCallback(hInet: HINTERNET;
  lpfnInternetCallback: pointer): pointer; stdcall; external winetdll;
function InternetOpenUrlA(hInet: HINTERNET; lpszUrl: PAnsiChar;
  lpszHeaders: PAnsiChar; dwHeadersLength: DWORD; dwFlags: DWORD;
  dwContext: DWORD): HINTERNET; stdcall; external winetdll;
function InternetQueryDataAvailable(hFile: HINTERNET; var lpdwNumberOfBytesAvailable: DWORD;
  dwFlags, dwContext: DWORD): BOOL; stdcall; external winetdll;
function InternetReadFile(hFile: HINTERNET; lpBuffer: Pointer;
  dwNumberOfBytesToRead: DWORD; var lpdwNumberOfBytesRead: DWORD): BOOL; stdcall; external winetdll;
function InternetCloseHandle(hInet: HINTERNET): BOOL; stdcall; external winetdll;

var
  SessionHandle : HINTERNET;
  SessionWait : boolean;


procedure NetCallback(hInternet : HINTERNET;
    dwContext : DWORD_PTR;
    dwInternetStatus :DWORD;
    lpvStatusInformation : pointer;
    dwStatusInformationLength : DWORD
    ); stdcall;
begin
//  OutputDebugString( PChar(IntToStr(dwInternetStatus)) );
  case dwInternetStatus of
    INTERNET_STATUS_HANDLE_CREATED :
      begin
        TWebOpen(dwContext).Handle := pointer(PInternetAsyncResult(lpvStatusInformation).dwResult);
      end;
    INTERNET_STATUS_REQUEST_COMPLETE :
      begin
        if SessionWait then
          SessionWait := False
        else
          TWebOpen(dwContext).StartReading;
      end;
    INTERNET_STATUS_RESPONSE_RECEIVED :
        if SessionWait then
          SessionWait := False
  end;

end;

function ShellExecuteA(hWnd: HWND; Operation, FileName, Parameters,
  Directory: PAnsiChar; ShowCmd: Integer): HINST; stdcall;  external 'shell32.dll';

procedure Platform_NetOpen(Url : PAnsiChar; InBrowser : boolean; WebOpen : pointer);
begin
  if InBrowser then
  begin
    //Workaround for mouse/key-up messages being lost
    FillChar(Keys,SizeOf(Keys),0);
    ShellExecuteA(0, PAnsiChar('open'),Url, nil, nil, SW_SHOWDEFAULT);
  end
  else
  begin
    if SessionHandle=nil then
    begin
      SessionHandle := InternetOpenA(PAnsiChar('ZGE'),
        INTERNET_OPEN_TYPE_PRECONFIG,
        nil,nil,INTERNET_FLAG_ASYNC);
      InternetSetStatusCallback(SessionHandle,@NetCallBack);
    end;
    InternetOpenUrlA(SessionHandle,Url,nil,0,0,Cardinal(WebOpen));
  end;
  {$ifndef minimal}
  ZLog.GetLog('Platform').Write('Net open: ' + Url);
  {$endif}
end;

function Platform_NetRead(Handle,Buffer : pointer; Size : integer) : integer;
var
  BytesRead,Avail,ThisSize : dword;
begin
  Result := 0;
  while True and (Size>0) do
  begin
    SessionWait := True;
    ThisSize := Size;
    if InternetQueryDataAvailable(Handle,Avail,0,0) then
    begin
      if Avail=0 then
        Break;
      if ThisSize>Avail then
        ThisSize:=Avail;
      //Without this sleep InternetReadFile will hang on status 40
      Sleep(100);
    end;

    if not InternetReadFile(Handle,Buffer,ThisSize,BytesRead) then
    begin
      if (GetLastError()<>ERROR_IO_PENDING) then
        Break;
      //BytesRead isn't updated until INTERNET_STATUS_REQUEST_COMPLETE
      while SessionWait do
        Sleep(200);
    end;

    if (BytesRead=0) then
      Break;

    Inc(pbyte(Buffer),BytesRead);
    Inc(Result,BytesRead);
    Dec(Size,BytesRead);
  end;
  InternetCloseHandle(Handle);
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
  S := AnsiString(ChangeFileExt(String(DesignerFilePath) + String(Name),'.dll'));
  if FileExists(string(S)) then
    Result := LoadLibraryA(PAnsiChar(S))
  else
    //Not found in zgeproj-path, assume system32-dll such as user32 and load without path
    Result := LoadLibraryA(Name)
  {$else}
  //In engine always let the OS find the dll without path
  Result := LoadLibraryA(Name);
  {$endif}
end;

function Platform_GetModuleProc(Module : NativeUInt; const Name : PAnsiChar) : pointer;
begin
  Result := GetProcAddress(Module,Name);
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

function ThreadWrapper(Parameter: Pointer): Integer; stdcall;
begin
  TZThread(Parameter).Execute;
  Result := 0;
end;

function Platform_CreateThread(ZThread : TZThread) : pointer;
var
  ThreadId : DWord;
begin
  Result := pointer(CreateThread(nil,
    0,
    @ThreadWrapper,
    ZThread, //param
    0,
    ThreadId));
end;

procedure Platform_FreeThread(T : pointer);
begin
  CloseHandle(NativeUInt(T));
end;

function Platform_CreateEvent : pointer;
begin
  Result := Pointer(CreateEvent(nil, False, False, nil));
end;

procedure Platform_SignalEvent(E : pointer);
begin
  SetEvent(NativeUInt(E));
end;

procedure Platform_WaitEvent(E : pointer);
begin
  WaitForSingleObject(NativeUInt(E),INFINITE);
end;

procedure Platform_FreeEvent(E : pointer);
begin
  CloseHandle(NativeUInt(E));
end;

function Platform_GetCpuCount : integer;
var
  sysinfo : SYSTEM_INFO;
begin
  GetSystemInfo( sysinfo );
  Result := sysinfo.dwNumberOfProcessors;
end;

{initialization

finalization}

end.