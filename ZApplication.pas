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

unit ZApplication;

{$include zzdc_globalopt.inc}

interface

uses ZClasses,Meshes,Collision,Commands,AudioComponents, GLDrivers
  {$ifndef minimal},Generics.Collections,uSymTab,Contnrs,Renderer{$endif}
;

resourcestring
  strOnLoaded = 'OnLoaded';
  strStates = 'States';
  strOnUpdate = 'OnUpdate';
  strOnBeginRenderPass = 'OnBeginRenderPass';
  strOnRender = 'OnRender';
  strOnClose = 'OnClose';
  strLights = 'Lights';
  strContent = 'Content';
  strCaption = 'Caption';
  strDeltaTime = 'DeltaTime';
  strDeltaTimeHint = 'Read only. The time in seconds that has passed since last frame was rendered.';
  strTime = 'Time';
  strTimeHint = 'Read only. Time in seconds since application was started.';
  strGLBase = 'GLBase';
  strThreadsEnabled = 'ThreadsEnabled';
  strFpsCounter = 'FpsCounter';
  strFpsCounterHint = 'Read only. The current number of frames per second.';
  strCurrentRenderPass = 'CurrentRenderPass';
  strMousePosition = 'MousePosition';
  strMouseWheelDelta = 'MouseWheelDelta';
  strClearScreenMode = 'ClearScreenMode';
  strRenderPasses = 'RenderPasses';
  strWindowHandle = 'WindowHandle';
  strClearColor = 'ClearColor';
  strAmbientLightColor = 'AmbientLightColor';
  strFullScreen = 'FullScreen';
  strFrameRateStyle = 'FrameRateStyle';
  strFixedFrameRate = 'FixedFrameRate';
  strScreenMode = 'ScreenMode';
  strShowOptionsDialog = 'ShowOptionsDialog';
  strCustomScreenWidth = 'CustomScreenWidth';
  strCustomScreenHeight = 'CustomScreenHeight';
  strCameraPosition = 'CameraPosition';
  strCameraRotation = 'CameraRotation';
  strCamera = 'Camera';
  strLightPosition = 'LightPosition';
  strViewportRatio = 'ViewportRatio';
  strCustomViewportRatio = 'CustomViewportRatio';
  strFOV = 'FOV';
  strClipNear = 'ClipNear';
  strClipFar = 'ClipFar';
  strMouseVisible = 'MouseVisible';
  strEscapeToQuit = 'EscapeToQuit';
  strUseStencilBuffer = 'UseStencilBuffer';
  strRenderOrder = 'RenderOrder';
  strViewportX = 'ViewportX';
  strViewportY = 'ViewportY';
  strViewportWidth = 'ViewportWidth';
  strViewportHeight = 'ViewportHeight';
  strScreenHeight = 'ScreenHeight';
  strScreenWidth = 'ScreenWidth';
  strConstantPool = 'ConstantPool';
  strGlobalVariables = 'GlobalVariables';
  strUserClasses = 'UserClasses';
  strNoSound = 'NoSound';
  strPointerSize = 'PointerSize';
  strFileVersion = 'FileVersion';
  strIcon = 'Icon';
  strPreviewClearColor = 'PreviewClearColor';
  strAndroidPackageName = 'AndroidPackageName';
  strAndroidVersionName = 'AndroidVersionName';
  strAndroidVersionNumber = 'AndroidVersionNumber';
  strAndroidPortrait = 'AndroidPortrait';
  strAndroidSdk = 'AndroidSdk';
  strModelUpdatesEnabled = 'ModelUpdatesEnabled';
  strCollisionsEnabled = 'CollisionsEnabled';
  strState = 'State';
  strKind = 'Kind';
  strPosition = 'Position';
  strRotation = 'Rotation';
  strOrthoZoom = 'OrthoZoom';

type
  //Application states
  TAppState = class(TStateBase)
  private
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    ModelUpdatesEnabled : boolean;
    CollisionsEnabled : boolean;
  end;

  TZApplication = class;

  {$ifdef zgeviz}
  TAppCallback = procedure(App : TZApplication) of object;
  {$endif}

  TCamera = class(TZComponent)
  private
    procedure ApplyTransform(const Aspect : single);
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Kind : (catPerspective,catOrthograpic);
    Position : TZVector3f;
    Rotation : TZVector3f;
    ClipNear,ClipFar,OrthoZoom,FOV : single;
  end;

  TAspectRatio = (vprFullWindow,vprCustom,vpr4_3,vpr16_9);
  TAppScreenMode = (vmFullScreenDesktop,vm640x480,vm800x600,vm1024x768,vm1280x800,vm1280x1024);
  TZApplication = class(TZComponent)
  strict private
    DepthList : TZArrayList;
    ConstantPool : TZComponentList;
    {$ifndef minimal}
    ConstantMap : TDictionary<AnsiString,TObject>;
    {$endif}
    FpsFrames : integer;
    FpsCounter,FpsTime : single;
    HasShutdown : boolean;
    TargetFrameRate : integer;
    NextFrameTime : single;
    LastTime : single;
    procedure RenderModels;
    procedure MainSlice;
    procedure Init;
    procedure Shutdown;
  {$ifdef zgeviz}
    procedure ViewportChanged;
  public
  {$endif}
    procedure UpdateScreen;
  private
    CurrentState : TAppState;
    procedure ApplyCameraTransform;
    {$ifdef minimal}
    procedure CreateWindow;
    {$endif}
    {$ifndef minimal}public{$endif}
    procedure UpdateTime;
    {$if (not defined(minimal)) or defined(android)}public{$endif}
    function Main : boolean;
  protected
    procedure DefineProperties(List: TZPropertyList); override;
    {$ifndef minimal}
    procedure InitAfterPropsAreSet; override;
    {$endif}
  public
    Content : TZComponentList;
    Models : TModels;  //not a property
    Driver : TGLDriverBase;
    Collisions : TCollisionChecks;
    States : TZComponentList;
    OnLoaded : TZComponentList;
    OnClose : TZComponentList;
    OnUpdate : TZComponentList;
    OnRender : TZComponentList;
    OnBeginRenderPass : TZComponentList;
    Lights : TZComponentList;
    GlobalVariables : TZComponentList;
    UserClasses : TZComponentList;
    Terminating : boolean;
    Time,DeltaTime : single;
    Caption : TPropString;
    MousePosition : TZVector3f;
    ClearScreenMode : (csmClear,csmNoClear);
    ClearColor : TZColorf;
    AmbientLightColor : TZColorf;
    Fullscreen : boolean;
    ScreenMode : TAppScreenMode;
    ShowOptionsDialog : boolean;
    CustomScreenWidth,CustomScreenHeight : integer;
    CameraPosition : TZVector3f;
    CameraRotation : TZVector3f;
    LightPosition : TZVector4f;
    CustomViewportRatio,FOV,ClipNear,ClipFar : single;
    ViewportRatio : TAspectRatio;
    ActualViewportRatio : single;
    FrameRateStyle : (frsSyncedWithMonitor,frsFree,frsFixed);
    FixedFrameRate : integer;
    MouseVisible : boolean;
    EscapeToQuit : boolean;
    CurrentRenderPass,RenderPasses : integer;
    WindowHandle : integer;
    ViewportX,ViewportY,ViewportWidth,ViewportHeight : integer;
    MouseWheelDelta : integer;
    Camera : TCamera;
    GLBase : TGLBase;
    NoSound : boolean;
    FrameLoss : boolean;  //s�tts till true ifall vi tappar frames
    ScreenWidth : integer;
    ScreenHeight : integer;
    ThreadedProcessingEnabled : boolean;
    UseStencilBuffer : boolean;
    RenderOrder : (aroModelsBeforeApp,aroAppBeforeModels);
    PointerSize : byte;
    {$ifndef minimal}
    LiveThreadCount : integer;
    Icon : TZBinaryPropValue;
    AndroidPackageName,AndroidVersionName : TPropString;
    AndroidVersionNumber : integer;
    AndroidPortrait : byte;
    AndroidSdk : byte;
    PreviewClearColor : TZColorf;
    DesignerIsRunning : boolean;
    FileVersion : integer;
    //Zc-parsing
    SymTab : TSymbolTable;
    ZcGlobalNames : TObjectList;
    FunctionCleanUps : TObjectList;
    OnGetLibraryPath : function : string of object;
    OnShaderCacheUse : function(Shader : TShader) : boolean of object;
    OnShaderCacheAdd : procedure(Shader : TShader) of object;
    OnContentCacheUse : function(OriginalContent : TContent; out NewContent : TContent) : boolean of object;
    OnContentCacheAdd : procedure(OriginalContent,Content : TContent) of object;
    HasScriptCreatedComponents : boolean;
    {$endif}
    {$ifdef zgeviz}
    ZgeVizCameraRotation : TZVector3f;
    ZgeVizTime : single;
    ZgeVizCameraCallback,ZgeVizViewportCallback : TAppCallback;
    MainRenderTarget : TRenderTarget;
    {$endif}
    constructor Create(OwnerList: TZComponentList); override;
    destructor Destroy; override;
    procedure Run;
    procedure AddModel(Model : TModel);
    procedure Update; override;
    procedure UpdateViewport; overload;
    procedure UpdateViewport(const W,H : integer); overload;
    function NormalizeToScreen(P : TZPointi) : TZVector2f;
    procedure CenterMouse;
    procedure ResetGpuResources; override;
    {$ifndef minimal}
    procedure Terminate;
    procedure DesignerStart(const ViewW,ViewH : integer; const InitTime : single = 0);
    procedure DesignerStop;
    procedure DesignerSetUpView;
    function AddToConstantPool(const Value : string) : TObject;
    procedure ClearConstantPool;
    procedure RefreshSymbolTable;
    procedure Compile;
    procedure CompileProperty(C : TZComponent; Expr : TZPropertyValue; const Prop : TZProperty);
    function BindComponent<T : class>(const CompName : string; out O : T) : boolean;
    {$endif}
  end;

  TComponentHelper = class helper for TZComponent
  public
    function ZApp : TZApplication;
    {$ifndef minimal}
    function HasZApp: boolean;
    {$endif}
  end;

  TSetAppState = class(TCommand)
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    State : TAppState;
    procedure Execute; override;
    {$ifndef minimal}
    function GetDisplayName: AnsiString; override;
    {$endif}
  end;


function LoadApplicationComponent : TZApplication;


const
  TicksPerSecond = 1000;
  AppFileVersion = 2;

var
  ScreenModeChanging : boolean;

implementation

uses ZPlatform,ZLog,AudioPlayer,ZMath,ZOpenGL
  {$ifndef minimal}
  ,ZExpressions,SysUtils,Zc_Ops,Classes,Compiler
  {$else}
  ,Renderer
  {$endif}
  ;

{ TZApplication }

constructor TZApplication.Create(OwnerList: TZComponentList);
begin
  inherited;
  Models := TModels.Create;
  Models.App := Self;

  Collisions := TCollisionChecks.Create(Models,Self);
  DepthList := TZArrayList.CreateReferenced;

  {$ifndef minimal}
  ConstantMap := TDictionary<AnsiString,TObject>.Create;
  SymTab := TSymbolTable.Create;
  ZcGlobalNames := TObjectList.Create(True);
  FunctionCleanUps := TObjectList.Create(True);
  {$endif}
end;

destructor TZApplication.Destroy;
begin
  Terminating := True;

  Models.RemoveAll;

  Collisions.Free;
  DepthList.Free;
  if not HasShutdown then
    Shutdown;

  //ev problem med models.free, borde g�ra models.removeall vid terminate f�rst
  //annars kan models referera modeller som redan har gjorts free p�
  Models.Free;
  Driver.Free;

  {$ifndef minimal}
  ConstantMap.Free;
  SymTab.Free;
  ZcGlobalNames.Free;
  FunctionCleanUps.Free;
  {$endif}
  inherited;
end;

procedure TZApplication.Init;
begin
  {$ifndef zgeviz}
  Platform_InitGlobals;  //Nollst�ll timer etc
  {$endif}

//  ZClasses.Tasks.Enabled := Self.ThreadedProcessingEnabled;

  {$ifndef minimal}
    //no init if inside designer tool
    if Self.Driver=nil then
      Self.Driver := GLDrivers.CreateDriver(Self.GLBase);
    UpdateViewport;
  {$else}
    if Platform_CommandLine('f') then
      Self.FullScreen := True;
    if Platform_CommandLine('w') then
      Self.FullScreen := False;

    if ShowOptionsDialog then
    begin
      if not Platform_ShowOptionDialog(Self) then Halt;
    end;

    CreateWindow;

    TargetFrameRate := Platform_GetDisplayRefreshRate;

    if Platform_CommandLine('s') then
      NoSound := True;
    if not NoSound then
      Platform_InitAudio;
  {$endif}
end;

{$ifdef minimal}
procedure TZApplication.CreateWindow;
var
  I : integer;
begin
  {$ifndef android}
  if((CustomScreenWidth > 0) and (CustomScreenHeight > 0)) then
  begin
    ScreenWidth := Self.CustomScreenWidth;
    ScreenHeight := Self.CustomScreenHeight;
  end
  else
  begin
    I := Ord(Self.ScreenMode);
    ScreenWidth := ScreenModes[ I ].W;
    ScreenHeight := ScreenModes[ I ].H;
  end;
  {$endif}

  Self.WindowHandle := Platform_InitScreen(ScreenWidth,ScreenHeight, Self.Fullscreen , PAnsiChar(Self.Caption), Self);
  Self.Driver := GLDrivers.CreateDriver(Self.GLBase);
  Driver.InitGL;

  Platform_ShowMouse(MouseVisible);
  UpdateViewport;
end;
{$endif}

{$ifndef minimal}
procedure TZApplication.InitAfterPropsAreSet;
begin
  Self.RefreshSymbolTable;
  Self.FileVersion := AppFileVersion;
end;
{$endif}

procedure TZApplication.Shutdown;
begin
  {$ifdef minimal}
  if not NoSound then
    Platform_ShutdownAudio;
  {$endif}

  {$if defined(minimal)}
  if (Self.CurrentState<>nil) then
    Self.CurrentState.OnLeave.ExecuteCommands;
  OnClose.ExecuteCommands; //Running OnClose in designer crashes too often
  {$endif}

  {$ifdef minimal}
  Platform_ShutdownScreen;
  {$endif}

  HasShutdown:=True;
end;

procedure TZApplication.Update;
begin
  Content.Update;
  States.Update;
end;

procedure TZApplication.UpdateTime;
const
  //Maximum time step for update and collision=1/10 second
  MaxUpdateStep = 1.0 / 10;
begin
  {$ifdef zgeviz}
  Time := Self.ZgeVizTime;
  {$else}
  Time := Platform_GetTime;
  {$endif}
  DeltaTime := Time - LastTime;

  //Avoid too high steps such as when grabbing win-caption with mouse
  if DeltaTime>MaxUpdateStep then
  begin
    DeltaTime := MaxUpdateStep;
    Self.FrameLoss := True;
  end
  else
    Self.FrameLoss := False;
    //todo: om deltatime �r f�r h�g, �ka LostTime, och justera Time och Accumulated med den

  if DeltaTime<0 then
    //Apparantly deltatime can become negative in multicore processors
    DeltaTime := 0;

  //Also see this discussion
  //http://www.yakyak.org/viewtopic.php?t=48231&highlight=&sid=7a0084f72cfc50768c1ed4ce3fe0ae8c

  LastTime := Time;
end;

function TZApplication.NormalizeToScreen(P : TZPointi) : TZVector2f;
begin
  //Clip coord to current viewport
  Dec(P.X,ViewportX);
  Dec(P.Y,ViewportY);
  if P.X<0 then
    P.X := 0;
  if P.X>=ViewportWidth then
    P.X := ViewportWidth-1;
  if P.Y<0 then
    P.Y := 0;
  if P.Y>=ViewportHeight then
    P.Y := ViewportHeight-1;

  //-1 .. 1, 0 �r center
  Result[0] := (P.X / ViewportWidth) * 2 - 1;
  //Y-axis is reversed compared to our opengl camera
  Result[1] := (((ViewportHeight-P.Y) / ViewportHeight) * 2 - 1);

  {$ifndef minimal}
  //issue http://www.emix8.org/forum/viewtopic.php?t=947
  if Abs(Result[0])<=2/ViewportWidth then
    Result[0] := 0;
  if Abs(Result[1])<=2/ViewportHeight then
    Result[1] := 0;
  {$endif}
end;

procedure TZApplication.CenterMouse;
begin
  Platform_SetMousePos(ZApp.ScreenWidth shr 1,ZApp.ScreenHeight shr 1);
  MousePosition[0] := 0;
  MousePosition[1] := 0;
end;

function TZApplication.Main : boolean;
var
{$ifdef minimal}
  Remaining : integer;
{$endif}
  Now : single;

  {$if not (defined(minimal) or defined(ZgeViz))}
  procedure InDumpDebugInfo;
  var
    I,J : integer;
  begin
    J := 0;
    for I := 0 to Models.Cats.Count - 1 do
      Inc(J,Models.Get(I).Count);
    ZLog.GetLog(Self.ClassName).Write( 'Models: ' + IntToStr(J) + ', managed: ' + ManagedHeap_GetStatus );
  end;
  {$endif}

begin
  Now := Platform_GetTime;

  if (Now>=NextFrameTime) {$ifdef minimal}or (Self.FrameRateStyle=frsSyncedWithMonitor){$endif} then
  begin  //Delay until next frame
    if (FrameRateStyle<>frsFree) then //But not if set to "Free"
    begin
      if FrameRateStyle=frsFixed then
        TargetFrameRate := Self.FixedFrameRate;
      if (TargetFrameRate<1) or (TargetFrameRate>200) then
        TargetFrameRate := 60;
      if NextFrameTime<Now-1 then
        NextFrameTime := Now;
      NextFrameTime := NextFrameTime + (1.0 / TargetFrameRate);
    end;

    UpdateTime;

    {$ifndef zgeviz}
    PZVector2f(@MousePosition)^ := NormalizeToScreen(Platform_GetMousePos);
    {$endif}

    Self.MainSlice;

    //Draw all
    UpdateScreen;

    Inc(FpsFrames);
    if (Time-FpsTime)>1.0 then
    begin
      FpsCounter := FpsFrames / (Time-FpsTime);
      FpsFrames := 0;
      FpsTime := Time;
      {$if not (defined(minimal) or defined(ZgeViz))}
      InDumpDebugInfo;
      {$endif}
    end;

    {$ifndef zgeviz}
    if ManagedHeap_GetAllocCount>0 then
      ManagedHeap_GarbageCollect;
    {$endif}

    //Reset keyboard events
    if Assigned(ZPlatform.KeyDownList) then
      ZPlatform.KeyDownList.Clear;
    if Assigned(ZPlatform.KeyUpList) then
      ZPlatform.KeyUpList.Clear;

    {$ifdef minimal}
    if FrameRateStyle<>frsFree then
    begin //Give remaining time back to the OS to avoid 100% cpu pressure
      Remaining := Trunc((NextFrameTime - Platform_GetTime) * 1000);
      if (Self.FrameRateStyle=frsSyncedWithMonitor) and Assigned(Platform_SyncWithMonitor) then
        Platform_SyncWithMonitor(Remaining)
      else
      begin
        if Remaining>0 then
          Platform_Sleep(Remaining);
      end;
    end;
    {$endif}
  end;

  //Keep calling main until Terminate is called
  Result := not Terminating;
end;

procedure TZApplication.MainSlice;
begin
  if AudioComponents.CurrentMusic<>nil then
    AudioComponents.CurrentMusic.Update;

  //Uppdatera current state
  if CurrentState<>nil then
    CurrentState.Update;

  //Application onupdate
  OnUpdate.ExecuteCommands;
  //Update application renderers: particlesystems, beams etc
  OnRender.Update;

  //Update models
  if (CurrentState=nil) or CurrentState.ModelUpdatesEnabled then
    Models.Update;

  //Test collisions
  if (CurrentState=nil) or CurrentState.CollisionsEnabled then
    Collisions.Update;

  //Emit sounds queued by models and music
  if not NoSound then
    AudioPlayer.EmitSoundsInEmitList;

  //Notify that net-data has been read
  if Commands.NetResultList.Count>0 then
    TWebOpen.FlushResultList;

  {$ifndef minimal}
  if (Self.Driver<>nil) and (Self.Driver.Kind<>Self.GLBase) then
  begin
    FreeAndNil(Self.Driver);
    Self.Driver := GLDrivers.CreateDriver(Self.GLBase);
  end;
  {$endif}

  //Reset wheel delta (it is set from platform)
  MouseWheelDelta := 0;
end;

procedure TZApplication.Run;
begin
  if Self.PointerSize<>SizeOf(Pointer) then
    ZHalt('Data must be saved in same bitness version of ZGameEditor');

  Init;
  //Skip initial tree update for now because it triggers AppState.OnStart etc. which can cause trouble
  //Any component needing init should override InitAfterPropsAreSet instead.
  //Self.Update;
  OnLoaded.ExecuteCommands;
  Platform_Run(Self.Main);
end;

{$ifndef minimal}
procedure TZApplication.Terminate;
begin
  Terminating := True;
  Shutdown;
end;
{$endif}

procedure TZApplication.UpdateViewport;
begin
  UpdateViewport(ScreenWidth,ScreenHeight);
end;

//Update ActualViewportRatio which is the ratio that is used
procedure TZApplication.UpdateViewport(const W,H : integer);
begin
  if Self.ViewportRatio=vprFullWindow then
  begin
    ActualViewportRatio := W/H;
    glViewport(0,0,W,H);
    ViewportWidth := W;
    ViewportHeight := H;
    ViewportX := 0;
    ViewportY := 0;
  end
  else
  begin
    case Self.ViewportRatio of
      vprCustom : ActualViewportRatio := Self.CustomViewportRatio;
      vpr4_3 : ActualViewportRatio := 4/3;
      vpr16_9 : ActualViewportRatio := 16/9;
    {$ifndef minimal}
    else
      ;
    {$endif}
    end;
    if W/H > ActualViewportRatio then
    begin
      ViewportWidth := Round(H*ActualViewportRatio);
      ViewportHeight := H;
      ViewportX := Round((W-H*ActualViewportRatio)*0.5);
      ViewportY := 0;
    end
    else
    begin
      ViewportWidth := W;
      ViewportHeight := Round(W/ActualViewportRatio);
      ViewportX := 0;
      ViewportY := Round((H-W/ActualViewportRatio)*0.5);
    end;
    glViewport(ViewportX, ViewportY, ViewportWidth, ViewportHeight);
  end;
  {$ifdef zgeviz}
  ViewportChanged;
  {$endif}
end;

{$ifdef zgeviz}
procedure TZApplication.ViewportChanged;
begin
  if Assigned(Self.ZgeVizViewportCallback) then
    Self.ZgeVizViewportCallback(Self);
end;
{$endif}

procedure SortModels(List : TZArrayList);
//http://en.wikipedia.org/wiki/Cocktail_sort
//http://www.algorithm-code.com/wiki/Cocktail_sort
var
  First,Last,I,Shift : integer;
begin
  Shift := 0;
  First := 0;
  Last := List.Count-1;
  while First<Last do
  begin
    for I := First+1 to Last do
    begin
      if TModel(List[I]).SortKey<TModel(List[I-1]).SortKey then
      begin
        List.Swap(I,I-1);
        Shift := I;
      end;
    end;
    Last := Shift;
    for I := Last downto First+1 do
    begin
      if TModel(List[I]).SortKey<TModel(List[I-1]).SortKey then
      begin
        List.Swap(I,I-1);
        Shift := I;
      end;
    end;
    First := Shift;
  end;
end;

procedure TZApplication.RenderModels;
var
  I,J : integer;
  Model : TModel;
  List : TZArrayList;
  Matrix,TmpM : TZMatrix4f;
  V : TZVector3f;
  HasMatrix : boolean;
begin
  DepthList.Clear;

  HasMatrix := False;
  for I := 0 to Models.Cats.Count-1 do
  begin
    List := Models.Get(I);
    for J := 0 to List.Count-1 do
    begin
      Model := TModel(List[J]);
      if Model.RenderOrder=roDepthsorted then
      begin
        if not HasMatrix then
        begin
          Driver.GetMatrix(GL_PROJECTION_MATRIX-GL_MODELVIEW_MATRIX, @Matrix);
          Driver.GetMatrix(GL_MODELVIEW_MATRIX-GL_MODELVIEW_MATRIX, @TmpM);
          Matrix := MatrixMultiply(TmpM,Matrix);
          HasMatrix := True;
        end;
        //Get screen Z-position
        VectorTransform(Model.Position,Matrix,V);
        Model.SortKey := V[2];
        DepthList.Add(Model)
      end
      else
        Renderer.RenderModel(Model);
    end;
  end;

  SortModels(DepthList);

  for I := DepthList.Count-1 downto 0 do
  begin
    Model := TModel(DepthList[I]);
    Renderer.RenderModel(Model);
  end;

  Meshes.CurrentModel := nil;
end;

procedure TZApplication.ApplyCameraTransform;
begin
  if Camera<>nil then
    Camera.ApplyTransform(Self.ActualViewportRatio)
  else
  begin
    {$ifdef zgeviz}
    if Assigned(Self.ZgeVizCameraCallback) then
      Self.ZgeVizCameraCallback(Self)
    else
    begin
    {$endif}
      //Setup view and camera
      Driver.MatrixMode(GL_PROJECTION);
      Driver.LoadIdentity;
      Driver.Perspective(Self.FOV, Self.ActualViewportRatio, Self.ClipNear, Self.ClipFar);
      Driver.MatrixMode(GL_MODELVIEW);
      Driver.LoadIdentity;
    {$ifdef zgeviz}
    end;
    {$endif}

    {$ifdef zgeviz}
    Driver.Rotate( (ZgeVizCameraRotation[2]*360) , 0, 0, 1);
    {$endif}

    //Reverse order to make XYZ-rotation
    Driver.Rotate( (CameraRotation[0]*360) , 1, 0, 0);
    Driver.Rotate( (CameraRotation[1]*360) , 0, 1, 0);
    Driver.Rotate( (CameraRotation[2]*360) , 0, 0, 1);
    //M�ste ta negativt p� cameraposition f�r att dess axlar ska bete sig
    //likadant som modell-koordinater (positiv y = upp�t t.ex.)
    Driver.Translate(-CameraPosition[0], -CameraPosition[1], -CameraPosition[2]);
  end;
end;

procedure TZApplication.UpdateScreen;
var
  I,J : integer;
begin

  for I := 0 to Self.RenderPasses-1 do
  begin
    Self.CurrentRenderPass := I;

    if Self.OnBeginRenderPass.Count>0 then
      Self.OnBeginRenderPass.ExecuteCommands;

    {$ifndef zgeviz}
    if {$if defined(minimal) and (not defined(android))}(ViewportRatio=vprCustom) and {$endif}
      (CurrentRenderTarget=nil) then
      UpdateViewport;
    {$endif}

    //Use custom camera or default
    ApplyCameraTransform;

    if (ClearScreenMode=csmClear) and (CurrentRenderTarget=nil) then
    begin
      glClearColor(ClearColor.V[0],ClearColor.V[1],ClearColor.V[2],ClearColor.V[3]);
      glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
    end;

    glLightModelfv(GL_LIGHT_MODEL_AMBIENT, @Self.AmbientLightColor);

    if Lights.Count=0 then
    begin
      glLightfv(GL_LIGHT0,GL_POSITION,@LightPosition)
    end
    else
    begin
      for J := 0 to Lights.Count-1 do
        TLight(Lights[J]).ApplyLight(J);
    end;

    Driver.EnableMaterial(DefaultMaterial);

    //Render models and application in order specified with App.RendeOrder.
    for J := 0 to 1 do
    begin
      case Ord(Self.RenderOrder) xor J  of
        0 : RenderModels;
        1 :
          begin
            //Render application
            if Self.OnRender.Count>0 then
              Self.OnRender.ExecuteCommands;
            if Self.CurrentState<>nil then
              Self.CurrentState.OnRender.ExecuteCommands;
          end;
      end;
    end;

    Driver.EnableMaterial(DefaultMaterial);

    for J := 0 to Lights.Count-1 do
      TLight(Lights[J]).RemoveLight(J);
  end;

  {$ifndef zgeviz} //Sometimes crash on Intel GPU on Mac if using flush here
  glFlush;
  {$endif}
  //End frame
  {$ifdef minimal}
  //I designer s� ansvarar GL-komponenten f�r swapbuffer
  Platform_SwapBuffers;
  {$endif}
end;

procedure TZApplication.ResetGpuResources;
var
  I,J : integer;
  Model : TModel;
  List : TZArrayList;
begin
  inherited;

  //Instanced models are not part of the project tree if they have been spawned
  //using clone so they need to be handled separately.
  for I := 0 to Models.Cats.Count-1 do
  begin
    List := Models.Get(I);
    for J := 0 to List.Count-1 do
    begin
      Model := TModel(List[J]);
      Model.ResetGpuResources;
    end;
  end;
end;

{$ifndef minimal}
function TZApplication.BindComponent<T>(const CompName: string;
  out O: T): boolean;
var
  Temp : TObject;
begin
  Temp := Self.SymTab.Lookup(CompName);
  if (Temp is T) then
  begin
    O := Temp as T;
    Result := True
  end
  else
  begin
    O := default(T);
    Result := False;
  end;
end;

procedure TZApplication.DesignerSetUpView;
begin
  //Used for previewing app-state in designer
  UpdateViewport;

  Self.ApplyCameraTransform;

  glClearColor(ClearColor.V[0],ClearColor.V[1],ClearColor.V[2],ClearColor.V[3]);

  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  glLightfv(GL_LIGHT0,GL_POSITION,@LightPosition);
end;

function TZApplication.AddToConstantPool(const Value : string) : TObject;
var
  Key : AnsiString;
  Con : TExpStringConstant;
begin
  Key := AnsiString(Value);
  if not ConstantMap.TryGetValue(Key,Result) then
  begin
    Con := TExpStringConstant.Create(ConstantPool);
    Con.SetString('Value',Key);
    ConstantMap.Add(Key,Con);
    Result := Con;
  end;
end;

procedure TZApplication.ClearConstantPool;
begin
  ConstantPool.Clear;
  ConstantMap.Clear;
end;

procedure TZApplication.RefreshSymbolTable;
var
  List : TStringList;
  I : integer;
  Con : TDefineConstant;
  BuiltInFunctions,BuiltInConstants : TObjectList;
  Func : TZcOpFunctionBuiltIn;
begin
  ZcGlobalNames.Clear;
  SymTab.ClearAll;

  //Scope 0: global constants and built-in functions
  BuiltInConstants := Zc_Ops.GetBuiltInConstants;
  for I := 0 to BuiltInConstants.Count - 1 do
  begin
    Con := TDefineConstant(BuiltInConstants[I]);
    SymTab.Add(String(Con.Name),Con);
  end;

  BuiltInFunctions := Zc_Ops.GetBuiltInFunctions;
  for I := 0 to BuiltInFunctions.Count - 1 do
  begin
    Func := TZcOpFunctionBuiltIn(BuiltInFunctions[I]);
    SymTab.Add(Func.MangledName,Func);
  end;

  //Scope 1: object names
  SymTab.PushScope;
  List := TStringList.Create;
  try
    GetObjectNames(Self,List);
    for I := 0 to List.Count-1 do
      SymTab.Add( List[I], List.Objects[I] );
  finally
    List.Free;
  end;
end;

procedure TZApplication.Compile;
var
  I,J: integer;
  C : TZComponent;
  Prop : TZProperty;
  PropValue : TZPropertyValue;
  PropList : TZPropertyList;
  List : TObjectList;
  OldSep : char;
begin
  OldSep := FormatSettings.DecimalSeparator;
  FormatSettings.DecimalSeparator := '.';

  {$ifdef CPUX64}
  (ZApp.SymTab.Lookup('CPUX64') as TDefineConstant).Value:=1;
  {$endif}

  for I := 0 to ZcGlobalNames.Count - 1 do
  begin
    //Remove user function names from symtab
    if (ZcGlobalNames[I] is TZcOpFunctionUserDefined) and SymTab.Contains( TZcOpFunctionUserDefined(ZcGlobalNames[I]).MangledName ) then
      SymTab.Remove( TZcOpFunctionUserDefined(ZcGlobalNames[I]).MangledName )
    else if (ZcGlobalNames[I] is TDefineConstant) and SymTab.Contains(String((ZcGlobalNames[I] as TDefineConstant).Name)) then
      //Also remove inline constants
      SymTab.Remove(String(TDefineConstant(ZcGlobalNames[I]).Name))
    else if (ZcGlobalNames[I] is TZcOpClass) and SymTab.Contains(String((ZcGlobalNames[I] as TZcOpClass).Id)) then
      //Also remove user defined classes
      SymTab.Remove(String(TZcOpClass(ZcGlobalNames[I]).Id))
    else if (ZcGlobalNames[I] is TZcOpGlobalVar) then
      SymTab.Remove(TZcOpGlobalVar(ZcGlobalNames[I]).Id);
  end;
  ZcGlobalNames.Clear;

  //Remove global variables (declared in zlibraries)
  for I := 0 to GlobalVariables.Count-1 do
    SymTab.Remove(String(TDefineVariableBase(GlobalVariables[I]).Name));
  GlobalVariables.Clear;

  //Remove user-defined classes
  UserClasses.Clear;

  //Remove stringconstants
  ClearConstantPool;

  FunctionCleanUps.Clear;

  List := TObjectList.Create(False);
  try
    Self.GetAllChildren(List,False);

    for I := 0 to List.Count - 1 do
    begin
      C := List[I] as TZComponent;
      PropList := C.GetProperties;
      for J := 0 to PropList.Count-1 do
      begin
        Prop := TZProperty(PropList[J]);
        if Prop.PropertyType=zptExpression then
        begin
          PropValue := C.GetProperty(Prop);
          CompileProperty(C,PropValue,Prop);
        end;
      end;
    end;
  finally
    List.Free;
  end;

  FormatSettings.DecimalSeparator := OldSep;
end;

procedure TZApplication.CompileProperty(C : TZComponent; Expr : TZPropertyValue; const Prop : TZProperty);
var
  CurParent: TZComponent;
  Model : TZComponent;
  S : string;
  DefContent : TStringList;
begin
  //ExternalLibrary can have definitions in separate file
  if (C is TZExternalLibrary) and (Prop.Name='Source') then
  begin
    S := string( TZExternalLibrary(C).DefinitionsFile );
    if S<>'' then
    begin
      if not Assigned(Self.OnGetLibraryPath) then
        raise Exception.Create('OnGetLibraryPath not set');
      S := Self.OnGetLibraryPath + PathDelim + S;
      if not FileExists(S) then
        raise Exception.Create('Definitions file not found: ' + S);
      DefContent := TStringList.Create;
      DefContent.LoadFromFile(S);
      Expr.ExpressionValue.Source := DefContent.Text;
      DefContent.Free;
    end;
  end;

  if Prop.IsDefaultValue(Expr) then
  begin
    //Generate no code for an empty expression
    Expr.ExpressionValue.Code.Clear;
    Exit;
  end;

  //Om det finns en model-parent s� skriv den till symbol 'CurrentModel'
  //s� att den kan anv�ndas i uttryck.
  CurParent := C.GetOwner;
  Model := nil;
  while CurParent <> nil do
  begin
    if CurParent is TModel then
    begin
      Model := CurParent as TModel;
      Break;
    end;
    CurParent := CurParent.GetOwner;
  end;
  if Assigned(Model) then
    SymTab.Add('CurrentModel',Model);

  SymTab.Add('this',C);
  try
    Compiler.Compile(Self,C,Expr.ExpressionValue,SymTab,Prop.ReturnType,ZcGlobalNames,Prop.ExpressionKind);
    {$IFDEF DEBUG}
    //if not (C is TZExternalLibrary) then ZLog.GetLog(Self.ClassName).Write(Compiler.CompileDebugString);
    {$ENDIF}
  finally
    if Assigned(Model) then
      SymTab.Remove('CurrentModel');
    SymTab.Remove('this');
  end;
end;
{$endif}

procedure TZApplication.AddModel(Model: TModel);
begin
  if Models.Cats.Count<=Model.Category then
    Models.RegisterCat(Model.Category);
  Models.Add(Model);
end;

procedure AppCaptionChanged(Instance : TZComponent; const PropId : integer; const NewValue : pointer);
begin
  if TZApplication(Instance).Caption<>NewValue then
  begin
    TZApplication(Instance).Caption := NewValue;
    Platform_SetWindowCaption( TZApplication(Instance).Caption );
  end;
end;

procedure AppCameraChanged(Instance : TZComponent; const PropId : integer; const NewValue : pointer);
begin
  TZApplication(Instance).Camera := TCamera(NewValue);
  TZApplication(Instance).ApplyCameraTransform;
  if CurrentModel<>nil then
    Renderer.ApplyModelTransform(CurrentModel);
end;

procedure AppScreenModeChanged(Instance : TZComponent; const PropId : integer; const NewValue : pointer);
var
  B : integer;
begin
  B := integer(NewValue);
  if Ord(TZApplication(Instance).ScreenMode)<>B then
  begin
    ScreenModeChanging := True;
    TZApplication(Instance).ScreenMode := TAppScreenMode(B);
    {$ifdef minimal}
    TZApplication(Instance).ResetGpuResources;
    Platform_ShutdownScreen;
    TZApplication(Instance).CreateWindow;
    {$endif}
    ScreenModeChanging := False;
  end;
end;

procedure TZApplication.DefineProperties(List: TZPropertyList);
begin
  inherited;

  List.AddProperty({$IFNDEF MINIMAL}strOnLoaded,{$ENDIF}(@OnLoaded), zptComponentList);
  List.AddProperty({$IFNDEF MINIMAL}strStates,{$ENDIF}(@States), zptComponentList);
    {$ifndef minimal}List.GetLast.SetChildClasses([TAppState]);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}strOnUpdate,{$ENDIF}(@OnUpdate), zptComponentList);
  List.AddProperty({$IFNDEF MINIMAL}strOnBeginRenderPass,{$ENDIF}(@OnBeginRenderPass), zptComponentList);
  List.AddProperty({$IFNDEF MINIMAL}strOnRender,{$ENDIF}(@OnRender), zptComponentList);
    {$ifndef minimal}{List.GetLast.SetChildClasses([TRenderCommand]);}{$endif}
  List.AddProperty({$IFNDEF MINIMAL}strOnClose,{$ENDIF}(@OnClose), zptComponentList);
  List.AddProperty({$IFNDEF MINIMAL}strLights,{$ENDIF}(@Lights), zptComponentList);
    {$ifndef minimal}List.GetLast.SetChildClasses([TLight]);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}strContent,{$ENDIF}(@Content), zptComponentList);

  List.AddProperty({$IFNDEF MINIMAL}strCaption,{$ENDIF}(@Caption), zptString);
    List.GetLast.IsManagedTarget := True;
    List.GetLast.NotifyWhenChanged := @AppCaptionChanged;
  List.AddProperty({$IFNDEF MINIMAL}strDeltaTime,{$ENDIF}(@DeltaTime), zptFloat);
    List.GetLast.NeverPersist := True;
    List.GetLast.Hint := strDeltaTimeHint;
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
  List.AddProperty({$IFNDEF MINIMAL}strTime,{$ENDIF}(@Time), zptFloat);
    List.GetLast.NeverPersist := True;
    List.GetLast.Hint := strTimeHint;
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}

  List.AddProperty({$IFNDEF MINIMAL}strGLBase,{$ENDIF}(@GLBase), zptByte);
    {$ifndef minimal}List.GetLast.SetOptions(['Compatible','ES2/GL3']);{$endif}
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}

//  List.AddProperty({$IFNDEF MINIMAL}strThreadsEnabled,{$ENDIF}@ThreadedProcessingEnabled, zptBoolean);
//    List.GetLast.DefaultValue.BooleanValue := True;

  List.AddProperty({$IFNDEF MINIMAL}strFpsCounter,{$ENDIF}(@FpsCounter), zptFloat);
    List.GetLast.NeverPersist := True;
    List.GetLast.Hint := strFpsCounterHint;
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
  List.AddProperty({$IFNDEF MINIMAL}strCurrentRenderPass,{$ENDIF}(@CurrentRenderPass), zptInteger);
    List.GetLast.NeverPersist := True;
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
  List.AddProperty({$IFNDEF MINIMAL}strMousePosition,{$ENDIF}(@MousePosition), zptVector3f);
    List.GetLast.NeverPersist := True;
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
  List.AddProperty({$IFNDEF MINIMAL}strMouseWheelDelta,{$ENDIF}(@MouseWheelDelta), zptInteger);
    List.GetLast.NeverPersist := True;
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
  List.AddProperty({$IFNDEF MINIMAL}strClearScreenMode,{$ENDIF}@ClearScreenMode, zptByte);
    {$ifndef minimal}List.GetLast.SetOptions(['ClearScreen','NoClearScreen']);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}strRenderPasses,{$ENDIF}(@RenderPasses), zptInteger);
    List.GetLast.DefaultValue.IntegerValue := 1;
  List.AddProperty({$IFNDEF MINIMAL}strWindowHandle,{$ENDIF}(@WindowHandle), zptInteger);
    List.GetLast.NeverPersist := True;
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}

  List.AddProperty({$IFNDEF MINIMAL}strClearColor,{$ENDIF}(@ClearColor), zptColorf);
  List.AddProperty({$IFNDEF MINIMAL}strAmbientLightColor,{$ENDIF}(@AmbientLightColor), zptColorf);
    List.GetLast.DefaultValue.ColorfValue := MakeColorf(0.4,0.4,0.4,1);

  List.AddProperty({$IFNDEF MINIMAL}strFullScreen,{$ENDIF}(@FullScreen), zptBoolean);
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}

  List.AddProperty({$IFNDEF MINIMAL}strFrameRateStyle,{$ENDIF}(@FrameRateStyle), zptByte);
    {$ifndef minimal}List.GetLast.SetOptions(['SyncedWithMonitor','Free','Fixed']);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}strFixedFrameRate,{$ENDIF}(@FixedFrameRate), zptInteger);

  List.AddProperty({$IFNDEF MINIMAL}strScreenMode,{$ENDIF}(@ScreenMode), zptByte);
    {$ifndef minimal}List.GetLast.SetOptions(['Use Desktop resolution','640x480','800x600','1024x768','1280x800','1280x1024']);{$endif}
    List.GetLast.DefaultValue.ByteValue := 2;
    List.GetLast.NotifyWhenChanged := @AppScreenModeChanged;
  List.AddProperty({$IFNDEF MINIMAL}strShowOptionsDialog,{$ENDIF}(@ShowOptionsDialog), zptBoolean);

  List.AddProperty({$IFNDEF MINIMAL}strCustomScreenWidth,{$ENDIF}(@CustomScreenWidth), zptInteger);
  List.AddProperty({$IFNDEF MINIMAL}strCustomScreenHeight,{$ENDIF}(@CustomScreenHeight), zptInteger);

  List.AddProperty({$IFNDEF MINIMAL}strCameraPosition,{$ENDIF}(@CameraPosition), zptVector3f);
    //Camera default is z 10
    List.GetLast.DefaultValue.Vector3fValue[2] := 10;
  List.AddProperty({$IFNDEF MINIMAL}strCameraRotation,{$ENDIF}(@CameraRotation), zptVector3f);

  List.AddProperty({$IFNDEF MINIMAL}strCamera,{$ENDIF}(@Camera), zptComponentRef);
    {$ifndef minimal}List.GetLast.SetChildClasses([TCamera]);{$endif}
    List.GetLast.NotifyWhenChanged := @AppCameraChanged;

  List.AddProperty({$IFNDEF MINIMAL}strLightPosition,{$ENDIF}(@LightPosition), zptVector3f);
    //Light default is down the Z axis
    List.GetLast.DefaultValue.Vector3fValue[2] := 1;

  List.AddProperty({$IFNDEF MINIMAL}strViewportRatio,{$ENDIF}(@ViewportRatio), zptByte);
    {$ifndef minimal}List.GetLast.SetOptions(['Full window','Custom','4:3','16:9']);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}strCustomViewportRatio,{$ENDIF}(@CustomViewportRatio), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}strFOV,{$ENDIF}(@FOV), zptFloat);
    List.GetLast.DefaultValue.FloatValue := 45;
  List.AddProperty({$IFNDEF MINIMAL}strClipNear,{$ENDIF}(@ClipNear), zptFloat);
    List.GetLast.DefaultValue.FloatValue := 0.1;
  List.AddProperty({$IFNDEF MINIMAL}strClipFar,{$ENDIF}(@ClipFar), zptFloat);
    List.GetLast.DefaultValue.FloatValue := 100;
  List.AddProperty({$IFNDEF MINIMAL}strMouseVisible,{$ENDIF}(@MouseVisible), zptBoolean);
  List.AddProperty({$IFNDEF MINIMAL}strEscapeToQuit,{$ENDIF}(@EscapeToQuit), zptBoolean);
    List.GetLast.DefaultValue.BooleanValue := True;
  List.AddProperty({$IFNDEF MINIMAL}strUseStencilBuffer,{$ENDIF}@UseStencilBuffer, zptBoolean);
  List.AddProperty({$IFNDEF MINIMAL}strRenderOrder,{$ENDIF}@RenderOrder, zptByte);
    {$ifndef minimal}List.GetLast.SetOptions(['ModelsBeforeApp','AppBeforeModels']);{$endif}

  List.AddProperty({$IFNDEF MINIMAL}strViewportX,{$ENDIF}(@ViewportX), zptInteger);
    List.GetLast.NeverPersist := True;
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
  List.AddProperty({$IFNDEF MINIMAL}strViewportY,{$ENDIF}(@ViewportY), zptInteger);
    List.GetLast.NeverPersist := True;
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
  List.AddProperty({$IFNDEF MINIMAL}strViewportWidth,{$ENDIF}(@ViewportWidth), zptInteger);
    List.GetLast.NeverPersist := True;
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
  List.AddProperty({$IFNDEF MINIMAL}strViewportHeight,{$ENDIF}(@ViewportHeight), zptInteger);
    List.GetLast.NeverPersist := True;
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}

  List.AddProperty({$IFNDEF MINIMAL}strScreenHeight,{$ENDIF}(@ScreenHeight), zptInteger);
    List.GetLast.NeverPersist := True;
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
    {$ifndef minimal}List.GetLast.DefaultValue.IntegerValue := 600;{$endif}
  List.AddProperty({$IFNDEF MINIMAL}strScreenWidth,{$ENDIF}(@ScreenWidth), zptInteger);
    List.GetLast.NeverPersist := True;
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
    {$ifndef minimal}List.GetLast.DefaultValue.IntegerValue := 800;{$endif}

  List.AddProperty({$IFNDEF MINIMAL}strConstantPool,{$ENDIF}(@ConstantPool), zptComponentList);
    {$ifndef minimal}List.GetLast.ExcludeFromXml := True;{$endif}
    {$ifndef minimal}List.GetLast.HideInGui := True;{$endif}
  List.AddProperty({$IFNDEF MINIMAL}strGlobalVariables,{$ENDIF}(@GlobalVariables), zptComponentList);
    {$ifndef minimal}List.GetLast.ExcludeFromXml := True;{$endif}
    {$ifndef minimal}List.GetLast.HideInGui := True;{$endif}
  List.AddProperty({$IFNDEF MINIMAL}strUserClasses,{$ENDIF}@UserClasses, zptComponentList);
    {$ifndef minimal}List.GetLast.ExcludeFromXml := True;{$endif}
    {$ifndef minimal}List.GetLast.HideInGui := True;{$endif}

  List.AddProperty({$IFNDEF MINIMAL}strNoSound,{$ENDIF}(@NoSound), zptBoolean);
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
    {$ifndef minimal}List.GetLast.HideInGui := True;{$endif}
    {$ifndef minimal}List.GetLast.ExcludeFromXml := True;{$endif}

  List.AddProperty({$IFNDEF MINIMAL}strPointerSize,{$ENDIF}@PointerSize, zptByte);
    List.GetLast.DefaultValue.ByteValue := SizeOf(Pointer);
    {$ifndef minimal}List.GetLast.ExcludeFromXml := True;{$endif}
    {$ifndef minimal}List.GetLast.HideInGui := True;{$endif}

  {$IFNDEF MINIMAL}
  List.AddProperty(strFileVersion,@FileVersion, zptInteger);
    List.SetDesignerProperty;
    List.GetLast.HideInGui := True;
  List.AddProperty(strIcon,@Icon, zptBinary);
    List.SetDesignerProperty;
  List.AddProperty(strPreviewClearColor,@PreviewClearColor, zptColorf);
    List.GetLast.DefaultValue.ColorfValue := MakeColorf(0.5,0.5,0.5,0);
    List.SetDesignerProperty;

  List.AddProperty(strAndroidPackageName,(@AndroidPackageName), zptString);
    List.GetLast.DefaultValue.StringValue := 'com.mydomain.mygame1';
    List.SetDesignerProperty;
  List.AddProperty(strAndroidVersionName,(@AndroidVersionName), zptString);
    List.GetLast.DefaultValue.StringValue := '1.0';
    List.SetDesignerProperty;
  List.AddProperty(strAndroidVersionNumber,(@AndroidVersionNumber), zptInteger);
    List.GetLast.DefaultValue.IntegerValue := 1;
    List.SetDesignerProperty;
  List.AddProperty(strAndroidPortrait,@AndroidPortrait, zptByte);
    List.SetDesignerProperty;
  List.AddProperty(strAndroidSdk,(@AndroidSdk), zptByte);
    {$ifndef minimal}List.GetLast.SetOptions(['2.2 (API Level 8)','4.1 (API Level 16)']);{$endif}
    {$ifndef minimal}List.GetLast.DefaultValue.ByteValue := 1;{$endif}
    List.SetDesignerProperty;
  {$ENDIF}
end;

{$ifndef minimal}
procedure TZApplication.DesignerStart(const ViewW,ViewH : integer; const InitTime : single = 0);
begin
//  Assert(ZApp=Self);
  Self.Init;

  ScreenWidth := ViewW;
  ScreenHeight := ViewH;

  Self.Time := InitTime;
  Self.DeltaTime := 0;
  Self.LastTime := InitTime;
  Self.FpsTime := 0;
  Self.NextFrameTime := Self.Time;

  //Initial tree update
  //Content.Update;
  OnLoaded.ExecuteCommands;
  DesignerIsRunning := True;
end;

procedure TZApplication.DesignerStop;
begin
//  Assert(ZApp=Self);
  if (Self.CurrentState<>nil) then
    Self.CurrentState.OnLeave.ExecuteCommands;
  DesignerIsRunning := False;
  Models.RemoveAll;
  Models.FlushRemoveList;
  Collisions.ClearAll;
  OnClose.ExecuteCommands;
  Renderer.DesignerRenderStop;
  AudioPlayer.DesignerStopAllAudio;
  Self.CurrentState := nil;
  Self.DeltaTime := 0;

  while Self.LiveThreadCount>0 do
    Platform_Sleep(0);
end;
{$endif}


/////////////////////////////

function LoadApplicationComponent : TZApplication;
begin
  Result := TZApplication(ComponentManager.LoadBinary);
end;


{ TAppState }

procedure TAppState.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}strModelUpdatesEnabled,{$ENDIF}(@ModelUpdatesEnabled), zptBoolean);
    List.GetLast.DefaultValue.BooleanValue := True;
  List.AddProperty({$IFNDEF MINIMAL}strCollisionsEnabled,{$ENDIF}(@CollisionsEnabled), zptBoolean);
    List.GetLast.DefaultValue.BooleanValue := True;
end;


{ TSetAppState }

procedure TSetAppState.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}strState,{$ENDIF}(@State), zptComponentRef);
    {$ifndef minimal}List.GetLast.SetChildClasses([TAppState]);{$endif}
end;

procedure TSetAppState.Execute;
var
  OldState : TStateBase;
begin
  OldState := ZApp.CurrentState;
  ZApp.CurrentState := Self.State;
  if (OldState<>nil) then
    OldState.OnLeave.ExecuteCommands;
  if Self.State<>nil then
    Self.State.OnStart.ExecuteCommands;
end;

{$ifndef minimal}
function TSetAppState.GetDisplayName: AnsiString;
begin
  Result := inherited GetDisplayName;
  if Assigned(State) then
    Result := Result + '  ' + State.Name;
end;
{$endif}

{ TCamera }

procedure TCamera.ApplyTransform(const Aspect : single);
var
  W,H : single;
  D : TGLDriverBase;
begin
  D := Self.ZApp.Driver;

  //Setup view and camera
  {$ifdef zgeviz}
  if Assigned(Self.ZApp.ZgeVizCameraCallback) then
    Self.ZApp.ZgeVizCameraCallback(Self.ZApp)
  else
  begin
  {$endif}
    D.MatrixMode(GL_PROJECTION);
    D.LoadIdentity;
    case Self.Kind of
      catPerspective :
        begin
          D.Perspective(Self.FOV, Aspect, Self.ClipNear, Self.ClipFar);
        end;
    else
      begin
        //Ortho
        {$ifndef minimal}
        if Abs(Self.OrthoZoom)>0.000001 then
        begin //Avoid divide by zero
        {$endif}
        H := Self.OrthoZoom;
        W := Aspect * H;
        D.Ortho(-W,W,-H,H,Self.ClipNear, Self.ClipFar);
        {$ifndef minimal}
        end;
        {$endif}
      end;
    end;
    D.MatrixMode(GL_MODELVIEW);
    D.LoadIdentity;
  {$ifdef zgeviz}
  end;
  {$endif}

  //Reverse order to make XYZ-rotation
  D.Rotate( (Rotation[0]*360) , 1, 0, 0);
  D.Rotate( (Rotation[1]*360) , 0, 1, 0);
  D.Rotate( (Rotation[2]*360) , 0, 0, 1);
  //M�ste ta negativt p� cameraposition f�r att dess axlar ska bete sig
  //likadant som modell-koordinater (positiv y = upp�t t.ex.)
  D.Translate(-Position[0], -Position[1], -Position[2]);
end;

procedure TCamera.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}strKind,{$ENDIF}(@Kind), zptByte);
    {$ifndef minimal}List.GetLast.SetOptions(['Perspective','Orthographic']);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}strPosition,{$ENDIF}(@Position), zptVector3f);
  List.AddProperty({$IFNDEF MINIMAL}strRotation,{$ENDIF}(@Rotation), zptVector3f);
  List.AddProperty({$IFNDEF MINIMAL}strClipNear,{$ENDIF}(@ClipNear), zptFloat);
    List.GetLast.DefaultValue.FloatValue := 0.1;
  List.AddProperty({$IFNDEF MINIMAL}strClipFar,{$ENDIF}(@ClipFar), zptFloat);
    List.GetLast.DefaultValue.FloatValue := 100;
  List.AddProperty({$IFNDEF MINIMAL}strOrthoZoom,{$ENDIF}(@OrthoZoom), zptFloat);
    List.GetLast.DefaultValue.FloatValue := 1.0;
  List.AddProperty({$IFNDEF MINIMAL}strFOV,{$ENDIF}(@FOV), zptFloat);
    List.GetLast.DefaultValue.FloatValue := 45;
end;

{ TComponentHelper }

function TComponentHelper.ZApp: TZApplication;
begin
  //Get the application that this component belongs to
  //Put in a helper class to avoid circular dependency on ZClasses and ZApplication units
  if _ZApp=nil then
  begin
    if Self is TZApplication then
      _ZApp := Self
    else
    begin
      {$ifndef minimal}
      Assert((Self.OwnerList<>nil) and (Self.OwnerList.Owner<>nil),'Failed to reach ZApp parent');
      {$endif}
      _ZApp := Self.OwnerList.Owner.ZApp;
    end;
  end;
  Result := _ZApp;
end;

{$ifndef minimal}
function TComponentHelper.HasZApp: boolean;
begin
  if _ZApp=nil then
  begin
    if Self is TZApplication then
      Exit(True)
    else
    begin
      if (Self.OwnerList=nil) or (Self.OwnerList.Owner=nil) then
        Exit(False);
      Exit( Self.OwnerList.Owner.HasZApp );
    end;
  end else
    Exit(True);
end;
{$endif}

initialization

  ZClasses.Register(TZApplication,ApplicationClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NoUserCreate:=True;{$endif}
    {$ifndef minimal}ComponentManager.LastAdded.ImageIndex:=6;{$endif}

  ZClasses.Register(TAppState,AppStateClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NoTopLevelCreate:=True;{$endif}
    {$ifndef minimal}ComponentManager.LastAdded.AutoName:=True;{$endif}
    {$ifndef minimal}ComponentManager.LastAdded.ImageIndex:=20;{$endif}
  ZClasses.Register(TSetAppState,SetAppStateClassId);
    {$ifndef minimal}ComponentManager.LastAdded.ImageIndex := 54;{$endif}
  ZClasses.Register(TCamera,CameraClassId);
    {$ifndef minimal}ComponentManager.LastAdded.AutoName := True;{$endif}
    {$ifndef minimal}ComponentManager.LastAdded.ImageIndex:=26;{$endif}

end.
