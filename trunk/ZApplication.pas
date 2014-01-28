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

interface

uses ZClasses,Meshes,Collision,Commands,AudioComponents, GLDrivers
  {$ifndef minimal},Generics.Collections,uSymTab,Contnrs{$endif}
;

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
    procedure ApplyTransform(App : TZApplication);
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Kind : (catPerspective,catOrthograpic);
    Position : TZVector3f;
    Rotation : TZVector3f;
    ClipNear,ClipFar,OrthoZoom,FOV : single;
  end;

  TZApplication = class(TZComponent)
  strict private
    DepthList : TZArrayList;
    ConstantPool : TZComponentList;
    {$ifndef minimal}
    ConstantMap : TDictionary<AnsiString,TObject>;
    {$endif}
    OldCaption : pointer;
    FpsFrames : integer;
    FpsCounter,FpsTime : single;
    HasShutdown : boolean;
    TargetFrameRate : integer;
    NextFrameTime : single;
    LastTime : single;
    procedure RenderModels;
    procedure ApplyCameraTransform;
    procedure MainSlice;
    procedure Init;
    procedure Shutdown;
    procedure UpdateScreen;
  private
    CurrentState : TAppState;
    {$ifndef minimal}public{$endif}
    procedure UpdateTime;
    procedure UpdateStateVars;
    {$if (not defined(minimal)) or defined(android)}public{$ifend}
    function Main : boolean;
  protected
    procedure DefineProperties(List: TZPropertyList); override;
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
    GlobalVars : TZComponentList;
    Terminating : boolean;
    Time,DeltaTime : single;
    CurrentMusic : TMusic;
    Caption : TPropString;
    MousePosition : TZVector3f;
    ClearScreenMode : integer;
    ClearColor : TZColorf;
    AmbientLightColor : TZColorf;
    Fullscreen : boolean;
    ScreenMode : (vmFullScreenDesktop,vm640x480,vm800x600,vm1024x768,vm1280x800,vm1280x1024);
    ShowOptionsDialog : boolean;
    CustomScreenWidth,CustomScreenHeight : integer;
    CameraPosition : TZVector3f;
    CameraRotation : TZVector3f;
    LightPosition : TZVector4f;
    CustomViewportRatio,FOV,ClipNear,ClipFar : single;
    ViewportRatio : (vprFullWindow,vprCustom,vpr4_3,vpr16_9);
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
    FrameLoss : boolean;  //sätts till true ifall vi tappar frames
    ScreenWidth : integer;
    ScreenHeight : integer;
    {$ifndef minimal}
    Icon : TZBinaryPropValue;
    AndroidPackageName,AndroidVersionName : TPropString;
    AndroidVersionNumber : integer;
    AndroidPortrait : boolean;
    PreviewClearColor : TZColorf;
    DesignerIsRunning : boolean;
    //Zc-parsing
    SymTab : TSymbolTable;
    ZcGlobalNames : TObjectList;
    OnGetLibraryPath : function : string of object;
    {$endif}
    {$ifdef zgeviz}
    ZgeVizCameraRotation : TZVector3f;
    ZgeVizTime : single;
    ZgeVizRenderPassOverride : integer;
    ZgeVizCameraCallback,ZgeVizViewportCallback : TAppCallback;
    procedure ViewportChanged;
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
    procedure CompileProperty(C : TZComponent; Expr : TZPropertyValue; Prop : TZProperty);
    {$endif}
  end;

  TComponentHelper = class helper for TZComponent
  public
    function ZApp : TZApplication;
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

implementation

uses ZPlatform,ZLog,AudioPlayer,ZMath,Renderer,ZOpenGL
  {$ifndef minimal}
  ,ZExpressions,SysUtils,Zc_Ops,Classes,Compiler
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
  {$endif}
end;

destructor TZApplication.Destroy;
begin
  Terminating := True;

  //ev problem med models.free, borde göra models.removeall vid terminate först
  //annars kan models referera modeller som redan har gjorts free på
  Models.Free;

  Collisions.Free;
  DepthList.Free;
  if not HasShutdown then
    Shutdown;

  Driver.Free;

  {$ifndef minimal}
  ConstantMap.Free;
  SymTab.Free;
  ZcGlobalNames.Free;
  {$endif}
  inherited;
end;

procedure TZApplication.Init;
{$ifdef minimal}
var
  I : integer;
{$endif}
begin
  {$ifndef zgeviz}
  Platform_InitGlobals;  //Nollställ timer etc
  {$endif}

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

    TargetFrameRate := Platform_GetDisplayRefreshRate;

    if Platform_CommandLine('s') then
      NoSound := True;
    if not NoSound then
      Platform_InitAudio;
  {$endif}
end;

procedure TZApplication.Shutdown;
begin
  {$ifdef minimal}
  OnClose.ExecuteCommands;
  Platform_ShutdownScreen;
  if not NoSound then
    Platform_ShutdownAudio;
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
    //todo: om deltatime är för hög, öka LostTime, och justera Time och Accumulated med den

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

  //-1 .. 1, 0 är center
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

procedure TZApplication.UpdateStateVars;
begin
  PZVector2f(@MousePosition)^ := NormalizeToScreen(Platform_GetMousePos);
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
  {$ifend}

begin
  Now := Platform_GetTime;

  {$ifdef minimal}
  if Now>=NextFrameTime then
  {$endif}
  begin
    if FrameRateStyle<>frsFree then
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

    UpdateStateVars;

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
      {$ifend}
    end;

    if ManagedHeap_GetAllocCount>0 then
      ManagedHeap_GarbageCollect(False);

    {$ifdef minimal}
    if FrameRateStyle<>frsFree then
    begin //Give remaining time back to the OS to avoid 100% cpu pressure
      Remaining := Trunc((NextFrameTime - Platform_GetTime) * 1000);
      if Remaining>0 then
        Platform_Sleep(Remaining);
    end;
    {$endif}
  end;

  //Keep calling main until Terminate is called
  Result := not Terminating;
end;

procedure TZApplication.MainSlice;
begin
  if CurrentMusic<>nil then
    CurrentMusic.Update;

  //Uppdatera current state
  if CurrentState<>nil then
    CurrentState.Update;

  //Application onupdate
  OnUpdate.ExecuteCommands;
  //Update application renderers: particlesystems, beams etc
  OnRender.Update;

  //Uppdatera modeller, flytta noder
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

  //Update window caption
  if pointer(Self.Caption)<>Self.OldCaption then
  begin
    Self.OldCaption := Self.Caption;
    Platform_SetWindowCaption(Self.Caption);
  end;

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
  Init;
  //Skip initial tree update for now because it triggers AppState.OnStart etc. which can cause trouble
  //Any component needing init should ovveride Loaded instead.
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
begin
  DepthList.Clear;

  {$ifndef android}
  glGetFloatv(GL_PROJECTION_MATRIX, @Matrix);
  glGetFloatv(GL_MODELVIEW_MATRIX, @TmpM);
  Matrix := MatrixMultiply(TmpM,Matrix);
  {$else}
  Matrix := IdentityHmgMatrix;
  {$endif}

  for I := 0 to Models.Cats.Count-1 do
  begin
    List := Models.Get(I);
    for J := 0 to List.Count-1 do
    begin
      Model := TModel(List[J]);
      if Model.RenderOrder=roDepthsorted then
      begin
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
  //Måste ta negativt på cameraposition för att dess axlar ska bete sig
  //likadant som modell-koordinater (positiv y = uppåt t.ex.)
  Driver.Translate(-CameraPosition[0], -CameraPosition[1], -CameraPosition[2]);
end;

procedure TZApplication.UpdateScreen;
var
  I,J : integer;
begin

  for I := 0 to Self.RenderPasses-1 do
  begin
    Self.CurrentRenderPass := I;
    {$ifdef zgeviz}
    if ZgeVizRenderPassOverride<>0 then
      Self.CurrentRenderPass := ZgeVizRenderPassOverride-1;
    {$endif}

    if Self.OnBeginRenderPass.Count>0 then
      Self.OnBeginRenderPass.ExecuteCommands;

    {$ifndef zgeviz}
    if {$if defined(minimal) and (not defined(android))}(ViewportRatio=vprCustom) and {$ifend}
      (CurrentRenderTarget=nil) then
      UpdateViewport;
    {$endif}

    //Use custom camera or default
    if Camera<>nil then
      Camera.ApplyTransform(Self)
    else
      ApplyCameraTransform;

    if (ClearScreenMode=0) and (CurrentRenderTarget=nil) then
    begin
      glClearColor(ClearColor.V[0],ClearColor.V[1],ClearColor.V[2],0);
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
      RenderModels;

      //Render application
      Driver.PushMatrix;
      if Self.OnRender.Count>0 then
        Self.OnRender.ExecuteCommands;
      if Self.CurrentState<>nil then
        Self.CurrentState.OnRender.ExecuteCommands;
      Driver.PopMatrix;
    Driver.EnableMaterial(DefaultMaterial);

    for J := 0 to Lights.Count-1 do
      TLight(Lights[J]).RemoveLight(J);
  end;

  //End frame
  glFlush;
  {$ifdef minimal}
  //I designer så ansvarar GL-komponenten för swapbuffer
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
procedure TZApplication.DesignerSetUpView;
begin
  //Used for previewing app-state in designer
  UpdateViewport;

  if Camera<>nil then
    Camera.ApplyTransform(Self)
  else
    Self.ApplyCameraTransform;

  glClearColor(ClearColor.V[0],ClearColor.V[1],ClearColor.V[2],0);

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
    SymTab.Add(Func.Id,Func);
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

  for I := 0 to ZcGlobalNames.Count - 1 do
  begin
    //Remove user function names from symtab
    if (ZcGlobalNames[I] is TZcOpFunctionUserDefined) and SymTab.Contains((ZcGlobalNames[I] as TZcOp).Id) then
      SymTab.Remove(TZcOp(ZcGlobalNames[I]).Id)
    else if (ZcGlobalNames[I] is TDefineConstant) and SymTab.Contains(String((ZcGlobalNames[I] as TDefineConstant).Name)) then
      //Also remove inline constants
      SymTab.Remove(String(TDefineConstant(ZcGlobalNames[I]).Name));
  end;
  ZcGlobalNames.Clear;

  //Remove global variables (declared in zlibraries)
  for I := 0 to GlobalVars.Count-1 do
  begin
    SymTab.Remove(String(TDefineVariableBase(GlobalVars[I]).Name))
  end;
  GlobalVars.Clear;

  //Remove stringconstants
  ClearConstantPool;

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
          C.GetProperty(Prop,PropValue);
          CompileProperty(C,PropValue,Prop);
        end;
      end;
    end;
  finally
    List.Free;
  end;

  FormatSettings.DecimalSeparator := OldSep;
end;

procedure TZApplication.CompileProperty(C : TZComponent; Expr : TZPropertyValue; Prop : TZProperty);
var
  CurParent: TZComponent;
  Model : TZComponent;
  S : string;
  DefContent : TStringList;
  AllowFuncDefs : boolean;
begin
  //ExternalLibrary can have definitions in separate file
  if (C is TZExternalLibrary) and (Prop.Name='Source') then
  begin
    S := string( TZExternalLibrary(C).DefinitionsFile );
    if S<>'' then
    begin
      if not Assigned(Self.OnGetLibraryPath) then
        raise Exception.Create('OnGetLibraryPath not set');
      S := Self.OnGetLibraryPath + '\' + S;
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

  //Om det finns en model-parent så skriv den till symbol 'CurrentModel'
  //så att den kan användas i uttryck.
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

  //allow function definitions if compiling a library
  AllowFuncDefs := (C is TZLibrary) or ((C is TZExternalLibrary) and (Prop.Name='Source'));

  SymTab.Add('this',C);
  try
    Compiler.Compile(Self,C,Expr.ExpressionValue,SymTab,Prop.ReturnType,ZcGlobalNames,AllowFuncDefs);
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

procedure TZApplication.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'OnLoaded',{$ENDIF}(@OnLoaded), zptComponentList);
  List.AddProperty({$IFNDEF MINIMAL}'OnClose',{$ENDIF}(@OnClose), zptComponentList);
  List.AddProperty({$IFNDEF MINIMAL}'States',{$ENDIF}(@States), zptComponentList);
    {$ifndef minimal}List.GetLast.SetChildClasses([TAppState]);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'OnUpdate',{$ENDIF}(@OnUpdate), zptComponentList);
  List.AddProperty({$IFNDEF MINIMAL}'OnRender',{$ENDIF}(@OnRender), zptComponentList);
    {$ifndef minimal}{List.GetLast.SetChildClasses([TRenderCommand]);}{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'OnBeginRenderPass',{$ENDIF}(@OnBeginRenderPass), zptComponentList);
  List.AddProperty({$IFNDEF MINIMAL}'Lights',{$ENDIF}(@Lights), zptComponentList);
    {$ifndef minimal}List.GetLast.SetChildClasses([TLight]);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'Content',{$ENDIF}(@Content), zptComponentList);
  List.AddProperty({$IFNDEF MINIMAL}'Caption',{$ENDIF}(@Caption), zptString);
    List.GetLast.IsManagedTarget := True;
  List.AddProperty({$IFNDEF MINIMAL}'DeltaTime',{$ENDIF}(@DeltaTime), zptFloat);
    List.GetLast.NeverPersist := True;
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'Time',{$ENDIF}(@Time), zptFloat);
    List.GetLast.NeverPersist := True;
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}

  List.AddProperty({$IFNDEF MINIMAL}'GLBase',{$ENDIF}(@GLBase), zptByte);
    {$ifndef minimal}List.GetLast.SetOptions(['Compatible','ES2/GL3']);{$endif}
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}

  List.AddProperty({$IFNDEF MINIMAL}'FpsCounter',{$ENDIF}(@FpsCounter), zptFloat);
    List.GetLast.NeverPersist := True;
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'CurrentRenderPass',{$ENDIF}(@CurrentRenderPass), zptInteger);
    List.GetLast.NeverPersist := True;
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'MousePosition',{$ENDIF}(@MousePosition), zptVector3f);
    List.GetLast.NeverPersist := True;
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'MouseWheelDelta',{$ENDIF}(@MouseWheelDelta), zptInteger);
    List.GetLast.NeverPersist := True;
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'ClearScreenMode',{$ENDIF}(@ClearScreenMode), zptInteger);
    List.GetLast.NeverPersist := True;
  List.AddProperty({$IFNDEF MINIMAL}'RenderPasses',{$ENDIF}(@RenderPasses), zptInteger);
    List.GetLast.DefaultValue.IntegerValue := 1;
  List.AddProperty({$IFNDEF MINIMAL}'WindowHandle',{$ENDIF}(@WindowHandle), zptInteger);
    List.GetLast.NeverPersist := True;
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}

  List.AddProperty({$IFNDEF MINIMAL}'ClearColor',{$ENDIF}(@ClearColor), zptColorf);
  List.AddProperty({$IFNDEF MINIMAL}'AmbientLightColor',{$ENDIF}(@AmbientLightColor), zptColorf);
    List.GetLast.DefaultValue.ColorfValue := MakeColorf(0.4,0.4,0.4,1);

  List.AddProperty({$IFNDEF MINIMAL}'FullScreen',{$ENDIF}(@FullScreen), zptBoolean);

  List.AddProperty({$IFNDEF MINIMAL}'FrameRateStyle',{$ENDIF}(@FrameRateStyle), zptByte);
    {$ifndef minimal}List.GetLast.SetOptions(['SyncedWithMonitor','Free','Fixed']);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'FixedFrameRate',{$ENDIF}(@FixedFrameRate), zptInteger);

  List.AddProperty({$IFNDEF MINIMAL}'ScreenMode',{$ENDIF}(@ScreenMode), zptByte);
    {$ifndef minimal}List.GetLast.SetOptions(['Use Desktop resolution','640x480','800x600','1024x768','1280x800','1280x1024']);{$endif}
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
    List.GetLast.DefaultValue.ByteValue := 2;
  List.AddProperty({$IFNDEF MINIMAL}'ShowOptionsDialog',{$ENDIF}(@ShowOptionsDialog), zptBoolean);

  List.AddProperty({$IFNDEF MINIMAL}'CustomScreenWidth',{$ENDIF}(@CustomScreenWidth), zptInteger);
  List.AddProperty({$IFNDEF MINIMAL}'CustomScreenHeight',{$ENDIF}(@CustomScreenHeight), zptInteger);

  List.AddProperty({$IFNDEF MINIMAL}'CameraPosition',{$ENDIF}(@CameraPosition), zptVector3f);
    //Camera default is z 10
    List.GetLast.DefaultValue.Vector3fValue[2] := 10;
  List.AddProperty({$IFNDEF MINIMAL}'CameraRotation',{$ENDIF}(@CameraRotation), zptVector3f);

  List.AddProperty({$IFNDEF MINIMAL}'Camera',{$ENDIF}(@Camera), zptComponentRef);
    {$ifndef minimal}List.GetLast.SetChildClasses([TCamera]);{$endif}

  List.AddProperty({$IFNDEF MINIMAL}'LightPosition',{$ENDIF}(@LightPosition), zptVector3f);
    //Light default is down the Z axis
    List.GetLast.DefaultValue.Vector3fValue[2] := 1;

  List.AddProperty({$IFNDEF MINIMAL}'ViewportRatio',{$ENDIF}(@ViewportRatio), zptByte);
    {$ifndef minimal}List.GetLast.SetOptions(['Full window','Custom','4:3','16:9']);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'CustomViewportRatio',{$ENDIF}(@CustomViewportRatio), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}'FOV',{$ENDIF}(@FOV), zptFloat);
    List.GetLast.DefaultValue.FloatValue := 45;
  List.AddProperty({$IFNDEF MINIMAL}'ClipNear',{$ENDIF}(@ClipNear), zptFloat);
    List.GetLast.DefaultValue.FloatValue := 0.1;
  List.AddProperty({$IFNDEF MINIMAL}'ClipFar',{$ENDIF}(@ClipFar), zptFloat);
    List.GetLast.DefaultValue.FloatValue := 100;
  List.AddProperty({$IFNDEF MINIMAL}'MouseVisible',{$ENDIF}(@MouseVisible), zptBoolean);
  List.AddProperty({$IFNDEF MINIMAL}'EscapeToQuit',{$ENDIF}(@EscapeToQuit), zptBoolean);
    List.GetLast.DefaultValue.BooleanValue := True;

  List.AddProperty({$IFNDEF MINIMAL}'ViewportX',{$ENDIF}(@ViewportX), zptInteger);
    List.GetLast.NeverPersist := True;
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'ViewportY',{$ENDIF}(@ViewportX), zptInteger);
    List.GetLast.NeverPersist := True;
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'ViewportWidth',{$ENDIF}(@ViewportWidth), zptInteger);
    List.GetLast.NeverPersist := True;
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'ViewportHeight',{$ENDIF}(@ViewportHeight), zptInteger);
    List.GetLast.NeverPersist := True;
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}

  List.AddProperty({$IFNDEF MINIMAL}'ScreenHeight',{$ENDIF}(@ScreenHeight), zptInteger);
    List.GetLast.NeverPersist := True;
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
    {$ifndef minimal}List.GetLast.DefaultValue.IntegerValue := 600;{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'ScreenWidth',{$ENDIF}(@ScreenWidth), zptInteger);
    List.GetLast.NeverPersist := True;
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
    {$ifndef minimal}List.GetLast.DefaultValue.IntegerValue := 800;{$endif}

  List.AddProperty({$IFNDEF MINIMAL}'ConstantPool',{$ENDIF}(@ConstantPool), zptComponentList);
    {$ifndef minimal}List.GetLast.ExcludeFromXml := True;{$endif}
    {$ifndef minimal}List.GetLast.HideInGui := True;{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'GlobalVars',{$ENDIF}(@GlobalVars), zptComponentList);
    {$ifndef minimal}List.GetLast.ExcludeFromXml := True;{$endif}
    {$ifndef minimal}List.GetLast.HideInGui := True;{$endif}

  List.AddProperty({$IFNDEF MINIMAL}'NoSound',{$ENDIF}(@NoSound), zptBoolean);
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
    {$ifndef minimal}List.GetLast.HideInGui := True;{$endif}

  {$IFNDEF MINIMAL}
  List.AddProperty('Icon',@Icon, zptBinary);
    List.SetDesignerProperty;
  List.AddProperty('PreviewClearColor',@PreviewClearColor, zptColorf);
    List.GetLast.DefaultValue.ColorfValue := MakeColorf(0.5,0.5,0.5,0);
    List.SetDesignerProperty;

  List.AddProperty('AndroidPackageName',(@AndroidPackageName), zptString);
    List.GetLast.DefaultValue.StringValue := 'com.mydomain.mygame1';
    List.SetDesignerProperty;
  List.AddProperty('AndroidVersionName',(@AndroidVersionName), zptString);
    List.GetLast.DefaultValue.StringValue := '1.0';
    List.SetDesignerProperty;
  List.AddProperty('AndroidVersionNumber',(@AndroidVersionNumber), zptInteger);
    List.GetLast.DefaultValue.IntegerValue := 1;
    List.SetDesignerProperty;
  List.AddProperty('AndroidPortrait',(@AndroidPortrait), zptBoolean);
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

  //Initial tree update
  //Content.Update;
  Self.AfterLoaded;
  OnLoaded.ExecuteCommands;
  DesignerIsRunning := True;
end;

procedure TZApplication.DesignerStop;
begin
//  Assert(ZApp=Self);
  DesignerIsRunning := False;
  Models.RemoveAll;
  Models.FlushRemoveList;
  Collisions.ClearAll;
  OnClose.ExecuteCommands;
  Renderer.DesignerRenderStop;
  AudioPlayer.DesignerStopAllAudio;
  Self.CurrentState := nil;
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
  List.AddProperty({$IFNDEF MINIMAL}'ModelUpdatesEnabled',{$ENDIF}(@ModelUpdatesEnabled), zptBoolean);
    List.GetLast.DefaultValue.BooleanValue := True;
  List.AddProperty({$IFNDEF MINIMAL}'CollisionsEnabled',{$ENDIF}(@CollisionsEnabled), zptBoolean);
    List.GetLast.DefaultValue.BooleanValue := True;
end;


{ TSetAppState }

procedure TSetAppState.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'State',{$ENDIF}(@State), zptComponentRef);
    {$ifndef minimal}List.GetLast.SetChildClasses([TAppState]);{$endif}
end;

procedure TSetAppState.Execute;
var
  OldState : TStateBase;
begin
  OldState := ZApp.CurrentState;
  ZApp.CurrentState := Self.State;
  if (OldState<>nil) and (OldState<>Self.State) then
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

procedure TCamera.ApplyTransform(App : TZApplication);
var
  W,H : single;
begin
  //Setup view and camera

  App.Driver.MatrixMode(GL_PROJECTION);
  App.Driver.LoadIdentity;
  case Self.Kind of
    catPerspective :
      begin
        App.Driver.Perspective(Self.FOV, App.ActualViewportRatio, Self.ClipNear, Self.ClipFar);
      end;
  else
    begin
      //Ortho
      {$ifndef minimal}
      if Abs(Self.OrthoZoom)>0.000001 then
      begin //Avoid divide by zero
      {$endif}
      H := Self.OrthoZoom;
      W := App.ActualViewportRatio * H;
      App.Driver.Ortho(-W,W,-H,H,Self.ClipNear, Self.ClipFar);
      {$ifndef minimal}
      end;
      {$endif}
    end;
  end;
  App.Driver.MatrixMode(GL_MODELVIEW);
  App.Driver.LoadIdentity;

  //Reverse order to make XYZ-rotation
  App.Driver.Rotate( (Rotation[0]*360) , 1, 0, 0);
  App.Driver.Rotate( (Rotation[1]*360) , 0, 1, 0);
  App.Driver.Rotate( (Rotation[2]*360) , 0, 0, 1);
  //Måste ta negativt på cameraposition för att dess axlar ska bete sig
  //likadant som modell-koordinater (positiv y = uppåt t.ex.)
  App.Driver.Translate(-Position[0], -Position[1], -Position[2]);
end;

procedure TCamera.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Kind',{$ENDIF}(@Kind), zptByte);
    {$ifndef minimal}List.GetLast.SetOptions(['Perspective','Orthographic']);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'Position',{$ENDIF}(@Position), zptVector3f);
  List.AddProperty({$IFNDEF MINIMAL}'Rotation',{$ENDIF}(@Rotation), zptVector3f);
  List.AddProperty({$IFNDEF MINIMAL}'ClipNear',{$ENDIF}(@ClipNear), zptFloat);
    List.GetLast.DefaultValue.FloatValue := 0.1;
  List.AddProperty({$IFNDEF MINIMAL}'ClipFar',{$ENDIF}(@ClipFar), zptFloat);
    List.GetLast.DefaultValue.FloatValue := 100;
  List.AddProperty({$IFNDEF MINIMAL}'OrthoZoom',{$ENDIF}(@OrthoZoom), zptFloat);
    List.GetLast.DefaultValue.FloatValue := 1.0;
  List.AddProperty({$IFNDEF MINIMAL}'FOV',{$ENDIF}(@FOV), zptFloat);
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