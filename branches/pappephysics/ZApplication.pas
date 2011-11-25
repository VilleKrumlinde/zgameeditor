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

unit ZApplication;

interface

uses ZClasses,Meshes,Collision,Commands,AudioComponents,PAPPE
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
    Physics: TPhysics;
    PhysicsCollide:TPhysicsCollide;
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
    {$if (not defined(minimal))}public{$ifend}
    function Main : boolean;
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Content : TZComponentList;
    Models : TModels;  //not a property
    Collisions : TCollisionChecks;
    States : TZComponentList;
    OnLoaded : TZComponentList;
    OnClose : TZComponentList;
    OnUpdate : TZComponentList;
    OnRender : TZComponentList;
    OnBeginRenderPass : TZComponentList;
    Terminating : boolean;
    Time,DeltaTime : single;
    CurrentMusic : TMusic;
    Caption : TPropString;
    MousePosition : TZVector3f;
    ClearScreenMode : integer;
    ClearColor : TZColorf;
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
    NoSound : boolean;
    FrameLoss : boolean;  //sätts till true ifall vi tappar frames
    ScreenWidth : integer;
    ScreenHeight : integer;
    {$ifndef minimal}
    Icon : TZBinaryPropValue;
    PreviewClearColor : TZColorf;
    DesignerIsRunning : boolean;
    //Zc-parsing
    SymTab : TSymbolTable;
    ZcGlobalNames : TObjectList;
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
    {$ifndef minimal}
    procedure Terminate;
    procedure DesignerStart(const ViewW,ViewH : integer);
    procedure DesignerStop;
    procedure DesignerSetUpView;
    function AddToConstantPool(const Value : string) : TObject;
    procedure ClearConstantPool;
    procedure RefreshSymbolTable;
    procedure Compile;
    procedure CompileProperty(C : TZComponent; const Expr : TZPropertyValue; Prop : TZProperty);
    procedure DesignerFreeResources; override;
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

var
  ZApp : TZApplication;


implementation

uses ZPlatform,ZOpenGL,ZLog,AudioPlayer,ZMath,Renderer
  {$ifndef minimal}
  ,ZExpressions,SysUtils,Zc_Ops,Classes,Compiler
  {$endif}
  ;

{ TZApplication }

constructor TZApplication.Create(OwnerList: TZComponentList);
begin
  inherited;
  PhysicsInit(Physics);
  PhysicsInstance:=@Physics;
//PhysicsInstance^.SweepAndPruneWorkMode:=sapwmOFF;
  PhysicsCollideInit(PhysicsCollide);
  Models := TModels.Create;

  Collisions := TCollisionChecks.Create(Models);
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

  PhysicsCollideDone(PhysicsCollide);
  PhysicsDone(Physics);

  Collisions.Free;
  DepthList.Free;
  if not HasShutdown then
    Shutdown;

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
    UpdateViewport;
  {$else}
    if Platform_CommandLine('f') then
      Self.FullScreen := True;
    if Platform_CommandLine('w') then
      Self.FullScreen := False;

    if ShowOptionsDialog then
    begin
      if not Platform_ShowOptionDialog then Halt;
    end;

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

    Self.WindowHandle := Platform_InitScreen(ScreenWidth,ScreenHeight, Self.Fullscreen , PAnsiChar(Self.Caption) );
    Platform_ShowMouse(MouseVisible);
    Renderer.InitRenderer;
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

procedure TZApplication.UpdateStateVars;
var
  P : TZPointi;
begin
  P := Platform_GetMousePos;

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
  MousePosition[0] := (P.X / ViewportWidth) * 2 - 1;
  //Y-axis is reversed compared to our opengl camera
  MousePosition[1] := (((ViewportHeight-P.Y) / ViewportHeight) * 2 - 1);
end;

function TZApplication.Main : boolean;
{$ifdef minimal}
var
  Remaining : integer;
{$endif}

  {$if not (defined(minimal) or defined(ZgeViz))}
  procedure InDumpDebugInfo;
  var
    I,J : integer;
  begin
    J := 0;
    for I := 0 to Models.Cats.Count - 1 do
      Inc(J,Models.Get(I).Count);
    ZLog.GetLog(Self.ClassName).Write( 'Models: ' + IntToStr(J) + ', strings: ' + ManagedHeap_GetStatus );
  end;
  {$ifend}

begin

  {$ifdef xminimal} //**
  if (not LimitFrameRate) or
     ( (Platform_GetTime>=NextFrameTime) {or (FpsCounter<TargetFrames)} ) then
  {$endif}
  begin
    if FrameRateStyle<>frsFree then
    begin
      if FrameRateStyle=frsFixed then
        TargetFrameRate := Self.FixedFrameRate;
      if (TargetFrameRate<1) or (TargetFrameRate>200) then
        TargetFrameRate := 60;
      NextFrameTime := Platform_GetTime + (1.0 / TargetFrameRate);
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

  PhysicsUpdate(Physics,DeltaTime);

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
  //Reset wheel delta (it is set from platform)
  MouseWheelDelta := 0;
end;

procedure TZApplication.Run;
begin
  Init;
  //Initial tree update
  Self.Update;
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

  glGetFloatv(GL_PROJECTION_MATRIX, @Matrix);
  glGetFloatv(GL_MODELVIEW_MATRIX, @TmpM);
  Matrix := MatrixMultiply(TmpM,Matrix);

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
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity;
    gluPerspective(Self.FOV, Self.ActualViewportRatio, Self.ClipNear, Self.ClipFar);
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity;
  {$ifdef zgeviz}
  end;
  {$endif}

  {$ifdef zgeviz}
  glRotatef( (ZgeVizCameraRotation[2]*360) , 0, 0, 1);
  {$endif}

  //Reverse order to make XYZ-rotation
  glRotatef( (CameraRotation[0]*360) , 1, 0, 0);
  glRotatef( (CameraRotation[1]*360) , 0, 1, 0);
  glRotatef( (CameraRotation[2]*360) , 0, 0, 1);
  //Måste ta negativt på cameraposition för att dess axlar ska bete sig
  //likadant som modell-koordinater (positiv y = uppåt t.ex.)
  glTranslatef(-CameraPosition[0], -CameraPosition[1], -CameraPosition[2]);
end;

procedure TZApplication.UpdateScreen;
var
  I : integer;
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
    if {$ifdef minimal}(ViewportRatio=vprCustom) and {$endif}
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

    glLightfv(GL_LIGHT0,GL_POSITION,@LightPosition);

    Renderer.Render_Begin;
      RenderModels;

      //Render application
      glPushMatrix;
      if Self.OnRender.Count>0 then
        Self.OnRender.ExecuteCommands;
      if Self.CurrentState<>nil then
        Self.CurrentState.OnRender.ExecuteCommands;
      glPopMatrix;
    Renderer.Render_End;
  end;

  //End frame
  glFlush;
  {$ifdef minimal}
  //I designer så ansvarar GL-komponenten för swapbuffer
  Platform_SwapBuffers;
  {$endif}
end;

{$ifndef minimal}
procedure TZApplication.DesignerFreeResources;
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
      Model.DesignerFreeResources;
    end;
  end;
end;

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
begin

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
end;

procedure TZApplication.CompileProperty(C : TZComponent; const Expr : TZPropertyValue; Prop : TZProperty);
var
  CurParent: TZComponent;
  Model : TZComponent;
begin
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

  SymTab.Add('this',C);
  try
    Compiler.Compile(C,Expr.ExpressionValue,SymTab,Prop.ReturnType,ZcGlobalNames);
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
  List.AddProperty({$IFNDEF MINIMAL}'OnLoaded',{$ENDIF}integer(@OnLoaded), zptComponentList);
  List.AddProperty({$IFNDEF MINIMAL}'OnClose',{$ENDIF}integer(@OnClose), zptComponentList);
  List.AddProperty({$IFNDEF MINIMAL}'States',{$ENDIF}integer(@States), zptComponentList);
    {$ifndef minimal}List.GetLast.SetChildClasses([TAppState]);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'OnUpdate',{$ENDIF}integer(@OnUpdate), zptComponentList);
  List.AddProperty({$IFNDEF MINIMAL}'OnRender',{$ENDIF}integer(@OnRender), zptComponentList);
    {$ifndef minimal}{List.GetLast.SetChildClasses([TRenderCommand]);}{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'OnBeginRenderPass',{$ENDIF}integer(@OnBeginRenderPass), zptComponentList);
  List.AddProperty({$IFNDEF MINIMAL}'Content',{$ENDIF}integer(@Content), zptComponentList);
  List.AddProperty({$IFNDEF MINIMAL}'Caption',{$ENDIF}integer(@Caption), zptString);
    List.GetLast.IsStringTarget := True;
  List.AddProperty({$IFNDEF MINIMAL}'DeltaTime',{$ENDIF}integer(@DeltaTime), zptFloat);
    List.GetLast.NeverPersist := True;
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'Time',{$ENDIF}integer(@Time), zptFloat);
    List.GetLast.NeverPersist := True;
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}

  List.AddProperty({$IFNDEF MINIMAL}'FpsCounter',{$ENDIF}integer(@FpsCounter), zptFloat);
    List.GetLast.NeverPersist := True;
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'CurrentRenderPass',{$ENDIF}integer(@CurrentRenderPass), zptInteger);
    List.GetLast.NeverPersist := True;
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'MousePosition',{$ENDIF}integer(@MousePosition), zptVector3f);
    List.GetLast.NeverPersist := True;
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'MouseWheelDelta',{$ENDIF}integer(@MouseWheelDelta), zptInteger);
    List.GetLast.NeverPersist := True;
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'ClearScreenMode',{$ENDIF}integer(@ClearScreenMode), zptInteger);
    List.GetLast.NeverPersist := True;
  List.AddProperty({$IFNDEF MINIMAL}'RenderPasses',{$ENDIF}integer(@RenderPasses), zptInteger);
    List.GetLast.DefaultValue.IntegerValue := 1;
  List.AddProperty({$IFNDEF MINIMAL}'WindowHandle',{$ENDIF}integer(@WindowHandle), zptInteger);
    List.GetLast.NeverPersist := True;
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}

  List.AddProperty({$IFNDEF MINIMAL}'ClearColor',{$ENDIF}integer(@ClearColor), zptColorf);

  List.AddProperty({$IFNDEF MINIMAL}'FullScreen',{$ENDIF}integer(@FullScreen), zptBoolean);

  List.AddProperty({$IFNDEF MINIMAL}'FrameRateStyle',{$ENDIF}integer(@FrameRateStyle), zptByte);
    {$ifndef minimal}List.GetLast.SetOptions(['SyncedWithMonitor','Free','Fixed']);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'FixedFrameRate',{$ENDIF}integer(@FixedFrameRate), zptInteger);

  List.AddProperty({$IFNDEF MINIMAL}'ScreenMode',{$ENDIF}integer(@ScreenMode), zptByte);
    {$ifndef minimal}List.GetLast.SetOptions(['Use Desktop resolution','640x480','800x600','1024x768','1280x800','1280x1024']);{$endif}
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
    List.GetLast.DefaultValue.ByteValue := 2;
  List.AddProperty({$IFNDEF MINIMAL}'ShowOptionsDialog',{$ENDIF}integer(@ShowOptionsDialog), zptBoolean);

  List.AddProperty({$IFNDEF MINIMAL}'CustomScreenWidth',{$ENDIF}integer(@CustomScreenWidth), zptInteger);
  List.AddProperty({$IFNDEF MINIMAL}'CustomScreenHeight',{$ENDIF}integer(@CustomScreenHeight), zptInteger);

  List.AddProperty({$IFNDEF MINIMAL}'CameraPosition',{$ENDIF}integer(@CameraPosition), zptVector3f);
    //Camera default is z 10
    List.GetLast.DefaultValue.Vector3fValue[2] := 10;
  List.AddProperty({$IFNDEF MINIMAL}'CameraRotation',{$ENDIF}integer(@CameraRotation), zptVector3f);

  List.AddProperty({$IFNDEF MINIMAL}'Camera',{$ENDIF}integer(@Camera), zptComponentRef);
    {$ifndef minimal}List.GetLast.SetChildClasses([TCamera]);{$endif}

  List.AddProperty({$IFNDEF MINIMAL}'LightPosition',{$ENDIF}integer(@LightPosition), zptVector3f);
    //Light default is down the Z axis
    List.GetLast.DefaultValue.Vector3fValue[2] := 1;

  List.AddProperty({$IFNDEF MINIMAL}'ViewportRatio',{$ENDIF}integer(@ViewportRatio), zptByte);
    {$ifndef minimal}List.GetLast.SetOptions(['Full window','Custom','4:3','16:9']);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'CustomViewportRatio',{$ENDIF}integer(@CustomViewportRatio), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}'FOV',{$ENDIF}integer(@FOV), zptFloat);
    List.GetLast.DefaultValue.FloatValue := 45;
  List.AddProperty({$IFNDEF MINIMAL}'ClipNear',{$ENDIF}integer(@ClipNear), zptFloat);
    List.GetLast.DefaultValue.FloatValue := 0.1;
  List.AddProperty({$IFNDEF MINIMAL}'ClipFar',{$ENDIF}integer(@ClipFar), zptFloat);
    List.GetLast.DefaultValue.FloatValue := 100;
  List.AddProperty({$IFNDEF MINIMAL}'MouseVisible',{$ENDIF}integer(@MouseVisible), zptBoolean);
  List.AddProperty({$IFNDEF MINIMAL}'EscapeToQuit',{$ENDIF}integer(@EscapeToQuit), zptBoolean);
    List.GetLast.DefaultValue.BooleanValue := True;

  List.AddProperty({$IFNDEF MINIMAL}'ViewportX',{$ENDIF}integer(@ViewportX), zptInteger);
    List.GetLast.NeverPersist := True;
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'ViewportY',{$ENDIF}integer(@ViewportX), zptInteger);
    List.GetLast.NeverPersist := True;
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'ViewportWidth',{$ENDIF}integer(@ViewportWidth), zptInteger);
    List.GetLast.NeverPersist := True;
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'ViewportHeight',{$ENDIF}integer(@ViewportHeight), zptInteger);
    List.GetLast.NeverPersist := True;
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}

  List.AddProperty({$IFNDEF MINIMAL}'ScreenHeight',{$ENDIF}integer(@ScreenHeight), zptInteger);
    List.GetLast.NeverPersist := True;
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
    {$ifndef minimal}List.GetLast.DefaultValue.IntegerValue := 600;{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'ScreenWidth',{$ENDIF}integer(@ScreenWidth), zptInteger);
    List.GetLast.NeverPersist := True;
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
    {$ifndef minimal}List.GetLast.DefaultValue.IntegerValue := 800;{$endif}

  List.AddProperty({$IFNDEF MINIMAL}'ConstantPool',{$ENDIF}integer(@ConstantPool), zptComponentList);
    {$ifndef minimal}List.GetLast.ExcludeFromXml := True;{$endif}
    {$ifndef minimal}List.GetLast.HideInGui := True;{$endif}

  List.AddProperty({$IFNDEF MINIMAL}'NoSound',{$ENDIF}integer(@NoSound), zptBoolean);
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
    {$ifndef minimal}List.GetLast.HideInGui := True;{$endif}

  {$IFNDEF MINIMAL}
  List.AddProperty('Icon',integer(@Icon), zptBinary);
    List.SetDesignerProperty;
  List.AddProperty('PreviewClearColor',integer(@PreviewClearColor), zptColorf);
    List.GetLast.DefaultValue.ColorfValue := MakeColorf(0.5,0.5,0.5,0);
    List.SetDesignerProperty;
  {$ENDIF}
end;

{$ifndef minimal}
procedure TZApplication.DesignerStart(const ViewW,ViewH : integer);
begin
  Self.Init;

  ScreenWidth := ViewW;
  ScreenHeight := ViewH;

  Self.Time := 0;
  Self.DeltaTime := 0;
  Self.LastTime := 0;
  Self.FpsTime := 0;

  //Initial tree update
  Content.Update;
  OnLoaded.ExecuteCommands;
  DesignerIsRunning := True;
end;

procedure TZApplication.DesignerStop;
begin
  DesignerIsRunning := False;
  Models.RemoveAll;
  Models.FlushRemoveList;
  Collisions.ClearAll;
  OnClose.ExecuteCommands;
  Renderer.DesignerRenderStop;
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
  List.AddProperty({$IFNDEF MINIMAL}'ModelUpdatesEnabled',{$ENDIF}integer(@ModelUpdatesEnabled), zptBoolean);
    List.GetLast.DefaultValue.BooleanValue := True;
  List.AddProperty({$IFNDEF MINIMAL}'CollisionsEnabled',{$ENDIF}integer(@CollisionsEnabled), zptBoolean);
    List.GetLast.DefaultValue.BooleanValue := True;
end;


{ TSetAppState }

procedure TSetAppState.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'State',{$ENDIF}integer(@State), zptComponentRef);
    {$ifndef minimal}List.GetLast.SetChildClasses([TAppState]);{$endif}
end;

procedure TSetAppState.Execute;
var
  OldState : TStateBase;
begin
  OldState := ZApp.CurrentState;
  ZApp.CurrentState := State;
  if OldState<>nil then
    OldState.OnLeave.ExecuteCommands;
  if State<>nil then
    State.OnStart.ExecuteCommands;
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

  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  case Self.Kind of
    catPerspective :
      begin
        gluPerspective(Self.FOV, App.ActualViewportRatio, Self.ClipNear, Self.ClipFar);
      end;
  else
    begin
      //Ortho
      {$ifndef minimal}
      if Abs(Self.OrthoZoom)>0.000001 then
      begin //Avoid divide by zero
      {$endif}
      H := (1.0/Self.OrthoZoom);
      W := App.ActualViewportRatio * H;
      glOrtho(-W,W,-H,H,Self.ClipNear, Self.ClipFar);
      {$ifndef minimal}
      end;
      {$endif}
    end;
  end;
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;

  //Reverse order to make XYZ-rotation
  glRotatef( (Rotation[0]*360) , 1, 0, 0);
  glRotatef( (Rotation[1]*360) , 0, 1, 0);
  glRotatef( (Rotation[2]*360) , 0, 0, 1);
  //Måste ta negativt på cameraposition för att dess axlar ska bete sig
  //likadant som modell-koordinater (positiv y = uppåt t.ex.)
  glTranslatef(-Position[0], -Position[1], -Position[2]);
end;

procedure TCamera.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Kind',{$ENDIF}integer(@Kind), zptByte);
    {$ifndef minimal}List.GetLast.SetOptions(['Perspective','Orthographic']);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'Position',{$ENDIF}integer(@Position), zptVector3f);
  List.AddProperty({$IFNDEF MINIMAL}'Rotation',{$ENDIF}integer(@Rotation), zptVector3f);
  List.AddProperty({$IFNDEF MINIMAL}'ClipNear',{$ENDIF}integer(@ClipNear), zptFloat);
    List.GetLast.DefaultValue.FloatValue := 0.1;
  List.AddProperty({$IFNDEF MINIMAL}'ClipFar',{$ENDIF}integer(@ClipFar), zptFloat);
    List.GetLast.DefaultValue.FloatValue := 100;
  List.AddProperty({$IFNDEF MINIMAL}'OrthoZoom',{$ENDIF}integer(@OrthoZoom), zptFloat);
    List.GetLast.DefaultValue.FloatValue := 1.0;
  List.AddProperty({$IFNDEF MINIMAL}'FOV',{$ENDIF}integer(@FOV), zptFloat);
    List.GetLast.DefaultValue.FloatValue := 45;
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
  ZClasses.Register(TCamera,CameraClassId);
    {$ifndef minimal}ComponentManager.LastAdded.AutoName := True;{$endif}

end.