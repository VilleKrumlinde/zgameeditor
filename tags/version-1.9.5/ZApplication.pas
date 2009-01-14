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

uses ZClasses,Meshes,Collision,Commands,Renderer{Tfont},AudioComponents;

type

  TSliceCallback = procedure of object;
  TClock = class
  protected
    LastTime : single;
    procedure Slice(Callback : TSliceCallback);
  public
    Time : single;    //1.0 = 1 second
    AccumulatedTime : single;
    DeltaTime : single;
    FrameLoss : boolean;  //sätts till true ifall vi tappar frames
    procedure UpdateTime;
  end;

  //Application states
  TAppState = class(TStateBase)
  private
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    ModelUpdatesEnabled : boolean;
    CollisionsEnabled : boolean;
  end;

  TZApplication = class(TZComponent)
  private
    FpsFrames : integer;
    FpsCounter,FpsTime : single;
    CurrentState : TAppState;
    HasShutdown : boolean;
    TargetFrameRate : integer;
    NextFrameTime : single;
    ViewportX,ViewportY,ViewportWidth,ViewportHeight : integer;
    procedure Init;
    procedure Shutdown;
    procedure MainSlice;
    procedure UpdateScreen;
    procedure UpdateViewport;
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
    OnUpdate : TZComponentList;
    OnRender : TZComponentList;
    Terminating : boolean;
    Time,DeltaTime : single;
    Font : TFont;
    CurrentMusic : TMusic;
    Caption : TPropString;
    EventState : //Global variables reachable from zc event-code
      record
        CollidedCategory : single;  //should be int
        MousePosition : TZVector3f;
        ClearScreenMode : single;  //should be int
      end;
    ClearColor : TZColorf;
    Fullscreen : boolean;
    ScreenMode : (vm640x480,vm800x600,vm1024x768,vm1280x800,vm1280x1024);
    ShowOptionsDialog : boolean;
    CustomScreenWidth,CustomScreenHeight : integer;
    CameraPosition : TZVector3f;
    CameraRotation : TZVector3f;
    LightPosition : TZVector4f;
    CustomViewportRatio,FOV,ClipNear,ClipFar : single;
    ViewportRatio : (vprFullWindow,vprCustom,vpr4_3,vpr16_9);
    ActualViewportRatio : single;
    Clock : TClock;
    FrameRateStyle : (frsSyncedWithMonitor,frsFree,frsFixed);
    FixedFrameRate : integer;
    MouseVisible : boolean;
    {$ifndef minimal}
    Icon : TZBinaryPropValue;
    PreviewClearColor : TZColorf;
    {$endif}
    constructor Create(OwnerList: TZComponentList); override;
    destructor Destroy; override;
    procedure Run;
    procedure Terminate;
    procedure AddModel(Model : TModel);
    procedure Update; override;
    {$ifndef minimal}
    procedure DesignerStart(const ViewW,ViewH : integer);
    procedure DesignerStop;
    {$endif}
  end;


  TSetAppState = class(TCommand)
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    State : TAppState;
    procedure Execute; override;
    {$ifndef minimal}
    function GetDisplayName: string; override;
    {$endif}
  end;


function LoadApplicationComponent : TZApplication;


const
  TicksPerSecond = 1000;

var
  ZApp : TZApplication;
  ScreenWidth : integer{$ifndef minimal}=800{$endif};
  ScreenHeight : integer{$ifndef minimal}=600{$endif};
  NoSound : boolean;


implementation

uses ZPlatform,ZOpenGL,ZLog,AudioPlayer
  {$ifndef minimal}
  ,SysUtils
  {$endif}
  ;

{ TZApplication }

constructor TZApplication.Create(OwnerList: TZComponentList);
begin
  inherited;
  //todo: font ska kanske skapas vid laddning? inline-komponent?
  Font := TFont.Create(nil);
  Clock := TClock.Create;

  Models := TModels.Create;

  Collisions := TCollisionChecks.Create(Models);
end;

destructor TZApplication.Destroy;
begin
  Terminating := True;
  Clock.Free;

  //ev problem med models.free, borde göra models.removeall vid terminate först
  //annars kan models referera modeller som redan har gjorts free på
  Models.Free;

  Collisions.Free;
  Font.Free;
  if not HasShutdown then
    Shutdown;
  inherited;
end;

procedure TZApplication.Init;
{$ifdef minimal}
var
  I : integer;
{$endif}
begin
  Platform_InitGlobals;  //Nollställ timer etc

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

    Platform_InitScreen(ScreenWidth,ScreenHeight, Self.Fullscreen , PChar(Self.Caption) );
    Platform_ShowMouse(MouseVisible);
    Renderer.InitRenderer;
    UpdateViewport;

    case FrameRateStyle of
      frsSyncedWithMonitor: TargetFrameRate := Platform_GetDisplayRefreshRate;
      frsFree: ;
      frsFixed: TargetFrameRate := Self.FixedFrameRate;
    end;
    if (TargetFrameRate<10) or (TargetFrameRate>200) then
      TargetFrameRate := 60;

    NoSound := Platform_CommandLine('s');
    if not NoSound then
      Platform_InitAudio;
  {$endif}
end;

procedure TZApplication.Shutdown;
begin
  {$ifdef minimal}
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
begin
  Clock.UpdateTime;
  //Copy deltatime so it can be read as a property from zexpressions
  DeltaTime := Clock.DeltaTime;
  Time := Clock.Time;
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
  EventState.MousePosition[0] := (P.X / ViewportWidth) * 2 - 1;
  //Y-axis is reversed compared to our opengl camera
  EventState.MousePosition[1] := (((ViewportHeight-P.Y) / ViewportHeight) * 2 - 1);
end;

function TZApplication.Main : boolean;
{$ifdef minimal}
var
  Remaining : integer;
{$endif}

  {$ifndef minimal}
  procedure InDumpDebugInfo;
  var
    I,J : integer;
  begin
    J := 0;
    for I := 0 to Models.Cats.Count - 1 do
      Inc(J,Models.Get(I).Count);
    ZLog.GetLog(Self.ClassName).Write( 'Models: ' + IntToStr(J) );
  end;
 {$endif}
begin

  {$ifdef xminimal} //**
  if (not LimitFrameRate) or
     ( (Platform_GetTime>=NextFrameTime) {or (FpsCounter<TargetFrames)} ) then
  {$endif}
  begin
    if FrameRateStyle<>frsFree then
      NextFrameTime := Platform_GetTime + (1.0 / TargetFrameRate);

    UpdateTime;

    UpdateStateVars;

    //Dela upp tiden.
    //MainSlice anropas för varje movementfactor 1, detta så att collision sker
    //för varje enskilt movement.
    Clock.Slice(Self.MainSlice);

    //Draw all
    UpdateScreen;

    Inc(FpsFrames);
    if (Time-FpsTime)>1.0 then
    begin
      FpsCounter := FpsFrames / (Time-FpsTime);
      FpsFrames := 0;
      FpsTime := Time;
      {$ifndef minimal}
      InDumpDebugInfo;
      {$endif}
    end;

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
end;

procedure TZApplication.Run;
begin
  Init;
  //Initial tree update
  Self.Update;
  OnLoaded.ExecuteCommands;
  Platform_Run(Self.Main);
end;

procedure TZApplication.Terminate;
begin
  Terminating := True;
  Shutdown;
end;

//Update ActualViewportRatio which is the ratio that is used
procedure TZApplication.UpdateViewport;
begin
  if Self.ViewportRatio=vprFullWindow then
  begin
    ActualViewportRatio := ScreenWidth/ScreenHeight;
    glViewport(0,0,ScreenWidth,ScreenHeight);
    ViewportWidth := ScreenWidth;
    ViewportHeight := ScreenHeight;
    ViewportX := 0;
    ViewportY := 0;
  end
  else
  begin
    case Self.ViewportRatio of
      vprCustom : ActualViewportRatio := Self.CustomViewportRatio;
      vpr4_3 : ActualViewportRatio := 4/3;
      vpr16_9 : ActualViewportRatio := 16/9;
    {$ifndef zminimal}
    else
      ;
    {$endif}
    end;
    if ScreenWidth/ScreenHeight > ActualViewportRatio then
    begin
      ViewportWidth := Trunc(ScreenHeight*ActualViewportRatio);
      ViewportHeight := ScreenHeight;
      ViewportX := Trunc((ScreenWidth-ScreenHeight*ActualViewportRatio)*0.5);
      ViewportY := 0;
    end
    else
    begin
      ViewportWidth := ScreenWidth;
      ViewportHeight := Trunc(ScreenWidth/ActualViewportRatio);
      ViewportX := 0;
      ViewportY := Trunc((ScreenHeight-ScreenWidth/ActualViewportRatio)*0.5);
    end;
    glViewport(ViewportX, ViewportY, ViewportWidth, ViewportHeight);
  end;
end;


procedure TZApplication.UpdateScreen;
var
  I,J : integer;
  Model : TModel;
  List : TZArrayList;
begin
  {$ifdef zminimal}
  //always update in designer because user may have changed the ViewportRatio dropdown
  if ViewportRation=vprCustom then
  {$endif}
     UpdateViewport;

  //Setup view and camera
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;

  //we'll use a perspective matrix to view our scene
  gluPerspective(Self.FOV, ActualViewportRatio , Self.ClipNear, Self.ClipFar);
//  glOrtho(-1,1,-1,1,-100,100);
//  glFrustum(-1, 1, -1, 1, 9, 100);

  //Använd sedan property CameraPosition
  //Måste ta negativt på cameraposition för att dess axlar ska bete sig
  //likadant som modell-koordinater (positiv y = uppåt t.ex.)
  ApplyRotation(CameraRotation);
  glTranslatef(-CameraPosition[0], -CameraPosition[1], -CameraPosition[2]);

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;

  glClearColor(ClearColor.V[0],ClearColor.V[1],ClearColor.V[2],0);

  if EventState.ClearScreenMode=0 then
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  glLightfv(GL_LIGHT0,GL_POSITION,@LightPosition);

  Renderer.Render_Begin;

  //Render models
  for I := 0 to Models.Cats.Count-1 do
  begin
    List := Models.Get(I);
    for J := 0 to List.Count-1 do
    begin
      Model := TModel(List[J]);
      Meshes.CurrentModel := Model;
      Renderer.RenderModel(Model);
    end;
  end;
  Meshes.CurrentModel := nil;

  //Render application
  glPushMatrix;
  if Self.OnRender.Count>0 then
    Self.OnRender.ExecuteCommands;
  if Self.CurrentState<>nil then
    Self.CurrentState.OnRender.ExecuteCommands;
  glPopMatrix;

  Renderer.Render_End;

  //End frame
  glFlush;
  {$ifdef minimal}
  //I designer så ansvarar GL-komponenten för swapbuffer
  Platform_SwapBuffers;
  {$endif}
end;

procedure TZApplication.AddModel(Model: TModel);
begin
  if Models.Cats.Count<=Model.Category then
    Models.RegisterCat(Model.Category);
  Models.Add(Model);
end;

procedure TZApplication.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'States',{$ENDIF}integer(@States) - integer(Self), zptComponentList);
    {$ifndef minimal}List.GetLast.SetChildClasses([TAppState]);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'OnLoaded',{$ENDIF}integer(@OnLoaded) - integer(Self), zptComponentList);
  List.AddProperty({$IFNDEF MINIMAL}'OnUpdate',{$ENDIF}integer(@OnUpdate) - integer(Self), zptComponentList);
  List.AddProperty({$IFNDEF MINIMAL}'OnRender',{$ENDIF}integer(@OnRender) - integer(Self), zptComponentList);
    {$ifndef minimal}List.GetLast.SetChildClasses([TRenderCommand]);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'Content',{$ENDIF}integer(@Content) - integer(Self), zptComponentList);
  List.AddProperty({$IFNDEF MINIMAL}'Caption',{$ENDIF}integer(@Caption) - integer(Self), zptString);
  List.AddProperty({$IFNDEF MINIMAL}'DeltaTime',{$ENDIF}integer(@DeltaTime) - integer(Self), zptFloat);
    List.GetLast.NeverPersist := True;
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'Time',{$ENDIF}integer(@Time) - integer(Self), zptFloat);
    List.GetLast.NeverPersist := True;
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}

  List.AddProperty({$IFNDEF MINIMAL}'FpsCounter',{$ENDIF}integer(@FpsCounter) - integer(Self), zptFloat);
    List.GetLast.NeverPersist := True;
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'CollidedCategory',{$ENDIF}integer(@EventState.CollidedCategory) - integer(Self), zptFloat);
    List.GetLast.NeverPersist := True;
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'MousePosition',{$ENDIF}integer(@EventState.MousePosition) - integer(Self), zptVector3f);
    List.GetLast.NeverPersist := True;
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'ClearScreenMode',{$ENDIF}integer(@EventState.ClearScreenMode) - integer(Self), zptFloat);
    List.GetLast.NeverPersist := True;

  List.AddProperty({$IFNDEF MINIMAL}'ClearColor',{$ENDIF}integer(@ClearColor) - integer(Self), zptColorf);

  List.AddProperty({$IFNDEF MINIMAL}'FullScreen',{$ENDIF}integer(@FullScreen) - integer(Self), zptBoolean);

  List.AddProperty({$IFNDEF MINIMAL}'FrameRateStyle',{$ENDIF}integer(@FrameRateStyle) - integer(Self), zptByte);
    {$ifndef minimal}List.GetLast.SetOptions(['SyncedWithMonitor','Free','Fixed']);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'FixedFrameRate',{$ENDIF}integer(@FixedFrameRate) - integer(Self), zptInteger);

  List.AddProperty({$IFNDEF MINIMAL}'ScreenMode',{$ENDIF}integer(@ScreenMode) - integer(Self), zptByte);
    {$ifndef minimal}List.GetLast.SetOptions(['640x480','800x600','1024x768','1280x800','1280x1024']);{$endif}
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
    List.GetLast.DefaultValue.ByteValue := 1;
  List.AddProperty({$IFNDEF MINIMAL}'ShowOptionsDialog',{$ENDIF}integer(@ShowOptionsDialog) - integer(Self), zptBoolean);

  List.AddProperty({$IFNDEF MINIMAL}'CustomScreenWidth',{$ENDIF}integer(@CustomScreenWidth) - integer(Self), zptInteger);
  List.AddProperty({$IFNDEF MINIMAL}'CustomScreenHeight',{$ENDIF}integer(@CustomScreenHeight) - integer(Self), zptInteger);

  List.AddProperty({$IFNDEF MINIMAL}'CameraPosition',{$ENDIF}integer(@CameraPosition) - integer(Self), zptVector3f);
    //Camera default is z 10
    List.GetLast.DefaultValue.Vector3fValue[2] := 10;
  List.AddProperty({$IFNDEF MINIMAL}'CameraRotation',{$ENDIF}integer(@CameraRotation) - integer(Self), zptVector3f);
  List.AddProperty({$IFNDEF MINIMAL}'LightPosition',{$ENDIF}integer(@LightPosition) - integer(Self), zptVector3f);
    //Light default is down the Z axis
    List.GetLast.DefaultValue.Vector3fValue[2] := 1;

  List.AddProperty({$IFNDEF MINIMAL}'ViewportRatio',{$ENDIF}integer(@ViewportRatio) - integer(Self), zptByte);
    {$ifndef minimal}List.GetLast.SetOptions(['Full window','Custom','4:3','16:9']);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'CustomViewportRatio',{$ENDIF}integer(@CustomViewportRatio) - integer(Self), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}'FOV',{$ENDIF}integer(@FOV) - integer(Self), zptFloat);
    List.GetLast.DefaultValue.FloatValue := 45;
  List.AddProperty({$IFNDEF MINIMAL}'ClipNear',{$ENDIF}integer(@ClipNear) - integer(Self), zptFloat);
    List.GetLast.DefaultValue.FloatValue := 0.1;
  List.AddProperty({$IFNDEF MINIMAL}'ClipFar',{$ENDIF}integer(@ClipFar) - integer(Self), zptFloat);
    List.GetLast.DefaultValue.FloatValue := 100;
  List.AddProperty({$IFNDEF MINIMAL}'MouseVisible',{$ENDIF}integer(@MouseVisible) - integer(Self), zptBoolean);

  {$IFNDEF MINIMAL}
  List.AddProperty('Icon',integer(@Icon) - integer(Self), zptBinary);
    List.SetDesignerProperty;
  List.AddProperty('PreviewClearColor',integer(@PreviewClearColor) - integer(Self), zptColorf);
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
  Self.Clock.LastTime := 0;

  //Uppdatera träd initialt
  Content.Update;
  OnLoaded.ExecuteCommands;
end;

procedure TZApplication.DesignerStop;
begin
  Models.RemoveAll;
  Models.FlushRemoveList;
  Collisions.ClearAll
end;
{$endif}

{ TClock }

procedure TClock.Slice(Callback : TSliceCallback);
begin
  //Skip slicing now, detlatime is maxed out directly in updatetime instead
  Callback;
end;

procedure TClock.UpdateTime;
const
  //Maximum time step for update and collision=1/10 second
  MaxUpdateStep = 1.0 / 10;
begin
  Time := Platform_GetTime;
  DeltaTime := Time - LastTime;

  //Avoid too high steps, for instance when grabbing win-caption with mouse
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

  AccumulatedTime := Time;
end;

/////////////////////////////

function LoadApplicationComponent : TZApplication;
begin
  Result := TZApplication(ComponentManager.LoadBinary);
end;


{ TAppState }

procedure TAppState.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'ModelUpdatesEnabled',{$ENDIF}integer(@ModelUpdatesEnabled) - integer(Self), zptBoolean);
    List.GetLast.DefaultValue.BooleanValue := True;
  List.AddProperty({$IFNDEF MINIMAL}'CollisionsEnabled',{$ENDIF}integer(@CollisionsEnabled) - integer(Self), zptBoolean);
    List.GetLast.DefaultValue.BooleanValue := True;
end;


{ TSetAppState }

procedure TSetAppState.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'State',{$ENDIF}integer(@State) - integer(Self), zptComponentRef);
    {$ifndef minimal}List.GetLast.SetChildClasses([TAppState]);{$endif}
end;

procedure TSetAppState.Execute;
var
  OldState : TStateBase;
begin
  {$ifndef minimal}if state=nil then exit;{$endif}
  OldState := ZApp.CurrentState;
  ZApp.CurrentState := State;
  if OldState<>nil then
    OldState.OnLeave.ExecuteCommands;
  State.OnStart.ExecuteCommands;
end;

{$ifndef minimal}
function TSetAppState.GetDisplayName: string;
begin
  Result := inherited GetDisplayName;
  if Assigned(State) then
    Result := Result + '  ' + State.Name;
end;
{$endif}

initialization

  ZClasses.Register(TZApplication,ApplicationClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NoUserCreate:=True;{$endif}
    {$ifndef minimal}ComponentManager.LastAdded.ImageIndex:=6;{$endif}

  ZClasses.Register(TAppState,AppStateClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NoTopLevelCreate:=True;{$endif}
    {$ifndef minimal}ComponentManager.LastAdded.AutoName:=True;{$endif}
  ZClasses.Register(TSetAppState,SetAppStateClassId);

end.