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

unit Animators;

interface

uses ZClasses,Meshes;

type

{     TAnimatorBase
       Start()
         Reset()
         Active=true
       Reset()
         Time=0
       Update
         if Active then UpdateWithTimeStep(app.DeltaTime)
       UpdateWithTimeStep(dtime)
         kan anropas direkt av sound
         Time+=dtime
         DoAnimate()
       DoAnimate() virtual abstract
       private
       Time
       Active  true medans den är igång
}
  TAnimatorBase = class(TZComponent)
  protected
    Time,LocalTime : single;
    Active,HasFinished : boolean;
    procedure Reset;
    procedure DoAnimate; virtual;
    procedure DefineProperties(List: TZPropertyList); override;
    function SetLocalTime : boolean; virtual;
  public
    BeginTime : single;
    Duration : single;
    AutoStart : boolean;
    procedure UpdateWithTimeStep(DeltaTime : single); virtual;
    procedure Update; override;
    procedure Start; virtual;
    procedure Stop; virtual;
    {$ifndef minimal}
    procedure DesignerReset; override;
    {$endif}
  end;

  TAnimatorGroup = class(TAnimatorBase)
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Animators : TZComponentList;
    OnStart : TZComponentList;
    OnStop : TZComponentList;
    procedure UpdateWithTimeStep(DeltaTime: Single); override;
    procedure Start; override;
    procedure Stop; override;
  end;
{     TAnimatorGroup(TAnimatorBase)
       Animators
         kan innehålla tanimator och animatorgroups
       anropar animators.update när grupp är aktiv
       start()
         anropar reset på varje animator
       UpdateWithTimeStep(dtime)
         inherited, loop children c.UpdateWithTimeStep}

  TAnimatorWithTargetBase = class(TAnimatorBase)
  private
    IsGoingForward : boolean;
    CurrentIteration : integer;
    LastBeginTime : single;
    TargetPropPtr : pointer;
  protected
    procedure DefineProperties(List: TZPropertyList); override;
    function SetLocalTime: Boolean; override;
  public
    AutoReverse : boolean;
    RepeatCount : integer;
    Target : TZPropertyRef;
    procedure Start; override;
    {$ifndef minimal}
    function GetDisplayName: AnsiString; override;
    {$endif}
  end;

  TAnimatorSimple = class(TAnimatorWithTargetBase)
  strict private
    InternalFromValue : single;
  protected
    procedure DefineProperties(List: TZPropertyList); override;
    procedure DoAnimate; override;
  public
    FromKind : (frkUseFromValue,frkUseTargetValue);
    FromValue : single;
    ToValue : single;
    Smooth : boolean;
    procedure Start; override;
  end;

  TMouseModelController = class(TCommand)
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    ScaleX,ScaleY : single;
    ControlX,ControlY : boolean;
    procedure Execute; override;
  end;

  TStartAnimator = class(TCommand)
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Animator : TAnimatorBase;
    procedure Execute; override;
    {$ifndef minimal}function GetDisplayName: AnsiString; override;{$endif}
  end;

  TCurvePoint = class(TZComponent)
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Frame,Value : single;
    InX,InY,OutX,OutY : single;
  end;

  TCurve = class(TZComponent)
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Points : TZComponentList;
    function GetValueAt(const T : single) : single;
  end;

  TAnimatorCurve = class(TAnimatorWithTargetBase)
  protected
    procedure DefineProperties(List: TZPropertyList); override;
    procedure DoAnimate; override;
  public
    Curve : TCurve;
  end;

implementation

uses ZPlatform, ZApplication, ZMath, ZExpressions;


{ TAnimatorBase }

//Calc "LocalTime", which is Time with regard to BeginTime and Duration
function TAnimatorBase.SetLocalTime : boolean;
var
  X : single;
begin
  Result := False;
  X := Time - BeginTime;
  if X<0 then
    Exit;  //not yet started
  if (Duration<>0) and (X>Duration)  then
  begin
    Stop;
    Exit;
  end;
  LocalTime := X;
  Result := True;
end;

procedure TAnimatorBase.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Duration',{$ENDIF}(@Duration), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}'BeginTime',{$ENDIF}(@BeginTime), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}'AutoStart',{$ENDIF}(@AutoStart), zptBoolean);
end;

{$ifndef minimal}
procedure TAnimatorBase.DesignerReset;
begin
  inherited;
  Self.Reset;
end;
{$endif}

procedure TAnimatorBase.DoAnimate;
begin
  //baseclass do nothing here
end;

procedure TAnimatorBase.Reset;
begin
  HasFinished := False;
  Time := 0;
end;

procedure TAnimatorBase.Start;
begin
  Reset;
  Active := True;
end;

procedure TAnimatorBase.Stop;
begin
  Active := False;
  HasFinished := True;
end;

procedure TAnimatorBase.Update;
begin
  UpdateWithTimeStep(ZApp.DeltaTime)
end;

procedure TAnimatorBase.UpdateWithTimeStep(DeltaTime: single);
begin
  if Active then
  begin
    Time := Time + DeltaTime;
    if SetLocalTime then
      DoAnimate;
  end
  else if AutoStart and not HasFinished then
    Start;
end;

{ TMouseModelController }

procedure TMouseModelController.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'ScaleX',{$ENDIF}(@ScaleX), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}'ScaleY',{$ENDIF}(@ScaleY), zptFloat);
end;

procedure TMouseModelController.Execute;
const
  Speed = 10;
var
  Value,Diff : single;
begin
  {$ifndef minimal}if (CurrentModel=nil) then Exit;{$endif}

  //-1 .. 1, 0 is center
  Value := ZApp.MousePosition[0] * ScaleX;
  Diff := (Value - CurrentModel.Position[0]);
  //Velocity is how far to move per second
  //Multiply by constant to reach target faster
  CurrentModel.Velocity[0] := Diff * Speed;

  //Y-axis is reversed compared to OpenGL
  Value := ZApp.MousePosition[1] * ScaleY;
  Diff := (Value - CurrentModel.Position[1]);
  CurrentModel.Velocity[1] := Diff * Speed;
end;

{ TAnimatorGroup }

procedure TAnimatorGroup.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Animators',{$ENDIF}(@Animators), zptComponentList);
    {$ifndef minimal}List.GetLast.SetChildClasses([TAnimatorBase]);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'OnStart',{$ENDIF}(@OnStart), zptComponentList);
  List.AddProperty({$IFNDEF MINIMAL}'OnStop',{$ENDIF}(@OnStop), zptComponentList);
end;

procedure TAnimatorGroup.Start;
var
  I : integer;
begin
  inherited;
  OnStart.ExecuteCommands;
  for I := 0 to Animators.Count-1 do
    TAnimatorBase(Animators[I]).Start;
end;

procedure TAnimatorGroup.Stop;
var
  I : integer;
begin
  inherited;
  for I := 0 to Animators.Count-1 do
    TAnimatorBase(Animators[I]).Stop;
  OnStop.ExecuteCommands;
end;

procedure TAnimatorGroup.UpdateWithTimeStep(DeltaTime: Single);
var
  I : integer;
  A : TAnimatorBase;
  AllFinished : boolean;
begin
  inherited;
  if Active then
  begin
    AllFinished := Animators.Count>0;
    for I := 0 to Animators.Count-1 do
    begin
      A := TAnimatorBase(Animators[I]);
      A.UpdateWithTimeStep(DeltaTime);
      if not A.HasFinished then
        AllFinished := False;
    end;
    if (Duration=0) and AllFinished then
      //When no duration set, automatically stop when all children stopped
      Stop;
  end;
end;

{ TAnimatorSimple }

procedure TAnimatorSimple.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'FromValue',{$ENDIF}(@FromValue), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}'FromKind',{$ENDIF}(@FromKind), zptByte);
    {$ifndef minimal}List.GetLast.SetOptions(['UseFromValue','UseTargetValue']);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'ToValue',{$ENDIF}(@ToValue), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}'Smooth',{$ENDIF}(@Smooth), zptBoolean);
end;


procedure TAnimatorSimple.DoAnimate;
var
  X : single;
begin
  {$ifndef minimal}if Target.Component=nil then exit;{$endif}
  X:=LocalTime/Duration;  //0..1 in animation

  if Smooth then
    if X>=0.5 then
      X := 1 - SmoothStep(0,1,1-X)
    else
      X := SmoothStep(0,1,X);
  if not IsGoingForward then
    X := 1-X;

  PFloat(TargetPropPtr)^ := InternalFromValue + ( (ToValue-InternalFromValue) * X );
end;

procedure TAnimatorSimple.Start;
begin
  inherited;
  case FromKind of
    frkUseFromValue :
      begin
        InternalFromValue := FromValue;
        PFloat(TargetPropPtr)^ := FromValue;
      end;
    frkUseTargetValue :
      InternalFromValue := PFloat(TargetPropPtr)^;
  end;
end;

{ TAnimatorWithTargetBase }

procedure TAnimatorWithTargetBase.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Target',{$ENDIF}(@Target), zptPropertyRef);
    {$ifndef minimal}List.GetLast.NeedRefreshNodeName := True;{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'AutoReverse',{$ENDIF}(@AutoReverse), zptBoolean);
  List.AddProperty({$IFNDEF MINIMAL}'RepeatCount',{$ENDIF}(@RepeatCount), zptInteger);
end;

{$ifndef minimal}
function TAnimatorWithTargetBase.GetDisplayName: AnsiString;
begin
  Result := inherited GetDisplayName;
  if Assigned(Target.Component) then
    Result := Result + '  ' + AnsiString(ZClasses.GetPropRefAsString(Target));
end;
{$endif}

procedure TAnimatorWithTargetBase.Start;
begin
  inherited;
  {$ifndef minimal}if Target.Component=nil then exit;{$endif}
  TargetPropPtr := Target.Component.GetPropertyPtr(Target.Prop,Target.Index);
  IsGoingForward := True;
  LastBeginTime := BeginTime;
  CurrentIteration := 0;
end;

//Override localtime for addition of Repeat and AutoReverse behaviour
function TAnimatorWithTargetBase.SetLocalTime: Boolean;
var
  X : single;
begin
  Result := False;
  X := Time - LastBeginTime;
  if X<0 then
    Exit;  //not yet started

  if (Duration<>0) and (X>Duration) then
  begin
    if IsGoingForward and AutoReverse then
    begin
      //Switch direction
      IsGoingForward := False;
      LastBeginTime := Time;
      X := Time - LastBeginTime;
    end else
    begin
      Inc(CurrentIteration);
      if (CurrentIteration>RepeatCount) and (RepeatCount<>-1) then
        //End of animation, set x=1 to guarantee write final value
        Stop
      else
      begin
        //Next iteration
        IsGoingForward := True;
        LastBeginTime := Time;
        X := Time - LastBeginTime;
     end;
    end;
  end;

  Result := True;

  LocalTime := X;
end;


{ TStartAnimator }

procedure TStartAnimator.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Animator',{$ENDIF}(@Animator), zptComponentRef);
    {$ifndef minimal}List.GetLast.SetChildClasses([TAnimatorBase]);{$endif}
end;

procedure TStartAnimator.Execute;
begin
  {$ifndef minimal}if Animator=nil then Exit;{$endif}
  Animator.Start;
end;

{$ifndef minimal}
function TStartAnimator.GetDisplayName: AnsiString;
begin
  Result := inherited GetDisplayName;
  if Assigned(Animator) then
    Result := Result + '  ' + Animator.Name;
end;
{$endif}


{ TCurvePoint }

procedure TCurvePoint.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Frame',{$ENDIF}(@Frame), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}'Value',{$ENDIF}(@Value), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}'InX',{$ENDIF}(@InX), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}'InY',{$ENDIF}(@InY), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}'OutX',{$ENDIF}(@OutX), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}'OutY',{$ENDIF}(@OutY), zptFloat);
end;

{ TCurve }

procedure TCurve.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Points',{$ENDIF}(@Points), zptComponentList);
    {$ifndef minimal}List.GetLast.SetChildClasses([TCurvePoint]);{$endif}
end;

function TCurve.GetValueAt(const T: single): single;
var
  X1, Y1, X2, Y2, X3, Y3, X4, Y4, A, B, X, Y : single;
  Key : integer;
  P1,P2 : TCurvePoint;
begin
  Key := 0;
  while (Key<Points.Count) and ( TCurvePoint(Points[Key]).Frame<T ) do
    Inc(Key);

  if (Key=0) or (Key=Points.Count) then
    Exit( 0 );

  P1 := TCurvePoint(Points[Key-1]);
  P2 := TCurvePoint(Points[Key]);

  X1 := P1.Frame;
  Y1 := P1.Value;

  X4 := P2.Frame;
  Y4 := P2.Value;

  X2 := P1.OutX + X1;
  Y2 := P1.OutY + Y1;

  X3 := P2.InX + X4;
  Y3 := P2.InY + Y4;

  B := (T-X1)/(X4-X1);
  A := 1-B;

  X := X1*A*A*A+X2*3*A*A*B+X3*3*A*B*B+X4*B*B*B;

  B := (X-X1)/(X4-X1);
  A := 1-B;

  Y := Y1*A*A*A+Y2*3*A*A*B+Y3*3*A*B*B+Y4*B*B*B;

  Result := Y;
end;

{ TAnimatorCurve }

procedure TAnimatorCurve.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Curve',{$ENDIF}(@Curve), zptComponentRef);
    {$ifndef minimal}List.GetLast.SetChildClasses([TCurve]);{$endif}
end;

procedure TAnimatorCurve.DoAnimate;
var
  T : single;
begin
  {$ifndef minimal}if (Target.Component=nil) or (Curve=nil) then exit;{$endif}

  T := LocalTime;
  if not IsGoingForward then
    T := Duration-T;

  PFloat(TargetPropPtr)^ := Curve.GetValueAt(T);
end;

initialization

  ZClasses.Register(TAnimatorGroup,AnimatorGroupClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NeedParentList := 'OnUpdate';{$endif}
    {$ifndef minimal}ComponentManager.LastAdded.ImageIndex:=23;{$endif}
  ZClasses.Register(TAnimatorSimple,AnimatorSimpleClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NeedParentList := 'OnUpdate';{$endif}
    {$ifndef minimal}ComponentManager.LastAdded.ImageIndex:=24;{$endif}

  ZClasses.Register(TMouseModelController,MouseModelControllerClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NeedParentList := 'OnUpdate';{$endif}
    {$ifndef minimal}ComponentManager.LastAdded.ImageIndex:=43;{$endif}
  ZClasses.Register(TStartAnimator,StartAnimatorClassId);
    {$ifndef minimal}ComponentManager.LastAdded.ImageIndex := 59;{$endif}

//Remove until we know how to implement correctly
//  ZClasses.Register(TCurvePoint,CurvePointClassId);
//  ZClasses.Register(TCurve,CurveClassId);
//  ZClasses.Register(TAnimatorCurve,AnimatorCurveClassId);

end.
