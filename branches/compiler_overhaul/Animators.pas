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
    OnStop : TZComponentList;   //Körs när group har kört färdigt
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
  protected
    TargetPropPtr : pointer;
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Target : TZPropertyRef;
    procedure Start; override;
    {$ifndef minimal}
    function GetDisplayName: AnsiString; override;
    {$endif}
  end;

  TAnimatorSimple = class(TAnimatorWithTargetBase)
  private
    LastBeginTime,InternalFromValue : single;
    IsGoingForward : boolean;
    CurrentIteration : integer;
  protected
    procedure DefineProperties(List: TZPropertyList); override;
    procedure DoAnimate; override;
    function SetLocalTime: Boolean; override;
  public
    FromKind : (frkUseFromValue,frkUseTargetValue);
    FromValue : single;
    ToValue : single;
    Smooth : boolean;
    AutoReverse : boolean;
    RepeatCount : integer;
    procedure Start; override;
    procedure Stop; override;
  end;
{
     TAnimatorSimple
       Target
       FromValue ToValue
       BeginTime
       Duration
       DoAnimate
         x=time-begintime
         if x<0 exit  //not yet started
         if x>duration active=false exit
         x=x/duration  //0..1 in animation
         target=fromvalue + ( (tovalue-fromvalue) * x );
       Reset
         inherited
         target = fromvalue
}

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
  List.AddProperty({$IFNDEF MINIMAL}'Duration',{$ENDIF}integer(@Duration), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}'BeginTime',{$ENDIF}integer(@BeginTime), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}'AutoStart',{$ENDIF}integer(@AutoStart), zptBoolean);
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
  UpdateWithTimeStep(ZApp.Clock.DeltaTime)
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
  List.AddProperty({$IFNDEF MINIMAL}'ScaleX',{$ENDIF}integer(@ScaleX), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}'ScaleY',{$ENDIF}integer(@ScaleY), zptFloat);
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
  List.AddProperty({$IFNDEF MINIMAL}'Animators',{$ENDIF}integer(@Animators), zptComponentList);
    {$ifndef minimal}List.GetLast.SetChildClasses([TAnimatorBase]);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'OnStop',{$ENDIF}integer(@OnStop), zptComponentList);
end;

procedure TAnimatorGroup.Start;
var
  I : integer;
begin
  inherited;
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
  List.AddProperty({$IFNDEF MINIMAL}'FromValue',{$ENDIF}integer(@FromValue), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}'FromKind',{$ENDIF}integer(@FromKind), zptByte);
    {$ifndef minimal}List.GetLast.SetOptions(['UseFromValue','UseTargetValue']);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'ToValue',{$ENDIF}integer(@ToValue), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}'Smooth',{$ENDIF}integer(@Smooth), zptBoolean);
  List.AddProperty({$IFNDEF MINIMAL}'AutoReverse',{$ENDIF}integer(@AutoReverse), zptBoolean);
  List.AddProperty({$IFNDEF MINIMAL}'RepeatCount',{$ENDIF}integer(@RepeatCount), zptInteger);
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

//Override localtime for adddition of Repeat and AutoReverse behaviour
//Maybe move this functionality to baseclass
function TAnimatorSimple.SetLocalTime: Boolean;
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
  IsGoingForward := True;
  LastBeginTime := BeginTime;
  CurrentIteration := 0;
end;

procedure TAnimatorSimple.Stop;
begin
  inherited;
//  PFloat(TargetPropPtr)^ := ToValue;
end;

{ TAnimatorWithTargetBase }

procedure TAnimatorWithTargetBase.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Target',{$ENDIF}integer(@Target), zptPropertyRef);
    {$ifndef minimal}List.GetLast.NeedRefreshNodeName := True;{$endif}
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
end;

{ TStartAnimator }

procedure TStartAnimator.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Animator',{$ENDIF}integer(@Animator), zptComponentRef);
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


initialization

  ZClasses.Register(TAnimatorGroup,AnimatorGroupClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NeedParentList := 'OnUpdate';{$endif}
  ZClasses.Register(TAnimatorSimple,AnimatorSimpleClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NeedParentList := 'OnUpdate';{$endif}

  ZClasses.Register(TMouseModelController,MouseModelControllerClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NeedParentList := 'OnUpdate';{$endif}
  ZClasses.Register(TStartAnimator,StartAnimatorClassId);

end.
