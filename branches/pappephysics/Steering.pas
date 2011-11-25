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

unit Steering;

//"Steering" movement
//Based on http://opensteer.sourceforge.net/

interface

uses ZClasses,ZMath,Meshes;

type
  TSteeringBehaviour = class(TZComponent)
  protected
    OutVector : TZVector3f;
    procedure DefineProperties(List: TZPropertyList); override;
  public
    TargetModel : TModel;
    TargetCategory : integer;
    Weight : single;
    Kind : (sbkSeekModel,sbkSeparation,sbkNoise,sbkFleeModel,sbkExpression,sbkWallAvoidance);
    Expression : TZExpressionPropValue;
    {$ifndef minimal}
    function GetDisplayName: AnsiString; override;
    {$endif}
  end;

  TSteeringController = class(TCommand)
  private
    Model : TModel;
    ForwardV : TZVector3f;
    CurrentAngle : single;
    function InBoidNeighborhood(Other : TModel; const MinDistance,MaxDistance,CosMaxAngle : single) : boolean;
    procedure SteerForSeek(const Target : TZVector3f; out Result : TZVector3f);
    procedure SteerForSeparation(const MaxDistance : single; const CosMaxAngle : single; Flock : TZArrayList; out Result : TZVector3f);
    procedure SteerToNoise(out Result : TZVector3f);
    procedure SteerForFlee(const Target : TZVector3f; out Result : TZVector3f);
    procedure SteerExpression(B : TSteeringBehaviour; out Result : TZVector3f);
    procedure SteerWallAvoidance(WallModels : TZArrayList; out Result : TZVector3f);
    procedure ApplySteeringForce(const Force : TZVector3f);
    function AccumulateForce(var RunningTot : TZVector3f; const ForceToAdd : TZVector3f) : boolean;
    class function LineIntersection2D(const A, B, C, D: TZVector2f; out Dist: single;
      out HitPoint: TZVector2f): boolean;
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Behaviours : TZComponentList;
    MaxForce : single;
    MaxSpeed : single;
    Mass : single;   //default 1 = tröghet i acceleration
    Radius : single; //Storlek
    AdjustHeading : boolean;  //if true, rotera z i den riktning som är forward
    procedure Execute; override;
  end;

implementation

uses ZApplication, ZExpressions;

procedure TSteeringController.ApplySteeringForce(const Force: TZVector3f);
var
  Acceleration,NewVelocity : TZVector3f;
  InvMass,Rot : single;
begin
  //Clamp sväng-radie
//  VecTruncateLength3(Force,MaxForce,ClippedForce);

  //Modifiera acceleration med massa
  InvMass := 1.0 / Mass;
  VecScalarMult3(Force,InvMass,Acceleration);

  //Velocity += acc * time
  VecCopy3(Model.Velocity,NewVelocity);
  VecScalarMult3(Acceleration,ZApp.DeltaTime,Acceleration);
  VecAdd3(NewVelocity,Acceleration,NewVelocity);

  //Clamp velocity med maxspeed
  VecTruncateLength3(NewVelocity,MaxSpeed,NewVelocity);

  //Skriv velocity till model
  VecCopy3(NewVelocity,Model.Velocity);

  if AdjustHeading then
  begin
//    if not VecIsNull3( NewVelocity ) then
//    if VecLength3(NewVelocity)>0.9 then
    begin
      Rot := ArcTan2(NewVelocity[1], NewVelocity[0]);
      //todo: rotation vectors should be in radians
      //todo: rotation skulle behöva smoothas in
      //men om aktuell rot är 0.9 och ny är 0.1 så fungerar det ej
      //då svänger den åt fel håll
      Rot := (1/(3.14*2)) * (Rot-3.14/2);
      Model.Rotation[2] := Rot;
      //Temp1 = Temp1-ShooterModel.Rotation.Z;
    end;
  end;

end;

procedure TSteeringController.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Behaviours',{$ENDIF}integer(@Behaviours), zptComponentList);
    {$ifndef minimal}List.GetLast.SetChildClasses([TSteeringBehaviour]);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'Mass',{$ENDIF}integer(@Mass), zptFloat);
    List.GetLast.DefaultValue.FloatValue := 1.0;
  List.AddProperty({$IFNDEF MINIMAL}'MaxSpeed',{$ENDIF}integer(@MaxSpeed), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}'MaxForce',{$ENDIF}integer(@MaxForce), zptFloat);
    List.GetLast.DefaultValue.FloatValue := 1.0;
  List.AddProperty({$IFNDEF MINIMAL}'AdjustHeading',{$ENDIF}integer(@AdjustHeading), zptBoolean);
  List.AddProperty({$IFNDEF MINIMAL}'Radius',{$ENDIF}integer(@Radius), zptFloat);
end;

procedure TSteeringController.Execute;
var
  I : integer;
  B : TSteeringBehaviour;
  V,BV : TZVector3f;
begin
  //Jobba mot currentmodel
  Self.Model := Meshes.CurrentModel;
  {$ifndef minimal}
  if Model=nil then
    Exit;
  {$endif}

  //Beräkna forward-vector
  VecCopy3(Model.Velocity,Self.ForwardV);
  VecNormalize3(Self.ForwardV);

  //Current heading
  Self.CurrentAngle := ArcTan2(ForwardV[1], ForwardV[0]);

  FillChar(V,SizeOf(V),0);
  for I := 0 to Behaviours.Count-1 do
  begin
    B := TSteeringBehaviour(Behaviours[I]);
    case B.Kind of
      sbkSeekModel :
        SteerForSeek(B.TargetModel.Position,BV);
      sbkSeparation :
        SteerForSeparation(Radius*4,-0.707, ZApp.Models.Get(B.TargetCategory) ,BV);
      sbkNoise :
        SteerToNoise(BV);
      sbkFleeModel :
        SteerForFlee(B.TargetModel.Position,BV);
      sbkExpression :
        SteerExpression(B, BV);
      sbkWallAvoidance :
        SteerWallAvoidance(ZApp.Models.Get(B.TargetCategory), BV);
    end;

    VecScalarMult3(BV,B.Weight,BV);

    //Avbryt så fort vi nått max
    //Detta gör att behaviours ligger i prioriteringsordning
    //Om WallAvoidance ligger först så kan den stöta ut alla andra
    if not AccumulateForce(V, BV) then
      Break;

  end;
  ApplySteeringForce(V);
end;


function TSteeringController.AccumulateForce(var RunningTot : TZVector3f; const ForceToAdd : TZVector3f) : boolean;
var
  MagnitudeSoFar,MagnitudeRemaining,MagnitudeToAdd : single;
  Tmp : TZVector3f;
begin
  //calculate how much steering force the vehicle has used so far
  MagnitudeSoFar := VecLength3(RunningTot);

  //calculate how much steering force remains to be used by this vehicle
  MagnitudeRemaining := Self.MaxForce - MagnitudeSoFar;

  //return false if there is no more force left to use
  if MagnitudeRemaining <= 0.0 then
  begin
    Result := False;
    Exit;
  end;

  //calculate the magnitude of the force we want to add
  MagnitudeToAdd := VecLength3(ForceToAdd);

  //if the magnitude of the sum of ForceToAdd and the running total
  //does not exceed the maximum force available to this vehicle, just
  //add together. Otherwise add as much of the ForceToAdd vector is
  //possible without going over the max.
  if MagnitudeToAdd < MagnitudeRemaining then
    VecAdd3(RunningTot,ForceToAdd,RunningTot)
  else
  begin
    //add it to the steering force
    Tmp := ForceToAdd;
    VecNormalize3(Tmp);
    VecScalarMult3(Tmp,MagnitudeRemaining,Tmp);
    VecAdd3(RunningTot,Tmp,RunningTot);
  end;

  Result := True;
end;


procedure TSteeringController.SteerForSeek(const Target : TZVector3f; out Result : TZVector3f);
var
  DesiredVelocity : TZVector3f;
begin
  VecSub3(Target,Model.Position,DesiredVelocity);
  VecTruncateLength3(DesiredVelocity,Self.MaxSpeed,DesiredVelocity);
  VecSub3(DesiredVelocity,Model.Velocity,Result);
end;

procedure TSteeringController.SteerForFlee(const Target: TZVector3f; out Result: TZVector3f);
var
  DesiredVelocity : TZVector3f;
begin
  VecSub3(Model.Position,Target,DesiredVelocity);
  VecTruncateLength3(DesiredVelocity,Self.MaxSpeed,DesiredVelocity);
  VecSub3(DesiredVelocity,Model.Velocity,Result);
end;

function TSteeringController.InBoidNeighborhood(Other : TModel; const MinDistance,MaxDistance,CosMaxAngle : single) : boolean;
var
  Offset,UnitOffset : TZVector3f;
  Forwardness,DistanceSquared : single;
begin
  if Other=Model then
    Result := False
  else
  begin
    VecSub3(Other.Position,Model.Position,Offset);
    DistanceSquared := VecLengthSquared3(Offset);

    // definitely in neighborhood if inside minDistance sphere
    if DistanceSquared < (MinDistance * MinDistance) then
      Result := True
    else
    begin
      // definitely not in neighborhood if outside maxDistance sphere
      if DistanceSquared > (MaxDistance * MaxDistance) then
        Result := False
      else
      begin
        // otherwise, test angular offset from forward axis
        VecDiv3(Offset,Sqrt(DistanceSquared),UnitOffset);
        Forwardness := VecDot3(ForwardV,UnitOffset);
        Result := Forwardness > CosMaxAngle;
      end;
    end;
  end;
end;


procedure TSteeringController.SteerForSeparation(const MaxDistance,
  CosMaxAngle: single; Flock: TZArrayList; out Result: TZVector3f);
var
  Offset,Temp : TZVector3f;
  Neighbors : integer;
  Other : TModel;
  I : integer;
  DistanceSquared : single;
begin
  // steering accumulator and count of neighbors, both initially zero
  FillChar(Result,SizeOf(Result),0);
  Neighbors := 0;

  // for each of the other vehicles...
  for I := 0 to Flock.Count-1 do
  begin
    Other := TModel(Flock[I]);
    if InBoidNeighborhood(Other,Self.Radius*3,MaxDistance,CosMaxAngle) then
    begin
      // add in steering contribution
      // (opposite of the offset direction, divided once by distance
      // to normalize, divided another time to get 1/d falloff)
      VecSub3(Other.Position,Model.Position,Offset);
      DistanceSquared := VecLengthSquared3(Offset);
      if DistanceSquared<Epsilon then
        //Avoid div by zero
        DistanceSquared:=Epsilon;
      VecDiv3(Offset,-DistanceSquared,Temp);
      VecAdd3(Result,Temp,Result);
      // count neighbors
      Inc(Neighbors);
    end;
  end;

  // divide by neighbors, then normalize to pure direction
  if Neighbors > 0 then
  begin
    VecDiv3(Result,Neighbors,Result);
    VecNormalize3(Result);
  end;
end;


procedure TSteeringController.SteerToNoise(out Result: TZVector3f);
var
  Angle : single;
begin
  //Beräkna en kurs som är max +/- 25% av aktuell kurs
  Angle := ZMath.PerlinNoise3( Model.Position[0],Model.Position[1],ZApp.Time) * (PI*2);
  Angle := CurrentAngle + Angle*0.25;
//  Angle := System.Random * (PI*2);
  Result[0] := cos(Angle);
  Result[1] := sin(Angle);
  Result[2] := 0;
end;


procedure TSteeringController.SteerExpression(
  B : TSteeringBehaviour; out Result: TZVector3f);
begin
  ZExpressions.RunCode(B.Expression.Code);
  Result := B.OutVector;
end;


//-------------------- LineIntersection2D-------------------------
//
//	Given 2 lines in 2D space AB, CD this returns true if an
//	intersection occurs and sets dist to the distance the intersection
//  occurs along AB. Also sets the 2d vector point to the point of
//  intersection
//-----------------------------------------------------------------
class function TSteeringController.LineIntersection2D(const A,B,C,D : TZVector2f;
  out Dist : single;
  out HitPoint : TZVector2f) : boolean;
var
  rTop,rBot : single;
  sTop,sBot : single;
  r,s : single;
  Tmp : TZVector2f;
begin
  Result := False;

  rTop := (A[1]-C[1])*(D[0]-C[0])-(A[0]-C[0])*(D[1]-C[1]);
  rBot := (B[0]-A[0])*(D[1]-C[1])-(B[1]-A[1])*(D[0]-C[0]);

  sTop := (A[1]-C[1])*(B[0]-A[0])-(A[0]-C[0])*(B[1]-A[1]);
  sBot := (B[0]-A[0])*(D[1]-C[1])-(B[1]-A[1])*(D[0]-C[0]);

  if (rBot=0) or (sBot=0) then
    //lines are parallel
    Exit;

  r := rTop/rBot;
  s := sTop/sBot;

  if (r > 0) and (r < 1) and (s > 0) and (s < 1) then
  begin
    Dist := Vec2DDistance(A,B) * r;
    VecSub2(B,A,Tmp);
    Tmp := VecScalarMult2(Tmp,R);
    HitPoint := VecAdd2(A,Tmp);
    //point = A + r * (B - A);
    Result := True;
  end
  else
    Dist := 0;
end;


procedure TSteeringController.SteerWallAvoidance(WallModels: TZArrayList; out Result: TZVector3f);
//Från boken "Programming game AI by example"
type
  TWall2D =
    record
      P1,P2 : TZVector2f;  //Start and endpoint of wall
    end;
const
  FeelerLength = 0.75;
var
  Feelers : array[0..2] of TZVector2f;
  Feeler : PZVector2f;
  Walls : array[0..3] of TWall2D;
  Bounds : PCollisionCoordinates;
  I,J,K,ClosestWall : integer;
  ModelPos2D,HitPoint,ClosestPoint,OverShoot,Tmp : TZVector2f;
  DistToThis,DistToClosest,T1 : single;

    function GetWallNormal(const W : TWall2d) : TZVector2f;
    var
      Tmp : TZVector2f;
    begin
      VecSub2(W.P2,W.P1,Tmp);
      VecNormalize2(Tmp);
      Result[0] := -Tmp[1];
      Result[1] := Tmp[0];
    end;

begin
  FillChar(Result,SizeOf(Result),0);

  ModelPos2D := Vector2f(Model.Position[0],Model.Position[1]);

  //Skapa "känselsprön"
{  Feelers[0] := VecAdd2(ModelPos2D,
    VecScalarMult2(
    VecScalarMult2( Vector2f(ForwardV[0],ForwardV[1]) , FeelerLength ) ,
    VecLength2( Vector2f(Self.Model.Velocity[0],Self.Model.Velocity[1]) ) ) );}
//  Feelers[0] := VecAdd2(ModelPos2D, Vector2f(Self.Model.Velocity[0],Self.Model.Velocity[1]) );
//  Feelers[0] := VecAdd2(ModelPos2D, VecScalarMult2( Vector2f(ForwardV[0],ForwardV[1]) , FeelerLength ) );

  //Forward feeler måste scalas med length(velocity) för att hinna bromsa
  //när model rör sig snabbt
  T1 := VecLength2( Vector2f(Self.Model.Velocity[0],Self.Model.Velocity[1]) );
  Feelers[0] := VecAdd2(ModelPos2D,
    VecScalarMult2(
      VecScalarMult2( Vector2f(ForwardV[0],ForwardV[1]) , FeelerLength ),
      T1)
    );
  //Right feeler
  T1 := Self.CurrentAngle + PI*0.25;
  Feelers[1] := VecAdd2(ModelPos2D, VecScalarMult2( Vector2f(Cos(T1),Sin(T1)) , FeelerLength ) );
  //Left feeler
  T1 := Self.CurrentAngle - PI*0.25;
  Feelers[2] := VecAdd2(ModelPos2D, VecScalarMult2( Vector2f(Cos(T1),Sin(T1)) , FeelerLength ) );

  DistToClosest := 100000; //default to high value

  for I := 0 to WallModels.Count-1 do
  begin
    //obs, collisionstyle måste vara rect2d
    TModel(WallModels[I]).UpdateCollisionCoordinates;
    Bounds := @TModel(WallModels[I]).CollisionCoordinates;

    Walls[0].P1 := Vector2f(Bounds.Rect.Right,Bounds.Rect.Top);
    Walls[0].P2 := Vector2f(Bounds.Rect.Left,Bounds.Rect.Top);

    Walls[1].P1 := Vector2f(Bounds.Rect.Left,Bounds.Rect.Bottom);
    Walls[1].P2 := Vector2f(Bounds.Rect.Right,Bounds.Rect.Bottom);

    Walls[2].P1 := Vector2f(Bounds.Rect.Left,Bounds.Rect.Top);
    Walls[2].P2 := Vector2f(Bounds.Rect.Left,Bounds.Rect.Bottom);

    Walls[3].P1 := Vector2f(Bounds.Rect.Right,Bounds.Rect.Bottom);
    Walls[3].P2 := Vector2f(Bounds.Rect.Right,Bounds.Rect.Top);

    Feeler := @Feelers[0];
    for J := 0 to High(Feelers) do
    begin

      ClosestWall := -1;
      for K := 0 to High(Walls) do
      begin
        if not LineIntersection2D(ModelPos2D,
                 Feeler^,
                 Walls[K].P1,
                 Walls[K].P2,
                 DistToThis,
                 HitPoint) then
          Continue;

        if DistToThis<DistToClosest then
        begin
          //is this the closest found so far? If so keep a record
          DistToClosest := DistToThis;
          ClosestWall := K;
          ClosestPoint := HitPoint;
        end;
      end;

      //if an intersection point has been detected, calculate a force
      //that will direct the agent away
      if ClosestWall>-1 then
      begin
        //calculate by what distance the projected position of the agent
        //will overshoot the wall
        VecSub2(Feeler^,ClosestPoint,OverShoot);
        //create a force in the direction of the wall normal, with a
        //magnitude of the overshoot
        Tmp := VecScalarMult2( GetWallNormal(Walls[ClosestWall]) , VecLength2(OverShoot));
        //Gör om 2d vector till 3d
        Result := Vector3f(Tmp[0],Tmp[1],0);
      end;

      Inc(Feeler);
    end;
  end;

end;


{ TSteeringBehaviour }

{$ifndef minimal}
const
  SteerKindNames : array [0..5] of string = (('SeekModel'),('SeparationCategory'),
    ('Noise'),('FleeModel'),('Expression'),('WallAvoidance2D'));
{$endif}

procedure TSteeringBehaviour.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Kind',{$ENDIF}integer(@Kind), zptByte);
    {$ifndef minimal}List.GetLast.SetOptions(SteerKindNames);{$endif}
    {$ifndef minimal}List.GetLast.NeedRefreshNodeName:=True;{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'TargetModel',{$ENDIF}integer(@TargetModel), zptComponentRef);
    {$ifndef minimal}List.GetLast.SetChildClasses([TModel]);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'TargetCategory',{$ENDIF}integer(@TargetCategory), zptInteger);
  List.AddProperty({$IFNDEF MINIMAL}'Weight',{$ENDIF}integer(@Weight), zptFloat);
    List.GetLast.DefaultValue.FloatValue := 1.0;
  List.AddProperty({$IFNDEF MINIMAL}'Expression',{$ENDIF}integer(@Expression), zptExpression);
    {$ifndef minimal}
    List.GetLast.DefaultValue.ExpressionValue.Source :=
      '//OutVector : result steer vector';
    {$endif}
  //Outvector for expressions
  List.AddProperty({$IFNDEF MINIMAL}'OutVector',{$ENDIF}integer(@OutVector), zptVector3f);
    List.GetLast.NeverPersist := True;
end;

{$ifndef minimal}
function TSteeringBehaviour.GetDisplayName: AnsiString;
begin
  Result := inherited GetDisplayName;
  Result := Result + '  ' + AnsiString(SteerKindNames[ ord(Kind) ]);
end;
{$endif}


initialization

  ZClasses.Register(TSteeringController,SteeringControllerClassId);
  ZClasses.Register(TSteeringBehaviour,SteeringBehaviourClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NeedParentComp := 'SteeringController';{$endif}

end.
