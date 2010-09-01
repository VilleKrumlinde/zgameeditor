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

unit Collision;

interface

uses ZClasses,Meshes;

type
  //Action to take when colliding
  TCollisionAction =
    (
     caCollision,    //Actor.collision is called
     caStopMovement  //Position is adjusted out of collision, and collision is called
     );

  TCollisionChecks = class
  private
    Checks : TZArrayList;
    Models : TModels;
    HitList : TZArrayList;
    HitListCat : integer;
    function Test(const Act1, Act2: TModel) : boolean;
    procedure PerformActionOnHitList(const Act1 : TModel; const Action : TCollisionAction);
  public
    constructor Create(Models : TModels);
    destructor Destroy; override;
    procedure Add(Cat1,Cat2 : integer; Action : TCollisionAction = caCollision);
    procedure Update;
    {$ifndef minimal}
    procedure ClearAll;
    {$endif}
  end;

  TDefineCollision = class(TCommand)
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Cat1,Cat2 : integer;
    Action : TCollisionAction;
    procedure Execute; override;
  end;


implementation

uses ZApplication,ZMath
 {$IFNDEF MINIMAL},ZLog,Sysutils{$ENDIF}
 ;

type
  TCollisionCheck = class
    Cat1,Cat2 : integer;
    Action : TCollisionAction;
  end;

//http://www.gamedev.net/community/forums/topic.asp?topic_id=364789

function GetProjectedRadius(const O : TOBB_2D; const D : TZVector2f) : single;
begin
  Result := O.E[0] * Abs( VecDot2(D,O.U[0]) ) +
            O.E[1] * Abs( VecDot2(D,O.U[1]) );
end;

function SeparatedOnAxis(const A,B : TOBB_2D; const D : TZVector2f) : boolean;
var
  R : single;
  Tmp : TZVector2f;
begin
  // Get projected distance between OBB centers
  VecSub2(A.C,B.C,Tmp);
  R := Abs( VecDot2(Tmp,D) );
  Result := GetProjectedRadius(A,D) + GetProjectedRadius(B,D) < R;
end;

function IntersectObb(const A,B : TOBB_2D) : boolean;
begin
  Result := False;
  // Exit early if not overlapping
  if (SeparatedOnAxis(a, b, a.u[0])) then Exit;
  if (SeparatedOnAxis(a, b, a.u[1])) then Exit;
  if (SeparatedOnAxis(a, b, b.u[0])) then Exit;
  if (SeparatedOnAxis(a, b, b.u[1])) then Exit;
  // Must be overlapping
  Result := True;
end;



function IntersectRect(const R1,R2 : TZRectf) : boolean;
//Kod utgått ifrån Types.pas
var
  Rect : TZRectf;
begin
  Rect := R1;
  if R2.TopLeft.X > R1.TopLeft.X then Rect.TopLeft.X := R2.TopLeft.X;
  if R2.TopLeft.Y > R1.TopLeft.Y then Rect.TopLeft.Y := R2.TopLeft.Y;
  if R2.BottomRight.X < R1.BottomRight.X then Rect.BottomRight.X := R2.BottomRight.X;
  if R2.BottomRight.Y < R1.BottomRight.Y then Rect.BottomRight.Y := R2.BottomRight.Y;
  Result := not ((Rect.BottomRight.X <= Rect.TopLeft.X) or (Rect.BottomRight.Y <= Rect.TopLeft.Y));
end;

function IntersectSphereSphere(const R1,R2 : TZRectf) : boolean;
var
  Tmp : TZVector3f;
  L : single;
begin
  // return sphereA[1].distanceto(sphereB[1]) - (sphereA[2] + sphereB[2]) <= 0
  VecSub3(PZVector3f(@R1)^,PZVector3f(@R2)^,Tmp);
  L := VecLength3(Tmp);
  Result := L<R1.Area[3]+R2.Area[3];
end;

//2D circle intersection
function IntersectCircle(const R1,R2 : TZRectf) : boolean;
var
  L : single;
begin
  L := Vec2DDistance(PZVector2f(@R1)^,PZVector2f(@R2)^);
  Result := L<R1.Area[2]+R2.Area[2];
end;

//http://www.geometrictools.com/LibFoundation/Intersection/Intersection.html
//http://www.geometrictools.com/LibFoundation/Intersection/Wm4IntrBox3Sphere3.cpp
function IntersectObbCircle(const A : TOBB_2D; const B : TZRectf) : boolean;
var
  kCDiff : TZVector2f;
  fAx,fAy,fDx,fDy,Radius : single;
begin
  VecSub2(PZVector2f(@B)^,A.C,kCDiff);

  fAx := Abs( VecDot2(kCDiff,A.U[0]) );
  fAy := Abs( VecDot2(kCDiff,A.U[1]) );
  fDx := fAx - A.E[0];
  fDy := fAy - A.E[1];

  Radius := B.Area[2];

  if fAx <= A.E[0] then
  begin
    if fAy <= A.E[1] then
      Result := True
    else
      Result := fDy <= Radius;
  end else
  begin
    if fAy <= A.E[1] then
      Result := fDx<=Radius
    else
      Result := (fDx * fDx) + (fDy * fDy) <= (Radius * Radius);
  end;
end;

function IntersectBoxSphere(const Box : TZBox3D; const Sphere : TZRectf) : boolean;
var
  S,D : single;
  I : integer;
begin
  D := 0;
  for I := 0 to 2 do
  begin
    if Sphere.Area[I]<Box.Min[I] then
    begin
      S := Sphere.Area[I] - Box.Min[I];
      D := D + S * S;
    end else if Sphere.Area[I]>Box.Max[I] then
    begin
      S := Sphere.Area[I] - Box.Max[I];
      D := D + S * S;
    end;
  end;
  Result := D <= (Sphere.Area[3]*Sphere.Area[3]);
end;

//http://www.andrewaye.com/tgs_collision.html

function IntersectBoxBox(const Box1,Box2 : TZBox3D) : boolean;
begin
  Result :=
        (Box2.Max[0] >= Box1.Min[0]) and (Box1.Max[0] >= Box2.Min[0]) and
        (Box2.Max[1] >= Box1.Min[1]) and (Box1.Max[1] >= Box2.Min[1]) and
        (Box2.Max[2] >= Box1.Min[2]) and (Box1.Max[2] >= Box2.Min[2]);
end;

{ TCollisionChecks }

{$ifndef minimal}
procedure TCollisionChecks.ClearAll;
begin
  Checks.Clear;
end;
{$endif}

constructor TCollisionChecks.Create(Models : TModels);
begin
  Self.Models := Models;
  Checks := TZArrayList.Create;
  HitList := TZArrayList.CreateReferenced;
end;

destructor TCollisionChecks.Destroy;
begin
  Checks.Free;
  HitList.Free;
  inherited;
end;

procedure TCollisionChecks.PerformActionOnHitList(const Act1: TModel; const Action : TCollisionAction);
var
  I : integer;
  Act2 : TModel;

  function InTestAll : boolean;
  var
    J : integer;
    L  : TZArrayList;
  begin
    Result := False;
    for J := 0 to HitList.Count - 1 do
      if Test(Act1,TModel(HitList[J])) then
      begin
        Result := True;
        Exit;
      end;
    //No hit against hitlist, but tests still need to be made against
    //the complete original list because axis-adjusted movement may
    //have moved the actor into a new objects bounding box.
    L := Models.Get(Self.HitListCat);
    for J := 0 to L.Count - 1 do
    begin
      if Test(Act1,TModel(L[J])) then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;

  procedure InStopMovement;
  //Act1 har slagit i väggen mot objekten i HitList, försök backa position så att kollision ej sker
  //Testar hela tiden mot listan med alla träff-objekt, det är enda sättet att få det
  //att stämma.
  var
    MoveV,StartV : TZVector3f;
    I,ProblemAxis : integer;
    {Scale,}Tmp : single;
  begin
    VecSub3(Act1.Position,Act1.LastPosition,MoveV);
//    VecSub3(Act1.Position,MoveV,StartV);
    //if VecIsNull3(MoveV) then
    //  Exit;
    //MoveX,MoveY är pixels som act har flyttats i denna frame
    //Loopa tills move är noll, eller ej kollision

    //Testa först ifall det räcker med att nolla ut rörelse i en axis.
    //Detta gör att man kan glida mot väggar när man rör sig diagonalt.
    VecCopy3(Act1.LastPosition,StartV);
    ProblemAxis := -1;
    for I := 0 to 2 do
    begin
      if MoveV[I]=0 then
        Continue;
      Tmp:=Act1.Position[I];
      Act1.Position[I]:=StartV[I];
      Act1.CollisionCoordinatesUpdatedTime := 0;
      if not InTestAll then
      begin
        ProblemAxis := I;
        Break;
      end;
      Act1.Position[I] := Tmp;
    end;

    if ProblemAxis>=0 then
    begin
      //Act1.LastPosition[ ProblemAxis ] := Act1.Position[ ProblemAxis ];
      Act1.Velocity[ ProblemAxis ] := 0.0;
    end
    else
    begin
      //Ifall det är problem med fler än 1 axis, backa hela rörelsen
      VecCopy3(StartV,Act1.Position);
      FillChar(Act1.Velocity,SizeOf(Act1.Velocity),0);
      {$ifdef zlog}
      //ZLog.GetLog(Self.ClassName).Write('Collision multi-axis');
      {$endif}
      //Act1.Velocity := Vector3f(0,0,0);
      {Scale:=1;
      while Scale>=0 do
      begin
        VecCopy3( VecAdd3(StartV,VecScalarMult3(MoveV,Scale)), Act1.Position);
        if not InTestAll then
          Break;
        Scale := Scale - 0.1;
      end;
      if Scale<0.5 then
        VecCopy3(StartV,Act1.Position);}
    end;

{
  startPos=lastPos
  problemAxis=-1
  loop axis
    pos=startPos
    pos[axis]=move[axis]
    om ej kollision
      problemAxis=axis
      break
  if problemAxis>=0
    lastPos[problemAxis]=pos[problemAxis]
  else
    Scale=1
    loop while scale>0
      pos=startPos+MoveV*Scale
      if ej kollision
        break
      scale=scale - 0.1
    om scale=0
      pos=startpos  //ge upp
}
  end;

begin

  if Action=caStopMovement then
    InStopMovement;

  for I := 0 to Self.HitList.Count - 1 do
  begin
    Act2 := TModel(HitList[I]);
    Act1.Collision(Act2);
    Act2.Collision(Act1);
  end;

end;

procedure TCollisionChecks.Add(Cat1, Cat2: integer; Action : TCollisionAction = caCollision);
var
  Check : TCollisionCheck;
begin
  Check := TCollisionCheck.Create;
  Check.Cat1 := Cat1;
  Check.Cat2 := Cat2;
  Check.Action := Action;
  Checks.Add(Check);
  ZApp.Models.RegisterCat(Cat1);
  ZApp.Models.RegisterCat(Cat2);
end;

procedure TCollisionChecks.Update;
var
  Act1,Act2 : TModel;
  L1,L2 : TZArrayList;
  Check : TCollisionCheck;
  I,J,K,StartK : integer;
begin
  //todo: optimize, håll listor y-sorterade, då kan loop brytas när man passerat y-gräns
  for I := 0 to Checks.Count-1 do
  begin
    Check := TCollisionCheck(Checks[I]);
    L1 := Models.Get(Check.Cat1);
    L2 := Models.Get(Check.Cat2);
    Self.HitListCat := Check.Cat2;
    for J := 0 to L1.Count-1 do
    begin
      Act1 := TModel(L1[J]);
      Self.HitList.Clear;
      if not Act1.Active then
        Continue;

      //Om samma kategori testas mot sig själv så kan antal tester reduceras
      //Skippa test om act1=act2, samt act2 vs act1 ifall act1 vs act2 har redan testats
      if L1=L2 then
        StartK := J + 1
      else
        StartK := 0;

      for K := StartK to L2.Count-1 do
      begin
        Act2 := TModel(L2[K]);
        if not Act2.Active then
          Continue;
        if Test(Act1,Act2) then
          HitList.Add(Act2);
      end; //actor list cat 2

      if Hitlist.Count>0 then
        PerformActionOnHitList(Act1, Check.Action);

    end; //actor list cat 1
  end; //checks
end;

function TCollisionChecks.Test(const Act1, Act2: TModel): boolean;
type
  TCollMethod = (Unknown,Rect2D_vs_Rect2D,Sphere_vs_Sphere,Box_vs_Sphere,
    Box_vs_Box,Obb_vs_Obb,Circle_Vs_Circle,Obb_Vs_Circle);
var
  RedAct,BlueAct : TModel;
  Style : TCollMethod;
  SwapAct : boolean;
  RedB,BlueB : PCollisionCoordinates;

const
  HitMap : packed array[TCollisionStyle,TCollisionStyle] of
    record
      M : TCollMethod;
      Swap : boolean;
    end = (
                //(csRect2D,csSphere3D,csBox3D,csRect2D_OBB,csCircle2D)
{csRect2D}     ((M:Rect2D_vs_Rect2D; Swap:False; ),(M:Unknown; Swap:False; ),         (M:Unknown; Swap:False) ,     (M:Unknown; Swap:False),   (M:Unknown; Swap:False)),
{csSphere3D}   ((M:Unknown; Swap:False; ),         (M:Sphere_vs_Sphere; Swap:False; ),(M:Box_vs_Sphere; Swap:True) ,(M:Unknown; Swap:False),   (M:Unknown; Swap:False)),
{csBox3D}      ((M:Unknown; Swap:False; ),         (M:Box_vs_Sphere; Swap:False; ),   (M:Box_Vs_Box; Swap:False),   (M:Unknown; Swap:False),   (M:Unknown; Swap:False)),
{csRect2D_OBB} ((M:Unknown; Swap:False; ),         (M:Unknown; Swap:False; ),         (M:Unknown; Swap:False),      (M:Obb_Vs_Obb; Swap:False),(M:Obb_Vs_Circle; Swap:False)),
{csCircle2D}   ((M:Unknown; Swap:False; ),         (M:Unknown; Swap:False; ),         (M:Unknown; Swap:False),      (M:Obb_Vs_Circle; Swap:True),(M:Circle_Vs_Circle; Swap:False))
);


begin
  Result := False;

  Style := HitMap[ Act1.CollisionStyle, Act2.CollisionStyle ].M;
  SwapAct := HitMap[ Act1.CollisionStyle, Act2.CollisionStyle ].Swap;

  if Style=Unknown then
  begin
    //Unsupported collision style
    {$ifndef minimal}
    ZLog.GetLog(Self.ClassName).Warning('Collision combination not supported: ' + CollisionStyleNames[Ord(Act1.CollisionStyle)] + ' ' + CollisionStyleNames[Ord(Act2.CollisionStyle)]);
    {$endif}
    Exit;
  end;

  if SwapAct then
  begin
    RedAct := Act2;
    BlueAct := Act1;
  end
  else
  begin
    RedAct := Act1;
    BlueAct := Act2;
  end;

  RedAct.UpdateCollisionCoordinates;
  RedB := @RedAct.CollisionCoordinates;
  BlueAct.UpdateCollisionCoordinates;
  BlueB := @BlueAct.CollisionCoordinates;

  case Style of
    Rect2D_vs_Rect2D :
      begin
        Result := IntersectRect(RedB.Rect,BlueB.Rect);
      end;
    Sphere_vs_Sphere :
      begin
        Result := IntersectSphereSphere(RedB.Rect,BlueB.Rect);
      end;
    Box_vs_Sphere :
      begin
        Result := IntersectBoxSphere(RedB.Box,BlueB.Rect);
      end;
    Box_vs_Box :
      begin
        Result := IntersectBoxBox(RedB.Box,BlueB.Box);
      end;
    Obb_vs_Obb :
      begin
        Result := IntersectObb(RedB.Obb,BlueB.Obb);
      end;
    Circle_Vs_Circle : Result := IntersectCircle(RedB.Rect,BlueB.Rect);
    Obb_vs_Circle : Result := IntersectObbCircle(RedB.Obb,BlueB.Rect);
    {$ifndef minimal}
    else
      Assert(False,'bad collision style');
    {$endif}
  end;

end;


{ TDefineCollision }

procedure TDefineCollision.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Cat1',{$ENDIF}integer(@Cat1), zptInteger);
  List.AddProperty({$IFNDEF MINIMAL}'Cat2',{$ENDIF}integer(@Cat2), zptInteger);
  List.AddProperty({$IFNDEF MINIMAL}'Action',{$ENDIF}integer(@Action), zptByte);
    {$ifndef minimal}List.GetLast.SetOptions(['Collision','Stop']);{$endif}
end;

procedure TDefineCollision.Execute;
begin
  ZApp.Collisions.Add(Cat1,Cat2,Action);
end;

initialization

  ZClasses.Register(TDefineCollision,DefineCollisionClassId);

end.
