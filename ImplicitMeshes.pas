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

unit ImplicitMeshes;

{
  Code based on http://www.unchainedgeometry.com/jbloom/
}

interface

uses ZClasses,Meshes,ZExpressions;

type
  //Precision: single or double
  //Double get better results but adds 1kb to final binary
  //Obs: vector och matris är alltid single, borde nog ändra dom också
  TImpType = single;

  TImplicitFunction = class(TZComponent)
  protected
    InverseTransform : TZMatrix4f;
    function DoEval(const X,Y,Z : TImpType): TImpType; virtual; abstract;
    function Eval(const X,Y,Z : TImpType) : TImpType;
    procedure Prepare; virtual; //Prepare calculations
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Position : TZVector3f;
    Scale : TZVector3f;
    Rotation : TZVector3f;
  end;


  TMeshImplicit = class(TMeshProducer)
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Functions : TZArrayList;
    Size : single; //Size of the cell
    Bounds : integer; //limit to how far away we will look for components of the implicit surface
    MultipleSurfaces : boolean;
    procedure ProduceOutput(Content : TContent; Stack : TZArrayList); override;
  end;


  TImplicitPrimitive = class(TImplicitFunction)
  protected
    function DoEval(const X,Y,Z : TImpType): TImpType; override;
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Param1,Param2 : single;
    Kind : (ikSphere,ikTorus,ikCube);
    {$ifndef minimal}
    function GetDisplayName: AnsiString; override;
    {$endif}
  end;

  TImplicitExpression = class(TImplicitFunction)
  protected
    function DoEval(const X,Y,Z : TImpType): TImpType; override;
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Expression : TZExpressionPropValue;
    X,Y,Z : single;
  end;

  TImplicitCombine = class(TImplicitFunction)
  protected
    function DoEval(const X,Y,Z : TImpType): TImpType; override;
    procedure DefineProperties(List: TZPropertyList); override;
    procedure Prepare; override;
  public
    Children : TZArrayList;
    Kind : (ickUnion,ickIntersect,ickDifference);
    Blending : single;
  end;

  TImplicitWarp = class(TImplicitFunction)
  protected
    function DoEval(const X,Y,Z : TImpType): TImpType; override;
    procedure DefineProperties(List: TZPropertyList); override;
    procedure Prepare; override;
  public
    Functions : TZArrayList;
    Kind : (iwkTwist,iwkBend,iwkTaper);
    WarpAmount : single;
  end;

implementation

uses ZMath,ZLog;

const
  RES = 10; //* # converge iterations    */
  ImpHASHBIT = 5;
  ImpHASHSIZE = (1 shl (3*ImpHASHBIT));
  ImpMASK = ((1 shl ImpHASHBIT)-1);
  ImpL=0;   //left direction: -x, -i */
  ImpR=1;   //right direction: +x, +i */
  ImpB=2;   //bottom direction: -y, -j */
  ImpT=3;   //top direction: +y, +j */
  ImpN=4;   //near direction: -z, -k */
  ImpF=5;   //far direction: +z, +k */
  LBN = 0;  //* left bottom near corner  */
  LBF = 1;  //* left bottom far corner   */
  LTN = 2;  //* left top near corner     */
  LTF = 3;  //* left top far corner      */
  RBN = 4;  //* right bottom near corner */
  RBF = 5;  //* right bottom far corner  */
  RTN = 6;  //* right top near corner    */
  RTF = 7;  //* right top far corner     */

type
  TImpPoint = record
    X,Y,Z : TImpType;
  end;

  TImpPointInList = class
    Point : TImpPoint;
    constructor Create(const P : TImpPoint);
  end;

  //corner of a cube
  TImpCorner = class
    I,J,K : integer; //Index withing lattice
    X,Y,Z,Value : TImpType; //location and function value
  end;

  //cube location
  TImpCenter = class
    I,J,K : integer;
  end;

  //partitioning cell (cube)
  TImpCube = class
    I,J,K : integer; //lattice location of cube
    Corners : array[0..7] of TImpCorner; //eight corners
  end;

  //test the function for a signed value
  TImpTest = record
    P : TImpPoint;
    Value : TImpType;
    Ok : boolean;
  end;

  TImpEdge = class
    i1, j1, k1, i2, j2, k2 : integer; //edge corner ids
    vid : integer; //vertex id
  end;

  //contains the indices of the vertices comprising the triangle
  TImpTriangle = class
    V0,V1,V2 : integer;
  end;

  //Den klass som faktiskt gör jobbet.
  //Att den ligger i en separat klass gör att det mesta kan ligga
  //i implementation-delen av uniten.
  TImpProcess = class
  private
    Delta : TImpType;
    Start : TImpPoint;  //start point on surface
    Size : TImpType; //Size of the cell
    Bounds : integer; //limit to how far away we will look for components of the implicit surface
    Func : TImplicitFunction;
    Cubes : TZArrayList;
    CornerOwner : TZArrayList;  //Äger minnet för alla corners
    Corners : array[0..ImpHASHSIZE] of TZArrayList;     //Cornercache. List of lists.
    Centers : array[0..ImpHASHSIZE] of TZArrayList;     //Cube center hash table
    Edges : array[0..(ImpHASHSIZE*2)] of TZArrayList;
    Vertices,Normals,Triangles : TZArrayList;
    MultipleSurfaces : boolean; //if true, compute whole area to look for more surfaces
    procedure Find(var Test : TImpTest; Sign: boolean; X,Y,Z : TImpType);
    procedure Converge(var P1,P2 : TImpPoint; V : TImpType; MaxIter : integer; out P : TImpPoint);
    procedure March(const X,Y,Z : TImpType);
    constructor Create;
    function SetCorner(I, J, K: integer): TImpCorner;
    function SetCenter(I, J, K: integer): boolean;
    procedure DoTet(Cube: TImpCube; C1, C2, C3, C4: integer);
    function VertId(C1, C2: TImpCorner): integer;
    procedure VNormal(const Point: TImpPoint; out N: TImpPoint);
    function GetEdge(I1, J1, K1, I2, J2, K2: integer): integer;
    procedure SetEdge(I1, J1, K1, I2, J2, K2, Vid: integer);
    procedure Triangle(I1, I2, I3: integer);
    procedure TestFace(I, J, K: integer; Old: TImpCube; Face, C1, C2, C3,
      C4: integer);
  public
    destructor Destroy; override;
  end;


function ImpBIT(I,Bit : integer) : integer;
begin
  Result := (I shr Bit) and 1;
end;

function ImpHASH(I,J,K : integer) : integer;
begin
  Result := (((((I and ImpMASK) shl ImpHASHBIT) or
    (J and ImpMASK)) shl ImpHASHBIT) or (K and ImpMASK));
end;

function ImpFLIP(I,Bit : integer) : integer;
//flip the given bit of i
begin
  Result := I xor (1 shl Bit);
end;

{ TImpProcess }

//from two points of differing sign, converge to zero crossing
procedure TImpProcess.Converge(var P1, P2 : TImpPoint; V: TImpType; MaxIter : integer; out P : TImpPoint);
var
  I : integer;
  Pos,Neg : TImpPoint;
begin
  if V<0 then
  begin
    Pos := P2;
    Neg := P1;
  end
  else
  begin
    Pos := P1;
    Neg := P2;
  end;

  I := 0;
  while True do
  begin
    P.X := 0.5 * (Pos.X + Neg.X);
    P.Y := 0.5 * (Pos.Y + Neg.Y);
    P.Z := 0.5 * (Pos.Z + Neg.Z);
    if I=MaxIter then
      Break;
    Inc(I);

    if Func.Eval(P.X,P.Y,P.Z)>0 then
      Pos := P
    else
      Neg := P;
  end;
end;

constructor TImpProcess.Create;
begin
  Cubes := TZArrayList.CreateReferenced;
  CornerOwner := TZArrayList.Create;
end;

destructor TImpProcess.Destroy;
var
  I : integer;
begin
  Cubes.Free;
  CornerOwner.Free;
  for I := 0 to High(Corners) do
    if Corners[I]<>nil then
      Corners[I].Free;
  for I := 0 to High(Centers) do
    if Centers[I]<>nil then
      Centers[I].Free;
  for I := 0 to High(Edges) do
    if Edges[I]<>nil then
      Edges[I].Free;
end;

procedure TImpProcess.Find(var Test: TImpTest; Sign: boolean; X, Y, Z: TImpType);
//search for point with value of given sign (false: neg, true: pos)
var
  I : integer;
  Range : TImpType;
begin
  Range := Self.Size;
  Test.Ok := True;
  for I := 0 to 10000 do
  begin
    Test.P.X := ZMath.Random(X,Range*0.5);
    Test.P.Y := ZMath.Random(Y,Range*0.5);
    Test.P.Z := ZMath.Random(Z,Range*0.5);
    Test.Value := Func.Eval(Test.P.X,Test.P.Y,Test.P.Z);
    if (Test.Value>0)=Sign then
      Exit; //Ok
    Range := Range * 1.0005; //slowly expand search outwards
  end;
  //Failed
  Test.Ok := False;
end;

function TImpProcess.SetCorner(I,J,K : integer) : TImpCorner;
//Return corner with the given lattice location
//Set (and cache) its function value
var
  C,CacheC : TImpCorner;
  Index : integer;
  Cache : TZArrayList;
  M : integer;
begin
  C := TImpCorner.Create;
  CornerOwner.Add( C );

  Index := ImpHASH(I,J,K);

  C.I := I;
  C.X := Start.X + (I - 0.5) * Size;

  C.J := J;
  C.Y := Start.Y + (J - 0.5) * Size;

  C.K := K;
  C.Z := Start.Z + (K - 0.5) * Size;

  //Check for match in cache
  Cache := Corners[Index];
  if Cache=nil then
  begin
    Cache := TZArrayList.Create;
    Corners[Index] := Cache;
  end;

  for M := Cache.Count-1 downto 0 do
  begin
    CacheC := TImpCorner(Cache[M]);
    if (CacheC.I=I) and (CacheC.J=K) and (CacheC.K=K) then
    begin //match found, copy function value and exit
      C.Value := CacheC.Value;
      Result := C;
      Exit;
    end;
  end;

  C.Value := Func.Eval(C.X,C.Y,C.Z);

  //Denna funktion returnerar alltid en nyallokerad corner.
  //Lägg en separat kopia i cachen. Oklar om detta är nödvändigt
  //men originalet gör detta.
  CacheC := TImpCorner.Create;
  CacheC.I := I;
  CacheC.J := J;
  CacheC.K := K;
  CacheC.Value := C.Value;
  Cache.Add( CacheC );

  Result := C;
end;


function TImpProcess.SetCenter(I,J,K : integer) : boolean;
//setcenter: set (i,j,k) entry in cache
//return true if already set
var
  Index,M : integer;
  C : TImpCenter;
  Cache : TZArrayList;
begin
  Index := ImpHASH(I,J,K);

  Cache := Centers[Index];
  if Cache=nil then
  begin
    Cache := TZArrayList.Create;
    Centers[Index] := Cache;
  end;

  for M := Cache.Count-1 downto 0 do
  begin
    C := TImpCenter(Cache[M]);
    if (C.I=I) and (C.J=J) and (C.K=K) then
    begin
      Result := True;
      Exit;
    end;
  end;

  C := TImpCenter.Create;
  C.I := I;
  C.J := J;
  C.K := K;
  Cache.Add(C);
  Result := False;
end;

procedure TImpProcess.VNormal(const Point : TImpPoint; out N : TImpPoint);
//compute unit length surface normal at point
var
  F,Inv : TImpType;
begin
  F := Func.Eval(Point.X,Point.Y,Point.Z);
  N.X := Func.Eval(Point.X + Delta,Point.Y,Point.Z) - F;
  N.Y := Func.Eval(Point.X,Point.Y + Delta,Point.Z) - F;
  N.Z := Func.Eval(Point.X,Point.Y,Point.Z + Delta) - F;

  F := Sqrt(N.X * N.X + N.Y * N.Y + N.Z * N.Z);
  if F<>0.0 then
  begin
    Inv := 1.0 / F;
    N.X := N.X * Inv;
    N.Y := N.Y * Inv;
    N.Z := N.Z * Inv;
  end;
end;


function TImpProcess.GetEdge(I1,J1,K1,I2,J2,K2 : integer) : integer;
//return vertex id for edge; return -1 if not set
var
  T,Index,I : integer;
  Cache : TZArrayList;
  E : TImpEdge;
begin
  if (I1>I2) or ((I1=I2) and ((J1>J2) or ((J1=J2) and (K1>K2)))) then
  begin
    t:=i1; i1:=i2; i2:=t; t:=j1; j1:=j2; j2:=t; t:=k1; k1:=k2; k2:=t;
  end;

  Index := ImpHash(I1,J1,K1) + ImpHash(I2,J2,K2);
  Cache:=Edges[Index];
  if (Cache<>nil) then
  begin
    for I := Cache.Count-1 downto 0 do
    begin
      E := TImpEdge(Cache[I]);
      if (E.I1=I1) and (E.J1=J1) and (E.K1=K1) and
         (E.I2=I2) and (E.J2=J2) and (E.K2=K2) then
      begin //match found
        Result := E.Vid;
        Exit;
      end;
    end;
  end;

  Result := -1;
end;

procedure TImpProcess.SetEdge(I1,J1,K1,I2,J2,K2,Vid : integer);
//set vertex id for edge
var
  Index,T : integer;
  New : TImpEdge;
  Cache : TZArrayList;
begin
  //(i1>i2 || (i1==i2 && (j1>j2 || (j1==j2 && k1>k2))))
  if (I1>I2) or ((I1=I2) and ((J1>J2) or ((J1=J2) and (K1>K2)))) then
  begin
    t:=i1; i1:=i2; i2:=t; t:=j1; j1:=j2; j2:=t; t:=k1; k1:=k2; k2:=t;
  end;

  New := TImpEdge.Create;
  New.I1 := I1; New.J1 := J1; New.K1 := K1;
  New.I2 := I2; New.J2 := J2; New.K2 := K2;
  New.Vid := Vid;

  Index := ImpHash(I1,J1,K1) + ImpHash(I2,J2,K2);
  Cache:=Edges[Index];
  if Cache=nil then
  begin
    Cache := TZArrayList.Create;
    Edges[Index] := Cache;
  end;
  Cache.Add(New);
end;

function TImpProcess.VertId(C1,C2 : TImpCorner) : integer;
//vertid: return index for vertex on edge:
//c1->value and c2->value are presumed of different sign
//return saved index if any; else compute vertex and save */
var
  V,N,A,B : TImpPoint;
  Vid : integer;
begin
  Vid := GetEdge(C1.I,C1.J,C1.K, C2.I,C2.J,C2.K);
  if Vid<>-1 then
  begin
    Result := Vid;
    Exit; //previously computed
  end;

  A.X := C1.X; A.Y := C1.Y; A.Z := C1.Z;
  B.X := C2.X; B.Y := C2.Y; B.Z := C2.Z;

  Converge(A,B,C1.Value,4,V);  //position

  VNormal(V,N);
  Vertices.Push( TImpPointInList.Create(V) );
  Normals.Push( TImpPointInList.Create(N) );

  Vid := Vertices.Count-1;
  SetEdge(C1.I,C1.J,C1.K, C2.I,C2.J,C2.K, Vid);

  Result := Vid;
end;

procedure TImpProcess.Triangle(I1,I2,I3 : integer);
var
  T : TImpTriangle;
begin
  T := TImpTriangle.Create;
  T.V0 := I1;
  T.V1 := I2;
  T.V2 := I3;
  Triangles.Add(T);
end;

procedure TImpProcess.DoTet(Cube : TImpCube; C1,C2,C3,C4 : integer);
//triangulate the tetrahedron
//B, C, D should appear clockwise when viewed from A
var
  A,B,C,D : TImpCorner;
  Index : integer;
  e1, e2, e3, e4, e5, e6 : integer;
  apos, bpos, cpos, dpos : boolean;
begin
  A := Cube.Corners[C1];
  B := Cube.Corners[C2];
  C := Cube.Corners[C3];
  D := Cube.Corners[C4];

  Index := 0;

  APos := A.Value>0;
  if APos then
    Inc(Index,8);
  BPos := B.Value>0;
  if BPos then
    Inc(Index,4);
  CPos := C.Value>0;
  if CPos then
    Inc(Index,2);
  DPos := D.Value>0;
  if DPos then
    Inc(Index,1);

  E1 := 0; E2 := 0; E3 := 0;
  E4 := 0; E5 := 0; E6 := 0;

  //index is now 4-bit number representing one of the 16 possible cases
  if APos<>BPos then E1 := VertId(A,B);
  if APos<>CPos then E2 := VertId(A,C);
  if APos<>DPos then E3 := VertId(A,D);

  if BPos<>CPos then E4 := VertId(B,C);
  if BPos<>DPos then E5 := VertId(B,D);
  if CPos<>DPos then E6 := VertId(C,D);

  case Index of
    1 : Triangle(E5,E6,E3);
    2 : Triangle(E2,E6,E4);
    3 :
      begin
        Triangle(E3,E5,E4);
        Triangle(E3,E4,E2);
      end;
    4 : Triangle(E1,E4,E5);
    5 :
      begin
        Triangle(E3,E1,E4);
        Triangle(E3,E4,E6);
      end;
    6 :
      begin
        Triangle(E1,E2,E6);
        Triangle(E1,E6,E5);
      end;
    7 : Triangle(E1,E2,E3);
    8 : Triangle(E1,E3,E2);
    9 :
      begin
        Triangle(E1,E5,E6);
        Triangle(E1,E6,E2);
      end;
    10 :
      begin
        Triangle(E1,E3,E6);
        Triangle(E1,E6,E4);
      end;
    11 : Triangle(E1,E5,E4);
    12 :
      begin
        Triangle(E3,E2,E4);
        Triangle(E3,E4,E5);
      end;
    13 : Triangle(E6,E2,E4);
    14 : Triangle(E5,E3,E6);
  end;
end;



procedure TImpProcess.TestFace(I,J,K : integer; Old : TImpCube;
  Face, C1, C2, C3, C4 : integer);
// testface: given cube at lattice (i, j, k), and four corners of face,
// if surface crosses face, compute other four corners of adjacent cube
// and add new cube to cube stack
const
  FaceBit : array[0..5] of byte = (2,2,1,1,0,0);
var
  New : TImpCube;
  N,Bit : integer;
  Pos : boolean;
begin

  Pos := Old.Corners[C1].Value>0;
  Bit := FaceBit[Face];

  //test if no surface crossing
  if (not MultipleSurfaces) and
     ((Old.Corners[C2].Value>0)=Pos) and
     ((Old.Corners[C3].Value>0)=Pos) and
     ((Old.Corners[C4].Value>0)=Pos) then
    Exit;

  //test cube out of bounds,
  if (Abs(I)>Bounds) or (Abs(J)>Bounds) or (Abs(K)>Bounds) then
    Exit;

  //test already visited
  if SetCenter(I,J,K) then
    Exit;

  //create new cube
  New := TImpCube.Create;
  New.I := I;
  New.J := J;
  New.K := K;

  New.Corners[ ImpFLIP(C1,Bit) ] := Old.Corners[C1];
  New.Corners[ ImpFLIP(C2,Bit) ] := Old.Corners[C2];
  New.Corners[ ImpFLIP(C3,Bit) ] := Old.Corners[C3];
  New.Corners[ ImpFLIP(C4,Bit) ] := Old.Corners[C4];

  for N := 0 to High(New.Corners) do
    if New.Corners[N]=nil then
      New.Corners[N] := SetCorner(I + ImpBIT(N,2), J + ImpBIT(N,1), K + ImpBIT(N,0));

  // Add new cube to top of stack
  Cubes.Push(New);
end;

procedure TImpProcess.March(const X, Y, Z: TImpType);
var
  TIn,TOut : TImpTest;
  Cube : TImpCube;
  I : integer;
begin
  Delta := Size / (RES*RES);

  //find point on surface, beginning search at (x, y, z): */
  //srand(1);
  if not MultipleSurfaces then
  begin
    Find(TIn,True, x, y, z);
    Find(TOut,False, x, y, z);
    if (not TIn.Ok) or (not TOut.Ok) then
      Exit; //ZHalt('can''t find starting point');
    Converge(TIn.P, TOut.P, TIn.Value, RES, Self.Start);
  end;
  //else If multiplesurfaces just leave start point at zero, whole area will be calculated anyway

  //push initial cube on stack
  Cube := TImpCube.Create;
  Cubes.Push(Cube);

  //set corners of initial cube
  for I := 0 to 7 do
    Cube.Corners[I] := SetCorner( ImpBIT(I,2), ImpBIT(I,1), ImpBIT(I,0) );

  SetCenter(0,0,0);

  //process active cubes till none left
  while Cubes.Count>0 do
  begin
    Cube := TImpCube(Cubes.Last);

    //decompose into tetrahedra and polygonize
    DoTet(Cube, LBN, LTN, RBN, LBF);
    DoTet(Cube, RTN, LTN, LBF, RBN);
    DoTet(Cube, RTN, LTN, LTF, LBF);
    DoTet(Cube, RTN, RBN, LBF, RBF);
    DoTet(Cube, RTN, LBF, LTF, RBF);
    DoTet(Cube, RTN, LTF, RTF, RBF);

    //pop current cube from stack
    Cubes.Pop;

    //test six face directions, maybe add to stack
    TestFace(Cube.i-1, Cube.j, Cube.k, Cube, ImpL, LBN, LBF, LTN, LTF);
    TestFace(Cube.i+1, Cube.j, Cube.k, Cube, ImpR, RBN, RBF, RTN, RTF);
    TestFace(Cube.i, Cube.j-1, Cube.k, Cube, ImpB, LBN, LBF, RBN, RBF);
    TestFace(Cube.i, Cube.j+1, Cube.k, Cube, ImpT, LTN, LTF, RTN, RTF);
    TestFace(Cube.i, Cube.j, Cube.k-1, Cube, ImpN, LBN, LTN, RBN, RTN);
    TestFace(Cube.i, Cube.j, Cube.k+1, Cube, ImpF, LBF, LTF, RBF, RTF);

    Cube.Free;
  end;
end;


{ TMeshImplicit }

procedure TMeshImplicit.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Function',{$ENDIF}(@Functions), zptComponentList);
    {$ifndef minimal}List.GetLast.SetChildClasses([TImplicitFunction]);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'TriangleSize',{$ENDIF}(@Size), zptScalar);
    List.GetLast.DefaultValue.FloatValue:=0.05;
  List.AddProperty({$IFNDEF MINIMAL}'Bounds',{$ENDIF}(@Bounds), zptInteger);
    List.GetLast.DefaultValue.IntegerValue:=100;
  List.AddProperty({$IFNDEF MINIMAL}'MultipleSurfaces',{$ENDIF}(@MultipleSurfaces), zptBoolean);
end;

procedure TMeshImplicit.ProduceOutput(Content: TContent; Stack: TZArrayList);
var
  Mesh : TMesh;
  VertDest : PZVector3f;
  VertSrc : TImpPointInList;
  Tri : TImpTriangle;
  IDest : PMeshVertexIndex;
  I : integer;
  Vertices,Normals,Triangles : TZArrayList;
  P : TImpProcess;
  Func : TImplicitFunction;
  SaveSeed : longint;
begin
  {$ifndef minimal}
  if Self.Functions.Count=0 then
    Exit;
  {$endif}

  {$ifdef zlog}
  ZLog.GetLog(Self.ClassName).Write('Updating implicit geometry...');
  {$endif}

  Mesh := TMesh.Create(nil);

  Vertices := TZArrayList.Create;
  Normals := TZArrayList.Create;
  Triangles := TZArrayList.Create;

  //Set randseed so that vertices stay the same between runs
  SaveSeed := RandSeed;
  RandSeed := 0;

  Func := TImplicitFunction(Functions[0]);
  Func.Prepare;

    P := TImpProcess.Create;
      P.Func := Func;
      P.Size := Self.Size;
      P.Bounds := Self.Bounds;
      P.Normals := Normals;
      P.Triangles := Triangles;
      P.Vertices := Vertices;
      P.MultipleSurfaces := MultipleSurfaces;
      P.March(0,0,0);
    P.Free;

    Mesh.CreateData(Vertices.Count,Triangles.Count);

    //Copy vertices
    VertDest := @Mesh.Vertices[0];
    for I := 0 to Vertices.Count-1 do
    begin
      VertSrc := TImpPointInList(Vertices[I]);
      VertDest[0]:=VertSrc.Point.X;
      VertDest[1]:=VertSrc.Point.Y;
      VertDest[2]:=VertSrc.Point.Z;
      Inc(VertDest);
    end;

    //Copy indices
    IDest := @Mesh.Indices[0];
    for I := 0 to Triangles.Count-1 do
    begin
      Tri := TImpTriangle(Triangles[I]);
{      IDest^ := Tri.V0; Inc(IDest);
      IDest^ := Tri.V1; Inc(IDest);
      IDest^ := Tri.V2; Inc(IDest);}
      //Omvänd ordning stämmer bättre med våran computenormals
      IDest^ := Tri.V2; Inc(IDest);
      IDest^ := Tri.V1; Inc(IDest);
      IDest^ := Tri.V0; Inc(IDest);
    end;

    //Copy normals
    //Normals för implicit ger mycket bättre normals än mesh.computenormals
    VertDest := @Mesh.Normals[0];
    for I := 0 to Normals.Count-1 do
    begin
      VertSrc := TImpPointInList(Normals[I]);
      VertDest[0]:=VertSrc.Point.X;
      VertDest[1]:=VertSrc.Point.Y;
      VertDest[2]:=VertSrc.Point.Z;
      Inc(VertDest);
    end;
    //Nix, nu blir det korrekt med egna normals i alla fall
//    Mesh.ComputeNormals;

  RandSeed := SaveSeed;

  Vertices.Free;
  Normals.Free;
  Triangles.Free;

  Stack.Push(Mesh);
end;



{ TImpPointInList }

constructor TImpPointInList.Create(const P: TImpPoint);
begin
  Self.Point := P;
end;

{ TImplicitPrimitive }

{$ifndef minimal}
const
  PrimitiveNames : array [0..2] of string = (('Sphere'),('Torus'),('Cube'));
{$endif}

procedure TImplicitPrimitive.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Kind',{$ENDIF}(@Kind), zptByte);
    {$ifndef minimal}List.GetLast.SetOptions(PrimitiveNames);{$endif}
    {$ifndef minimal}List.GetLast.NeedRefreshNodeName := True;{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'Param1',{$ENDIF}(@Param1), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}'Param2',{$ENDIF}(@Param2), zptFloat);
end;

{$ifndef minimal}
function TImplicitPrimitive.GetDisplayName: AnsiString;
begin
  Result := inherited GetDisplayName;
  Result := Result + '  ' + AnsiString(PrimitiveNames[ ord(Kind) ]);
end;
{$endif}


{$ifdef minimal} {$WARNINGS OFF} {$endif}
function TImplicitPrimitive.DoEval(const X, Y, Z: TImpType): TImpType;
var
  X2,Y2,Z2,A2,B2,A : TImpType;
begin
  {$ifndef minimal}
  Result:=0;
  {$endif}
  case Kind of
    ikSphere :
      begin
        Result := X*X + Y*Y + Z*Z - 1.0;
        //Result := sqrt(X*X + Y*Y + Z*Z) - 1.0;
      end;
    ikTorus :
      begin
        //Torus
        {$ifndef minimal}
        if Param1=0 then Param1 := 1.0;
        if Param2=0 then Param2 := 0.5;
        {$endif}
        x2 := x*x;
        y2 := y*y;
        z2 := z*z;
        a2 := Param1*Param1;
        b2 := Param2*Param2;
        //a := x2+y2+z2 - ((Param2*Param2)+(Param1*Param1));
        A := x2+y2+z2-(a2+b2);
        Result := A*A - 4.0 * a2*(b2-z2);
      end;
    ikCube :
      begin
        Result :=  Power(X,8) + Power(Y,8) + Power(Z,8)  - 1.0;
      end;
  end;
end;
{$ifdef minimal} {$WARNINGS ON} {$endif}




{ TImplicitExpression }

procedure TImplicitExpression.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Expression',{$ENDIF}(@Expression), zptExpression);
    {$ifndef minimal}List.GetLast.ReturnType.Kind := zctFloat;{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'X',{$ENDIF}(@X), zptFloat);
    List.GetLast.NeverPersist := True;
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'Y',{$ENDIF}(@Y), zptFloat);
    List.GetLast.NeverPersist := True;
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'Z',{$ENDIF}(@Z), zptFloat);
    List.GetLast.NeverPersist := True;
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
end;

function TImplicitExpression.DoEval(const X, Y, Z: TImpType): TImpType;
begin
  Self.X := X;
  Self.Y := Y;
  Self.Z := Z;
  Result := Single(ZExpressions.RunCode(Expression.Code));
end;

{ TImplicitCombine }

procedure TImplicitCombine.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Kind',{$ENDIF}(@Kind), zptByte);
    {$ifndef minimal}List.GetLast.SetOptions(['Union','Intersect','Difference']);{$endif}
    {$ifndef minimal}List.GetLast.NeedRefreshNodeName := True;{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'Blending',{$ENDIF}(@Blending), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}'Children',{$ENDIF}(@Children), zptComponentList);
    {$ifndef minimal}List.GetLast.SetChildClasses([TImplicitFunction]);{$endif}
end;


//Blend, according to http://citeseer.ist.psu.edu/dekkers96combining.html
function Blend(const X,N : TImpType) : TImpType;
begin
  if X>=0.25*N then
    Result := 0
  else
    Result := N * Power((X/N) - (1/4),2);
end;

{$ifdef minimal} {$WARNINGS OFF} {$endif}
function TImplicitCombine.DoEval(const X, Y, Z: TImpType): TImpType;
var
  A,B : TImpType;
begin
  {$ifndef minimal}
  Result := 0; //remove warning
  if Children.Count<2 then
    Exit;
  {$endif}
  A := TImplicitFunction(Children[0]).Eval(X,Y,Z);
  B := TImplicitFunction(Children[1]).Eval(X,Y,Z);
  case Kind of
    ickUnion :
      begin
        Result := Min(A,B) - Blend( Abs(A-B) ,Blending);
      end;
    ickIntersect :
      begin
        Result := Max(A,B) + Blend( Abs(A-B) ,Blending);
      end;
    ickDifference :
      begin
        Result := Max(A,-B) + Blend( Abs(A-(-B)) ,Blending);
      end;
  end;
end;
{$ifdef minimal} {$WARNINGS ON} {$endif}

procedure TImplicitCombine.Prepare;
var
  I : integer;
begin
  inherited;
  for I := 0 to Children.Count-1 do
    TImplicitFunction(Children[I]).Prepare;
end;

{ TImplicitFunction }

procedure TImplicitFunction.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Position',{$ENDIF}(@Position), zptVector3f);
  List.AddProperty({$IFNDEF MINIMAL}'Scale',{$ENDIF}(@Scale), zptVector3f);
    List.GetLast.DefaultValue.Vector3fValue := ZMath.UNIT_XYZ3;
  List.AddProperty({$IFNDEF MINIMAL}'Rotation',{$ENDIF}(@Rotation), zptVector3f);
end;

function TImplicitFunction.Eval(const X, Y, Z: TImpType): TImpType;
var
  V,TransV : TZVector3f;
begin
  V[0] := X;
  V[1] := Y;
  V[2] := Z;
  VectorTransform(V,InverseTransform,TransV);
  Result := DoEval(TransV[0],TransV[1],TransV[2]);
end;

procedure TImplicitFunction.Prepare;
var
  M : TZMatrix4f;
begin
  M := CreateTransform(Self.Rotation,Self.Scale,Self.Position);
  InvertMatrix(M);
  InverseTransform := M;
end;

{ TImplicitWarp }

procedure TImplicitWarp.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Kind',{$ENDIF}(@Kind), zptByte);
    {$ifndef minimal}List.GetLast.SetOptions(['Twist','Bend','Taper']);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'Function',{$ENDIF}(@Functions), zptComponentList);
    {$ifndef minimal}List.GetLast.SetChildClasses([TImplicitFunction]);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'WarpAmount',{$ENDIF}(@WarpAmount), zptFloat);
end;

{$ifdef minimal} {$WARNINGS OFF} {$endif}
function TImplicitWarp.DoEval(const X, Y, Z: TImpType): TImpType;
var
  X1,Y1,Z1,A : TImpType;
begin
  {$ifndef minimal}
  X1:=0; Y1:=0; Z1:=0;
  Result := 0; if Functions.Count=0 then Exit;
  {$endif}
  case Self.Kind of
    iwkTwist :
      begin
        A := Y * Self.WarpAmount;
        X1 := X * Cos(A) - Z * Sin(A);
        Y1 := Y;
        Z1 := X * Sin(A) + Z * Cos(A);
      end;
    iwkBend :
      begin
        A := X * Self.WarpAmount;
        X1 := -Sin(A) * (Y-1/Self.WarpAmount);
        Y1 := Cos(A) * (Y-1/Self.WarpAmount) + 1/Self.WarpAmount;
        Z1 := Z;
      end;
    iwkTaper :
      begin
        X1 := X * (1 + Y * Self.WarpAmount);
        Y1 := Y;
        Z1 := Z;
      end;
  end;
  Result := TImplicitFunction(Functions[0]).Eval(X1,Y1,Z1);
end;
{$ifdef minimal} {$WARNINGS ON} {$endif}

procedure TImplicitWarp.Prepare;
begin
  inherited;
  {$ifndef minimal}
  if Functions.Count=0 then Exit;
  {$endif}
  TImplicitFunction(Functions[0]).Prepare;
end;

initialization

  ZClasses.Register(TMeshImplicit,MeshImplicitClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NeedParentComp := 'Mesh';{$endif}
  ZClasses.Register(TImplicitPrimitive,ImplicitPrimitiveClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NeedParentComp := 'MeshImplicit';{$endif}
  ZClasses.Register(TImplicitExpression,ImplicitExpressionClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NeedParentComp := 'MeshImplicit';{$endif}
  ZClasses.Register(TImplicitCombine,ImplicitCombineClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NeedParentComp := 'MeshImplicit';{$endif}
  ZClasses.Register(TImplicitWarp,ImplicitWarpClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NeedParentComp := 'MeshImplicit';{$endif}

end.
