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

unit ZMath;

interface

uses ZClasses;

function Vector2f(const x, y : Single): TZVector2f;
function Vector3f(const x, y, z: Single): TZVector3f;
function Vector4f(const x, y, z, w: Single): TZVector4f;

//function VecMult3(const v1, v2: TZVector3f): TZVector3f; overload;
procedure VecMult3(var v1 : TZVector3f; const v2: TZVector3f); overload;
function VecAdd3(const v1, v2: TZVector3f): TZVector3f; overload;
procedure VecAdd3(const v1, v2: TZVector3f; out Result : TZVector3f); overload;

function VecAdd2(const v1, v2: TZVector2f): TZVector2f; overload;
procedure VecAdd2_Inplace(var Result : TZVector2f; const v2: TZVector2f);

function VecScalarMult3(const v: TZVector3f; s: Single): TZVector3f; overload;
procedure VecScalarMult3(const v: TZVector3f; s: Single; out Result : TZVector3f); overload;
function VecScalarMult2(const v: TZVector2f; s: Single): TZVector2f;
procedure VecScalarMult2_Inplace(var Result: TZVector2f; const s: Single);
procedure VecNormalize3(var V: TZVector3f);
procedure VecNormalize2(var V: TZVector2f);
function VecIsIdentity3(const V : TZVector3f): boolean;
function VecIsNull3(const V : TZVector3f): boolean;
function VecIsEqual3(const V1,V2 : TZVector3f): boolean;
function VecLengthSquared3(const v: TZVector3f): single;
function VecLength2(const v: TZVector2f): Single;
function VecLength3(const v: TZVector3f): Single;
procedure VecTruncateLength3(const V : TZVector3f; const MaxLength : single; out Result : TZVector3f);
procedure VecSub3(const v1, v2: TZVector3f; out Result : TZVector3f);
procedure VecSub2(const v1, v2: TZVector2f; out Result : TZVector2f);

procedure VecCopy3(const Source : TZVector3f; out Dest : TZVector3f);
function VecDot3(const V1,V2 : TZVector3f) : single;
function VecDot2(const V1,V2 : TZVector2f) : single;
procedure VecDiv3(const V1 : TZVector3f; const S : single; var Result : TZVector3f);

function VecIsNull4(const V : TZVector4f): boolean;
function VecIsEqual4(const V1,V2 : TZVector4f): boolean;


//Math.pas replacements, from kolmath.pas
//http://bonanzas.rinet.ru/e_downloads.htm
function Tan(const X: single): single;
function ArcTan2(const Y, X: single): single;

function ArcSin(const X : Single) : Single;
function ArcCos(const X : Single) : Single;
function Log2(const X : Single) : Single;

function PerlinNoise2(const X,Y : single) : single;
function PerlinNoise3(const X,Y,Z : single) : single;


function CycleToRad(const Cycles: single): single; { Radians := Cycles * 2PI }

function Power(const Base, Exponent: single): single;
function SmoothStep(const A,B,X : single) : single;
function Clamp(const X,Min,Max : single) : single;

function Random(const Base,Diff : single) : single; overload;

function Min(const A,B : single) : single; overload;
function Max(const A, B: Single): Single; overload;

function Min(const A,B : Integer) : Integer; overload;
function Max(const A, B: Integer): Integer; overload;

function Ceil(const X: single): single;
function Floor(const X: single): integer;


function ColorFtoB(const C : TZColorf) : integer;


//Matrix-functions. From GL-Scene.
procedure InvertMatrix(var M : TZMatrix4f);
procedure ScaleMatrix(var M : TZMatrix4f; const factor : Single);
procedure VectorTransform(const V: TZVector3f; const M: TZMatrix4f; out Result : TZVector3f);
procedure CreateScaleAndTranslationMatrix(const scale, offset : TZVector3f; out Result : TZMatrix4f);
function CreateTransform(const Rotation,Scale,Position : TZVector3f) : TZMatrix4f;

procedure SinCos(const Theta: Single; out Sin, Cos: Single);
procedure CreateRotationMatrixX(const Angle : Single; out Result : TZMatrix4f);
procedure CreateRotationMatrixY(const Angle : Single; out Result : TZMatrix4f);
procedure CreateRotationMatrixZ(const Angle : Single; out Result : TZMatrix4f);

function MatrixMultiply(const M1, M2: TZMatrix4f) : TZMatrix4f;

function Vec2DDistance(const v1,v2 : TZVector2f) : single;

const
  UNIT_Z3: TZVector3f = (0,0,1);
  UNIT_XYZ3 : TZVector3f = (1,1,1);
  UNIT_XYZ4 : TZVector4f = (1,1,1,1);
  IdentityHmgMatrix: TZMatrix4f = ((1, 0, 0, 0),
                                (0, 1, 0, 0),
                                (0, 0, 1, 0),
                                (0, 0, 0, 1));
  EPSILON  : Single = 1e-40;

implementation


{$ifndef CPU386}
uses Math;
{$endif}


function ColorFtoB(const C : TZColorf) : integer;
begin
  Result := ( Round( clamp(C.V[3],0,1)*255) shl 24) or
    (Round( clamp(C.V[2],0,1)*255) shl 16) or
    (Round( clamp(C.V[1],0,1)*255) shl 8) or
    (Round( clamp(C.V[0],0,1)*255));
end;


function Min(const A,B : single) : single;  overload;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

function Min(const A,B : Integer) : integer; overload;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

function Max(const A, B: Single): Single; overload;
begin
  if A > B then
    Result := A
  else
    Result := B;
end;


function Max(const A, B: Integer): Integer; overload;
begin
  if A > B then
    Result := A
  else
    Result := B;
end;

function Vector2f(const x, y : Single): TZVector2f;
begin
  Result[0] := X;
  Result[1] := Y;
end;

function Vector3f(const x, y, z: Single): TZVector3f;
begin
  Result[0] := X;
  Result[1] := Y;
  Result[2] := Z;
end;

procedure VecMult3(var v1 : TZVector3f; const v2: TZVector3f); overload;
begin
  v1[0] := v1[0] * v2[0];
  v1[1] := v1[1] * v2[1];
  v1[2] := v1[2] * v2[2];
end;

//Dot product
function VecDot3(const V1,V2 : TZVector3f) : single;
begin
  Result := (V1[0] * V2[0]) + (V1[1] * V2[1]) + (V1[2] * V2[2]);
end;

function VecDot2(const V1,V2 : TZVector2f) : single;
begin
  Result := (V1[0] * V2[0]) + (V1[1] * V2[1]);
end;

procedure VecDiv3(const V1 : TZVector3f; const S : single; var Result : TZVector3f);
begin
  Result[0] := V1[0] / S;
  Result[1] := V1[1] / S;
  Result[2] := V1[2] / S;
end;

function Vector4f(const x, y, z, w: Single): TZVector4f;
begin
  Result[0] := X;
  Result[1] := Y;
  Result[2] := Z;
  Result[3] := W;
end;

function VecAdd3(const v1, v2: TZVector3f): TZVector3f; overload;
begin
  Result[0] := v1[0] + v2[0];
  Result[1] := v1[1] + v2[1];
  Result[2] := v1[2] + v2[2];
end;

function VecAdd2(const v1, v2: TZVector2f): TZVector2f; overload;
begin
  Result[0] := v1[0] + v2[0];
  Result[1] := v1[1] + v2[1];
end;

procedure VecAdd2_Inplace(var Result : TZVector2f; const v2: TZVector2f);
begin
  Result[0] := Result[0] + v2[0];
  Result[1] := Result[1] + v2[1];
end;


procedure VecAdd3(const v1, v2: TZVector3f; out Result : TZVector3f); overload;
begin
  Result := VecAdd3(V1,V2);
end;

function VecScalarMult3(const v: TZVector3f; s: Single): TZVector3f;
begin
  Result[0] := v[0] * s;
  Result[1] := v[1] * s;
  Result[2] := v[2] * s;
end;

function VecScalarMult2(const v: TZVector2f; s: Single): TZVector2f;
begin
  Result[0] := v[0] * s;
  Result[1] := v[1] * s;
end;

procedure VecScalarMult2_Inplace(var Result: TZVector2f; const s: Single);
begin
  Result[0] := Result[0] * s;
  Result[1] := Result[1] * s;
end;

procedure VecScalarMult3(const v: TZVector3f; s: Single; out Result : TZVector3f); overload;
begin
  Result := VecScalarMult3(V,S);
end;

function Tan(const X: single): single;
{$IFDEF CPU386}
asm
        FLD    X
        FPTAN
        FSTP   ST(0)      { FPTAN pushes 1.0 after result }
        FWAIT
end;
{$else}
begin
  {$ifdef FPC}
  Result := Math.Tan(X);
  {$else}
  Result := System.Tangent(X);
  {$endif}
end;
{$endif}

function VecLengthSquared3(const v: TZVector3f): Single;
begin
  Result := v[0]*v[0] + v[1]*v[1] + v[2]*v[2];
end;

function VecLength2(const v: TZVector2f): Single;
begin
  Result := sqrt( v[0]*v[0] + v[1]*v[1] );
end;

function VecLength3(const v: TZVector3f): Single;
begin
  Result := sqrt( v[0]*v[0] + v[1]*v[1] + v[2]*v[2]);
end;

procedure VecNormalize3(var V: TZVector3f);
var
  L,InvL: Single;
begin
  L := VecLengthSquared3(v);
  if L = 0 then
    Exit; //L := 1;
  L := sqrt(L);
  InvL := 1.0 / L;
  v[0] := v[0] * InvL;
  v[1] := v[1] * InvL;
  v[2] := v[2] * InvL;
end;

procedure VecNormalize2(var V: TZVector2f);
var
  L,InvL: Single;
begin
  L := VecLength2(v);
  if L = 0 then
    Exit; //L := 1;
  InvL := 1.0 / L;
  v[0] := v[0] * InvL;
  v[1] := v[1] * InvL;
end;

procedure VecTruncateLength3(const V : TZVector3f; const MaxLength : single; out Result : TZVector3f);
var
  VLengthSquared,MaxLengthSquared : single;
  InvL : single;
begin
  MaxLengthSquared := MaxLength * MaxLength;
  VLengthSquared := VecLengthSquared3(V);
  if VLengthSquared <= MaxLengthSquared then
    VecCopy3(V,Result)
  else
  begin
    InvL := MaxLength / sqrt(VLengthSquared);
    Result:=VecScalarMult3(V,InvL);
  end;
end;

function VecIsIdentity3(const V : TZVector3f): boolean;
begin
  Result := (V[0]=1) and (V[1]=1) and (V[2]=1);
end;

function VecIsNull3(const V : TZVector3f): boolean;
begin
  //Noll-test är snabbare och kompaktare att göra med intar
//  Result := (V[0]=0) and (V[1]=0) and (V[2]=0);
  Result := (PInteger(@V[0])^=0) and (PInteger(@V[1])^=0) and (PInteger(@V[2])^=0);
  {$ifndef minimal}
//  if Result then
//    ZAssert(((abs(V[0])<EPSILON) and (abs(V[1])<EPSILON) and (abs(V[2])<EPSILON)),'VecIsNull3');
  {$endif}
end;

function VecIsNull4(const V : TZVector4f): boolean;
begin
//  Result := (V[0]=0) and (V[1]=0) and (V[2]=0) and (V[3]=0);
  Result := (PInteger(@V[0])^=0) and (PInteger(@V[1])^=0) and (PInteger(@V[2])^=0) and (PInteger(@V[3])^=0);
  {$ifndef minimal}
//  if Result then
//    ZAssert(result=(abs(V[0])<EPSILON) and (abs(V[1])<EPSILON) and (abs(V[2])<EPSILON) and (abs(V[3])<EPSILON),'VecIsNull4');
  {$endif}
end;

function VecIsEqual4(const V1,V2 : TZVector4f): boolean;
begin
  Result := (PInteger(@V1[0])^=PInteger(@V2[0])^) and
    (PInteger(@V1[1])^=PInteger(@V2[1])^) and
    (PInteger(@V1[2])^=PInteger(@V2[2])^) and
    (PInteger(@V1[3])^=PInteger(@V2[3])^);
  {$ifndef minimal}
//  ZAssert(Result =
//    (V1[0]=V2[0]) and (V1[1]=V2[1]) and (V1[2]=V2[2]) and (V1[3]=V2[3]),
//    'VecIsEqual4'
//    );
  {$endif}
end;

function VecIsEqual3(const V1,V2 : TZVector3f): boolean;
begin
  Result := (V1[0]=V2[0]) and (V1[1]=V2[1]) and (V1[2]=V2[2]);
end;

procedure VecCopy3(const Source : TZVector3f; out Dest : TZVector3f);
begin
  Dest := Source;
end;

procedure VecSub3(const v1, v2: TZVector3f; out Result : TZVector3f);
begin
  Result[0] := V1[0] - V2[0];
  Result[1] := V1[1] - V2[1];
  Result[2] := V1[2] - V2[2];
end;

procedure VecSub2(const v1, v2: TZVector2f; out Result : TZVector2f);
begin
  Result[0] := V1[0] - V2[0];
  Result[1] := V1[1] - V2[1];
end;


function ArcTan2(const Y, X: single): single;
{$IFDEF CPU386}
asm
        FLD     Y
        FLD     X
        FPATAN
        FWAIT
end;
{$else}
begin
  Result := {$ifndef FPC}System.{$endif}Math.ArcTan2(Y,X);
end;
{$endif}

//Perlin new noise for hardware
//Approx range: -0.3 .. 0.3
function PerlinNoise3(const X,Y,Z : single) : single;
const
  T : array[0..7] of integer = ($15,$38,$32,$2c,$0d,$13,$07,$2a);
var
  I,J,KK : integer;
  U,V,W : single;
  A : array[0..2] of integer;

  S : single;
  Hi,Lo : integer;

  function B2(N,B : integer) : integer; inline;
  begin
    Result := (N shr B) and 1;
  end;

  function B(I,J,K,B : integer) : integer; inline; //this inline increases performance of about 10%
  begin
    Result := T[ (B2(I,B) shl 2) or (B2(J,B) shl 1) or B2(K,B) ];
  end;

  function Shuffle(I,J,K : integer) : integer; inline;
  begin
    Result := B(i,j,k,0) + B(j,k,i,1) + B(k,i,j,2) + B(i,j,k,3) +
      B(j,k,i,4) + B(k,i,j,5) + B(i,j,k,6) + B(J,K,I,7);
  end;

  function K(AA : integer) : single;
  var
    S,P,Q,R : single;
    X,Y,Z,T,Tmp1 : single;
    H, B5,B4,B3,B2,BB : integer;
  begin
    S := (A[0]+A[1]+A[2])/6.0;
    X:= U - A[0]+S;
    Y := V-A[1] + S;
    Z:= W - A[2] +  S;
    T := 0.6 - X*X-Y*Y-Z*Z;
    H := Shuffle(I+A[0],J+A[1],KK+A[2]);
    Inc(A[AA]);
    if T<0 then
    begin
      Result := 0;
      Exit;
    end;
    B5 := (H shr 5) and 1;
    B4 := (H shr 4) and 1;
    B3 := (H shr 3) and 1;
    B2 := (H shr 2) and 1;
    BB := H and 3;
    if BB=1 then
    begin
      P := X;
      Q := Y;
      R := Z;
    end
    else
    begin
      if BB=2 then
      begin
        P := Y;
        Q := Z;
        R := X;
      end
      else
      begin
        P := Z;
        Q := X;
        R := Y;
      end;
    end;

    if B5=B3 then
      P := -P;
    if B5=B4 then
      Q := -Q;
    if B5<>(B4 xor B3) then
      R := -R;

    T := T * T;

    if BB=0 then
      Tmp1 := Q+R
    else
    begin
      if B2=0 then
        Tmp1 := Q
      else
        Tmp1 := R;
    end;

    Result :=  8.0 * T * T * (P + Tmp1);
  end;

  function CHack(C : boolean; Left,Right : integer) : integer;
  begin
    if C then
      Result := Left
    else
      Result := Right;
  end;

begin
  S := (X+Y+Z) * (1/3.0);

  I:= Floor(X+S);
  J:= Floor(Y+S);
  KK:= Floor(Z+S);

  S := (I+J+KK) * (1/6.0);
  U := X-I+S;
  V := Y-J+S;
  W := Z-KK+S;

  A[0]:=0;
  A[1]:=0;
  A[2]:=0;

  Hi := CHack(U>=W,  CHack(U>=V,0, 1), CHack(V>=W, 1, 2) );
  Lo := CHack(U< W,  CHack(U< V,0, 1), CHack(V< W, 1, 2) );

  Result := K(Hi) + K(3-Hi-Lo) + K(Lo) + K(0);
end;


function PerlinNoise2(const X,Y : single) : single;
begin
  Result := PerlinNoise3(X,Y,0);
end;


//Från math.pas
function IntPower(const Base: Extended; const Exponent: Integer): Extended;
{$IFDEF CPU386}
asm
        mov     ecx, eax
        cdq
        fld1                      { Result := 1 }
        xor     eax, edx
        sub     eax, edx          { eax := Abs(Exponent) }
        jz      @@3
        fld     Base
        jmp     @@2
@@1:    fmul    ST, ST            { X := Base * Base }
@@2:    shr     eax,1
        jnc     @@1
        fmul    ST(1),ST          { Result := Result * X }
        jnz     @@1
        fstp    st                { pop X from FPU stack }
        cmp     ecx, 0
        jge     @@3
        fld1
        fdivrp                    { Result := 1 / Result }
@@3:
        fwait
end;
{$else}
begin
  Result := {$ifndef FPC}System.{$endif}Math.IntPower(Base,Exponent);
end;
{$endif}

function Power(const Base, Exponent: single): single;
begin
  if Exponent = 0.0 then
    Result := 1.0               { n**0 = 1 }
  else if (Base = 0.0) and (Exponent > 0.0) then
    Result := 0.0               { 0**n = 0, n > 0 }
  else if (Frac(Exponent) = 0.0) and (Abs(Exponent) <= MaxInt) then
    Result := IntPower(Base, Integer(Trunc(Exponent)))
  else
    Result := Exp(Exponent * Ln(Base))
end;

//Used by Animator-component for smooth transitions
function SmoothStep(const A,B,X : single) : single;
var
  Xx : single;
begin
  if X<A then
    Result := 0
  else if X>=B then
    Result := 1
  else
  begin
    Xx := (X-A) / (B-A);
    Result := (Xx * Xx * (3 - 2 * Xx));
  end;
end;

function Clamp(const X,Min,Max : single) : single;
begin
  if X<Min then
    Result := Min
  else if X>Max then
    Result := Max
  else
    Result := X;
end;

function Random(const Base,Diff : single) : single;
begin
  Result := Base + ((2*System.Random-1.0) * Diff);
end;

//Matrix-functions. From GL-scene

const
  // to be used as descriptive indices
  X = 0;
  Y = 1;
  Z = 2;
  W = 3;

//this function takes a matrix and "removes" a line and a column by shifting the values
//one place
function SubMatrix(const M : TZMatrix4f; LineSkip, ColumnSkip : integer) : TZMatrix4f;
var I,J,K,L : integer;
begin
  K := 0;

  for I := 0 to 3 do
    begin
      if I = LineSkip then
        continue;
      L := 0;
      for J := 0 to 3 do
      begin
        if J = ColumnSkip then
          continue;

        Result[K,L] := M[I,J];
        L := L + 1;
      end;
      K := K+1; //increase current Line position
    end;

end;

//recursive function to calculate the determinant of a matrix
function NMatrixDet(const M : TZMatrix4f; const Dimension : integer): single;
var L : integer;
begin
  if Dimension = 2 then //BASE CASE
  begin
    Result := M[0,0]*M[1,1] - M[0,1]*M[1,0];
  end
  else    //DIMENSION <> 2
  begin
    Result := 0;
    for L := 0 to Dimension - 1 do
      begin
        //create the SubMatrix
        //containing all the elements except the first column and the I-nth line
        //the calc the det of the submatrix and add the result
        Result := Result + Power(-1,L)*M[L,0]*NMatrixDet(SubMatrix(M,L,0), Dimension-1);
      end;
  end;
end;

function AdjointMatrix(const M : TZMatrix4f): TZMatrix4f;
var I,J : integer;
begin
  for I := 0 to 3 do
    for J := 0 to 3 do
      Result[J,I] := Power(-1,I+J)*NMatrixDet(SubMatrix(M,I,J),3)
end;

procedure ScaleMatrix(var M : TZMatrix4f; const factor : Single);
var
  I : Integer;
begin
  for I := 0 to 3 do
  begin
    M[I, 0]:=M[I, 0] * Factor;
    M[I, 1]:=M[I, 1] * Factor;
    M[I, 2]:=M[I, 2] * Factor;
    M[I, 3]:=M[I, 3] * Factor;
  end;
end;

procedure InvertMatrix(var M : TZMatrix4f);
var
  det : Single;
begin
  det := NMatrixDet(M,4);
  if Abs(Det)<EPSILON then
    M := IdentityHmgMatrix
  else
  begin
    M := AdjointMatrix(M);
    ScaleMatrix(M, 1/det);
  end;
end;

procedure VectorTransform(const V: TZVector3f; const M: TZMatrix4f; out Result : TZVector3f);
begin
   Result[X]:=V[X] * M[X, X] + V[Y] * M[Y, X] + V[Z] * M[Z, X] + M[W, X];
   Result[Y]:=V[X] * M[X, Y] + V[Y] * M[Y, Y] + V[Z] * M[Z, Y] + M[W, Y];
   Result[Z]:=V[X] * M[X, Z] + V[Y] * M[Y, Z] + V[Z] * M[Z, Z] + M[W, Z];
end;


procedure SinCos(const Theta: Single; out Sin, Cos: Single);
{$IFDEF CPU386}
// EAX contains address of Sin
// EDX contains address of Cos
// Theta is passed over the stack
asm
   FLD  Theta
   FSINCOS
   FSTP DWORD PTR [EDX]    // cosine
   FSTP DWORD PTR [EAX]    // sine
end;
{$else}
var
  S,C : double;
begin
  {$ifdef FPC}
  Math.SinCos(Theta, S, C);
  {$else}
  System.SineCosine(Theta, S, C);
  {$endif}
  Sin := S;
  Cos := C;
end;
{$endif}

procedure CreateScaleAndTranslationMatrix(const scale, offset : TZVector3f; out Result : TZMatrix4f);
begin
  Result:=IdentityHmgMatrix;
  Result[X, X]:=scale[X];   Result[W, X]:=offset[X];
  Result[Y, Y]:=scale[Y];   Result[W, Y]:=offset[Y];
  Result[Z, Z]:=scale[Z];   Result[W, Z]:=offset[Z];
end;

procedure CreateRotationMatrixX(const Angle : Single; out Result : TZMatrix4f);
var
  Sine, Cosine : Single;
begin
  SinCos(Angle, Sine, Cosine);
  FillChar(Result,SizeOf(Result),0);
  Result[X, X]:=1;
  Result[Y, Y]:=cosine;
  Result[Y, Z]:=sine;
  Result[Z, Y]:=-sine;
  Result[Z, Z]:=cosine;
  Result[W, W]:=1;
end;

procedure CreateRotationMatrixY(const Angle : Single; out Result : TZMatrix4f);
var
  Sine, Cosine : Single;
begin
  SinCos(Angle, Sine, Cosine);
  FillChar(Result,SizeOf(Result),0);
  Result[X, X]:=cosine;
  Result[X, Z]:=-sine;
  Result[Y, Y]:=1;
  Result[Z, X]:=sine;
  Result[Z, Z]:=cosine;
  Result[W, W]:=1;
end;

procedure CreateRotationMatrixZ(const Angle : Single; out Result : TZMatrix4f);
var
  Sine, Cosine : Single;
begin
  SinCos(Angle, Sine, Cosine);
  FillChar(Result,SizeOf(Result),0);
  Result[X, X]:=cosine;
  Result[X, Y]:=sine;
  Result[Y, X]:=-sine;
  Result[Y, Y]:=cosine;
  Result[Z, Z]:=1;
  Result[W, W]:=1;
end;

function MatrixMultiply(const M1, M2: TZMatrix4f) : TZMatrix4f;
var I,J : integer;
begin
  for I := 0 to 3 do
    for J := 0 to 3 do
      Result[I,J] := M1[I,0]*M2[0,J] + M1[I,1]*M2[1,J] + M1[I,2]*M2[2,J] + M1[I,3]*M2[3,J];
end;

function CycleToRad(const Cycles: single): single; { Radians := Cycles * 2PI }
begin
  Result := Cycles * (2 * PI);
end;

function Vec2DDistance(const v1,v2 : TZVector2f) : single;
var
  yd,xd : single;
begin
  yd := v2[1] - v1[1];
  xd := v2[0] - v1[0];

  Result := sqrt(yd*yd + xd*xd);
end;

function Ceil(const X: single): single;
var
  F : single;
begin
  F := Frac(X);
  Result := X-F;
  if F>0 then
    Result := Result + 1.0;
end;

//Fastcode project: Floor32_PLR_IA32_1
function Floor(const X: Single): integer;
{$ifdef CPU386}
var
  LOldCW, LNewCW: Word;
  LResult: Integer;
asm
  fnstcw LOldCW
  mov ax, 1111001111111111B
  and ax, LOldCW
  or ax, 0000010000000000B
  mov LNewCW, ax
  fldcw LNewCW
  fld X
  fistp LResult
  mov eax, LResult
  fldcw LOldCW
end;
{$else}
begin
  Result := Integer(Trunc(X));
  if Frac(X) < 0 then
    Dec(Result);
end;
{$endif}


//From Fastcode project: by John O'Harrow and Norbert Juffa
function ArcSin(const X : Single) : Single;
{$ifdef CPU386}
asm
  fld1
  fld    X
  fst    st(2)
  fmul   st(0), st(0)
  fsubp
  fsqrt
  fpatan
end;
{$else}
begin
  Result := ArcTan2(X, Sqrt((1 + X) * (1 - X)))
end;
{$endif}

//From Fastcode project: by John O'Harrow and Norbert Juffa
function ArcCos(const X : Single) : Single;
{$ifdef CPU386}
asm
  fld1
  fld    X
  fst    st(2)
  fmul   st(0), st(0)
  fsubp
  fsqrt
  fxch
  fpatan
end;
{$else}
begin
  Result := ArcTan2(Sqrt((1 + X) * (1 - X)), X);
end;
{$endif}

function Log2(const X : Single) : Single;
{$ifdef CPU386}
asm
  FLD1
  FLD     X
  FYL2X
  FWAIT
end;
{$else}
begin
  Result := {$ifndef FPC}System.{$endif}Math.Log2(X);
end;
{$endif}

function CreateTransform(const Rotation,Scale,Position : TZVector3f) : TZMatrix4f;
var
  Tmp : TZMatrix4f;
begin
  CreateScaleAndTranslationMatrix(Scale, Vector3f(0,0,0), Result);
  //Rotation är i cycles för att då är det lättare att rotera interaktivt i zdesigner
  //0.5 = ett kvarts varv etc
  if Rotation[0]<>0 then
  begin
    CreateRotationMatrixX( CycleToRad(Rotation[0]) ,Tmp);
    Result := MatrixMultiply(Result,Tmp);
  end;
  if Rotation[1]<>0 then
  begin
    CreateRotationMatrixY( CycleToRad(Rotation[1]),Tmp);
    Result := MatrixMultiply(Result,Tmp);
  end;
  if Rotation[2]<>0 then
  begin
    CreateRotationMatrixZ( CycleToRad(Rotation[2]),Tmp);
    Result := MatrixMultiply(Result,Tmp);
  end;
  CreateScaleAndTranslationMatrix(UNIT_XYZ3 ,Position,Tmp);
  Result := MatrixMultiply(Result,Tmp);
end;

end.
