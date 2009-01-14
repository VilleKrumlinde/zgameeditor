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

function PerlinNoise2(X,Y : single) : single;
function PerlinNoise3(X,Y,Z : single) : single;
//function PerlinNoise1D(const X,Persistence : single; Octaves : integer) : single;


function CycleToRad(const Cycles: single): single; { Radians := Cycles * 2PI }

function Power(const Base, Exponent: single): single;
function SmoothStep(const A,B,X : single) : single;
function Clamp(const X,Min,Max : single) : single;

function Random(const Base,Diff : single) : single; overload;

function Min(const A,B : single) : single;
function Max(const A, B: Single): Single;

function Ceil(const X: single): single;
function Floor(const X: single): single;


function ColorFtoB(const C : TZColorf) : integer;


//Matrix-functions. From GL-Scene.
procedure InvertMatrix(var M : TZMatrix4f);
procedure ScaleMatrix(var M : TZMatrix4f; const factor : Single);
procedure VectorTransform(const V: TZVector3f; const M: TZMatrix4f; out Result : TZVector3f);
procedure CreateScaleAndTranslationMatrix(const scale, offset : TZVector3f; out Result : TZMatrix4f);

procedure SinCos(const Theta: Single; out Sin, Cos: Single);
procedure CreateRotationMatrixX(const Angle : Single; out Result : TZMatrix4f);
procedure CreateRotationMatrixY(const Angle : Single; out Result : TZMatrix4f);
procedure CreateRotationMatrixZ(const Angle : Single; out Result : TZMatrix4f);

function MatrixMultiply(const M1, M2: TZMatrix4f) : TZMatrix4f;

function Vec2DDistance(const v1,v2 : TZVector2f) : single;
function LineIntersection2D(const A,B,C,D : TZVector2f;
  out Dist : single;
  out HitPoint : TZVector2f) : boolean;

const
  UNIT_Z3: TZVector3f = (0,0,1);
  UNIT_XYZ3 : TZVector3f = (1,1,1);
  IdentityHmgMatrix: TZMatrix4f = ((1, 0, 0, 0),
                                (0, 1, 0, 0),
                                (0, 0, 1, 0),
                                (0, 0, 0, 1));
  EPSILON  : Single = 1e-40;

implementation

{$ifndef minimal}
uses ZLog;
{$endif}

function ColorFtoB(const C : TZColorf) : integer;
begin
  Result := ( Round( clamp(C.V[3],0,1)*255) shl 24) or
    (Round( clamp(C.V[2],0,1)*255) shl 16) or
    (Round( clamp(C.V[1],0,1)*255) shl 8) or
    (Round( clamp(C.V[0],0,1)*255));
end;


function Min(const A,B : single) : single;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

function Max(const A, B: Single): Single;
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


{function VecMult3(const v1, v2: TZVector3f): TZVector3f;
begin
  Result[0] := v1[0] * v2[0];
  Result[1] := v1[1] * v2[1];
  Result[2] := v1[2] * v2[2];
end;}

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
asm
        FLD    X
        FPTAN
        FSTP   ST(0)      { FPTAN pushes 1.0 after result }
        FWAIT
end;

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
  Result := (PInteger(@V[0])^=0) and (PInteger(@V[1])^=0) and
    (PInteger(@V[2])^=0);
  {$ifndef minimal}
  if Result then
    ZAssert(((abs(V[0])<EPSILON) and (abs(V[1])<EPSILON) and (abs(V[2])<EPSILON)),'VecIsNull3');
  {$endif}
end;

function VecIsNull4(const V : TZVector4f): boolean;
begin
//  Result := (V[0]=0) and (V[1]=0) and (V[2]=0) and (V[3]=0);
  Result := (PInteger(@V[0])^=0) and (PInteger(@V[1])^=0) and
    (PInteger(@V[2])^=0) and (PInteger(@V[3])^=0);
  {$ifndef minimal}
  if Result then
    ZAssert(result=(abs(V[0])<EPSILON) and (abs(V[1])<EPSILON) and (abs(V[2])<EPSILON) and (abs(V[3])<EPSILON),'VecIsNull4');
  {$endif}
end;

function VecIsEqual4(const V1,V2 : TZVector4f): boolean;
begin
  Result := (PInteger(@V1[0])^=PInteger(@V2[0])^) and
    (PInteger(@V1[1])^=PInteger(@V2[1])^) and
    (PInteger(@V1[2])^=PInteger(@V2[2])^) and
    (PInteger(@V1[3])^=PInteger(@V2[3])^);
  {$ifndef minimal}
  ZAssert(Result =
    (V1[0]=V2[0]) and (V1[1]=V2[1]) and (V1[2]=V2[2]) and (V1[3]=V2[3]),
    'VecIsEqual4'
    );
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
asm
        FLD     Y
        FLD     X
        FPATAN
        FWAIT
end;


//Perlin new noise for hardware
//Approx range: -0.3 .. 0.3
function PerlinNoiseHW(X,Y,Z : single) : single;
const
  T : array[0..7] of integer = ($15,$38,$32,$2c,$0d,$13,$07,$2a);
var
  I,J,KK : integer;
  U,V,W : single;
  A : array[0..2] of integer;

  S : single;
  Hi,Lo : integer;

  function B2(N,B : integer) : integer;
  begin
    Result := (N shr B) and 1;
  end;

  function B(I,J,K,B : integer) : integer;
  begin
    Result := T[ (B2(I,B) shl 2) or (B2(J,B) shl 1) or B2(K,B) ];
  end;

  function Shuffle(I,J,K : integer) : integer;
  begin
    Result := B(i,j,k,0) + B(j,k,i,1) + b(k,i,j,2) + b(i,j,k,3) +
      B(j,k,i,4) + b(k,i,j,5) + B(i,j,k,6) + B(J,K,I,7);
  end;

  function K(aa : integer) : single;
  var
    S,P,Q,R : single;
    X,Y,Z,T,Tmp1 : single;
    H, B5,B4,B3,B2,BB : integer;
  begin
    S := (A[0]+A[1]+A[2])/6.0;
    X:= U - A[0]+S;
    Y := V-A[1] + S;
    Z:= W - A[2] +  S;
    T := 0.6 - X*x-Y*y-Z*Z;
    H := Shuffle(I+A[0],J+A[1],KK+A[2]);
    Inc(A[aa]);
    if T<0 then
    begin
      Result := 0;
      Exit;
    end;
    B5 := (H shr 5) and 1;
    B4 := (H shr 4) and 1;
    b3 := (H shr 3) and 1;
    B2 := (H shr 2) and 1;
    BB := h and 3;
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
  //todo: Obs, rutinen buggar på negativa koordinater, då är den ej smooth
{  X := Abs(X);
  Y := Abs(Y);
  Z := Abs(Z);}

  S := (X+Y+Z) * (1/3.0);

  I:= Trunc(X+S);
  J:= Trunc(Y+S);
  KK:= Trunc(Z+S);

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


function PerlinNoise2(X,Y : single) : single;
begin
  //todo: noise tar 1.5 kb exesize
  Result := PerlinNoiseHW(X,Y,0);
//  Result := 0;
end;

function PerlinNoise3(X,Y,Z : single) : single;
begin
  Result := PerlinNoiseHW(X,Y,Z);
end;


//Från math.pas
function IntPower(const Base: Extended; const Exponent: Integer): Extended;
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

function MatrixDetInternal(const a1, a2, a3, b1, b2, b3, c1, c2, c3: Single): Single;
// internal version for the determinant of a 3x3 matrix
begin
  Result:=  a1 * (b2 * c3 - b3 * c2)
          - b1 * (a2 * c3 - a3 * c2)
          + c1 * (a2 * b3 - a3 * b2);
end;

function MatrixDeterminant(const M: TZMatrix4f): Single;
begin
  Result:= M[X, X]*MatrixDetInternal(M[Y, Y], M[Z, Y], M[W, Y], M[Y, Z], M[Z, Z], M[W, Z], M[Y, W], M[Z, W], M[W, W])
          -M[X, Y]*MatrixDetInternal(M[Y, X], M[Z, X], M[W, X], M[Y, Z], M[Z, Z], M[W, Z], M[Y, W], M[Z, W], M[W, W])
          +M[X, Z]*MatrixDetInternal(M[Y, X], M[Z, X], M[W, X], M[Y, Y], M[Z, Y], M[W, Y], M[Y, W], M[Z, W], M[W, W])
          -M[X, W]*MatrixDetInternal(M[Y, X], M[Z, X], M[W, X], M[Y, Y], M[Z, Y], M[W, Y], M[Y, Z], M[Z, Z], M[W, Z]);
end;

procedure AdjointMatrix(var M : TZMatrix4f);
var
   a1, a2, a3, a4,
   b1, b2, b3, b4,
   c1, c2, c3, c4,
   d1, d2, d3, d4: Single;
begin
    a1:= M[X, X]; b1:= M[X, Y];
    c1:= M[X, Z]; d1:= M[X, W];
    a2:= M[Y, X]; b2:= M[Y, Y];
    c2:= M[Y, Z]; d2:= M[Y, W];
    a3:= M[Z, X]; b3:= M[Z, Y];
    c3:= M[Z, Z]; d3:= M[Z, W];
    a4:= M[W, X]; b4:= M[W, Y];
    c4:= M[W, Z]; d4:= M[W, W];

    // row column labeling reversed since we transpose rows & columns
    M[X, X]:= MatrixDetInternal(b2, b3, b4, c2, c3, c4, d2, d3, d4);
    M[Y, X]:=-MatrixDetInternal(a2, a3, a4, c2, c3, c4, d2, d3, d4);
    M[Z, X]:= MatrixDetInternal(a2, a3, a4, b2, b3, b4, d2, d3, d4);
    M[W, X]:=-MatrixDetInternal(a2, a3, a4, b2, b3, b4, c2, c3, c4);

    M[X, Y]:=-MatrixDetInternal(b1, b3, b4, c1, c3, c4, d1, d3, d4);
    M[Y, Y]:= MatrixDetInternal(a1, a3, a4, c1, c3, c4, d1, d3, d4);
    M[Z, Y]:=-MatrixDetInternal(a1, a3, a4, b1, b3, b4, d1, d3, d4);
    M[W, Y]:= MatrixDetInternal(a1, a3, a4, b1, b3, b4, c1, c3, c4);

    M[X, Z]:= MatrixDetInternal(b1, b2, b4, c1, c2, c4, d1, d2, d4);
    M[Y, Z]:=-MatrixDetInternal(a1, a2, a4, c1, c2, c4, d1, d2, d4);
    M[Z, Z]:= MatrixDetInternal(a1, a2, a4, b1, b2, b4, d1, d2, d4);
    M[W, Z]:=-MatrixDetInternal(a1, a2, a4, b1, b2, b4, c1, c2, c4);

    M[X, W]:=-MatrixDetInternal(b1, b2, b3, c1, c2, c3, d1, d2, d3);
    M[Y, W]:= MatrixDetInternal(a1, a2, a3, c1, c2, c3, d1, d2, d3);
    M[Z, W]:=-MatrixDetInternal(a1, a2, a3, b1, b2, b3, d1, d2, d3);
    M[W, W]:= MatrixDetInternal(a1, a2, a3, b1, b2, b3, c1, c2, c3);
end;

procedure ScaleMatrix(var M : TZMatrix4f; const factor : Single);
var
   i : Integer;
begin
   for i:=0 to 3 do begin
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
  det:=MatrixDeterminant(M);
  if Abs(Det)<EPSILON then
    M:=IdentityHmgMatrix
  else
  begin
    AdjointMatrix(M);
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
// EAX contains address of Sin
// EDX contains address of Cos
// Theta is passed over the stack
asm
   FLD  Theta
   FSINCOS
   FSTP DWORD PTR [EDX]    // cosine
   FSTP DWORD PTR [EAX]    // sine
end;

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
begin
  Result[X,X]:=M1[X,X]*M2[X,X]+M1[X,Y]*M2[Y,X]+M1[X,Z]*M2[Z,X]+M1[X,W]*M2[W,X];
  Result[X,Y]:=M1[X,X]*M2[X,Y]+M1[X,Y]*M2[Y,Y]+M1[X,Z]*M2[Z,Y]+M1[X,W]*M2[W,Y];
  Result[X,Z]:=M1[X,X]*M2[X,Z]+M1[X,Y]*M2[Y,Z]+M1[X,Z]*M2[Z,Z]+M1[X,W]*M2[W,Z];
  Result[X,W]:=M1[X,X]*M2[X,W]+M1[X,Y]*M2[Y,W]+M1[X,Z]*M2[Z,W]+M1[X,W]*M2[W,W];
  Result[Y,X]:=M1[Y,X]*M2[X,X]+M1[Y,Y]*M2[Y,X]+M1[Y,Z]*M2[Z,X]+M1[Y,W]*M2[W,X];
  Result[Y,Y]:=M1[Y,X]*M2[X,Y]+M1[Y,Y]*M2[Y,Y]+M1[Y,Z]*M2[Z,Y]+M1[Y,W]*M2[W,Y];
  Result[Y,Z]:=M1[Y,X]*M2[X,Z]+M1[Y,Y]*M2[Y,Z]+M1[Y,Z]*M2[Z,Z]+M1[Y,W]*M2[W,Z];
  Result[Y,W]:=M1[Y,X]*M2[X,W]+M1[Y,Y]*M2[Y,W]+M1[Y,Z]*M2[Z,W]+M1[Y,W]*M2[W,W];
  Result[Z,X]:=M1[Z,X]*M2[X,X]+M1[Z,Y]*M2[Y,X]+M1[Z,Z]*M2[Z,X]+M1[Z,W]*M2[W,X];
  Result[Z,Y]:=M1[Z,X]*M2[X,Y]+M1[Z,Y]*M2[Y,Y]+M1[Z,Z]*M2[Z,Y]+M1[Z,W]*M2[W,Y];
  Result[Z,Z]:=M1[Z,X]*M2[X,Z]+M1[Z,Y]*M2[Y,Z]+M1[Z,Z]*M2[Z,Z]+M1[Z,W]*M2[W,Z];
  Result[Z,W]:=M1[Z,X]*M2[X,W]+M1[Z,Y]*M2[Y,W]+M1[Z,Z]*M2[Z,W]+M1[Z,W]*M2[W,W];
  Result[W,X]:=M1[W,X]*M2[X,X]+M1[W,Y]*M2[Y,X]+M1[W,Z]*M2[Z,X]+M1[W,W]*M2[W,X];
  Result[W,Y]:=M1[W,X]*M2[X,Y]+M1[W,Y]*M2[Y,Y]+M1[W,Z]*M2[Z,Y]+M1[W,W]*M2[W,Y];
  Result[W,Z]:=M1[W,X]*M2[X,Z]+M1[W,Y]*M2[Y,Z]+M1[W,Z]*M2[Z,Z]+M1[W,W]*M2[W,Z];
  Result[W,W]:=M1[W,X]*M2[X,W]+M1[W,Y]*M2[Y,W]+M1[W,Z]*M2[Z,W]+M1[W,W]*M2[W,W];
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

function Floor(const X: single): single;
var
  F : single;
begin
  F := Frac(X);
  Result := X-F;
  if F<0 then
    Result := Result - 1.0;
end;


//-------------------- LineIntersection2D-------------------------
//
//	Given 2 lines in 2D space AB, CD this returns true if an
//	intersection occurs and sets dist to the distance the intersection
//  occurs along AB. Also sets the 2d vector point to the point of
//  intersection
//-----------------------------------------------------------------
function LineIntersection2D(const A,B,C,D : TZVector2f;
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

//From Fastcode project: by John O'Harrow and Norbert Juffa
function ArcSin(const X : Single) : Single;
asm
  fld1
  fld    X
  fst    st(2)
  fmul   st(0), st(0)
  fsubp
  fsqrt
  fpatan
end;

//From Fastcode project: by John O'Harrow and Norbert Juffa
function ArcCos(const X : Single) : Single;
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

//http://freespace.virgin.net/hugo.elias/models/m_perlin.htm

(*
//Todo: använd olika primtal för varje oktav
function Noise1(const X : integer) : single;
//Returns a random value in range -1.0 .. 1.0
var
  N : integer;
begin
  //X = X + Y * 59 + Z * 137
  //N := X * 125313;
  N := (X shl 13) xor X;
  Result := (1.0 - ((N * (N * N * 15731 + 789221) +
    1376312589) and $7FFFFFFF) / 1073741824.0);
end;

function SmoothedNoise1(const X : integer) : single;
begin
  Result := Noise1(x)/2  +  Noise1(x-1)/4  +  Noise1(x+1)/4;
end;

function InterpolatedNoise1(const X : single) : single;
var
  IntX : integer;
  FracX : single;
  V1,V2 : single;
begin
  IntX := Trunc(x);
  FracX := Frac(X);

  V1 := SmoothedNoise1(IntX);
  V2 := SmoothedNoise1(IntX + 1);

  Result := SmoothStep(V1 , V2 , FracX);
end;

function PerlinNoise1D(const X,Persistence : single; Octaves : integer) : single;
var
  Total,Amp,Freq : single;
  I : integer;
begin
  Total := 0;

  for I := 0 to Octaves-1 do
  begin
    Amp := ZMath.Power(Persistence,I);
    Freq := ZMath.Power(2,I);
    Total := Total + InterpolatedNoise1(X * Freq) * Amp;
  end;

  Result := Total;
end;
*)

(*
var store : single;
procedure perlintest;
var
  v,X,min,max,sum,avg : single;
  i : integer;
begin
  x:=42; min:=10; max:=-10; sum:=0;
  for I:=0 to 10000 do
  begin
    v:=perlinnoise3(x,x*3,x*17);
    if v<min then
      min:=v;
    if v>max then
      max:=v;
    sum:=sum+v;
    x:=x + 0.27;
  end;
  avg:=sum/1000;
  store := avg+min+max;
end;

initialization
  perlintest;*)

end.
