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

unit ZExpressions;

{
  Expressions.
  Use global proc RunCode(...) to execute code.

  Runtime Virtual Machine
}

interface

uses ZClasses;

type
  //Klass med en expression-prop
  TZExpression = class(TCommand)
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Expression : TZExpressionPropValue;
    Value : single;
    procedure Execute; override;
  end;

  //User-defined functions
  TZLibrary = class(TZComponent)
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Source : TZExpressionPropValue;
  end;

  TDefineVariableBase = class(TZComponent)
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    _Type : (dvbFloat,dvbInt);
  end;


  //Define a global variable that can be used in expressions
  TDefineVariable = class(TDefineVariableBase)
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Value : single;
    IntValue : integer;
  end;

  //Define a global constant that can be used in expressions
  //Value is copied into code, this component is not streamed in final binary
  TDefineConstant = class(TZComponent)
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Value : single;
    {$ifndef minimal}function GetDisplayName: string; override;{$endif}
  end;

  TDefineArray = class(TDefineVariableBase)
  private
    Limit : integer;
    Data : PFloatArray;
    function PopAndGetElement : PFloat;
    procedure CleanUp;
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Dimensions : (dadOne,dadTwo,dadThree);
    SizeDim1,SizeDim2,SizeDim3 : integer;
    destructor Destroy; override;
  end;

  //Virtual machine instruction baseclass
  TExpBase = class(TZComponent)
  protected
    procedure Execute; virtual; abstract;
    {$ifndef minimal}public function ExpAsText : string;{$endif}
  end;

  //Load value of prop to stack
  TExpPropValueBase = class(TExpBase)
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Source : TZPropertyRef;
  end;

  TExpPropValue4 = class(TExpPropValueBase)
  protected
    procedure Execute; override;
  end;

  TExpPropValue1 = class(TExpPropValueBase)
  protected
    procedure Execute; override;
  end;

  //Load pointer to prop on stack, used with assign
  TExpPropPtr = class(TExpBase)
  protected
    procedure Execute; override;
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Target : TZPropertyRef;
  end;

  TExpConstantFloat = class(TExpBase)
  protected
    procedure Execute; override;
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Constant : single;
    {$ifndef minimal}
    function GetDisplayName: String; override;
    {$endif}
  end;

  TExpConstantInt = class(TExpBase)
  protected
    procedure Execute; override;
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Constant : integer;
    {$ifndef minimal}
    function GetDisplayName: String; override;
    {$endif}
  end;

  TExpOpBinaryKind = (vbkPlus,vbkMinus,vbkMul,vbkDiv);

  TExpOpBinaryBase = class(TExpBase)
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Kind : TExpOpBinaryKind;
  end;

  TExpOpBinaryFloat = class(TExpOpBinaryBase)
  protected
    procedure Execute; override;
  end;

  TExpOpBinaryInt = class(TExpOpBinaryBase)
  protected
    procedure Execute; override;
  end;

  TExpOpJumpKind = (jsJumpAlways,jsJumpLT,jsJumpGT,jsJumpLE,jsJumpGE,jsJumpNE,jsJumpEQ);
  TExpJump = class(TExpBase)
  protected
    procedure Execute; override;
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Kind : TExpOpJumpKind;
    Destination : integer;  //todo could be smallint or byte
    _Type : (jutFloat,jutInt);
  end;

  TExpFuncCallKind = (fcSin,fcSqrt,fcCos,fcAbs,fcRnd,fcFrac,fcExp,
     fcTan,fcCeil,fcFloor,fcAcos,fcAsin,fcRound,
     fcRandom,fcAtan2,fcNoise2,fcNoise3,fcClamp,fcPow,fcCenterMouse,fcSetRandomSeed);

  //Built-in function call
  TExpFuncCall = class(TExpBase)
  protected
    procedure Execute; override;
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Kind : TExpFuncCallKind;
  end;

  //Read value from array and push on stack
  TExpArrayRead = class(TExpBase)
  protected
    procedure Execute; override;
    procedure DefineProperties(List: TZPropertyList); override;
  public
    TheArray : TDefineArray;
  end;

  //Push ptr to element in array on stack, used with assign
  TExpArrayWrite = class(TExpBase)
  protected
    procedure Execute; override;
    procedure DefineProperties(List: TZPropertyList); override;
  public
    TheArray : TDefineArray;
  end;

  //Setup local stack frame
  TExpStackFrame = class(TExpBase)
  protected
    procedure Execute; override;
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Size : integer;
  end;

  //Load/store local value
  TExpAccessLocal = class(TExpBase)
  protected
    procedure Execute; override;
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Kind : (loLoad,loStore);
    Index : integer;
  end;

  //Return from function
  TExpReturn = class(TExpBase)
  protected
    procedure Execute; override;
    procedure DefineProperties(List: TZPropertyList); override;
  public
    HasFrame : boolean;
    IsFunction : boolean;      //if false=simple expression
    HasReturnValue : boolean;
    Arguments : integer;
  end;

  TExpMisc = class(TExpBase)
  protected
    procedure Execute; override;
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Kind : (emPop,emDup);
  end;

  TExpUserFuncCall = class(TExpBase)
  protected
    procedure Execute; override;
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Lib : TZLibrary;
    Index : integer;
  end;

  TExpConvertKind = (eckFloatToInt,eckIntToFloat);
  TExpConvert = class(TExpBase)
  protected
    procedure Execute; override;
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Kind : TExpConvertKind;
  end;

  //Assign ptr to 4-byte value, both on stack
  TExpAssign4 = class(TExpBase)
  protected
    procedure Execute; override;
  end;

  //Assign ptr to 1-byte value, both on stack
  TExpAssign1 = class(TExpBase)
  protected
    procedure Execute; override;
  end;

//Run a compiled expression
//Uses global vars for state.
procedure RunCode(Code : TZComponentList);

var
  //Return value of last executed expression
  gReturnValue : single;


implementation


uses ZMath,ZPlatform,ZApplication
{$ifndef minimal},ZLog,SysUtils,Math{$endif};

var
  //Expression execution context
  gCurrentPc : ^TExpBase;
  gCurrentBP : integer;
  gStack : TZArrayList;

procedure RunCode(Code : TZComponentList);
{$ifndef minimal}
var
  GuardLimit : integer;
{$endif}
begin
  //Pc can be modified in jump-code
  if Code.Count=0 then
    Exit;
  gCurrentPc := Code.GetPtrToItem(0);
  gCurrentBP := 0;
  gStack.Clear;
  gStack.Push(nil); //Push return adress nil
  {$ifndef minimal}
  GuardLimit := 10 * 1000000;
  {$endif}
  while True do
  begin
    TExpBase(gCurrentPc^).Execute;
    if gCurrentPc=nil then
       break;
    Inc(gCurrentPc);
    {$ifndef minimal}
    Dec(GuardLimit);
    if GuardLimit=0 then
      ZHalt('Ten million instructions executed. Infinite loop?');
    {$endif}
  end;
  if gStack.Count=1 then
    gReturnValue := gStack.PopFloat;
  {$ifndef minimal}
  if gStack.Count>0 then
    ZLog.GetLog('Zc').Write('Warning, stack not empty on script completion');
  {$endif}
end;

{ TZExpression }

procedure TZExpression.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Expression',{$ENDIF}integer(@Expression) - integer(Self), zptExpression);
end;

procedure TZExpression.Execute;
begin
  ZExpressions.RunCode(Expression.Code);
  Value := ZExpressions.gReturnValue;
//  IsChanged := False;
//  Expression.Code.IsChanged := False;
end;

{ TExpPropValueBase }

procedure TExpPropValueBase.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Source',{$ENDIF}integer(@Source) - integer(Self), zptPropertyRef);
end;

procedure TExpPropValue4.Execute;
begin
  gStack.Push( TObject(ZClasses.GetPropertyRef(Source)^) );
end;

//Load byte value and cast to integer
procedure TExpPropValue1.Execute;
var
  I : integer;
  B : integer;
begin
  B := PByte(ZClasses.GetPropertyRef(Source))^;
  I := B;
  gStack.Push( TObject(I) );
end;

{ TExpConstantFloat }

procedure TExpConstantFloat.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Constant',{$ENDIF}integer(@Constant) - integer(Self), zptFloat);
end;

procedure TExpConstantFloat.Execute;
begin
  gStack.Push( TObject(Constant) );
end;

{$ifndef minimal}
function TExpConstantFloat.GetDisplayName: String;
begin
  Result := inherited GetDisplayName + ' ' + FloatToStr(Constant);
end;
{$endif}

{ TExpConstantInt }

procedure TExpConstantInt.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Constant',{$ENDIF}integer(@Constant) - integer(Self), zptInteger);
end;

procedure TExpConstantInt.Execute;
begin
  gStack.Push( TObject(Constant) );
end;

{$ifndef minimal}
function TExpConstantInt.GetDisplayName: String;
begin
  Result := inherited GetDisplayName + ' ' + IntToStr(Constant);
end;
{$endif}

{ TExpOpBinary }

procedure TExpOpBinaryBase.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Kind',{$ENDIF}integer(@Kind) - integer(Self), zptByte);
end;

{$ifdef minimal} {$WARNINGS OFF} {$endif}
procedure TExpOpBinaryFloat.Execute;
var
  A1,A2,V : single;
begin
  A1 := gStack.PopFloat;
  A2 := gStack.PopFloat;
  case Kind of
    vbkPlus : V := A1 + A2;
    vbkMinus : V := A2 - A1;
    vbkMul : V := A2 * A1;
    vbkDiv : V := A2 / A1;
    {$ifndef minimal}else begin ZHalt('Invalid binary op'); exit; end;{$endif}
  end;
  gStack.Push(TObject(V));
end;
{$ifdef minimal} {$WARNINGS ON} {$endif}

{$ifdef minimal} {$WARNINGS OFF} {$endif}
procedure TExpOpBinaryInt.Execute;
var
  A1,A2,V : integer;
begin
  A1 := Integer(gStack.Pop);
  A2 := Integer(gStack.Pop);
  case Kind of
    vbkPlus : V := A1 + A2;
    vbkMinus : V := A2 - A1;
    vbkMul : V := A2 * A1;
    vbkDiv : V := A2 div A1;
    {$ifndef minimal}else begin ZHalt('Invalid binary op'); exit; end;{$endif}
  end;
  gStack.Push(TObject(V));
end;
{$ifdef minimal} {$WARNINGS ON} {$endif}

{ TExpPropPtr }

procedure TExpPropPtr.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Target',{$ENDIF}integer(@Target) - integer(Self), zptPropertyRef);
end;

procedure TExpPropPtr.Execute;
begin
  gStack.Push(TObject(GetPropertyRef(Target)));
end;

{ TExpJump }

procedure TExpJump.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Kind',{$ENDIF}integer(@Kind) - integer(Self), zptByte);
  List.AddProperty({$IFNDEF MINIMAL}'Destination',{$ENDIF}integer(@Destination) - integer(Self), zptInteger);
  List.AddProperty({$IFNDEF MINIMAL}'Type',{$ENDIF}integer(@_Type) - integer(Self), zptByte);
end;

procedure TExpJump.Execute;
var
  L,R : single;
  Li,Ri : integer;
  Jump : boolean;
begin
  Jump := True;
  case Kind of
    jsJumpAlways : ;
  else
    begin
      if _Type=jutFloat then
      begin
        R := gStack.PopFloat;
        L := gStack.PopFloat;
        case Kind of
          jsJumpLT : Jump := L<R;
          jsJumpGT : Jump := L>R;
          jsJumpLE : Jump := L<=R;
          jsJumpGE : Jump := L>=R;
          jsJumpNE : Jump := L<>R;
          jsJumpEQ : Jump := L=R;
        {$ifndef minimal}else ZHalt('Invalid jump op');{$endif}
        end;
      end else
      begin
        Ri := Integer(gStack.Pop);
        Li := Integer(gStack.Pop);
        case Kind of
          jsJumpLT : Jump := Li<Ri;
          jsJumpGT : Jump := Li>Ri;
          jsJumpLE : Jump := Li<=Ri;
          jsJumpGE : Jump := Li>=Ri;
          jsJumpNE : Jump := Li<>Ri;
          jsJumpEQ : Jump := Li=Ri;
        {$ifndef minimal}else ZHalt('Invalid jump op');{$endif}
        end;
      end;
    end;
  end;
  if Jump then
    Inc(gCurrentPc,Destination);
end;

{ TDefineVariable }

procedure TDefineVariable.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Value',{$ENDIF}integer(@Value) - integer(Self), zptFloat);
    //Variabler är ingen ide att spara, de måste sättas ifrån kod
    List.GetLast.NeverPersist := True;
  List.AddProperty({$IFNDEF MINIMAL}'IntValue',{$ENDIF}integer(@IntValue) - integer(Self), zptInteger);
    List.GetLast.NeverPersist := True;
end;

{ TExpFuncCall }

procedure TExpFuncCall.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Kind',{$ENDIF}integer(@Kind) - integer(Self), zptByte);
end;


{$ifdef minimal} {$WARNINGS OFF} {$endif}
procedure TExpFuncCall.Execute;
var
  V,A1,A2,A3 : single;
begin
  case Kind of
    fcSin :  V := Sin(gStack.PopFloat);
    fcSqrt : V := Sqrt(gStack.PopFloat);
    fcCos : V := Cos(gStack.PopFloat);
    fcAbs : V := Abs(gStack.PopFloat);
    fcRnd : V := System.Random;
    fcFrac : V := Frac(gStack.PopFloat);
    fcExp : V := Exp(gStack.PopFloat);
    fcTan : V := Tan(gStack.PopFloat);
    fcCeil : V := Ceil(gStack.PopFloat);
    fcFloor : V := Floor(gStack.PopFloat);
    fcAcos : V := ArcCos(gStack.PopFloat);
    fcAsin : V := ArcSin(gStack.PopFloat);
    fcRound : V := Round(gStack.PopFloat);

    fcRandom :
      begin
        A2 := gStack.PopFloat; //Variance
        A1 := gStack.PopFloat; //Base
        V := A1 + ((2*System.Random-1.0) * A2);
      end;
    fcAtan2 :
      begin
        A2 := gStack.PopFloat;
        A1 := gStack.PopFloat;
        V := ArcTan2(A1,A2);
      end;
    fcNoise2 :
      begin
        A2 := gStack.PopFloat;
        A1 := gStack.PopFloat;
        V := PerlinNoise2(A1,A2);
      end;
    fcNoise3 :
      begin
        A3 := gStack.PopFloat;
        A2 := gStack.PopFloat;
        A1 := gStack.PopFloat;
        V := PerlinNoise3(A1,A2,A3);
      end;
    fcClamp :
      begin
        A3 := gStack.PopFloat;
        A2 := gStack.PopFloat;
        A1 := gStack.PopFloat;
        V := Clamp(A1,A2,A3);
      end;
    fcPow :
      begin
        A2 := gStack.PopFloat;
        A1 := gStack.PopFloat;
        V := ZMath.Power(A1,A2);
      end;
    fcCenterMouse :
      begin
        V := 0; //todo: does not return a value
        Platform_SetMousePos(ScreenWidth div 2,ScreenHeight div 2);
      end;
    fcSetRandomSeed :
      begin
        V := System.RandSeed; //int to float
        System.RandSeed := Round(gStack.PopFloat); //float to int
      end;
  {$ifndef minimal}else begin ZHalt('Invalid func op'); exit; end;{$endif}
  end;
  gStack.Push(TObject(V));
end;
{$ifdef minimal} {$WARNINGS ON} {$endif}

{ TDefineConstant }

procedure TDefineConstant.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Value',{$ENDIF}integer(@Value) - integer(Self), zptFloat);
   {$ifndef minimal}List.GetLast.NeedRefreshNodeName := True; {$endif}
end;

{$ifndef minimal}
function TDefineConstant.GetDisplayName: string;
begin
  Result := inherited GetDisplayName + ' ' + FormatFloat('###0.#',Value);
end;
{$endif}

{ TExpArrayRead }

procedure TExpArrayRead.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'TheArray',{$ENDIF}integer(@TheArray) - integer(Self), zptComponentRef);
end;

procedure TExpArrayRead.Execute;
var
  V : single;
  P : PFloat;
begin
  P := TheArray.PopAndGetElement;
  {$ifndef minimal}
  if P=nil then
    ZHalt('Array read outside range: ' + TheArray.Name);
  {$endif}
  V := P^;
  gStack.Push( TObject( V ) );
end;

{ TDefineArray }

procedure TDefineArray.CleanUp;
begin
  if Data<>nil then
    FreeMem(Data);
end;

procedure TDefineArray.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Dimensions',{$ENDIF}integer(@Dimensions) - integer(Self), zptByte);
    {$ifndef minimal}List.GetLast.SetOptions(['One','Two','Three']);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'SizeDim1',{$ENDIF}integer(@SizeDim1) - integer(Self), zptInteger);
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'SizeDim2',{$ENDIF}integer(@SizeDim2) - integer(Self), zptInteger);
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'SizeDim3',{$ENDIF}integer(@SizeDim3) - integer(Self), zptInteger);
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
end;

destructor TDefineArray.Destroy;
begin
  CleanUp;
  inherited;
end;

function TDefineArray.PopAndGetElement : PFloat;
var
  Index : integer;
  I1,I2,I3,ByteSize : integer;
begin
  I3 := Integer(gStack.Pop);
  if Self.Dimensions>=dadTwo then
    I2 := Integer(gStack.Pop)
  else
    I2 := 0;
  if Self.Dimensions=dadThree then
    I1 := Integer(gStack.Pop)
  else
    I1 := 0;

  {$ifndef minimal}
  //Array size can only be changed in zdesigner, not runtime
  if Limit<>SizeDim1 * (SizeDim2+1) * (SizeDim3+1) then
  begin
    CleanUp;
    Data := nil;
  end;
  {$endif}

  if Data=nil then
  begin
    Limit := SizeDim1 * (SizeDim2+1) * (SizeDim3+1);;
    ByteSize := Limit*SizeOf(single);
    GetMem(Data, ByteSize );
    FillChar(Data^, ByteSize ,0);
  end;

  case Self.Dimensions of
    dadOne: Index := I3;
    dadTwo: Index := (I2*SizeDim2) + I3;
  else
    Index := (I1*SizeDim2*SizeDim3) + (I2*SizeDim3) + I3;
  end;

  {$ifndef minimal}
  if ((Index<0) or (Index>=Limit)) or
    ((I1<0) or (I2<0) or (I3<0)) or
    ((Dimensions=dadOne) and (I3>=SizeDim1)) or
    ((Dimensions=dadTwo) and ((I2>=SizeDim1) or (I3>=SizeDim2))) or
    ((Dimensions=dadThree) and ((I1>=SizeDim1) or (I2>=SizeDim2) or (I3>=SizeDim3)))
    then
  begin
    {$ifdef zlog}
    ZLog.GetLog(Self.ClassName).Write('Array access outside range: ' + Self.Name + ' ' + IntToStr(I1) + ' ' + IntToStr(I2) + ' ' + IntToStr(I3));
    {$endif}
    Result := nil;
    Exit;
  end;
  {$endif}

  Result := @Data^[ Index ];
end;

{ TExpArrayWrite }

procedure TExpArrayWrite.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'TheArray',{$ENDIF}integer(@TheArray) - integer(Self), zptComponentRef);
end;

procedure TExpArrayWrite.Execute;
var
  P : Pointer;
begin
  P := TheArray.PopAndGetElement;
  {$ifndef minimal}
  if P=nil then
    ZHalt('Array assign outside range: ' + TheArray.Name);
  {$endif}
  gStack.Push(P);
end;

{ TExpStackFrame }

procedure TExpStackFrame.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Size',{$ENDIF}integer(@Size) - integer(Self), zptInteger);
end;

procedure TExpStackFrame.Execute;
//http://en.wikipedia.org/wiki/Function_prologue
var
  I : integer;
begin
  gStack.Push(pointer(gCurrentBP));
  gCurrentBP := gStack.Count;
  //Todo: make ZStack-class with MakeSpace-method
  for I := 0 to Self.Size - 1 do
    gStack.Add(nil);
end;

{ TExpAccessLocal }

procedure TExpAccessLocal.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Kind',{$ENDIF}integer(@Kind) - integer(Self), zptByte);
  List.AddProperty({$IFNDEF MINIMAL}'Index',{$ENDIF}integer(@Index) - integer(Self), zptInteger);
end;

procedure TExpAccessLocal.Execute;
var
  P : ^integer;  //4 byte data
begin
  P := gStack.GetPtrToItem( gCurrentBP + Self.Index );
  case Kind of
    loLoad: gStack.Push(TObject(P^));
    loStore: P^ := integer(gStack.Pop);
  end;
end;

{ TExpReturn }

procedure TExpReturn.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'HasFrame',{$ENDIF}integer(@HasFrame) - integer(Self), zptBoolean);
  List.AddProperty({$IFNDEF MINIMAL}'IsFunction',{$ENDIF}integer(@IsFunction) - integer(Self), zptBoolean);
  List.AddProperty({$IFNDEF MINIMAL}'HasReturnValue',{$ENDIF}integer(@HasReturnValue) - integer(Self), zptBoolean);
  List.AddProperty({$IFNDEF MINIMAL}'Arguments',{$ENDIF}integer(@Arguments) - integer(Self), zptInteger);
end;

{$warnings off}
procedure TExpReturn.Execute;
var
  RetVal : integer;
  I : integer;
begin
  if HasReturnValue then
  begin
    //Local0 holds returnvalue
    RetVal := PInteger( gStack.GetPtrToItem( gCurrentBP ) )^;
  end;

  if HasFrame then
  begin
    while gStack.Count>gCurrentBP do
      gStack.Pop;
    gCurrentBP := integer(gStack.Pop);
  end;

  //Get return adress
  gCurrentPc := pointer(gStack.Pop);

  //Clean stack of function arguments
  for I := 0 to Arguments - 1 do
    gStack.Pop;

  if HasReturnValue then
  begin
    gStack.Push(TObject(RetVal));
  end;
end;
{$warnings on}

{ TExpBase }

{$ifndef minimal}
function TExpBase.ExpAsText: string;
var
  PropList : TZPropertyList;
  Prop : TZProperty;
  Value : TZPropertyValue;
  I : integer;
  S : string;
begin
  Result := Copy(ComponentManager.GetInfo(Self).ZClassName,4,255);
  PropList := Self.GetProperties;
  for I := 3 to PropList.Count-1 do
  begin
    Prop := TZProperty(PropList[I]);
    Self.GetProperty(Prop,Value);
    case Prop.PropertyType of
      zptFloat,zptScalar : S := FloatToStr( RoundTo( Value.FloatValue ,-FloatTextDecimals) );
      zptInteger : S := IntToStr(Value.IntegerValue);
      zptComponentRef : S := Value.ComponentValue.Name;
      zptPropertyRef :
        begin
          S := Value.PropertyValue.Component.Name + ' ' + Value.PropertyValue.Prop.Name;
          if Value.PropertyValue.Index>0 then
            S := S + ' ' + IntToStr(Value.PropertyValue.Index);
        end;
      zptByte : S := IntToStr(Value.ByteValue);
      zptBoolean : S := IntToStr( byte(Value.BooleanValue) );
    else
      S := '';
    end;
    Result:=Result + ' ' + S;
  end;
end;
{$endif}

{ TExpMisc }

procedure TExpMisc.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Kind',{$ENDIF}integer(@Kind) - integer(Self), zptByte);
end;

procedure TExpMisc.Execute;
var
  V : integer;
begin
  case Kind of
    emPop: gStack.Pop;  //Pop, discard value from top of stack
    emDup :
      begin
        V := Integer(gStack.Pop);
        gStack.Push(TObject(V));
        gStack.Push(TObject(V));
      end;
  end;
end;

{ TZLibrary }

procedure TZLibrary.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Source',{$ENDIF}integer(@Source) - integer(Self), zptExpression);
end;

{ TExpUserFuncCall }

procedure TExpUserFuncCall.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Lib',{$ENDIF}integer(@Lib) - integer(Self), zptComponentRef);
  List.AddProperty({$IFNDEF MINIMAL}'Index',{$ENDIF}integer(@Index) - integer(Self), zptInteger);
end;

procedure TExpUserFuncCall.Execute;
begin
  {$ifndef minimal}
  if gStack.Count>10000 then
    ZHalt('Stack overflow in expression.');
  {$endif}
  gStack.Push(TObject(gCurrentPC));
  gCurrentPC := Lib.Source.Code.GetPtrToItem(Index);
  Dec(gCurrentPc);
end;

{ TExpConvert }

procedure TExpConvert.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Kind',{$ENDIF}integer(@Kind) - integer(Self), zptByte);
end;

procedure TExpConvert.Execute;
var
  V : single;
begin
  case Kind of
    eckFloatToInt:
      begin
        gStack.Push(TObject(Trunc(gStack.PopFloat)));
      end;
    eckIntToFloat :
      begin
        V := Integer(gStack.Pop);
        gStack.Push( TObject( V ) );
      end;
  end;
end;

{ TExpAssign4 }

procedure TExpAssign4.Execute;
var
  V : single;
  P : pointer;
begin
  V := gStack.PopFloat;
  P := gStack.Pop;
  PFloat(P)^ := V;
end;

{ TExpAssign1 }

procedure TExpAssign1.Execute;
var
  V : integer;
  B : byte;
  P : pointer;
begin
  //Cast integer to byte before assigning
  V := Integer(gStack.Pop);
  P := gStack.Pop;
  B := V;
  PByte(P)^ := B;
end;

{ TDefineVariableBase }

procedure TDefineVariableBase.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Type',{$ENDIF}integer(@_Type) - integer(Self), zptByte);
    {$ifndef minimal}List.GetLast.SetOptions(['Float','Integer']);{$endif}
end;

initialization

  //Init vm
  gStack := TZArrayList.Create;
  gStack.ReferenceOnly := True;

  ZClasses.Register(TZExpression,ZExpressionClassId);
    {$ifndef minimal}ComponentManager.LastAdded.ImageIndex:=2;{$endif}
  ZClasses.Register(TZLibrary,ZLibraryClassId);
    {$ifndef minimal}ComponentManager.LastAdded.ImageIndex:=2;{$endif}
  ZClasses.Register(TDefineVariable,DefineVariableClassId);
    {$ifndef minimal}ComponentManager.LastAdded.ImageIndex:=8;{$endif}
  ZClasses.Register(TDefineConstant,DefineConstantClassId);
    {$ifndef minimal}ComponentManager.LastAdded.ExcludeFromBinary:=True;{$endif}
  ZClasses.Register(TDefineArray,DefineArrayClassId);
    {$ifndef minimal}ComponentManager.LastAdded.ImageIndex:=8;{$endif}

  ZClasses.Register(TExpConstantFloat,ExpConstantFloatClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NoUserCreate:=True;{$endif}
  ZClasses.Register(TExpConstantInt,ExpConstantIntClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NoUserCreate:=True;{$endif}
  ZClasses.Register(TExpOpBinaryFloat,ExpOpBinaryFloatClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NoUserCreate:=True;{$endif}
  ZClasses.Register(TExpOpBinaryInt,ExpOpBinaryIntClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NoUserCreate:=True;{$endif}
  ZClasses.Register(TExpPropValue4,ExpPropValue4ClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NoUserCreate:=True;{$endif}
  ZClasses.Register(TExpPropValue1,ExpPropValue1ClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NoUserCreate:=True;{$endif}
  ZClasses.Register(TExpPropPtr,ExpPropPtrClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NoUserCreate:=True;{$endif}
  ZClasses.Register(TExpJump,ExpJumpClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NoUserCreate:=True;{$endif}
  ZClasses.Register(TExpFuncCall,ExpFuncCallClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NoUserCreate:=True;{$endif}
  ZClasses.Register(TExpArrayRead,ExpArrayReadClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NoUserCreate:=True;{$endif}
  ZClasses.Register(TExpArrayWrite,ExpArrayWriteClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NoUserCreate:=True;{$endif}
  ZClasses.Register(TExpStackFrame,ExpStackFrameClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NoUserCreate:=True;{$endif}
  ZClasses.Register(TExpAccessLocal,ExpAccessLocalClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NoUserCreate:=True;{$endif}
  ZClasses.Register(TExpReturn,ExpReturnClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NoUserCreate:=True;{$endif}
  ZClasses.Register(TExpMisc,ExpMiscClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NoUserCreate:=True;{$endif}
  ZClasses.Register(TExpUserFuncCall,ExpUserFuncCallClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NoUserCreate:=True;{$endif}
  ZClasses.Register(TExpConvert,ExpConvertClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NoUserCreate:=True;{$endif}
  ZClasses.Register(TExpAssign4,ExpAssign4ClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NoUserCreate:=True;{$endif}
  ZClasses.Register(TExpAssign1,ExpAssign1ClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NoUserCreate:=True;{$endif}

{$ifndef minimal}
finalization
  gStack.Free;
{$endif}

end.
