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

  //Define a global variable that can be used in expressions
  TDefineVariable = class(TZComponent)
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Value : single;
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

  //Virtual machine instruction baseclass
  TExpBase = class(TZComponent)
  protected
    procedure Execute; virtual; abstract;
  end;

  //Load value of prop to stack
  TExpPropValue = class(TExpBase)
  protected
    procedure Execute; override;
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Source : TZPropertyRef;
  end;

  //Load pointer to prop on stack, used with assign
  TExpPropPtr = class(TExpBase)
  protected
    procedure Execute; override;
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Target : TZPropertyRef;
  end;

  TExpConstant = class(TExpBase)
  protected
    procedure Execute; override;
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Constant : single;
    {$ifndef minimal}
    function GetDisplayName: String; override;
    {$endif}
  end;

{  TExpFunc1Arg = class(TExpProducer)
  protected
    procedure ProduceOutput(Stack: TZArrayList); override;
  public
    Kind : (vukSine,vukCos);
  end;}

  TExpOpBinaryKind = (vbkPlus,vbkMinus,vbkMul,vbkDiv,vbkAssign);

  TExpOpBinary = class(TExpBase)
  protected
    procedure Execute; override;
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Kind : TExpOpBinaryKind;
  end;

  TExpOpJumpKind = (jsJumpAlways,jsJumpLT,jsJumpGT,jsJumpLE,jsJumpGE,jsJumpNE,jsJumpEQ,jsJumpReturn);
  TExpJump = class(TExpBase)
  protected
    procedure Execute; override;
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Kind : TExpOpJumpKind;
    Destination : integer;  //todo could be smallint or byte
  end;

  TExpFuncCallKind = (fcSin,fcSqrt,fcCos,fcAbs,fcRnd,fcFrac,fcExp,
     fcTan,fcCeil,fcFloor,fcAcos,fcAsin,
     fcRandom,fcAtan2,fcNoise2,fcNoise3,fcClamp,fcPow,fcCenterMouse,fcSetRandomSeed);

  TExpFuncCall = class(TExpBase)
  protected
    procedure Execute; override;
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Kind : TExpFuncCallKind;
  end;


//Run a compiled expression
//Uses global vars for state.
procedure RunCode(Code : TZComponentList);

var
  //Return value of last executed expression
  gReturnValue : single;


implementation


uses ZMath,ZPlatform,ZApplication
{$ifndef minimal},ZLog,SysUtils{$endif};

var
  //Expression execution context
  gCurrentPc : integer;
  gStack : TZArrayList;

procedure RunCode(Code : TZComponentList);
var
  Limit : integer;
begin
  //Pc can be modified in jump-code
  gCurrentPc := 0;
  Limit := Code.Count;
  while gCurrentPc<Limit do
  begin
    TExpBase(Code[gCurrentPc]).Execute;
    Inc(gCurrentPc);
  end;
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

{ TExpPropValue }

procedure TExpPropValue.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Source',{$ENDIF}integer(@Source) - integer(Self), zptPropertyRef);
end;

procedure TExpPropValue.Execute;
begin
  gStack.Push( TObject(ZClasses.GetPropertyRef(Source)^) );
end;

{ TExpConstant }

procedure TExpConstant.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Constant',{$ENDIF}integer(@Constant) - integer(Self), zptFloat);
end;

procedure TExpConstant.Execute;
begin
  gStack.Push( TObject(Constant) );
end;

{$ifndef minimal}
function TExpConstant.GetDisplayName: String;
begin
  Result := inherited GetDisplayName + ' ' + FloatToStr(Constant);
end;
{$endif}

{ TExpOpBinary }

procedure TExpOpBinary.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Kind',{$ENDIF}integer(@Kind) - integer(Self), zptByte);
end;

{$ifdef minimal} {$WARNINGS OFF} {$endif}
procedure TExpOpBinary.Execute;
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
    vbkAssign :
      begin
        PFloat(A2)^ := A1;
        Exit; //no result
      end;
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
end;

procedure TExpJump.Execute;
var
  L,R : single;
  Jump : boolean;
begin
  Jump := True;
  case Kind of
    jsJumpAlways : ;
    jsJumpReturn : gReturnValue := gStack.PopFloat;
  else
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

initialization

  //Init vm
  gStack := TZArrayList.Create;
  gStack.ReferenceOnly := True;

  ZClasses.Register(TZExpression,ZExpressionClassId);
    {$ifndef minimal}ComponentManager.LastAdded.ImageIndex:=2;{$endif}
  ZClasses.Register(TDefineVariable,DefineVariableClassId);
    {$ifndef minimal}ComponentManager.LastAdded.ImageIndex:=8;{$endif}
  ZClasses.Register(TDefineConstant,DefineConstantClassId);
    {$ifndef minimal}ComponentManager.LastAdded.ExcludeFromBinary:=True;{$endif}

  ZClasses.Register(TExpConstant,ExpConstantClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NoUserCreate:=True;{$endif}
  ZClasses.Register(TExpOpBinary,ExpOpBinaryClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NoUserCreate:=True;{$endif}
  ZClasses.Register(TExpPropValue,ExpPropValueClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NoUserCreate:=True;{$endif}
  ZClasses.Register(TExpPropPtr,ExpPropPtrClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NoUserCreate:=True;{$endif}
  ZClasses.Register(TExpJump,ExpJumpClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NoUserCreate:=True;{$endif}
  ZClasses.Register(TExpFuncCall,ExpFuncCallClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NoUserCreate:=True;{$endif}

{$ifndef minimal}
finalization
  gStack.Free;
{$endif}

end.
