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

//This unit is the glue between ZExpressions and Zc
//VM code generation
unit ExprEdit;

interface

uses ZClasses,ZExpressions,Classes,uSymTab,SysUtils,Contnrs;

type
  EZcErrorBase = class(Exception);
  ECodeGenError = class(EZcErrorBase);
  EParseError = class(EZcErrorBase)
  public
    Line,Col : integer;
  end;


procedure Compile(ThisC : TZComponent;
  const Ze : TZExpressionPropValue;
  SymTab : TSymbolTable;
  ReturnType : TZcDataType;
  GlobalNames : TObjectList);

function ParsePropRef(SymTab : TSymbolTable;
  ThisC : TZComponent;
  const VarName: string;
  var Ref : TZPropertyRef) : boolean;


var
  CompileDebugString : string;

implementation

uses Zc,Zc_Ops,Dialogs, ZApplication,
  DesignerGUI,CocoBase;


//ThisC = object som är 'this'
function ParsePropRef(SymTab : TSymbolTable;
  ThisC : TZComponent;
  const VarName: string;
  var Ref : TZPropertyRef) : boolean;
var
  I : integer;
  CName,PName,IName : string;
  C : TZComponent;
  Prop : TZProperty;
begin
  Result := False;

  //bryt upp i objektnamn och propnamn, split '.'

  I := Pos('.',VarName);
  if I=0 then
  begin
    CName := '';
    PName := VarName;
  end
  else
  begin
    CName := Copy(VarName,1,I-1);
    PName := Copy(VarName,I+1,200);
  end;

  //Ta ev Propindex
  I := Pos('.',PName);
  if I=0 then
  begin
    Ref.Index := 0;
    Ref.HasPropIndex := False;
  end
  else
  begin
    IName := Copy(PName,I+1,200);
    PName := Copy(PName,1,I-1);
    if Length(IName)<>1 then
      Exit;
    //todo: testa typ av prop och sätt verifiera propindex
    //"color.x" t.ex. ska ej gå
    case Upcase(IName[1]) of
      'X','R' : I := 0;
      'Y','G' : I := 1;
      'Z','B' : I := 2;
      'W','A' : I := 3;
    else
      Exit;
    end;
    Ref.Index := I;
    Ref.HasPropIndex := True;
  end;

  C := nil;
  if (CName='this') then
    C := ThisC
  else if (Length(CName)=0) then
  begin
    if SymTab.Contains(PName) then
    begin
      C := TZComponent(SymTab.Lookup(PName));
      if (C is TDefineVariable) or (C is TDefineConstant) then
      begin
        PName := 'Value';
        case (C as TDefineVariableBase)._Type of
          dvbInt : PName := 'IntValue';
          dvbString : PName := 'StringValue';
        end;
      end else
        C := nil;
    end
    else
      C := ThisC;
  end
  else
  begin
    if SymTab.Contains(CName) then
      C := TZComponent(SymTab.Lookup(CName));
  end;

  if Assigned(C) then
  begin
    Prop := C.GetProperties.GetByName(PName);
    if Assigned(Prop) then
    begin
      Ref.Component := C;
      Ref.Prop := Prop;
      Result := True;
    end;
  end;
end;


type
  TLabelUse = class
  private
    AdrPtr : PInteger;
    AdrPC : integer;
  end;

  TZCodeLabel = class
  private
    Usage : TObjectList;
    Definition : integer;
    constructor Create;
    destructor Destroy; override;
  end;

  TAssignLeaveValueStyle = (alvNone,alvPre,alvPost);

  TZCodeGen = class
  private
    Target : TZComponentList;
    Component : TZComponent;
    SymTab : TSymbolTable;
    Labels : TObjectList;
    LReturn : TZCodeLabel;
    CurrentFunction : TZcOpFunctionUserDefined;
    IsLibrary,IsExternalLibrary : boolean;
    procedure Gen(Op : TZcOp);
    procedure GenJump(Kind : TExpOpJumpKind; Lbl : TZCodeLabel; T : TZcDataType = zctFloat);
    function GetPropRef(const VarName: string; var Ref : TZPropertyRef) : boolean;
    function NewLabel : TZCodeLabel;
    procedure DefineLabel(Lbl : TZCodeLabel);
    procedure ResolveLabels;
    procedure RemoveConstants(StmtList : TList);
    procedure FallTrue(Op : TZcOp; Lbl : TZCodeLabel);
    procedure FallFalse(Op : TZcOp; Lbl : TZCodeLabel);
    procedure GenValue(Op : TZcOp);
    procedure GenFuncCall(Op : TZcOp; NeedReturnValue : boolean);
    procedure GenAssign(Op: TZcOp; LeaveValue : TAssignLeaveValueStyle);
    procedure MakeLiteralOp(const Value: single; Typ: TZcDataType);
    procedure MakeStringLiteralOp(const Value : string);
  public
    procedure GenRoot(StmtList : TList);
    constructor Create;
    destructor Destroy; override;
  end;


function TZCodeGen.GetPropRef(const VarName: string; var Ref : TZPropertyRef) : boolean;
begin
  Result := ParsePropRef(SymTab,Component,VarName,Ref);
  if Result and ((not (Ref.Prop.PropertyType in ZClasses.FloatTypes + [zptString,zptInteger,zptByte,zptBoolean])) or
    Ref.Prop.ExcludeFromBinary) then
    raise ECodeGenError.Create('This type of property can not be used in expressions: ' + VarName);
end;


function MakeBinaryOp(Kind : TExpOpBinaryKind; Typ : TZcDataType) : TExpBase;
begin
  case Typ of
    zctFloat : Result := TExpOpBinaryFloat.Create(nil,Kind);
    zctInt : Result := TExpOpBinaryInt.Create(nil,Kind);
    zctString :
      begin
        if Kind<>vbkPlus then
          raise ECodeGenError.Create('Cannot use this operator on a string-expression');
        Result := TExpStringConCat.Create(nil);
      end;
  else
    raise ECodeGenError.Create('Wrong datatype for binaryop');
  end;
end;

function MakeAssignOp(Size : integer) : TExpBase;
begin
  case Size of
    4 : Result := TExpAssign4.Create(nil);
    1 : Result := TExpAssign1.Create(nil);
  else
    raise ECodeGenError.Create('Wrong datatype for assign');
  end;
end;


procedure TZCodeGen.MakeLiteralOp(const Value : single; Typ : TZcDataType);
begin
  case Typ of
    zctFloat :
      with TExpConstantFloat.Create(Target) do
        Constant := Value;
    zctInt :
      with TExpConstantInt.Create(Target) do
        Constant := Round(Value);
    else
      raise ECodeGenError.Create('Invalid literal');
  end;
end;

procedure TZCodeGen.MakeStringLiteralOp(const Value : string);
var
  C : TExpPropValueBase;
  Con : TExpStringConstant;
begin
  Con := ZApp.AddToConstantPool(Value) as TExpStringConstant;
  C := TExpPropValue4.Create(Target);
  C.Source.Component := Con;
  C.Source.Prop := Con.GetProperties.GetByName('Value');
end;

//Genererar en op som skapar ett värde på stacken
procedure TZCodeGen.GenValue(Op : TZcOp);

  procedure DoGenBinary(Kind : TExpOpBinaryKind);
  begin
    //Assert(Op.Arguments.Count=2);
    GenValue(Op.Child(0));
    GenValue(Op.Child(1));
    Target.AddComponent( MakeBinaryOp(Kind,Op.GetDataType) );
  end;

  procedure DoGenVariableValue;
  var
    C : TExpPropValueBase;
    L : TExpAccessLocal;
    Source : TZPropertyRef;
  begin
    if (Op.Ref<>nil) and
      ((Op.Ref is TZcOpLocalVar) or (Op.Ref is TZcOpArgumentVar))then
    begin
      //Local variable or argument
      L := TExpAccessLocal.Create(Target);
      L.Index := (Op.Ref as TZcOpVariableBase).Ordinal;
      L.Kind := loLoad;
    end else
    begin
      //Property reference
      if not GetPropRef(Op.Id,Source) then
        raise ECodeGenError.Create('Unknown identifier ' + Op.Id);
      if Source.Prop.PropertyType in [zptByte,zptBoolean] then
        C := TExpPropValue1.Create(Target)
      else
        C := TExpPropValue4.Create(Target);
      C.Source := Source;
    end;
  end;

  procedure DoGenBoolean;
  //boolexpr "x<5" generates: if(boolexpr) push(1) else push(0)
  var
    LExit,LFalse : TZCodeLabel;
  begin
    LExit := NewLabel;
    LFalse := NewLabel;
    FallTrue(Op,LFalse);

    //Gen "true" body
    MakeLiteralOp(1, Op.GetDataType);
    //jump to exit
    GenJump(jsJumpAlways,LExit);

    //Gen "false"
    DefineLabel(LFalse);
    MakeLiteralOp(0, Op.GetDataType);

    DefineLabel(LExit);
  end;

  procedure DoGenArrayRead;
  var
    A : TZComponent;
    C : TExpArrayRead;
    I : integer;
  begin
    A := TZComponent(SymTab.Lookup(Op.Id));
    if (A=nil) or (not (A is TDefineArray)) then
      raise ECodeGenError.Create('Identifier is not an array: ' + Op.Id);
    if Ord((A as TDefineArray).Dimensions)+1<>Op.Children.Count then
      raise ECodeGenError.Create('Wrong nr of array indices: ' + Op.ToString);
    for I := 0 to Ord((A as TDefineArray).Dimensions) do
      GenValue(Op.Child(I));
    C := TExpArrayRead.Create(Target);
    C.TheArray := A as TDefineArray;
  end;

  procedure DoGenConvert;
  var
    C : TExpConvert;
    COp : TZcOpConvert;
    Kind : TExpConvertKind;
  begin
    GenValue(Op.Child(0));
    COp := Op As TZcOpConvert;
    Kind := TExpConvertKind(99);
    C := TExpConvert.Create(Target);
    case Cop.Child(0).GetDataType of
      zctFloat :
        case Cop.ToType of
          zctInt: Kind := eckFloatToInt;
        end;
      zctInt :
        case Cop.ToType of
          zctFloat: Kind := eckIntToFloat;
        end;
    end;
    if Ord(Kind)=99 then
      raise ECodeGenError.Create('Invalid conversion operator');
    C.Kind := Kind;
  end;

  procedure DoLiteral;
  begin
    if Op.GetDataType=zctString then
      MakeStringLiteralOp((Op as TZcOpLiteral).StringValue)
    else
      MakeLiteralOp((Op as TZcOpLiteral).Value, Op.GetDataType);
  end;

begin
  case Op.Kind of
    zcMul : DoGenBinary(vbkMul);
    zcDiv : DoGenBinary(vbkDiv);
    zcPlus : DoGenBinary(vbkPlus);
    zcMinus : DoGenBinary(vbkMinus);
    zcConstLiteral : DoLiteral;
    zcIdentifier : DoGenVariableValue;
    zcFuncCall : GenFuncCall(Op,True);
    zcCompLT,zcCompGT,zcCompEQ,
    zcCompNE,zcCompLE,zcCompGE,
    zcAnd, zcOr : DoGenBoolean;
    zcArrayAccess : DoGenArrayRead;
    zcConvert : DoGenConvert;
    zcAssign,zcPreInc,zcPreDec : GenAssign(Op,alvPost);
    zcPostInc,zcPostDec : GenAssign(Op,alvPre);
  else
    raise ECodeGenError.Create('Unsupported operator for value expression: ' + IntToStr(ord(Op.Kind)) );
  end;
end;

procedure TZCodeGen.GenAssign(Op : TZcOp; LeaveValue : TAssignLeaveValueStyle);
//LeaveValue : Optionally leave a value of the assignement on stack.
var
  C,C2 : TExpPropPtr;
  I,LastIndex,AssignSize : integer;

  A : TZComponent;
  Aw : TExpArrayWrite;
  LeftOp,RightOp : TZcOp;
  L : TExpAccessLocal;
begin
  //Left-hand side of the assignment
  LeftOp := Op.Child(0);
  RightOp := Op.Child(1);

  if LeaveValue=alvPre then
    GenValue(LeftOp);

  if (LeftOp.Kind=zcIdentifier) and Assigned(LeftOp.Ref) and
    ((LeftOp.Ref is TZcOpLocalVar) or (LeftOp.Ref is TZcOpArgumentVar))  then
  begin
    //Local variable or argument
    GenValue(RightOp);
    if LeaveValue=alvPost then
      with TExpMisc.Create(Target) do
        Kind := emDup;
    L := TExpAccessLocal.Create(Target);
    L.Index := (LeftOp.Ref as TZcOpVariableBase).Ordinal;
    L.Kind := loStore;
  end
  else if LeftOp.Kind=zcIdentifier then
  begin
    if LeaveValue=alvPost then
      raise ECodeGenError.Create('Assign syntax not supported for this kind of variable');
    C := TExpPropPtr.Create(Target);
    if not GetPropRef(LeftOp.Id,C.Target) then
      raise ECodeGenError.Create('Unknown assigment identifier: ' + LeftOp.Id);
    if C.Target.Prop.IsReadOnly then
      raise ECodeGenError.Create('Cannot assign readonly property identifier: ' + LeftOp.Id);
    if (C.Target.Prop.PropertyType=zptString) and (not C.Target.Prop.IsStringTarget) then
      raise ECodeGenError.Create('Cannot assign readonly property identifier: ' + LeftOp.Id);

    if C.Target.Prop.PropertyType in [zptByte,zptBoolean] then
      AssignSize:=1
    else
      AssignSize:=4;

    GenValue(RightOp);
    Target.AddComponent( MakeAssignOp(AssignSize) );

    //Allow "x.Scale" be shorthand for assign x,y,z individually
    //Note: "x.Scale=0.5" is ok, but "x.Scale+=1" is the same as "x.Scale=x.Scale.X+1"
    if (C.Target.Prop.PropertyType in [zptColorf,zptVector3f,zptRectf]) and (not C.Target.HasPropIndex) then
    begin
      if (Op.Kind<>zcAssign) then
        raise ECodeGenError.Create('Assign syntax not supported: ' + LeftOp.Id);
      if (RightOp.Kind=zcIdentifier) then
      begin
        //((C.Target.Prop.PropertyType in [zptColorf,zptVector3f,zptRectf]) and (not C.Target.HasPropIndex))
      end;

      if C.Target.Prop.PropertyType=zptVector3f then
        LastIndex := 2
      else
        LastIndex := 3;
      for I := 1 to LastIndex do
      begin
        C2 := TExpPropPtr.Create(Target);
        C2.Target := C.Target;
        C2.Target.Index := I;
        GenValue(LeftOp);
        Target.AddComponent( MakeAssignOp(AssignSize) );
      end;
    end;
  end else if LeftOp.Kind=zcArrayAccess then
  begin
    if LeaveValue=alvPost then
      raise ECodeGenError.Create('Assign syntax not supported for this kind of variable');
    A := TZComponent(SymTab.Lookup(LeftOp.Id));
    if (A=nil) or (not (A is TDefineArray)) then
      raise ECodeGenError.Create('Identifier is not an array: ' + LeftOp.Id);
    if Ord((A as TDefineArray).Dimensions)+1<>LeftOp.Children.Count then
      raise ECodeGenError.Create('Wrong nr of array indices: ' + Op.ToString);
    //Assert(Op.Arguments.Count=2);
    for I := 0 to Ord((A as TDefineArray).Dimensions) do
      GenValue(LeftOp.Child(I));
    Aw := TExpArrayWrite.Create(Target);
    Aw.TheArray := A as TDefineArray;
    GenValue(Op.Child(1));
    Target.AddComponent( MakeAssignOp(4) );
  end else
    raise ECodeGenError.Create('Assignment destination must be variable or array: ' + Op.Child(0).Id);

end;

procedure TZCodeGen.Gen(Op : TZcOp);
var
  I : integer;

  procedure DoGenIf;
  var
    LExit,LElse : TZCodeLabel;
    HasElse : boolean;
  begin
    HasElse := Assigned(Op.Child(2));
    LExit := NewLabel;
    if HasElse then
    begin
      LElse := NewLabel;
      FallTrue(Op.Child(0),LElse);
    end
    else
    begin
      LElse := nil;
      FallTrue(Op.Child(0),LExit);
    end;
    //Gen "then" body
    Gen(Op.Child(1));
    if HasElse then
    begin //ELSE
      //Write jump past else-body for then-body
      GenJump(jsJumpAlways,LExit);
      DefineLabel(LElse);
      //Gen else-body
      Gen(Op.Child(2));
    end;
    DefineLabel(LExit);
  end;

  procedure DoGenForLoop;
  var
    LExit,LLoop : TZCodeLabel;
  begin
    //Children: [ForInitOp,ForCondOp,ForIncOp,ForBodyOp]
    if Assigned(Op.Child(0)) then
      Gen(Op.Child(0));

    LExit := NewLabel;

    LLoop := NewLabel;
    DefineLabel(LLoop);

    if Assigned(Op.Child(1)) then
      FallTrue(Op.Child(1),LExit);

    if Assigned(Op.Child(3)) then
      Gen(Op.Child(3));

    if Assigned(Op.Child(2)) then
      Gen(Op.Child(2));

    GenJump(jsJumpAlways,LLoop);

    DefineLabel(LExit);
  end;

  procedure DoWhile;
  var
    LExit,LLoop : TZCodeLabel;
  begin
    //Children: [WhileCondOp,WhileBodyOp]
    LExit := NewLabel;

    LLoop := NewLabel;
    DefineLabel(LLoop);

    if Assigned(Op.Child(0)) then
      FallTrue(Op.Child(0),LExit);

    if Assigned(Op.Child(1)) then
      Gen(Op.Child(1));

    GenJump(jsJumpAlways,LLoop);

    DefineLabel(LExit);
  end;

  procedure DoGenReturn;
  var
    L : TExpAccessLocal;
  begin
    //"return x", generate value + jump to exit
    if not Assigned(LReturn) then
      //Global label shared for all return statements
      LReturn := NewLabel;
    if CurrentFunction.ReturnType<>zctVoid then
    begin
      GenValue(Op.Child(0));
      //Store return value in local0
      L := TExpAccessLocal.Create(Target);
      L.Index := 0;
      L.Kind := loStore;
    end;
    GenJump(jsJumpAlways,LReturn);
  end;

  procedure DoGenFunction(Func : TZcOpFunctionUserDefined);
  var
    I : integer;
    Frame : TExpStackFrame;
    Ret : TExpReturn;
  begin
    if IsLibrary then
    begin
      Func.Lib := Component as TZLibrary;
      Func.LibIndex := Target.Count;
    end;
    if IsExternalLibrary then
    begin
      Func.IsExternal := True;
      if Func.Statements.Count>0 then
        raise ECodeGenError.Create('External functions definitions can not have a body: ' + Func.Id );
      Func.ExtLib := Component as TExternalLibrary;
    end;
    Self.CurrentFunction := Func;
    if Func.NeedFrame then
    begin
      Frame := TExpStackFrame.Create(Target);
      Frame.Size := Func.GetStackSize;
    end;
    for I := 0 to Func.Statements.Count - 1 do
    begin
      Gen(Func.Statements[I] as TZcOp);
    end;
    if Assigned(LReturn) then
    begin
      DefineLabel(LReturn);
      LReturn := nil;
    end;
    Ret := TExpReturn.Create(Target);
    Ret.HasFrame := Func.NeedFrame;
    Ret.HasReturnValue := Func.ReturnType<>zctVoid;
    Ret.Arguments := Func.Arguments.Count;
  end;

begin
  case Op.Kind of
    zcAssign,zcPreInc,zcPreDec,zcPostDec,zcPostInc : GenAssign(Op,alvNone);
    zcIf : DoGenIf;
    zcNop : ;
    zcBlock :
      for I := 0 to Op.Children.Count-1 do
        Gen(Op.Child(I));
    zcReturn : DoGenReturn;
    zcFuncCall : GenFuncCall(Op,False);
    zcFunction : DoGenFunction(Op as TZcOpFunctionUserDefined);
    zcForLoop : DoGenForLoop;
    zcWhile : DoWhile;
  else
    raise ECodeGenError.Create('Unsupported operator: ' + IntToStr(ord(Op.Kind)) );
  end;
end;

destructor TZCodeGen.Destroy;
begin
  Labels.Free;
  inherited;
end;

constructor TZCodeGen.Create;
begin
  Labels := TObjectList.Create;
end;

procedure TZCodeGen.DefineLabel(Lbl: TZCodeLabel);
begin
  if Lbl.Definition<>-1 then
    raise ECodeGenError.Create('Label already defined');
  Lbl.Definition := Target.Count;
end;

function TZCodeGen.NewLabel: TZCodeLabel;
begin
  Result := TZCodeLabel.Create;
  Result.Definition := -1;
  Labels.Add(Result);
end;

procedure TZCodeGen.GenJump(Kind: TExpOpJumpKind; Lbl: TZCodeLabel; T : TZcDataType = zctFloat);
var
  Op : TExpJump;
  U : TLabelUse;
begin
  Op := TExpJump.Create(Target);
  Op.Kind := Kind;
  case T of
    zctFloat: Op._Type := jutFloat;
    zctInt: Op._Type := jutInt;
    zctString:
      begin
        Op._Type := jutString;
        if not (Kind in [jsJumpNE,jsJumpEQ]) then
          raise ECodeGenError.Create('Invalid string comparison');
      end
  else
    raise ECodeGenError.Create('Invalid datatype for jump');
  end;
  U := TLabelUse.Create;
  U.AdrPtr := @Op.Destination;
  U.AdrPC := Target.Count-1;
  Lbl.Usage.Add( U );
end;

procedure TZCodeGen.GenRoot(StmtList: TList);
var
  I : integer;
begin
  IsLibrary := Component is TZLibrary;
  IsExternalLibrary := Component is TExternalLibrary;
  RemoveConstants(StmtList);
  for I := 0 to StmtList.Count-1 do
    Gen(StmtList[I]);
  ResolveLabels;
end;

procedure TZCodeGen.ResolveLabels;
var
  I,J,Adr : integer;
  Lbl : TZCodeLabel;
  U : TLabelUse;
begin
  for I := 0 to Labels.Count-1 do
  begin
    Lbl := TZCodeLabel(Labels[I]);
    if Lbl.Definition=-1 then
      raise ECodeGenError.Create('Label with missing definition');
    for J := 0 to Lbl.Usage.Count-1 do
    begin
      U := TLabelUse(Lbl.Usage[J]);
      Adr := Lbl.Definition - U.AdrPC - 1;
      U.AdrPtr^ := Adr;
    end;
  end;
end;

//Fall igenom om false, annars hoppa till Lbl
procedure TZCodeGen.FallFalse(Op: TZcOp; Lbl: TZCodeLabel);

  procedure DoGenComp(Kind : TExpOpJumpKind);
  begin
    //Assert(Op.Arguments.Count=2);
    GenValue(Op.Child(0));
    GenValue(Op.Child(1));
    GenJump(Kind,Lbl,Op.Child(0).GetDataType);
  end;

  procedure DoGenAnd;
  var
    LAnd : TZCodeLabel;
  begin
    LAnd := NewLabel;
    FallTrue(Op.Child(0),LAnd);
    FallFalse(Op.Child(1),Lbl);
    DefineLabel(LAnd);
  end;

  procedure DoGenOr;
  begin
    FallFalse(Op.Child(0),Lbl);
    FallFalse(Op.Child(1),Lbl);
  end;

  procedure DoGenValue;
  begin
    //if(1) blir: value,0, compare and jump
    GenValue(Op);
    MakeLiteralOp(0, Op.GetDataType);
    GenJump(jsJumpNE,Lbl, Op.GetDataType);
  end;

begin
  case Op.Kind of
    zcCompLT : DoGenComp(jsJumpLT);
    zcCompGT : DoGenComp(jsJumpGT);
    zcCompEQ : DoGenComp(jsJumpEQ);
    zcCompNE : DoGenComp(jsJumpNE);
    zcCompLE : DoGenComp(jsJumpLE);
    zcCompGE : DoGenComp(jsJumpGE);
    zcAnd : DoGenAnd;
    zcOr : DoGenOr;
    zcNot : FallTrue(Op.Child(0),Lbl);
  else
    //zcConst,zcIdentifier,zcFuncCall etc
    DoGenValue;
  end;
end;

//Fall igenom om true, annars hoppa till Lbl
procedure TZCodeGen.FallTrue(Op: TZcOp; Lbl: TZCodeLabel);

  procedure DoGenComp(Kind : TExpOpJumpKind);
  begin
    //Assert(Op.Arguments.Count=2);
    GenValue(Op.Child(0));
    GenValue(Op.Child(1));
    GenJump(Kind,Lbl,Op.Child(0).GetDataType);
  end;

  procedure DoGenAnd;
  begin
    FallTrue(Op.Child(0),Lbl);
    FallTrue(Op.Child(1),Lbl);
  end;

  procedure DoGenOr;
  var
    LOr : TZCodeLabel;
  begin
    LOr := NewLabel;
    FallFalse(Op.Child(0),LOr);
    FallTrue(Op.Child(1),Lbl);
    DefineLabel(LOr);
  end;

  procedure DoGenValue;
  begin
    //if(1) blir: value,0, compare and jump
    GenValue(Op);
    MakeLiteralOp(0, Op.GetDataType);
    GenJump(jsJumpEQ,Lbl,Op.GetDataType);
  end;

begin
  case Op.Kind of
    //Generera varje jämförelses motsats
    zcCompLT : DoGenComp(jsJumpGE);
    zcCompGT : DoGenComp(jsJumpLE);
    zcCompEQ : DoGenComp(jsJumpNE);
    zcCompNE : DoGenComp(jsJumpEQ);
    zcCompLE : DoGenComp(jsJumpGT);
    zcCompGE : DoGenComp(jsJumpLT);
    zcAnd : DoGenAnd;
    zcOr : DoGenOr;
    zcNot : FallFalse(Op.Child(0),Lbl);
  else
    //zcConst,zcIdentifier,zcFuncCall etc
    DoGenValue;
  end;
end;

procedure TZCodeGen.GenFuncCall(Op: TZcOp; NeedReturnValue : boolean);

  procedure DoGenBuiltInFunc(Func : TZcOpFunctionBuiltIn);
  var
    I : integer;
    F : TExpFuncCall;
    SF : TExpStringFuncCall;
  begin
    if NeedReturnValue and (Func.ReturnType=zctVoid) then
      raise ECodeGenError.Create('Function in expression must return a value: ' + Op.Id);
    if Op.Children.Count<>Func.Arguments.Count then
      raise ECodeGenError.Create('Invalid nr of arguments: ' + Op.Id);
    for I := 0 to Func.Arguments.Count-1 do
      GenValue(Op.Child(I));
    if Func.FuncId in [fcIntToStr,fcSubStr,fcChr] then
    begin
      SF := TExpStringFuncCall.Create(Target);
      SF.Kind := Func.FuncId;
    end else
    begin
      F := TExpFuncCall.Create(Target);
      F.Kind := Func.FuncId;
    end;
    if (not NeedReturnValue) and (Func.ReturnType<>zctVoid) then
      //discard return value from stack
      with TExpMisc.Create(Target) do
        Kind := emPop;
  end;

  procedure DoGenUserFunc(UserFunc : TZcOpFunctionUserDefined);
  var
    I : integer;
    F : TExpUserFuncCall;
    FE : TExpExternalFuncCall;
  begin
    if NeedReturnValue and (UserFunc.ReturnType=zctVoid) then
      raise ECodeGenError.Create('Function in expression must return a value: ' + Op.Id);
    if Op.Children.Count<>UserFunc.Arguments.Count then
      raise ECodeGenError.Create('Invalid nr of arguments: ' + Op.Id);
    for I := 0 to UserFunc.Arguments.Count-1 do
      GenValue(Op.Child(I));

    if UserFunc.IsExternal then
    begin
      FE := TExpExternalFuncCall.Create(Target);
      FE.Lib := UserFunc.ExtLib;
      FE.SetString('FuncName',AnsiString(UserFunc.Id));
      FE.ArgCount := UserFunc.Arguments.Count;
      FE.HasReturnValue := UserFunc.ReturnType<>zctVoid;
    end
    else
    begin
      F := TExpUserFuncCall.Create(Target);
      F.Lib := UserFunc.Lib;
      F.Index := UserFunc.LibIndex;
    end;

    if (not NeedReturnValue) and (UserFunc.ReturnType<>zctVoid) then
      //discard return value from stack
      with TExpMisc.Create(Target) do
        Kind := emPop;
  end;

begin
  Assert(Op.Kind=zcFuncCall);
  if SymTab.Contains(Op.Id) and (SymTab.Lookup(Op.Id) is TZcOpFunctionUserDefined) then
  begin
    DoGenUserFunc(SymTab.Lookup(Op.Id) as TZcOpFunctionUserDefined);
  end else if SymTab.Contains(Op.Id) and (SymTab.Lookup(Op.Id) is TZcOpFunctionBuiltIn) then
  begin
    DoGenBuiltInFunc(SymTab.Lookup(Op.Id) as TZcOpFunctionBuiltIn);
  end else raise ECodeGenError.Create('Unknown function: ' + Op.Id);
end;


//Replace references to DefineConstant.Value with the actual constant value.
//Then reoptimze nodes to fold constants.
procedure TZCodeGen.RemoveConstants(StmtList : TList);
var
  I : integer;
  Op : TZcOp;

  function DoRemoveConstants(Op: TZcOp) : TZcOp;
  var
    P : TZPropertyRef;
    Value : TZPropertyValue;
    I : integer;
  begin
    Result := Op;
    if Op.Kind=Zc_Ops.zcIdentifier then
    begin
      if GetPropRef(Op.Id,P) then
      begin
        P.Component.GetProperty(P.Prop,Value);
        if P.Component is TDefineConstant then
        begin
          case P.Prop.PropertyType of
            zptFloat : Result := TZcOpLiteral.Create(zctFloat,Value.FloatValue);
            zptInteger : Result := TZcOpLiteral.Create(zctInt,Value.IntegerValue);
            zptString : Result := TZcOpLiteral.Create(zctString,'"' + String(Value.StringValue) + '"');
          else
            Assert(False);
          end;
        end;
      end;
    end else if Op.Kind=Zc_Ops.zcFunction then
    begin
      for I := 0 to (Op as TZcOpFunctionBase).Statements.Count-1 do
        TZcOpFunctionBase(Op).Statements[I] := DoRemoveConstants(TZcOpFunctionBase(Op).Statements[I] as TZcOp);
    end else
    begin
      for I := 0 to Op.Children.Count-1 do
        if Assigned(Op.Child(I)) then
          Op.Children[I] := DoRemoveConstants(Op.Child(I));
    end;
  end;

begin
  for I := 0 to StmtList.Count-1 do
  begin
    Op := StmtList[I];
    Op := DoRemoveConstants(Op);
    Op := Op.Optimize;
    StmtList[I] := Op;
  end;
end;

{ TZCodeLabel }

constructor TZCodeLabel.Create;
begin
  Usage := TObjectList.Create;
end;

destructor TZCodeLabel.Destroy;
begin
  Usage.Free;
  inherited;
end;

//////////////////////////



procedure Compile(ThisC : TZComponent; const Ze : TZExpressionPropValue;
  SymTab : TSymbolTable; ReturnType : TZcDataType;
  GlobalNames : TObjectList);
var
  Compiler : TZc;
  CodeGen : TZCodeGen;
  I : integer;
  Err,S : string;
  Target : TZComponentList;
  PError : EParseError;
  Error : TCocoError;
  AllowFuncDefs : boolean;
begin
  //allow function definitions if compiling a library
  AllowFuncDefs := (ThisC is TZLibrary) or (ThisC is TExternalLibrary);

  S := Ze.Source;
  Target := Ze.Code;

  CompilerContext.SymTab := SymTab;
  CompilerContext.ThisC := ThisC;

  Compiler := TZc.Create(nil);
  try
    Compiler.SymTab := SymTab;
    Compiler.ReturnType := ReturnType;
    Compiler.GlobalNames := GlobalNames;
    Compiler.SourceStream.Write(S[1],Length(S)*SizeOf(Char));
    Compiler.AllowFunctions := AllowFuncDefs;
    Compiler.Execute;
    if not Compiler.Successful then
    begin
      I := Compiler.ListStream.Size;
      SetLength(Err,I);
      Compiler.ListStream.Read(Err[1],I);
      PError := EParseError.Create(Err);
      if Compiler.ErrorList.Count>0 then
      begin
        Error := TCocoError(Compiler.ErrorList[0]);
        PError.Message := Compiler.ErrorStr(Error.ErrorCode,Error.Data);
        PError.Line := TCocoError(Compiler.ErrorList[0]).Line;
        PError.Col := TCocoError(Compiler.ErrorList[0]).Col;
      end;
      raise PError;
      //ShowMessage( Err );
      //Exit;
    end;

    Target.Clear;
    CodeGen := TZCodeGen.Create;
    try
      CodeGen.Target := Target;
      CodeGen.Component := ThisC;
      CodeGen.SymTab := SymTab;
      try
        CodeGen.GenRoot(Compiler.ZFunctions);
      except
        //Om något går fel under kodgenereringen så rensa koden så att den inte körs
        Target.Clear;
        raise;
      end;

      //Show tree as source-code for debugging
      CompileDebugString := '';
      for I := 0 to Compiler.ZFunctions.Count-1 do
        CompileDebugString := CompileDebugString + (Compiler.ZFunctions[I] as TZcOp).ToString + #13#10;

    finally
      CodeGen.Free;
    end;
  finally
    Compiler.Free;
  end;

end;

end.
