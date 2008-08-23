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

uses ZClasses,ZExpressions,Classes,uSymTab,SysUtils;

type
  EZcErrorBase = class(Exception);
  ECodeGenError = class(EZcErrorBase);
  EParseError = class(EZcErrorBase)
  public
    Line,Col : integer;
  end;


procedure Compile(ThisC : TZComponent; const Ze : TZExpressionPropValue; Root : TZComponent; SymTab : TSymbolTable);

function ParsePropRef(SymTab : TSymbolTable;
  ThisC : TZComponent;
  const VarName: string;
  var Ref : TZPropertyRef) : boolean;


implementation

uses Zc,Dialogs,
  DesignerGUI,CocoBase,Contnrs;


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
        PName := 'Value'
      else
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

  TZCodeGen = class
  private
    Target : TZComponentList;
    Root : TZComponent;
    Component : TZComponent;
    SymTab : TSymbolTable;
    Labels : TObjectList;
    LReturn : TZCodeLabel;
    procedure Gen(Op : TZcOp);
    procedure GenJump(Kind : TExpOpJumpKind; Lbl : TZCodeLabel);
    function GetPropRef(const VarName: string; var Ref : TZPropertyRef) : boolean;
    function NewLabel : TZCodeLabel;
    procedure DefineLabel(Lbl : TZCodeLabel);
    procedure ResolveLabels;
    procedure RemoveConstants(StmtList : TList);
    procedure FallTrue(Op : TZcOp; Lbl : TZCodeLabel);
    procedure FallFalse(Op : TZcOp; Lbl : TZCodeLabel);
    procedure GenValue(Op : TZcOp);
    procedure GenFuncCall(Op : TZcOp; NeedReturnValue : boolean);
  public
    procedure GenRoot(StmtList : TList);
    constructor Create;
    destructor Destroy; override;
  end;


function TZCodeGen.GetPropRef(const VarName: string; var Ref : TZPropertyRef) : boolean;
begin
  Result := ParsePropRef(SymTab,Component,VarName,Ref);
  if Result and (not (Ref.Prop.PropertyType in ZClasses.FloatTypes)) then
    raise ECodeGenError.Create('Only float type properties can be used: ' + VarName);
end;


function MakeBinaryOp(Kind : TExpOpBinaryKind) : TExpOpBinary;
begin
  Result := TExpOpBinary.Create(nil);
  Result.Kind := Kind;
end;

function MakeConstOp(const Value : single) : TExpConstant;
begin
  Result := TExpConstant.Create(nil);
  Result.Constant := Value;
end;


//Genererar en op som skapar ett värde på stacken
procedure TZCodeGen.GenValue(Op : TZcOp);

  procedure DoGenBinary(Kind : TExpOpBinaryKind);
  begin
    //Assert(Op.Arguments.Count=2);
    GenValue(Op.Child(0));
    GenValue(Op.Child(1));
    Target.AddComponent( MakeBinaryOp(Kind) );
  end;

  procedure DoGenPropValue;
  var
    C : TExpPropValue;
  begin
    C := TExpPropValue.Create(Target);
    if not GetPropRef(Op.Id,C.Source) then
      raise ECodeGenError.Create('Unknown identifier ' + Op.Id);
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
    Target.AddComponent( MakeConstOp(1) );
    //jump to exit
    GenJump(jsJumpAlways,LExit);

    //Gen "false"
    DefineLabel(LFalse);
    Target.AddComponent( MakeConstOp(0) );

    DefineLabel(LExit);
  end;

  procedure DoGenArrayRead;
  var
    A : TZComponent;
    C : TExpArrayRead;
  begin
    A := TZComponent(SymTab.Lookup(Op.Id));
    if (A=nil) or (not (A is TDefineArray)) then
      raise ECodeGenError.Create('Identifier is not an array: ' + Op.Id);
    //Assert(Op.Arguments.Count=2);
    GenValue(Op.Child(0));
    GenValue(Op.Child(1));
    C := TExpArrayRead.Create(nil);
    C.TheArray := A as TDefineArray;
    Target.AddComponent( C );
  end;

begin
  case Op.Kind of
    zcMul : DoGenBinary(vbkMul);
    zcDiv : DoGenBinary(vbkDiv);
    zcPlus : DoGenBinary(vbkPlus);
    zcMinus : DoGenBinary(vbkMinus);
    zcConst : Target.AddComponent( MakeConstOp(Op.Value) );
    zcIdentifier : DoGenPropValue;
    zcFuncCall : GenFuncCall(Op,True);
    zcCompLT,zcCompGT,zcCompEQ,
    zcCompNE,zcCompLE,zcCompGE,
    zcAnd, zcOr : DoGenBoolean;
    zcArrayAccess : DoGenArrayRead;
  else
    raise ECodeGenError.Create('Unsupported operator for value expression: ' + IntToStr(ord(Op.Kind)) );
  end;
end;


procedure TZCodeGen.Gen(Op : TZcOp);
var
  I : integer;

  procedure DoGenAssign;
  var
    C,C2 : TExpPropPtr;
    I,LastIndex : integer;

    A : TZComponent;
    Aw : TExpArrayWrite;
  begin
    if Op.Child(0).Kind=zcIdentifier then
    begin
      C := TExpPropPtr.Create(Target);
      if not GetPropRef(Op.Child(0).Id,C.Target) then
        raise ECodeGenError.Create('Unknown assigment identifier: ' + Op.Child(0).Id);
      if C.Target.Prop.IsReadOnly then
        raise ECodeGenError.Create('Cannot assign readonly property identifier: ' + Op.Child(0).Id);
      GenValue(Op.Child(1));
      Target.AddComponent( MakeBinaryOp(vbkAssign) );

      //Allow "x.Scale" be shorthand for assign x,y,z individually
      //Note: "x.Scale=0.5" is ok, but "x.Scale+=1" is the same as "x.Scale=x.Scale.X+1"
      if (C.Target.Prop.PropertyType in [zptColorf,zptVector3f,zptRectf]) and (not C.Target.HasPropIndex) then
      begin
        if C.Target.Prop.PropertyType=zptVector3f then
          LastIndex := 2
        else
          LastIndex := 3;
        for I := 1 to LastIndex do
        begin
          C2 := TExpPropPtr.Create(Target);
          C2.Target := C.Target;
          C2.Target.Index := I;
          GenValue(Op.Child(0));
          Target.AddComponent( MakeBinaryOp(vbkAssign) );
        end;
      end;
    end else if Op.Child(0).Kind=zcArrayAccess then
    begin
      A := TZComponent(SymTab.Lookup(Op.Child(0).Id));
      if (A=nil) or (not (A is TDefineArray)) then
        raise ECodeGenError.Create('Identifier is not an array: ' + Op.Child(0).Id);
      //Assert(Op.Arguments.Count=2);
      GenValue(Op.Child(0).Child(0));
      GenValue(Op.Child(0).Child(1));
      Aw := TExpArrayWrite.Create(Target);
      Aw.TheArray := A as TDefineArray;
      Target.AddComponent( MakeBinaryOp(vbkAssign) );
    end else
      raise ECodeGenError.Create('Assignment destination must be variable or array: ' + Op.Child(0).Id);

  end;

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

  procedure DoGenReturn;
  begin
    //"return x", generate value + jump to exit
    if not Assigned(LReturn) then
      //Global label shared for all return statements
      LReturn := NewLabel;
    GenValue(Op.Child(0));
    GenJump(jsJumpReturn,LReturn);
  end;

begin
  case Op.Kind of
    zcAssign : DoGenAssign;
    zcIf : DoGenIf;
    zcNop : ;
    zcBlock :
      for I := 0 to Op.Children.Count-1 do
        Gen(Op.Child(I));
    zcReturn : DoGenReturn;
    zcFuncCall : GenFuncCall(Op,False);
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
  Lbl.Definition := Target.Count;
end;

function TZCodeGen.NewLabel: TZCodeLabel;
begin
  Result := TZCodeLabel.Create;
  Labels.Add(Result);
end;

procedure TZCodeGen.GenJump(Kind: TExpOpJumpKind; Lbl: TZCodeLabel);
var
  Op : TExpJump;
  U : TLabelUse;
begin
  Op := TExpJump.Create(Target);
  Op.Kind := Kind;
  U := TLabelUse.Create;
  U.AdrPtr := @Op.Destination;
  U.AdrPC := Target.Count-1;
  Lbl.Usage.Add( U );
end;

procedure TZCodeGen.GenRoot(StmtList: TList);
var
  I : integer;
begin
  RemoveConstants(StmtList);
  for I := 0 to StmtList.Count-1 do
    Gen(StmtList[I]);
  if Assigned(LReturn) then
    DefineLabel(LReturn);
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
    if Lbl.Definition=0 then
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
    GenJump(Kind,Lbl);
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
    Target.AddComponent( MakeConstOp(0) );
    GenJump(jsJumpNE,Lbl);
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
    GenJump(Kind,Lbl);
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
    Target.AddComponent( MakeConstOp(0) );
    GenJump(jsJumpEQ,Lbl);
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
  else
    //zcConst,zcIdentifier,zcFuncCall etc
    DoGenValue;
  end;
end;

procedure TZCodeGen.GenFuncCall(Op: TZcOp; NeedReturnValue : boolean);

  procedure DoGenFunc(Kind : TExpFuncCallKind; ArgCount : integer = 1; HasReturnValue : boolean = True);
  var
    I : integer;
    F : TExpFuncCall;
  begin
    if NeedReturnValue and (not HasReturnValue) then
      raise ECodeGenError.Create('Function in expression must return a value: ' + Op.Id);
    if Op.Children.Count<>ArgCount then
      raise ECodeGenError.Create('Invalid nr of arguments: ' + Op.Id);
    for I := 0 to ArgCount-1 do
      GenValue(Op.Child(I));
    F := TExpFuncCall.Create(Target);
    F.Kind := Kind;
    //todo: if !needreturnvalue generate code to discard return value from stack
  end;

begin
  Assert(Op.Kind=zcFuncCall);
  if (Op.Id='sin') then DoGenFunc(fcSin)
  else if (Op.Id='sqrt') then DoGenFunc(fcSqrt)
  else if (Op.Id='cos') then DoGenFunc(fcCos)
  else if (Op.Id='tan') then DoGenFunc(fcTan)
  else if (Op.Id='abs') then DoGenFunc(fcAbs)
  else if (Op.Id='rnd') then DoGenFunc(fcRnd,0)
  else if (Op.Id='random') then DoGenFunc(fcRandom,2)
  else if (Op.Id='atan2') then DoGenFunc(fcAtan2,2)
  else if (Op.Id='noise2') then DoGenFunc(fcNoise2,2)
  else if (Op.Id='noise3') then DoGenFunc(fcNoise3,3)
  else if (Op.Id='frac') then DoGenFunc(fcFrac)
  else if (Op.Id='exp') then DoGenFunc(fcExp)
  else if (Op.Id='clamp') then DoGenFunc(fcClamp,3)
  else if (Op.Id='pow') then DoGenFunc(fcPow,2)
  else if (Op.Id='centerMouse') then DoGenFunc(fcCenterMouse,0,False)
  else if (Op.Id='setRandomSeed') then DoGenFunc(fcSetRandomSeed)
  else if (Op.Id='ceil') then DoGenFunc(fcCeil)
  else if (Op.Id='floor') then DoGenFunc(fcFloor)
  else if (Op.Id='acos') then DoGenFunc(fcAcos)
  else if (Op.Id='asin') then DoGenFunc(fcAsin)
  else raise ECodeGenError.Create('Unknown function: ' + Op.Id);
end;


//Replace references to DefineConstant.Value with the actual constant value.
//Then reoptimze nodes to fold constants.
procedure TZCodeGen.RemoveConstants(StmtList : TList);
var
  I : integer;
  Op : TZcOp;

  procedure DoRemoveConstants(Op: TZcOp);
  var
    P : TZPropertyRef;
    Value : TZPropertyValue;
    I : integer;
  begin
    if Op.Kind=Zc.zcIdentifier then
    begin
      if GetPropRef(Op.Id,P) then
      begin
        P.Component.GetProperty(P.Prop,Value);
        if P.Component is TDefineConstant then
        begin
          Assert(P.Prop.PropertyType=zptFloat);
          Op.Kind := zcConst;
          Op.Value := Value.FloatValue;
        end;
      end;
    end else
    begin
      for I := 0 to Op.Children.Count-1 do
        if Assigned(Op.Child(I)) then
          DoRemoveConstants(Op.Child(I));
    end;
  end;

begin
  for I := 0 to StmtList.Count-1 do
  begin
    Op := StmtList[I];
    DoRemoveConstants(Op);
    Op.Optimize;
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

procedure Compile(ThisC : TZComponent; const Ze : TZExpressionPropValue; Root : TZComponent; SymTab : TSymbolTable);
var
  Compiler : TZc;
  CodeGen : TZCodeGen;
  I : integer;
  Err,S : string;
  Target : TZComponentList;
  PError : EParseError;
  Error : TCocoError;
begin
  S := Ze.Source;
  Target := Ze.Code;

  Compiler := TZc.Create(nil);
  try
    Compiler.SourceStream.Write(S[1],Length(S));
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
      CodeGen.Root := Root;
      CodeGen.Target := Target;
      CodeGen.Component := ThisC;
      CodeGen.SymTab := SymTab;
      try
        CodeGen.GenRoot(Compiler.ZStatements);
      except
        //Om något går fel under kodgenereringen så rensa koden så att den inte körs
        Target.Clear;
        raise;
      end;
    finally
      CodeGen.Free;
    end;
  finally
    Compiler.Free;
  end;

end;

end.
