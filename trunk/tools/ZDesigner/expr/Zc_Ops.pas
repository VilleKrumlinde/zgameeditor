unit Zc_Ops;

//Abstract Syntax Tree for Zc-script

interface

uses Contnrs, ZClasses, ZExpressions, uSymTab;

type
  TZcAssignType = (atAssign,atMulAssign,atDivAssign,atPlusAssign,atMinusAssign);
  TZcOpKind = (zcNop,zcMul,zcDiv,zcPlus,zcMinus,zcConstLiteral,zcIdentifier,zcAssign,zcIf,
          zcCompLT,zcCompGT,zcCompLE,zcCompGE,zcCompNE,zcCompEQ,
          zcBlock,zcNegate,zcOr,zcAnd,zcFuncCall,zcReturn,zcArrayAccess,
          zcFunction,zcConvert,zcForLoop,
          zcPreInc,zcPreDec,zcPostInc,zcPostDec);

  TZcOp = class
  public
    Kind : TZcOpKind;
    Id : string;
    Children : TObjectList;
    Ref : TObject;
    constructor Create(Owner : TObjectList); virtual;
    destructor Destroy; override;
    function ToString : string; virtual;
    function Child(I : integer) : TZcOp;
    function Optimize : TZcOp; virtual;
    function GetDataType : TZcDataType; virtual;
  end;

  TZcOpVariableBase = class(TZcOp)
  public
    Ordinal : integer;
    Typ : TZcDataType;
    function GetDataType : TZcDataType; override;
  end;

  TZcOpLocalVar = class(TZcOpVariableBase)
  public
    InitExpression : TZcOp;
    function ToString : string; override;
  end;

  TZcOpArgumentVar = class(TZcOpVariableBase)
  end;

  TZcOpLiteral = class(TZcOp)
  public
    Typ : TZcDataType;
    Value : single;
    constructor Create(Typ : TZcDataType; Value : single); reintroduce;
    function ToString : string; override;
    function GetDataType : TZcDataType; override;
  end;


  TZcOpConvert = class(TZcOp)
  public
    ToType : TZcDataType;
    constructor Create(ToType : TZcDataType; Op : TZcOp); reintroduce;
    function ToString : string; override;
    function GetDataType : TZcDataType; override;
    function Optimize : TZcOp; override;
  end;

  TZcOpFunctionBase = class(TZcOp)
  public
    Locals : TObjectList;
    Arguments : TObjectList;
    Statements : TObjectList;
    ReturnType : TZcDataType;
    constructor Create(Owner : TObjectList); override;
    destructor Destroy; override;
    function ToString : string; override;
    function Optimize : TZcOp; override;
    procedure AddLocal(Local : TZcOpLocalVar);
    procedure AddArgument(Arg: TZcOpArgumentVar);
    function GetStackSize : integer;
    function GetDataType : TZcDataType; override;
  end;

  TZcOpFunctionUserDefined = class(TZcOpFunctionBase)
  public
    //Assigned during codegen
    Lib : TZLibrary;
    LibIndex : integer;
  end;

  TZcOpFunctionBuiltIn = class(TZcOpFunctionBase)
  public
    FuncId : TExpFuncCallKind;
  end;

function MakeOp(Kind : TZcOpKind; const Children : array of TZcOp) : TZcOp; overload;
function MakeOp(Kind : TZcOpKind; Id :string) : TZcOp; overload;
function MakeOp(Kind : TZcOpKind) : TZcOp; overload;

function MakeCompatible(Op : TZcOp; WantedType : TZcDataType) : TZcOp;
function MakeBinary(Kind : TZcOpKind; Op1,Op2 : TZcOp) : TZcOp;
function MakeAssign(Kind : TZcAssignType; Op1,Op2 : TZcOp) : TZcOp;
function VerifyFunctionCall(Op : TZcOp; var Error : String) : boolean;
function MakePrePostIncDec(Kind : TZcOpKind; LeftOp : TZcOp) : TZcOp;

function GetBuiltInFunctions : TObjectList;

var
  //Nodes owned by the current compiled function/expression
  //Used for memory management
  FunctionCleanUps : TObjectList;

  //State shared between this unit and Zc-commpiler
  CompilerContext : record
    SymTab : TSymbolTable;
    ThisC : TZComponent;
  end;

implementation

uses SysUtils,Math,ExprEdit;

var
  BuiltInFunctions : TObjectList=nil;
  BuiltInCleanUps : TObjectList;

const
  ZcTypeNames : array[TZcDataType] of string =
(('void'),('float'),('int'));

constructor TZcOp.Create(Owner : TObjectList);
begin
  if Owner=nil then
    Owner := FunctionCleanUps;
  Owner.Add(Self);
  Children := TObjectList.Create(False);
end;

destructor TZcOp.Destroy;
begin
  FreeAndNil(Children);
end;

function TZcOp.GetDataType: TZcDataType;

  procedure DoIdentifier;
  var
    Ref : TZPropertyRef;
  begin
    if ParsePropRef(CompilerContext.SymTab,CompilerContext.ThisC,Self.Id,Ref) then
    begin
      if Ref.Prop.PropertyType in ZClasses.FloatTypes then
        Result := zctFloat
      else
        case Ref.Prop.PropertyType of
          zptInteger,zptByte : Result := zctInt;
        end;
    end;
  end;

begin
  Result := zctVoid;
  if (Ref<>nil) and (Ref is TZcOp) then
  begin
    Result := (Ref as TZcOp).GetDataType;
  end else if Kind=zcIdentifier then
    DoIdentifier
  else if Kind=zcArrayAccess then
  begin
    if (Ref<>nil) and (Ref is TDefineArray) and ((Ref as TDefineArray)._Type=dvbInt) then
      Result := zctInt
    else
      Result := zctFloat
  end else if Kind=zcFuncCall then
    Result := zctFloat //built in function
  else if Kind=zcConstLiteral then
    Result := zctFloat
  else if Children.Count>0 then
    Result := Child(0).GetDataType;
end;

function TZcOp.Child(I : integer) : TZcOp;
begin
  Result := TZcOp(Children[I]);
end;

function TZcOp.ToString : string;
var
  I : integer;
begin
  case Kind of
    zcMul : Result := Child(0).ToString + '*' + Child(1).ToString;
    zcDiv : Result := Child(0).ToString + '/' + Child(1).ToString;
    zcPlus : Result := Child(0).ToString + '+' + Child(1).ToString;
    zcMinus : Result := Child(0).ToString + '-' + Child(1).ToString;
    zcIdentifier : Result := Id;
    zcAssign,zcPreInc,zcPreDec,zcPostInc,zcPostDec : Result := Child(0).ToString + '=' + Child(1).ToString;
    zcIf :
      begin
        Result := 'if(' + Child(0).ToString + ') ' + Child(1).ToString;
        if Assigned(Child(2)) then
          Result := Result + ' else ' + Child(2).ToString;
      end;
    zcCompLT : Result := Child(0).ToString + '<' + Child(1).ToString;
    zcCompGT : Result := Child(0).ToString + '>' + Child(1).ToString;
    zcCompLE : Result := Child(0).ToString + '<=' + Child(1).ToString;
    zcCompGE : Result := Child(0).ToString + '>=' + Child(1).ToString;
    zcCompNE : Result := Child(0).ToString + '!=' + Child(1).ToString;
    zcCompEQ : Result := Child(0).ToString + '==' + Child(1).ToString;
    zcBlock :
      begin
        if Children.Count=1 then
          Result := Child(0).ToString
        else
        begin
          Result := '{'#13#10;
          for I := 0 to Children.Count-1 do
            Result := Result + Child(I).ToString + '; ' + #13#10;
          Result := Result + '}';//#13#10;
        end;
      end;
    zcNegate : Result := '-' + Child(0).ToString;
    zcOr : Result := Child(0).ToString + ' || ' + Child(1).ToString;
    zcAnd : Result := Child(0).ToString + ' && ' + Child(1).ToString;
    zcFuncCall :
      begin
        Result := Id + '(';
        for I := 0 to Children.Count-1 do
        begin
          if I>0 then
            Result := Result + ',';
          Result := Result + Child(I).ToString;
        end;
        Result := Result + ')';
      end;
    zcNop : Result := '<nop>;';       //Empty statement
    zcReturn :
      begin
        Result := 'return';
        if Children.Count>0 then
          Result := Result + Child(0).ToString;
        Result := Result + ';';
      end;
    zcArrayAccess:
      begin
        Result := Id + '[';
        for I := 0 to Children.Count-1 do
        begin
          if I>0 then
            Result := Result + ',';
          Result := Result + Child(I).ToString;
        end;
        Result := Result + ']';
      end;
    zcForLoop :
      begin
        Result := 'for(';
        if Assigned(Child(0)) then
          for I := 0 to Child(0).Children.Count-1 do
          begin
            if I>0 then
              Result := Result + ',';
            Result := Result + Child(0).Child(I).ToString;
          end;
        Result := Result + ';';

        if Assigned(Child(1)) then
          Result := Result + Child(1).ToString;
        Result := Result + ';';

        if Assigned(Child(2)) then
          for I := 0 to Child(2).Children.Count-1 do
          begin
            if I>0 then
              Result := Result + ',';
            Result := Result + Child(2).Child(I).ToString;
          end;
        Result := Result + ')';
        if Assigned(Child(3)) then
          Result := Result + Child(3).ToString;
      end;
  end;
end;

function TZcOp.Optimize : TZcOp;
var
  I : integer;
  C1,C2 : TZcOpLiteral;

  procedure DoConstant(NewValue : single);
  begin
    Result := TZcOpLiteral.Create(C1.Typ,NewValue);
  end;

begin
  for I := 0 to Children.Count-1 do
    if Assigned(Child(I)) then Children[I] := Child(I).Optimize;

  Result := Self;

  if (Children.Count=2) and (Child(0).Kind=zcConstLiteral) and (Child(1).Kind=zcConstLiteral) then
  begin
    C1 := Child(0) as TZcOpLiteral;
    C2 := Child(1) as TZcOpLiteral;
    case Kind of
      //todo: more optimizations
      zcMul : DoConstant(C1.Value * C2.Value);
      zcDiv : DoConstant(C1.Value / C2.Value);
      zcPlus : DoConstant(C1.Value + C2.Value);
      zcMinus : DoConstant(C1.Value - C2.Value);
    end;
  end;

  if (Children.Count=1) and (Child(0).Kind=zcConstLiteral) then
  begin
    C1 := Child(0) as TZcOpLiteral;
    case Kind of
      zcNegate : Result := TZcOpLiteral.Create(C1.Typ,C1.Value * -1);
    end;
  end;
end;

{ TZcOpFunctionBase }

constructor TZcOpFunctionBase.Create(Owner: TObjectList);
begin
  inherited;
  Kind := zcFunction;
  Statements := TObjectList.Create(False);
  Locals := TObjectList.Create(False);
  Arguments := TObjectList.Create(False);
end;

destructor TZcOpFunctionBase.Destroy;
begin
  Statements.Free;
  Locals.Free;
  Arguments.Free;
  inherited;
end;

procedure TZcOpFunctionBase.AddLocal(Local: TZcOpLocalVar);
begin
  Local.Ordinal := Locals.Count;
  if ReturnType<>zctVoid then
    Inc(Local.Ordinal);
  Locals.Add(Local);
end;


procedure TZcOpFunctionBase.AddArgument(Arg: TZcOpArgumentVar);
var
  I : integer;
begin
  Arguments.Add(Arg);
  for I := 0 to Arguments.Count - 1 do
    //-2 = old bp, return pc
    (Arguments[I] as TZcOpArgumentVar).Ordinal := -Arguments.Count - 2 + I;
end;

function TZcOpFunctionBase.GetDataType: TZcDataType;
begin
  Result := ReturnType;
end;

function TZcOpFunctionBase.GetStackSize: integer;
begin
  //One entry per local var + one entry for return value
  Result := Locals.Count;
  if ReturnType<>zctVoid then
    Inc(Result);
end;

function TZcOpFunctionBase.Optimize : TZcOp;
var
  I : integer;
begin
  for I := 0 to Statements.Count-1 do
    Statements[I] := TZcOp(Statements[I]).Optimize;
  Result := Self;
end;

function TZcOpFunctionBase.ToString: string;
var
  I : integer;
  UseCurly : boolean;
begin
  UseCurly := (Self.Id<>'');// or ((Statements.Count>1) or (Locals.Count>0));
  if UseCurly then
    Result := '{'#13#10;
  for I := 0 to Locals.Count-1 do
    Result := Result + TZcOp(Locals[I]).ToString;
  for I := 0 to Statements.Count-1 do
    Result := Result + TZcOp(Statements[I]).ToString + ';' + #13#10;
  if UseCurly then
    Result := Result + '}'#13#10;
end;

{ TZcOpLocalVar }

function TZcOpLocalVar.ToString: string;
begin
  Result := ZcTypeNames[Typ] + ' ' + Id + ';'#13#10;
end;

{ TZcOpVariableBase }

function TZcOpVariableBase.GetDataType: TZcDataType;
begin
  Result := Typ;
end;



{ TZcOpConvert }

constructor TZcOpConvert.Create(ToType: TZcDataType; Op: TZcOp);
begin
  inherited Create(nil);
  Self.Kind := zcConvert;
  Self.ToType := ToType;
  Children.Add(Op);
end;

function TZcOpConvert.GetDataType: TZcDataType;
begin
  Result := ToType;
end;

function TZcOpConvert.Optimize: TZcOp;
var
  C1 : TZcOpLiteral;
begin
  Result := inherited Optimize;
  if (Child(0).Kind=zcConstLiteral) then
  begin
    C1 := Child(0) as TZcOpLiteral;
    Result := TZcOpLiteral.Create(Self.ToType,C1.Value);
  end;
end;

function TZcOpConvert.ToString: string;
begin
  Result := '((' + ZcTypeNames[ToType] + ')' + Child(0).ToString + ')';
end;

//-----------------

function MakeOp(Kind : TZcOpKind) : TZcOp; overload;
begin
  Result := TZcOp.Create(nil);
  Result.Kind := Kind;
end;
function MakeOp(Kind : TZcOpKind; Id :string) : TZcOp; overload;
begin
  Result := MakeOp(Kind);
  Result.Id := Id;
  if (Kind=zcIdentifier) and CompilerContext.SymTab.Contains(Id) then
    Result.Ref := CompilerContext.SymTab.Lookup(Id);
end;

function MakeOp(Kind : TZcOpKind; const Children : array of TZcOp) : TZcOp; overload;
var
  I : integer;
begin
  Result := MakeOp(Kind);
  for I := 0 to High(Children) do
    Result.Children.Add(Children[I]);
end;

function MakeCompatible(Op : TZcOp; WantedType : TZcDataType) : TZcOp;
begin
  if (Op.GetDataType=WantedType) or (WantedType=zctVoid) or (Op.GetDataType=zctVoid) then
    Result := Op
  else
  begin
    Result := TZcOpConvert.Create(WantedType,Op);
  end;
end;

function MakeBinary(Kind : TZcOpKind; Op1,Op2 : TZcOp) : TZcOp;
var
  T1,T2 : TZcDataType;
begin
  T1 := Op1.GetDataType;
  T2 := Op2.GetDataType;
  //Cast to common type that does not lose precision
  if T2=zctFloat then
    Op1 := MakeCompatible(Op1,T2)
  else
    Op2 := MakeCompatible(Op2,T1);
  Result := MakeOp(Kind,[Op1,Op2]);
end;

function MakePrePostIncDec(Kind : TZcOpKind; LeftOp : TZcOp) : TZcOp;
var
  Op : TZcOp;
begin
  Op := TZcOpLiteral.Create(LeftOp.GetDataType,1);
  case Kind of
    zcPreInc,zcPostInc:
      Result := MakeOp(Kind, [LeftOp, MakeOp(zcPlus,[LeftOp,Op]) ]);
    zcPreDec,zcPostDec:
      Result := MakeOp(Kind, [LeftOp, MakeOp(zcMinus,[LeftOp,Op]) ]);
  else
    Result := nil;
  end;
end;

function MakeAssign(Kind : TZcAssignType; Op1,Op2 : TZcOp) : TZcOp;
const
  AssignMap : array[TZcAssignType] of TZcOpKind = (zcNop,zcMul,zcDiv,zcPlus,zcMinus);
begin
  Op2 := MakeCompatible(Op2,Op1.GetDataType);
  case Kind of
    atMulAssign,atDivAssign,atPlusAssign,atMinusAssign :  //Convert x*=2 to x=x*2
      begin
        //Note: op1 becomes inserted at a second position in the tree
        //This works because nodes do not own each other
        Op2 := MakeOp(AssignMap[Kind],[Op1,Op2]);
      end;
  end;
  Result := MakeOp(zcAssign,[Op1,Op2]);
end;

function VerifyFunctionCall(Op : TZcOp; var Error : String) : boolean;
var
  FOp : TZcOpFunctionBase;
  I : integer;
begin
  Result := False;
  if CompilerContext.SymTab.Contains(Op.Id) and (CompilerContext.SymTab.Lookup(Op.Id) is TZcOpFunctionBase) then
  begin  //Function
    FOp := CompilerContext.SymTab.Lookup(Op.Id) as TZcOpFunctionBase;
    if FOp.Arguments.Count<>Op.Children.Count then
    begin
      Error := 'Wrong nr of arguments';
      Exit;
    end;
    for I := 0 to FOp.Arguments.Count - 1 do
      Op.Children[I] := MakeCompatible(Op.Child(I),(FOp.Arguments[I] as TZcOp).GetDataType);
  end else
  begin
    Error := 'Unknown function';
    Exit;
  end;
  Result := True;
end;

{ TZcOpLiteral }

constructor TZcOpLiteral.Create(Typ: TZcDataType; Value: single);
begin
  inherited Create(nil);
  Kind := zcConstLiteral;
  Self.Typ := Typ;
  Self.Value := Value;
end;

function TZcOpLiteral.GetDataType: TZcDataType;
begin
  Result := Typ;
end;

function TZcOpLiteral.ToString: string;
begin
  Result := FloatToStr( RoundTo( Value ,-FloatTextDecimals) );
end;



procedure InitBuiltIns;

  function CharToType(C : char) : TZcDataType;
  begin
    case C of
      'V' : Result := zctVoid;
      'F' : Result := zctFloat;
      'I' : Result := zctInt;
    else
      raise Exception.Create('Unknown type: ' + C);
    end;
  end;

  procedure MakeOne(Id :string; Kind : TExpFuncCallKind; Sig : string);
  var
    F : TZcOpFunctionBuiltIn;
    I : integer;
    Arg : TZcOpArgumentVar;
  begin
    F := TZcOpFunctionBuiltIn.Create(BuiltInCleanUps);
    F.Id := Id;
    F.FuncId := Kind;
    F.ReturnType := CharToType(Sig[1]);
    for I := 2 to Length(Sig) do
    begin
      Arg := TZcOpArgumentVar.Create(BuiltInCleanUps);
      Arg.Typ := CharToType(Sig[I]);
      F.AddArgument(Arg);
    end;

    BuiltInFunctions.Add(F);
  end;

begin
  BuiltInFunctions := TObjectList.Create(False);
  BuiltInCleanUps := TObjectList.Create(True);

  MakeOne('sin',fcSin,'FF');
  MakeOne('sqrt',fcSqrt,'FF');
  MakeOne('cos',fcCos,'FF');
  MakeOne('tan',fcTan,'FF');
  MakeOne('abs',fcAbs,'FF');
  MakeOne('rnd',fcRnd,'F');
  MakeOne('random',fcRandom,'FFF');
  MakeOne('atan2',fcAtan2,'FFF');
  MakeOne('noise2',fcNoise2,'FFF');
  MakeOne('noise3',fcNoise3,'FFFF');
  MakeOne('frac',fcFrac,'FF');
  MakeOne('exp',fcExp,'FF');
  MakeOne('clamp',fcClamp,'FFFF');
  MakeOne('pow',fcPow,'FFF');
  MakeOne('centerMouse',fcCenterMouse,'I');
  MakeOne('setRandomSeed',fcSetRandomSeed,'FF');
  MakeOne('ceil',fcCeil,'FF');
  MakeOne('floor',fcFloor,'FF');
  MakeOne('acos',fcAcos,'FF');
  MakeOne('asin',fcAsin,'FF');
  MakeOne('round',fcRound,'IF');
  MakeOne('quit',fcQuit,'I');
  MakeOne('joyGetAxis',fcJoyGetAxis,'FII');
  MakeOne('joyGetButton',fcJoyGetButton,'III');
  MakeOne('joyGetPOV',fcJoyGetPOV,'FI');
end;


function GetBuiltInFunctions : TObjectList;
begin
  if BuiltInFunctions=nil then
    InitBuiltIns;
  Result := BuiltInFunctions;
end;


initialization

  FunctionCleanUps := TObjectList.Create(True);

finalization

  FreeAndNil(FunctionCleanUps);
  FreeAndNil(BuiltInFunctions);
  FreeAndNil(BuiltInCleanUps);

end.
