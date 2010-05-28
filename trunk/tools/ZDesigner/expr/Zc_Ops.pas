unit Zc_Ops;

//Abstract Syntax Tree for Zc-script

interface

uses Contnrs, ZClasses, ZExpressions, uSymTab, Generics.Collections;

type
  TZcAssignType = (atAssign,atMulAssign,atDivAssign,atPlusAssign,atMinusAssign);
  TZcOpKind = (zcNop,zcMul,zcDiv,zcPlus,zcMinus,zcConstLiteral,zcIdentifier,zcAssign,zcIf,
          zcCompLT,zcCompGT,zcCompLE,zcCompGE,zcCompNE,zcCompEQ,
          zcBlock,zcNegate,zcOr,zcAnd,zcFuncCall,zcReturn,zcArrayAccess,
          zcFunction,zcConvert,zcForLoop,
          zcPreInc,zcPreDec,zcPostInc,zcPostDec,
          zcWhile,zcNot,zcBinaryOr,zcBinaryAnd,zcBinaryShiftL,zcBinaryShiftR,
          zcBreak,zcContinue,zcConditional,zcSwitch);


  TZcOp = class;
  TZcOpList = TObjectList<TZcOp>;

  TZcOp = class
  public
    Kind : TZcOpKind;
    Id : string;
    Children : TZcOpList;
    Ref : TObject;
    constructor Create(Owner : TObjectList); virtual;
    destructor Destroy; override;
    function ToString : string; reintroduce; virtual;
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
  strict private
    OriginalString : string;
  public
    Typ : TZcDataType;
    Value : single;
    StringValue : string;
    constructor Create(Typ : TZcDataType; Value : single); reintroduce; overload;
    constructor Create(Typ : TZcDataType; const StringValue : string); reintroduce; overload;
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
    //For z-functions
    Lib : TZLibrary;
    LibIndex : integer;
    //For external functions
    IsExternal : boolean;
    ExtLib : TZExternalLibrary;
    function NeedFrame : boolean;
  end;

  TZcOpFunctionBuiltIn = class(TZcOpFunctionBase)
  public
    FuncId : TExpFuncCallKind;
  end;

  TZcOpSwitch = class(TZcOp)
  public
    HasDefault : boolean;
    ValueOp : TZcOp;
    CaseOps : TZcOpList;
    StatementsOps : TZcOpList;
    constructor Create(Owner : TObjectList); override;
    destructor Destroy; override;
    function ToString : string; override;
    function Optimize : TZcOp; override;
  end;

function MakeOp(Kind : TZcOpKind; const Children : array of TZcOp) : TZcOp; overload;
function MakeOp(Kind : TZcOpKind; Id :string) : TZcOp; overload;
function MakeOp(Kind : TZcOpKind) : TZcOp; overload;

function MakeCompatible(Op : TZcOp; WantedType : TZcDataType) : TZcOp;
function MakeBinary(Kind : TZcOpKind; Op1,Op2 : TZcOp) : TZcOp;
function MakeAssign(Kind : TZcAssignType; Op1,Op2 : TZcOp) : TZcOp;
function VerifyFunctionCall(Op : TZcOp; var Error : String) : boolean;
function MakePrePostIncDec(Kind : TZcOpKind; LeftOp : TZcOp) : TZcOp;

function GetZcTypeName(Typ : TZcDataType) : string;
function ZcStrToFloat(const S : string) : single;

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

uses SysUtils,Math,ExprEdit,Classes;

var
  BuiltInFunctions : TObjectList=nil;
  BuiltInCleanUps : TObjectList;

const
  ZcTypeNames : array[TZcDataType] of string =
(('void'),('float'),('int'),('string'));

function GetZcTypeName(Typ : TZcDataType) : string;
begin
  Result := ZcTypeNames[Typ];
end;

constructor TZcOp.Create(Owner : TObjectList);
begin
  if Owner=nil then
    Owner := FunctionCleanUps;
  Owner.Add(Self);
  Children := TZcOpList.Create(False);
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
          zptInteger,zptByte,zptBoolean : Result := zctInt;
          zptString : Result := zctString;
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
    if (Ref<>nil) and (Ref is TDefineArray) then
    begin
      case (Ref as TDefineArray)._Type of
        dvbFloat : Result := zctFloat;
        dvbInt : Result := zctInt;
        dvbString : Result := zctString;
      end;
    end;
  end else if Kind=zcFuncCall then
  begin
    //Result := zctFloat //built in function
    //should never get here, funcalls have ref set to function
    Assert(False,'Compile error: ' + Self.Id);
  end
  else if Kind=zcConstLiteral then
    Result := zctFloat
  else if Kind=zcConditional then
    Result := Child(1).GetDataType
  else if Children.Count>0 then
    Result := Child(0).GetDataType;
end;

function TZcOp.Child(I : integer) : TZcOp;
begin
  Result := Children[I];
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
    zcAssign,zcPreInc,zcPreDec,zcPostInc,zcPostDec : Result := Child(0).ToString + '=' + Child(1).ToString;
    zcIdentifier : Result := Id;
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
    zcBinaryOr : Result := Child(0).ToString + ' | ' + Child(1).ToString;
    zcBinaryAnd : Result := Child(0).ToString + ' & ' + Child(1).ToString;
    zcBinaryShiftL : Result := Child(0).ToString + ' << ' + Child(1).ToString;
    zcBinaryShiftR : Result := Child(0).ToString + ' >> ' + Child(1).ToString;
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
    zcNot : Result := '!(' + Child(0).ToString +')';
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
    zcWhile :
      begin
        Result := 'while(';
        if Assigned(Child(0)) then
          Result := Result + Child(0).ToString;
        Result := Result + ')';
        if Assigned(Child(1)) then
          Result := Result + Child(1).ToString;
      end;
    zcBreak : Result := 'break';
    zcContinue : Result := 'continue';
    zcConditional :
      begin
        Result := '(' + Child(0).ToString + ') ? ' + Child(1).ToString + ' : ' + Child(2).ToString;
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

  procedure DoIntConstant(NewValue : integer);
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
    if C1.Typ=zctString then
    begin
      if Kind=zcPlus then
        Result := TZcOpLiteral.Create(zctString,'"' + C1.StringValue + C2.StringValue + '"');
    end
    else
    begin
      case Kind of
        //todo: more optimizations
        zcMul : DoConstant(C1.Value * C2.Value);
        zcDiv : DoConstant(C1.Value / C2.Value);
        zcPlus : DoConstant(C1.Value + C2.Value);
        zcMinus : DoConstant(C1.Value - C2.Value);
        zcBinaryOr : DoIntConstant(Trunc(C1.Value) or Trunc(C2.Value));
        zcBinaryAnd : DoIntConstant(Trunc(C1.Value) and Trunc(C2.Value));
        zcBinaryShiftL : DoIntConstant(Trunc(C1.Value) shl Trunc(C2.Value));
        zcBinaryShiftR : DoIntConstant(Trunc(C1.Value) shr Trunc(C2.Value));
      end;
    end;
  end;

  if (Children.Count=1) and (Child(0)<>nil) and (Child(0).Kind=zcConstLiteral) then
  begin
    C1 := Child(0) as TZcOpLiteral;
    if C1.Typ<>zctString then
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
  I,Frame : integer;
begin
  Arguments.Add(Arg);
  //old bp, return pc
  Frame := 2;
  for I := 0 to Arguments.Count - 1 do
    (Arguments[I] as TZcOpArgumentVar).Ordinal := -Arguments.Count - Frame + I;
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
    if (WantedType=zctString) or (Op.GetDataType=zctString)  then
      raise ECodeGenError.Create('Cannot convert to/from string: ' + Op.ToString);
    Result := TZcOpConvert.Create(WantedType,Op);
  end;
end;

function MakeBinary(Kind : TZcOpKind; Op1,Op2 : TZcOp) : TZcOp;
var
  T1,T2 : TZcDataType;
begin
  if (Op1=nil) or (Op2=nil) then
    raise ECodeGenError.Create('Missing op in binary expression');
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

function ZDequoteString(const S : string) : string;
begin
  Result := Copy(S,2, Length(S)-2 );
  if Pos('\',Result)>=0 then
  begin
    Result := StringReplace(Result,'\\','\',[rfReplaceAll]);
    Result := StringReplace(Result,'\n',#13#10,[rfReplaceAll]);
    Result := StringReplace(Result,'\"','"',[rfReplaceAll]);
  end;
end;

constructor TZcOpLiteral.Create(Typ: TZcDataType; const StringValue: string);
begin
  inherited Create(nil);
  Kind := zcConstLiteral;
  Self.Typ := Typ;
  Self.OriginalString := StringValue;
  Self.StringValue := ZDequoteString(StringValue);
end;

function TZcOpLiteral.GetDataType: TZcDataType;
begin
  Result := Typ;
end;

function TZcOpLiteral.ToString: string;
begin
  if GetDataType=zctString then
    Result := Self.OriginalString
  else
    Result := FloatToStr( RoundTo( Value ,-FloatTextDecimals) );
end;



procedure InitBuiltIns;

  function CharToType(C : char) : TZcDataType;
  begin
    case C of
      'V' : Result := zctVoid;
      'F' : Result := zctFloat;
      'I' : Result := zctInt;
      'S' : Result := zctString;
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
  MakeOne('centerMouse',fcCenterMouse,'V');
  MakeOne('setRandomSeed',fcSetRandomSeed,'FF');
  MakeOne('ceil',fcCeil,'FF');
  MakeOne('floor',fcFloor,'FF');
  MakeOne('acos',fcAcos,'FF');
  MakeOne('asin',fcAsin,'FF');
  MakeOne('log2',fcLog2,'FF');
  MakeOne('round',fcRound,'IF');
  MakeOne('quit',fcQuit,'V');
  MakeOne('joyGetAxis',fcJoyGetAxis,'FII');
  MakeOne('joyGetButton',fcJoyGetButton,'III');
  MakeOne('joyGetPOV',fcJoyGetPOV,'FI');
  MakeOne('getSystemTime',fcSystemTime,'I');
  MakeOne('length',fcStringLength,'IS');
  MakeOne('indexOf',fcStringIndexOf,'ISSI');
  MakeOne('strToInt',fcStrToInt,'IS');
  MakeOne('intToStr',fcIntToStr,'SI');
  MakeOne('subStr',fcSubStr,'SSII');
  MakeOne('chr',fcChr,'SI');
  MakeOne('ord',fcOrd,'IS');
end;


function GetBuiltInFunctions : TObjectList;
begin
  if BuiltInFunctions=nil then
    InitBuiltIns;
  Result := BuiltInFunctions;
end;


{ TZcOpFunctionUserDefined }

function TZcOpFunctionUserDefined.NeedFrame: boolean;
begin
  Result := (Arguments.Count>0) or (GetStackSize>0);
end;

function ZcStrToFloat(const S : string) : single;
begin
  if (Length(S)>2) and (LowerCase(Copy(S,1,2))='0x') then
    Exit( StrToInt('$' + Copy(S,3,255)) )
  else if CharInSet(S[ Length(S) ], ['F','f']) then
    Exit( StrToFloat(Copy(S,1,Length(S)-1)) )
  else
    Exit( StrToFloat(S) );
end;

{ TZcOpSwitch }

constructor TZcOpSwitch.Create(Owner: TObjectList);
begin
  inherited;
  CaseOps := TZcOpList.Create(False);
  StatementsOps := TZcOpList.Create(False);
  Kind := zcSwitch;
end;

destructor TZcOpSwitch.Destroy;
begin
  CaseOps.Free;
  StatementsOps.Free;
  inherited;
end;

function TZcOpSwitch.Optimize: TZcOp;
var
  Op : TZcOp;
begin
  for Op in CaseOps do
    Op.Optimize;
  for Op in StatementsOps do
    Op.Optimize;
  ValueOp.Optimize;
  Result := Self;
end;

function TZcOpSwitch.ToString: string;
var
  CaseOp,StatementsOp,Op : TZcOp;
  I : integer;
begin
  Result := 'switch(' + ValueOp.ToString + ') {'#13#10;
  for I := 0 to CaseOps.Count - 1 do
  begin
    CaseOp := CaseOps[I];
    for Op in CaseOp.Children do
    begin
      if Op=nil then
        Result := Result + '  default : '#13#10
      else
        Result := Result + '  case ' + Op.ToString + ' :'#13#10;
    end;
    StatementsOp := StatementsOps[I];
    for Op in StatementsOp.Children do
      Result := Result + '    ' + Op.ToString + ';'#13#10;
  end;

end;

initialization

  FunctionCleanUps := TObjectList.Create(True);

finalization

  FreeAndNil(FunctionCleanUps);
  FreeAndNil(BuiltInFunctions);
  FreeAndNil(BuiltInCleanUps);

end.
