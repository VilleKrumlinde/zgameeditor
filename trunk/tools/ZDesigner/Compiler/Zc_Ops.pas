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
          zcWhile,zcNot,zcBinaryOr,zcBinaryAnd,zcBinaryXor,zcBinaryShiftL,zcBinaryShiftR,
          zcBreak,zcContinue,zcConditional,zcSwitch,zcSelect,zcInvokeComponent,
          zcReinterpretCast,zcMod);

  TZcIdentifierInfo = record
    Kind : (edtComponent,edtProperty,edtPropIndex,edtModelDefined);
    DefinedIndex : integer; //modeldefined: the index of the component in model.definitions
    case Integer of
      0 : (Component : TZComponent);
      1 : (Prop : TZProperty);
      2 : (PropIndex : integer);
  end;


  TZcOp = class;
  TZcOpList = TObjectList<TZcOp>;

  TZcOp = class
  public
    Kind : TZcOpKind;
    Id : string;
    Children : TZcOpList;
    Ref : TObject;
    constructor Create(Owner : TObjectList); overload; virtual;
    constructor Create; overload;
    destructor Destroy; override;
    function ToString : string; reintroduce; virtual;
    function Child(I : integer) : TZcOp;
    function Optimize : TZcOp; virtual;
    function GetDataType : TZcDataType; virtual;
    function GetIdentifierInfo : TZcIdentifierInfo;
    function Clone : TZcOp; virtual;
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
    Value : double;
    StringValue : string;
    constructor Create(TypeKind : TZcDataTypeKind; Value : double); reintroduce; overload;
    constructor Create(TypeKind : TZcDataTypeKind; const StringValue : string); reintroduce; overload;
    function ToString : string; override;
    function GetDataType : TZcDataType; override;
  end;

  TZcOpArrayAccess = class(TZcOp)
  public
    ArrayOp : TZcOp;
    IsRawMem : boolean;
    constructor Create(const Id: string; A : TZcOp); reintroduce; overload;
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
  private
    StackSlot : integer;
  public
    Locals : TObjectList<TZcOpLocalVar>;
    Arguments : TObjectList<TZcOpArgumentVar>;
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

  TZcOpInvokeComponent = class(TZcOp)
  public
    constructor Create(Owner : TObjectList); override;
    function ToString : string; override;
  end;

  TZcOpReinterpretCast = class(TZcOp)
  public
    Typ : TZcDataType;
    constructor Create(Owner : TObjectList); override;
    function ToString : string; override;
    function GetDataType : TZcDataType; override;
  end;

function MakeOp(Kind : TZcOpKind; const Children : array of TZcOp) : TZcOp; overload;
function MakeOp(Kind : TZcOpKind; Id :string) : TZcOp; overload;
function MakeOp(Kind : TZcOpKind) : TZcOp; overload;
function MakeIdentifier(const Id : string) : TZcOp;

function MakeCompatible(Op : TZcOp; const WantedType : TZcDataType) : TZcOp; overload;
function MakeCompatible(Op : TZcOp; const WantedKind : TZcDataTypeKind) : TZcOp; overload;

function MakeBinary(Kind : TZcOpKind; Op1,Op2 : TZcOp) : TZcOp;
function MakeAssign(Kind : TZcAssignType; LeftOp,RightOp : TZcOp) : TZcOp;
function VerifyFunctionCall(Op : TZcOp; var Error : String) : boolean;
function MakePrePostIncDec(Kind : TZcOpKind; LeftOp : TZcOp) : TZcOp;
function CheckPrimary(Op : TZcOp) : TZcOp;

function MakeArrayAccess(ArrayOp : TZcOp) : TZcOp;

function GetZcTypeName(const Typ : TZcDataType) : string;
function ZcStrToFloat(const S : string) : double;
function PropTypeToZType(const PTyp : TZPropertyType) : TZcDataType;

function GetBuiltInFunctions : TObjectList;
function GetBuiltInConstants : TObjectList;

procedure CleanUp;

var
  //Nodes owned by the current compiled function/expression
  //Used for memory management
  FunctionCleanUps : TObjectList;

  //State shared between this unit and Zc-compiler
  CompilerContext : record
    SymTab : TSymbolTable;
    ThisC : TZComponent;
  end;

  Prototypes : record
    Mat4Array,Vec2Array,Vec3Array,Vec4Array : TDefineArray;
  end;

implementation

uses SysUtils,Math,Compiler,Classes,StrUtils;

var
  BuiltInFunctions : TObjectList=nil;
  BuiltInConstants : TObjectList=nil;
  BuiltInCleanUps : TObjectList;

function GetZcTypeName(const Typ : TZcDataType) : string;
begin
  if Typ.Kind=zctReference then
    Result := ComponentManager.GetInfoFromId(Typ.ReferenceClassId).ZClassName
  else
    Result := ZcTypeNames[Typ.Kind];
end;

function PropTypeToZType(const PTyp : TZPropertyType) : TZcDataType;
begin
  FillChar(Result,SizeOf(Result),0);
  Result.Kind := zctVoid;
  if PTyp in ZClasses.FloatTypes then
    Result.Kind := zctFloat
  else
    case PTyp of
      zptInteger,zptByte,zptBoolean : Result.Kind := zctInt;
      zptString : Result.Kind := zctString;
      zptComponentRef : Result.Kind := zctNull;
    end;
end;

function TZcOp.Clone: TZcOp;
var
  Op : TZcOp;
begin
  Assert(Self.ClassType = TZcOp);
  Result := TZcop.Create(nil);
  Result.Kind := Self.Kind;
  Result.Id := Self.Id;
  Result.Ref := Self.Ref;
  for Op in Children do
    Result.Children.Add(Op.Clone);
end;

constructor TZcOp.Create(Owner : TObjectList);
begin
  if Owner=nil then
    Owner := FunctionCleanUps;
  Owner.Add(Self);
  Create;
end;

constructor TZcOp.Create;
begin
  Children := TZcOpList.Create(False);
end;

destructor TZcOp.Destroy;
begin
  FreeAndNil(Children);
end;

function TZcOp.GetDataType: TZcDataType;
var
  Etyp : TZcIdentifierInfo;

  procedure DoIdentifier;
  var
    C : TZComponent;
  begin
    if Ref is TZComponent then
    begin
      Result.Kind := zctReference;
      C := Ref as TZComponent;
      if ComponentManager.GetInfo(C).ClassId=ModelClassId then
        Result.Kind := zctModel
      else
        Result.ReferenceClassId := ComponentManager.GetInfo(C).ClassId;
      if Result.ReferenceClassId=DefineArrayClassId then
        Result.TheArray := C;
    end else if SameText(Id,'currentmodel') then
      Result.Kind := zctModel;
  end;

  procedure DoProp;
  begin
    if Etyp.Prop.PropertyType=zptComponentRef then
    begin
      if ComponentManager.GetInfoFromClass(Etyp.Prop.ChildClasses[0]).ClassId=ModelClassId then
        Result.Kind := zctModel
      else
      begin
        Result.Kind := zctReference;
        Result.ReferenceClassId := ComponentManager.GetInfoFromClass(Etyp.Prop.ChildClasses[0]).ClassId;
      end;
    end else if (Self.Children.Count>0) and (Self.Child(0).Ref is TDefineVariableBase) and (Self.Id='ManagedValue') then
    begin //Allow DefineVariable.ManagedType=mat4/vec3/string even though proptype is string
      Result.Kind := TDefineVariableBase(Self.Child(0).Ref)._Type;
    end else
      Result := PropTypeToZType(Etyp.Prop.PropertyType);
  end;

begin
  FillChar(Result,SizeOf(Result),0);
  Result.Kind := zctVoid;
  if (Ref is TZcOp) and (Kind<>zcSelect) then
  begin
    //Local vars, func calls
    Result := (Ref as TZcOp).GetDataType;
  end else if Kind=zcIdentifier then
    DoIdentifier
  else if Kind=zcConstLiteral then
    Result.Kind := zctFloat
  else if Kind=zcConditional then
    Result := Children[1].GetDataType
  else if Kind=zcSelect then
  begin
    Etyp := Self.GetIdentifierInfo;
    case Etyp.Kind of
      edtComponent:
        begin
          Result.Kind := zctReference;
          Result.ReferenceClassId := ComponentManager.GetInfo(Etyp.Component).ClassId;
        end;
      edtProperty: DoProp;
      edtPropIndex: Result.Kind := zctFloat;
      edtModelDefined : Result.Kind := zctReference;
    else
      raise ECodeGenError.Create('Could not determine type: ' + Self.ToString);
    end;
  end
  else if Kind in [zcCompLT,zcCompGT,zcCompLE,zcCompGE,zcCompNE,zcCompEQ] then
    //Comparisons are always of int-type
    Result.Kind := zctInt
  else if Children.Count>0 then
  begin
    Result := Children.First.GetDataType;
  end;
end;

function GetModelDefined(Op : TZcOp) : TZComponent;
{
  If op is referring to a component that is defined in Model.Definitions
  then return that component, else return nil.
}
var
  O : TObject;
  OwnerC,C : TZComponent;
  PropValue : TZPropertyValue;
begin
  Result := nil;
  O := CompilerContext.SymTab.Lookup(Op.Id);
  if not (O is TZComponent) then
    Exit;
  C := O as TZComponent;
  if C.OwnerList=nil then
    Exit;
  OwnerC := C.OwnerList.Owner;
  if ComponentManager.GetInfo(OwnerC).ClassId<>ModelClassId then
    Exit;
  //Var must be in Definitions-list
  OwnerC.GetProperty( OwnerC.GetProperties.GetByName('Definitions'), PropValue );
  if PropValue.ComponentListValue<>C.OwnerList then
    Exit;
  Result := C;
end;

function TZcOp.GetIdentifierInfo: TZcIdentifierInfo;
var
  Etyp : TZcIdentifierInfo;
  I : integer;

  function DoModelDefined(var Edt : TZcIdentifierInfo) : boolean;
  var
    Parent,Model,C : TZComponent;
    I : integer;
    PropValue : TZPropertyValue;
  begin
    Result := False;
    C := GetModelDefined(Self);
    if C=nil then
      Exit;
    Edt.Kind := edtModelDefined;
    Edt.Component := C;

    I := C.OwnerList.IndexOf(C);
    Model := C.OwnerList.Owner;
    repeat
      //Get the correct item index by adding up definitions in the  model inheritance chain
      Model.GetProperty( Model.GetProperties.GetByName('BaseModel'), PropValue );
      Parent := PropValue.ComponentValue;
      if Parent<>nil then
      begin
        Parent.GetProperty( Parent.GetProperties.GetByName('Definitions'), PropValue );
        Inc(I,PropValue.ComponentListValue.Count);
      end;
      Model := Parent;
    until Model=nil;

    Edt.DefinedIndex := I;
    Result := True;
  end;

  procedure DoProp(Props : TZPropertyList; IsModel : boolean = False);
  begin
    Result.Kind := edtProperty;
    Result.Prop := Props.GetByName(Self.Id);
    if Result.Prop=nil then
    begin
      if IsModel and DoModelDefined(Result) then
        Exit
      else
        raise ECodeGenError.Create('Unknown property: ' + Self.Id);
    end;
  end;

var
  FirstType : TZcDataType;
begin
  //Kind should only be zcIdentifier or zcSelect

  if (Kind=zcIdentifier) and (Children.Count=0) then
  begin
    if (Ref is TZComponent) then
    begin
      Result.Kind := edtComponent;
      Result.Component := Ref as TZComponent;
      Exit;
    end;
  end else if (Children.Count>0) then
  begin
    FirstType := Children.First.GetDataType;
    if FirstType.Kind=zctModel then
    begin
      DoProp(ComponentManager.GetInfoFromId(ModelClassId).GetProperties,True);
      Exit;
    end else if (FirstType.Kind=zctReference) and (Ord(FirstType.ReferenceClassId)>0) then
    begin
      DoProp(ComponentManager.GetInfoFromId(FirstType.ReferenceClassId).GetProperties);
      Exit;
    end else if FirstType.Kind=zctArray then
    begin
      //Local arrays
      DoProp(ComponentManager.GetInfoFromId(DefineArrayClassId).GetProperties);
      Exit;
    end;

    ETyp := Children.First.GetIdentifierInfo;
    if ETyp.Kind in [edtComponent,edtModelDefined] then
    begin
      DoProp(ETyp.Component.GetProperties);
      Exit;
    end;
    if ETyp.Kind=edtProperty then
    begin
      if Etyp.Prop.PropertyType=zptComponentRef then
      begin
        DoProp(ComponentManager.GetInfoFromClass(Etyp.Prop.ChildClasses[0]).GetProperties);
        Exit;
      end else
      begin if ETyp.Prop.PropertyType in [zptRectf,zptColorf,zptVector3f] then
        Result.Kind := edtPropIndex;
        I := -1;
        if Length(Id)=1 then
          case Upcase(Id[1]) of
            'X','R' : I := 0;
            'Y','G' : I := 1;
            'Z','B' : I := 2;
            'W','A' : I := 3;
          end;
        if I<>-1 then
        begin
          Result.PropIndex := I;
          Exit;
        end;
      end;
    end;
  end;
  raise ECodeGenError.Create('Could not determine type: ' + Self.Id);
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
    zcBinaryXor : Result := Child(0).ToString + ' ^ ' + Child(1).ToString;
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
    zcSelect : Result := Child(0).ToString + '.' + Self.Id;
    zcMod : Result := Child(0).ToString + '%' + Child(1).ToString;
  end;
end;

function TZcOp.Optimize : TZcOp;
var
  I : integer;
  C1,C2 : TZcOpLiteral;

  procedure DoConstant(const NewValue : double);
  begin
    Result := TZcOpLiteral.Create(C1.Typ.Kind,NewValue);
  end;

  procedure DoIntConstant(const NewValue : double);
  begin
    Result := TZcOpLiteral.Create(C1.Typ.Kind,NewValue);
  end;

begin
  for I := 0 to Children.Count-1 do
    if Assigned(Child(I)) then Children[I] := Child(I).Optimize;

  Result := Self;

  if (Self.Kind=zcIf) then
  begin
    if Child(0).Kind=zcConstLiteral then
    begin
      C1 := Child(0) as TZcOpLiteral;
      if (C1.Typ.Kind in [zctFloat,zctInt]) then
      begin
        if IsZero(C1.Value) then
        begin
          //Constant False
          if (Children.Count=3) and Assigned(Child(2)) then
            Exit( Child(2) )  //Replace with Else
          else
            Exit( MakeOp(zcNop) );  //Replace with nop
        end else
        begin
          //Constant True
          Exit( Child(1) )  //Replace with Then
        end;
      end;
    end;
  end;

  if Self.Kind=zcConditional then
  begin
    if Child(0).Kind=zcConstLiteral then
    begin
      C1 := Child(0) as TZcOpLiteral;
      if (C1.Typ.Kind in [zctFloat,zctInt]) then
      begin
        if IsZero(C1.Value) then
          Exit( Child(2) )  //Replace with False
        else
          Exit( Child(1) );  //Replace with True
      end;
    end;
  end;

  if (Children.Count=2) and (Child(0).Kind=zcConstLiteral) and (Child(1).Kind=zcConstLiteral) then
  begin
    C1 := Child(0) as TZcOpLiteral;
    C2 := Child(1) as TZcOpLiteral;
    if C1.Typ.Kind=zctString then
    begin
      if Kind=zcPlus then
        Exit( TZcOpLiteral.Create(zctString,'"' + C1.StringValue + C2.StringValue + '"') );
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
        zcBinaryXor : DoIntConstant(Trunc(C1.Value) xor Trunc(C2.Value));
        zcBinaryShiftL : DoIntConstant(Trunc(C1.Value) shl Trunc(C2.Value));
        zcBinaryShiftR : DoIntConstant(Trunc(C1.Value) shr Trunc(C2.Value));
        zcMod : DoConstant(Trunc(C1.Value) mod Trunc(C2.Value));
      end;
    end;
  end;

  if (Children.Count=1) and (Child(0)<>nil) and (Child(0).Kind=zcConstLiteral) then
  begin
    C1 := Child(0) as TZcOpLiteral;
    if C1.Typ.Kind<>zctString then
      case Kind of
        zcNegate : Exit( TZcOpLiteral.Create(C1.Typ.Kind,C1.Value * -1) );
      end;
  end;

  //Optimize for trees such as:
  //   plus ( plus(i,1) , const(1) )
  //replaced with:
  //   plus(i,2)
  if (Kind=zcPlus) and (Self.GetDataType.Kind=zctString) and
     (Children.Count=2) and (Child(0).Kind=zcPlus) and (Child(1).Kind=zcConstLiteral) and
     (Child(0).Child(1).Kind=zcConstLiteral) then
  begin //S + "1" + "2"  ->  s + "12"
    C1 := TZcOpLiteral(Child(0).Child(1));
    Child(0).Children[1] := TZcOpLiteral.Create(zctString,'"' + C1.StringValue + TZcOpLiteral(Child(1)).StringValue + '"');
    Exit( Child(0) );
  end;

  if (Kind in [zcPlus,zcMul]) and (Self.GetDataType.Kind in [zctByte,zctInt,zctFloat]) and
     (Children.Count=2) and (Child(0).Kind=Kind) and (Child(1).Kind=zcConstLiteral) and
     (Child(0).Child(1).Kind=zcConstLiteral) then
  begin //i + 1 + 2  ->  i + 3
    C1 := TZcOpLiteral(Child(0).Child(1));
    C2 := TZcOpLiteral(Child(1));
    case Kind of
      zcPlus : Child(0).Children[1] := TZcOpLiteral.Create(C1.Typ.Kind,C1.Value + C2.Value);
      zcMul : Child(0).Children[1] := TZcOpLiteral.Create(C1.Typ.Kind,C1.Value * C2.Value);
    end;
    Exit( Child(0) );
  end;

end;

{ TZcOpFunctionBase }

constructor TZcOpFunctionBase.Create(Owner: TObjectList);
begin
  inherited;
  Kind := zcFunction;
  Statements := TObjectList.Create(False);
  Locals := TObjectList<TZcOpLocalVar>.Create(False);
  Arguments := TObjectList<TZcOpArgumentVar>.Create(True);
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
  Local.Ordinal := Self.StackSlot;
  if ReturnType.Kind<>zctVoid then
    Inc(Local.Ordinal);
  Inc(Self.StackSlot);
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
    Arguments[I].Ordinal := -Arguments.Count - Frame + I;
end;

function TZcOpFunctionBase.GetDataType: TZcDataType;
begin
  Result := ReturnType;
end;

function TZcOpFunctionBase.GetStackSize: integer;
begin
  //One entry per local var + one entry for return value
  Result := Locals.Count;
  if ReturnType.Kind<>zctVoid then
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
  Result := GetZcTypeName(Typ) + ' ' + Id + ';'#13#10;
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
    Result := TZcOpLiteral.Create(Self.ToType.Kind,C1.Value);
  end;
end;

function TZcOpConvert.ToString: string;
begin
  Result := '((' + GetZcTypeName(ToType) + ')' + Child(0).ToString + ')';
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
  if (Kind in [zcIdentifier,zcSelect]) then
    Result.Ref := CompilerContext.SymTab.Lookup(Id);
end;

function MakeIdentifier(const Id : string) : TZcOp;
var
  C : TDefineConstant;
begin
  Result := MakeOp(zcIdentifier,Id);
  if Result.Ref is TDefineConstant then
  begin
    //If identifier is a constant then replace with the constant value
    C := Result.Ref as TDefineConstant;
    case C._Type of
      zctFloat : Result := TZcOpLiteral.Create(zctFloat,C.Value);
      zctInt : Result := TZcOpLiteral.Create(zctInt,C.IntValue);
      zctString : Result := TZcOpLiteral.Create(zctString,'"' + String(C.StringValue) + '"');
    else
      Assert(False);
    end;
  end;
end;

function MakeOp(Kind : TZcOpKind; const Children : array of TZcOp) : TZcOp; overload;
var
  I : integer;
begin
  Result := MakeOp(Kind);
  for I := 0 to High(Children) do
    Result.Children.Add(Children[I]);
end;

function MakeCompatible(Op : TZcOp; const WantedType : TZcDataType) : TZcOp;
const
  NullCompatible : set of TZcDataTypeKind = [zctModel,zctReference,zctXptr,zctArray];
var
  ExistingType : TZcDataType;
  IdInfo : TZcIdentifierInfo;

  procedure CheckArray(A1,A2 : TDefineArray);
  begin
    if (A1.Dimensions<>A2.Dimensions) or (A1._Type<>A2._Type) then
      raise ECodeGenError.Create('Arrays not compatible');
  end;

begin
  ExistingType := Op.GetDataType;

  //The order of all the IF-statements below are important

  if (WantedType.Kind=zctArray) and (ExistingType.Kind=zctArray) then
  begin
    CheckArray(TDefineArray(WantedType.TheArray), TDefineArray(ExistingType.TheArray));
    Exit(Op);
  end;

  if (ExistingType.Kind=zctReference) and (WantedType.Kind=zctReference) and
    (ExistingType.ReferenceClassId<>WantedType.ReferenceClassId) then
    raise ECodeGenError.Create('Cannot convert ' + GetZcTypeName(ExistingType) + ' to ' + GetZcTypeName(WantedType));

  if (WantedType.Kind in [zctVec2,zctVec3,zctVec4,zctMat4]) and
    (ExistingType.Kind=WantedType.Kind) and
    (Op.Kind=zcArrayAccess) then
    Exit( TZcOpConvert.Create(WantedType,Op) );

  if (ExistingType.Kind=zctVoid) and (WantedType.Kind=zctXptr) then
  begin
    IdInfo := Op.GetIdentifierInfo;
    if (IdInfo.Kind=edtProperty) and (idInfo.Prop.PropertyType=zptBinary) then
    begin
      //convert binary property to xptr
      Exit( TZcOpConvert.Create(WantedType,Op) );
    end;
  end;

  if (ExistingType.Kind=WantedType.Kind) or (WantedType.Kind=zctVoid) or (ExistingType.Kind=zctVoid) then
    Exit(Op);

  if (WantedType.Kind=zctArray) and (ExistingType.Kind=zctReference) and (ExistingType.ReferenceClassId=DefineArrayClassId) then
  begin
    CheckArray(TDefineArray(WantedType.TheArray), TDefineArray(Op.Ref));
    Exit(Op);
  end;

  if (WantedType.Kind=zctByte) and (ExistingType.Kind=zctInt) then
    Exit(Op);
  if (WantedType.Kind=zctInt) and (ExistingType.Kind=zctByte) then
    Exit(Op);

  if ((WantedType.Kind=zctNull) and (ExistingType.Kind in NullCompatible)) or
    ((ExistingType.Kind=zctNull) and (WantedType.Kind in NullCompatible)) then
    Exit(Op);

  if (WantedType.Kind=zctNull) or (ExistingType.Kind=zctNull)  then
    raise ECodeGenError.Create('Cannot convert to/from null: ' + Op.ToString);

  if (WantedType.Kind=zctString) or (ExistingType.Kind=zctString)  then
    raise ECodeGenError.Create('Cannot convert to/from string: ' + Op.ToString);

  Result := TZcOpConvert.Create(WantedType,Op);
end;

function MakeCompatible(Op : TZcOp; const WantedKind : TZcDataTypeKind) : TZcOp;
var
  WantedType : TZcDataType;
begin
  FillChar(WantedType,SizeOf(WantedType),0);
  WantedType.Kind := WantedKind;
  Result := MakeCompatible(Op,WantedType);
end;

function CheckPrimary(Op : TZcOp) : TZcOp;
var
  PName : string;
  C : TZComponent;
  PropValue : TZPropertyValue;
  Owner : TZComponent;
begin
  Result := Op;

  if (Op.Kind=zcSelect) and (Op.Ref<>nil) and (GetModelDefined(Op)=nil) then
    //Avoid situations like "App.Time" conflicting with global variable "Time"
    Op.Ref := nil;

  if (Op.Kind=zcIdentifier) and (Op.Ref<>nil) and (Op.Ref is TZComponent) and ((Op.Ref as TZComponent).OwnerList<>nil) then
  begin
    //Prefix items in Model.Definitions with "CurrentModel". This is needed to get model inheritance right.
    C := (Op.Ref as TZComponent).OwnerList.Owner;
    if ComponentManager.GetInfo(C).ClassId=ModelClassId then
    begin
      //Skip if code is not in a component inside model (because code outside should be able to set the global values)
      Owner := CompilerContext.ThisC;
      repeat
        Owner := Owner.GetOwner;
      until (Owner=nil) or (ComponentManager.GetInfo(Owner).ClassId=ModelClassId);
      if Owner<>nil then
      begin
        Owner.GetProperty( C.GetProperties.GetByName('Definitions'), PropValue );
        if PropValue.ComponentListValue.IndexOf(Op.Ref)>-1 then
        begin
          Result := MakeOp(zcSelect,[ MakeIdentifier('CurrentModel') ]);
          Result.Id := Op.Id;
          Result.Ref := Op.Ref;
        end;
      end;
    end;
  end;

  if (Op.Kind in [zcIdentifier,zcSelect])
    and ((Op.Ref is TDefineVariable) or (Op.Ref is TDefineConstant))
    and (CompilerContext.ThisC.GetProperties.GetByName(Op.Id)=nil)
  then
  begin
    //Qualifies identifier referencing Variable-component with appropriate value-property
    PName := 'Value';
    case (Op.Ref as TDefineVariableBase)._Type of
      zctInt : PName := 'IntValue';
      zctString,zctMat4,zctVec2,zctVec3,zctVec4 : PName := 'ManagedValue';
      zctModel : PName := 'ModelValue';
      zctByte : PName := 'ByteValue';
    end;
    Result := MakeOp(zcSelect,[Result]);
    Result.Id := PName;
  end else if (Op.Kind=zcIdentifier) and (Op.Ref=nil) then
  begin
    //Qualifies identifier referencing property of current component with "this"-prefix
    if CompilerContext.ThisC.GetProperties.GetByName(Op.Id)<>nil then
    begin
      Result := MakeOp(zcSelect,[ MakeIdentifier('this') ]);
      Result.Id := Op.Id;
    end;
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
  if T2.Kind=zctFloat then
    Op1 := MakeCompatible(Op1,T2)
  else
    Op2 := MakeCompatible(Op2,T1);
  Result := MakeOp(Kind,[Op1,Op2]);
end;

function MakePrePostIncDec(Kind : TZcOpKind; LeftOp : TZcOp) : TZcOp;
var
  Op : TZcOp;
begin
  Op := TZcOpLiteral.Create(LeftOp.GetDataType.Kind,1);
  case Kind of
    zcPreInc,zcPostInc:
      Result := MakeOp(Kind, [LeftOp, MakeOp(zcPlus,[LeftOp,Op]) ]);
    zcPreDec,zcPostDec:
      Result := MakeOp(Kind, [LeftOp, MakeOp(zcMinus,[LeftOp,Op]) ]);
  else
    Result := nil;
  end;
end;

function MakeAssign(Kind : TZcAssignType; LeftOp,RightOp : TZcOp) : TZcOp;
const
  AssignMap : array[TZcAssignType] of TZcOpKind = (zcNop,zcMul,zcDiv,zcPlus,zcMinus);
var
  Etyp : TZcIdentifierInfo;

  function InMultiAssign : TZcOp;
  const
    Names = 'RGBA';
  var
    I,LastIndex : integer;
    BlockOp,Op : TZcOp;

    function InSelect(Op : TZcOp; const S : string; const Index : integer) : TZcOp;
    var
      Etyp : TZcIdentifierInfo;
      //I : integer;
    begin
      if Op.Kind=zcSelect then
      begin
        ETyp := Op.GetIdentifierInfo;
        if (ETyp.Kind=edtProperty) and (Etyp.Prop.PropertyType in [zptColorf,zptVector3f,zptRectf])  then
        begin
          Op := MakeOp(zcSelect,Op);
          Op.Id := S;
        end;
      end else if (Op.GetDataType.Kind in [zctVec2,zctVec3,zctVec4]) then
      begin
        if Index < (2+Ord(Op.GetDataType.Kind)-Ord(zctVec2)) then
        begin
          Op := MakeArrayAccess(Op);
          Op.Children.Add( TZcOpLiteral.Create(zctInt,Index) );
        end;
      end;

      { else
      begin
        if Op.Children.Count>0 then
        begin
          Op := Op.Clone;
          for I := 0 to Op.Children.Count - 1 do
            Op.Children[I] := InSelect(Op.Children[I],S);
        end;
      end; }
      Result := Op;
    end;

  begin
    if Etyp.Prop.PropertyType=zptVector3f then
      LastIndex := 2
    else
      LastIndex := 3;
    BlockOp := MakeOp(zcBlock);
    for I := 0 to LastIndex do
    begin
      Op := MakeOp(zcSelect,Names[I+1]);
      Op.Children.Add(LeftOp);
      BlockOp.Children.Add( MakeAssign(Kind,Op, InSelect(RightOp,Op.Id,I) ) );
    end;
    Result := BlockOp;
  end;

begin
  if (LeftOp.Kind=zcSelect) then
  begin
    //Detect multiassigns such as Model1.Position=Model2.Position;
    //This is converted into a block of individual assignment (position.x=position.x etc)
    ETyp := LeftOp.GetIdentifierInfo;
    if (ETyp.Kind=edtProperty) and (Etyp.Prop.PropertyType in [zptColorf,zptVector3f,zptRectf])  then
      Exit( InMultiAssign );
  end;

  RightOp := MakeCompatible(RightOp,LeftOp.GetDataType);
  case Kind of
    atMulAssign,atDivAssign,atPlusAssign,atMinusAssign :  //Convert x*=2 to x=x*2
      begin
        //Note: op1 becomes inserted at a second position in the tree
        //This works because nodes do not own each other
        RightOp := MakeOp(AssignMap[Kind],[LeftOp,RightOp]);
      end;
  end;
  Result := MakeOp(zcAssign,[LeftOp,RightOp]);
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
    Error := 'Unknown function: ' + Op.Id;
    Exit;
  end;
  Result := True;
end;

{ TZcOpLiteral }

constructor TZcOpLiteral.Create(TypeKind: TZcDataTypeKind; Value: double);
begin
  inherited Create(nil);
  Kind := zcConstLiteral;
  Self.Typ.Kind := TypeKind;
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

constructor TZcOpLiteral.Create(TypeKind: TZcDataTypeKind; const StringValue: string);
begin
  inherited Create(nil);
  Kind := zcConstLiteral;
  Self.Typ.Kind := TypeKind;
  Self.OriginalString := StringValue;
  Self.StringValue := ZDequoteString(StringValue);
end;

function TZcOpLiteral.GetDataType: TZcDataType;
begin
  Result := Typ;
end;

function TZcOpLiteral.ToString: string;
begin
  if GetDataType.Kind=zctString then
    Result := Self.OriginalString
  else
    Result := FloatToStr( RoundTo( Value ,-FloatTextDecimals) );
end;



procedure InitBuiltIns;
var
  Con : TDefineConstant;

  function CharToType(C : char) : TZcDataType;
  begin
    FillChar(Result,SizeOf(Result),0);
    case C of
      'V' : Result.Kind := zctVoid;
      'F' : Result.Kind := zctFloat;
      'I' : Result.Kind := zctInt;
      'S' : Result.Kind := zctString;
      'M' : Result.Kind := zctModel;
      'R' : Result.Kind := zctReference;
      'A' : Result.Kind := zctArray;
      'm' : Result.Kind := zctMat4;
      'v' : Result.Kind := zctVec2;
    else
      raise Exception.Create('Unknown type: ' + C);
    end;
  end;

  procedure MakeOne(Id :string; Kind : TExpFuncCallKind; Sig : string);
  var
    F : TZcOpFunctionBuiltIn;
    I,J : integer;
    Arg : TZcOpArgumentVar;
    A : TDefineArray;
  begin
    F := TZcOpFunctionBuiltIn.Create(BuiltInCleanUps);
    F.Id := Id;
    F.FuncId := Kind;
    F.ReturnType := CharToType(Sig[1]);
    if F.ReturnType.Kind=zctVec2 then
    begin
      Inc(F.ReturnType.Kind,Ord(Sig[2])-Ord('2'));
      I := 3;
    end else
      I := 2;
    while I<=Length(Sig) do
    begin
      Arg := TZcOpArgumentVar.Create;
      Arg.Typ := CharToType(Sig[I]);
      if Arg.Typ.Kind=zctVoid then
        Arg.Typ.IsPointer := True;
      if Arg.Typ.Kind=zctVec2 then
      begin
        Inc(Arg.Typ.Kind,Ord(Sig[I+1])-Ord('2'));
        I := I + 2;
      end else if Arg.Typ.Kind=zctReference then
      begin
        J := PosEx('}',Sig,I+1);
        Arg.Typ.ReferenceClassId := ComponentManager.GetInfoFromName(Copy(Sig,I+2, J-I-2)).ClassId;
        I := J+1;
      end else if Arg.Typ.Kind=zctArray then
      begin
        J := PosEx('}',Sig,I+1);
        A := TDefineArray.Create(nil);
        BuiltInCleanUps.Add(A);
        Arg.Typ.TheArray := A;
        A._Type := CharToType( Sig[I+2] ).Kind;
        I := J+1;
      end else
        Inc(I);
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
  MakeOne('createModel',fcCreateModel,'MM');
  MakeOne('trace',fcTrace,'VS');
  MakeOne('touchGetCount',fcTouchGetCount,'I');
  MakeOne('touchGetX',fcTouchGetX,'FI');
  MakeOne('touchGetY',fcTouchGetY,'FI');
  MakeOne('touchGetID',fcTouchGetID,'II');
  MakeOne('getBinaryProp',fcGetBinaryProp,'VVR{Array}');
  MakeOne('setBinaryProp',fcSetBinaryProp,'VVR{Array}');
  MakeOne('getModels',fcGetModels,'VA{M}I');
  MakeOne('transformPoint',fcMatTransformPoint,'v3mv3');
  MakeOne('getMatrix',fcGetMatrix,'VIm');
  MakeOne('setMatrix',fcSetMatrix,'VIm');
  MakeOne('vector2',fcVec2,'v2FF');
  MakeOne('vector3',fcVec3,'v3FFF');
  MakeOne('vector4',fcVec4,'v4FFFF');

  BuiltInConstants := TObjectList.Create(True);
  Con := TDefineConstant.Create(nil);
  Con.SetString('Name','PI');
  Con.Value := PI;
  BuiltInConstants.Add(Con);
  Con := TDefineConstant.Create(nil);
  Con.SetString('Name','ANDROID');
  Con.Value := 0;
  BuiltInConstants.Add(Con);
end;


function GetBuiltInFunctions : TObjectList;
begin
  if BuiltInFunctions=nil then
    InitBuiltIns;
  Result := BuiltInFunctions;
end;

function GetBuiltInConstants : TObjectList;
begin
  if BuiltInConstants=nil then
    InitBuiltIns;
  Result := BuiltInConstants;
end;

{ TZcOpFunctionUserDefined }

function TZcOpFunctionUserDefined.NeedFrame: boolean;
begin
  Result := (Arguments.Count>0) or (GetStackSize>0);
end;

function ZcStrToFloat(const S : string) : double;
begin
  if (Length(S)>2) and (LowerCase(Copy(S,1,2))='0x') then
    Exit( StrToInt(S) )
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
//also remember to check ExprEdit removeconstants
var
  I : integer;
begin
  for I := 0 to CaseOps.Count-1 do
    CaseOps[I] := TZcOp(CaseOps[I]).Optimize;
  for I := 0 to StatementsOps.Count-1 do
    StatementsOps[I] := TZcOp(StatementsOps[I]).Optimize;
  ValueOp := ValueOp.Optimize;
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

procedure CleanUp;
begin
  FreeAndNil(FunctionCleanUps);
  FreeAndNil(BuiltInFunctions);
  FreeAndNil(BuiltInConstants);
  FreeAndNil(BuiltInCleanUps);
  with Prototypes do
  begin
    Vec2Array.Free;
    Vec3Array.Free;
    Vec4Array.Free;
    Mat4Array.Free;
  end;
end;

{ TZcOpInvokeComponent }

constructor TZcOpInvokeComponent.Create(Owner: TObjectList);
begin
  inherited;
  Self.Kind := zcInvokeComponent;
end;

function TZcOpInvokeComponent.ToString: string;
var
  Op : TZcOp;
begin
  Result := '@' + Id + '(';
  for Op in Children do
  begin
    Result := Result + Op.Id + ' : ' + Op.Children.First.ToString;
    if Op<>Children.Last then
      Result := Result + ', ';
  end;
  Result := Result + ')';
end;

{ TZcOpReinterpretCast }

constructor TZcOpReinterpretCast.Create(Owner: TObjectList);
begin
  inherited;
  Self.Kind := zcReinterpretCast;
end;

function TZcOpReinterpretCast.GetDataType: TZcDataType;
begin
  Result := Typ;
end;

function TZcOpReinterpretCast.ToString: string;
begin
  Result := 'reinterpret_cast<' + GetZcTypeName(Typ) + '>(' + Child(0).ToString + ')';
end;

{ TZcOpArrayAccess }

constructor TZcOpArrayAccess.Create(const Id: string; A : TZcOp);
begin
  inherited Create(nil);
  Self.Kind := zcArrayAccess;
  Self.ArrayOp := A;
  Self.Id := Id;
  Self.Ref := CompilerContext.SymTab.Lookup(Id);
end;

function TZcOpArrayAccess.GetDataType: TZcDataType;
begin
  Result.Kind := zctVoid;
  if Ref<>nil then
  begin
    if (Ref is TZcOpVariableBase) then
    begin
      if (Ref as TZcOpVariableBase).Typ.TheArray=nil then
        raise ECodeGenError.Create(Self.Id + ' is not an array')
      else
      begin
        Result.Kind := TDefineArray((Ref as TZcOpVariableBase).Typ.TheArray)._Type;
        if Result.Kind=zctReference then
          //Allow "Bitmap[3] b;" declarations
          Result.ReferenceClassId := ArrayOp.GetDataType.ReferenceClassId;
      end;
    end
    else if (Ref is TDefineArray) then
      Result.Kind := (Ref as TDefineArray)._Type;
  end;

  if Result.Kind=zctVoid then
  begin
    if ArrayOp.GetDataType.Kind in [zctMat4,zctVec2,zctVec3,zctVec4] then
      Result.Kind := zctFloat;
  end;
end;

function TZcOpArrayAccess.ToString: string;
var
  I : integer;
begin
  Result := ArrayOp.ToString + '[';
  for I := 0 to Children.Count-1 do
  begin
    if I>0 then
      Result := Result + ',';
    Result := Result + Child(I).ToString;
  end;
  Result := Result + ']';
end;

function GetArray(Kind : TZcDataTypeKind) : TDefineArray;
begin
  Result := nil;
  case Kind of
    zctMat4 : Result := Prototypes.Mat4Array;
    zctVec3 : Result := Prototypes.Vec3Array;
    zctVec2 : Result := Prototypes.Vec2Array;
    zctVec4 : Result := Prototypes.Vec4Array;
  else
    Assert(False);
  end;
end;

function MakeArrayAccess(ArrayOp : TZcOp) : TZcOp;
var
  Op : TZcOp;
begin
  Op := TZcOpArrayAccess.Create(ArrayOp.Id, ArrayOp);

  //todo: verify that ArrayOp actually is an array type

  if ArrayOp.Kind=zcArrayAccess then
  begin
   //Array of arrays
   Op.Ref := GetArray(TDefineArray(TZcOpArrayAccess(ArrayOp).Arrayop.GetDataType.TheArray)._Type);
   TZcOpArrayAccess(Op).IsRawMem := True;
  end;

  if ArrayOp.Kind=zcFuncCall then
  begin
   //Function returning array
   Op.Ref := GetArray((ArrayOp.Ref as TZcOpFunctionBase).ReturnType.Kind);
  end;

  if ArrayOp.Kind=zcSelect then
   if (ArrayOp.Children.First.Ref is TDefineVariable) then
     //DefineVariable managedvalue
     Op.Ref := GetArray( (ArrayOp.Children.First.Ref as TDefineVariable)._Type );
   //else it is a model defined array, so ArrayOp.ref is already correct

  Result := Op;
end;

initialization

  FunctionCleanUps := TObjectList.Create(True);
  with Prototypes do
  begin
    Mat4Array := TDefineArray.Create(nil);
    Mat4Array.Dimensions := dadTwo;
    Mat4Array.SizeDim1 := 4;
    Mat4Array.SizeDim2 := 4;
    Mat4Array._Type := zctFloat;

    Vec2Array := TDefineArray.Create(nil);
    Vec2Array.Dimensions := dadOne;
    Vec2Array.SizeDim1 := 2;
    Vec2Array._Type := zctFloat;

    Vec3Array := TDefineArray.Create(nil);
    Vec3Array.Dimensions := dadOne;
    Vec3Array.SizeDim1 := 3;
    Vec3Array._Type := zctFloat;

    Vec4Array := TDefineArray.Create(nil);
    Vec4Array.Dimensions := dadOne;
    Vec4Array.SizeDim1 := 4;
    Vec4Array._Type := zctFloat;
  end;

end.
