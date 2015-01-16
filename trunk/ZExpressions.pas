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
  TExpBase = class;

  //Expression execution context
  PExecutionEnvironment = ^TExecutionEnvironment;
  TExecutionEnvironment = record
  const
    ZcStackSize=16384;
  private
    gCurrentPc : ^TExpBase;
    gCurrentBP : integer;
  type
    TStackElement = NativeUInt;
    PStackElement = ^TStackElement;
  var
    ZcStack : array[0..ZcStackSize div SizeOf(TStackElement)] of TStackElement;
    ZcStackPtr : PStackElement;
  private
    function StackGetDepth : integer; inline;
    procedure StackPopTo(var X);
    procedure StackPopToPointer(var X);
    function StackPopFloat : single;
  public
    function StackGetPtrToItem(const Index : integer) : PStackElement; inline;
    procedure StackPush(const X);
    procedure StackPushPointer(const X);
    procedure Init;
  end;


  //Klass med en expression-prop
  TZExpression = class(TCommand)
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Expression : TZExpressionPropValue;
    procedure Execute; override;
  end;

  //User-defined functions
  TZLibrary = class(TZComponent)
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Source : TZExpressionPropValue;
  end;

  //Import of external library (dll)
  TZExternalLibrary = class(TZComponent)
  strict private
    ModuleHandle : NativeUInt;
  private
    function LoadFunction(P : PAnsiChar) : pointer;
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    ModuleName : TPropString;
    CallingConvention : (ccStdCall,ccCdecl);
    Source : TZExpressionPropValue;
    BeforeInitExp : TZExpressionPropValue;
    {$ifndef minimal}
    DefinitionsFile : TPropString;
    function GetDisplayName: ansistring; override;
    procedure DesignerReset; override;
    {$endif}
  end;


  TDefineVariableBase = class(TZComponent)
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    _Type : TZcDataTypeKind;
    {$ifndef minimal}function GetDisplayName: ansistring; override;{$endif}
  end;


  //Define a global variable that can be used in expressions
  TDefineVariable = class(TDefineVariableBase)
  strict private
    procedure InitManaged;
  protected
    procedure DefineProperties(List: TZPropertyList); override;
    procedure InitAfterPropsAreSet; override;
  public
    Value : single;
    IntValue : integer;
    ManagedValue : pointer;
    ModelValue : TZComponent;
    ByteValue : byte;
    {$ifndef minimal}
    procedure DesignerReset; override;
    {$endif}
  end;

  //Define a global constant that can be used in expressions
  //Value is copied into code
  TDefineConstant = class(TDefineVariableBase)
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Value : single;
    IntValue : integer;
    StringValue : TPropString;
    ByteValue : byte;
    {$ifndef minimal}function GetDisplayName: ansistring; override;{$endif}
  end;

  //Holds a stringconstant used in expressions
  //It is automatically inserted in App.ConstantPool
  TExpStringConstant = class(TZComponent)
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Value : TPropString;
  end;

  TArrayDimensions = (dadOne,dadTwo,dadThree);
  TDefineArray = class(TDefineVariableBase)
  strict private
    Data : PFloatArray;
    Limit : integer;
    AllocItemCount : integer;
    AllocType : TZcDataTypeKind;
    AllocPtr : PPointer;
    IsExternal : boolean;
    procedure CleanUpManagedValues(TheType : TZcDataTypeKind; Count : integer; P : PPointer);
    procedure AllocData;
  private
    function PopAndGetElement(Env : PExecutionEnvironment) : PFloat;
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Dimensions : TArrayDimensions;
    SizeDim1,SizeDim2,SizeDim3 : integer;
    Persistent : boolean;
    Values : TZBinaryPropValue;
    destructor Destroy; override;
    function GetData : PFloat;
    function CalcLimit : integer;
    function GetElementSize : integer;
    function SetExternalData(P: pointer) : PPointer;
  end;

  //Virtual machine instruction baseclass
  TExpBase = class(TZComponent)
  protected
    procedure Execute(Env : PExecutionEnvironment); virtual; abstract;
    {$ifndef minimal}public function ExpAsText : string; virtual;{$endif}
  end;

  TExpConstantFloat = class(TExpBase)
  protected
    procedure Execute(Env : PExecutionEnvironment); override;
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Constant : single;
    {$ifndef minimal}
    function GetDisplayName: ansistring; override;
    {$endif}
  end;

  TExpConstantInt = class(TExpBase)
  protected
    procedure Execute(Env : PExecutionEnvironment); override;
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Constant : integer;
    {$ifndef minimal}
    function GetDisplayName: ansistring; override;
    {$endif}
  end;

  TExpOpBinaryKind = (vbkPlus,vbkMinus,vbkMul,vbkDiv,vbkBinaryOr,vbkBinaryAnd,
    vbkBinaryShiftLeft,vbkBinaryShiftRight,vbkBinaryXor,vbkMod);

  TExpOpBinaryBase = class(TExpBase)
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Kind : TExpOpBinaryKind;
    {$ifndef minimal}
    constructor Create(OwnerList: TZComponentList; Kind : TExpOpBinaryKind); reintroduce;
    function ExpAsText : string; override;
    {$endif}
  end;

  TExpOpBinaryFloat = class(TExpOpBinaryBase)
  protected
    procedure Execute(Env : PExecutionEnvironment); override;
  end;

  TExpOpBinaryInt = class(TExpOpBinaryBase)
  protected
    procedure Execute(Env : PExecutionEnvironment); override;
  end;

  TExpOpJumpKind = (jsJumpAlways,jsJumpLT,jsJumpGT,jsJumpLE,jsJumpGE,jsJumpNE,jsJumpEQ);
  TExpJump = class(TExpBase)
  protected
    procedure Execute(Env : PExecutionEnvironment); override;
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Kind : TExpOpJumpKind;
    Destination : integer;
    _Type : (jutFloat,jutInt,jutString);
    {$ifndef minimal}
    function ExpAsText : string; override;
    {$endif}
  end;

  TExpFuncCallKind = (fcSin,fcSqrt,fcCos,fcAbs,fcRnd,fcFrac,fcExp,
     fcTan,fcCeil,fcFloor,fcAcos,fcAsin,fcLog2,fcRound,
     fcRandom,fcAtan2,fcNoise2,fcNoise3,fcClamp,fcPow,fcCenterMouse,
     fcSetRandomSeed,fcQuit,
     fcJoyGetAxis,fcJoyGetButton,fcJoyGetPOV,fcSystemTime,
     fcStringLength,fcStringIndexOf,fcStrToInt,fcOrd,
     fcIntToStr,fcSubStr,fcChr,fcCreateModel,fcTrace,
     fcTouchGetCount,fcTouchGetX,fcTouchGetY,fcTouchGetID,
     fcGetBinaryProp,fcSetBinaryProp,fcGetModels,fcSleep,fcStartThread,
     //Mat4
     fcMatMultiply,fcMatTransformPoint,fcGetMatrix,fcSetMatrix,
     fcVec2,fcVec3,fcVec4
     {$ifndef minimal}
     ,fcGenLValue
     {$endif}
     );


  TExpFuncCallBase = class(TExpBase)
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Kind : TExpFuncCallKind;
    {$ifndef minimal}
    function ExpAsText : string; override;
    {$endif}
  end;

  //Built-in function call
  TExpFuncCall = class(TExpFuncCallBase)
  protected
    procedure Execute(Env : PExecutionEnvironment); override;
  end;

  //Built-in functions that return pointer
  TExpPointerFuncCall = class(TExpFuncCallBase)
  protected
    procedure Execute(Env : PExecutionEnvironment); override;
  end;

  //Matrix functions
  TExpMat4FuncCall = class(TExpFuncCallBase)
  protected
    procedure Execute(Env : PExecutionEnvironment); override;
  end;

  //Read value from array and push on stack
  TExpArrayRead = class(TExpBase)
  protected
    procedure Execute(Env : PExecutionEnvironment); override;
  end;

  //Push ptr to element in array on stack, used with assign
  TExpArrayGetElement = class(TExpBase)
  protected
    procedure Execute(Env : PExecutionEnvironment); override;
  end;

  //Setup local stack frame
  TExpStackFrame = class(TExpBase)
  protected
    procedure Execute(Env : PExecutionEnvironment); override;
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Size : integer;
  end;

  //Load/store local value
  TExpAccessLocal = class(TExpBase)
  protected
    procedure Execute(Env : PExecutionEnvironment); override;
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Kind : (loLoad,loStore,loGetAddress);
    Index : integer;
    {$ifndef minimal}
    function ExpAsText : string; override;
    {$endif}
  end;

  //Return from function
  TExpReturn = class(TExpBase)
  protected
    procedure Execute(Env : PExecutionEnvironment); override;
    procedure DefineProperties(List: TZPropertyList); override;
  public
    HasFrame : boolean;
    HasReturnValue : boolean;
    Arguments : integer;
  end;

  TExpMiscKind = (emPop,emDup,emLoadCurrentModel,emPtrDeref4,emPtrDeref1,
    emPtrDerefPointer);
  TExpMisc = class(TExpBase)
  protected
    procedure Execute(Env : PExecutionEnvironment); override;
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Kind : TExpMiscKind;
    {$ifndef minimal}
    constructor Create(OwnerList: TZComponentList; Kind : TExpMiscKind); reintroduce;
    public function ExpAsText : string; override;
    {$endif}
  end;

  TExpUserFuncCall = class(TExpBase)
  protected
    procedure Execute(Env : PExecutionEnvironment); override;
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Lib : TZLibrary;
    Index : integer;
  end;

  TExpExternalFuncCall = class(TExpBase)
  strict private
    Proc : pointer;
    {$ifdef cpux64}
    Trampoline : pointer;
    {$endif}
  protected
    procedure Execute(Env : PExecutionEnvironment); override;
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Lib : TZExternalLibrary;
    FuncName : TPropString;
    ArgCount : integer;
    ReturnType : TZcDataType;
    {$ifdef cpux64}
    ArgTypes : TPropString;
    destructor Destroy; override;
    {$endif}
    {$ifndef minimal}
    procedure DesignerReset; override;
    {$endif}
  end;

  TExpConvertKind = (eckFloatToInt,eckIntToFloat,eckArrayToXptr,eckBinaryToXptr,
    eckPropToVec3,eckPropToVec4);
  TExpConvert = class(TExpBase)
  protected
    procedure Execute(Env : PExecutionEnvironment); override;
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Kind : TExpConvertKind;
  end;

  //Assign ptr to 4-byte value, both on stack
  TExpAssign4 = class(TExpBase)
  protected
    procedure Execute(Env : PExecutionEnvironment); override;
  end;

  //Assign ptr to 1-byte value, both on stack
  TExpAssign1 = class(TExpBase)
  protected
    procedure Execute(Env : PExecutionEnvironment); override;
  end;

  //Assign ptr to Pointersize value, both on stack
  TExpAssignPointer = class(TExpBase)
  protected
    procedure Execute(Env : PExecutionEnvironment); override;
  end;

  //Join two strings
  TExpStringConCat = class(TExpBase)
  protected
    procedure Execute(Env : PExecutionEnvironment); override;
  end;

  TExpLoadComponent = class(TExpBase)
  protected
    procedure Execute(Env : PExecutionEnvironment); override;
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Component : TZComponent;
  end;

  TExpLoadPropOffset = class(TExpBase)
  strict private
    IsInit : boolean;
    Offset : integer;
  protected
    procedure Execute(Env : PExecutionEnvironment); override;
    procedure DefineProperties(List: TZPropertyList); override;
  public
    PropId : integer;
  end;

  TExpLoadModelDefined = class(TExpBase)
  protected
    procedure Execute(Env : PExecutionEnvironment); override;
    procedure DefineProperties(List: TZPropertyList); override;
  public
    DefinedIndex : integer;
    {$ifndef minimal}
    DefinedName : TPropString;
    {$endif}
  end;

  TExpAddToPointer = class(TExpBase)
  protected
    procedure Execute(Env : PExecutionEnvironment); override;
  end;

  TExpInvokeComponent = class(TExpBase)
  strict private
    InvokeC : TZComponent;
  protected
    procedure Execute(Env : PExecutionEnvironment); override;
    procedure DefineProperties(List: TZPropertyList); override;
  public
    InvokedItemList : TZComponentList;
    InvokeClassId : integer;
    InvokeArgCount : integer;
    IsValue : boolean;
  end;

  TExpInitLocalArray = class(TExpBase)
  protected
    procedure Execute(Env : PExecutionEnvironment); override;
    procedure DefineProperties(List: TZPropertyList); override;
  public
    StackSlot : integer;
    Dimensions : TArrayDimensions;
    _Type : TZcDataTypeKind;
    Size1,Size2,Size3 : integer;
  end;

  TExpGetRawMemElement = class(TExpBase)
  protected
    procedure Execute(Env : PExecutionEnvironment); override;
    procedure DefineProperties(List: TZPropertyList); override;
  public
    _Type : TZcDataTypeKind;
  end;

  TExpArrayUtil = class(TExpBase)
  protected
    procedure Execute(Env : PExecutionEnvironment); override;
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Kind : (auArrayToRawMem, auRawMemToArray, auArrayToArray);
    _Type : TZcDataTypeKind;
  end;

  TCodeReturnValue = record
  case boolean of
    True : (PointerValue : pointer);
    False : (SingleValue : single);
  end;

//Run a compiled expression
//Uses global vars for state.
function RunCode(Code : TZComponentList; Env : PExecutionEnvironment=nil) : TCodeReturnValue;

function ExpGetValue(Code : TZComponentList) : single;
function ExpGetPointer(Code : TZComponentList) : PFloat;


implementation


uses ZMath, ZPlatform, ZApplication, ZLog, Meshes,
  AudioComponents, AudioPlayer
{$if (not defined(minimal))}, System.SysUtils, System.Math, System.TypInfo{$ifend}
{$if (not defined(minimal)) or (defined(cpux64))}, WinApi.Windows{$ifend};

{$POINTERMATH ON}

function ExpGetValue(Code : TZComponentList) : single;
begin
  Result := RunCode(Code).SingleValue;
end;

function ExpGetPointer(Code : TZComponentList) : PFloat;
begin
  Result := RunCode(Code).PointerValue;
end;


procedure TExecutionEnvironment.Init;
begin
  ZcStackPtr := @ZcStack;
  gCurrentBP := 0;
end;

function TExecutionEnvironment.StackGetDepth : integer;
begin
  //Returns the number of stack elements from start of stack
  Result := (ZcStackPtr - PStackElement(@ZcStack));
end;

//Push 32-bit value
procedure TExecutionEnvironment.StackPush(const X);
begin
  {$ifndef minimal}
  if StackGetDepth>=High(ZcStack) then
    ZHalt('Zc Stack Overflow (infinite recursion?)');
  {$endif}
  PInteger(ZcStackPtr)^ := PInteger(@X)^;
  Inc(ZcStackPtr);
end;

//Push 32 or 64-bit value depending on architechture
procedure TExecutionEnvironment.StackPushPointer(const X);
begin
  {$ifndef minimal}
  if StackGetDepth>=High(ZcStack) then
    ZHalt('Zc Stack Overflow (infinite recursion?)');
  {$endif}
  ZcStackPtr^ := TStackElement( PPointer(@X)^ );
  Inc(ZcStackPtr);
end;

//Pop 32-bit value
procedure TExecutionEnvironment.StackPopTo(var X);
begin
  {$ifndef minimal}
  if StackGetDepth=0 then
    ZHalt('Zc Stack Underflow');
  {$endif}
  Dec(ZcStackPtr);
  PInteger(@X)^ := PInteger(ZcStackPtr)^;
end;

//Pop 32 or 64-bit value depending on architechture
procedure TExecutionEnvironment.StackPopToPointer(var X);
begin
  {$ifndef minimal}
  if StackGetDepth=0 then
    ZHalt('Zc Stack Underflow');
  {$endif}
  Dec(ZcStackPtr);
  PPointer(@X)^ := pointer(ZcStackPtr^);
end;

function TExecutionEnvironment.StackPopFloat : single;
begin
  StackPopTo(Result);
end;

function TExecutionEnvironment.StackGetPtrToItem(const Index : integer) : PStackElement;
begin
  Result := @ZcStack;
  Inc(Result,Index);
end;

function RunCode(Code : TZComponentList; Env : PExecutionEnvironment=nil) : TCodeReturnValue;
const
  NilP : pointer = nil;
var
{$ifndef minimal}
  GuardLimit,GuardAllocLimit : integer;
{$endif}
  LocalEnv : TExecutionEnvironment;
begin
  //Pc can be modified in jump-code
  if Code.Count=0 then
    Exit;

  if Env=nil then
  begin
    Env := @LocalEnv;
    Env.Init;
  end;

  Env.gCurrentPc := Code.GetPtrToItem(0);

  Env.StackPushPointer(NilP); //Push return adress nil

  {$ifndef minimal}
  GuardLimit := 500 * 1000000;
  GuardAllocLimit := ManagedHeap_GetAllocCount + 1000000*10;
  {$endif}
  while True do
  begin
    TExpBase(Env.gCurrentPc^).Execute(Env);
    if Env.gCurrentPc=nil then
       break;
    Inc(Env.gCurrentPc);
    {$ifndef minimal}
    Dec(GuardLimit);
    if GuardLimit=0 then
      ZHalt('Five hundered million instructions executed. Infinite loop?');
    if ManagedHeap_GetAllocCount>GuardAllocLimit then
      ZHalt('Ten million strings allocated. Infinite loop?');
    {$endif}
  end;
  if Env.StackGetDepth=1 then
    Env.StackPopToPointer(Result.PointerValue);
  {$ifndef minimal}
  if (Env=@LocalEnv) and (Env.StackGetDepth>0) then
    ZLog.GetLog('Zc').Warning('Stack not empty on script completion');
  {$endif}
end;

{$ifndef minimal}
procedure CheckNilDeref(P : pointer);
begin
  ZAssert( NativeUInt(P)>1024,'Null pointer referenced in expression');
end;
{$endif}

function CreateManagedValue(const Typ : TZcDataTypeKind) : pointer;
var
  A : TDefineArray;
begin
  Result := nil;
  case Typ of
    zctMat4:
      begin
        A := TDefineArray.Create(nil);
        A.Dimensions := dadTwo;
        A.SizeDim1 := 4;
        A.SizeDim2 := 4;
        A._Type := zctFloat;
        Result := A;
      end;
    zctVec2, zctVec3, zctVec4 :
      begin
        A := TDefineArray.Create(nil);
        A.Dimensions := dadOne;
        A.SizeDim1 := 2 + Ord(Typ)-Ord(zctVec2);
        A._Type := zctFloat;
        Result := A;
      end;
  end;
  ManagedHeap_AddValueObject(Result);
end;

{ TZExpression }

procedure TZExpression.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Expression',{$ENDIF}(@Expression), zptExpression);
end;

procedure TZExpression.Execute;
begin
  ZExpressions.RunCode(Expression.Code);
end;

{ TExpConstantFloat }

procedure TExpConstantFloat.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Constant',{$ENDIF}(@Constant), zptFloat);
end;

procedure TExpConstantFloat.Execute(Env : PExecutionEnvironment);
begin
  Env.StackPush( Constant );
end;

{$ifndef minimal}
function TExpConstantFloat.GetDisplayName: AnsiString;
begin
  Result := inherited GetDisplayName + ' ' + AnsiString(FloatToStr(Constant));
end;
{$endif}

{ TExpConstantInt }

procedure TExpConstantInt.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Constant',{$ENDIF}(@Constant), zptInteger);
end;

procedure TExpConstantInt.Execute(Env : PExecutionEnvironment);
begin
  Env.StackPush( Constant );
end;

{$ifndef minimal}
function TExpConstantInt.GetDisplayName: AnsiString;
begin
  Result := inherited GetDisplayName + ' ' + AnsiString(IntToStr(Constant));
end;
{$endif}

{ TExpOpBinary }

procedure TExpOpBinaryBase.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Kind',{$ENDIF}(@Kind), zptByte);
end;

{$ifndef minimal}
constructor TExpOpBinaryBase.Create(OwnerList: TZComponentList; Kind : TExpOpBinaryKind);
begin
  inherited Create(OwnerList);
  Self.Kind := Kind;
end;
{$endif}

{$ifndef minimal}
function TExpOpBinaryBase.ExpAsText : string;
begin
  Result := Copy(GetEnumName(TypeInfo(TExpOpBinaryKind),Ord(Self.Kind)),4,100) + ' (' + Copy(ComponentManager.GetInfo(Self).ZClassName,4,255) + ')';
end;
{$endif}

{$ifdef minimal} {$WARNINGS OFF} {$endif}
procedure TExpOpBinaryFloat.Execute(Env : PExecutionEnvironment);
var
  A1,A2,V : single;
begin
  Env.StackPopTo(A1);
  Env.StackPopTo(A2);
  case Kind of
    vbkPlus : V := A1 + A2;
    vbkMinus : V := A2 - A1;
    vbkMul : V := A2 * A1;
    vbkDiv : V := A2 / A1;
    {$ifndef minimal}else begin ZHalt('Invalid binary op'); exit; end;{$endif}
  end;
  Env.StackPush(V);
end;
{$ifdef minimal} {$WARNINGS ON} {$endif}

{$ifdef minimal} {$WARNINGS OFF} {$endif}
procedure TExpOpBinaryInt.Execute(Env : PExecutionEnvironment);
var
  A1,A2,V : integer;
begin
  Env.StackPopTo(A1);
  Env.StackPopTo(A2);
  case Kind of
    vbkPlus : V := A1 + A2;
    vbkMinus : V := A2 - A1;
    vbkMul : V := A2 * A1;
    vbkDiv : V := A2 div A1;
    vbkBinaryOr : V := A2 or A1;
    vbkBinaryAnd : V := A2 and A1;
    vbkBinaryXor : V := A2 xor A1;
    vbkBinaryShiftLeft : V := A2 shl A1;
    vbkBinaryShiftRight : V := A2 shr A1;
    vbkMod :
      if A1<>0 then
        V := A2 mod A1
      else
        V := 0; //avoid runtime div by zero error
    {$ifndef minimal}else begin ZHalt('Invalid binary op'); exit; end;{$endif}
  end;
  Env.StackPush(V);
end;
{$ifdef minimal} {$WARNINGS ON} {$endif}

{ TExpJump }

procedure TExpJump.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Kind',{$ENDIF}(@Kind), zptByte);
  List.AddProperty({$IFNDEF MINIMAL}'Destination',{$ENDIF}(@Destination), zptInteger);
  List.AddProperty({$IFNDEF MINIMAL}'Type',{$ENDIF}(@_Type), zptByte);
end;

procedure TExpJump.Execute(Env : PExecutionEnvironment);
var
  L,R : single;
  Li,Ri : integer;
  Lp,Rp : pointer;
  Jump : boolean;
begin
  Jump := True;
  case Kind of
    jsJumpAlways : ;
  else
    begin
      case _Type of
        jutFloat:
          begin
            Env.StackPopTo(R);
            Env.StackPopTo(L);
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
        jutInt:
          begin
            Env.StackPopTo(Ri);
            Env.StackPopTo(Li);
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
        jutString:
          begin
            Env.StackPopToPointer(Rp);
            Env.StackPopToPointer(Lp);
            Jump := ZStrCompare(PAnsiChar(Lp),PAnsiChar(Rp));
            if Kind=jsJumpNE then
               Jump := not Jump;
          end;
        //todo: need to compare pointer-size here for win64
      end;
    end;
  end;
  if Jump then
    Inc(Env.gCurrentPc,Destination);
end;

{$ifndef minimal}
function TExpJump.ExpAsText : string;
begin
  Result := inherited ExpAsText + ' ' + Copy(GetEnumName(TypeInfo(TExpOpJumpKind),Ord(Kind)),7,100);
end;
{$endif}

{ TDefineVariable }

procedure TDefineVariable.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Value',{$ENDIF}(@Value), zptFloat);
    //Variabler är ingen ide att spara, de måste sättas ifrån kod
    List.GetLast.NeverPersist := True;
  List.AddProperty({$IFNDEF MINIMAL}'IntValue',{$ENDIF}(@IntValue), zptInteger);
    List.GetLast.NeverPersist := True;
  List.AddProperty({$IFNDEF MINIMAL}'ManagedValue',{$ENDIF}(@ManagedValue), zptString);
    List.GetLast.NeverPersist := True;
    List.GetLast.IsManagedTarget := True;
    List.GetLast.DontClone := True;
    {$ifndef minimal}List.GetLast.HideInGui := True;{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'ModelValue',{$ENDIF}(@ModelValue), zptComponentRef);
    List.GetLast.NeverPersist := True;
    {$ifndef minimal}List.GetLast.SetChildClasses([TModel]);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'ByteValue',{$ENDIF}(@ByteValue), zptByte);
    List.GetLast.NeverPersist := True;
end;

{$ifndef minimal}
procedure TDefineVariable.DesignerReset;
begin
  inherited;
  InitManaged;
end;
{$endif}

procedure TDefineVariable.InitManaged;
begin
  if Self._Type in [zctMat4,zctVec2,zctVec3,zctVec4] then
    Self.ManagedValue := CreateManagedValue(Self._Type);
end;

procedure TDefineVariable.InitAfterPropsAreSet;
begin
  inherited;
  InitManaged;
end;

{ TExpFuncCallBase }

procedure TExpFuncCallBase.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Kind',{$ENDIF}(@Kind), zptByte);
end;

{$ifndef minimal}
function TExpFuncCallBase.ExpAsText : string;
begin
  Result := 'Call ' + Copy(GetEnumName(TypeInfo(TExpFuncCallKind),Ord(Kind)),3,100);
end;
{$endif}

{ TExpFuncCall }

{$ifdef minimal} {$WARNINGS OFF} {$endif}
procedure TExpFuncCall.Execute(Env : PExecutionEnvironment);
var
  V,A1,A2,A3 : single;
  I1,I2 : integer;
  P1,P2 : pointer;
  Bp : PZBinaryPropValue;
  A : TDefineArray;
  L : TZArrayList;
  HasReturnValue : boolean;
begin
  HasReturnValue := True;
  case Kind of
    fcSin :  V := Sin(Env.StackPopFloat);
    fcSqrt : V := Sqrt(Env.StackPopFloat);
    fcCos : V := Cos(Env.StackPopFloat);
    fcAbs : V := Abs(Env.StackPopFloat);
    fcRnd : V := System.Random;
    fcFrac : V := Frac(Env.StackPopFloat);
    fcExp : V := Exp(Env.StackPopFloat);
    fcTan : V := Tan(Env.StackPopFloat);
    fcCeil : V := Ceil(Env.StackPopFloat);
    fcFloor : V := Floor(Env.StackPopFloat);
    fcAcos : V := ArcCos(Env.StackPopFloat);
    fcAsin : V := ArcSin(Env.StackPopFloat);
    fcLog2 : V := Log2(Env.StackPopFloat);
    fcRound : PInteger(@V)^ := Round(Env.StackPopFloat);

    fcRandom :
      begin
        Env.StackPopTo(A2); //Variance
        Env.StackPopTo(A1); //Base
        V := A1 + ((2*System.Random-1.0) * A2);
      end;
    fcAtan2 :
      begin
        Env.StackPopTo(A2);
        Env.StackPopTo(A1);
        V := ArcTan2(A1,A2);
      end;
    fcNoise2 :
      begin
        Env.StackPopTo(A2);
        Env.StackPopTo(A1);
        V := PerlinNoise2(A1,A2);
      end;
    fcNoise3 :
      begin
        Env.StackPopTo(A3);
        Env.StackPopTo(A2);
        Env.StackPopTo(A1);
        V := PerlinNoise3(A1,A2,A3);
      end;
    fcClamp :
      begin
        Env.StackPopTo(A3);
        Env.StackPopTo(A2);
        Env.StackPopTo(A1);
        V := Clamp(A1,A2,A3);
      end;
    fcPow :
      begin
        Env.StackPopTo(A2);
        Env.StackPopTo(A1);
        V := ZMath.Power(A1,A2);
      end;
    fcCenterMouse :
      begin
        HasReturnValue := False;
        ZApp.CenterMouse;
      end;
    fcSetRandomSeed :
      begin
        V := System.RandSeed; //int to float
        System.RandSeed := Round(Env.StackPopFloat); //float to int
      end;
    fcQuit :
      begin
        {$ifndef minimal}
        raise EZHalted.Create('Quit called');
        {$else}
        HasReturnValue := False;
        ZApp.Terminating := True;
        {$endif}
      end;
    fcJoyGetAxis :
      begin
        Env.StackPopTo(I2);
        Env.StackPopTo(I1);
        V := Platform_GetJoystickAxis(I1,I2);
      end;
    fcJoyGetButton :
      begin
        Env.StackPopTo(I2);
        Env.StackPopTo(I1);
        PInteger(@V)^ := Ord(Platform_GetJoystickButton(I1,I2)) and 1;
      end;
    fcJoyGetPOV :
      begin
        Env.StackPopTo(I1);
        V := Platform_GetJoystickPOV(I1);
      end;
    fcSystemTime :
      begin
        PInteger(@V)^ := Platform_GetSystemTime;
      end;
    fcStringLength :
      begin
        Env.StackPopToPointer(P1);
        PInteger(@V)^ := ZStrLength(PAnsiChar(P1));
      end;
    fcStringIndexOf :
      begin
        //x=indexOf("lo","hello",2)
        Env.StackPopTo(I1);
        Env.StackPopToPointer(P1);
        Env.StackPopToPointer(P2);
        PInteger(@V)^ := ZStrPos(PAnsiChar(P2),PAnsiChar(P1),I1);
      end;
    fcStrToInt :
      begin
        Env.StackPopToPointer(P1);
        PInteger(@V)^ := ZStrToInt(PAnsiChar(P1));
      end;
    fcOrd :
      begin
        //i=ord("A")
        Env.StackPopToPointer(P1);
        PInteger(@V)^ := PByte(P1)^;
      end;
    fcTrace :
      begin
        HasReturnValue := False;
        Env.StackPopToPointer(P1);
        {$ifndef minimal}
        ZLog.GetLog('Zc').Write(String(PAnsiChar(P1)),lleUserTrace);
        {$endif}
        {$ifdef android}
        AndroidLog(PAnsiChar(P1));
        {$endif}
      end;
    fcTouchGetCount :
      begin
        PInteger(@V)^ := Platform_TouchGetCount;
      end;
    fcTouchGetX :
      begin
        Env.StackPopTo(I1);
        V := ZApp.NormalizeToScreen( Platform_TouchGetPos(I1) )[0];
      end;
    fcTouchGetY :
      begin
        Env.StackPopTo(I1);
        V := ZApp.NormalizeToScreen( Platform_TouchGetPos(I1) )[1];
      end;
    fcTouchGetID :
      begin
        Env.StackPopTo(I1);
        PInteger(@V)^ := Platform_TouchGetID(I1);
      end;
    fcGetBinaryProp :
      begin
        Env.StackPopToPointer(A);
        Env.StackPopToPointer(Bp);
        A.SizeDim1 := Bp.Size div A.GetElementSize;
        Move(Bp^.Data^, A.GetData^, Bp^.Size);
        HasReturnValue := False;
      end;
    fcSetBinaryProp :
      begin
        Env.StackPopToPointer(A);
        Env.StackPopToPointer(Bp);
        Bp^.Size := A.CalcLimit * A.GetElementSize;
        ReallocMem(Bp^.Data,Bp^.Size);
        Move(A.GetData^ , Bp^.Data^, Bp^.Size);
        HasReturnValue := False;
      end;
    fcGetModels :
      begin
        Env.StackPopTo(I1);
        Env.StackPopToPointer(A);
        L := ZApp.Models.Get(I1);
        A.SizeDim1 := L.Count;
        P1 := A.GetData;
        for I2 := 0 to L.Count-1 do
        begin
          PPointer(P1)^ := L[I2];
          Inc(PPointer(P1));
        end;
        HasReturnValue := False;
      end;
    fcSleep :
      begin
        Env.StackPopTo(I1);
        Platform_Sleep(I1);
        HasReturnValue := False;
      end;
    fcStartThread :
      begin
        Env.StackPopTo(I1);
        Env.StackPopToPointer(P1);
        TZThreadComponent(P1).Start(I1);
        HasReturnValue := False;
      end;
  {$ifndef minimal}else begin ZHalt('Invalid func op'); exit; end;{$endif}
  end;
  if HasReturnValue then
    Env.StackPush(V);
end;
{$ifdef minimal} {$WARNINGS ON} {$endif}

{ TDefineConstant }

procedure TDefineConstant.DefineProperties(List: TZPropertyList);
begin
  inherited;
  //Defineconstant class or properties are never stored in binary
  List.AddProperty({$IFNDEF MINIMAL}'Value',{$ENDIF}(@Value), zptFloat);
   {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
   {$ifndef minimal}List.GetLast.NeedRefreshNodeName := True; {$endif}
   {$ifndef minimal}List.GetLast.ExcludeFromBinary := True; {$endif}
  List.AddProperty({$IFNDEF MINIMAL}'IntValue',{$ENDIF}(@IntValue), zptInteger);
   {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
   {$ifndef minimal}List.GetLast.NeedRefreshNodeName := True; {$endif}
   {$ifndef minimal}List.GetLast.ExcludeFromBinary := True; {$endif}
  List.AddProperty({$IFNDEF MINIMAL}'StringValue',{$ENDIF}(@StringValue), zptString);
   {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
   {$ifndef minimal}List.GetLast.NeedRefreshNodeName := True; {$endif}
   {$ifndef minimal}List.GetLast.ExcludeFromBinary := True; {$endif}
  List.AddProperty({$IFNDEF MINIMAL}'ByteValue',{$ENDIF}(@ByteValue), zptByte);
   {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
   {$ifndef minimal}List.GetLast.NeedRefreshNodeName := True; {$endif}
   {$ifndef minimal}List.GetLast.ExcludeFromBinary := True; {$endif}
end;

{$ifndef minimal}
function TDefineConstant.GetDisplayName: AnsiString;
begin
  Result := inherited GetDisplayName + ' ';
  case _Type of
    zctFloat: Result := Result + AnsiString(FormatFloat('###0.#',Value));
    zctInt: Result := Result + AnsiString(IntToStr(IntValue));
    zctString: Result := Result + '"' + StringValue + '"';
    zctByte: Result := Result + AnsiString(IntToStr(ByteValue));
  end;
end;
{$endif}

{ TExpArrayRead }

procedure TExpArrayRead.Execute(Env : PExecutionEnvironment);
var
  V : single;
  P : PFloat;
  I : integer;
  A : TDefineArray;
begin
  Env.StackPopToPointer(A);
  P := A.PopAndGetElement(Env);
  {$ifndef minimal}
  if P=nil then
    ZHalt('Array read outside range: ' + String(A.Name));
  {$endif}
  if A._Type=zctByte then
  begin
    I := PByte(P)^;
    Env.StackPush(I);
  end
  else
  begin
    V := P^;
    //todo: 64 bit
    Env.StackPush( V );
  end;
end;

{ TDefineArray }

procedure TDefineArray.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Dimensions',{$ENDIF}(@Dimensions), zptByte);
    {$ifndef minimal}List.GetLast.SetOptions(['One','Two','Three']);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'SizeDim1',{$ENDIF}(@SizeDim1), zptInteger);
  List.AddProperty({$IFNDEF MINIMAL}'SizeDim2',{$ENDIF}(@SizeDim2), zptInteger);
  List.AddProperty({$IFNDEF MINIMAL}'SizeDim3',{$ENDIF}(@SizeDim3), zptInteger);
  List.AddProperty({$IFNDEF MINIMAL}'Persistent',{$ENDIF}(@Persistent), zptBoolean);
  List.AddProperty({$IFNDEF MINIMAL}'Values',{$ENDIF}(@Values), zptBinary);
end;

destructor TDefineArray.Destroy;
begin
  CleanUpManagedValues(_Type,Limit,AllocPtr);
  if (Data<>nil) and (not IsExternal) then
    FreeMem(Data);
  inherited;
end;

function TDefineArray.GetData: PFloat;
begin
  //Check if Array size has changed
  if (Limit<>CalcLimit) then
    AllocData;
  {$ifndef minimal}
  ZAssert(not (Persistent and (_Type in [zctString,zctModel,zctMat4,zctVec3,zctVec2,zctVec4])),'Persistent arrays of this datatype not supported');
  {$endif}
  if Persistent then
  begin
    if Values.Data=nil then
      AllocData;
    Result := PFloat(Values.Data)
  end
  else
  begin
    if Data=nil then
      AllocData;
    Result := PFloat(Data);
  end;
end;

function TDefineArray.GetElementSize: integer;
begin
  Result := GetZcTypeSize(Self._Type);
end;

function TDefineArray.CalcLimit: integer;
begin
  Result := SizeDim1;
  if SizeDim2>0 then
    Result := Result * SizeDim2;
  if SizeDim3>0 then
    Result := Result * SizeDim3;
end;

procedure TDefineArray.AllocData;
var
  ByteSize: Integer;
  P : PPointer;
  I : integer;
  WasNil : boolean;
begin
  CleanUpManagedValues(AllocType,AllocItemCount,AllocPtr);
  Self.Limit := CalcLimit;

  if IsExternal then
    Exit;

  ByteSize := Limit * GetElementSize;
  if Persistent then
  begin
    Self.Values.Size := ByteSize;
    P := @Self.Values.Data
  end
  else
    P := @Self.Data;

  WasNil := P^ = nil;
  ReallocMem(P^, ByteSize);
  if WasNil then
    FillChar(P^^, ByteSize, 0);

  Self.AllocPtr := P^;
  Self.AllocItemCount := Self.Limit;
  Self.AllocType := Self._Type;

  if Self._Type in [zctString] then
  begin
    P := P^;
    for I := 0 to Self.Limit - 1 do
    begin
      ManagedHeap_AddTarget(P);
      Inc(P);
    end;
  end;
end;

procedure TDefineArray.CleanUpManagedValues(TheType : TZcDataTypeKind; Count : integer; P : PPointer);
var
  I : integer;
begin
  if (not (TheType in [zctString])) or (Count=0) then
    Exit;
  for I := 0 to Count - 1 do
  begin
    ManagedHeap_RemoveTarget(P);
    Inc(P);
  end;
end;

function TDefineArray.PopAndGetElement(Env : PExecutionEnvironment) : PFloat;
var
  Index,I1,I2,I3 : integer;
  P : PBytes;
begin
  Env.StackPopTo(I3);
  if Self.Dimensions>=dadTwo then
    Env.StackPopTo(I2)
  else
    I2 := 0;
  if Self.Dimensions=dadThree then
    Env.StackPopTo(I1)
  else
    I1 := 0;

  case Self.Dimensions of
    dadOne: Index := I3;
    dadTwo: Index := (I2*SizeDim2) + I3;
  else
    Index := (I1*SizeDim2*SizeDim3) + (I2*SizeDim3) + I3;
  end;

  P := PBytes(GetData);

  {$ifndef minimal}
  if ((Index<0) or (Index>=Limit)) or
    ((I1<0) or (I2<0) or (I3<0)) or
    ((Dimensions=dadOne) and (I3>=SizeDim1)) or
    ((Dimensions=dadTwo) and ((I2>=SizeDim1) or (I3>=SizeDim2))) or
    ((Dimensions=dadThree) and ((I1>=SizeDim1) or (I2>=SizeDim2) or (I3>=SizeDim3)))
    then
  begin
    ZHalt('Array access outside range: ' + String(Self.Name) + ' ' + IntToStr(I1) + ' ' + IntToStr(I2) + ' ' + IntToStr(I3));
    Result := nil;
    Exit;
  end;
  {$endif}

  Result := @P^[Index * Self.GetElementSize];
end;

function TDefineArray.SetExternalData(P: pointer) : PPointer;
begin
  Self.Data := P;
  Self.IsExternal := True;
  Result := @Self.Data;
end;

{ TExpArrayWrite }

procedure TExpArrayGetElement.Execute(Env : PExecutionEnvironment);
var
  P : Pointer;
  A : TDefineArray;
begin
  Env.StackPopToPointer(A);
  P := A.PopAndGetElement(Env);
  {$ifndef minimal}
  if P=nil then
    ZHalt('Array assign outside range: ' + String(A.Name));
  {$endif}
  Env.StackPushPointer(P);
end;

{ TExpStackFrame }

procedure TExpStackFrame.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Size',{$ENDIF}(@Size), zptInteger);
end;

procedure TExpStackFrame.Execute(Env : PExecutionEnvironment);
//http://en.wikipedia.org/wiki/Function_prologue
begin
  Env.StackPush(Env.gCurrentBP);
  Env.gCurrentBP := Env.StackGetDepth;
  //Null-initialize stack frame
  FillChar(Env.ZcStackPtr^,Self.Size * SizeOf(Env.ZcStackPtr^),0);
  //Add frame to stack
  Inc(Env.ZcStackPtr,Self.Size);
end;

{ TExpAccessLocal }

procedure TExpAccessLocal.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Kind',{$ENDIF}(@Kind), zptByte);
  List.AddProperty({$IFNDEF MINIMAL}'Index',{$ENDIF}(@Index), zptInteger);
end;

procedure TExpAccessLocal.Execute(Env : PExecutionEnvironment);
var
  P : TExecutionEnvironment.PStackElement;
begin
  //Use pointer size to get all bits in 64-bit mode
  P := Env.StackGetPtrToItem( Env.gCurrentBP + Self.Index );
  case Kind of
    loLoad: Env.StackPushPointer(P^);
    loStore: Env.StackPopToPointer(P^);
    loGetAddress: Env.StackPushPointer(P);
  end;
end;

{$ifndef minimal}
function TExpAccessLocal.ExpAsText : string;
begin
  if Kind=loLoad then
    Result := 'Load'
  else if Kind=loStore then
    Result := 'Store'
  else
    Result := 'GetAddress';
  Result :=  Result + ' ' + IntToStr(Self.Index) +  ' (local)';
end;
{$endif}

{ TExpReturn }

procedure TExpReturn.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'HasFrame',{$ENDIF}(@HasFrame), zptBoolean);
  List.AddProperty({$IFNDEF MINIMAL}'HasReturnValue',{$ENDIF}(@HasReturnValue), zptBoolean);
  List.AddProperty({$IFNDEF MINIMAL}'Arguments',{$ENDIF}(@Arguments), zptInteger);
end;

{$warnings off}
procedure TExpReturn.Execute(Env : PExecutionEnvironment);
var
  RetVal : pointer;
begin
  if HasReturnValue then
  begin
    //Local0 holds returnvalue
    //Treat return value as pointer to get all bits in 64-bit mode
    RetVal := PPointer( Env.StackGetPtrToItem( Env.gCurrentBP ) )^;
  end;

  if HasFrame then
  begin
    Dec(Env.ZcStackPtr,Env.StackGetDepth-Env.gCurrentBP);
    Env.StackPopTo(Env.gCurrentBP);
  end;

  //Get return adress
  Env.StackPopToPointer(Env.gCurrentPc);

  //Clean stack of function arguments
  Dec(Env.ZcStackPtr,Arguments);

  if HasReturnValue then
  begin
    Env.StackPushPointer(RetVal);
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
  for I := 4 to PropList.Count-1 do
  begin
    Prop := TZProperty(PropList[I]);
    Self.GetProperty(Prop,Value);
    case Prop.PropertyType of
      zptFloat,zptScalar : S := FloatToStr( RoundTo( Value.FloatValue ,-FloatTextDecimals) );
      zptInteger : S := IntToStr(Value.IntegerValue);
      zptComponentRef :
        begin
          if Value.ComponentValue=nil then
            S := '*null*'
          else
          begin
            S := String(Value.ComponentValue.Name);
            if S='' then
              S := Value.ComponentValue.ClassName;
          end;
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
  List.AddProperty({$IFNDEF MINIMAL}'Kind',{$ENDIF}(@Kind), zptByte);
end;

procedure TExpMisc.Execute(Env : PExecutionEnvironment);
var
  V : integer;
  P : pointer;
begin
  case Kind of
    emPop: Env.StackPopFloat;  //Pop, discard value from top of stack
    emDup :
      begin
        Env.StackPopTo(V);
        Env.StackPush(V);
        Env.StackPush(V);
      end;
    emLoadCurrentModel :
      Env.StackPushPointer( Meshes.CurrentModel );
    emPtrDeref4 :
      begin
        Env.StackPopToPointer(P);
        {$ifndef MINIMAL}
        CheckNilDeref(P);
        {$endif}
        V := PInteger(P)^;
        Env.StackPush(V);
      end;
    emPtrDeref1 :
      begin
        Env.StackPopToPointer(P);
        {$ifndef MINIMAL}
        CheckNilDeref(P);
        {$endif}
        V := PByte(P)^;
        Env.StackPush(V);
      end;
    emPtrDerefPointer :
      begin
        Env.StackPopToPointer(P);
        {$ifndef MINIMAL}
        CheckNilDeref(P);
        {$endif}
        Env.StackPushPointer(P^);
      end;
  end;
end;

{$ifndef minimal}
constructor TExpMisc.Create(OwnerList: TZComponentList; Kind: TExpMiscKind);
begin
  inherited Create(OwnerList);
  Self.Kind := Kind;
end;

function TExpMisc.ExpAsText : string;
begin
  Result := Copy(GetEnumName(TypeInfo(TExpMiscKind),Ord(Kind)),3,100) + ' (misc)';
end;
{$endif}

{ TZLibrary }

procedure TZLibrary.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Source',{$ENDIF}(@Source), zptExpression);
    {$ifndef minimal}List.GetLast.ExpressionKind := ekiLibrary;{$endif}
end;

{ TExpUserFuncCall }

procedure TExpUserFuncCall.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Lib',{$ENDIF}(@Lib), zptComponentRef);
  List.AddProperty({$IFNDEF MINIMAL}'Index',{$ENDIF}(@Index), zptInteger);
end;

procedure TExpUserFuncCall.Execute(Env : PExecutionEnvironment);
begin
  Env.StackPushPointer(Env.gCurrentPC);
  Env.gCurrentPC := Lib.Source.Code.GetPtrToItem(Index);
  Dec(Env.gCurrentPc);
end;

{ TExpConvert }

procedure TExpConvert.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Kind',{$ENDIF}(@Kind), zptByte);
end;

procedure TExpConvert.Execute(Env : PExecutionEnvironment);
var
  V : single;
  I : integer;
  D : TDefineArray;
  P : pointer;
  Bp : PZBinaryPropValue;
begin
  case Kind of
    eckFloatToInt:
      begin
        I := Trunc(Env.StackPopFloat);
        Env.StackPush(I);
      end;
    eckIntToFloat :
      begin
        Env.StackPopTo(I);
        V := I;
        Env.StackPush(V);
      end;
    eckArrayToXptr :
      begin
        Env.StackPopToPointer(D);
        P := D.GetData;
        Env.StackPushPointer(P);
      end;
    eckBinaryToXptr :
      begin
        Env.StackPopToPointer(Bp);
        P := Bp.Data;
        Env.StackPushPointer(P);
      end;
    eckPropToVec3,eckPropToVec4 :
      begin
        Env.StackPopToPointer(P);
        D := TDefineArray(CreateManagedValue(TZcDataTypeKind(Ord(zctVec3)+Ord(Kind)-Ord(eckPropToVec3))));
        D.SetExternalData(P);
        Env.StackPushPointer(D);
      end;
  end;
end;

{ TExpAssign4 }

procedure TExpAssign4.Execute(Env : PExecutionEnvironment);
var
  I : integer;
  P : pointer;
begin
  Env.StackPopTo(I);
  Env.StackPopToPointer(P);
  PInteger(P)^ := I;
end;

{ TExpAssign1 }

procedure TExpAssign1.Execute(Env : PExecutionEnvironment);
var
  V : integer;
  B : byte;
  P : pointer;
begin
  //Cast integer to byte before assigning
  Env.StackPopTo(V);
  Env.StackPopToPointer(P);
  B := V;
  PByte(P)^ := B;
end;

{ TExpAssignPointer }

procedure TExpAssignPointer.Execute(Env : PExecutionEnvironment);
var
  V,P : pointer;
begin
  Env.StackPopToPointer(V);
  Env.StackPopToPointer(P);
  PPointer(P)^ := V;
end;

{ TDefineVariableBase }

procedure TDefineVariableBase.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Type',{$ENDIF}(@_Type), zptByte);
    {$ifndef minimal}List.GetLast.SetOptions(['float','int','string','model','byte','mat4','vec2','vec3','vec4']);{$endif}
    {$ifndef minimal}List.GetLast.NeedRefreshNodeName:=True;{$endif}
end;

{$ifndef minimal}
function TDefineVariableBase.GetDisplayName: AnsiString;
var
  I : integer;
  P : TZProperty;
begin
  I := Ord(Self._Type);
  P := Self.GetProperties.GetByName('Type');
  if I<Length(P.Options) then
    Result := inherited GetDisplayName + ' <' + AnsiString(P.Options[ I ]) + '>'
  else
    Result := inherited GetDisplayName + ' <error>';
end;
{$endif}

{ TExpStringConstant }

procedure TExpStringConstant.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$ifndef minimal}'Value',{$ENDIF}(@Value), zptString);
end;

{ TExpStringConCat }

procedure TExpStringConCat.Execute(Env : PExecutionEnvironment);
var
  P1,P2,Dest : PAnsiChar;
  I : integer;
begin
  Env.StackPopToPointer(P2);
  Env.StackPopToPointer(P1);

  I := ZStrLength(P1) + ZStrLength(P2);

  //Add to gc
  Dest := ManagedHeap_Alloc(I+1);

  ZStrCopy(Dest,P1);
  ZStrCat(Dest,P2);

  Env.StackPushPointer(Dest);
end;

{ TExpPointerFuncCall }

procedure TExpPointerFuncCall.Execute(Env : PExecutionEnvironment);
var
  I1,I2 : integer;
  Buf : array[0..15] of ansichar;
  P1,Dest : PAnsiChar;
  M : TModel;
begin
  case Kind of
    fcIntToStr:
      begin
        Env.StackPopTo(I1);
        ZStrConvertInt(I1,PAnsiChar(@Buf));
        Dest := ManagedHeap_Alloc(ZStrLength(@Buf)+1);
        ZStrCopy(Dest,@Buf);
      end;
    fcSubStr :
      begin
        //s=subStr("hello",0,2)
        Env.StackPopTo(I1);
        Env.StackPopTo(I2);
        Env.StackPopToPointer(P1);
        Dest := ManagedHeap_Alloc(I1+1);
        ZStrSubString(P1,Dest,I2,I1);
      end;
    fcChr :
      begin
        //s=chr(65);
        Env.StackPopTo(I1);
        Dest := ManagedHeap_Alloc(2);
        Dest^ := PAnsiChar(@I1)^;
        PBytes(Dest)^[1] := 0;
      end;
    fcCreateModel :
      begin
        Env.StackPopToPointer(M);
          //AddToScene will call m.OnSpawn which in turn can run expressions
          M := TModel(M.CloneModel);
          M.AddToScene(ZApp);
        Dest := pointer(M);
      end;
  end;
  Env.StackPushPointer(Dest);
end;

{ TExternalLibrary }

procedure TZExternalLibrary.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'ModuleName',{$ENDIF}(@ModuleName), zptString);
    {$ifndef minimal}List.GetLast.NeedRefreshNodeName := True;{$endif}
    List.GetLast.IsManagedTarget := True;
  List.AddProperty({$IFNDEF MINIMAL}'CallingConvention',{$ENDIF}(@CallingConvention), zptByte);
    {$ifndef minimal}List.GetLast.SetOptions(['Stdcall','Cdecl']);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'BeforeInitExp',{$ENDIF}(@BeforeInitExp), zptExpression);
  List.AddProperty({$IFNDEF MINIMAL}'Source',{$ENDIF}(@Source), zptExpression);
    {$ifndef minimal}
    List.GetLast.DefaultValue.ExpressionValue.Source :=
'//Import a DLL-library by setting ModuleName to name of the DLL'#13#10 +
'//and then declaring the function headers here. For example:'#13#10 +
'//'#13#10 +
'//  int SetWindowLongA(int hWnd, int nIndex, int dwNewLong) { } '#13#10 +
'//  int SetWindowTextA(int hWnd,string lpString) { }';
    List.GetLast.ExcludeFromBinary := True;
    List.GetLast.ExpressionKind := ekiLibrary;
    {$endif}

  {$ifndef minimal}
  List.AddProperty({$IFNDEF MINIMAL}'DefinitionsFile',{$ENDIF}(@DefinitionsFile), zptString);
    List.SetDesignerProperty;
  {$endif}
end;

function TZExternalLibrary.LoadFunction(P: PAnsiChar): pointer;
begin
  if ModuleHandle=0 then
  begin
    ZExpressions.RunCode(BeforeInitExp.Code);
    ModuleHandle := Platform_LoadModule(Self.ModuleName);
    if ModuleHandle=0 then
      {$ifndef minimal}
      ZHalt(Self.ModuleName + ' not found');
      {$else}
      ZHalt(Self.ModuleName);
      {$endif}
  end;
  Result := Platform_GetModuleProc(ModuleHandle,P);

  if Result=nil then
    //OpenGL functions needs to be handled differently (at least on Win32)
    Result := Platform_GLLoadProc(P);

  if Result=nil then
    {$ifndef minimal}
    ZHalt(P + ' not found');
    {$else}
    ZHalt(P);
    {$endif}
end;

{$ifndef minimal}
procedure TZExternalLibrary.DesignerReset;
begin
  inherited;
  if Self.ModuleHandle<>0 then
  begin
    WinApi.Windows.FreeLibrary(Self.ModuleHandle);
    Self.ModuleHandle := 0;
  end;
end;

function TZExternalLibrary.GetDisplayName: AnsiString;
begin
  Result := inherited GetDisplayName + ' ' + Self.ModuleName;
end;
{$endif}

{ TExpExternalFuncCall }

procedure TExpExternalFuncCall.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Lib',{$ENDIF}(@Lib), zptComponentRef);
  List.AddProperty({$IFNDEF MINIMAL}'FuncName',{$ENDIF}(@FuncName), zptString);
  List.AddProperty({$IFNDEF MINIMAL}'ArgCount',{$ENDIF}(@ArgCount), zptInteger);
  List.AddProperty({$IFNDEF MINIMAL}'ReturnType',{$ENDIF}(@ReturnType), zptByte);
  {$ifdef CPUX64}
  List.AddProperty({$IFNDEF MINIMAL}'ArgTypes',{$ENDIF}(@ArgTypes), zptString);
  {$endif}
end;

{$ifndef minimal}
procedure TExpExternalFuncCall.DesignerReset;
begin
  inherited;
  Self.Proc := nil;
end;
{$endif}

{$if defined(android)}
procedure TExpExternalFuncCall.Execute(Env : PExecutionEnvironment);
const
  MaxArgs = 16;
type
  TArgArray = array[0..MaxArgs-1] of integer;
  TFunc = procedure();
  PFunc = ^TFunc;
  TDummyFunc = procedure(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15,i16 : integer);
  PDummyFunc = ^TDummyFunc;
var
  Arg1,I,RetVal,Tmp : integer;
  P : pointer;
  TheFunc : PFunc;
  Args : TArgArray;
begin
  if Self.Proc=nil then
  begin
    Self.Proc := Lib.LoadFunction(Self.FuncName);
    //Make sure android generates enough stack space for calling a func with maxargs params
    if ArgCount<0 then
      PDummyFunc(Self.Proc)^(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1);  //Should never execute
  end;
  TheFunc := Self.Proc;

  if ArgCount>MaxArgs then
  begin
    Platform_Error('Too many arguments to external function call');
    Exit;
  end;

  //Transfer arguments from Zc-stack to hardware stack
  for I := 0 to ArgCount-1 do
    Env.StackPopTo(Args[ArgCount-I-1]);

  //http://en.wikipedia.org/wiki/Calling_convention#ARM
  //First params in r-registers
  //Then on stack
  if ArgCount>4 then
  begin
    P := @Args+16;
    Tmp := ArgCount-4;
    asm
      ldr r0, Tmp
      ldr r1, P
      mov r3, #0
   .Lmyloop:
      ldr r2, [r1, r3]
      str r2, [r13, r3]
      add r3,r3,#4
      sub	r0,r0,#1
      cmp r0,#0
      bgt .Lmyloop
    end;
  end;

  asm
    ldr r0, Args
    ldr r1, Args+4
    ldr r2, Args+8
    ldr r3, Args+12

    ldr r4,TheFunc
    blx r4

    str r0,RetVal
  end;

//    ldr r4, Args+16
//	  str	r4,[r13]
//    ldr r4, Args+20
//	  str	r4,[r13, #4]
//    ldr r4, Args+24
//	  str	r4,[r13, #8]
//    ldr r4, Args+28
//	  str	r4,[r13, #12]

  if Self.ReturnType.Kind<>zctVoid then
    Env.StackPush(RetVal);
end;

{$elseif defined(cpux64)}
procedure DummyProc(i1,i2,i3,i4,i5,i6,i7,i8 : NativeInt);
begin
end;

function GenerateTrampoline(const ArgCount : integer; ArgTypes : PAnsiChar; Proc : pointer) : pointer;
const
  Int32Regs : array[0..3] of array[0..3] of byte =
( ($41,$8B,$4A,0),  //mov ecx,[r10+x]
  ($41,$8B,$52,0),
  ($45,$8B,$42,0),
  ($45,$8B,$4A,0)
);
  Int64Regs : array[0..3] of array[0..3] of byte =
( ($49,$8B,$4A,0), //mov rcx,[r10+x]
  ($49,$8B,$52,0),
  ($4d,$8B,$42,0),
  ($4d,$8B,$4A,0)
);
  Float32Regs : array[0..3] of array[0..5] of byte =
( ($66,$41,$0F,$6E,$42,0), //movd xmm0,[r10+x]
  ($66,$41,$0F,$6E,$4A,0),
  ($66,$41,$0F,$6E,$52,0),
  ($66,$41,$0F,$6E,$5A,0)
);
  Int32Stack1 : array[0..3] of byte = ($41,$8B,$42,0);  //mov eax,[r10+$10]
  Int32Stack2 : array[0..3] of byte = ($89,$44,$24,0);  //mov [rsp+$10],eax
  Int64Stack1 : array[0..3] of byte = ($49,$8B,$42,0);  //mov rax,[r10+$10]
  Int64Stack2 : array[0..4] of byte = ($48,$89,$44,$24,0);  //mov [rsp+$10],rax
var
  I,Offs : integer;
  P : PByte;

  procedure W(const code : array of byte);
  var
    I : integer;
  begin
    for I := 0 to High(Code) do
    begin
      P^ := Code[I];
      Inc(P);
    end;
  end;

var
  OldProtect : dword;
  CodeSize : integer;
  StackOffs : integer;
begin
  CodeSize := 64 + 4 * ArgCount;
  GetMem(Result,CodeSize);

  P := Result;

  if ArgCount>0 then
    W([$49,$89,$ca]); //mov r10,rcx

  Offs := 0;
  StackOffs := $28;
  for I := 0 to ArgCount-1 do
  begin
    if I<4 then
    begin
      case TZcDataTypeKind(ArgTypes[I]) of
        zctInt :
          begin
            W(Int32Regs[I]);
            P[-1] := Offs;
          end;
        zctString,zctModel,zctXptr,zctMat4,zctVec2,zctVec3,zctVec4 :
          begin
            W(Int64Regs[I]);
            P[-1] := Offs;
          end;
        zctFloat :
          begin
            W(Float32Regs[I]);
            P[-1] := Offs;
          end;
      else
        Assert(False,'This argument type not yet supported on 64-bit:');
      end;
    end else
    begin
      //push on stack
      case GetZcTypeSize(TZcDataTypeKind(ArgTypes[I])) of
        4 :
          begin
            W(Int32Stack1);
            P[-1] := Offs;
            W(Int32Stack2);
            P[-1] := StackOffs;
          end;
        8 :
          begin
            W(Int64Stack1);
            P[-1] := Offs;
            W(Int64Stack2);
            P[-1] := StackOffs;
          end;
      else
        Assert(False,'This argument type not yet supported on 64-bit');
      end;
      Inc(StackOffs, 8);
    end;
    Inc(Offs, SizeOf(NativeInt) );
  end;

  W([$49,$bb]); //mov r11,x
  PPointer(P)^ := Proc; Inc(P,8);  //x
  W([$49,$ff,$e3]); //jmp r11

  VirtualProtect(Result,CodeSize,PAGE_EXECUTE_READWRITE,@OldProtect);
end;

procedure TExpExternalFuncCall.Execute(Env : PExecutionEnvironment);
type
  TFunc = function(Args : pointer) : NativeInt;
  PFunc = ^TFunc;
var
  I : integer;
  RetVal : NativeInt;
  Args : array[0..31] of NativeInt;
begin
  if Self.Proc=nil then
  begin
    Self.Proc := Lib.LoadFunction(Self.FuncName);
    //Make sure there is enough stack space for calling a func with 8 params
    DummyProc(1,2,3,4,5,6,7,8);
    FreeMem(Self.Trampoline);
    Self.Trampoline := GenerateTrampoline(ArgCount,Self.ArgTypes,Self.Proc);
  end;

  //Transfer arguments from Zc-stack to hardware stack
  for I := ArgCount-1 downto 0 do
    if GetZcTypeSize(TZcDataTypeKind(Self.ArgTypes[I]))=SizeOf(Pointer) then
      Env.StackPopToPointer(Args[I])
    else
      Env.StackPopTo(Args[I]);

  Retval := TFunc(Self.Trampoline)(@Args);

  if Self.ReturnType.Kind<>zctVoid then
  begin
    if GetZcTypeSize(Self.ReturnType.Kind)=SizeOf(Pointer) then
      Env.StackPushPointer(RetVal)
    else
      Env.StackPush(RetVal);
  end;
end;

destructor TExpExternalFuncCall.Destroy;
begin
  FreeMem(Trampoline);
end;

{$elseif defined(cpux86) or defined(CPU32)}  //CPU32 is for Freepascal

procedure TExpExternalFuncCall.Execute(Env : PExecutionEnvironment);
{.$define darwin}
type
  TFunc = procedure();
  PFunc = ^TFunc;
var
  Arg1,I,RetVal : integer;
  TheFunc : PFunc;
  Args : array[0..31] of integer;
  RetValFloat : single;
  {$ifndef minimal}
  BeforeSP,AfterSP : integer;
  {$endif}
  {$ifdef darwin}
  OsxExtra : integer;
  {$endif}
begin
  {$ifndef minimal}
  Assert(ArgCount<High(Args),'Too many arguments to external function');
  {$endif}

  if Self.Proc=nil then
    Self.Proc := Lib.LoadFunction(Self.FuncName);
  TheFunc := Self.Proc;

  //Transfer arguments from Zc-stack to hardware stack
  for I := 0 to ArgCount-1 do
    Env.StackPopTo(Args[I]);

  {$ifndef minimal}
  asm
    mov BeforeSP,esp
  end;
  {$endif}

  {$ifdef darwin}
  //http://blogs.embarcadero.com/eboling/2009/05/20/5607
  //http://blogs.embarcadero.com/abauer/2010/01/14/38904
  I := ArgCount * 4 + 4;
  while (I and 15)<>0 do Inc(I,4);
  OsxExtra := (I-4) - (ArgCount*4);
  if OsxExtra>0 then
    asm
      sub esp,OsxExtra
    end;
  {$endif}

  for I := 0 to ArgCount-1 do
  begin
    Arg1 := Args[I];
    asm
      push Arg1
    end;
  end;

  {$ifdef darwin}
  asm
    mov eax,esp
    and eax,8
    mov I,eax
  end;
  if I<>0 then
    ZHalt('Zzdc Stack error');
  {$endif}

  asm
    //Make the call
    call TheFunc
    //Non-float results are returned in eax
    mov RetVal,eax
  end;

  //Cdecl requires caller to clean up stack
  if Lib.CallingConvention=ccCdecl then
  begin
    I := ArgCount * 4;
    asm
      add esp,I
    end;
  end;

  {$ifdef darwin}
  if OsxExtra>0 then
    asm
      add esp,OsxExtra
    end;
  {$endif}

  if Self.ReturnType.Kind=zctFloat then
  begin
    //Float-values results returned on float-stack
    asm
      fstp RetValFloat
      wait
    end;
    RetVal := PInteger(@RetValFloat)^;
  end;

  {$ifndef minimal}
  //Check hw-stack consistency
  asm
    mov AfterSP,esp
  end;
  if AfterSP<>BeforeSP then
  begin
    asm
      mov esp,BeforeSP
    end;
    ZHalt('Hardware stack error after call to ' + Self.FuncName + '. Check argument count and sizes, and calling convention.');
  end;
  {$endif}

  if Self.ReturnType.Kind<>zctVoid then
    Env.StackPush(RetVal);
end;
{$ifend}

{ TExpLoadComponent }

procedure TExpLoadComponent.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Component',{$ENDIF}(@Component), zptComponentRef);
end;

procedure TExpLoadComponent.Execute(Env : PExecutionEnvironment);
begin
  Env.StackPushPointer(Self.Component);
end;

{ TExpLoadPropOffset }

procedure TExpLoadPropOffset.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'PropId',{$ENDIF}(@PropId), zptInteger);
end;

procedure TExpLoadPropOffset.Execute(Env : PExecutionEnvironment);
var
  C : TZComponent;
begin
  if not IsInit then
  begin
    Env.StackPopToPointer(C);
    {$ifndef minimal}
    CheckNilDeref(C);
    {$endif}
    Self.Offset := C.GetProperties.GetById(Self.PropId).Offset;
    Env.StackPushPointer(C);
    IsInit := True;
  end;
  Env.StackPush(Self.Offset);
end;

{ TExpLoadModelDefined }

procedure TExpLoadModelDefined.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'DefinedIndex',{$ENDIF}(@DefinedIndex), zptInteger);
  {$ifndef minimal}
  List.AddProperty({$IFNDEF MINIMAL}'DefinedName',{$ENDIF}(@DefinedName), zptString);
    List.SetDesignerProperty;
  {$endif}
end;

procedure TExpLoadModelDefined.Execute(Env : PExecutionEnvironment);
var
  M : TModel;
  C : TZComponent;
begin
  Env.StackPopToPointer(M);
  {$ifndef minimal}
  CheckNilDeref(M);
  if (Self.DefinedIndex>=M.Definitions.Count) or
    (not SameText(String(TZComponent(M.Definitions[Self.DefinedIndex]).Name),String(DefinedName))) then
  begin
    ZHalt('Defined var mismatch "' + DefinedName + '" in model "' + String(M.Name) + '" must be at position ' + IntToStr(Self.DefinedIndex) + ' in Definitions-list.');
  end;
  {$endif}
  C := TZComponent(M.Definitions[Self.DefinedIndex]);
  Env.StackPushPointer( C );
end;

{ TExpAddToPointer }

procedure TExpAddToPointer.Execute(Env : PExecutionEnvironment);
//Add 32-bit value to pointer and store the result as pointer
var
  V : integer;
  P : pbyte;
begin
  Env.StackPopTo(V);
  Env.StackPopToPointer(P);
  Inc(P,V);
  Env.StackPushPointer(P);
end;

{ TExpInvokeComponent }

procedure TExpInvokeComponent.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'InvokeClassId',{$ENDIF}(@InvokeClassId), zptInteger);
  List.AddProperty({$IFNDEF MINIMAL}'InvokeArgCount',{$ENDIF}(@InvokeArgCount), zptInteger);
  List.AddProperty({$IFNDEF MINIMAL}'InvokedItemList',{$ENDIF}(@InvokedItemList), zptComponentList);
    List.GetLast.NeverPersist := True;
  List.AddProperty({$IFNDEF MINIMAL}'IsValue',{$ENDIF}(@IsValue), zptBoolean);
end;

procedure TExpInvokeComponent.Execute(Env : PExecutionEnvironment);
var
  Ci : TZComponentInfo;
  I,PropId,RawValue : integer;
  Prop : TZProperty;
  V : TZPropertyValue;
begin

  if InvokeC=nil then
  begin
    Ci := ComponentManager.GetInfoFromId(TZClassIds(Self.InvokeClassId));
    Self.InvokeC := Ci.ZClass.Create(Self.InvokedItemList);
    Self.InvokeC._ZApp := Self.ZApp;
  end;

  for I := 0 to InvokeArgCount-1 do
  begin
    Env.StackPopTo(PropId);
    Env.StackPopTo(RawValue);
    Prop := InvokeC.GetProperties.GetById(PropId);
    //todo: Pointer properties need separate treatment for 64-bit compilation
    case Prop.PropertyType of
      zptFloat: V.FloatValue := PFloat(@RawValue)^;
      zptInteger: V.IntegerValue := RawValue;
      zptByte: V.ByteValue := RawValue;
      zptBoolean: V.BooleanValue := ByteBool(RawValue);
      zptComponentRef : V.ComponentValue := TZComponent(RawValue);
      zptString : V.StringValue := PAnsiChar(RawValue);
    {$ifndef minimal}
    else
      ZHalt(ClassName + ' invalid datatype for argument');
    {$endif}
    end;

    Self.InvokeC.SetProperty(Prop,V);
  end;

  if IsValue then
  begin
    Env.StackPushPointer(Self.InvokeC);
  end else
  begin
    TCommand(InvokeC).Execute;
  end;
end;

{ TExpInitLocalArray }

procedure TExpInitLocalArray.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'StackSlot',{$ENDIF}(@StackSlot), zptInteger);
  List.AddProperty({$IFNDEF MINIMAL}'Dimensions',{$ENDIF}(@Dimensions), zptByte);
  List.AddProperty({$IFNDEF MINIMAL}'Type',{$ENDIF}(@_Type), zptByte);
  List.AddProperty({$IFNDEF MINIMAL}'Size1',{$ENDIF}(@Size1), zptInteger);
  List.AddProperty({$IFNDEF MINIMAL}'Size2',{$ENDIF}(@Size2), zptInteger);
  List.AddProperty({$IFNDEF MINIMAL}'Size3',{$ENDIF}(@Size3), zptInteger);
end;

procedure TExpInitLocalArray.Execute(Env : PExecutionEnvironment);
var
  P : TExecutionEnvironment.PStackElement;
  A : TDefineArray;
begin
  //Use pointer size to get all bits in 64-bit mode
  P := Env.StackGetPtrToItem( Env.gCurrentBP + Self.StackSlot );
  A := TDefineArray.Create(nil);
  ManagedHeap_AddValueObject(A);
  A.Dimensions := Self.Dimensions;
  A._Type := Self._Type;
  A.SizeDim1 := Self.Size1;
  A.SizeDim2 := Self.Size2;
  A.SizeDim3 := Self.Size3;
  PPointer(P)^ := A;
end;

{ TExpMat4 }

procedure TExpMat4FuncCall.Execute(Env : PExecutionEnvironment);
var
  M1,M2 : PZMatrix4f;
  V1 : PZVector3f;
  I : integer;
  A : TDefineArray;
  X,Y,Z,W : single;
begin
  case Kind of
    fcMatMultiply:
      begin
        Env.StackPopToPointer(M1);
        Env.StackPopToPointer(M2);
        A := TDefineArray(CreateManagedValue(zctMat4));
        M1 := PZMatrix4f(TDefineArray(M1).GetData);
        M2 := PZMatrix4f(TDefineArray(M2).GetData);
        PZMatrix4f(A.GetData)^ := MatrixMultiply(M1^,M2^);
        Env.StackPushPointer(A);
      end;
    fcMatTransformPoint :
      begin
        Env.StackPopToPointer(V1);
        Env.StackPopToPointer(M1);
        A := TDefineArray(CreateManagedValue(zctVec3));
        V1 := PZVector3f(TDefineArray(V1).GetData);
        M1 := PZMatrix4f(TDefineArray(M1).GetData);
        VectorTransform(V1^,M1^,PZVector3f(A.GetData)^);
        Env.StackPushPointer(A);
      end;
    fcGetMatrix :
      begin
        Env.StackPopToPointer(A);
        Env.StackPopTo(I);
        Self.ZApp.Driver.GetMatrix(I, PZMatrix4f(A.GetData));
      end;
    fcSetMatrix :
      begin
        Env.StackPopToPointer(A);
        Env.StackPopTo(I);
        Self.ZApp.Driver.SetMatrix(I, PZMatrix4f(A.GetData)^);
      end;
    fcVec2 :
      begin
        Env.StackPopTo(Y);
        Env.StackPopTo(X);
        A := TDefineArray(CreateManagedValue(zctVec2));
        PZVector2f(A.GetData)^ := Vector2f(X,Y);
        Env.StackPushPointer(A);
      end;
    fcVec3 :
      begin
        Env.StackPopTo(Z);
        Env.StackPopTo(Y);
        Env.StackPopTo(X);
        A := TDefineArray(CreateManagedValue(zctVec3));
        PZVector3f(A.GetData)^ := Vector3f(X,Y,Z);
        Env.StackPushPointer(A);
      end;
    fcVec4 :
      begin
        Env.StackPopTo(W);
        Env.StackPopTo(Z);
        Env.StackPopTo(Y);
        Env.StackPopTo(X);
        A := TDefineArray(CreateManagedValue(zctVec4));
        PColorf(A.GetData)^ := MakeColorf(X,Y,Z,W);
        Env.StackPushPointer(A);
      end;
  end;
end;


{ TExpGetRawMemElement }

procedure TExpGetRawMemElement.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Type',{$ENDIF}(@_Type), zptByte);
end;

procedure TExpGetRawMemElement.Execute(Env : PExecutionEnvironment);
var
  I3,I2,Index : integer;
  P : PBytes;
begin
  Env.StackPopToPointer(P);

  case Self._Type of
    zctMat4:
      begin
        Env.StackPopTo(I3);
        Env.StackPopTo(I2);
        Index := (I2*4) + I3;
      end;
    zctVec2..zctVec4:
      begin
        Env.StackPopTo(I3);
        Index := I3;
      end;
  else
    Index := 0;
  end;

  P := @P^[Index * 4];
  Env.StackPushPointer(P);
end;

{ TExpArrayUtil }

procedure TExpArrayUtil.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Kind',{$ENDIF}(@Kind), zptByte);
  List.AddProperty({$IFNDEF MINIMAL}'Type',{$ENDIF}(@_Type), zptByte);
end;

procedure TExpArrayUtil.Execute(Env : PExecutionEnvironment);
var
  A,A2 : TDefineArray;
  P : pointer;
begin
  case Kind of
    auArrayToRawMem:
      begin
        Env.StackPopToPointer(A);
        Env.StackPopToPointer(P);
        Move(A.GetData^, P^, A.GetElementSize * A.CalcLimit);
      end;
    auRawMemToArray:
      begin
        Env.StackPopToPointer(P);
        A := TDefineArray(CreateManagedValue(Self._Type));
        Move(P^, A.GetData^, A.GetElementSize * A.CalcLimit);
        Env.StackPushPointer(A);
      end;
    auArrayToArray: //Copy from one array into the memory of another
      begin
        Env.StackPopToPointer(A);
        Env.StackPopToPointer(A2);
        Move(A2.GetData^, A.GetData^, A.GetElementSize * A.CalcLimit);
      end;
  end;
end;

initialization

  ZClasses.Register(TZExpression,ZExpressionClassId);
    {$ifndef minimal}ComponentManager.LastAdded.ImageIndex:=2;{$endif}
  ZClasses.Register(TZLibrary,ZLibraryClassId);
    {$ifndef minimal}ComponentManager.LastAdded.ImageIndex:=36;{$endif}
  ZClasses.Register(TDefineVariable,DefineVariableClassId);
    {$ifndef minimal}ComponentManager.LastAdded.ImageIndex:=8;{$endif}
    {$ifndef minimal}ComponentManager.LastAdded.ZClassName := 'Variable';{$endif}
  ZClasses.Register(TDefineConstant,DefineConstantClassId);
    {$ifndef minimal}ComponentManager.LastAdded.ImageIndex:=29;{$endif}
    {$ifndef minimal}ComponentManager.LastAdded.ZClassName := 'Constant';{$endif}
  ZClasses.Register(TDefineArray,DefineArrayClassId);
    {$ifndef minimal}ComponentManager.LastAdded.ImageIndex:=22;{$endif}
    {$ifndef minimal}ComponentManager.LastAdded.ZClassName := 'Array';{$endif}
  ZClasses.Register(TZExternalLibrary,ExternalLibraryClassId);
    {$ifndef minimal}ComponentManager.LastAdded.ImageIndex:=35;{$endif}

  ZClasses.Register(TExpConstantFloat,ExpConstantFloatClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NoUserCreate:=True;{$endif}
  ZClasses.Register(TExpConstantInt,ExpConstantIntClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NoUserCreate:=True;{$endif}
  ZClasses.Register(TExpOpBinaryFloat,ExpOpBinaryFloatClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NoUserCreate:=True;{$endif}
  ZClasses.Register(TExpOpBinaryInt,ExpOpBinaryIntClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NoUserCreate:=True;{$endif}
  ZClasses.Register(TExpJump,ExpJumpClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NoUserCreate:=True;{$endif}
  ZClasses.Register(TExpFuncCall,ExpFuncCallClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NoUserCreate:=True;{$endif}
  ZClasses.Register(TExpExternalFuncCall,ExpExternalFuncCallClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NoUserCreate:=True;{$endif}
  ZClasses.Register(TExpArrayRead,ExpArrayReadClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NoUserCreate:=True;{$endif}
  ZClasses.Register(TExpArrayGetElement,ExpArrayWriteClassId);
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
  ZClasses.Register(TExpAssignPointer,ExpAssignPointerClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NoUserCreate:=True;{$endif}
  ZClasses.Register(TExpStringConstant,ExpStringConstantClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NoUserCreate:=True;{$endif}
  ZClasses.Register(TExpStringConCat,ExpStringConCatClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NoUserCreate:=True;{$endif}
  ZClasses.Register(TExpPointerFuncCall,ExpPointerFuncCallClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NoUserCreate:=True;{$endif}
  ZClasses.Register(TExpLoadComponent,ExpLoadComponentClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NoUserCreate:=True;{$endif}
  ZClasses.Register(TExpLoadPropOffset,ExpLoadPropOffsetClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NoUserCreate:=True;{$endif}
  ZClasses.Register(TExpLoadModelDefined,ExpLoadModelDefinedClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NoUserCreate:=True;{$endif}
  ZClasses.Register(TExpAddToPointer,ExpAddToPointerClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NoUserCreate:=True;{$endif}
  ZClasses.Register(TExpInvokeComponent,ExpInvokeComponentClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NoUserCreate:=True;{$endif}
  ZClasses.Register(TExpInitLocalArray,ExpInitLocalArrayClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NoUserCreate:=True;{$endif}
  ZClasses.Register(TExpMat4FuncCall,ExpMat4FuncCallClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NoUserCreate:=True;{$endif}
  ZClasses.Register(TExpGetRawMemElement,ExpGetRawMemElementClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NoUserCreate:=True;{$endif}
  ZClasses.Register(TExpArrayUtil,ExpArrayUtilClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NoUserCreate:=True;{$endif}

end.
