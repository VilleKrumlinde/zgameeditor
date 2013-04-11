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
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Value : single;
    IntValue : integer;
    ManagedValue : pointer;
    ModelValue : TZComponent;
    ByteValue : byte;
    IsInitialized : boolean;
    procedure Update; override;
  end;

  //Define a global constant that can be used in expressions
  //Value is copied into code, this component is not streamed in final binary
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
    procedure CleanUpManagedValues(TheType : TZcDataTypeKind; Count : integer; P : PPointer);
    procedure AllocData;
  private
    function PopAndGetElement : PFloat;
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
  end;

  //Virtual machine instruction baseclass
  TExpBase = class(TZComponent)
  protected
    procedure Execute; virtual; abstract;
    {$ifndef minimal}public function ExpAsText : string; virtual;{$endif}
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
    function GetDisplayName: ansistring; override;
    {$endif}
  end;

  TExpConstantInt = class(TExpBase)
  protected
    procedure Execute; override;
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
     fcGetBinaryProp,fcSetBinaryProp,fcGetModels,
     //Mat4
     fcMatMultiply,fcMatTransformPoint,fcGetMatrix,fcSetMatrix,
     fcVec2,fcVec3,fcVec4
     );


  TExpFuncCallBase = class(TExpBase)
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Kind : TExpFuncCallKind;
  end;

  //Built-in function call
  TExpFuncCall = class(TExpFuncCallBase)
  protected
    procedure Execute; override;
  end;

  //Built-in functions that return pointer
  TExpPointerFuncCall = class(TExpFuncCallBase)
  protected
    procedure Execute; override;
  end;

  //Matrix functions
  TExpMat4FuncCall = class(TExpFuncCallBase)
  protected
    procedure Execute; override;
  end;

  //Read value from array and push on stack
  TExpArrayRead = class(TExpBase)
  protected
    procedure Execute; override;
  end;

  //Push ptr to element in array on stack, used with assign
  TExpArrayGetElement = class(TExpBase)
  protected
    procedure Execute; override;
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
    Kind : (loLoad,loStore,loGetAddress);
    Index : integer;
    {$ifndef minimal}
    function ExpAsText : string; override;
    {$endif}
  end;

  //Return from function
  TExpReturn = class(TExpBase)
  protected
    procedure Execute; override;
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
    procedure Execute; override;
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Kind : TExpMiscKind;
    {$ifndef minimal}
    constructor Create(OwnerList: TZComponentList; Kind : TExpMiscKind); overload;
    public function ExpAsText : string; override;
    {$endif}
  end;

  TExpUserFuncCall = class(TExpBase)
  protected
    procedure Execute; override;
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
    procedure Execute; override;
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

  TExpConvertKind = (eckFloatToInt,eckIntToFloat,eckArrayToXptr,eckBinaryToXptr);
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

  //Assign ptr to Pointersize value, both on stack
  TExpAssignPointer = class(TExpBase)
  protected
    procedure Execute; override;
  end;

  //Join two strings
  TExpStringConCat = class(TExpBase)
  protected
    procedure Execute; override;
  end;

  TExpLoadComponent = class(TExpBase)
  protected
    procedure Execute; override;
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Component : TZComponent;
  end;

  TExpLoadPropOffset = class(TExpBase)
  strict private
    IsInit : boolean;
    Offset : integer;
  protected
    procedure Execute; override;
    procedure DefineProperties(List: TZPropertyList); override;
  public
    PropId : integer;
  end;

  TExpLoadModelDefined = class(TExpBase)
  protected
    procedure Execute; override;
    procedure DefineProperties(List: TZPropertyList); override;
  public
    DefinedIndex : integer;
    {$ifndef minimal}
    DefinedName : TPropString;
    {$endif}
  end;

  TExpAddToPointer = class(TExpBase)
  protected
    procedure Execute; override;
  end;

  TExpInvokeComponent = class(TExpBase)
  strict private
    InvokeC : TZComponent;
  protected
    procedure Execute; override;
    procedure DefineProperties(List: TZPropertyList); override;
  public
    InvokedItemList : TZComponentList;
    InvokeClassId : integer;
    InvokeArgCount : integer;
  end;

  TExpInitLocalArray = class(TExpBase)
  protected
    procedure Execute; override;
    procedure DefineProperties(List: TZPropertyList); override;
  public
    StackSlot : integer;
    Dimensions : TArrayDimensions;
    _Type : TZcDataTypeKind;
    Size1,Size2,Size3 : integer;
  end;

  TExpGetRawMemElement = class(TExpBase)
  protected
    procedure Execute; override;
    procedure DefineProperties(List: TZPropertyList); override;
  public
    _Type : TZcDataTypeKind;
  end;

  TExpArrayUtil = class(TExpBase)
  protected
    procedure Execute; override;
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Kind : (auArrayToRawMem, auRawMemToArray, auRawMemToRawMem);
  end;

//Run a compiled expression
//Uses global vars for state.
procedure RunCode(Code : TZComponentList);

{$ifndef minimal}
procedure ResetScriptState;
function GetStackSlots(const Bytes : integer) : integer;
{$endif}

var
  //Return value of last executed expression
  gReturnValue : single;


implementation


uses ZMath, ZPlatform, ZApplication, ZLog, Meshes,
  AudioComponents, AudioPlayer
{$if (not defined(minimal))}, System.SysUtils, System.Math, System.TypInfo{$ifend}
{$if (not defined(minimal)) or (defined(cpux64))}, WinApi.Windows{$ifend};

var
  //Expression execution context
  gCurrentPc : ^TExpBase;
  gCurrentBP : integer;


const
  ZcStackSize=16384;

type
  TStackElement = NativeUInt;
  PStackElement = ^TStackElement;

var
  ZcStack : array[0..ZcStackSize div SizeOf(TStackElement)] of TStackElement;
  ZcStackPtr : PStackElement;

const
  ZcStackBegin : PStackElement = @ZcStack;

{$POINTERMATH ON}

function StackGetDepth : integer; inline;
begin
  //Returns the number of stack elements from start of stack
  Result := (ZcStackPtr - ZcStackBegin);
end;

//Push 32-bit value
procedure StackPush(const X);
begin
  {$ifndef minimal}
  if StackGetDepth>=High(ZcStack) then
    ZHalt('Zc Stack Overflow (infinite recursion?)');
  {$endif}
  PInteger(ZcStackPtr)^ := PInteger(@X)^;
  Inc(ZcStackPtr);
end;

//Push 32 or 64-bit value depending on architechture
procedure StackPushPointer(const X);
begin
  {$ifndef minimal}
  if StackGetDepth>=High(ZcStack) then
    ZHalt('Zc Stack Overflow (infinite recursion?)');
  {$endif}
  ZcStackPtr^ := TStackElement( PPointer(@X)^ );
  Inc(ZcStackPtr);
end;

procedure StackPushValue(X : pointer); inline;
begin
  StackPush(X);
end;

//Pop 32-bit value
procedure StackPopTo(var X);
begin
  {$ifndef minimal}
  if StackGetDepth=0 then
    ZHalt('Zc Stack Underflow');
  {$endif}
  Dec(ZcStackPtr);
  PInteger(@X)^ := PInteger(ZcStackPtr)^;
end;

//Pop 32 or 64-bit value depending on architechture
procedure StackPopToPointer(var X);
begin
  {$ifndef minimal}
  if StackGetDepth=0 then
    ZHalt('Zc Stack Underflow');
  {$endif}
  Dec(ZcStackPtr);
  PPointer(@X)^ := pointer(ZcStackPtr^);
end;

function StackPopFloat : single;
begin
  StackPopTo(Result);
end;

function GetStackSlots(const Bytes : integer) : integer;
begin
  Result := Bytes div SizeOf(TStackElement);
end;

procedure StackPopXBytes(var X; const Bytes : integer);
begin
  {$ifndef minimal}
  if StackGetDepth=0 then
    ZHalt('Zc Stack Underflow');
  {$endif}
  Dec(ZcStackPtr, GetStackSlots(Bytes) );
  Move(ZcStackPtr^, Pointer(@X)^, Bytes);
end;

function StackGetPtrToItem(const Index : integer) : PStackElement; inline;
begin
  Result := @ZcStack;
  Inc(Result,Index);
end;

procedure SaveExecutionState;
begin
  StackPushPointer(gCurrentPc);
  StackPush(gCurrentBp);
end;

procedure RestoreExecutionState;
begin
  StackPopTo(gCurrentBp);
  StackPopToPointer(gCurrentPc);
end;

{$ifndef minimal}
procedure ResetScriptState;
begin
  //Reset stack
  ZcStackPtr := ZcStackBegin;
  gCurrentBP := 0;
end;
{$endif}

procedure RunCode(Code : TZComponentList);
const
  NilP : pointer = nil;
var
{$ifndef minimal}
  GuardLimit,GuardAllocLimit : integer;
{$endif}
  SaveDepth : integer;
begin
  //Pc can be modified in jump-code
  if Code.Count=0 then
    Exit;
  gCurrentPc := Code.GetPtrToItem(0);
  gCurrentBP := 0;

  SaveDepth := StackGetDepth;

  StackPushPointer(NilP); //Push return adress nil

  {$ifndef minimal}
  GuardLimit := 500 * 1000000;
  GuardAllocLimit := ManagedHeap_GetAllocCount + 1000000;
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
      ZHalt('Five hundered million instructions executed. Infinite loop?');
    if ManagedHeap_GetAllocCount>GuardAllocLimit then
      ZHalt('One million strings allocated. Infinite loop?');
    {$endif}
  end;
  if StackGetDepth-SaveDepth=1 then
    StackPopTo(gReturnValue);
  {$ifndef minimal}
  if StackGetDepth-SaveDepth>0 then
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

procedure TExpConstantFloat.Execute;
begin
  StackPush( Constant );
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

procedure TExpConstantInt.Execute;
begin
  StackPush( Constant );
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
procedure TExpOpBinaryFloat.Execute;
var
  A1,A2,V : single;
begin
  StackPopTo(A1);
  StackPopTo(A2);
  case Kind of
    vbkPlus : V := A1 + A2;
    vbkMinus : V := A2 - A1;
    vbkMul : V := A2 * A1;
    vbkDiv : V := A2 / A1;
    {$ifndef minimal}else begin ZHalt('Invalid binary op'); exit; end;{$endif}
  end;
  StackPush(V);
end;
{$ifdef minimal} {$WARNINGS ON} {$endif}

{$ifdef minimal} {$WARNINGS OFF} {$endif}
procedure TExpOpBinaryInt.Execute;
var
  A1,A2,V : integer;
begin
  StackPopTo(A1);
  StackPopTo(A2);
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
    vbkMod : V := A2 mod A1;
    {$ifndef minimal}else begin ZHalt('Invalid binary op'); exit; end;{$endif}
  end;
  StackPush(V);
end;
{$ifdef minimal} {$WARNINGS ON} {$endif}

{ TExpPropPtr }

procedure TExpPropPtr.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Target',{$ENDIF}(@Target), zptPropertyRef);
end;

procedure TExpPropPtr.Execute;
begin
  StackPushValue(GetPropertyRef(Target));
end;

{ TExpJump }

procedure TExpJump.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Kind',{$ENDIF}(@Kind), zptByte);
  List.AddProperty({$IFNDEF MINIMAL}'Destination',{$ENDIF}(@Destination), zptInteger);
  List.AddProperty({$IFNDEF MINIMAL}'Type',{$ENDIF}(@_Type), zptByte);
end;

procedure TExpJump.Execute;
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
            StackPopTo(R);
            StackPopTo(L);
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
            StackPopTo(Ri);
            StackPopTo(Li);
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
            StackPopToPointer(Rp);
            StackPopToPointer(Lp);
            Jump := ZStrCompare(PAnsiChar(Lp),PAnsiChar(Rp));
            if Kind=jsJumpNE then
               Jump := not Jump;
          end;
        //todo: need to compare pointer-size here for win64
      end;
    end;
  end;
  if Jump then
    Inc(gCurrentPc,Destination);
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
    {$ifndef minimal}List.GetLast.HideInGui := True;{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'ModelValue',{$ENDIF}(@ModelValue), zptComponentRef);
    List.GetLast.NeverPersist := True;
    {$ifndef minimal}List.GetLast.SetChildClasses([TModel]);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'ByteValue',{$ENDIF}(@ByteValue), zptByte);
    List.GetLast.NeverPersist := True;
end;

procedure TDefineVariable.Update;
begin
  inherited;
  if not IsInitialized then
  begin
    IsInitialized := True;
    if _Type in [zctMat4,zctVec2,zctVec3,zctVec4] then
      Self.ManagedValue := CreateManagedValue(Self._Type);
  end;
end;

{ TExpFuncCallBase }

procedure TExpFuncCallBase.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Kind',{$ENDIF}(@Kind), zptByte);
end;

{ TExpFuncCall }

{$ifdef minimal} {$WARNINGS OFF} {$endif}
procedure TExpFuncCall.Execute;
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
    fcSin :  V := Sin(StackPopFloat);
    fcSqrt : V := Sqrt(StackPopFloat);
    fcCos : V := Cos(StackPopFloat);
    fcAbs : V := Abs(StackPopFloat);
    fcRnd : V := System.Random;
    fcFrac : V := Frac(StackPopFloat);
    fcExp : V := Exp(StackPopFloat);
    fcTan : V := Tan(StackPopFloat);
    fcCeil : V := Ceil(StackPopFloat);
    fcFloor : V := Floor(StackPopFloat);
    fcAcos : V := ArcCos(StackPopFloat);
    fcAsin : V := ArcSin(StackPopFloat);
    fcLog2 : V := Log2(StackPopFloat);
    fcRound : PInteger(@V)^ := Round(StackPopFloat);

    fcRandom :
      begin
        StackPopTo(A2); //Variance
        StackPopTo(A1); //Base
        V := A1 + ((2*System.Random-1.0) * A2);
      end;
    fcAtan2 :
      begin
        StackPopTo(A2);
        StackPopTo(A1);
        V := ArcTan2(A1,A2);
      end;
    fcNoise2 :
      begin
        StackPopTo(A2);
        StackPopTo(A1);
        V := PerlinNoise2(A1,A2);
      end;
    fcNoise3 :
      begin
        StackPopTo(A3);
        StackPopTo(A2);
        StackPopTo(A1);
        V := PerlinNoise3(A1,A2,A3);
      end;
    fcClamp :
      begin
        StackPopTo(A3);
        StackPopTo(A2);
        StackPopTo(A1);
        V := Clamp(A1,A2,A3);
      end;
    fcPow :
      begin
        StackPopTo(A2);
        StackPopTo(A1);
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
        System.RandSeed := Round(StackPopFloat); //float to int
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
        StackPopTo(I2);
        StackPopTo(I1);
        V := Platform_GetJoystickAxis(I1,I2);
      end;
    fcJoyGetButton :
      begin
        StackPopTo(I2);
        StackPopTo(I1);
        PInteger(@V)^ := Ord(Platform_GetJoystickButton(I1,I2)) and 1;
      end;
    fcJoyGetPOV :
      begin
        StackPopTo(I1);
        V := Platform_GetJoystickPOV(I1);
      end;
    fcSystemTime :
      begin
        PInteger(@V)^ := Platform_GetSystemTime;
      end;
    fcStringLength :
      begin
        StackPopToPointer(P1);
        PInteger(@V)^ := ZStrLength(PAnsiChar(P1));
      end;
    fcStringIndexOf :
      begin
        //x=indexOf("lo","hello",2)
        StackPopTo(I1);
        StackPopToPointer(P1);
        StackPopToPointer(P2);
        PInteger(@V)^ := ZStrPos(PAnsiChar(P2),PAnsiChar(P1),I1);
      end;
    fcStrToInt :
      begin
        StackPopToPointer(P1);
        PInteger(@V)^ := ZStrToInt(PAnsiChar(P1));
      end;
    fcOrd :
      begin
        //i=ord("A")
        StackPopToPointer(P1);
        PInteger(@V)^ := PByte(P1)^;
      end;
    fcTrace :
      begin
        HasReturnValue := False;
        StackPopToPointer(P1);
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
        StackPopTo(I1);
        V := ZApp.NormalizeToScreen( Platform_TouchGetPos(I1) )[0];
      end;
    fcTouchGetY :
      begin
        StackPopTo(I1);
        V := ZApp.NormalizeToScreen( Platform_TouchGetPos(I1) )[1];
      end;
    fcTouchGetID :
      begin
        StackPopTo(I1);
        PInteger(@V)^ := Platform_TouchGetID(I1);
      end;
    fcGetBinaryProp :
      begin
        StackPopToPointer(A);
        StackPopToPointer(Bp);
        A.SizeDim1 := Bp.Size div A.GetElementSize;
        Move(Bp^.Data^, A.GetData^, Bp^.Size);
        HasReturnValue := False;
      end;
    fcSetBinaryProp :
      begin
        StackPopToPointer(A);
        StackPopToPointer(Bp);
        Bp^.Size := A.CalcLimit * A.GetElementSize;
        ReallocMem(Bp^.Data,Bp^.Size);
        Move(A.GetData^ , Bp^.Data^, Bp^.Size);
        HasReturnValue := False;
      end;
    fcGetModels :
      begin
        StackPopTo(I1);
        StackPopToPointer(A);
        L := ZApp.Models.Get(I1);
        A.SizeDim1 := L.Count;
        P1 := A.GetData;
        for I2 := 0 to L.Count-1 do
        begin
          PPointer(P1)^ := L[I2];
          Inc(PPointer(P1));
        end;
      end;
  {$ifndef minimal}else begin ZHalt('Invalid func op'); exit; end;{$endif}
  end;
  if HasReturnValue then
    StackPush(V);
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
  List.AddProperty({$IFNDEF MINIMAL}'IntValue',{$ENDIF}(@IntValue), zptInteger);
   {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
   {$ifndef minimal}List.GetLast.NeedRefreshNodeName := True; {$endif}
  List.AddProperty({$IFNDEF MINIMAL}'StringValue',{$ENDIF}(@StringValue), zptString);
   {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
   {$ifndef minimal}List.GetLast.NeedRefreshNodeName := True; {$endif}
  List.AddProperty({$IFNDEF MINIMAL}'ByteValue',{$ENDIF}(@ByteValue), zptByte);
   {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
   {$ifndef minimal}List.GetLast.NeedRefreshNodeName := True; {$endif}
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

procedure TExpArrayRead.Execute;
var
  V : single;
  P : PFloat;
  I : integer;
  A : TDefineArray;
begin
  StackPopToPointer(A);
  P := A.PopAndGetElement;
  {$ifndef minimal}
  if P=nil then
    ZHalt('Array read outside range: ' + String(A.Name));
  {$endif}
  if A._Type=zctByte then
  begin
    I := PByte(P)^;
    StackPush(I);
  end
  else
  begin
    V := P^;
    //todo: 64 bit
    StackPush( V );
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
  if Data<>nil then
    FreeMem(Data);
  inherited;
end;

function TDefineArray.GetData: PFloat;
begin
  //Check if Array size has changed
  if Limit<>CalcLimit then
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
  Result := SizeDim1 * (SizeDim2 + 1) * (SizeDim3 + 1);
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

function TDefineArray.PopAndGetElement : PFloat;
var
  Index,I1,I2,I3 : integer;
  P : PBytes;
begin
  StackPopTo(I3);
  if Self.Dimensions>=dadTwo then
    StackPopTo(I2)
  else
    I2 := 0;
  if Self.Dimensions=dadThree then
    StackPopTo(I1)
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

{ TExpArrayWrite }

procedure TExpArrayGetElement.Execute;
var
  P : Pointer;
  A : TDefineArray;
begin
  StackPopToPointer(A);
  P := A.PopAndGetElement;
  {$ifndef minimal}
  if P=nil then
    ZHalt('Array assign outside range: ' + String(A.Name));
  {$endif}
  StackPush(P);
end;

{ TExpStackFrame }

procedure TExpStackFrame.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Size',{$ENDIF}(@Size), zptInteger);
end;

procedure TExpStackFrame.Execute;
//http://en.wikipedia.org/wiki/Function_prologue
begin
  StackPush(gCurrentBP);
  gCurrentBP := StackGetDepth;
  //Null-initialize stack frame
  FillChar(ZcStackPtr^,Self.Size * SizeOf(ZcStackPtr^),0);
  //Add frame to stack
  Inc(ZcStackPtr,Self.Size);
end;

{ TExpAccessLocal }

procedure TExpAccessLocal.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Kind',{$ENDIF}(@Kind), zptByte);
  List.AddProperty({$IFNDEF MINIMAL}'Index',{$ENDIF}(@Index), zptInteger);
end;

procedure TExpAccessLocal.Execute;
var
  P : PStackElement;
begin
  //Use pointer size to get all bits in 64-bit mode
  P := StackGetPtrToItem( gCurrentBP + Self.Index );
  case Kind of
    loLoad: StackPushPointer(P^);
    loStore: StackPopToPointer(P^);
    loGetAddress: StackPushPointer(P);
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
procedure TExpReturn.Execute;
var
  RetVal : pointer;
begin
  if HasReturnValue then
  begin
    //Local0 holds returnvalue
    //Treat return value as pointer to get all bits in 64-bit mode
    RetVal := PPointer( StackGetPtrToItem( gCurrentBP ) )^;
  end;

  if HasFrame then
  begin
    Dec(ZcStackPtr,StackGetDepth-gCurrentBP);
    StackPopTo(gCurrentBP);
  end;

  //Get return adress
  StackPopToPointer(gCurrentPc);

  //Clean stack of function arguments
  Dec(ZcStackPtr,Arguments);

  if HasReturnValue then
  begin
    StackPushPointer(RetVal);
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
      zptPropertyRef :
        begin
          S := String(Value.PropertyValue.Component.Name) + ' ' + String(Value.PropertyValue.Prop.Name);
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
  List.AddProperty({$IFNDEF MINIMAL}'Kind',{$ENDIF}(@Kind), zptByte);
end;

procedure TExpMisc.Execute;
var
  V : integer;
  P : pointer;
begin
  case Kind of
    emPop: StackPopFloat;  //Pop, discard value from top of stack
    emDup :
      begin
        StackPopTo(V);
        StackPush(V);
        StackPush(V);
      end;
    emLoadCurrentModel :
      StackPushPointer( Meshes.CurrentModel );
    emPtrDeref4 :
      begin
        StackPopToPointer(P);
        {$ifndef MINIMAL}
        CheckNilDeref(P);
        {$endif}
        V := PInteger(P)^;
        StackPush(V);
      end;
    emPtrDeref1 :
      begin
        StackPopToPointer(P);
        {$ifndef MINIMAL}
        CheckNilDeref(P);
        {$endif}
        V := PByte(P)^;
        StackPush(V);
      end;
    emPtrDerefPointer :
      begin
        StackPopToPointer(P);
        {$ifndef MINIMAL}
        CheckNilDeref(P);
        {$endif}
        StackPushPointer(P^);
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
end;

{ TExpUserFuncCall }

procedure TExpUserFuncCall.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Lib',{$ENDIF}(@Lib), zptComponentRef);
  List.AddProperty({$IFNDEF MINIMAL}'Index',{$ENDIF}(@Index), zptInteger);
end;

procedure TExpUserFuncCall.Execute;
begin
  StackPushPointer(gCurrentPC);
  gCurrentPC := Lib.Source.Code.GetPtrToItem(Index);
  Dec(gCurrentPc);
end;

{ TExpConvert }

procedure TExpConvert.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Kind',{$ENDIF}(@Kind), zptByte);
end;

procedure TExpConvert.Execute;
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
        I := Trunc(StackPopFloat);
        StackPush(I);
      end;
    eckIntToFloat :
      begin
        StackPopTo(I);
        V := I;
        StackPush(V);
      end;
    eckArrayToXptr :
      begin
        StackPopToPointer(D);
        P := D.GetData;
        StackPushPointer(P);
      end;
    eckBinaryToXptr :
      begin
        StackPopToPointer(Bp);
        P := Bp.Data;
        StackPushPointer(P);
      end;
  end;
end;

{ TExpAssign4 }

procedure TExpAssign4.Execute;
var
  I : integer;
  P : pointer;
begin
  StackPopTo(I);
  StackPopToPointer(P);
  PInteger(P)^ := I;
end;

{ TExpAssign1 }

procedure TExpAssign1.Execute;
var
  V : integer;
  B : byte;
  P : pointer;
begin
  //Cast integer to byte before assigning
  StackPopTo(V);
  StackPopToPointer(P);
  B := V;
  PByte(P)^ := B;
end;

{ TExpAssignPointer }

procedure TExpAssignPointer.Execute;
var
  V,P : pointer;
begin
  StackPopToPointer(V);
  StackPopToPointer(P);
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
begin
  Result := inherited GetDisplayName + ' <' +
    AnsiString(Self.GetProperties.GetByName('Type').Options[ Ord(Self._Type) ]) + '>';
end;
{$endif}

{ TExpStringConstant }

procedure TExpStringConstant.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$ifndef minimal}'Value',{$ENDIF}(@Value), zptString);
end;

{ TExpStringConCat }

procedure TExpStringConCat.Execute;
var
  P1,P2,Dest : PAnsiChar;
  I : integer;
begin
  StackPopToPointer(P2);
  StackPopToPointer(P1);

  I := ZStrLength(P1) + ZStrLength(P2);

  //Add to gc
  Dest := ManagedHeap_Alloc(I+1);

  ZStrCopy(Dest,P1);
  ZStrCat(Dest,P2);

  StackPushPointer(Dest);
end;

{ TExpPointerFuncCall }

procedure TExpPointerFuncCall.Execute;
var
  I1,I2 : integer;
  Buf : array[0..15] of ansichar;
  P1,Dest : PAnsiChar;
  M : TModel;
begin
  case Kind of
    fcIntToStr:
      begin
        StackPopTo(I1);
        ZStrConvertInt(I1,PAnsiChar(@Buf));
        Dest := ManagedHeap_Alloc(ZStrLength(@Buf)+1);
        ZStrCopy(Dest,@Buf);
      end;
    fcSubStr :
      begin
        //s=subStr("hello",0,2)
        StackPopTo(I1);
        StackPopTo(I2);
        StackPopToPointer(P1);
        Dest := ManagedHeap_Alloc(I1+1);
        ZStrSubString(P1,Dest,I2,I1);
      end;
    fcChr :
      begin
        //s=chr(65);
        StackPopTo(I1);
        Dest := ManagedHeap_Alloc(2);
        Dest^ := PAnsiChar(@I1)^;
        PBytes(Dest)^[1] := 0;
      end;
    fcCreateModel :
      begin
        StackPopToPointer(M);
        SaveExecutionState;
          //AddToScene will call m.OnSpawn which in turn can run expressions
          M := TModel(M.Clone);
          M.AddToScene(ZApp);
        RestoreExecutionState;
        Dest := pointer(M);
      end;
  end;
  StackPushPointer(Dest);
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
    SaveExecutionState;
      ZExpressions.RunCode(BeforeInitExp.Code);
    RestoreExecutionState;
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
procedure DummyProc(i1,i2,i3,i4,i5,i6,i7,i8 : integer);
begin
end;

procedure TExpExternalFuncCall.Execute;
type
  TFunc = procedure();
  PFunc = ^TFunc;
var
  Arg1,I,RetVal : integer;
  TheFunc : PFunc;
  Args : array[0..31] of integer;
begin
  if Self.Proc=nil then
  begin
    Self.Proc := Lib.LoadFunction(Self.FuncName);
    //Make sure android generates enough stack space for calling a func with 8 params
    DummyProc(1,2,3,4,5,6,7,8);
  end;
  TheFunc := Self.Proc;

  //Transfer arguments from Zc-stack to hardware stack
  for I := 0 to ArgCount-1 do
    StackPopTo(Args[ArgCount-I-1]);

  //http://en.wikipedia.org/wiki/Calling_convention#ARM
  //First params in r-registers
  //Then on stack
  asm
    ldr r0, Args
    ldr r1, Args+4
    ldr r2, Args+8
    ldr r3, Args+12

    ldr r4, Args+16
	  str	r4,[r13]
    ldr r4, Args+20
	  str	r4,[r13, #4]
    ldr r4, Args+24
	  str	r4,[r13, #8]
    ldr r4, Args+28
	  str	r4,[r13, #12]

    ldr r4,TheFunc
    blx r4

    str r0,RetVal
  end;

  if Self.ReturnType.Kind<>zctVoid then
    StackPush(RetVal);
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
        Assert('This argument type not yet supported on 64-bit:');
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
        Assert('This argument type not yet supported on 64-bit');
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

procedure TExpExternalFuncCall.Execute;
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
      StackPopToPointer(Args[I])
    else
      StackPopTo(Args[I]);

  Retval := TFunc(Self.Trampoline)(@Args);

  if Self.ReturnType.Kind<>zctVoid then
  begin
    if GetZcTypeSize(Self.ReturnType.Kind)=SizeOf(Pointer) then
      StackPushPointer(RetVal)
    else
      StackPush(RetVal);
  end;
end;

destructor TExpExternalFuncCall.Destroy;
begin
  FreeMem(Trampoline);
end;

{$elseif defined(cpux86) or defined(CPU32)}  //CPU32 is for Freepascal

procedure TExpExternalFuncCall.Execute;
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
    StackPopTo(Args[I]);

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
    StackPush(RetVal);
end;
{$ifend}

{ TExpLoadComponent }

procedure TExpLoadComponent.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Component',{$ENDIF}(@Component), zptComponentRef);
end;

procedure TExpLoadComponent.Execute;
begin
  StackPushPointer(Self.Component);
end;

{ TExpLoadPropOffset }

procedure TExpLoadPropOffset.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'PropId',{$ENDIF}(@PropId), zptInteger);
end;

procedure TExpLoadPropOffset.Execute;
var
  C : TZComponent;
begin
  if not IsInit then
  begin
    StackPopToPointer(C);
    {$ifndef minimal}
    CheckNilDeref(C);
    {$endif}
    Self.Offset := C.GetProperties.GetById(Self.PropId).Offset;
    StackPushPointer(C);
    IsInit := True;
  end;
  StackPush(Self.Offset);
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

procedure TExpLoadModelDefined.Execute;
var
  M : TModel;
  C : TZComponent;
begin
  StackPopToPointer(M);
  {$ifndef minimal}
  CheckNilDeref(M);
  if (Self.DefinedIndex>=M.Definitions.Count) or
    (not SameText(String(TZComponent(M.Definitions[Self.DefinedIndex]).Name),String(DefinedName))) then
  begin
    ZHalt('Defined var mismatch "' + DefinedName + '" in model "' + String(M.Name) + '" must be at position ' + IntToStr(Self.DefinedIndex) + ' in Definitions-list.');
  end;
  {$endif}
  C := TZComponent(M.Definitions[Self.DefinedIndex]);
  StackPushPointer( C );
end;

{ TExpAddToPointer }

procedure TExpAddToPointer.Execute;
//Add 32-bit value to pointer and store the result as pointer
var
  V : integer;
  P : pbyte;
begin
  StackPopTo(V);
  StackPopToPointer(P);
  Inc(P,V);
  StackPushPointer(P);
end;

{ TExpInvokeComponent }

procedure TExpInvokeComponent.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'InvokeClassId',{$ENDIF}(@InvokeClassId), zptInteger);
  List.AddProperty({$IFNDEF MINIMAL}'InvokeArgCount',{$ENDIF}(@InvokeArgCount), zptInteger);
  List.AddProperty({$IFNDEF MINIMAL}'InvokedItemList',{$ENDIF}(@InvokedItemList), zptComponentList);
    List.GetLast.NeverPersist := True;
end;

procedure TExpInvokeComponent.Execute;
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
  end;

  for I := 0 to InvokeArgCount-1 do
  begin
    StackPopTo(PropId);
    StackPopTo(RawValue);
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

  SaveExecutionState;
    TCommand(InvokeC).Execute;
  RestoreExecutionState;
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

procedure TExpInitLocalArray.Execute;
var
  P : PStackElement;
  A : TDefineArray;
begin
  //Use pointer size to get all bits in 64-bit mode
  P := StackGetPtrToItem( gCurrentBP + Self.StackSlot );
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

procedure TExpMat4FuncCall.Execute;
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
        StackPopToPointer(M1);
        StackPopToPointer(M2);
        A := TDefineArray(CreateManagedValue(zctMat4));
        M1 := PZMatrix4f(TDefineArray(M1).GetData);
        M2 := PZMatrix4f(TDefineArray(M2).GetData);
        PZMatrix4f(A.GetData)^ := MatrixMultiply(M1^,M2^);
        StackPushPointer(A);
      end;
    fcMatTransformPoint :
      begin
        StackPopToPointer(V1);
        StackPopToPointer(M1);
        A := TDefineArray(CreateManagedValue(zctVec3));
        V1 := PZVector3f(TDefineArray(V1).GetData);
        M1 := PZMatrix4f(TDefineArray(M1).GetData);
        VectorTransform(V1^,M1^,PZVector3f(A.GetData)^);
        StackPushPointer(A);
      end;
    fcGetMatrix :
      begin
        StackPopToPointer(A);
        StackPopTo(I);
        Self.ZApp.Driver.GetMatrix(I, PZMatrix4f(A.GetData));
      end;
    fcSetMatrix :
      begin
        StackPopToPointer(A);
        StackPopTo(I);
        Self.ZApp.Driver.SetMatrix(I, PZMatrix4f(A.GetData)^);
      end;
    fcVec2 :
      begin
        StackPopTo(Y);
        StackPopTo(X);
        A := TDefineArray(CreateManagedValue(zctVec2));
        PZVector2f(A.GetData)^ := Vector2f(X,Y);
        StackPushPointer(A);
      end;
    fcVec3 :
      begin
        StackPopTo(Z);
        StackPopTo(Y);
        StackPopTo(X);
        A := TDefineArray(CreateManagedValue(zctVec3));
        PZVector3f(A.GetData)^ := Vector3f(X,Y,Z);
        StackPushPointer(A);
      end;
    fcVec4 :
      begin
        StackPopTo(W);
        StackPopTo(Z);
        StackPopTo(Y);
        StackPopTo(X);
        A := TDefineArray(CreateManagedValue(zctVec4));
        PColorf(A.GetData)^ := MakeColorf(X,Y,Z,W);
        StackPushPointer(A);
      end;
  end;
end;


{ TExpGetRawMemElement }

procedure TExpGetRawMemElement.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Type',{$ENDIF}(@_Type), zptByte);
end;

procedure TExpGetRawMemElement.Execute;
var
  I3,I2,Index : integer;
  P : PBytes;
begin
  StackPopToPointer(P);

  case Self._Type of
    zctMat4:
      begin
        StackPopTo(I3);
        StackPopTo(I2);
        Index := (I2*4) + I3;
      end;
    zctVec2..zctVec4:
      begin
        StackPopTo(I3);
        Index := I3;
      end;
  end;

  P := @P^[Index * 4];
  StackPushPointer(P);
end;

{ TExpArrayUtil }

procedure TExpArrayUtil.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Kind',{$ENDIF}(@Kind), zptByte);
end;

procedure TExpArrayUtil.Execute;
var
  A : TDefineArray;
  P,P2 : pointer;
  Size : integer;
begin
  case Kind of
    auArrayToRawMem:
      begin
        StackPopToPointer(A);
        StackPopToPointer(P);
        Move(A.GetData^, P^, A.GetElementSize * A.CalcLimit);
      end;
    auRawMemToArray:
      begin
        StackPopToPointer(P);
        StackPopToPointer(A);
        Move(P^, A.GetData^, A.GetElementSize * A.CalcLimit);
      end;
    auRawMemToRawMem:
      begin
        StackPopTo(Size);
        StackPopToPointer(P);
        StackPopToPointer(P2);
        Move(P^, P2^, Size);
      end;
  end;
end;

initialization

  ZcStackPtr := ZcStackBegin;

  ZClasses.Register(TZExpression,ZExpressionClassId);
    {$ifndef minimal}ComponentManager.LastAdded.ImageIndex:=2;{$endif}
  ZClasses.Register(TZLibrary,ZLibraryClassId);
    {$ifndef minimal}ComponentManager.LastAdded.ImageIndex:=36;{$endif}
  ZClasses.Register(TDefineVariable,DefineVariableClassId);
    {$ifndef minimal}ComponentManager.LastAdded.ImageIndex:=8;{$endif}
    {$ifndef minimal}ComponentManager.LastAdded.ZClassName := 'Variable';{$endif}
  ZClasses.Register(TDefineConstant,DefineConstantClassId);
    {$ifndef minimal}ComponentManager.LastAdded.ExcludeFromBinary:=True;{$endif}
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
  ZClasses.Register(TExpPropPtr,ExpPropPtrClassId);
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
