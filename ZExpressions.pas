{Copyright (c) 2020 Ville Krumlinde

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

{$include zzdc_globalopt.inc}

interface

uses ZClasses;

resourcestring
  strExpression = 'Expression';
  strConstant = 'Constant';
  strKind = 'Kind';
  strDestination = 'Destination';
  strType = 'Type';
  strValue = 'Value';
  strIntValue = 'IntValue';
  strStringValue = 'StringValue';
  strModelValue = 'ModelValue';
  strByteValue = 'ByteValue';
  strPointerValue = 'PointerValue';
  strDimensions = 'Dimensions';
  strSizeDim1 = 'SizeDim1';
  strSizeDim2 = 'SizeDim2';
  strSizeDim3 = 'SizeDim3';
  strPersistent = 'Persistent';
  strValues = 'Values';
  strSize = 'Size';
  strIndex = 'Index';
  strOffset = 'Offset';
  strLib = 'Lib';
  strHasFrame = 'HasFrame';
  strHasReturnValue = 'HasReturnValue';
  strArguments = 'Arguments';
  strSource = 'Source';
  strUseThreadLock = 'UseThreadLock';
  strHasInitializer = 'HasInitializer';
  strGlobalAreaSize = 'GlobalAreaSize';
  strManagedVariables = 'ManagedVariables';
  strModuleName = 'ModuleName';
  strCallingConvention = 'CallingConvention';
  strBeforeInitExp = 'BeforeInitExp';
  strDefinitionsFile = 'DefinitionsFile';
  strFuncName = 'FuncName';
  strArgCount = 'ArgCount';
  strReturnType = 'ReturnType';
  strArgTypes = 'ArgTypes';
  strComponent = 'Component';
  strPropId = 'PropId';
  strDefinedIndex = 'DefinedIndex';
  strDefinedName = 'DefinedName';
  strComponentRef = 'ComponentRef';
  strInvokeClassId = 'InvokeClassId';
  strInvokeArgCount = 'InvokeArgCount';
  strInvokedItemList = 'InvokedItemList';
  strIsValue = 'IsValue';
  strSize1 = 'Size1';
  strSize2 = 'Size2';
  strSize3 = 'Size3';
  strLowBound = 'LowBound';
  strHighBound = 'HighBound';
  strDefaultOrExit = 'DefaultOrExit';
  strJumps = 'Jumps';
  strTheClass = 'TheClass';
  strSizeInBytes = 'SizeInBytes';
  strManagedFields = 'ManagedFields';
  strBaseClass = 'BaseClass';
  strVmt = 'Vmt';
  strDefinedInLib = 'DefinedInLib';
  strInitializerIndex = 'InitializerIndex';
  strVmtIndex = 'VmtIndex';

type
  TExpBase = class;

  //Expression execution context
  PExecutionEnvironment = ^TExecutionEnvironment;
  TExecutionEnvironment = record
  const
    ZcStackSize=16384;
  type
    TStackElement = NativeUInt;
    PStackElement = ^TStackElement;
    PExpBase = ^TExpBase;
  private
    gCurrentPc : PExpBase;
    gCurrentBP : integer;
  var
    ZcStack : array[0..ZcStackSize div SizeOf(TStackElement)] of TStackElement;
    ZcStackPtr : PStackElement;
  strict private
    {$ifdef CALLSTACK}
    procedure LogCallstack;
    {$endif}
  private
    function StackGetDepth : integer; inline;
    procedure StackPopTo(var X); {$IFDEF RELEASE}inline;{$ENDIF}
    procedure StackPopToPointer(var X); {$IFDEF RELEASE}inline;{$ENDIF}
    function StackPopFloat : single;
    {$ifndef minimal}
    procedure ScriptError(const S : string);
    {$endif}
    {$ifdef CALLSTACK}
    procedure CheckNilDeref(P : pointer);
    {$endif}
    {$if defined(cpux64) or defined(cpuaarch64)}
    function StackPopAndGetPtr(const Count : integer) : PStackElement; inline;
    {$endif}
  public
    function StackGetPtrToItem(const Index : integer) : PStackElement; inline;
    procedure StackPush(const X); {$IFDEF RELEASE}inline;{$ENDIF}
    procedure StackPushPointer(const X); {$IFDEF RELEASE}inline;{$ENDIF}
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
  strict private
    Lock : pointer;
    HasExecutedInitializer : boolean;
    procedure InitGlobalArea;
    procedure RemoveManagedTargets;
  private
    procedure AquireLock;
    procedure ReleaseLock;
  protected
    procedure DefineProperties(List: TZPropertyList); override;
    procedure InitAfterPropsAreSet; override;
  public
    UseThreadLock : boolean;
    Source : TZExpressionPropValue;
    HasInitializer : boolean;
    GlobalArea : pointer; //Storage for non-managed global variables
    GlobalAreaSize : integer;
    ManagedVariables : TZBinaryPropValue;
    procedure Update; override;
    destructor Destroy; override;
    {$ifndef minimal}
    procedure DesignerReset; override;
    procedure AddGlobalVar(const Typ : TZcDataType);
    {$endif}
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
    destructor Destroy; override;
    {$endif}
  end;


  TDefineVariableBase = class(TZComponent)
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    _Type : TZcDataType;
    {$ifndef minimal}
    function GetDisplayName: ansistring; override;
    {$endif}
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
    ModelValue : TZComponent;
    ByteValue : byte;
    StringValue : TPropString;
    PointerValue : pointer;
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
    _Type : (jutFloat,jutInt,jutString,jutPointer);
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
     ,fcGenLValue,
     //IDE
     fcFindComponent,fcCreateComponent,fcSetNumericProperty,fcSetStringProperty,
     fcSetObjectProperty,fcSaveComponentToTextFile,
     fcGetStringProperty
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

  {$ifndef minimal}
  //IDE/Visualizer "meta" functions
  TExpIDEFuncCall = class(TExpFuncCallBase)
  protected
    procedure Execute(Env : PExecutionEnvironment); override;
  end;
  {$endif}

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

  //Load/store global (non-managed) value
  TExpAccessGlobal = class(TExpBase)
  protected
    procedure Execute(Env : PExecutionEnvironment); override;
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Kind : (glLoad,glStore,glGetAddress);
    Offset : integer;
    Lib : TZLibrary;
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
    Lib : TZLibrary;
    HasFrame : boolean;
    HasReturnValue : boolean;
    Arguments : integer;
    {$ifdef CALLSTACK}
    FunctionName : string;
    {$endif}
  end;

  TExpMiscKind = (emPop,emDup,emLoadCurrentModel,emPtrDeref4,emPtrDeref1,
    emPtrDerefPointer, emNotifyPropChanged, emLoadNull, emNop, emBinaryNot,
    emNot,emGetUserClass,emGetGlobalDataProp);
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
    {$ifndef minimal}
    Ref : TObject; //points to TZcOpFunctionUserDefined, used in RetroCoding
    {$endif}
  end;

  TExpExternalFuncCall = class(TExpBase)
  strict private
    Proc : pointer;
    {$if defined(cpux64) or defined(cpuaarch64)}
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
    ArgTypes : TPropString;
    {$if defined(cpux64) or defined(cpuaarch64)}
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
    ComponentRef : TZComponent;
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

  TExpInitArray = class(TExpBase)
  protected
    procedure Execute(Env : PExecutionEnvironment); override;
    procedure DefineProperties(List: TZPropertyList); override;
  public
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

  TExpSwitchTable = class(TExpBase)
  protected
    procedure Execute(Env : PExecutionEnvironment); override;
    procedure DefineProperties(List: TZPropertyList); override;
  public
    LowBound,HighBound : integer;
    DefaultOrExit : integer;
    Jumps : TZBinaryPropValue;
  end;

  TCodeReturnValue = record
  case boolean of
    True : (PointerValue : pointer);
    False : (SingleValue : single);
  end;

  TUserClass = class(TZComponent)
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    SizeInBytes : integer;
    ManagedFields : TZBinaryPropValue;
    BaseClass : TUserClass;
    Vmt : TZBinaryPropValue;
    DefinedInLib : TZLibrary;
    InitializerIndex : integer;
  end;

  TUserClassInstance = class
  strict private
    ManagedCount : integer;
    ManagedCleanup : PPointer;
  public
    InstanceData : pointer;
    TheClass : TUserClass;
    constructor Create(TheClass : TUserClass);
    destructor Destroy; override;
  end;

  TExpNewClassInstance = class(TExpBase)
  protected
    procedure Execute(Env : PExecutionEnvironment); override;
    procedure DefineProperties(List: TZPropertyList); override;
  public
    TheClass : TUserClass;
  end;

  TExpVirtualFuncCall = class(TExpBase)
  protected
    procedure Execute(Env : PExecutionEnvironment); override;
    procedure DefineProperties(List: TZPropertyList); override;
  public
    VmtIndex : integer;
  end;


//Run a compiled expression
//Uses global vars for state.
function RunCode(Code : TZComponentList; Env : PExecutionEnvironment=nil) : TCodeReturnValue;

function ExpGetValue(Code : TZComponentList) : single;
function ExpGetPointer(Code : TZComponentList) : PFloat;


implementation


uses ZMath, ZPlatform, ZApplication, ZLog, Meshes,
  AudioComponents, AudioPlayer
{$ifndef MSWINDOWS} {$ifdef fpc}, BaseUnix{$endif} {$endif}
{$if (not defined(minimal))}, SysUtils, Math, TypInfo, Classes{$endif}
{$if defined(cpux64) and defined(mswindows)}, Windows{$endif}
  ;

{$POINTERMATH ON}

function ExpGetValue(Code : TZComponentList) : single;
begin
  Result := RunCode(Code).SingleValue;
end;

function ExpGetPointer(Code : TZComponentList) : ZClasses.PFloat;
begin
  Result := RunCode(Code).PointerValue;
end;


procedure TExecutionEnvironment.Init;
begin
  ZcStackPtr := @ZcStack;
  gCurrentBP := 0;
end;

{$if defined(cpux64) or defined(cpuaarch64)}
function TExecutionEnvironment.StackPopAndGetPtr(const Count: integer): PStackElement;
begin
  Dec(ZcStackPtr,Count);
  Result := ZcStackPtr;
end;
{$endif}

function TExecutionEnvironment.StackGetDepth : integer;
begin
  //Returns the number of stack elements from start of stack
  Result := (ZcStackPtr - PStackElement(@ZcStack));
end;

//Push 32-bit value
procedure TExecutionEnvironment.StackPush(const X);
begin
  {$ifdef debug}
  if StackGetDepth>=High(ZcStack) then
    ScriptError('Zc Stack Overflow (infinite recursion?)');
  {$endif}
  PInteger(ZcStackPtr)^ := PInteger(@X)^;
  Inc(ZcStackPtr);
end;

//Push 32 or 64-bit value depending on architechture
procedure TExecutionEnvironment.StackPushPointer(const X);
begin
  {$ifdef debug}
  if StackGetDepth>=High(ZcStack) then
    ScriptError('Zc Stack Overflow (infinite recursion?)');
  {$endif}
  ZcStackPtr^ := TStackElement( PPointer(@X)^ );
  Inc(ZcStackPtr);
end;

//Pop 32-bit value
procedure TExecutionEnvironment.StackPopTo(var X);
begin
  {$ifdef debug}
  if StackGetDepth=0 then
    ScriptError('Zc Stack Underflow');
  {$endif}
  Dec(ZcStackPtr);
  PInteger(@X)^ := PInteger(ZcStackPtr)^;
end;

//Pop 32 or 64-bit value depending on architechture
procedure TExecutionEnvironment.StackPopToPointer(var X);
begin
  {$ifdef debug}
  if StackGetDepth=0 then
    ScriptError('Zc Stack Underflow');
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

{$ifdef CALLSTACK}
procedure TExecutionEnvironment.LogCallstack;
var
  Log : TLog;

  procedure DoOne(P : PExpBase);
  begin
    while P<>nil do
    begin
      if (P^ is TExpReturn) then
      begin
        Log.Error(TExpReturn(P^).FunctionName);
        TExpReturn(P^).Execute(@Self);
        P := Self.gCurrentPc;
        Continue;
      end;
      Inc(P);
    end;
  end;

begin
  Log := ZLog.GetLog('Zc');
  Log.Error('Callstack:');
  DoOne(Self.gCurrentPc);
end;

procedure TExecutionEnvironment.CheckNilDeref(P : pointer);
begin
  if NativeUInt(P)<=1024 then
    Self.ScriptError('Null pointer referenced in expression');
end;
{$endif}

{$ifndef minimal}
procedure TExecutionEnvironment.ScriptError(const S : string);
begin
  {$ifdef CALLSTACK}
  LogCallstack;
  {$endif}
  ZHalt(S);
end;
{$endif}

function RunCode(Code : TZComponentList; Env : PExecutionEnvironment=nil) : TCodeReturnValue;
{$IFDEF ZDESIGNER}
  {$DEFINE GUARD_LIMIT}
{$ENDIF}
const
  NilP : pointer = nil;
var
{$ifdef GUARD_LIMIT}
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

  {$ifdef GUARD_LIMIT}
  GuardLimit := Round(1E8);
  GuardAllocLimit := ManagedHeap_GetAllocCount + 1000000*10;
  {$endif}
  while True do
  begin
    TExpBase(Env.gCurrentPc^).Execute(Env);
    if Env.gCurrentPc=nil then
      Break;
    Inc(Env.gCurrentPc);
    {$ifdef GUARD_LIMIT}
    Dec(GuardLimit);
    if (GuardLimit=0) and (TThread.CurrentThread.ThreadID=MainThreadID) then
      Env.ScriptError('Infinite loop?');
    if ManagedHeap_GetAllocCount>GuardAllocLimit then
      Env.ScriptError('Ten million strings allocated. Infinite loop?');
    {$endif}
  end;
  if Env.StackGetDepth=1 then
    Env.StackPopToPointer(Result.PointerValue);
  {$ifdef ZDESIGNER}
  if (Env=@LocalEnv) and (Env.StackGetDepth>0) then
    ZLog.GetLog('Zc').Warning('Stack not empty on script completion');
  {$endif}
end;

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
        A._Type.Kind := zctFloat;
        Result := A;
      end;
    zctVec2, zctVec3, zctVec4 :
      begin
        A := TDefineArray.Create(nil);
        A.Dimensions := dadOne;
        A.SizeDim1 := 2 + Ord(Typ)-Ord(zctVec2);
        A._Type.Kind := zctFloat;
        Result := A;
      end;
  end;
  ManagedHeap_AddValueObject(Result);
end;

{ TZExpression }

procedure TZExpression.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}strExpression,{$ENDIF}(@Expression), zptExpression);
end;

procedure TZExpression.Execute;
begin
  ZExpressions.RunCode(Expression.Code);
end;

{ TExpConstantFloat }

procedure TExpConstantFloat.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}strConstant,{$ENDIF}(@Constant), zptFloat);
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
  List.AddProperty({$IFNDEF MINIMAL}strConstant,{$ENDIF}(@Constant), zptInteger);
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
  List.AddProperty({$IFNDEF MINIMAL}strKind,{$ENDIF}(@Kind), zptByte);
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
    {$ifndef minimal}else Env.ScriptError('Invalid binary op'); {$endif}
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
    {$ifndef minimal}else Env.ScriptError('Invalid binary op'); {$endif}
  end;
  Env.StackPush(V);
end;
{$ifdef minimal} {$WARNINGS ON} {$endif}

{ TExpJump }

procedure TExpJump.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}strKind,{$ENDIF}(@Kind), zptByte);
  List.AddProperty({$IFNDEF MINIMAL}strDestination,{$ENDIF}(@Destination), zptInteger);
  List.AddProperty({$IFNDEF MINIMAL}strType,{$ENDIF}(@_Type), zptByte);
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
            {$ifndef minimal}else Env.ScriptError('Invalid jump op');{$endif}
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
            {$ifndef minimal}else Env.ScriptError('Invalid jump op');{$endif}
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
        jutPointer:
          begin
            Env.StackPopToPointer(Rp);
            Env.StackPopToPointer(Lp);
            Jump := Rp=Lp;
            if Kind=jsJumpNE then
              Jump := not Jump;
          end;
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
  List.AddProperty({$IFNDEF MINIMAL}strValue,{$ENDIF}(@Value), zptFloat);
    //Variabler �r ingen ide att spara, de m�ste s�ttas ifr�n kod
    List.GetLast.NeverPersist := True;
  List.AddProperty({$IFNDEF MINIMAL}strIntValue,{$ENDIF}(@IntValue), zptInteger);
    List.GetLast.NeverPersist := True;
  List.AddProperty({$IFNDEF MINIMAL}strStringValue,{$ENDIF}(@StringValue), zptString);
    List.GetLast.NeverPersist := True;
    List.GetLast.IsManagedTarget := True;
  List.AddProperty({$IFNDEF MINIMAL}strModelValue,{$ENDIF}(@ModelValue), zptComponentRef);
    List.GetLast.NeverPersist := True;
    {$ifndef minimal}List.GetLast.SetChildClasses([TModel]);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}strByteValue,{$ENDIF}(@ByteValue), zptByte);
    List.GetLast.NeverPersist := True;
  List.AddProperty({$IFNDEF MINIMAL}strPointerValue,{$ENDIF}(@PointerValue), zptPointer);
    List.GetLast.NeverPersist := True;
    List.GetLast.IsManagedTarget := True;
    {$ifdef zgeviz} //In zgeviz whole apps are cloned and then copy pointervalue is not correct behavior
    List.GetLast.DontClone := True;
    {$endif}
    {$ifndef minimal}List.GetLast.HideInGui := True;{$endif}
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
  if (Self._Type.Kind in [zctMat4,zctVec2,zctVec3,zctVec4]) and (Self.PointerValue=nil) then
    //Create default empty value
    Self.PointerValue := CreateManagedValue(Self._Type.Kind);
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
  List.AddProperty({$IFNDEF MINIMAL}strKind,{$ENDIF}(@Kind), zptByte);
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
  I,J : integer;
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
        Env.ScriptError('Quit called');
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
        {$ifdef linux}
        writeln(PAnsiChar(P1));
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
        if A.GetElementSize*A.CalcLimit<>Bp^.Size then
          //Only resize if needed
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
        J := 0;
        for I := 0 to L.Count-1 do
        begin
          if TModel(L[I]).Active then
          begin //Only copy non-removed models
            PPointer(P1)^ := L[I];
            Inc(PPointer(P1));
            Inc(J);
          end;
        end;
        A.SizeDim1 := J; //Set to actual models copied over
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
  {$ifndef minimal}else Env.ScriptError('Invalid func op'); {$endif}
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
  List.AddProperty({$IFNDEF MINIMAL}strValue,{$ENDIF}(@Value), zptFloat);
   {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
   {$ifndef minimal}List.GetLast.NeedRefreshNodeName := True; {$endif}
   {$ifndef minimal}List.GetLast.ExcludeFromBinary := True; {$endif}
  List.AddProperty({$IFNDEF MINIMAL}strIntValue,{$ENDIF}(@IntValue), zptInteger);
   {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
   {$ifndef minimal}List.GetLast.NeedRefreshNodeName := True; {$endif}
   {$ifndef minimal}List.GetLast.ExcludeFromBinary := True; {$endif}
  List.AddProperty({$IFNDEF MINIMAL}strStringValue,{$ENDIF}(@StringValue), zptString);
   {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
   {$ifndef minimal}List.GetLast.NeedRefreshNodeName := True; {$endif}
   {$ifndef minimal}List.GetLast.ExcludeFromBinary := True; {$endif}
  List.AddProperty({$IFNDEF MINIMAL}strByteValue,{$ENDIF}(@ByteValue), zptByte);
   {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
   {$ifndef minimal}List.GetLast.NeedRefreshNodeName := True; {$endif}
   {$ifndef minimal}List.GetLast.ExcludeFromBinary := True; {$endif}
end;

{$ifndef minimal}
function TDefineConstant.GetDisplayName: AnsiString;
begin
  Result := inherited GetDisplayName + ' ';
  case _Type.Kind of
    zctFloat: Result := Result + AnsiString(FormatFloat('###0.#',Value));
    zctInt: Result := Result + AnsiString(IntToStr(IntValue));
    zctString: Result := Result + '"' + StringValue + '"';
    zctByte: Result := Result + AnsiString(IntToStr(ByteValue));
  end;
end;
{$endif}

{ TDefineArray }

procedure TDefineArray.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}strDimensions,{$ENDIF}(@Dimensions), zptByte);
    {$ifndef minimal}List.GetLast.SetOptions(['One','Two','Three']);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}strSizeDim1,{$ENDIF}(@SizeDim1), zptInteger);
  List.AddProperty({$IFNDEF MINIMAL}strSizeDim2,{$ENDIF}(@SizeDim2), zptInteger);
  List.AddProperty({$IFNDEF MINIMAL}strSizeDim3,{$ENDIF}(@SizeDim3), zptInteger);
  List.AddProperty({$IFNDEF MINIMAL}strPersistent,{$ENDIF}(@Persistent), zptBoolean);
  List.AddProperty({$IFNDEF MINIMAL}strValues,{$ENDIF}(@Values), zptBinary);
end;

destructor TDefineArray.Destroy;
begin
  CleanUpManagedValues(_Type.Kind,Limit,AllocPtr);
  if (Data<>nil) and (not IsExternal) then
    FreeMem(Data);
  inherited;
end;

function TDefineArray.GetData: ZClasses.PFloat;
begin
  //Check if Array size has changed
  if (Limit<>CalcLimit) then
    AllocData;
  {$ifndef minimal}
  ZAssert(not (Persistent and (_Type.Kind in [zctString,zctModel,zctMat4,zctVec3,zctVec2,zctVec4])),'Persistent arrays of this datatype not supported');
  {$endif}
  if Persistent then
  begin
    if Values.Data=nil then
      AllocData;
    Result := ZClasses.PFloat(Values.Data)
  end
  else
  begin
    if Data=nil then
      AllocData;
    Result := ZClasses.PFloat(Data);
  end;
end;

function TDefineArray.GetElementSize: integer;
begin
  Result := GetZcTypeSize(Self._Type.Kind);
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
  Self.AllocType := Self._Type.Kind;

  if Self._Type.Kind in [zctString,zctClass] then
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
  if (not (TheType in [zctString,zctClass])) or (Count=0) then
    Exit;
  for I := 0 to Count - 1 do
  begin
    ManagedHeap_RemoveTarget(P);
    Inc(P);
  end;
end;

function TDefineArray.PopAndGetElement(Env : PExecutionEnvironment) : ZClasses.PFloat;
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
    Env.ScriptError('Array access outside range: ' + String(Self.Name) + ' ' + IntToStr(I1) + ' ' + IntToStr(I2) + ' ' + IntToStr(I3));
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
  Env.StackPushPointer(P);
end;

{ TExpStackFrame }

procedure TExpStackFrame.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}strSize,{$ENDIF}(@Size), zptInteger);
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
  List.AddProperty({$IFNDEF MINIMAL}strKind,{$ENDIF}(@Kind), zptByte);
  List.AddProperty({$IFNDEF MINIMAL}strIndex,{$ENDIF}(@Index), zptInteger);
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

{ TExpAccessGlobal }

procedure TExpAccessGlobal.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}strKind,{$ENDIF}@Kind, zptByte);
  List.AddProperty({$IFNDEF MINIMAL}strOffset,{$ENDIF}@Offset, zptInteger);
  List.AddProperty({$IFNDEF MINIMAL}strLib,{$ENDIF}@Lib, zptComponentRef);
end;

procedure TExpAccessGlobal.Execute(Env : PExecutionEnvironment);
var
  P : TExecutionEnvironment.PStackElement;
begin
  //Use pointer size to get all bits in 64-bit mode
  P := TExecutionEnvironment.PStackElement(NativeUInt(Lib.GlobalArea)+Cardinal(Self.Offset));
  case Kind of
    glLoad: Env.StackPushPointer(P^);
    glStore: Env.StackPopToPointer(P^);
    glGetAddress: Env.StackPushPointer(P);
  end;
end;

{$ifndef minimal}
function TExpAccessGlobal.ExpAsText : string;
begin
  if Kind=glLoad then
    Result := 'Load'
  else if Kind=glStore then
    Result := 'Store'
  else
    Result := 'GetAddress';
  Result :=  Result + ' ' + IntToStr(Self.Offset) +  ' (global)';
end;
{$endif}

{ TExpReturn }

procedure TExpReturn.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}strHasFrame,{$ENDIF}(@HasFrame), zptBoolean);
  List.AddProperty({$IFNDEF MINIMAL}strHasReturnValue,{$ENDIF}(@HasReturnValue), zptBoolean);
  List.AddProperty({$IFNDEF MINIMAL}strArguments,{$ENDIF}(@Arguments), zptInteger);
  List.AddProperty({$IFNDEF MINIMAL}strLib,{$ENDIF}(@Lib), zptComponentRef);
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

  if (Lib<>nil) and Lib.UseThreadLock then
    Lib.ReleaseLock
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
    Value := Self.GetProperty(Prop);
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
  List.AddProperty({$IFNDEF MINIMAL}strKind,{$ENDIF}(@Kind), zptByte);
end;

procedure TExpMisc.Execute(Env : PExecutionEnvironment);
var
  V : integer;
  P,ValueP : pointer;
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
        {$ifdef CALLSTACK}
        Env.CheckNilDeref(P);
        {$endif}
        V := PInteger(P)^;
        Env.StackPush(V);
      end;
    emPtrDeref1 :
      begin
        Env.StackPopToPointer(P);
        {$ifdef CALLSTACK}
        Env.CheckNilDeref(P);
        {$endif}
        V := PByte(P)^;
        Env.StackPush(V);
      end;
    emPtrDerefPointer :
      begin
        Env.StackPopToPointer(P);
        {$ifdef CALLSTACK}
        Env.CheckNilDeref(P);
        {$endif}
        Env.StackPushPointer(P^);
      end;
    emNotifyPropChanged :
      begin
        Env.StackPopTo(V);
        Env.StackPopToPointer(P);
        Env.StackPopToPointer(ValueP);
        TZComponent(P).GetProperties.GetById(V).NotifyWhenChanged(P,V,ValueP);
      end;
    emLoadNull :
      begin
        P := nil;
        Env.StackPushPointer(P);
      end;
    emNop : ;
    emBinaryNot :
      begin
        Env.StackPopTo(V);
        V := not V;
        Env.StackPush(V);
      end;
    emNot :
      begin
        Env.StackPopTo(V);
        if V=0 then
          V := 1
        else
          V := 0;
        Env.StackPush(V);
      end;
    emGetUserClass :
      begin
        //Convert TUserClassInstance to the instancedata for field access
        Env.StackPopToPointer(P);
        {$ifdef CALLSTACK}
        Env.CheckNilDeref(P);
        {$endif}
        Env.StackPushPointer( TUserClassInstance(P).InstanceData );
      end;
    emGetGlobalDataProp :
      begin
        Env.StackPopTo(V);
        Env.StackPopToPointer(P);
        P := TZComponent(P).GetPropertyPtr(TZComponent(P).GetProperties.GetById(V),0);
        Env.StackPushPointer(P);
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
  List.AddProperty({$IFNDEF MINIMAL}strSource,{$ENDIF}@Source, zptExpression);
    {$ifndef minimal}List.GetLast.ExpressionKind := ekiLibrary;{$endif}
  List.AddProperty({$IFNDEF MINIMAL}strUseThreadLock,{$ENDIF}@UseThreadLock, zptBoolean);
  List.AddProperty({$IFNDEF MINIMAL}strHasInitializer,{$ENDIF}@HasInitializer, zptBoolean);
   {$ifndef minimal}List.GetLast.HideInGui := True;{$endif}
  List.AddProperty({$IFNDEF MINIMAL}strGlobalAreaSize,{$ENDIF}@GlobalAreaSize, zptInteger);
   {$ifndef minimal}List.GetLast.HideInGui := True;{$endif}
   {$ifndef minimal}List.GetLast.ExcludeFromXml := True;{$endif}
  List.AddProperty({$IFNDEF MINIMAL}strManagedVariables,{$ENDIF}@ManagedVariables, zptBinary);
   {$ifndef minimal}List.GetLast.HideInGui := True;{$endif}
   {$ifndef minimal}List.GetLast.ExcludeFromXml := True;{$endif}
end;

procedure TZLibrary.AquireLock;
begin
  if Self.Lock=nil then
    Lock := Platform_CreateMutex;
  Platform_EnterMutex(Lock);
end;

procedure TZLibrary.ReleaseLock;
begin
  Platform_LeaveMutex(Lock);
end;

procedure TZLibrary.Update;
begin
  inherited;
  if Self.HasInitializer and (not Self.HasExecutedInitializer) then
  begin
    ZExpressions.RunCode(Source.Code);
    Self.HasExecutedInitializer := True;
  end;
end;

procedure TZLibrary.InitAfterPropsAreSet;
begin
  inherited;
  InitGlobalArea;
end;

procedure TZLibrary.RemoveManagedTargets;
var
  Offsets : PInteger;
  I,ManagedCount : integer;
  P : pointer;
begin
  if (Self.ManagedVariables.Size>0) and (GlobalArea<>nil) then
  begin
    //Remove targets for managed variables
    ManagedCount := Self.ManagedVariables.Size div 4;
    Offsets := PInteger(ManagedVariables.Data);
    for I := 0 to ManagedCount-1 do
    begin
      P := Pointer(IntPtr(Self.GlobalArea) + Offsets^);
      ManagedHeap_RemoveTarget( P );
      Inc(Offsets);
    end;
  end;
end;

procedure TZLibrary.InitGlobalArea;
var
  Offsets : PInteger;
  I,ManagedCount : integer;
  P : pointer;
begin
  if (Self.GlobalAreaSize>0) then
  begin
    ReAllocMem(Self.GlobalArea,Self.GlobalAreaSize);
    FillChar(Self.GlobalArea^,Self.GlobalAreaSize,0);

    if Self.ManagedVariables.Size>0 then
    begin
      //Add targets for managed fields
      ManagedCount := Self.ManagedVariables.Size div 4;
      Offsets := PInteger(ManagedVariables.Data);
      for I := 0 to ManagedCount-1 do
      begin
        P := Pointer(IntPtr(Self.GlobalArea) + Offsets^);
        ManagedHeap_AddTarget( P );
        Inc(Offsets);
      end;
    end;
  end;
end;

destructor TZLibrary.Destroy;
begin
  {$ifndef minimal}
  if Lock<>nil then
    Platform_FreeMutex(Lock);
  {$endif}
  RemoveManagedTargets;
  FreeMem(GlobalArea);
  inherited;
end;

{$ifndef minimal}
procedure TZLibrary.DesignerReset;
begin
  Self.HasExecutedInitializer := False;
  RemoveManagedTargets;
  InitGlobalArea;
  inherited;
end;

procedure TZLibrary.AddGlobalVar(const Typ: TZcDataType);
begin
  //Need to always increase 8 here instead of sizeof(pointer) to
  //allow generated binary to be compatible with both 32 and 64 bit runtime.
  if Typ.Kind in ManagedTypes then
  begin
    Inc(ManagedVariables.Size,4);
    ReallocMem(ManagedVariables.Data,ManagedVariables.Size);
    PInteger( pointer(IntPtr(ManagedVariables.Data)+ManagedVariables.Size-4) )^ := Self.GlobalAreaSize;
  end;
  Inc(Self.GlobalAreaSize,8);
end;
{$endif}

{ TExpUserFuncCall }

procedure TExpUserFuncCall.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}strLib,{$ENDIF}(@Lib), zptComponentRef);
  List.AddProperty({$IFNDEF MINIMAL}strIndex,{$ENDIF}(@Index), zptInteger);
end;

procedure TExpUserFuncCall.Execute(Env : PExecutionEnvironment);
begin
  if Lib.UseThreadLock then
    Lib.AquireLock;
  Env.StackPushPointer(Env.gCurrentPC);
  Env.gCurrentPC := Lib.Source.Code.GetPtrToItem(Index);
  Dec(Env.gCurrentPc);
end;

{ TExpConvert }

procedure TExpConvert.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}strKind,{$ENDIF}(@Kind), zptByte);
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
  List.AddProperty({$IFNDEF MINIMAL}strType,{$ENDIF}@_Type.Kind, zptByte);
    {$ifndef minimal}List.GetLast.SetOptions(['float','int','string','model','byte','mat4','vec2','vec3','vec4','xptr']);{$endif}
    {$ifndef minimal}List.GetLast.NeedRefreshNodeName:=True;{$endif}
end;

{$ifndef minimal}
function TDefineVariableBase.GetDisplayName: AnsiString;
var
  I : integer;
  P : TZProperty;
begin
  I := Ord(Self._Type.Kind);
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
  List.AddProperty({$ifndef minimal}strValue,{$ENDIF}(@Value), zptString);
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
  List.AddProperty({$IFNDEF MINIMAL}strModuleName,{$ENDIF}(@ModuleName), zptString);
    {$ifndef minimal}List.GetLast.NeedRefreshNodeName := True;{$endif}
    List.GetLast.IsManagedTarget := True;
  List.AddProperty({$IFNDEF MINIMAL}strCallingConvention,{$ENDIF}(@CallingConvention), zptByte);
    {$ifndef minimal}List.GetLast.SetOptions(['Stdcall','Cdecl']);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}strBeforeInitExp,{$ENDIF}(@BeforeInitExp), zptExpression);
  List.AddProperty({$IFNDEF MINIMAL}strSource,{$ENDIF}(@Source), zptExpression);
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
  List.AddProperty({$IFNDEF MINIMAL}strDefinitionsFile,{$ENDIF}(@DefinitionsFile), zptString);
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
    {$ifdef MSWINDOWS}
    Platform_FreeModule(Self.ModuleHandle);
    Self.ModuleHandle := 0;
    {$endif}
  end;
end;

destructor TZExternalLibrary.Destroy;
begin
  if Self.ModuleHandle<>0 then
    Platform_FreeModule(Self.ModuleHandle);
  inherited;
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
  List.AddProperty({$IFNDEF MINIMAL}strLib,{$ENDIF}(@Lib), zptComponentRef);
  List.AddProperty({$IFNDEF MINIMAL}strFuncName,{$ENDIF}(@FuncName), zptString);
  List.AddProperty({$IFNDEF MINIMAL}strArgCount,{$ENDIF}(@ArgCount), zptInteger);
  List.AddProperty({$IFNDEF MINIMAL}strReturnType,{$ENDIF}(@ReturnType), zptByte);
  List.AddProperty({$IFNDEF MINIMAL}strArgTypes,{$ENDIF}(@ArgTypes), zptString);
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
procedure DummyProc(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12 : NativeInt);
begin
end;

{$ifndef MSWINDOWS}
function mprotect(__addr:pointer;__len:cardinal;__prot:longint):longint; cdecl; external 'libc' name 'mprotect';
{$endif}

function GenerateTrampoline(const ArgCount : integer; ArgTypes : PAnsiChar; Proc : pointer) : pointer;
const
{$ifdef MSWINDOWS}
  IntRegCount = 4;
  FloatRegCount = 4;

  Int64Regs : array[0..IntRegCount-1] of array[0..3] of byte =
( ($49,$8B,$4A,0), //mov rcx,[r10+x]
  ($49,$8B,$52,0),
  ($4d,$8B,$42,0),
  ($4d,$8B,$4A,0)
);

  Int32Regs : array[0..IntRegCount-1] of array[0..3] of byte =
( ($41,$8B,$4A,0),  //mov ecx,[r10+x]
  ($41,$8B,$52,0),
  ($45,$8B,$42,0),
  ($45,$8B,$4A,0)
);

  Float32Regs : array[0..FloatRegCount-1] of array[0..5] of byte =
( ($66,$41,$0F,$6E,$42,0), //movd xmm0,[r10+x]
  ($66,$41,$0F,$6E,$4A,0),
  ($66,$41,$0F,$6E,$52,0),
  ($66,$41,$0F,$6E,$5A,0)
);

{$ELSE}
  //Non MS-abi: https://en.wikipedia.org/wiki/X86_calling_conventions#x86-64_calling_conventions
  IntRegCount = 6;
  FloatRegCount = 8;

  Int64Regs : array[0..IntRegCount-1] of array[0..3] of byte =
( ($49,$8B,$7A,0), //mov rdi,[r10+x]
  ($49,$8B,$72,0),
  ($49,$8B,$52,0),
  ($49,$8B,$4A,0),
  ($4D,$8B,$42,0),
  ($4D,$8B,$4A,0)
);

  Int32Regs : array[0..IntRegCount-1] of array[0..3] of byte =
( ($41,$8B,$7A,0),  //mov edi,[r10+x]
  ($41,$8B,$72,0),
  ($41,$8B,$52,0),
  ($41,$8B,$4A,0),
  ($45,$8B,$42,0),
  ($45,$8B,$4A,0)
);

  Float32Regs : array[0..FloatRegCount-1] of array[0..5] of byte =
( ($66,$41,$0F,$6E,$42,0), //movd xmm0,[r10+x]
  ($66,$41,$0F,$6E,$4A,0),
  ($66,$41,$0F,$6E,$52,0),
  ($66,$41,$0F,$6E,$5A,0),
  ($66,$41,$0F,$6E,$62,0),
  ($66,$41,$0F,$6E,$6A,0),
  ($66,$41,$0F,$6E,$72,0),
  ($66,$41,$0F,$6E,$7A,0)
);
{$ENDIF}

  Int32Stack1 : array[0..3] of byte = ($41,$8B,$42,0);  //mov eax,[r10+$10]
  Int32Stack2 : array[0..3] of byte = ($89,$44,$24,0);  //mov [rsp+$10],eax
  Int64Stack1 : array[0..3] of byte = ($49,$8B,$42,0);  //mov rax,[r10+$10]
  Int64Stack2 : array[0..4] of byte = ($48,$89,$44,$24,0);  //mov [rsp+$10],rax
var
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
  I,FloatI,IntI,Offs : integer;
  OldProtect : dword;
  CodeSize : integer;
  StackOffs : integer;
  UseStack : boolean;
begin

  {$ifdef MSWINDOWS}
  CodeSize := 64 + (6 * ArgCount);
  GetMem(Result,CodeSize);
  {$else}
  CodeSize := 512; //Use fixed size so we don't have to save the size for unmap call
  Result := fpmmap(nil,CodeSize,PROT_READ or PROT_WRITE or PROT_EXEC,MAP_PRIVATE or MAP_ANONYMOUS,-1,0);
  {$endif}

  P := Result;

  if ArgCount>0 then
    //Copy Args-ptr to R10
    {$ifdef MSWINDOWS}
    W([$49,$89,$ca]); //mov r10,rcx
    {$else}
    W([$49,$89,$fa]); //mov r10,rdi
    {$endif}

  Offs := 0; //Offset in Args
  {$ifdef MSWINDOWS}
  StackOffs := $28;
  {$else}
  StackOffs := 8;
  {$endif}

  {$ifndef MSWINDOWS}
  FloatI := 0; IntI := 0;
  {$endif}
  for I := 0 to ArgCount-1 do
  begin
    UseStack := False;

    {$ifdef MSWINDOWS}
    //MS-calling convention uses registers for 4 first parameters only, regardless of float or int.
    //Rest of the world uses 6 int + 8 float registers depending on argument types.
    FloatI := I;
    IntI := I;
    {$endif}

    case TZcDataTypeKind(Ord(ArgTypes[I])-65) of
      zctInt :
        begin
          if IntI<IntRegCount then
          begin
            W(Int32Regs[IntI]);
            P[-1] := Offs;
            {$ifndef MSWINDOWS}
            Inc(IntI);
            {$endif}
          end
          else
            UseStack := True;
        end;
      zctString,zctModel,zctXptr,zctMat4,zctVec2,zctVec3,zctVec4,zctArray :
        begin
          if IntI<IntRegCount then
          begin
            W(Int64Regs[IntI]);
            P[-1] := Offs;
            {$ifndef MSWINDOWS}
            Inc(IntI);
            {$endif}
          end
          else
            UseStack := True;
        end;
      zctFloat :
        begin
          if FloatI<FloatRegCount then
          begin
            W(Float32Regs[FloatI]);
            P[-1] := Offs;
            {$ifndef MSWINDOWS}
            Inc(FloatI);
            {$endif}
          end
          else
            UseStack := True;
        end;
    else
      Assert(False,'This argument type not yet supported on 64-bit:');
    end;

    if UseStack then
    begin
      //push on stack
      case GetZcTypeSize(TZcDataTypeKind(Ord(ArgTypes[I])-65)) of
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

    Inc(Offs, SizeOf(TExecutionEnvironment.TStackElement) );
  end;

  W([$49,$bb]); //mov r11,x
  PPointer(P)^ := Proc; Inc(P,8);  //x
  W([$49,$ff,$e3]); //jmp r11

  {$ifdef mswindows}
  Windows.VirtualProtect(Result,CodeSize,PAGE_EXECUTE_READWRITE,@OldProtect);
  {$else}
  mprotect(Result,CodeSize,PROT_READ or PROT_WRITE or PROT_EXEC);
  {$endif}
end;

procedure TExpExternalFuncCall.Execute(Env : PExecutionEnvironment);
type
  TFunc = function(Args : pointer) : NativeUInt;
  TFloatFunc = function(Args : pointer) : single;
var
  RetVal : NativeUInt;
begin
  if Self.Proc=nil then
  begin
    Self.Proc := Lib.LoadFunction(Self.FuncName);
    //Make sure there is enough stack space for calling a func with 8 params
    DummyProc(1,2,3,4,5,6,7,8,9,10,11,12);
    FreeMem(Self.Trampoline);
    Self.Trampoline := GenerateTrampoline(ArgCount,Self.ArgTypes,Self.Proc);
    {$ifndef minimal}
    Assert(Length(Self.ArgTypes)=ArgCount);
    {$endif}
  end;

  if Self.ReturnType.Kind=zctFloat then
  begin //Float returns in xmm0
    PSingle(@RetVal)^ := TFloatFunc(Self.Trampoline)( Env.StackPopAndGetPtr(ArgCount) );
  end
  else //other return types return in rax
    Retval := TFunc(Self.Trampoline)( Env.StackPopAndGetPtr(ArgCount) );

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
  {$ifdef MSWINDOWS}
  FreeMem(Trampoline);
  {$else}
  fpmunmap(Trampoline,512);  
  {$endif}
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
{$endif}

{$if defined(cpuaarch64)}  // ARM64

procedure DummyProc(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12 : NativeInt);
begin
end;

function mprotect(__addr:pointer;__len:cardinal;__prot:longint):longint; cdecl; external 'libc' name 'mprotect';

{$IFDEF MACOS}
procedure pthread_jit_write_protect_np(enabled : integer); cdecl; external 'pthread' name 'pthread_jit_write_protect_np';
procedure sys_icache_invalidate(start : pointer; len : nativeuint); cdecl; external 'libc' name 'sys_icache_invalidate';
{$ENDIF}

function GenerateTrampoline(const ArgCount : integer; ArgTypes : PAnsiChar; Proc : pointer) : pointer;
{
  https://docs.microsoft.com/en-us/cpp/build/arm64-windows-abi-conventions?view=msvc-160
  http://shell-storm.org/online/Online-Assembler-and-Disassembler/
  https://godbolt.org/
  https://developer.apple.com/documentation/apple_silicon/porting_just-in-time_compilers_to_apple_silicon?language=objc
}
const
  IntRegCount = 8;
  FloatRegCount = 8;

  PtrRegs : array[0..IntRegCount-1] of array[0..3] of byte =
( ($00,$01,$40,$f9), //ldr x0,[x8]
  ($01,$01,$40,$f9), //ldr x1,[x8]
  ($02,$01,$40,$f9), //ldr x2,[x8]
  ($03,$01,$40,$f9), //ldr x3,[x8]
  ($04,$01,$40,$f9), //ldr x4,[x8]
  ($05,$01,$40,$f9), //ldr x5,[x8]
  ($06,$01,$40,$f9), //ldr x6,[x8]
  ($07,$01,$40,$f9)  //ldr x7,[x8]
);

  Int32Regs : array[0..IntRegCount-1] of array[0..3] of byte =
( ($00,$01,$40,$b9),  //ldr w0,[x8]
  ($01,$01,$40,$b9),  //ldr w1,[x8]
  ($02,$01,$40,$b9),  //ldr w2,[x8]
  ($03,$01,$40,$b9),  //ldr w3,[x8]
  ($04,$01,$40,$b9),  //ldr w4,[x8]
  ($05,$01,$40,$b9),  //ldr w5,[x8]
  ($06,$01,$40,$b9),  //ldr w6,[x8]
  ($07,$01,$40,$b9)   //ldr w7,[x8]
);

  Float32Regs : array[0..FloatRegCount-1] of array[0..3] of byte =
( ($00,$01,$40,$bd), //ldr s0,[x8]
  ($01,$01,$40,$bd), //ldr s1,[x8]
  ($02,$01,$40,$bd), //ldr s2,[x8]
  ($03,$01,$40,$bd), //ldr s3,[x8]
  ($04,$01,$40,$bd), //ldr s4,[x8]
  ($05,$01,$40,$bd), //ldr s5,[x8]
  ($06,$01,$40,$bd), //ldr s6,[x8]
  ($07,$01,$40,$bd)  //ldr s7,[x8]
);

  Int32Stack : array[0..4-1] of array[0..7] of byte =
( ($09,$01,$40,$b9,$e9,$03,$00,$b9), //ldr w9,[x8]    str w9,[sp]
  ($09,$01,$40,$b9,$e9,$0b,$00,$b9), //ldr w9,[x8]    str w9,[sp,#8]
  ($09,$01,$40,$b9,$e9,$13,$00,$b9), //ldr w9,[x8]    str w9,[sp,#16]
  ($09,$01,$40,$b9,$e9,$1b,$00,$b9)  //ldr w9,[x8]    str w9,[sp,#24]
);

  PtrStack : array[0..4-1] of array[0..7] of byte =
( ($09,$01,$40,$f9,$e9,$03,$00,$f9), //ldr x9,[x8]    str x9,[sp]
  ($09,$01,$40,$f9,$e9,$07,$00,$f9), //ldr x9,[x8]    str x9,[sp,#8]
  ($09,$01,$40,$f9,$e9,$0b,$00,$f9), //ldr x9,[x8]    str x9,[sp,#16]
  ($09,$01,$40,$f9,$e9,$0f,$00,$f9)  //ldr x9,[x8]    str x9,[sp,#24]
);

var
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
  I,FloatI,IntI,StackI,Offs : integer;
  OldProtect : dword;
  CodeSize : integer;
  UseStack : boolean;
const
  MAP_JIT	=	$0800;
begin
  CodeSize := 512; //Use fixed size so we don't have to save the size for unmap call
  Result := fpmmap(nil,CodeSize,PROT_READ or PROT_WRITE or PROT_EXEC,MAP_PRIVATE or MAP_ANONYMOUS{$IFDEF MACOS}or MAP_JIT{$ENDIF},-1,0);
  {$IFDEF MACOS}
  pthread_jit_write_protect_np(0);
  {$ENDIF}

  P := Result;

  if ArgCount>0 then
    //Copy Args-ptr to x8
    W([$e8,$03,$00,$aa]); //mov x8,x0

  Offs := 0; //Offset in Args

  FloatI := 0; IntI := 0; StackI := 0;
  for I := 0 to ArgCount-1 do
  begin
    UseStack := False;

    case TZcDataTypeKind(Ord(ArgTypes[I])-65) of
      zctInt :
        begin
          if IntI<IntRegCount then
          begin
            W(Int32Regs[IntI]);
            // modify the offset so it becomes "ldr w0, [x8,#offset]"
            P[-3] := P[-3] or Offs;
            Inc(IntI);
          end
          else
            UseStack := True;
        end;
      zctString,zctModel,zctXptr,zctMat4,zctVec2,zctVec3,zctVec4,zctArray :
        begin
          if IntI<IntRegCount then
          begin
            W(PtrRegs[IntI]);
            P[-3] := P[-3] or (Offs shr 1);
            Inc(IntI);
          end
          else
            UseStack := True;
        end;
      zctFloat :
        begin
          if FloatI<FloatRegCount then
          begin
            W(Float32Regs[FloatI]);
            P[-3] := P[-3] or Offs;
            Inc(FloatI);
          end
          else
            UseStack := True;
        end;
    else
      Assert(False,'This argument type not yet supported on 64-bit:');
    end;

    if UseStack then
    begin
      //push on stack
      case GetZcTypeSize(TZcDataTypeKind(Ord(ArgTypes[I])-65)) of
        4 :
          begin
            Assert(StackI<High(Int32Stack));
            W(Int32Stack[StackI]);
            P[-7] := P[-7] or Offs;
          end;
        8 :
          begin
            Assert(StackI<High(PtrStack));
            W(PtrStack[StackI]);
            P[-7] := P[-7] or (Offs shr 1);
          end;
      else
        Assert(False,'This argument type not yet supported on 64-bit');
      end;
      Inc(StackI);
    end;

    Inc(Offs, SizeOf(TExecutionEnvironment.TStackElement) );
  end;

  W([$68,$00,$00,$10]); //adr x8,12  (PC relative offset to function pointer. 12 is the number of bytes in these three lines.)
  W([$08,$01,$40,$F9]); //ldr x8,[x8]
  W([$00,$01,$1f,$d6]); //br x8

  // Store function pointer
  PPointer(P)^ := Proc; Inc(P,8);

  {$IFDEF MACOS}
  pthread_jit_write_protect_np(1);
  sys_icache_invalidate(Result,CodeSize);
  {$ENDIF}
  mprotect(Result,CodeSize,PROT_READ or PROT_WRITE or PROT_EXEC);
end;

procedure TExpExternalFuncCall.Execute(Env : PExecutionEnvironment);
type
  TFunc = function(Args : pointer) : NativeUInt;
  TFloatFunc = function(Args : pointer) : single;
var
  RetVal : NativeUInt;
begin
  if Self.Proc=nil then
  begin
    Self.Proc := Lib.LoadFunction(Self.FuncName);
    //Make sure there is enough stack space for calling a func with many params
    DummyProc(1,2,3,4,5,6,7,8,9,10,11,12);
    FreeMem(Self.Trampoline);
    Self.Trampoline := GenerateTrampoline(ArgCount,Self.ArgTypes,Self.Proc);
    {$ifndef minimal}
    Assert(Length(Self.ArgTypes)=ArgCount);
    {$endif}
  end;

  if Self.ReturnType.Kind=zctFloat then
  begin //Float returns in different register than other types
    PSingle(@RetVal)^ := TFloatFunc(Self.Trampoline)( Env.StackPopAndGetPtr(ArgCount) );
  end
  else
    Retval := TFunc(Self.Trampoline)( Env.StackPopAndGetPtr(ArgCount) );

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
  fpmunmap(Trampoline,512);
end;

{$endif}

{ TExpLoadComponent }

procedure TExpLoadComponent.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}strComponent,{$ENDIF}(@Component), zptComponentRef);
end;

procedure TExpLoadComponent.Execute(Env : PExecutionEnvironment);
begin
  Env.StackPushPointer(Self.Component);
end;

{ TExpLoadPropOffset }

procedure TExpLoadPropOffset.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}strPropId,{$ENDIF}(@PropId), zptInteger);
end;

procedure TExpLoadPropOffset.Execute(Env : PExecutionEnvironment);
var
  C : TZComponent;
begin
  if not IsInit then
  begin
    Env.StackPopToPointer(C);
    {$ifdef CALLSTACK}
    Env.CheckNilDeref(C);
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
  List.AddProperty({$IFNDEF MINIMAL}strDefinedIndex,{$ENDIF}(@DefinedIndex), zptInteger);
  {$ifndef minimal}
  List.AddProperty({$IFNDEF MINIMAL}strDefinedName,{$ENDIF}(@DefinedName), zptString);
    List.SetDesignerProperty;
  List.AddProperty({$IFNDEF MINIMAL}strComponentRef,{$ENDIF}@ComponentRef, zptComponentRef);
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
  if (Self.DefinedIndex>=M.Definitions.Count) or
    (not SameText(String(TZComponent(M.Definitions[Self.DefinedIndex]).Name),String(DefinedName))) then
  begin
    raise TModelDefinedException.Create('Defined var mismatch "' + DefinedName + '" in model "' + String(M.Name) + '" must be at position ' + IntToStr(Self.DefinedIndex) + ' in Definitions-list.');
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
  List.AddProperty({$IFNDEF MINIMAL}strInvokeClassId,{$ENDIF}(@InvokeClassId), zptInteger);
  List.AddProperty({$IFNDEF MINIMAL}strInvokeArgCount,{$ENDIF}(@InvokeArgCount), zptInteger);
  List.AddProperty({$IFNDEF MINIMAL}strInvokedItemList,{$ENDIF}(@InvokedItemList), zptComponentList);
    List.GetLast.NeverPersist := True;
  List.AddProperty({$IFNDEF MINIMAL}strIsValue,{$ENDIF}(@IsValue), zptBoolean);
end;

procedure TExpInvokeComponent.Execute(Env : PExecutionEnvironment);
var
  Ci : TZComponentInfo;
  I,PropId : integer;
  RawValue : nativeint;
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
    {$if defined(CPUX64) or defined(cpuaarch64)}
    Env.StackPopToPointer(RawValue);
    {$else}
    Env.StackPopTo(RawValue);
    {$endif}
    Prop := InvokeC.GetProperties.GetById(PropId);
    case Prop.PropertyType of
      zptFloat: V.FloatValue := PFloat(@RawValue)^;
      zptInteger: V.IntegerValue := RawValue;
      zptByte: V.ByteValue := RawValue;
      zptBoolean: V.BooleanValue := ByteBool(RawValue);
      zptComponentRef : V.ComponentValue := TZComponent(RawValue);
      zptString : V.StringValue := PAnsiChar(RawValue);
    {$ifndef minimal}
    else
      Env.ScriptError(ClassName + ' invalid datatype for argument');
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

{ TExpInitArray }

procedure TExpInitArray.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}strDimensions,{$ENDIF}(@Dimensions), zptByte);
  List.AddProperty({$IFNDEF MINIMAL}strType,{$ENDIF}(@_Type), zptByte);
  List.AddProperty({$IFNDEF MINIMAL}strSize1,{$ENDIF}(@Size1), zptInteger);
  List.AddProperty({$IFNDEF MINIMAL}strSize2,{$ENDIF}(@Size2), zptInteger);
  List.AddProperty({$IFNDEF MINIMAL}strSize3,{$ENDIF}(@Size3), zptInteger);
end;

procedure TExpInitArray.Execute(Env : PExecutionEnvironment);
var
  A : TDefineArray;
begin
  //Use pointer size to get all bits in 64-bit mode
  A := TDefineArray.Create(nil);
  ManagedHeap_AddValueObject(A);
  A.Dimensions := Self.Dimensions;
  A._Type.Kind := Self._Type;
  A.SizeDim1 := Self.Size1;
  A.SizeDim2 := Self.Size2;
  A.SizeDim3 := Self.Size3;
  Env.StackPushPointer(A);
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

{ TExpIDEFuncCall }

{$ifndef minimal}
procedure TExpIDEFuncCall.Execute(Env : PExecutionEnvironment);
var
  P1,P2,P3 : pointer;
  C : TZComponent;
  Ci : TZComponentInfo;
  Prop : TZProperty;
  Value : TZPropertyValue;
  V : single;
  I : integer;
  Stream : TMemoryStream;
  TmpS : ansistring;
  Dest : PAnsiChar;
begin
  case Kind of
    fcFindComponent :
      begin
        Env.StackPopToPointer(P1);
        P2 := ZApp.Symtab.Lookup(String(PAnsiChar(P1)));
        Env.StackPushPointer(P2);
      end;
    fcCreateComponent :
      begin
        Env.StackPopToPointer(P1); //compname
        Env.StackPopToPointer(P2); //proplistname
        Env.StackPopToPointer(P3); //owner

        Ci := ZClasses.ComponentManager.GetInfoFromName(String(PAnsiChar(P1)));

        if (P2=nil) or (P3=nil) then
        begin
          C := Ci.ZClass.Create( nil );
          //No owner, add to GC
          ZClasses.ManagedHeap_AddValueObject(C);
        end else
        begin
          Prop := TZComponent(P3).GetProperties.GetByName( String(PAnsiChar(P2)) );
          if (Prop=nil) or (Prop.PropertyType<>zptComponentList) then
            Env.ScriptError('CreateComponent called with invalid proplistname: ' + String(PAnsiChar(P2)));
          C := Ci.ZClass.Create( TZComponent(P3).GetProperty(Prop).ComponentListValue );
          C.ZApp.HasScriptCreatedComponents := True;
        end;

        Env.StackPushPointer(C);
        ZLog.GetLog('Zc').Write('Creating component: ' + Ci.ZClassName,lleNormal);
      end;
    fcSetNumericProperty :
      begin
        //XSIF
        V := Env.StackPopFloat;  //value
        Env.StackPopTo(I); //index
        Env.StackPopToPointer(P2); //propname
        Env.StackPopToPointer(P1); //comp
        Prop := TZComponent(P1).GetProperties.GetByName( String(PAnsiChar(P2)) );
        if Prop=nil then
          Env.ScriptError('SetNumericProperty called with invalid propname: ' + String(PAnsiChar(P2)));
        Value := TZComponent(P1).GetProperty(Prop); //Must read prop first, when modifying rects
        case Prop.PropertyType of
          zptFloat,zptScalar: Value.FloatValue := V;
          zptRectf: Value.RectfValue.Area[I] := V;
          zptColorf: Value.ColorfValue.V[I] := V;
          zptInteger: Value.IntegerValue := Round(V);
          zptVector3f: Value.Vector3fValue[I] := V;
          zptByte: Value.ByteValue := Round(V);
          zptBoolean: Value.BooleanValue := ByteBool(Round(V));
        else
          Env.ScriptError('SetNumericProperty called with prop of unsupported type: ' + String(PAnsiChar(P2)));
        end;
        TZComponent(P1).SetProperty(Prop,Value);
      end;
    fcSetStringProperty :
      begin
        //XSS
        Env.StackPopToPointer(P3); //value
        Env.StackPopToPointer(P2); //propname
        Env.StackPopToPointer(P1); //comp
        Prop := TZComponent(P1).GetProperties.GetByName( String(PAnsiChar(P2)) );
        if Prop=nil then
          Env.ScriptError('SetStringProperty called with invalid propname: ' + String(PAnsiChar(P2)));
        case Prop.PropertyType of
          zptString : Value.StringValue := AnsiString(PAnsiChar(P3));
          zptExpression : Value.ExpressionValue.Source := String(PAnsiChar(P3));
        else
          Env.ScriptError('SetStringProperty called with prop of unsupported type: ' + String(PAnsiChar(P2)));
        end;
        if (Prop.Name='Name') then
        begin
          if Assigned(ZApp.SymTab.Lookup(String(PAnsiChar(P3)))) then
            Env.ScriptError('SetStringProperty tried to set duplicate name: ' + String(PAnsiChar(P3)));
          ZApp.SymTab.Add(String(PAnsiChar(P3)),TZComponent(P1));
        end;
        TZComponent(P1).SetProperty(Prop,Value);
        //If we set an expression, then compile this now so it will execute in same run.
        if Prop.PropertyType=zptExpression then
          Self.ZApp.CompileProperty(TZComponent(P1),TZComponent(P1).GetProperty(Prop),Prop);
      end;
    fcSetObjectProperty :
      begin
        Env.StackPopToPointer(P3); //value
        Env.StackPopToPointer(P2); //propname
        Env.StackPopToPointer(P1); //comp
        Prop := TZComponent(P1).GetProperties.GetByName( String(PAnsiChar(P2)) );
        if Prop=nil then
          Env.ScriptError('SetObjectProperty called with invalid propname: ' + String(PAnsiChar(P2)));
        case Prop.PropertyType of
          zptComponentRef : Value.ComponentValue := P3;
        else
          Env.ScriptError('SetObjectProperty called with prop of unsupported type: ' + String(PAnsiChar(P2)));
        end;
        TZComponent(P1).SetProperty(Prop,Value);
      end;
    fcSaveComponentToTextFile:
      begin
        Env.StackPopToPointer(P2); //filename
        Env.StackPopToPointer(P1); //comp
        Stream := ComponentManager.SaveXmlToStream(TZComponent(P1)) as TMemoryStream;
        Stream.SaveToFile( String(PAnsiChar(P2)) );
        Stream.Free;
      end;
    fcGetStringProperty :
      begin
        Env.StackPopToPointer(P2); //propname
        Env.StackPopToPointer(P1); //comp
        Prop := TZComponent(P1).GetProperties.GetByName( String(PAnsiChar(P2)) );
        if Prop=nil then
          Env.ScriptError('GetStringProperty called with invalid propname: ' + String(PAnsiChar(P2)));
        Value := TZComponent(P1).GetProperty(Prop);
        case Prop.PropertyType of
          zptString : TmpS := Value.StringValue;
          zptExpression : TmpS := AnsiString(Value.ExpressionValue.Source);
        else
          Env.ScriptError('GetStringProperty called with prop of unsupported type: ' + String(PAnsiChar(P2)));
        end;

        Dest := ManagedHeap_Alloc(Length(TmpS)+1);
        ZStrCopy(Dest,PAnsiChar(TmpS));

        Env.StackPushPointer(Dest);
      end;
  end;
end;
{$endif}

{ TExpGetRawMemElement }

procedure TExpGetRawMemElement.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}strType,{$ENDIF}(@_Type), zptByte);
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
  List.AddProperty({$IFNDEF MINIMAL}strKind,{$ENDIF}(@Kind), zptByte);
  List.AddProperty({$IFNDEF MINIMAL}strType,{$ENDIF}(@_Type), zptByte);
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

{ TExpSwitchTable }

procedure TExpSwitchTable.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}strLowBound,{$ENDIF}@LowBound, zptInteger);
  List.AddProperty({$IFNDEF MINIMAL}strHighBound,{$ENDIF}@HighBound, zptInteger);
  List.AddProperty({$IFNDEF MINIMAL}strDefaultOrExit,{$ENDIF}@DefaultOrExit, zptInteger);
  List.AddProperty({$IFNDEF MINIMAL}strJumps,{$ENDIF}@Jumps, zptBinary);
end;

procedure TExpSwitchTable.Execute(Env: PExecutionEnvironment);
var
  V,Destination : integer;
begin
  Env.StackPopTo(V);
  if (V<LowBound) or (V>HighBound) then
    Destination := Self.DefaultOrExit
  else
  begin
    Dec(V,LowBound);
    Destination := PIntegerArray(Self.Jumps.Data)^[V];
  end;
  Inc(Env.gCurrentPc,Destination);
end;

{ TExpNewClassInstance }

procedure TExpNewClassInstance.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}strTheClass,{$ENDIF}@TheClass, zptComponentRef);
end;

procedure TExpNewClassInstance.Execute(Env: PExecutionEnvironment);
var
  P : TUserClassInstance;
  Cls : TUserClass;
begin
  Cls := Self.TheClass;

  P := TUserClassInstance.Create(Cls);
  ManagedHeap_AddValueObject(P);
  Env.StackPushPointer(P);

  //todo: call initializer not here but generate code in constructor instead
  if Cls.InitializerIndex<>-1 then
  begin //Call initializer
    Env.StackPushPointer(P); //push "this" as argument to initializer
    Env.StackPushPointer(Env.gCurrentPC);
    Env.gCurrentPC := Cls.DefinedInLib.Source.Code.GetPtrToItem(Cls.InitializerIndex);
    Dec(Env.gCurrentPc);
  end;
end;

{ TUserClassInstance }

constructor TUserClassInstance.Create(TheClass: TUserClass);
var
  I : integer;
  P : pointer;
  Offsets : PInteger;
  Mp : PPointer;
begin
  Self.TheClass := TheClass;

  GetMem(Self.InstanceData,TheClass.SizeInBytes);
  FillChar(Self.InstanceData^,TheClass.SizeInBytes,0);

  ManagedCount := (TheClass.ManagedFields.Size div 4);
  if ManagedCount>0 then
  begin
    //Copy pointers to owned memory to avoid dangling pointer to UserClass
    GetMem(ManagedCleanup,ManagedCount*SizeOf(Pointer));

    //Add targets for managed fields
    Offsets := PInteger(TheClass.ManagedFields.Data);
    Mp := ManagedCleanup;
    for I := 0 to ManagedCount-1 do
    begin
      P := Pointer(IntPtr(Self.InstanceData) + Offsets^);
      ManagedHeap_AddTarget( P );
      Mp^ := P;
      Inc(Mp);
      Inc(Offsets);
    end;
  end;
end;

destructor TUserClassInstance.Destroy;
var
  I : integer;
  Mp : PPointer;
begin
  //Remove targets for managed fields
  if Self.ManagedCount>0 then
  begin
    Mp := ManagedCleanup;
    for I := 0 to Self.ManagedCount-1 do
    begin
      ManagedHeap_RemoveTarget( Mp^ );
      Inc(Mp);
    end;
    FreeMem(ManagedCleanup);
  end;

  FreeMem(InstanceData);
  inherited;
end;

{ TUserClass }

procedure TUserClass.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}strSizeInBytes,{$ENDIF}@SizeInBytes, zptInteger);
  List.AddProperty({$IFNDEF MINIMAL}strManagedFields,{$ENDIF}@ManagedFields, zptBinary);
  List.AddProperty({$IFNDEF MINIMAL}strBaseClass,{$ENDIF}@BaseClass, zptComponentRef);
  List.AddProperty({$IFNDEF MINIMAL}strVmt,{$ENDIF}@Vmt, zptBinary);
  List.AddProperty({$IFNDEF MINIMAL}strDefinedInLib,{$ENDIF}@DefinedInLib, zptComponentRef);
  List.AddProperty({$IFNDEF MINIMAL}strInitializerIndex,{$ENDIF}@InitializerIndex, zptInteger);
end;

{ TExpVirtualFuncCall }

procedure TExpVirtualFuncCall.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}strVmtIndex,{$ENDIF}@VmtIndex, zptInteger);
end;

procedure TExpVirtualFuncCall.Execute(Env: PExecutionEnvironment);

  procedure InCheck(C : TUserClass);
  var
    Pc : integer;
  begin
    Pc := PIntegerArray(C.Vmt.Data)^[ Self.VmtIndex ];
    if Pc<>-1 then
    begin
      Env.gCurrentPC := C.DefinedInLib.Source.Code.GetPtrToItem(Pc-1);
      Exit;
    end;
    {$ifdef debug}
    Assert(Assigned(C.BaseClass),'virtual method error');
    {$endif}
    InCheck(C.BaseClass);
  end;

var
  P : TUserClassInstance;
begin
  Env.StackPopToPointer(P);
  Env.StackPushPointer(Env.gCurrentPC);
  InCheck(P.TheClass);
end;

initialization

  ZClasses.Register(TZExpression,ZExpressionClassId);
    {$ifndef minimal}ComponentManager.LastAdded.ImageIndex:=2;{$endif}
  ZClasses.Register(TZLibrary,ZLibraryClassId);
    {$ifndef minimal}ComponentManager.LastAdded.ImageIndex:=36;{$endif}
  ZClasses.Register(TDefineVariable,DefineVariableClassId);
    {$ifndef minimal}ComponentManager.LastAdded.ImageIndex:=8;{$endif}
    {$ifndef minimal}ComponentManager.LastAdded.ZClassName := 'Variable';{$endif}
    {$ifndef minimal}ComponentManager.LastAdded.AutoName := True;{$endif}
  ZClasses.Register(TDefineConstant,DefineConstantClassId);
    {$ifndef minimal}ComponentManager.LastAdded.ImageIndex:=29;{$endif}
    {$ifndef minimal}ComponentManager.LastAdded.ZClassName := 'Constant';{$endif}
  ZClasses.Register(TDefineArray,DefineArrayClassId);
    {$ifndef minimal}ComponentManager.LastAdded.ImageIndex:=22;{$endif}
    {$ifndef minimal}ComponentManager.LastAdded.ZClassName := 'Array';{$endif}
    {$ifndef minimal}ComponentManager.LastAdded.AutoName := True;{$endif}
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
  ZClasses.Register(TExpInitArray,ExpInitLocalArrayClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NoUserCreate:=True;{$endif}
  ZClasses.Register(TExpMat4FuncCall,ExpMat4FuncCallClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NoUserCreate:=True;{$endif}
  ZClasses.Register(TExpGetRawMemElement,ExpGetRawMemElementClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NoUserCreate:=True;{$endif}
  ZClasses.Register(TExpArrayUtil,ExpArrayUtilClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NoUserCreate:=True;{$endif}
  ZClasses.Register(TExpSwitchTable,ExpSwitchTableClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NoUserCreate:=True;{$endif}
  ZClasses.Register(TExpAccessGlobal,ExpAccessGlobalClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NoUserCreate:=True;{$endif}

  ZClasses.Register(TUserClass,UserClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NoUserCreate:=True;{$endif}
  ZClasses.Register(TExpNewClassInstance,ExpNewClassInstanceId);
    {$ifndef minimal}ComponentManager.LastAdded.NoUserCreate:=True;{$endif}
  ZClasses.Register(TExpVirtualFuncCall,ExpVirtualFuncCallClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NoUserCreate:=True;{$endif}

  {$ifndef minimal}
  ZClasses.Register(TExpIDEFuncCall,ExpIDEFuncCallClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NoUserCreate:=True;{$endif}
  {$endif}

end.
