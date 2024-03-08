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

//Main classes and component model

unit ZClasses;

{$include zzdc_globalopt.inc}

interface

{$ifndef minimal}
uses uSymTab,Contnrs,Classes,Generics.Collections;
{$endif}

resourcestring
  strName = 'Name';
  strComment = 'Comment';
  strDesignDisable = 'DesignDisable';
  strObjId = 'ObjId';
  strChildren = 'Children';
  strProducers = 'Producers';
  strDefinitions = 'Definitions';
  strOnStart = 'OnStart';
  strOnUpdate = 'OnUpdate';
  strOnRender = 'OnRender';
  strOnLeave = 'OnLeave';
  strExpression = 'Expression';

type
  //Baseclasses for all central concepts in ZGE

  //List with unique ClassIDs
  TZClassIds = (
 LogicalGroupClassId,ZBitmapClassId,
 BitmapRectClassId,BitmapZoomRotateClassId,BitmapExpressionClassId,BitmapFromFileClassId,BitmapBlurClassId,
 BitmapLoadClassId,BitmapCombineClassId,BitmapCellsClassId,BitmapDistortClassId,BitmapPixelsClassId,
 BitmapConvolutionClassId,BitmapNoiseClassId,
 MeshClassId,ModelClassId,MaterialClassId,MaterialTextureClassId,SpawnModelClassId,RemoveModelClassId,
 MeshBoxClassId,MeshSphereClassId,MeshNoiseClassId,MeshExpressionClassId,
 MeshCombineClassId,MeshLoadClassId,MeshTransformClassId,MeshLoopClassId,
 RemoveAllModelsClassId,
 MeshImplicitClassId,ImplicitPrimitiveClassId,ImplicitExpressionClassId,ImplicitCombineClassId,
 ImplicitWarpClassId,MeshImportClassId,
 FontClassId,ModelStateClassId,SetModelStateClassId,
 AnimatorGroupClassId,AnimatorSimpleClassId,MouseModelControllerClassId,
 StartAnimatorClassId, {CurvePointClassId, CurveClassId, AnimatorCurveClassId,}
 UseMaterialClassId,RenderMeshClassId,RenderTransformClassId,RenderSpriteClassId,
 RenderBeamsClassId,RenderTransformGroupClassId,RenderTextClassId,RenderSetColorClassId,RenderNetClassId,
 RenderParticlesClassId,ShaderClassId,ShaderVariableClassId,
 RenderTargetClassId,SetRenderTargetClassId,LightClassId,
 RepeatClassId,ConditionClassId,KeyPressClassId,RefreshContentClassId,ZTimerClassId,WebOpenClassId,
 ApplicationClassId,AppStateClassId,SetAppStateClassId,CallComponentClassId,CameraClassId,
 ZExpressionClassId,ExpConstantFloatClassId,ExpConstantIntClassId,
 ExpOpBinaryFloatClassId,ExpOpBinaryIntClassId,
 ExpJumpClassId,DefineVariableClassId,ExpFuncCallClassId,ExpExternalFuncCallClassId,
 ExpArrayWriteClassId,ExpStackFrameClassId,ExpAccessLocalClassId,
 ExpReturnClassId,ExpMiscClassId,ExpUserFuncCallClassId,ExpConvertClassId,
 ExpAssign4ClassId,ExpAssign1ClassId,ExpAssignPointerClassId,ExpStringConstantClassId,ExpStringConCatClassId,
 ExpPointerFuncCallClassId,ExpLoadComponentClassId,ExpLoadPropOffsetClassId,ExpLoadModelDefinedClassId,ExpAddToPointerClassId,
 ExpInvokeComponentClassId,ExpInitLocalArrayClassId,ExpMat4FuncCallClassId,ExpGetRawMemElementClassId,
 ExpArrayUtilClassId,ExpSwitchTableClassId,ExpAccessGlobalClassId,
 UserClassId,ExpNewClassInstanceId,ExpVirtualFuncCallClassId,
 DefineConstantClassId,DefineArrayClassId,ZLibraryClassId,ExternalLibraryClassId,
 DefineCollisionClassId,
 SoundClassId,PlaySoundClassId,AudioMixerClassId,
 MusicClassId,MusicControlClassId,
 SampleClassId,SampleExpressionClassId,SampleImportClassId,
 SteeringControllerClassId,SteeringBehaviourClassId,
 ZFileClassId,FileActionClassId,FileMoveDataClassId,
 ThreadClassId,SpriteSheetClassId,TileSetClassId,RenderTileClassId
 {$ifndef minimal}
 ,ExpIDEFuncCallClassId
 ,AnyComponentClassId  //Used by the compiler to parse "Component" datatype
 {$endif}
);

  TZComponent = class;
  TZComponentClass = class of TZComponent;
  TZProperty = class;

  //Datatyp som  zptString-properties ska deklareras som i components (se app.caption)
  TPropString = PAnsiChar;
  PPropString = ^TPropString;

  PObjectArray = ^TObjectArray;
  TObjectArray = array[0..100000] of TObject;

  PPAnsiChar = ^PAnsiChar;

  TBytes = array[0..100000] of byte;
  PBytes = ^TBytes;
  TWords = array[0..100000] of word;
  PWords = ^TWords;
//  PByte = ^byte;

  PBoolean = ^ByteBool;
  PFloat = ^single;
  TFloatArray = array[0..10000] of single;
  PFloatArray = ^TFloatArray;
  {$ifndef minimal}
  PString = ^string;
  {$endif}

  PPointer = ^pointer;

  PZVector2f = ^TZVector2f;
  TZVector2f = array[0..1] of single;
  PZVector2Array = ^TZVector2Array;
  TZVector2Array = packed array[0..0] of TZVector2f;

  PZVector3f = ^TZVector3f;
  TZVector3f = array[0..2] of single;
  PZVector4f = ^TZVector4f;
  TZVector4f = array[0..3] of single;

  TZMatrix4f = array[0..3] of TZVector4f;
  PZMatrix4f = ^TZMatrix4f;

  PZVector3Array = ^TZVector3Array;
  TZVector3Array = packed array[0..100000] of TZVector3f;

  TZPointf =
    record
      X,Y : single;
    end;

  TZPointi =
    record
      X,Y : integer;
    end;

  PColorf = ^TZColorf;
  TZColorf =
    packed record
      case integer of
        0 : (V : TZVector4f);
        1 : (R, G, B, A: single);
    end;

  PRectf = ^TZRectf;
  TZRectf =
    record
      case integer of
        0 : (Area : TZVector4f);
        1 : (Left, Top, Right, Bottom: single);
        2 : (TopLeft, BottomRight: TZPointf);
    end;

  TZBox3D =
    record //3D-box defined as Min/Max for each axis
      Min : TZVector3f;
      Max : TZVector3f;
    end;

  //Oriented bounding box 2D collision info
  //http://www.gamedev.net/community/forums/topic.asp?topic_id=364789
  TOBB_2D =
    record
      C : TZVector2f;                 //Center
      U : array[0..1] of TZVector2f;  //X and Y axis
      E : array[0..1] of single;      //Extents
    end;

  TZArrayList = class
  private
    List : PObjectArray;
    Capacity : integer;
    FCount: integer;
    procedure Grow;
    function GetItem(Index: Integer): TObject;
    procedure SetItem(Index: Integer; const Value: TObject);
    {$ifdef debug}
    procedure CheckValidIndex(Index: Integer);
    {$endif}
  public
    ReferenceOnly : boolean;
    constructor CreateReferenced;
    destructor Destroy; override;
    procedure Add(Item : TObject);
    procedure RemoveAt(Index : integer);
    function IndexOf(Item: TObject): Integer;
    function Last : TObject;
    procedure Remove(Item : TObject);
    procedure SwapRemoveAt(Index : integer);
    procedure SwapRemove(Item: TObject);
    procedure Clear;
    property Items[Index: Integer]: TObject read GetItem write SetItem; default;
    property Count : integer read FCount;
    procedure Push(Item : TObject);
    function Pop : TObject;
    function GetPtrToItem(Index: Integer): pointer;
    procedure Swap(Index1,Index2 : integer);
  end;

  //Anv�nds som property i komponenter f�r att h�lla children-komponenter
  //samt event-tr�d med command-komponenter
  //Obs, �ger sina componenter trots att TZArrayList ReferenceOnly=true
  TZComponentList = class(TZArrayList)
  public
    Owner : TZComponent;
    IsChanged : boolean;
    constructor Create; overload;
    constructor Create(OwnerC : TZComponent); overload;
    destructor Destroy; override;
    procedure Clear;
    procedure AddComponent(Component: TZComponent);
    procedure RemoveComponent(Component: TZComponent);
    function GetComponent(Index : integer) : TZComponent;
    function ComponentCount : integer;
    procedure Update;
    procedure Change;
    procedure ExecuteCommands;
    {$ifndef minimal}
    procedure DesignerReset;
    procedure InsertComponent(Component: TZComponent; Index : integer);
    {$endif}
  end;


  //Expression �r en egen propertytyp
  //I designl�ge s� anv�nds b�de Source-str�ng och lista med kompilerad kod
  //I minimal endast kompilerad kod (i n�stlad komponentlista)
  PZExpressionPropValue = ^TZExpressionPropValue;
  TZExpressionPropValue = record
    {$ifndef minimal}
    Source : string;            //Expression source
    {$endif}
    Code : TZComponentList;     //Expression byte code
  end;

  //Datatypes in Zc-script
  TZcDataTypeKind = (
    zctFloat, //Public types (selectable in Variable and Array components)
    zctInt,
    zctString,
    zctModel,
    zctByte,
    zctMat4,
    zctVec2,
    zctVec3,
    zctVec4,
    zctXptr,
    zctVoid,  //Private types
    zctReference,
    zctNull,
    zctArray,
    zctClass); //User defined script class

  TZcDataType = record
    Kind : TZcDataTypeKind;
    {$ifndef minimal}
    ReferenceClassId : TZClassIds;
    IsPointer : boolean;  //True for type of argument in f(ref x) function
    TheArray : pointer;  //When zctArray: pointer to TDefineArray
    TheClass : TObject;  //When zctClass: pointer to TZcOpClass
    function Matches(const Other:TZcDataType) : boolean;
    {$endif}
  end;

  TExpressionKind = (ekiNormal,ekiLibrary,ekiGetValue,ekiGetPointer,ekiBitmap,
    ekiMesh,ekiThread,ekiGetStringValue);

  PZBinaryPropValue = ^TZBinaryPropValue;
  TZBinaryPropValue = record
    Size : integer;
    Data : pointer;
  end;

  TZPropertyValue = record
    {$IFNDEF MINIMAL}
    //String-props kan ej ligga i case-switch f�r designer
    ExpressionValue : TZExpressionPropValue;
    StringValue : AnsiString;
    {$ENDIF}
    case integer of
      0 : (FloatValue : single);
      2 : (RectfValue : TZRectf);
      3 : (ColorfValue : TZColorf);
      4 : (IntegerValue : integer);
      5 : (ComponentValue : TZComponent);
      {$ifdef minimal}6 : (StringValue : PAnsiChar);{$endif}
      8 : (Vector3fValue : TZVector3f);
      9 : (ComponentListValue : TZComponentList);
     10 : (GenericValue : array[0..2] of integer); //f�r default-data test
     11 : (ByteValue : byte);
     12 : (BooleanValue : ByteBool);
     {$ifdef minimal}
     13 : (ExpressionValue : TZExpressionPropValue);
     {$endif}
     14 : (BinaryValue : TZBinaryPropValue);
     15 : (PointerValue : pointer);
  end;

  //zptScalar = float with 0..1 range
  //zptPointer = managed pointer (vec/mat), never persisted
  TZPropertyType = (zptFloat,zptScalar,zptRectf,zptColorf,zptString,zptComponentRef,zptInteger,
    zptVector3f,zptComponentList,zptByte,zptBoolean,
    zptExpression,zptBinary,zptPointer);

  TPropNotifyProc = procedure(Instance : TZComponent; const PropId : integer; const NewValue : pointer);

  TZProperty = class
  public
    DefaultValue : TZPropertyValue;
    NeverPersist : boolean;
    DontClone : boolean;
    IsManagedTarget: boolean;   //Values are garbagecollected
    Offset : NativeInt;
    PropertyType : TZPropertyType;
    PropId : integer;             //Ordningsnr p� denna property f�r en klass
    GlobalData : pointer;      //Either Offset or GlobalData are set (see AudioMixer for global data)
    NotifyWhenChanged : TPropNotifyProc;  //Call PropertyHasChanged
    {$IFNDEF MINIMAL}public{$ELSE}private{$ENDIF}
    {$IFNDEF MINIMAL}
    Name : string;              //Namn p� property i designer 'Color'
    ExcludeFromBinary : boolean;  //Ta inte med denna prop i bin�rstr�m (designer only)
    ExcludeFromXml : boolean; //Spara ej i xml-fil
    IsReadOnly : boolean;       //Prop kan ej tilldelas i expressions
    NeedRefreshNodeName : boolean;//True f�r propertys som kr�ver refresh i nodtr�d vid �ndring av prop
    ChildClasses :               //F�r componentlists: krav p� vilka klasser som kan ligga i listan
      array of TZComponentClass; //F�r componentref: krav p� vilka klasser som g�r att referera till
    Options : array of string;  //F�r bytes: Valbara alternativ
    HideInGui : boolean;        //Visa inte denna prop i gui
    ReturnType : TZcDataType;      //For expresssions: return type of expression
    ExpressionKind : TExpressionKind;  //For expressions: kind of expression
    Hint : string; //For editor tooltips: a short explanation of this property
    procedure SetChildClasses(const C : array of TZComponentClass);
    procedure SetOptions(const O : array of string);
    constructor Create;
    {$ENDIF}
    function IsDefaultValue(const Value : TZPropertyValue) : boolean;
  end;

  TZPropertyList = class(TZArrayList)
  private
    NextId : integer;
  public
    TheSelf : NativeInt;
    procedure AddProperty({$IFNDEF MINIMAL}const Name : string; {$ENDIF} const Addr: pointer; const PropType : TZPropertyType);
    procedure AddGlobalDataProperty({$IFNDEF MINIMAL}const Name: string;{$ENDIF} const Addr: pointer; const PropType : TZPropertyType);
    {$IFNDEF MINIMAL}
    procedure SetDesignerProperty;
    function GetByName(const Name : string) : TZProperty;
    function GetByType(Kind : TZPropertyType) : TZProperty;
    {$ENDIF}
    function GetLast : TZProperty;
    function GetById(PropId : integer) : TZProperty;
  end;

  //ZGE base component class.
  //NOTE: for size coding, avoid virtual methods here since it will increate vmt for every child class
  TZComponent = class
  private
    function DoClone(ObjIds,FixUps : TZArrayList; Into : TZComponent) : TZComponent;
  protected
    ObjId : integer;    //only used in streaming
    IsChanged : boolean;
    procedure DefineProperties(List : TZPropertyList); virtual;
    procedure InitAfterPropsAreSet; virtual; //On creation after props have their values (streamed or cloned)
  public
    {$ifndef minimal}
    Name,Comment : TPropString;
    DesignDisable : boolean;
    {$endif}
    OwnerList : TZComponentList;
    _ZApp : pointer;
    constructor Create(OwnerList: TZComponentList); virtual;
    destructor Destroy; override;
    function GetProperties : TZPropertyList;
    procedure SetProperty(Prop : TZProperty; const Value : TZPropertyValue);
    function GetProperty(Prop: TZProperty) : TZPropertyValue;
    function GetPropertyPtr(Prop : TZProperty; Index : integer) : pointer;
    procedure Update; virtual;
    procedure Change;
    function Clone(Into : TZComponent = nil) : TZComponent;
    procedure ResetGpuResources; virtual; //Free resources such as GL-handles
    {$ifndef minimal}
    function GetDisplayName : AnsiString; virtual;
    procedure DesignerReset; virtual;  //Reset house-keeping state (such as localtime in animators)
    function GetOwner : TZComponent;
    procedure SetString(const PropName : string; const Value : AnsiString);
    procedure GetAllChildren(List : contnrs.TObjectList; IncludeExpressions : boolean);
    {$endif}
  end;


  //Command �r komponent som anv�nds i event-tr�d
  //Execute-metoden k�rs vid event-utv�rdering
  TCommand = class(TZComponent)
  public
    procedure Execute; virtual; abstract;
  end;


  //Standardkomponent med en definierad property: 'Children'
  //�rver TCommand s� att den exekverar children n�r den ligger i eventlists.
  TLogicalGroup = class(TCommand)
  protected
    procedure DefineProperties(List : TZPropertyList); override;
  public
    Children : TZComponentList;
    procedure Update; override;
    procedure Execute; override;
  end;



  //Info about one componentclass
  TZComponentInfo = class
  private
    Properties : TZPropertyList;
    {$IFNDEF MINIMAL}public{$ELSE}private{$ENDIF}
    ClassId : TZClassIds;
    {$IFNDEF MINIMAL}
    ZClassName : string;  //Klassnamn utan 'T'
    NoUserCreate : boolean;
    NoTopLevelCreate : boolean; //Till�t ej anv�ndare att skapa denna p� toppniv�
    ExcludeFromBinary : boolean; //Skippa hela komponenten i bin�rfilen
    ImageIndex : integer;  //Icon som ska visas i designertr�det
    HelpText : string;
    NeedParentComp : string;
    NeedParentList : string;
    AutoName : boolean;  //Give name automatically when inserted in designer
    ParamCount : integer; //Parameter count for contentproducers
    {$ENDIF}
  public
    ZClass : TZComponentClass;
    {$ifndef MINIMAL}
    HasGlobalData : boolean; //See audiomixer. Do not cache property-list.
    function GetProperties : TZPropertyList;
    {$endif}
  end;


  TComponentInfoArray = array[TZClassIds] of TZComponentInfo;
  PComponentInfoArray = ^TComponentInfoArray;

  //Singleton
  //Keeps track of all componentclasses
  TZComponentManager = class
  private
    ComponentInfos : TComponentInfoArray;
    {$IFNDEF MINIMAL}
    InfoDictionary : TDictionary<TClass,TZComponentInfo>;
    {$ENDIF}
    procedure Register(C : TZComponentClass; ClassId : TZClassIds);
    function GetProperties(Component : TZComponent) : TZPropertyList;
    {$ifndef minimal}public
    destructor Destroy; override;{$else}private{$endif}
    function GetInfo(Component : TZComponent) : TZComponentInfo;
  public
    {$if (not defined(MINIMAL)) or defined(zzdc_activex)}
    LastAdded : TZComponentInfo;
    {$endif}
    function GetInfoFromId(ClassId : TZClassIds) : TZComponentInfo;
  {$IFNDEF MINIMAL}
    function GetInfoFromClass(const C: TZComponentClass): TZComponentInfo;
    function SaveBinaryToStream(Component : TZComponent) : TObject;
    function LoadXmlFromFile(FileName : string) : TZComponent;
    function LoadXmlFromString(const XmlData : string; SymTab : TSymbolTable) : TZComponent;
    procedure SaveXml(Component : TZComponent; FileName : string);
    function SaveXmlToStream(Component: TZComponent) : TObject;
    function GetAllInfos : PComponentInfoArray;
    function GetInfoFromName(const ZClassName : string) : TZComponentInfo;
    function LoadBinaryFromFile(const FileName : string) : TZComponent;
  {$ENDIF}
    function LoadBinary : TZComponent;
  end;



  //Content that can be produced from contentproducers (bitmaps and meshes)
  TContent = class(TZComponent)
  protected
    procedure RefreshFromProducers;
    procedure CopyAndDestroy(Source : TContent); virtual; abstract;
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Producers : TZComponentList;
  end;

  TContentProducer = class(TCommand)
  protected
    procedure ProduceOutput(Content : TContent; Stack : TZArrayList); virtual; abstract;
  public
    procedure Execute; override;
  end;


  //Baseclass for AppState and ModelState
  TStateBase = class(TZComponent)
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    OnStart : TZComponentList;
    OnUpdate : TZComponentList;
    OnLeave : TZComponentList;
    OnRender : TZComponentList;
    Definitions : TZComponentList;
    procedure Update; override;
  end;


  //Readonly stream
  TZInputStream = class
  private
    OwnsMemory : boolean;
    Memory : PBytes;
    {$ifdef zdebug}
    IsBitMode : boolean;
    {$endif}
    BitNo : integer;
    Bits : byte;
  public
  {
    Position
    Size
    function GetMemory : pointer;
    procedure LoadFromFile
    procedure LoadFromResource
    procedure Read(p,count)
  }
    Size,Position : integer;
    constructor CreateFromFile(FileName : PAnsiChar; IsRelative : Boolean);
    constructor CreateFromMemory(M : pointer; Size : integer);
    destructor Destroy; override;
    procedure Read(var Buf; Count : integer);
    function GetMemory : PBytes;
    procedure BitsBegin;
    function ReadBit : boolean;
    procedure BitsEnd;
  end;

  //Threading support

  //Basic thread wrapper
  TZThread = class
  public
    Handle : pointer;
    Terminated : boolean;
    procedure Execute; virtual; abstract;
    procedure Start;
    destructor Destroy; override;
  end;

  //Thread component
  TZThreadComponent = class(TZComponent)
  strict private
    type
      TUserThread = class(TZThread)
      private
        Owner : TZThreadComponent;
        Parameter : integer;
      public
        procedure Execute; override;
      end;
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Expression : TZExpressionPropValue;
    procedure Start(Param : integer);
  end;

  //Threaded task execution
  TTaskProc = procedure(Task : pointer) of object;
  TTasks = class
  strict private
    type
      TWorkerThread = class(TZThread)
      private
        Tasks : TTasks;
      public
        procedure Execute; override;
      end;
    PWorkerThread = ^TWorkerThread;
  var
    TaskList : pointer;
    TaskProc : TTaskProc;
    InitialTaskCount,TaskCount,TaskStride : integer;
    FinishedTaskCount : integer;
    Lock : pointer;
    Event : pointer;
    ThreadCount : integer;
    Threads : PWorkerThread;
    function RunNext : boolean;
  private
    class var Instance : TTasks;
    constructor Create;
  public
    WorkerCount : integer;
    Enabled : boolean;
    procedure Run(TaskProc : TTaskProc; TaskList : pointer; TaskCount,TaskStride : integer);
    destructor Destroy; override;
  end;


function Tasks : TTasks;

function MakeColorf(const R,G,B,A : single) : TZColorf;

{.$IFNDEF MINIMAL}
function ComponentManager : TZComponentManager;
{.$ENDIF}

//Register componentclass
procedure Register(C : TZComponentClass; ClassId : TZClassIds);

function GetZcTypeSize(const Typ : TZcDataTypeKind) : integer;

//String functions
function ZStrLength(P : PAnsiChar) : integer;
procedure ZStrCopy(P : PAnsiChar; const Src : PAnsiChar);
procedure ZStrCat(P : PAnsiChar; const Src : PAnsiChar);
procedure ZStrConvertInt(const S : integer; Dest : PAnsiChar);
function ZStrPos(const SubStr,Str : PAnsiChar; const StartPos : integer) : integer;
function ZStrCompare(P1,P2 : PAnsiChar) : boolean;
procedure ZStrSubString(const Str,Dest : PAnsiChar; const StartPos,NChars : integer);
function ZStrToInt(const Str : PAnsiChar) : integer;

//Garbage collected managed heap
function ManagedHeap_Alloc(const Size : integer) : pointer;
procedure ManagedHeap_AddValueObject(const O : TObject);
function ManagedHeap_GetAllocCount : integer;
procedure ManagedHeap_GarbageCollect;
procedure ManagedHeap_AddTarget(const P : pointer);
procedure ManagedHeap_RemoveTarget(const P : pointer);
{$ifndef minimal}
function ManagedHeap_GetStatus : string;
{$endif}


{$ifndef minimal}
const
  FloatTypes : set of TZPropertyType = [zptFloat,zptScalar,zptRectf,zptColorf,zptVector3f];
  FloatTextDecimals = 4;  //Nr of fraction digits when presenting float-values as text

  ZcTypeNames : array[TZcDataTypeKind] of string =
(('float'),('int'),('string'),('model'),('byte'),('mat4'),('vec2'),('vec3'),('vec4'),('xptr'),
 ('void'),('Component'),('null'),('#array'),('#class'));

const ManagedTypes : set of TZcDataTypeKind =
 [zctClass,zctString,zctArray,zctMat4,zctVec2,zctVec3,zctVec4];

procedure GetAllObjects(C : TZComponent; List : contnrs.TObjectList);
procedure GetObjectNames(C : TZComponent; List : TStringList);
function HasReferers(Root, Target : TZComponent; Deep : boolean = True) : boolean;
procedure GetReferersTo(Root, Target : TZComponent; List : contnrs.TObjectList);
function FindInstanceOf(C : TZComponent; Zc : TZComponentClass) : TZComponent;

var
  DesignerPreviewProducer : TZComponent;
{$endif}

implementation

uses ZMath,ZLog, ZPlatform, ZApplication, ZExpressions
  {$ifndef minimal}
    ,LibXmlParserU
    ,SysUtils,Math,zlib,Zc_Ops
    {$ifndef fpc},AnsiStrings{$endif}
    {$ifdef fpc}
    ,zstream,strutils
    {$endif}
  {$endif}
  ;


type
{$IFNDEF MINIMAL}
  TZOutputStream = class(TMemoryStream)
  private
    IsBitMode : boolean;
    BitNo : integer;
    Bits : byte;
  protected
    procedure BitsBegin;
    procedure WriteBit(B : boolean);
    procedure BitsEnd;
  end;

  TZWriter = class
  private
    Stream : TZOutputStream;
    Root : TZComponent;
    procedure DoWriteComponent(C : TZComponent); virtual; abstract;
    procedure Write(const B; Count : integer);
    procedure OnDocumentStart; virtual; abstract;
    procedure OnDocumentEnd; virtual; abstract;
  public
    constructor Create(Stream : TZOutputStream);
    procedure WriteRootComponent(C : TZComponent);
  end;

  //Write a component as xml
  TZXmlWriter = class(TZWriter)
  private
    IndentLevel : integer;
    OldSeparator : char;
    procedure LevelDown;
    procedure LevelUp;
    procedure WriteString(const S : string);
    procedure WriteLine(const S : string);
    procedure OnDocumentStart; override;
    procedure OnDocumentEnd; override;
    procedure DoWriteComponent(C : TZComponent); override;
  end;

  //Sepearate stream for each property-type
  //Main-stream (Stream) contains ClassIDs and propmasks
  TZBinaryWriter = class(TZWriter)
  private
    AssignedObjs : TZArrayList;
    PStreams : array[TZPropertyType] of TZOutputStream;
    procedure OnDocumentStart; override;
    procedure OnDocumentEnd; override;
    procedure DoWriteComponent(C : TZComponent); override;
  end;
{$ENDIF}

  TZReader = class
  private
    function DoReadComponent(OwnerList : TZComponentList) : TZComponent; virtual; abstract;
    procedure OnDocumentStart; virtual; abstract;
    procedure OnDocumentEnd; virtual; abstract;
    function ReadRootComponent : TZComponent;
  end;

  TZBinaryReader = class(TZReader)
  private
    PStreams : array[TZPropertyType] of TZInputStream;
    Stream : TZInputStream;
    FixUps : TZArrayList;
    ObjIds : TZArrayList;
    procedure OnDocumentStart; override;
    procedure OnDocumentEnd; override;
    procedure Read(var B; Count : integer);
    function DoReadComponent(OwnerList : TZComponentList) : TZComponent; override;
    constructor Create(Stream : TZInputStream);
  end;

{$IFNDEF MINIMAL}
  TZXmlFixUp = class
    Name,PropName : string;
    Prop : TZProperty;
    Obj : TZComponent;
  end;

  TZXmlReader = class(TZReader)
  private
    MainXml : LibXmlParserU.TXmlParser;
    FixUps : TZArrayList;
    SymTab : TSymbolTable;
    OldSeparator : char;
    ExternalSymTab : boolean;
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFile(const FileName: string);
    procedure LoadFromString(const XmlData: string; SymTab : TSymbolTable);
    function DoReadComponent(OwnerList : TZComponentList) : TZComponent; override;
    function XmlDoReadComponent(Xml : TXmlParser; OwnerList : TZComponentList) : TZComponent;
    procedure OnDocumentStart; override;
    procedure OnDocumentEnd; override;
  end;
{$ENDIF}

const
  TBinaryNested : set of TZPropertyType = [zptComponentList,zptExpression];

{$ifndef MINIMAL}
//Manage memory for strings set in designer
var
  StringCache : TDictionary<AnsiString,AnsiString>;
{$endif}


{$ifndef MINIMAL}
//Faster TDictionary based GC
  {$define UseDictionaryGC}
{$endif}

//Managed Heap
var
  mh_Targets,mh_Allocations : TZArrayList;
  mh_Values : {$ifdef UseDictionaryGC}TDictionary<pointer,boolean>{$else}TZArrayList{$endif};
  mh_LastCount : integer;
  mh_Lock : pointer;

const
  NilString : AnsiChar = #0;

procedure ManagedHeap_Create;
begin
  mh_Targets := TZArrayList.CreateReferenced;
  mh_Allocations := TZArrayList.CreateReferenced;
  {$ifdef UseDictionaryGC}
  mh_Values := TDictionary<pointer,boolean>.Create;
  {$else}
  mh_Values := TZArrayList.CreateReferenced;
  {$endif}
  mh_Lock := Platform_CreateMutex;
end;

procedure ManagedHeap_FreeMemAt(const Index : integer);
var
  P : pointer;
begin
  P := mh_Allocations[Index];
  mh_Allocations.SwapRemoveAt(Index);
  if NativeInt(P) and 1=1 then
    TObject( NativeInt(P) and (not 1) ).Free
  else
    FreeMem(P);
end;

procedure ManagedHeap_Destroy;
begin
  while mh_Allocations.Count>0 do
    ManagedHeap_FreeMemAt( mh_Allocations.Count-1 );
  mh_Targets.Free;
  mh_Allocations.Free;
  mh_Values.Free;
  Platform_FreeMutex(mh_Lock);
end;

function ManagedHeap_Alloc(const Size : integer) : pointer;
begin
  {$ifdef debug}
  ZAssert(Size>=0,'Alloc called with size <=0');
  ZAssert(Size<1024*1024*128,'Alloc called with size > 128mb');
  {$endif}
  GetMem(Result,Size);
  Platform_EnterMutex(mh_Lock);
    mh_Allocations.Add(Result);
  Platform_LeaveMutex(mh_Lock);
  {$ifdef debug}
  ZAssert(NativeInt(Result) and 1=0,'Alloc fail');
  {$endif}
end;

procedure ManagedHeap_AddValueObject(const O : TObject);
//Add an TObject to managed heap. The destructor will be called when released.
begin
  {$ifdef debug}
  ZAssert(NativeInt(O) and 1=0,'AddValueObject fail');
  {$endif}
  //Use unused lower bits of pointer to flag that this is an object
  Platform_EnterMutex(mh_Lock);
    mh_Allocations.Add(Pointer(NativeInt(O) or 1));
  Platform_LeaveMutex(mh_Lock);
end;

{$ifdef debug}
  {$define extra_gc_checks}
{$endif}
procedure ManagedHeap_AddTarget(const P : pointer);
begin
  {$ifdef extra_gc_checks} //These checks are very inefficient so dont't enable by default even in debug build
  if mh_Targets.IndexOf(P)>-1 then
  begin
    GetLog('MH').Warning('Add target already in list');
    Exit;
  end;
  {$endif}
  Platform_EnterMutex(mh_Lock);
    mh_Targets.Add(P);
  Platform_LeaveMutex(mh_Lock);
end;

procedure ManagedHeap_RemoveTarget(const P : pointer);
begin
  {$ifdef extra_gc_checks}
  if mh_Targets.IndexOf(P)=-1 then
  begin
    GetLog('MH').Warning('Remove target not found');
    Exit;
  end;
  {$endif}
  Platform_EnterMutex(mh_Lock);
    mh_Targets.SwapRemove(P);
  Platform_LeaveMutex(mh_Lock);
end;

function ManagedHeap_GetAllocCount : integer;
begin
  Result := mh_Allocations.Count;
end;

{$ifndef minimal}
function ManagedHeap_GetStatus : string;
begin
  Result := IntToStr(mh_Allocations.Count) + ' (t: ' + IntToStr(mh_Targets.Count) + ')';
end;
{$endif}

procedure ManagedHeap_GarbageCollect;
var
  I : integer;
  {$ifndef UseDictionaryGC}J : integer;{$endif}
  PP : PPointer;
  P : pointer;
begin
  if mh_Allocations.Count=mh_LastCount then
    //Heap is stable since last call, no point in collecting
    Exit;
  mh_LastCount := mh_Allocations.Count;

  //Fill a list with all the current values of all variables that can hold a allocated pointer
  mh_Values.Clear;
  for I := 0 to mh_Targets.Count - 1 do
  begin
    PP := PPointer(mh_Targets[I]);
    P := PP^;
    if (P<>nil) and (P<>@NilString) then
      {$ifdef UseDictionaryGC}
      mh_Values.AddOrSetValue(P,True);
      {$else}
      mh_Values.Add(P);
      {$endif}
  end;

  Platform_EnterMutex(mh_Lock);

  I := 0;
  while I<mh_Allocations.Count do
  begin
    P := Pointer( NativeInt(mh_Allocations[I]) and (not 1) );
    {$ifdef UseDictionaryGC}
    if not mh_Values.ContainsKey(P) then
      ManagedHeap_FreeMemAt(I)
    else
      Inc(I);
    {$else}
    J := mh_Values.IndexOf(P);
    if J=-1 then
    begin
      //Pointer is no longer used
      ManagedHeap_FreeMemAt(I);
    end
    else
    begin
      //Pointer is used, remove this value to keep mh_Values as short as possible
      mh_Values.SwapRemoveAt(J);
      Inc(I);
    end;
    {$endif}
  end;

  Platform_LeaveMutex(mh_Lock);
end;


//Accessfunctions for componentmanager
var
  _ComponentManager : TZComponentManager = nil;

function ComponentManager : TZComponentManager;
begin
  if _ComponentManager = nil then
    _ComponentManager := TZComponentManager.Create;
  Result := _ComponentManager;
end;

procedure Register(C : TZComponentClass; ClassId : TZClassIds);
begin
  ComponentManager.Register(C,ClassId);
end;



//Utility

{$ifndef minimal}
procedure GetAllObjects(C : TZComponent; List : contnrs.TObjectList);
//Returns all objects
begin
  C.GetAllChildren(List,True);
end;

procedure GetReferersTo(Root, Target : TZComponent; List : contnrs.TObjectList);
//Returns all objects that refers to component Target
var
  PropList : TZPropertyList;
  Prop : TZProperty;
  Value : TZPropertyValue;
  I,J : integer;
  All : TObjectList;
  C : TZComponent;
begin
  All := TObjectList.Create(False);
  try
    GetAllObjects(Root,All);
    for I := 0 to All.Count-1 do
    begin
      C := TZComponent(All[I]);
      if C.DesignDisable then
        Continue;
      PropList := C.GetProperties;
      for J := 0 to PropList.Count-1 do
      begin
        Prop := TZProperty(PropList[J]);
        case Prop.PropertyType of
          zptComponentRef :
            begin
              Value := C.GetProperty(Prop);
              if Value.ComponentValue=Target then
                List.Add(C);
            end;
        end;
      end;
    end;
  finally
    All.Free;
  end;
end;

function HasReferers(Root, Target : TZComponent; Deep : boolean = True) : boolean;
var
  TargetList,List : TObjectList;
  I,J : integer;
  C : TZComponent;
begin
  TargetList:= TObjectList.Create(False);
  List:=TObjectList.Create(False);
  try
    //H�mta alla barnen till Target
    if Deep then
      GetAllObjects(Target,TargetList)
    else
      TargetList.Add(Target);
    for I := 0 to TargetList.Count-1 do
    begin
      //Kolla alla barnen om det finns referens som �r utanf�r targetlist
      C := TargetList[I] as TZComponent;
      List.Clear;
      GetReferersTo(Root,C,List);
      for J := 0 to List.Count-1 do
      begin
        if TargetList.IndexOf(List[J])=-1 then
        begin
          Result := True;
          Exit;
        end;
      end;
    end;
    Result := False;
  finally
    List.Free;
    TargetList.Free;
  end;
end;

procedure GetObjectNames(C : TZComponent; List : TStringList);
//Returns all objectnames
//Hoppar �ver objekt som ej har namn tilldelat
var
  I : integer;
  ObList : TObjectList;
  S : string;
begin
  ObList := TObjectList.Create(False);
  try
    GetAllObjects(C,ObList);
    for I := 0 to ObList.Count-1 do
    begin
      S := String(TZComponent(ObList[I]).Name);
      if Length(S)>0 then
        List.AddObject(S , TZComponent(ObList[I]) );
    end;
  finally
    ObList.Free;
  end;
end;

function FindInstanceOf(C : TZComponent; Zc : TZComponentClass) : TZComponent;
var
  I : integer;
  ObList : TObjectList;
begin
  Result := nil;
  ObList := TObjectList.Create(False);
  try
    GetAllObjects(C,ObList);
    for I := 0 to ObList.Count-1 do
    begin
      if TZComponent(ObList[I]) is Zc then
      begin
        Result := TZComponent(ObList[I]);
        Break;
      end;
    end;
  finally
    ObList.Free;
  end;
end;
{$endif}

function GetZcTypeSize(const Typ : TZcDataTypeKind) : integer;
begin
  case Typ of
    zctByte : Result := 1;
    zctModel,zctString,zctXptr,zctVoid,zctNull,zctArray,zctClass : Result := SizeOf(Pointer);
    zctMat4 : Result := SizeOf(single) * 16;
    zctVec2 : Result := SizeOf(single) * 2;
    zctVec3 : Result := SizeOf(single) * 3;
    zctVec4 : Result := SizeOf(single) * 4;
  else
    Result := SizeOf(Integer);
  end;
end;

{ TZComponent }

constructor TZComponent.Create(OwnerList: TZComponentList);
var
  PropList : TZPropertyList;
  Prop : TZProperty;
  I : integer;
  List : TZComponentList;
  P : Pointer;
begin
  PropList := GetProperties;
  for I := 0 to PropList.Count-1 do
  begin
    Prop := TZProperty(PropList[I]);
    //Initialize list properties
    case Prop.PropertyType of
      zptComponentList :
        begin
          List := TZComponentList.Create(Self);
          PPointer(GetPropertyPtr(Prop,0))^ := List;
        end;
      zptExpression :
        begin
          List := TZComponentList.Create(Self);
          PZExpressionPropValue(GetPropertyPtr(Prop,0))^.Code := List;
        end;
      zptString,zptPointer :
        begin
          P := GetPropertyPtr(Prop,0);
          if Prop.IsManagedTarget then
            ManagedHeap_AddTarget(P);
          if Prop.PropertyType=zptString then
            PPointer(P)^ := @NilString;
        end;
    end;
    //Set defaultvalue for property
    //todo: Robustare s�tt att testa ifall defaultv�rde finns, generic4 �r < sizeof(value)
    if (Prop.DefaultValue.GenericValue[0]<>0) or
      (Prop.DefaultValue.GenericValue[1]<>0) or
      (Prop.DefaultValue.GenericValue[2]<>0)
     {$ifndef minimal}or
       (Prop.DefaultValue.StringValue<>'') or (Prop.DefaultValue.ExpressionValue.Source<>'')
     {$endif}
     then
      SetProperty(Prop,Prop.DefaultValue);
  end;

  //add to ownerlist
  if OwnerList <> nil then
    OwnerList.AddComponent(Self);
end;


destructor TZComponent.Destroy;
var
  PropList : TZPropertyList;
  Prop : TZProperty;
  Value : TZPropertyValue;
  I : integer;
begin
  //Remove from ownerlist
  if OwnerList <> nil then
    OwnerList.RemoveComponent(Self);
  //Release memory for properties
  PropList := GetProperties;
  for I := 0 to PropList.Count-1 do
  begin
    Prop := TZProperty(PropList[I]);
    case Prop.PropertyType of
      zptComponentList :
        begin
          Value := GetProperty(Prop);
          Value.ComponentListValue.Free;
        end;
      zptExpression :
        begin
          Value := GetProperty(Prop);
          Value.ExpressionValue.Code.Free;
        end;
      zptString,zptPointer :
        begin
          if Prop.IsManagedTarget then
            ManagedHeap_RemoveTarget(GetPropertyPtr(Prop,0));
        end;
      {$ifndef minimal}
      zptBinary :
        begin
          //Frig�r binary mem enbart i designer.
          //I minimal s� klonas binary genom att peka p� samma source, mem skulle
          //d� frig�ras flera g�nger.
          Value := GetProperty(Prop);
          FreeMem(Value.BinaryValue.Data);
        end;
      {$endif}
    end;
  end;
  inherited;
end;

procedure TZComponent.DefineProperties(List: TZPropertyList);
begin
  {$IFNDEF MINIMAL}
  List.AddProperty(strName, @Name, zptString);
    List.SetDesignerProperty;
    List.GetLast.NeedRefreshNodeName := True;
  List.AddProperty(strComment, @Comment, zptString);
    List.SetDesignerProperty;
    List.GetLast.NeedRefreshNodeName := True;
  List.AddProperty(strDesignDisable, @DesignDisable, zptBoolean);
    List.SetDesignerProperty;
    List.GetLast.NeedRefreshNodeName := True;
  {$ENDIF}
  List.AddProperty({$IFNDEF MINIMAL}strObjId,{$ENDIF}@ObjId, zptInteger);
    {$IFNDEF MINIMAL}
    List.GetLast.ExcludeFromXml := True;
    List.GetLast.IsReadOnly := True;
    {$ENDIF}
    List.GetLast.DontClone := True;
end;

function TZComponent.GetProperty(Prop: TZProperty) : TZPropertyValue;
var
  P : pointer;
begin
  if Assigned(Prop.GlobalData) then
    P := Prop.GlobalData
  else
    P := pointer(NativeInt(Self) + Prop.Offset);
  case Prop.PropertyType of
    zptFloat,zptScalar :
      Result.FloatValue := PFloat(P)^;
    zptString :
      Result.StringValue := PAnsiChar(PPointer(P)^);
    zptComponentRef :
      Result.ComponentValue := TZComponent(PPointer(P)^);
    zptInteger :
      Result.IntegerValue := PInteger(P)^;
    zptRectf :
      Result.RectfValue := PRectf(P)^;
    zptColorf :
      Result.ColorfValue := PColorf(P)^;
    zptVector3f :
      Result.Vector3fValue := PZVector3f(P)^;
    zptComponentList :
      Result.ComponentListValue := TZComponentList(PPointer(P)^);
    zptByte :
      Result.ByteValue := PByte(P)^;
    zptBoolean :
      Result.BooleanValue := PBoolean(P)^;
    zptExpression :
      {$ifdef minimal}
      Result.ExpressionValue.Code := TZComponentList(PPointer(P)^);
      {$else}
      Result.ExpressionValue := PZExpressionPropValue(P)^;
      {$endif}
    zptBinary :
      Result.BinaryValue := PZBinaryPropValue(P)^;
    zptPointer :
      Result.PointerValue := PPointer(P)^;
    {$ifdef minimal}
    else
      ZHalt('GetProperty no handler');
    {$endif}
  end;
end;

procedure TZComponent.SetProperty(Prop: TZProperty; const Value: TZPropertyValue);
var
  P : pointer;
  {$ifndef MINIMAL}
  S : ansistring;
  {$endif}
begin
  if Assigned(Prop.GlobalData) then
    P := Prop.GlobalData
  else
    P := pointer(NativeInt(Self) + Prop.Offset);
  case Prop.PropertyType of
    zptFloat,zptScalar :
      PFloat(P)^ := Value.FloatValue;
    zptString :
      {$IFDEF MINIMAL}
      //string ska vara immutable.
      PPAnsiChar(P)^ := Value.StringValue;
      {$ELSE}
      begin
        S := Value.StringValue + #0;
        if not StringCache.ContainsKey(S) then
          StringCache.Add(S,S);
        PPointer(P)^ := @StringCache[S][1];
      end;
      {$ENDIF}
    zptComponentRef :
      PPointer(P)^ := pointer(Value.ComponentValue);
    zptInteger :
      PInteger(P)^ := Value.IntegerValue;
    zptByte :
      PByte(P)^ := Value.ByteValue;
    zptRectf :
      PRectf(P)^ := Value.RectfValue;
    zptColorf :
      PColorf(P)^ := Value.ColorfValue;
    zptVector3f :
      PZVector3f(P)^ := Value.Vector3fValue;
    zptComponentList :
      begin
        {$IFNDEF MINIMAL}
        //M�ste tilldelas till samma v�rde, annars s� ska vi g�ra free p� �ldre v�rdet
        Assert(PPointer(P)^ = pointer(Value.ComponentListValue));
        {$ENDIF}
        //On�digt att tilldela samma v�rde
        //PPointer(P)^ := pointer(Value.ComponentListValue);
      end;
    zptBoolean :
      PBoolean(P)^ := Value.BooleanValue;
    zptExpression :
      {$ifdef minimal}
      //Tilldelas ej direkt i minimal. Skapas i create och fylls p� i binary-load.
      ;
      {$else}
      PZExpressionPropValue(P)^.Source := Value.ExpressionValue.Source;  //Tool tilldelar source-str�ngen
      {$endif}
    zptBinary :
      begin
        {$ifndef minimal}
        if PZBinaryPropValue(P)^.Size>0 then
          FreeMem(PZBinaryPropValue(P)^.Data);
        {$endif}
        PZBinaryPropValue(P)^ := Value.BinaryValue;
      end;
    zptPointer :
      PPointer(P)^ := Value.PointerValue;
    {$ifdef minimal}
    else
      ZHalt('SetProperty no handler');
    {$endif}
  end;
  Change;
end;

function TZComponent.GetProperties: TZPropertyList;
begin
  Result := ComponentManager.GetProperties(Self);
end;



//Returnerar pekare till property-value
//Anv�nds f�r att hitta target f�r propertyrefs
function TZComponent.GetPropertyPtr(Prop: TZProperty; Index: integer): pointer;
begin
  if Assigned(Prop.GlobalData) then
    Exit(Prop.GlobalData)
  else
    Result := pointer(NativeInt(Self) + Prop.Offset);
  if Index>0 then
    Result := pointer(NativeInt(Result) + Index*4);
end;

procedure TZComponent.InitAfterPropsAreSet;
begin
end;

procedure TZComponent.Change;
begin
  IsChanged := True;
  //todo b�ttre hantering av change beh�vs nog
  if (OwnerList<>nil) then
    OwnerList.Change;
end;

procedure TZComponent.Update;
begin
  //
end;

procedure CloneAssignObjectIds(C : TZComponent; ObjIds,CleanUps : TZArrayList);
//Assigns all objects in tree with unique object-ids
var
  PropList : TZPropertyList;
  Prop : TZProperty;
  Value : TZPropertyValue;
  I,J : integer;
begin
  C.ObjId := ObjIds.Count;
  ObjIds.Add(nil);
  CleanUps.Add(@C.ObjId);
  PropList := C.GetProperties;
  for I := 0 to PropList.Count-1 do
  begin
    Prop := TZProperty(PropList[I]);
    case Prop.PropertyType of
      zptComponentList :
        begin
          Value := C.GetProperty(Prop);
          for J := 0 to Value.ComponentListValue.ComponentCount-1 do
            CloneAssignObjectIds(TZComponent(Value.ComponentListValue[J]),ObjIds,CleanUps);
        end;
      zptExpression :
        begin
          Value := C.GetProperty(Prop);
          for J := 0 to Value.ExpressionValue.Code.ComponentCount-1 do
            CloneAssignObjectIds(TZComponent(Value.ExpressionValue.Code[J]),ObjIds,CleanUps);
        end;
    end;
  end;
end;

function TZComponent.Clone(Into : TZComponent = nil): TZComponent;
var
  ObjIds,CleanUps,Fixups : TZArrayList;
  I,Id : integer;
  P : PPointer;
begin
{
  Set ObjIDs for this component and all children
  Clone children recursively
  If ref to component with ObjID set, keep in Fixup-list
  Every clone writes itself to ObjIds which is a map objid->cloned component replacing original.objid
  Loop fixups and set component references
  Zero out assigned objids
}
  ObjIds := TZArrayList.CreateReferenced;
  ObjIds.Add(nil);
  CleanUps := TZArrayList.CreateReferenced;
  FixUps := TZArrayList.CreateReferenced;
  CloneAssignObjectIds(Self,ObjIds,CleanUps);
  Result := DoClone(ObjIds,FixUps,Into);

  //component references
  for I := 0 to FixUps.Count-1 do
  begin
    P := PPointer(FixUps[I]);
    Id := TZComponent(P^).ObjId;
    P^ := ObjIds[Id];
  end;
  FixUps.Free;

  ObjIds.Free;
  //Zero out objids after whole tree is cloned
  for I := 0 to CleanUps.Count-1 do
    PInteger(CleanUps[I])^:=0;
  CleanUps.Free;

  {$ifndef minimal}
  if Self.HasZApp then
  {$endif}
  Result._ZApp := Self.ZApp;
end;



function TZComponent.DoClone(ObjIds,FixUps : TZArrayList; Into : TZComponent): TZComponent;
var
  PropList : TZPropertyList;
  Prop : TZProperty;
  Value,Tmp : TZPropertyValue;
  I : integer;

  procedure InCloneList(List,DestList : TZComponentList);
  var
    I : integer;
    C : TZComponent;
  begin
    for I := 0 to List.Count-1 do
    begin
      C := List.GetComponent(I);
      DestList.AddComponent( C.DoClone(ObjIds,FixUps,nil) );
    end;
  end;

begin
  if Into<>nil then
    Result := Into
  else
    Result := TZComponentClass(Self.ClassType).Create(nil);
  ObjIds[ Self.ObjId ] := Result;
  PropList := GetProperties;
  for I := 0 to PropList.Count-1 do
  begin
    Prop := TZProperty(PropList[I]);
    if Prop.DontClone then
      Continue; //Skip properties like: objid, model.personality
    Value := GetProperty(Prop);
    if (Into<>nil) and Prop.IsDefaultValue(Value) then
      //Skip default values when cloning into.
      //This is because child models should keep their base model property values unless values are overriden (i.e. non-default).
      Continue;
    case Prop.PropertyType of
      zptComponentRef :
        begin
          Result.SetProperty(Prop,Value);
          if (Value.ComponentValue<>nil) and (Value.ComponentValue.ObjId<>0) then
            FixUps.Add( Result.GetPropertyPtr(Prop,0) );
        end;
      zptComponentList :
        begin
          Tmp := Result.GetProperty(Prop);
          InCloneList(Value.ComponentListValue,Tmp.ComponentListValue);
        end;
      zptExpression :
        begin
          Tmp := Result.GetProperty(Prop);
          InCloneList(Value.ExpressionValue.Code,Tmp.ExpressionValue.Code);
          {$ifndef minimal}
          //Also copy source if in designer
          Tmp.ExpressionValue.Source := Value.ExpressionValue.Source;
          Result.SetProperty(Prop,Tmp);
          {$endif}
        end;
      {$ifndef minimal}
      zptBinary :
        begin
          //Kopiera binary mem enbart i designer.
          //I minimal s� klonas binary genom att peka p� samma source
          GetMem(Tmp.BinaryValue.Data,Value.BinaryValue.Size);
          Tmp.BinaryValue.Size := Value.BinaryValue.Size;
          Move(Value.BinaryValue.Data^,Tmp.BinaryValue.Data^,Value.BinaryValue.Size);
          Result.SetProperty(Prop,Tmp);
        end;
      {$endif}
    else
      Result.SetProperty(Prop,Value);
    end;
  end;
  //This is needed because managed variables need to init their values (the values have DontClone set)
  Result.InitAfterPropsAreSet;
end;

procedure TZComponent.ResetGpuResources;
var
  PropList : TZPropertyList;
  Prop : TZProperty;
  Value : TZPropertyValue;
  I,J : integer;
begin
  PropList := GetProperties;
  for I := 0 to PropList.Count-1 do
  begin
    Prop := TZProperty(PropList[I]);
    case Prop.PropertyType of
      zptComponentList :
        begin
          Value := GetProperty(Prop);
          for J := 0 to Value.ComponentListValue.Count - 1 do
            (Value.ComponentListValue[J] as TZComponent).ResetGpuResources;
        end;
    end;
  end;
end;

{$ifndef minimal}
function TZComponent.GetDisplayName: AnsiString;
var
  S,Cn : AnsiString;
begin
  S := Self.Name;
  Cn := AnsiString(ComponentManager.GetInfo(Self).ZClassName);
  if Length(S)=0 then
    S := Cn
  else
    S := S + ' : ' + Cn;
  Result := S;
end;

procedure TZComponent.SetString(const PropName : string; const Value : AnsiString);
var
  P : TZProperty;
  Pv : TZPropertyValue;
begin
  P := Self.GetProperties.GetByName(PropName);
  Pv.StringValue := Value;
  Self.SetProperty(P,Pv);
end;

procedure TZComponent.DesignerReset;
var
  PropList : TZPropertyList;
  Prop : TZProperty;
  Value : TZPropertyValue;
  I : integer;
begin
  //Reset all components in componentlists
  //This will reset Timers etc.
  PropList := GetProperties;
  for I := 0 to PropList.Count-1 do
  begin
    Prop := TZProperty(PropList[I]);
    case Prop.PropertyType of
      zptComponentList :
        begin
          Value := GetProperty(Prop);
          Value.ComponentListValue.DesignerReset;
        end;
    end;
  end;
end;

function TZComponent.GetOwner : TZComponent;
begin
  if Assigned(OwnerList) then
    Result := OwnerList.Owner
  else
    Result := nil;
end;

procedure TZComponent.GetAllChildren(List : contnrs.TObjectList; IncludeExpressions : boolean);
//Returns all objects
var
  PropList : TZPropertyList;
  Prop : TZProperty;
  Value : TZPropertyValue;
  I,J : integer;
begin
  List.Add(Self);
  PropList := Self.GetProperties;
  for I := 0 to PropList.Count-1 do
  begin
    Prop := TZProperty(PropList[I]);
    case Prop.PropertyType of
      zptComponentList :
        begin
          Value := Self.GetProperty(Prop);
          for J := 0 to Value.ComponentListValue.ComponentCount-1 do
            TZComponent(Value.ComponentListValue[J]).GetAllChildren(List,IncludeExpressions);
        end;
      zptExpression :
        begin
          if IncludeExpressions then
          begin
            Value := Self.GetProperty(Prop);
            for J := 0 to Value.ExpressionValue.Code.ComponentCount-1 do
              TZComponent(Value.ExpressionValue.Code[J]).GetAllChildren(List,IncludeExpressions);
          end;
        end;
    end;
  end;
end;
{$endif}

function MakeColorf(const R,G,B,A : single) : TZColorf;
begin
  Result.R := R;
  Result.G := G;
  Result.B := B;
  Result.A := A;
end;



{ TZArrayList }

procedure TZArrayList.Add(Item: TObject);
begin
  if FCount = Capacity then
    Grow;
  List^[FCount] := Item;
  Inc(FCount);
end;

procedure TZArrayList.Clear;
var
  I : integer;
begin
  if ReferenceOnly then
    FCount := 0
  else
    for I := FCount - 1 downto 0 do
      RemoveAt(I);
end;

destructor TZArrayList.Destroy;
begin
  Clear;
  if List<>nil then
    FreeMem(List);
  inherited;
end;

{$ifdef debug}
procedure TZArrayList.CheckValidIndex(Index: Integer);
begin
  if (Index < 0) or (Index >= FCount) then
    ZHalt('ZArrayList bad index');
end;
{$endif}

function TZArrayList.GetItem(Index: Integer): TObject;
begin
  {$ifdef debug}CheckValidIndex(Index);{$endif}
  Result := List^[Index];
end;

function TZArrayList.GetPtrToItem(Index: Integer): pointer;
begin
  {$ifdef debug}CheckValidIndex(Index);{$endif}
  Result := @List^[Index];
end;

procedure TZArrayList.Swap(Index1,Index2 : integer);
var
  Tmp : TObject;
begin
  Tmp := List^[Index1];
  List^[Index1] := List^[Index2];
  List^[Index2] := Tmp;
end;

procedure TZArrayList.SetItem(Index: Integer; const Value: TObject);
begin
  {$ifdef debug}CheckValidIndex(Index);{$endif}
  List^[Index] := Value;
end;

procedure TZArrayList.Grow;
var
  Delta: Integer;
begin
  if Capacity > 64 then
    Delta := Capacity div 4
  else
    if Capacity > 8 then
      Delta := 16
    else
      Delta := 4;
  Inc(Capacity,Delta);
  ReallocMem(List, Capacity * SizeOf(Pointer));
end;

function TZArrayList.IndexOf(Item: TObject): Integer;
begin
  Result := 0;
  while (Result < FCount) and (List[Result] <> Item) do
    Inc(Result);
  if Result = FCount then
    Result := -1;
end;

procedure TZArrayList.RemoveAt(Index: integer);
var
  Temp: TObject;
begin
  {$ifdef debug}CheckValidIndex(Index);{$endif}
  Temp := List^[Index];
  Dec(FCount);
  if Index < FCount then
    System.Move(List^[Index + 1], List^[Index],
      (FCount - Index) * SizeOf(Pointer));
  if (not ReferenceOnly) and (Temp<>nil) then
    Temp.Free;
end;


function TZArrayList.Last: TObject;
begin
  Result := Items[ Count-1 ];
end;

procedure TZArrayList.Remove(Item: TObject);
begin
  RemoveAt( IndexOf(Item) );
end;

procedure TZArrayList.SwapRemove(Item: TObject);
begin
  SwapRemoveAt( IndexOf(Item) );
end;

procedure TZArrayList.SwapRemoveAt(Index: integer);
var
  Temp: TObject;
begin
  //Remove by replacing item at index with last and decreasing count.
  //Avoids system.move call.
  Temp := List^[Index];
  if (FCount>1) and (Index<>FCount-1) then
    List^[Index] := Last;
  Dec(FCount);
  if (not ReferenceOnly) and (Temp<>nil) then
    Temp.Free;
end;

function TZArrayList.Pop: TObject;
begin
  Result := Last;
  RemoveAt(Count-1);
end;

procedure TZArrayList.Push(Item: TObject);
begin
  Add(Item);
end;

constructor TZArrayList.CreateReferenced;
begin
  ReferenceOnly := True;
end;


{ TZComponentManager }

function TZComponentManager.GetInfo(Component: TZComponent) : TZComponentInfo;
var
  C : TZComponentClass;
  {$IFDEF MINIMAL}
  I : TZClassIds;
  Ci : TZComponentInfo;
  {$ENDIF}
begin
  //todo borde ligga i hashlist
  C := TZComponentClass(Component.ClassType);
  {$IFNDEF MINIMAL}
  Exit( InfoDictionary[C] );
  {$ELSE}
  for I := Low(ComponentInfos) to High(ComponentInfos) do
  begin
    Ci := ComponentInfos[I];
    if Ci=nil then
      Continue;
    if Ci.ZClass=C then
    begin
      Result := Ci;
      Exit;
    end;
  end;
  {$ENDIF}

  {$IFNDEF MINIMAL}
//  ZHalt('getinfocomponent returned nil:' + Component.ClassName);
  raise Exception.Create('getinfocomponent not found: ' + Component.ClassName);
  {$ELSE}
  Result := nil;
  {$ENDIF}
end;


function TZComponentManager.GetInfoFromId(ClassId: TZClassIds): TZComponentInfo;
begin
  Result := ComponentInfos[ClassId];
  {$IFNDEF MINIMAL}
  Assert(Result.ClassId=ClassId);
  {$ENDIF}
end;

{$IFNDEF MINIMAL}
function TZComponentManager.GetInfoFromName(const ZClassName: string): TZComponentInfo;
var
  I : TZClassIds;
  Ci : TZComponentInfo;
begin
  for I := Low(ComponentInfos) to High(ComponentInfos) do
  begin
    Ci := ComponentInfos[I];
    if Ci=nil then
      Continue;
    if SysUtils.SameText(Ci.ZClassName,ZClassName) then
    begin
      Result := Ci;
      Exit;
    end;
  end;
  raise Exception.Create('Class not found: ' + ZClassName);
end;

function TZComponentManager.GetInfoFromClass(const C: TZComponentClass): TZComponentInfo;
var
  I : TZClassIds;
  Ci : TZComponentInfo;
begin
  for I := Low(ComponentInfos) to High(ComponentInfos) do
  begin
    Ci := ComponentInfos[I];
    if Ci=nil then
      Continue;
    if Ci.ZClass=C then
    begin
      Result := Ci;
      Exit;
    end;
  end;
  raise Exception.Create('Class not found: ' + C.ClassName);
end;
{$ENDIF}

procedure TZComponentManager.Register(C: TZComponentClass; ClassId : TZClassIds);
var
  Ci : TZComponentInfo;
begin
  Ci := TZComponentInfo.Create;
  Ci.ZClass := C;
  Ci.ClassId := ClassId;
  {$IFNDEF MINIMAL}
  Ci.ZClassName := Copy(C.ClassName,2,100);
  if InfoDictionary=nil then
    InfoDictionary := TDictionary<TClass,TZComponentInfo>.Create;
  InfoDictionary.Add(C,Ci);
  {$ENDIF}
  {$if (not defined(MINIMAL)) or defined(zzdc_activex)}
  LastAdded := Ci;
  {$endif}
  ComponentInfos[Ci.ClassId] := Ci;
end;

{$IFNDEF MINIMAL}
destructor TZComponentManager.Destroy;
var
  I : TZClassIds;
  Ci : TZComponentInfo;
begin
  for I := Low(ComponentInfos) to High(ComponentInfos) do
  begin
    Ci := ComponentInfos[I];
    if (Ci=nil) then
      Continue;
    Ci.Properties.Free;
    Ci.Free;
  end;
  {$IFNDEF MINIMAL}
  InfoDictionary.Free;
  {$ENDIF}
  inherited;
end;

//Stream component and all children to a binary stream
//Result can be cast to TMemoryStream
function TZComponentManager.SaveBinaryToStream(Component : TZComponent) : TObject;
var
  Stream : TZOutputStream;
  Writer : TZWriter;
begin
  Stream := TZOutputStream.Create;
  Writer := TZBinaryWriter.Create(Stream);
  try
    Writer.WriteRootComponent(Component);
  finally
    Writer.Free;
  end;
  Result := Stream;
end;

//Stream component and all children to a xml stream
//Result can be cast to TMemoryStream
function TZComponentManager.SaveXmlToStream(Component: TZComponent) : TObject;
var
  Stream : TZOutputStream;
  Writer : TZWriter;
begin
  Stream := TZOutputStream.Create;
  Writer := TZXmlWriter.Create(Stream);
  try
    Writer.WriteRootComponent(Component);
  finally
    Writer.Free;
  end;
  Result := Stream;
end;


procedure TZComponentManager.SaveXml(Component: TZComponent; FileName: string);
var
  Stream : TZOutputStream;
begin
  ZLog.GetLog(Self.ClassName).Write('Saving: ' + FileName);
  Stream := SaveXmlToStream(Component) as TZOutputStream;
  try
    Stream.SaveToFile(FileName);
  finally
    Stream.Free;
  end;
end;

function TZComponentManager.LoadXmlFromFile(FileName: string): TZComponent;
var
  Reader : TZXmlReader;
begin
  Reader := TZXmlReader.Create;
  try
    Reader.LoadFromFile(FileName);
    Result := Reader.ReadRootComponent;
  finally
    Reader.Free;
  end;
end;

function TZComponentManager.LoadXmlFromString(const XmlData: string; SymTab : TSymbolTable): TZComponent;
var
  Reader : TZXmlReader;
begin
  Reader := TZXmlReader.Create;
  try
    Reader.LoadFromString(XmlData,SymTab);
    Result := Reader.ReadRootComponent;
  finally
    Reader.Free;
  end;
end;

function TZComponentManager.GetAllInfos: PComponentInfoArray;
begin
  Result := @ComponentInfos;
end;

function TZComponentManager.LoadBinaryFromFile(const FileName : string) : TZComponent;
var
  Ms : TMemoryStream;
  Stream : TZInputStream;
  Reader : TZBinaryReader;
begin
  Ms := TMemoryStream.Create;
  Ms.LoadFromFile(FileName);
  Ms.Position := 0;

  Stream := TZInputStream.CreateFromMemory(Ms.Memory, Ms.Size);
  Reader := TZBinaryReader.Create(Stream);
  Result := Reader.ReadRootComponent;
  Reader.Free;
  Stream.Free;

  Ms.Free;
end;
{$ENDIF}

function TZComponentManager.LoadBinary: TZComponent;
var
  Stream : TZInputStream;
  Reader : TZBinaryReader;

  {$IFNDEF MSWINDOWS}
  function InLoadPiggyback : TZInputStream;
  var
    FileName : PAnsiChar;
    DataSize,Magic : integer;
    Stream : TZInputStream;
  begin
    Result := nil;
    Filename := Platform_GetExeFileName;
    Stream := TZInputStream.CreateFromFile(FileName,False);
    if Stream.Size<=0 then
      Exit;
    Stream.Position := Stream.Size - 8;
    Stream.Read(DataSize,4);
    Stream.Read(Magic,4);
    if (Magic<>$01020304) then
      Exit;
    //Set position to start of stream
    Stream.Position := Stream.Size - 8 - DataSize;
    //Problem with verifying piggyback: binaryreader is dependent on stream size
    //Remove magic nr from stream end
    Dec(Stream.Size,4);
    Result := Stream;
  end;
  {$ENDIF}

begin
  //First check linked data, returns nil if not present
  Stream := Platform_LoadLinkedResource;
  //Second: check piggyback data
  {$IFNDEF MSWINDOWS}
  if Stream=nil then
    Stream := InLoadPiggyback;
  {$ENDIF}
  //Last try: load from file
  if Stream=nil then
    Stream := TZInputStream.CreateFromFile('zzdc.dat',True);
  {$ifdef zlog}
  if (Stream=nil) or (Stream.Size=0) then
    ZHalt('no data');
  {$endif}

  Reader := TZBinaryReader.Create(Stream);
  Result := Reader.ReadRootComponent;
  Reader.Free;
  Stream.Free;
end;

function TZComponentManager.GetProperties(Component: TZComponent): TZPropertyList;
var
  Ci : TZComponentInfo;
begin
  //Returnerar propertylista f�r en komponent
  //Listan ligger i classinfon, initieras f�rsta g�ngen genom att anropa component.defineproperties
  //Listan kan ej skapas vid klassregistrering f�r att prop-adressoffsets endast kan ber�knas n�r instans finns.
  //Den h�r metoden �r private, App-kod ska anropa c.GetProperties
  Ci := GetInfo(Component);
  Result := Ci.Properties;
  if Result=nil then
  begin
    Result := TZPropertyList.Create;
    Result.TheSelf := NativeInt(Component);
    Component.DefineProperties(Result);
    Ci.Properties := Result;
  end
  {$if (not defined(MINIMAL)) or defined(zzdc_activex)}
  else if Ci.HasGlobalData then
  begin
    //Components that use global variables must be single instance
    //and redefines their properties each time (AudioMixer).
    Result.TheSelf := NativeInt(Component);
    Result.Clear;
    Result.NextId := 0;
    Component.DefineProperties(Result);
  end
  {$endif};
end;


{ TZPropertyList }

function TZPropertyList.GetLast;
begin
  Result := TZProperty(Self.Last);
end;

{$IFNDEF MINIMAL}
procedure TZPropertyList.SetDesignerProperty;
begin
  //S�tt senaste prop till bara ska anv�ndas i designer (t.ex. Name)
  GetLast.ExcludeFromBinary := True;
  GetLast.IsReadOnly := True;
  //Avallokera senaste id, dessa m�ste vara konstanta f�r alla bin�rprops
  Dec(NextId);
end;

function TZPropertyList.GetByName(const Name: string): TZProperty;
var
  I : integer;
begin
  for I := 0 to Count-1 do
  begin
    Result := TZProperty(Self[I]);
    if SameText(Result.Name,Name) then
      Exit;
  end;
  Result := nil;
end;

//Returnerar den f�rsta propertyn av en viss typ
function TZPropertyList.GetByType(Kind: TZPropertyType): TZProperty;
var
  I : integer;
begin
  for I := 0 to Count-1 do
  begin
    Result := TZProperty(Self[I]);
    if Result.PropertyType = Kind then
      Exit;
  end;
  Result := nil;
end;
{$ENDIF}

function TZPropertyList.GetById(PropId: integer): TZProperty;
var
  I : integer;
begin
  for I := 0 to Count-1 do
  begin
    Result := TZProperty(Self[I]);
    if Result.PropId=PropId then
      Exit;
  end;
  Result := nil;
end;

procedure TZPropertyList.AddProperty({$IFNDEF MINIMAL}const Name: string;{$ENDIF} const Addr: pointer; const PropType : TZPropertyType);
var
  P : TZProperty;
begin
  P := TZProperty.Create;
  P.PropertyType := PropType;
  P.Offset := NativeInt(Addr)-Self.TheSelf;

  P.PropId := NextId;
  Inc(NextId);
  {$IFNDEF MINIMAL}
  P.Name := Name;
  Assert( ((P.Offset>=0) and (P.Offset<32768)) );
  {$ENDIF}
  Self.Add(P);
end;

procedure TZPropertyList.AddGlobalDataProperty({$IFNDEF MINIMAL}const Name: string;{$ENDIF} const Addr: pointer; const PropType : TZPropertyType);
var
  P : TZProperty;
begin
  P := TZProperty.Create;
  P.PropertyType := PropType;
  P.GlobalData := Addr;

  P.PropId := NextId;
  Inc(NextId);
  {$IFNDEF MINIMAL}
  P.Name := Name;
  {$ENDIF}
  Self.Add(P);
end;

//No writers are included in minimal runtime binary

{ TZWriter }

{$IFNDEF MINIMAL}
procedure WriteVarLength(Stream : TStream; Value: integer);
var
  B : byte;
  W : word;
begin
  if Value<255 then
  begin //one byte
    B := Value;
    Stream.Write(B,1);
  end else
  begin //Larger than 255, write using three bytes
    Assert(Value<High(Word));
    B := 255;
    Stream.Write(B,1);
    W := Value;
    Stream.Write(W,2);
  end;
end;

constructor TZWriter.Create(Stream: TZOutputStream);
begin
  Self.Stream := Stream;
end;

procedure TZWriter.Write(const B; Count: integer);
begin
  Stream.Write(B,Count);
end;

procedure TZWriter.WriteRootComponent(C: TZComponent);
begin
  Root := C;
  OnDocumentStart;
  DoWriteComponent(C);
  OnDocumentEnd;
end;

{ TZBinaryWriter }

procedure TZBinaryWriter.DoWriteComponent(C: TZComponent);
var
  Ci : TZComponentInfo;
  B : byte;
  PropList : TZPropertyList;
  Value : TZPropertyValue;
  I,J,Temp : integer;
  Prop : TZProperty;
  PStream : TStream;
  AfterList : TZArrayList;

  procedure WriteNulls(Stream : TStream; Count : integer);
  var
    I : integer;
    B : byte;
  begin
    B := 0;
    for I := 0 to Count-1 do
      Stream.Write(B,1);
  end;

  procedure WriteScalar(Stream : TStream; F : single);
  var
    B : byte;
  begin
    //Assert( (F>=0) and (F<=1.0) );
    //todo: warn if out of range
    F := Clamp(F,0,1);
    B := Trunc(F*255);
    Stream.Write(B,1);
  end;

  procedure InWriteList(List : TZComponentList);
  var
    I,Count : integer;

    function InCountOneList(List : TZComponentList) : integer;
    var
      C : TZComponent;
      I : integer;
    begin
      Result := List.Count;
      for I := 0 to List.Count-1 do
      begin
        C := List.GetComponent(I);
        if ComponentManager.GetInfo(C).ExcludeFromBinary then
          Dec(Result);
        {if (C is TLogicalGroup) and (C<>Root) then
          //ParentComponent is ignored in stream
          Inc(Result, InCountOneList( (C as TLogicalGroup).Children ) );}
      end;
    end;

  begin
    Count := InCountOneList(List);
    WriteVarLength(Self.Stream,Count);
    if Count>0 then
      for I:=0 to List.Count-1 do
        //Try to write all, DoWriteComponent return directly if ExcludeFromBinary
        DoWriteComponent(List.GetComponent(I));
  end;

begin
  {if (C is TLogicalGroup) and (C<>Root) then
  begin //ParentComponent is ignored in stream
    for I := 0 to (C as TLogicalGroup).Children.Count-1 do
      DoWriteComponent( (C as TLogicalGroup).Children.GetComponent(I) );
    Exit;
  end;}

  Ci := ComponentManager.GetInfo(C);
  Assert(Ord(Ci.ClassId)<127);

  if Ci.ClassId=ExpIDEFuncCallClassId then
    ZHalt('Script using IDE-functions (createcomponent etc) cannot be written to binary.');

  if Ci.ExcludeFromBinary then
    Exit;

  //First byte: Classid
  B := Ord(Ci.ClassId);
  Write(B,1);

  //write properties
  PropList := C.GetProperties;
  AfterList := TZArrayList.CreateReferenced;
  Stream.BitsBegin;
  for I := 0 to PropList.Count-1 do
  begin
    Prop := TZProperty(PropList[I]);
    if Prop.ExcludeFromBinary or Prop.NeverPersist then
      Continue;
    Value := C.GetProperty(Prop);
    PStream := PStreams[Prop.PropertyType];
    if Prop.IsDefaultValue(Value) then
    begin
      //Prop has default value, write 0 in bitmask and skip to next
      Stream.WriteBit(False);
      Continue;
    end;
    Stream.WriteBit(True);
    case Prop.PropertyType of
      zptString :
        begin
          //Write null-terminated string
          Temp := Length(Value.StringValue);
          if Temp>0 then
            for J := 1 to Temp do
            begin
              B := Ord(Value.StringValue[J]) xor $aa;
              PStream.Write(B,1);
            end;
          B := 0;
          PStream.Write(B,1);
        end;
      zptFloat :
        begin
          PStream.Write(Value.FloatValue,4);
        end;
      zptScalar :
        WriteScalar(PStream,Value.FloatValue);
      zptRectf :
        PStream.Write(Value.RectfValue,SizeOf(TZRectf));
      zptColorf :
        for J := 0 to 3 do
          WriteScalar(PStream,Value.ColorfValue.V[J]);
      zptInteger :
        PStream.Write(Value.IntegerValue,SizeOf(integer));
      zptComponentRef :
        if Value.ComponentValue=nil then
          //todo: should not need to test for nil, n�r vi har defaultfiltrering
          WriteNulls(PStream,4)
        else
          PStream.Write(Value.ComponentValue.ObjId,4);
      zptVector3f :
        PStream.Write(Value.Vector3fValue,SizeOf(TZVector3f));
      zptByte :
        PStream.Write(Value.ByteValue,SizeOf(byte));
      zptBoolean :
        PStream.Write(Value.BooleanValue,SizeOf(ByteBool));
      zptComponentList,zptExpression :
        AfterList.Add(Prop);
      zptBinary :
        begin
          PStream.Write(Value.BinaryValue.Size,SizeOf(Value.BinaryValue.Size));
          if Value.BinaryValue.Size>0 then
            PStream.Write(Value.BinaryValue.Data^,Value.BinaryValue.Size);
        end
    else
      ZHalt('TZBinaryWriter: No writehandler');
    end;
  end;
  Stream.BitsEnd;
  //Skriv n�stlade componenter efter�t s� att alla propbits
  //hamnar i main-stream f�rst.
  for I := 0 to AfterList.Count-1 do
  begin
    Prop := TZProperty(AfterList[I]);
    Value := C.GetProperty(Prop);
    case Prop.PropertyType of
      zptComponentList :
        InWriteList(Value.ComponentListValue);
      zptExpression :
        InWriteList(Value.ExpressionValue.Code);
    else
      ZHalt('TZBinaryWriter: No writehandler');
    end;
  end;
  AfterList.Free;
end;

procedure TZBinaryWriter.OnDocumentEnd;
var
  I : integer;
  P : TZPropertyType;
  PStream : TMemoryStream;
  PSizes : packed array[TZPropertyType] of integer;
begin
  //Appenda alla propertystreams, samt g�r free p� dessa
  FillChar(PSizes,SizeOf(PSizes),0);
  for P := Low(TZPropertyType) to High(TZPropertyType) do
    if not (P in TBinaryNested) then
    begin
      PStream := PStreams[P];
      PStream.Position := 0;
      PSizes[P]:=PStream.Size;
      if PStream.Size>0 then
        Write(PStream.Memory^,PStream.Size);
      PStream.Free;
    end;
  //Skriv dictionary med sizes f�r varje pstream
  Stream.Write(PSizes,SizeOf(PSizes));
  //Write size of data last, this is used for piggybacking
  I := Stream.Size;
  Write(I,4);
  //Remove assigned object ids
  for I := 0 to AssignedObjs.Count-1 do
    TZComponent(AssignedObjs[I]).ObjId := 0;
  AssignedObjs.Free;
end;

procedure TZBinaryWriter.OnDocumentStart;
var
  NextObjId : integer;

  procedure InGiveObjIds(C : TZComponent);
  var
    PropList : TZPropertyList;
    Prop : TZProperty;
    Value : TZPropertyValue;
    I,J : integer;

    procedure InGiveOne(C : TZComponent);
    begin
      if (C=nil) or (C.ObjId<>0) then
        Exit;
      Inc(NextObjId);
      C.ObjId := NextObjId;
      AssignedObjs.Add(C);
    end;

  begin
    {if (C is TLogicalGroup) and (C<>Root) then
    begin //ParentComponent is ignored in stream
      for I := 0 to (C as TLogicalGroup).Children.Count-1 do
        InGiveObjIds( (C as TLogicalGroup).Children.GetComponent(I) );
      Exit;
    end;}
    PropList := C.GetProperties;
    for I := 0 to PropList.Count-1 do
    begin
      Prop := TZProperty(PropList[I]);
      case Prop.PropertyType of
        zptComponentRef :
          begin
            Value := C.GetProperty(Prop);
            InGiveOne(Value.ComponentValue);
          end;
        zptComponentList :
          begin
            Value := C.GetProperty(Prop);
            for J := 0 to Value.ComponentListValue.ComponentCount-1 do
              InGiveObjIds(Value.ComponentListValue.GetComponent(J));
          end;
        zptExpression :
          begin
            Value := C.GetProperty(Prop);
            for J := 0 to Value.ExpressionValue.Code.ComponentCount-1 do
              InGiveObjIds(Value.ExpressionValue.Code.GetComponent(J));
          end;
      else
        Continue;
      end;
    end;
  end;

  procedure InCreatePStreams;
  var
    P : TZPropertyType;
  begin
    for P := Low(TZPropertyType) to High(TZPropertyType) do
      if not (P in TBinaryNested) then
        PStreams[P] := TZOutputStream.Create;
  end;

begin
  AssignedObjs := TZArrayList.Create;
  AssignedObjs.ReferenceOnly := True;
  NextObjId := 0;
  InGiveObjIds(Root);
  InCreatePStreams;
end;


{ TZXmlWriter }

procedure TZXmlWriter.DoWriteComponent(C: TZComponent);
var
  Ci : TZComponentInfo;
  PropList : TZPropertyList;
  Value : TZPropertyValue;
  I,J : integer;
  Prop : TZProperty;
  S,V : string;
  NormalProps,NestedProps : TObjectList;

  function InFloat(F : single) : string;
  begin
    Result := FloatToStr( RoundTo( F ,-FloatTextDecimals) );
  end;

  function InArray(const A : array of single) : string;
  var
    I : integer;
    S : string;
  begin
    S := '';
    for I := 0 to High(A) do
      S:=S + InFloat(A[I]) + ' ';
    Result := Trim(S);
  end;

  function InAttrValue(const S : ansistring) : string;
  begin
    Result := String(S);
    Result := StringReplace(Result,'&','&amp;',[rfReplaceAll]);
    Result := StringReplace(Result,'"','&quot;',[rfReplaceAll]);
    Result := StringReplace(Result,'<','&lt;',[rfReplaceAll]);
    Result := StringReplace(Result,'>','&gt;',[rfReplaceAll]);
    Result := StringReplace(Result,'''','&apos;',[rfReplaceAll]);
  end;

  function InGetBinary(const BinaryValue : TZBinaryPropValue) : string;
  var
    Zs : TCompressionStream;
    Mem : TMemoryStream;
  begin
    Mem := TMemoryStream.Create;
    try
      Zs := TCompressionStream.Create(clDefault,Mem);
      try
        Zs.Write(BinaryValue.Data^,BinaryValue.Size)
      finally
        Zs.Free;
      end;
      Mem.Position:=0;
      SetLength(Result,Mem.Size*2);
      Classes.BinToHex(PAnsiChar(Mem.Memory),{$ifdef fpc}PAnsiChar(Result){$else}PWideChar(Result){$endif},Mem.Size);
    finally
      Mem.Free;
    end;
  end;

  function SafeCdata(const S : ansistring) : string;
  begin
    Result := String(S);
    //Cdata cannot contain ']]>' string
    if Pos(']]>',Result)>0 then
      //As recommended here: http://en.wikipedia.org/wiki/CDATA
      //Result := StringReplace(S,']]>',']]]]><![CDATA[>',[rfReplaceAll])
      //This is simpler to parse
      Result := StringReplace(Result,']]>',']] >',[rfReplaceAll])
  end;

begin
  Ci := ComponentManager.GetInfo(C);

  NormalProps := TObjectList.Create(False);
  NestedProps := TObjectList.Create(False);
  try
    //G� igenom props f�r att ta reda p� vilka som ska skrivas
    //Skilj p� props som skrivs som attributes, och de som skrivs nested som elements
    PropList := C.GetProperties;
    for I := 0 to PropList.Count-1 do
    begin
      Prop := TZProperty(PropList[I]);
      Value := C.GetProperty(Prop);
      if Prop.NeverPersist or Prop.ExcludeFromXml or Prop.IsDefaultValue(Value) then
        Continue;
      case Prop.PropertyType of
        zptString :
          {$ifndef fpc}
          if (AnsiStrings.AnsiPos(#10,Value.StringValue)=0) {and
            (Pos('<',Value.StringValue)=0) and
            (Pos('>',Value.StringValue)=0)} then
            NormalProps.Add(Prop)
          else
          {$endif}
            NestedProps.Add(Prop);
        zptComponentList :
          NestedProps.Add(Prop);
        zptBinary :
          if Value.BinaryValue.Size>0 then
            NestedProps.Add(Prop);
        zptExpression :
          if (Pos(#10,Value.ExpressionValue.Source)=0) then
            NormalProps.Add(Prop)
          else
            NestedProps.Add(Prop);
      else
        NormalProps.Add(Prop);
      end;
    end;

    S := '<' + Ci.ZClassName;

    for I := 0 to NormalProps.Count-1 do
    begin
      Prop := TZProperty(NormalProps[I]);
      Value := C.GetProperty(Prop);
      case Prop.PropertyType of
        zptString : V := InAttrValue( Value.StringValue );
        zptFloat,zptScalar :
          if IsNan(Value.FloatValue) then
          begin
            ZLog.GetLog(Self.ClassName).Warning('NaN value for property: ' + Prop.Name);
            V := '0';
          end
          else
            V := FloatToStr( RoundTo( Value.FloatValue ,-FloatTextDecimals) );
        zptRectf : V := InArray(Value.RectfValue.Area);
        zptColorf : V := InArray(Value.ColorfValue.V);
        zptInteger : V := IntToStr(Value.IntegerValue);
        zptComponentRef : V := String(Value.ComponentValue.Name);
        zptVector3f : V := InArray(Value.Vector3fValue);
        zptByte : V := IntToStr(Value.ByteValue);
        zptBoolean : V := IntToStr( byte(Value.BooleanValue) );
        zptExpression : V := InAttrValue( AnsiString(Value.ExpressionValue.Source) );
      else
        raise Exception.Create('TZXmlWriter: No writehandler ' + Prop.Name);
      end;
      S:=S + ' ' + Prop.Name + '="' + V + '"';
    end;

    if NestedProps.Count=0 then
    begin
      S := S + '/>';
      WriteLine(S);
    end
    else
    begin
      S := S + '>';
      WriteLine(S);
      for I := 0 to NestedProps.Count-1 do
      begin
        Prop := TZProperty(NestedProps[I]);
        Value := C.GetProperty(Prop);
        LevelDown;
        WriteLine('<' + Prop.Name + '>');
        case Prop.PropertyType of
          zptString :
            WriteString('<![CDATA[' + SafeCdata(Value.StringValue) + ']]>'#13#10);
          zptExpression :
            WriteString('<![CDATA[' + SafeCdata( AnsiString(Value.ExpressionValue.Source) ) + ']]>'#13#10);
          zptComponentList :
            begin
              LevelDown;
              for J:=0 to Value.ComponentListValue.Count-1 do
                DoWriteComponent(Value.ComponentListValue.GetComponent(J));
              LevelUp;
            end;
          zptBinary :
            begin
              S := InGetBinary(Value.BinaryValue);
              WriteString('<![CDATA[' + S + ']]>'#13#10);
            end;
        else
          raise Exception.Create('TZXmlWriter: No writehandler');
        end;
        WriteLine('</' + Prop.Name + '>');
        LevelUp;
      end;
      S := '</' + Ci.ZClassName + '>';
      if (C is TLogicalGroup) and (C.Name<>'') then
        S := S + ' <!-- ' + String(C.Name) + ' -->'#13#10;
      WriteLine(S);
    end;

  finally
    NormalProps.Free;
    NestedProps.Free;
  end;
end;

procedure TZXmlWriter.LevelDown;
begin
  Inc(IndentLevel);
end;

procedure TZXmlWriter.LevelUp;
begin
  Dec(IndentLevel);
end;

procedure TZXmlWriter.OnDocumentEnd;
begin
  FormatSettings.DecimalSeparator := Self.OldSeparator;
end;

procedure TZXmlWriter.OnDocumentStart;
begin
  WriteLine('<?xml version="1.0" encoding="iso-8859-1" ?>');
  Self.OldSeparator := FormatSettings.DecimalSeparator;
  FormatSettings.DecimalSeparator := '.';
end;

procedure TZXmlWriter.WriteString(const S: string);
var
  A : ansistring;
begin
  A := AnsiString(S);
  Write(A[1],Length(A));
end;

procedure TZXmlWriter.WriteLine(const S: string);
var
  Spaces : string;
  I : integer;
begin
  if Length(S)>0 then
  begin
    if IndentLevel>0 then
    begin
      SetLength(Spaces,IndentLevel*2);
      for I := 1 to Length(Spaces) do
        Spaces[I] := ' ';
      WriteString(Spaces);
    end;
    WriteString(S);
    WriteString(#13#10);
  end;
end;

{$ENDIF}

{ TZProperty }

{$IFNDEF MINIMAL}
constructor TZProperty.Create;
begin
  Self.ReturnType.Kind := zctVoid;
end;

procedure TZProperty.SetChildClasses(const C: array of TZComponentClass);
var
  I : integer;
begin
  SetLength(ChildClasses,Length(C));
  for I := 0 to Length(C)-1 do
    ChildClasses[I] := C[I];
end;

procedure TZProperty.SetOptions(const O: array of string);
var
  I : integer;
begin
  SetLength(Options,Length(O));
  for I := 0 to Length(O)-1 do
    Options[I] := O[I];
end;

{$ENDIF}

function TZProperty.IsDefaultValue(const Value: TZPropertyValue): boolean;
begin
  Result := False;
  //true ifall value.equals(self.defaultvalue)
  //eller null. d�p om till ShouldStreamValue?
  case PropertyType of
    zptString : Result := {$ifdef minimal}Value.StringValue=nil;{$else}Value.StringValue=DefaultValue.StringValue;{$endif}
    zptByte : Result := Value.ByteValue=DefaultValue.ByteValue;
    zptInteger : Result := Value.IntegerValue=DefaultValue.IntegerValue;
    zptComponentRef : Result := Value.ComponentValue=nil;
    zptComponentList : Result := Value.ComponentListValue.Count=0;
    zptBoolean : Result := Value.BooleanValue=DefaultValue.BooleanValue;
    zptColorf : Result := ZMath.VecIsEqual4( TZVector4f(Value.ColorfValue),TZVector4f(DefaultValue.ColorfValue));
    zptVector3f : Result := ZMath.VecIsEqual3(Value.Vector3fValue,DefaultValue.Vector3fValue);
    zptFloat,zptScalar : Result := Value.FloatValue=DefaultValue.FloatValue;
    zptRectf : Result := ZMath.VecIsEqual4( TZVector4f(Value.RectfValue),TZVector4f(DefaultValue.RectfValue));
    zptExpression:
      Result := {$ifdef minimal}Value.ExpressionValue.Code.Count=0;
        {$else}
          (Trim(Value.ExpressionValue.Source)=Trim(DefaultValue.ExpressionValue.Source)) or
          (Trim(Value.ExpressionValue.Source)='');
        {$endif}
    zptBinary : Result := Value.BinaryValue.Size=0;
    zptPointer : Result := Value.PointerValue=nil;
  end;
end;

{ TZInputStream }

constructor TZInputStream.CreateFromFile(FileName: PAnsiChar; IsRelative : Boolean);
begin
  Platform_ReadFile(FileName,pointer(Memory),Size,IsRelative);
  OwnsMemory := True;
end;

constructor TZInputStream.CreateFromMemory(M: pointer; Size: integer);
begin
  Self.Memory := M;
  Self.Size := Size;
end;

destructor TZInputStream.Destroy;
begin
  if (Memory<>nil) and (OwnsMemory) then
    FreeMem(Memory);
  inherited;
end;

function TZInputStream.GetMemory: PBytes;
begin
  Result := @Memory^[Position]
end;

procedure TZInputStream.Read(var Buf; Count: integer);
begin
  if Position+Count>Size then
  begin
    {$ifdef zlog}
    ZLog.GetLog(Self.ClassName).Write('Read beyond EOF attempted');
    {$endif}
    Exit;
  end;
  System.Move(Memory^[Position],Buf,Count);
  Inc(Position,Count);
end;

function TZInputStream.ReadBit: boolean;
begin
  {$ifdef zdebug}
  Assert(IsBitMode);
  {$endif}
  Inc(BitNo);
  if BitNo=8 then
  begin
    BitNo:=0;
    Read(Bits,1);
  end;
  Result := (Bits and (1 shl BitNo))<>0;
end;

procedure TZInputStream.BitsBegin;
begin
  {$ifdef zdebug}
  Assert(not IsBitMode);
  IsBitMode := True;
  {$endif}
  BitNo := 7;
end;

procedure TZInputStream.BitsEnd;
begin
  {$ifdef zdebug}
  Assert(IsBitMode);
  IsBitMode := False;
  {$endif}
end;

{ TZReader }


function TZReader.ReadRootComponent: TZComponent;
begin
  OnDocumentStart;
  Result := DoReadComponent(nil);
  OnDocumentEnd;
end;

{ TZBinaryReader }

function ReadVarLength(Stream : TZInputStream) : integer;
var
  B : byte;
  W : word;
begin
  Stream.Read(B,1);
  if B=255 then
  begin //List count is one or three bytes
    Stream.Read(W,2);
    Result := W;
  end else
    Result := B;
end;

constructor TZBinaryReader.Create(Stream: TZInputStream);
begin
  Self.Stream := Stream;
end;

procedure TZBinaryReader.Read(var B; Count: integer);
begin
  Stream.Read(B,Count);
end;

function TZBinaryReader.DoReadComponent(OwnerList : TZComponentList) : TZComponent;
var
  C : TZComponent;
  Ci : TZComponentInfo;
  B : byte;
  ClassId : TZClassIds;
  PropList : TZPropertyList;
  Value : TZPropertyValue;
  I,J,Temp : integer;
  Prop : TZProperty;
  PStream : TZInputStream;
  AfterList : TZArrayList;

  function ReadScalar(PStream : TZInputStream) : single;
  begin
    PStream.Read(B,1);
    Result := B / 255.0;
  end;

  procedure InReadList(List : TZComponentList);
  var
    I,Count : integer;
  begin
    Count := ReadVarLength(Self.Stream);
(*    Read(B,1);
    if B=255 then
    begin //List count is one or two bytes
      Read(W,2);
      Count := W;
    end else
      Count := B;*)
    for I := 0 to Count-1 do
      DoReadComponent(List);
  end;

begin
  //First byte: Classid
  Read(B,1);

  ClassId := TZClassIds(B);

  Ci := ComponentManager.GetInfoFromId(ClassId);
  C := Ci.ZClass.Create(OwnerList);

  //read properties
  PropList := C.GetProperties;
  AfterList := TZArrayList.CreateReferenced;
  Stream.BitsBegin;
  for I := 0 to PropList.Count-1 do
  begin
    Prop := TZProperty(PropList[I]);
    if Prop.NeverPersist then
      Continue;
    {$IFNDEF MINIMAL}
    if Prop.ExcludeFromBinary then
      Continue;
    {$ENDIF}
    //Read bitmask from main-stream, if zero then property
    //is not present in stream and has defaultvalue.
    if not Stream.ReadBit then
      Continue;
    PStream := PStreams[Prop.PropertyType];
    case Prop.PropertyType of
      zptString :
        begin
          //String is null-terminated
          Temp := ZStrLength(PAnsiChar(PStream.GetMemory));
          if Temp>0 then
          begin
            {$IFDEF MINIMAL}
            GetMem(Value.StringValue,Temp+1);
            PStream.Read(Value.StringValue^,Temp+1);
            for J := 0 to Temp-1 do
              PBytes(Value.StringValue)[J] := PBytes(Value.StringValue)[J] xor $aa;
            {$ELSE}
            SetLength(Value.StringValue,Temp);
            PStream.Read(Value.StringValue[1],Temp);
            {$ENDIF}
          end else
          begin
            {$ifdef MINIMAL}
            Value.StringValue := nil;
            {$endif}
          end;
        end;
      zptFloat :
        begin
          PStream.Read(Value.FloatValue,4);
        end;
      zptScalar :
        Value.FloatValue := ReadScalar(PStream);
      zptRectf :
        PStream.Read(Value.RectfValue,SizeOf(TZRectf));
      zptColorf :
        begin
          for J := 0 to 3 do
            Value.ColorfValue.V[J] := ReadScalar(PStream);
        end;
      zptInteger :
        PStream.Read(Value.IntegerValue,SizeOf(integer));
      zptByte :
        PStream.Read(Value.ByteValue,SizeOf(byte));
      zptBoolean :
        PStream.Read(Value.BooleanValue,SizeOf(byte));
      zptComponentRef :
        begin
          PStream.Read(Value.ComponentValue,4);
          if Value.ComponentValue<>nil then
            FixUps.Add( C.GetPropertyPtr(Prop,0) );
        end;
      zptVector3f :
        PStream.Read(Value.Vector3fValue,SizeOf(TZVector3f));
      zptComponentList,zptExpression :
        begin
          AfterList.Add(Prop);
          Continue;
        end;
      zptBinary :
       begin
         PStream.Read(Value.BinaryValue.Size,SizeOf(Value.BinaryValue.Size));
         if Value.BinaryValue.Size>0 then
         begin
           GetMem(Value.BinaryValue.Data,Value.BinaryValue.Size);
           PStream.Read(Value.BinaryValue.Data^,Value.BinaryValue.Size);
         end;
       end;
    {$ifdef zdebug}
    else
      ZHalt('TZBinaryReader: No readhandler');
    {$endif}
    end;
    C.SetProperty(Prop,Value);
  end;
  Stream.BitsEnd;

  //Ta n�stlade komponenter separat
  for I := 0 to AfterList.Count-1 do
  begin
    Prop := TZProperty(AfterList[I]);
    case Prop.PropertyType of
      zptComponentList :
        begin
          //l�s f�rst s� att vi f�r samma pekare (listan �gs av komponenten)
          Value := C.GetProperty(Prop);
          InReadList(Value.ComponentListValue);
        end;
      zptExpression :
        begin
          //l�s f�rst s� att vi f�r samma pekare (listan �gs av komponenten)
          Value := C.GetProperty(Prop);
          InReadList(Value.ExpressionValue.Code);
        end;
    {$ifdef zdebug}
    else
      ZHalt('TZBinaryReader: No readhandler');
    {$endif}
    end;
    C.SetProperty(Prop,Value);
  end;
  AfterList.Free;
  //If this has an objid, then it's a componentref target.
  //Store for fixups
  if C.ObjId>0 then
  begin
    while ObjIds.Count<=C.ObjId do
      ObjIds.Add(nil);
    ObjIds[C.ObjId] := C;
  end;

  C.InitAfterPropsAreSet;

  Result := C;
end;

procedure TZBinaryReader.OnDocumentEnd;
var
  I,ObjId : integer;
  P : PPointer;
  {$ifndef minimal}
  PropType : TZPropertyType;
  {$endif}
begin
  //component references
  for I := 0 to FixUps.Count-1 do
  begin
    P := PPointer(FixUps[I]);
    ObjId := integer(P^);
    P^ := ObjIds[ObjId];
  end;
  FixUps.Free;

  //nolla ut tilldelade objids s� att de kan anv�ndas runtime f�r clone
  for I := 0 to ObjIds.Count-1 do
    if ObjIds[I]<>nil then
      TZComponent(ObjIds[I]).ObjId:=0;

  ObjIds.Free;

  {$ifndef minimal}
  //g�r free p� pstreams
  for PropType := Low(Self.PStreams) to High(Self.PStreams) do
    Self.PStreams[PropType].Free;
  {$endif}
end;

procedure TZBinaryReader.OnDocumentStart;
var
  PSizes : packed array[TZPropertyType] of integer;
  OldPos : integer;
  P : TZPropertyType;
  PStream : TZInputStream;
  CurPos : integer;
begin
  OldPos := Stream.Position;
  CurPos := Stream.Size-4-SizeOf(PSizes);
  Stream.Position := CurPos;
  Stream.Read(PSizes,SizeOf(PSizes));
  //Loopa i omv�nd ordning och backa i stream
  //f�r att f� tag p� varje propstream.
  for P := High(PSizes) downto Low(PSizes) do
    if not (P in TBinaryNested) then
    begin
      if PSizes[P]>0 then
      begin
        Dec(CurPos,PSizes[P]);
        PStream := TZInputStream.CreateFromMemory(
          @Stream.Memory[CurPos],
          PSizes[P]
        );
        PStreams[P] := PStream;
      end;
    end;
  Stream.Position := OldPos;

  FixUps := TZArrayList.Create;
  FixUps.ReferenceOnly := True;
  ObjIds := TZArrayList.Create;
  ObjIds.ReferenceOnly := True;
end;

{ TZXmlReader }

{$IFNDEF MINIMAL}
constructor TZXmlReader.Create;
begin
  MainXml := TXmlParser.Create;
  FixUps := TZArrayList.Create;
  Self.OldSeparator := FormatSettings.DecimalSeparator;
  FormatSettings.DecimalSeparator := '.';
end;

procedure TZXmlReader.LoadFromFile(const FileName: string);
begin
  {$ifndef zgeviz}
  ZLog.GetLog(Self.ClassName).Write('Loading: ' + FileName);
  {$endif}
  ExternalSymTab := False;
  SymTab := TSymbolTable.Create;
  MainXml.LoadFromFile(FileName);
end;

procedure TZXmlReader.LoadFromString(const XmlData: string; SymTab : TSymbolTable);
begin
  ExternalSymTab := True;
  Self.SymTab := SymTab;
  //Use the global symbol table
  //Let the locals be defines in a local scope
  SymTab.PushScope;
  MainXml.LoadFromBuffer(PAnsiChar(AnsiString(XmlData)));;
end;

destructor TZXmlReader.Destroy;
begin
  FormatSettings.DecimalSeparator := Self.OldSeparator;
  MainXml.Free;
  FixUps.Free;
  if ExternalSymTab then
    SymTab.PopScope
  else
    SymTab.Free;
  inherited;
end;

function TZXmlReader.DoReadComponent(OwnerList: TZComponentList): TZComponent;
begin
  Result := XmlDoReadComponent(MainXml,OwnerList);
end;

function TZXmlReader.XmlDoReadComponent(Xml : TXmlParser; OwnerList: TZComponentList): TZComponent;
var
  ZClassName : string;
  C : TZComponent;
  Ci : TZComponentInfo;
  I,J : integer;
  PropList : TZPropertyList;
  Value : TZPropertyValue;
  Prop,NestedProp : TZProperty;
  S : string;
  L,NotFounds : TStringList;
  Fix : TZXmlFixUp;
  Found : boolean;
  SaveMainXml : TXmlParser;

  procedure InDecodeBinary(const HexS : string; var BinaryValue : TZBinaryPropValue);
  var
    CompMem,DecompMem : TMemoryStream;
    Zs : TDecompressionStream;
    Buf : array[0..1023] of byte;
    I : integer;
  begin
    CompMem := TMemoryStream.Create;
    DecompMem := TMemoryStream.Create;
    try
      CompMem.Size := Length(HexS) div 2;
      CompMem.Position := 0;
      Classes.HexToBin({$ifdef fpc}PAnsiChar(AnsiString(HexS)){$else}PChar(HexS){$endif},PAnsiChar(CompMem.Memory),CompMem.Size);
      Zs := TDecompressionStream.Create(CompMem);
      try
        I := Zs.Read(Buf,SizeOf(Buf));
        while I>0 do
        begin
          DecompMem.Write(Buf,I);
          I := Zs.Read(Buf,SizeOf(Buf));
        end;
        BinaryValue.Size := DecompMem.Size;
        GetMem(BinaryValue.Data,BinaryValue.Size);
        DecompMem.Position := 0;
        DecompMem.Read(BinaryValue.Data^,BinaryValue.Size);
      finally
        Zs.Free;
      end;
    finally
      CompMem.Free;
      DecompMem.Free;
    end;
  end;

  procedure PatchMaterialTextures;
  //Translate old-style texture settings to new MaterialTexture-component
  var
    S : ansistring;
    OtherXml : TXmlParser;
    procedure InFixTex(const Name : string);
    begin
      if NotFounds.Values[Name]<>'' then
        S := S + ansistring(Name) + '="' + ansistring(NotFounds.Values[Name]) + '" '
    end;
  begin
    if NotFounds.Values['Texture']='' then
      Exit;
    Prop := PropList.GetByName('Textures');
    Value := C.GetProperty(Prop);
    S := '<MaterialTexture Texture="' + ansistring(NotFounds.Values['Texture']) + '" ';
    InFixTex('TextureScale');
    InFixTex('TextureX');
    InFixTex('TextureY');
    InFixTex('TextureRotate');
    InFixTex('TextureWrapMode');
    InFixTex('TexCoords');
    S := S + '/>';
    OtherXml := TXmlParser.Create;

    OtherXml.LoadFromBuffer(PAnsiChar(S));
    OtherXml.Scan;
    XmlDoReadComponent(OtherXml,Value.ComponentListValue);

    if NotFounds.Values['Texture2']<>'' then
    begin
      OtherXml.LoadFromBuffer( PAnsiChar( AnsiString('<MaterialTexture Texture="' + NotFounds.Values['Texture2'] + '"/>') ));
      OtherXml.StartScan;
      OtherXml.Scan;
      XmlDoReadComponent(OtherXml,Value.ComponentListValue);
    end;
    if NotFounds.Values['Texture3']<>'' then
    begin
      OtherXml.LoadFromBuffer( PAnsiChar( AnsiString('<MaterialTexture Texture="' + NotFounds.Values['Texture3'] + '"/>') ));
      OtherXml.StartScan;
      OtherXml.Scan;
      XmlDoReadComponent(OtherXml,Value.ComponentListValue);
    end;

    OtherXml.Free;
  end;

  procedure PatchSoundSample;
  var
    S,SampleDataString : ansistring;
    OtherXml : TXmlParser;
    SampleFormat,SampleName : string;
  begin
    Xml.Scan;
    Assert(Xml.CurName='SampleData');
    Xml.Scan;
    SampleDataString := AnsiString(Xml.CurContent);
    Xml.Scan;
    Assert(Xml.CurName='SampleData');
    Xml.Scan;

    SampleName := Self.SymTab.MakeUnique('Sample');

    SampleFormat := NotFounds.Values['SampleFormat'];
    if SampleFormat='' then
      SampleFormat := '0';

    S := '<Sample Name="' + AnsiString(SampleName) + '">' +
      '<Producers>' +
      '<SampleImport SampleRate="' + AnsiString(NotFounds.Values['SampleRate']) + '" ' +
         'SampleFormat="' + AnsiString(SampleFormat) + '">' +
      '<SampleData>' +
      '<![CDATA[' + SampleDataString + ']]>' +
      '</SampleData>' +
      '</SampleImport>' +
      '</Producers>' +
      '</Sample>';

    Prop := PropList.GetByName('Sample');
    Value := C.GetProperty(Prop);

    OtherXml := TXmlParser.Create;
    OtherXml.LoadFromBuffer(PAnsiChar(S));
    OtherXml.Scan;
    Value.ComponentValue := XmlDoReadComponent(OtherXml,OwnerList);

    C.SetProperty(Prop,Value);

    OtherXml.Free;
  end;

  procedure PatchBitmapFromFile;
  begin
    if NotFounds.Values['IsJpegEncoded']='' then
      Exit;
    Value.ByteValue := 1;
    C.SetProperty(C.GetProperties.GetByName('FileFormat'),Value);
  end;

  function PatchOldPropRef(const S : string) : string;
  var
    C : char;
    SpaceCount,SpecialCount : integer;
    L  : TStringList;
  begin
    Result := S;
    if (Length(S)>0) and ( CharInSet(UpCase(S[1]),['A'..'Z']) ) then
    begin
      SpaceCount := 0; SpecialCount := 0;
      for C in S do
      begin
        if C=' ' then
          Inc(SpaceCount);
        if CharInSet(C,['.','+','-','*','/']) then
          Inc(SpecialCount);
      end;
      if (SpaceCount>0) and (SpaceCount<3) and (SpecialCount=0) then
      begin
        L := TStringList.Create;
        L.Delimiter := ' ';
        L.DelimitedText := S;
        if LowerCase(L[L.Count-1])='value' then
          L.Delete(L.Count-1)
        else if StrToIntDef(L[L.Count-1],99) in [0..3] then
          L[L.Count-1] := Copy('XYZW',StrToInt(L[L.Count-1])+1,1)
        else if (L.Count>1) and (SameText(L[L.Count-1],'Color') or SameText(L[L.Count-1],'Translate')
          or SameText(L[L.Count-1],'Rotate')
          or SameText(L[L.Count-1],'Scale')
          ) then
          L.Add('X');
        L.Delimiter := '.';
        Result := L.DelimitedText;
        GetLog(Self.ClassName).Write(String('Patched propref: ' + S + ' -> ' + Result) );
        L.Free;
      end;
    end;
  end;

  function PatchBitmapExp(const S : string) : string;
  var
    L : string;

    procedure One(const Name,NewName : string);
    begin
      if Pos(Name,L)=0 then
        Exit;
      Result := StringReplace(Result,Name,NewName,[rfReplaceAll,rfIgnoreCase]);
    end;

  begin
    Result := S;
    L := LowerCase(S);
    if Prop.ExpressionKind=ekiBitmap then
    begin
      One('this.x','x');
      One('this.y','y');
      One('this.pixel','pixel');
    end
    else if Prop.ExpressionKind=ekiMesh then
    begin
      One('this.v','v');
      One('this.n','n');
      One('this.c','c');
      One('this.texcoord','texcoord');
    end;
  end;

begin
  ZClassName := Xml.CurName;

  if ZClassName='DefineArray' then
    ZClassName := 'Array'
  else if ZClassName='DefineVariable' then
    ZClassName := 'Variable'
  else if ZClassName='DefineConstant' then
    ZClassName := 'Constant';

  SaveMainXml := Self.MainXml;
  Self.MainXml := Xml;

  Ci := ComponentManager.GetInfoFromName(ZClassName);
  C := Ci.ZClass.Create(OwnerList);

  L := TStringList.Create;
  NotFounds := TStringList.Create;
  try
    L.Delimiter := ' ';
    //read properties
    PropList := C.GetProperties;

    for I := 0 to Xml.CurAttr.Count-1 do
    begin
      S := Xml.CurAttr.Name(I);
      Found := False;
      for J := 0 to PropList.Count-1 do
      begin
        Prop := TZProperty(PropList[J]);
        if SameText(Prop.Name,S) then
        begin
          S := Xml.CurAttr.Value(I);
          Found := True;
          case Prop.PropertyType of
            zptString :
              Value.StringValue := AnsiString(S);
            zptFloat,zptScalar :
              Value.FloatValue := StrToFloatDef(S,0);
            zptRectf :
              begin
                L.DelimitedText := S;
                Value.RectfValue.Area[0] := StrToFloatDef(L[0],0);
                Value.RectfValue.Area[1] := StrToFloatDef(L[1],0);
                Value.RectfValue.Area[2] := StrToFloatDef(L[2],0);
                Value.RectfValue.Area[3] := StrToFloatDef(L[3],0);
              end;
            zptColorf :
              begin
                L.DelimitedText := S;
                Value.ColorfValue.V[0] := StrToFloatDef(L[0],0);
                Value.ColorfValue.V[1] := StrToFloatDef(L[1],0);
                Value.ColorfValue.V[2] := StrToFloatDef(L[2],0);
                Value.ColorfValue.V[3] := StrToFloatDef(L[3],0);
              end;
            zptVector3f :
              begin
                L.DelimitedText := S;
                Value.Vector3fValue[0] := StrToFloatDef(L[0],0);
                //Allow a single value to be specified, this is copied to all three elements
                //Used when switching type from float to vector3d (material.texturescale)
                if L.Count>1 then
                  Value.Vector3fValue[1] := StrToFloatDef(L[1],0)
                else
                  Value.Vector3fValue[1] := Value.Vector3fValue[0];
                if L.Count>2 then
                  Value.Vector3fValue[2] := StrToFloatDef(L[2],0)
                else
                  Value.Vector3fValue[2] := Value.Vector3fValue[0];
              end;
            zptInteger :
              Value.IntegerValue := StrToInt(S);
            zptByte :
              Value.ByteValue := StrToInt(S);
            zptBoolean :
              Value.BooleanValue := ByteBool(StrToInt(S));
            zptComponentRef :
              begin
                Fix := TZXmlFixUp.Create;
                Fix.Name := LowerCase(S);
                Fix.Prop := Prop;
                Fix.Obj := C;
                FixUps.Add( Fix );
              end;
            zptExpression :
              begin
                if C.HasZapp and (C.ZApp.FileVersion<2) then
                begin
                  if Prop.ExpressionKind in [ekiGetValue,ekiGetPointer] then
                    S := PatchOldPropRef(S)
                  else if (Prop.ExpressionKind in [ekiBitmap,ekiMesh]) then
                    S := PatchBitmapExp(S);
                end;
                Value.ExpressionValue.Source := String(S);
              end
          else
            ZHalt('TZXmlReader: No readhandler');
          end;
          C.SetProperty(Prop,Value);

          Break;
        end;
      end;
      if not Found then
        NotFounds.Values[S] := Xml.CurAttr.Value(I);
    end;

    if (NotFounds.Count>0) then
    begin
      if (ZClassName='Material') then
        PatchMaterialTextures;
      if (ZClassName='Sound') and (Xml.CurPartType=ptStartTag) then
        PatchSoundSample;
      if (ZClassName='BitmapFromFile') then
        PatchBitmapFromFile;
    end;

    if Xml.CurPartType=ptStartTag then
    begin
      while Xml.Scan do
        case Xml.CurPartType of
          ptStartTag :
            begin
              //Hantera n�stlade komponnenter
              //Det g�ller componentlists
              S := Xml.CurName;
              NestedProp:=nil;
              for I := 0 to PropList.Count-1 do
              begin
                Prop := TZProperty(PropList[I]);
                if SameText(Prop.Name,Xml.CurName) and
                  (Prop.PropertyType in [zptComponentList,zptString,zptExpression,zptBinary]) then
                begin
                  NestedProp := Prop;
                  Break;
                end;
              end;
              if NestedProp=nil then
                raise Exception.Create('TZXmlReader: Unknown nested property ' + Xml.CurName);
              Value := C.GetProperty(NestedProp);
              while Xml.Scan do
                case Xml.CurPartType of
                  ptStartTag,ptEmptyTag,ptCData  :
                    case NestedProp.PropertyType of
                      zptComponentList : DoReadComponent(Value.ComponentListValue);
                      zptString :
                        begin
                          Value.StringValue := AnsiString(Xml.CurContent);
                          C.SetProperty(NestedProp,Value);
                        end;
                      zptExpression :
                        begin
                          S := Trim(Xml.CurContent);
                          if Prop.ExpressionKind in [ekiBitmap,ekiMesh] then
                            S := PatchBitmapExp(S);
                          Value.ExpressionValue.Source := S;
                          C.SetProperty(NestedProp,Value);
                        end;
                      zptBinary :
                        begin
                          try
                            InDecodeBinary(String(Xml.CurContent),Value.BinaryValue);
                            C.SetProperty(NestedProp,Value);
                          except
                            ZLog.GetLog(Self.ClassName).Write('*** Failed to read binary property: ' + String(C.Name));
                          end;
                        end;
                    end;
                  ptEndTag :
                    if SameText(NestedProp.Name,Xml.CurName) then
                      Break;
                end;
            end;
          ptEndTag : Break;
        end;
    end;

  finally
    L.Free;
    NotFounds.Free;
  end;

  if C.Name<>'' then
    SymTab.Add(String(LowerCase(C.Name)),C);

  Self.MainXml := SaveMainXml;

  C.InitAfterPropsAreSet;

  Result := C;
end;

procedure TZXmlReader.OnDocumentEnd;
var
  I : integer;
  Fix :  TZXmlFixUp;
  Value : TZPropertyValue;
  C : TZComponent;
begin
  for I := 0 to FixUps.Count-1 do
  begin
    Fix := TZXmlFixUp(FixUps[I]);
    C := SymTab.LookUp(Fix.Name) as TZComponent;
    if not Assigned(C) then
    begin //Handle missing symbol
      if ExternalSymTab then
      begin
        //When copy/paste, allow unknown references. They will be nil.
        ZLog.GetLog(Self.ClassName).Write('Unknown reference: ' + Fix.Name);
        FillChar(Value,SizeOf(Value),0);
        Fix.Obj.SetProperty(Fix.Prop,Value);
        Continue;
      end
      else
        if C=nil then
          raise Exception.Create('Unknown reference: ' + Fix.Name);
    end;
    case Fix.Prop.PropertyType of
      zptComponentRef :
        Value.ComponentValue := C;
    end;
    Fix.Obj.SetProperty(Fix.Prop,Value);
  end;
end;

procedure TZXmlReader.OnDocumentStart;
begin
  while MainXml.Scan do
    if MainXml.CurPartType in [ptStartTag,ptEmptyTag] then
      Break;
end;
{$ENDIF}



{ TZComponentList }

procedure TZComponentList.Change;
begin
  IsChanged := True;
  if Owner<>nil then
    Owner.Change;
end;

function TZComponentList.ComponentCount: integer;
begin
  Result := Count;
end;

constructor TZComponentList.Create;
begin
  ReferenceOnly := True;
end;

constructor TZComponentList.Create(OwnerC: TZComponent);
begin
  Create;
  Self.Owner := OwnerC;
end;

destructor TZComponentList.Destroy;
begin
  Clear;
  inherited;
end;

procedure TZComponentList.ExecuteCommands;
var
  I : integer;
  C : TZComponent;
begin
  for I := 0 to Count-1 do
  begin
    C := TZComponent(Self[I]);
    {$ifndef minimal}
    if C.DesignDisable then
      Continue;
    {$endif}
    if C is TCommand then
      TCommand(C).Execute
    else
      //Call update on everything that isn't commands (expressions)
      C.Update;
    {$ifndef minimal}
    //Break after the producer that is marked as preview (in bitmap graph)
    if C=DesignerPreviewProducer then
      Break;
    {$endif}
  end;
end;

function TZComponentList.GetComponent(Index: integer): TZComponent;
begin
  Result := TZComponent(Self[Index]);
end;

procedure TZComponentList.AddComponent(Component: TZComponent);
begin
  Add(Component);
  Component.OwnerList := Self;
end;

procedure TZComponentList.RemoveComponent(Component: TZComponent);
begin
  Component.OwnerList := nil;
  Remove(Component);
end;

procedure TZComponentList.Update;
var
  I : integer;
  C : TZComponent;
begin
  for I := 0 to Count-1 do
  begin
    C := TZComponent(Self[I]);
    {$ifndef minimal}
    if C.DesignDisable then
      Continue;
    {$endif}
    C.Update;
  end;
end;

procedure TZComponentList.Clear;
var
  Instance: TZComponent;
begin
  //Destroy childcomponents
  while Count>0 do
  begin
    Instance := TZComponent(Last);
    //Because we know the index, the lines below are much faster than calling RemoveComponent.
    Instance.OwnerList := nil;
    RemoveAt(Count-1);
    Instance.Destroy;
  end;
end;

{$ifndef minimal}
procedure TZComponentList.DesignerReset;
var
  I : integer;
begin
  for I := 0 to Count-1 do
    TZComponent(Self[I]).DesignerReset;
end;

procedure TZComponentList.InsertComponent(Component: TZComponent; Index : integer);
var
  I : integer;
begin
  Component.OwnerList := Self;

  Add(nil);
  I := Count-1;
  while I>Index do
  begin
    Items[I] := Items[I-1];
    Dec(I);
  end;

  Items[ Index ] := Component;
end;
{$endif}



{ TLogicalGroup }

procedure TLogicalGroup.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}strChildren,{$ENDIF}@Children, zptComponentList);
end;

procedure TLogicalGroup.Execute;
begin
  Children.ExecuteCommands;
end;

procedure TLogicalGroup.Update;
begin
  inherited;
  Children.Update;
end;

{ TContent }


procedure TContent.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}strProducers,{$ENDIF}@Producers, zptComponentList);
end;

type
  TGlobalContent =
    record
      Content : TContent;
      Stack : TZArrayList;
    end;

var
  GlobalContent : TGlobalContent;
  {$ifndef minimal}
  RefreshDepth : integer;
  {$endif}

procedure TContent.RefreshFromProducers;
var
  Stack : TZArrayList;
  Save : TGlobalContent;
  {$ifndef minimal}
  NewContent : TContent;
  {$endif}
begin
  {$ifndef minimal}
  if Producers.Count>0 then
    ZLog.GetLog(Self.ClassName).BeginTimer;
  {$endif}

  Save := GlobalContent;

  Stack := TZArrayList.Create;
  {$ifndef minimal}
  try
    Inc(RefreshDepth);
  {$endif}
    Stack.ReferenceOnly := True;

    GlobalContent.Content := Self;
    GlobalContent.Stack := Stack;

    {$ifndef minimal}
    if HasZApp and Assigned(Self.ZApp.OnContentCacheUse) and Self.ZApp.OnContentCacheUse(Self,NewContent) then
      Stack.Add(NewContent)
    else
    begin
    {$endif}
      //Execute producers as commands
      //This way Repeat and Condition-statements can be used
      Producers.ExecuteCommands;
    {$ifndef minimal}
      if HasZApp and Assigned(Self.ZApp.OnContentCacheAdd) and (Stack.Count>0) then Self.ZApp.OnContentCacheAdd(Self,TContent(Stack[0]));
    end;
    {$endif}

    if Stack.Count>0 then
      CopyAndDestroy(TContent(Stack.Pop));
    while(Stack.Count>0) do
      Stack.Pop().Free;
  {$ifndef minimal}
  finally
    Dec(RefreshDepth);
  {$endif}
    IsChanged := False;
    Producers.IsChanged := False;
  Stack.Free;
  {$ifndef minimal}
  end;
  {$endif}

//  FillChar(GlobalContent,SizeOf(GlobalContent),0);
  GlobalContent := Save;

  {$if (not defined(minimal)) and (not defined(zgeviz))}
  if (Producers.Count>0) and Self.HasZApp and (not ZApp.DesignerIsRunning) and (RefreshDepth=0) then
    ZLog.GetLog(Self.ClassName).EndTimer('Refresh: ' + String(GetDisplayName));
  {$endif}
end;

///////////////////

{ TZOutputStream }

{$ifndef minimal}
procedure TZOutputStream.BitsBegin;
begin
  Assert(not IsBitMode);
  IsBitMode := True;
  BitNo := 0;
  Bits := 0;
end;

procedure TZOutputStream.BitsEnd;
begin
  Assert(IsBitMode);
  IsBitMode := False;
  if BitNo<>0 then
    Write(Bits,1);
end;

procedure TZOutputStream.WriteBit(B: boolean);
begin
  Assert(IsBitMode);
  if B then
    Bits := Bits or (1 shl BitNo)
  else
    Bits := Bits and (not (1 shl BitNo));
  Inc(BitNo);
  if BitNo=8 then
  begin
    Write(Bits,1);
    BitNo := 0;
    Bits := 0;
  end;
end;
{$endif}


{ TStateBase }

procedure TStateBase.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}strDefinitions,{$ENDIF}@Definitions, zptComponentList);
  List.AddProperty({$IFNDEF MINIMAL}strOnStart,{$ENDIF}@OnStart, zptComponentList);
  List.AddProperty({$IFNDEF MINIMAL}strOnUpdate,{$ENDIF}@OnUpdate, zptComponentList);
  List.AddProperty({$IFNDEF MINIMAL}strOnRender,{$ENDIF}@OnRender, zptComponentList);
  List.AddProperty({$IFNDEF MINIMAL}strOnLeave,{$ENDIF}@OnLeave, zptComponentList);
end;


//String functions

function ZStrFindEnd(P : PAnsiChar) : PAnsiChar;
begin
  while P^<>#0 do Inc(P);
  Result := P;
end;

function ZStrLength(P : PAnsiChar) : integer;
begin
  Result := ZStrFindEnd(P) - P;
end;

procedure ZStrCopy(P : PAnsiChar; const Src : PAnsiChar);
var
  Len : integer;
begin
  Len := ZStrLength(Src);
  System.Move(Src^,P^,Len+1);
end;

procedure ZStrCat(P : PAnsiChar; const Src : PAnsiChar);
begin
  P := ZStrFindEnd(P);
  ZStrCopy(P,Src);
end;

procedure ZStrConvertInt(const S : integer; Dest : PAnsiChar);
var
  Value : integer;
  Tmp : PAnsiChar;
  Buf : array[0..15] of ansichar;
begin
  Value := Abs(S);
  Tmp := @Buf[High(Buf)];
  Tmp^ := #0;
  Dec(Tmp);
  while (Value>9) and (Tmp>@Buf) do
  begin
    Tmp^:=AnsiChar(Value mod 10 + 48);
    Dec(Tmp);
    Value := Value div 10;
  end;
  Tmp^ := AnsiChar(Value + 48);
  if S<0 then
  begin
    Dec(Tmp);
    Tmp^ := '-';
  end;
  ZStrCopy(Dest,Tmp);
end;

function ZStrToInt(const Str : PAnsiChar) : integer;
var
  P : PAnsiChar;
  Neg : boolean;
begin
  Result := 0;
  P := Str;
  Neg := P^='-';
  if Neg then
    Inc(P);
  while P^<>#0 do
  begin
    Result := Result * 10 + byte(P^)-48;
    Inc(P);
  end;
  if Neg then
    Result := 0-Result;
end;

function ZStrPos(const SubStr,Str : PAnsiChar; const StartPos : integer) : integer;
var
  P,P1,SaveP : PAnsiChar;
begin
  Result := -1;
  {$ifndef minimal}
  ZAssert(StartPos<=ZStrLength(Str),'StrPos called with startpos>length');
  {$endif}
  P := Str + StartPos;
  while P^<>#0 do
  begin
    P1 := SubStr;
    if P^=P1^ then
    begin
      SaveP := P;
      repeat
        Inc(P); Inc(P1);
      until (P^<>P1^) or (P^=#0) or (P1^=#0);
      if P1^=#0 then
      begin
        Result := NativeInt(SaveP) - NativeInt(Str);
        Break;
      end;
    end else
      Inc(P);
  end;
end;

function ZStrCompare(P1,P2 : PAnsiChar) : boolean;
begin
  if (P1=nil) or (P2=nil) then
    Exit(P1=P2);
  while (P1^=P2^) and (P1^<>#0) and (P2^<>#0) do
  begin
    Inc(P1); Inc(P2);
  end;
  Result := (P1^=#0) and (P2^=#0);
end;

procedure ZStrSubString(const Str,Dest : PAnsiChar; const StartPos,NChars : integer);
var
  P : PAnsiChar;
begin
  {$ifndef minimal}
  ZAssert(StartPos+NChars<=ZStrLength(Str),'SubString called with startpos+NChars>length');
  {$endif}
  P := Str + StartPos;
  Move(P^,Dest^,NChars);
  Dest[NChars] := #0;
end;



procedure TStateBase.Update;
begin
  inherited;
  OnUpdate.ExecuteCommands;
  OnRender.Update;
end;

{ TContentProducer }

procedure TContentProducer.Execute;
begin
  Self.ProduceOutput(GlobalContent.Content,GlobalContent.Stack);
end;


{ TZComponentInfo }

{$ifndef minimal}
function TZComponentInfo.GetProperties: TZPropertyList;
var
  C : TZComponent;
begin
  Result := Self.Properties;
  if Result=nil then
  begin
    Result := TZPropertyList.Create;
    //ZComponent constructor call GetProperties to iterate and initiate properties
    //When C.Create is called the list will be empty
    Self.Properties := Result;
    C := Self.ZClass.Create(nil);
    Result.TheSelf := NativeInt(C);
    C.DefineProperties(Result);
    //ZComponent destroy also calls GetProperties
    //Give it an empty list to iterate too to keep in sync with constructor
    Self.Properties := TZPropertyList.Create;
    C.Free;
    Self.Properties.Free;
    Self.Properties := Result;
  end;
end;
{$endif}

{ TTasks }

constructor TTasks.Create;
begin
  Self.Lock := Platform_CreateMutex;
  Self.Event := Platform_CreateEvent;
  WorkerCount := Platform_GetCpuCount;
  ThreadCount := WorkerCount-1;
  Self.Enabled := True;
end;

destructor TTasks.Destroy;
var
  PT : PWorkerThread;
  I : integer;
begin
  if Threads<>nil then
  begin
    PT := Threads;
    for I := 0 to ThreadCount-1 do
    begin
      PT^.Terminated := True;
      Platform_SignalEvent(Event);
      PT^.Free;
      Inc(PT);
    end;
    FreeMem(Threads);
  end;
  Platform_FreeMutex(Lock);
  Platform_FreeEvent(Event);
  inherited;
end;

procedure TTasks.Run(TaskProc: TTaskProc; TaskList: pointer; TaskCount,
  TaskStride: integer);
var
  I : integer;
  PT : PWorkerThread;
  T : TWorkerThread;
begin
  //TODO: test if already busy, if so spawn to another singleprocess tasks
  Platform_EnterMutex(Self.Lock);
    Self.TaskList := TaskList;
    Self.TaskProc := TaskProc;
    Self.TaskCount := TaskCount;
    Self.InitialTaskCount := TaskCount;
    Self.TaskStride := TaskStride;
    FinishedTaskCount := 0;
  Platform_LeaveMutex(Self.Lock);

  if (ThreadCount>0) and Self.Enabled then
  begin
    if Threads=nil then
    begin
      {$ifdef zlog}
      {$ifndef minimal}
      ZLog.GetLog(Self.ClassName).Write('Worker count: ' + IntToStr(WorkerCount));
      {$endif}
      {$endif}
      IsMultiThread:=True; //Tell the Delphi mm that we are multithreaded
      //Create threads
      GetMem(Threads,ThreadCount*SizeOf(Pointer));
      PT := Threads;
      for I := 0 to ThreadCount-1 do
      begin
        T := TWorkerThread.Create;
        T.Tasks := Self;
        T.Start;
        PT^ := T;
        Inc(PT);
      end;
    end;
    //wake up threads
    for I := 0 to ZMath.Min(TaskCount,ThreadCount)-1 do
      Platform_SignalEvent(Event);
  end;

  while Self.RunNext do ;

  while (FinishedTaskCount<InitialTaskCount) do Platform_Sleep(0);
end;

function TTasks.RunNext: boolean;
var
  ATask : pointer;
begin
  ATask := nil;
  Platform_EnterMutex(Self.Lock);
    Dec(TaskCount);
    Result := TaskCount>=0;
    if Result then
    begin
      ATask := Self.TaskList;
      Inc(NativeUInt(Self.TaskList),Self.TaskStride);
    end;
  Platform_LeaveMutex(Self.Lock);
  if Result then
  begin
    Self.TaskProc(ATask);
    Platform_EnterMutex(Self.Lock);
      Inc(FinishedTaskCount);
    Platform_LeaveMutex(Self.Lock);
  end;
end;

function Tasks : TTasks;
begin
  if TTasks.Instance=nil then
    TTasks.Instance := TTasks.Create;
  Result := TTasks.Instance;
end;

{ TTasks.TWorkerThread }

procedure TTasks.TWorkerThread.Execute;
begin
  while not Terminated do
  begin
    Platform_WaitEvent(Self.Tasks.Event);
    while (not Terminated) and Self.Tasks.RunNext do ;
  end;
end;

{ TZThread }

destructor TZThread.Destroy;
begin
  if Handle<>nil then
    Platform_FreeThread(Handle);
  inherited;
end;

procedure TZThread.Start;
begin
  Handle := Platform_CreateThread(Self);
end;

{ TZThreadComponent }

procedure TZThreadComponent.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}strExpression,{$ENDIF}(@Expression), zptExpression);
    {$ifndef minimal}
    List.GetLast.DefaultValue.ExpressionValue.Source :=
      '//int param : parameter passed in the createThread call';
    List.GetLast.ExpressionKind := ekiThread;
    {$endif}
end;

procedure TZThreadComponent.Start(Param: integer);
var
  T : TUserThread;
begin
  T := TUserThread.Create;
  T.Owner := Self;
  T.Parameter := Param;
  T.Start;
end;

{ TZThreadComponent.TUserThread }

procedure TZThreadComponent.TUserThread.Execute;
var
  Env : TExecutionEnvironment;
begin
  {$ifndef minimal}
  Inc(Owner.ZApp.LiveThreadCount);
  GetLog(Self.ClassName).Write('Starting thread, parameter: ' + IntToStr(Self.Parameter));
  {$endif}

  Env.Init;
  Env.StackPush(Self.Parameter);

  ZExpressions.RunCode(Owner.Expression.Code,@Env);


  {$ifndef minimal}
  Dec(Owner.ZApp.LiveThreadCount);
  GetLog(Self.ClassName).Write('Exiting thread, parameter: ' + IntToStr(Self.Parameter));
  {$endif}

  Free;
end;

{$ifndef minimal}
function TZcDataType.Matches(const Other:TZcDataType) : boolean;
begin
  Result := Self.Kind=Other.Kind;
  if Result then
  begin
    case Kind of
      zctClass : Result := Self.TheClass=Other.TheClass;
      zctArray : Result := (TDefineArray(Self.TheArray).Dimensions=TDefineArray(Other.TheArray).Dimensions) and
        (TDefineArray(Self.TheArray)._Type.Matches(TDefineArray(Other.TheArray)._Type));
      zctReference : Result := (Self.ReferenceClassId=Other.ReferenceClassId)
        or
        ((Self.ReferenceClassId=AnyComponentClassId) or (Other.ReferenceClassId=AnyComponentClassId));
    end;
  end;
end;
{$endif}

initialization

  ManagedHeap_Create;

  Register(TLogicalGroup,LogicalGroupClassId);
    {$ifndef minimal}ComponentManager.LastAdded.ImageIndex:=4;{$endif}
    {$ifndef minimal}ComponentManager.LastAdded.ZClassName := 'Group';{$endif}
  Register(TZThreadComponent,ThreadClassId);
    {$ifndef minimal}ComponentManager.LastAdded.ZClassName := 'Thread';{$endif}
    {$ifndef minimal}ComponentManager.LastAdded.AutoName := True;{$endif}

{$ifndef minimal}
  StringCache := TDictionary<AnsiString,AnsiString>.Create;

finalization

  TTasks.Instance.Free;
  ManagedHeap_Destroy;

  Zc_Ops.CleanUp;
  if Assigned(_ComponentManager) then
    FreeAndNil(_ComponentManager);

  StringCache.Free;

{$endif}

end.

