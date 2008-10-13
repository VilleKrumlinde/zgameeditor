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

interface

{$ifndef minimal}
uses uSymTab;
{$endif}

type
  //Baseclasse for all central koncepts in ZGE
  //List with unique ClassIDs
  TZClassIds = (
 LogicalGroupClassId,ZBitmapClassId,
 BitmapRectClassId,BitmapZoomRotateClassId,BitmapExpressionClassId,BitmapFromFileClassId,BitmapBlurClassId,
 BitmapLoadClassId,BitmapCombineClassId,
 MeshClassId,ModelClassId,MaterialClassId,SpawnModelClassId,RemoveModelClassId,
 MeshBoxClassId,MeshSphereClassId,MeshNoiseClassId,MeshExpressionClassId,RemoveAllModelsClassId,
 MeshImplicitClassId,ImplicitPrimitiveClassId,ImplicitExpressionClassId,ImplicitCombineClassId,
 ImplicitWarpClassId,MeshImportClassId,
 FontClassId,ModelStateClassId,SetModelStateClassId,
 AnimatorGroupClassId,AnimatorSimpleClassId,MouseModelControllerClassId,
 StartAnimatorClassId,
 UseMaterialClassId,RenderMeshClassId,RenderTransformClassId,RenderSpriteClassId,
 RenderBeamsClassId,RenderTransformGroupClassId,RenderTextClassId,RenderSetColorClassId,RenderNetClassId,
 RenderParticlesClassId,ShaderClassId,ShaderVariableClassId,
 RepeatClassId,ConditionClassId,KeyPressClassId,RefreshContentClassId,ZTimerClassId,
 ApplicationClassId,AppStateClassId,SetAppStateClassId,
 ZExpressionClassId,ExpConstantClassId,ExpOpBinaryClassId,ExpPropValueClassId,
 ExpPropPtrClassId,ExpJumpClassId,DefineVariableClassId,ExpFuncCallClassId,
 ExpArrayReadClassId,ExpArrayWriteClassId,ExpStackFrameClassId,ExpAccessLocalClassId,
 ExpReturnClassId,ExpMiscClassId,ExpUserFuncCallClassId,
 DefineConstantClassId,DefineArrayClassId,ZLibraryClassId,
 DefineCollisionClassId,
 SoundClassId,PlaySoundClassId,AudioMixerClassId,
 MusicClassId,MusicControlClassId,
 SteeringControllerClassId,SteeringBehaviourClassId,
 ZFileClassId,FileActionClassId,FileMoveDataClassId
);

  TZComponent = class;
  TZComponentClass = class of TZComponent;
  TZProperty = class;

  PObjectArray = ^TObjectArray;
  TObjectArray = array[0..100000] of TObject;

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
    procedure Clear;
    property Items[Index: Integer]: TObject read GetItem write SetItem; default;
    property Count : integer read FCount;
    procedure Push(Item : TObject);
    function Pop : TObject;
    function PopFloat : single;
    function GetPtrToItem(Index: Integer): pointer;
  end;

  //Används som property i komponenter för att hålla children-komponenter
  //samt event-träd med command-komponenter
  //Obs, äger sina componenter trots att TZArrayList ReferenceOnly=true
  TZComponentList = class(TZArrayList)
  private
    Owner : TZComponent;
  public
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
    {$endif}
  end;


  //Info om en referens till en property på en komponent
  PZPropertyRef = ^TZPropertyRef;
  TZPropertyRef = record
    Component : TZComponent;
    Prop : TZProperty;
    //index för indexed properties (för att välja x,y,z i en vector)
    Index : integer;
    {$ifndef minimal}
    HasPropIndex : boolean;
    {$endif}
  end;

  //Expression är en egen propertytyp
  //I designläge så används både Source-sträng och lista med kompilerad kod
  //I minimal endast kompilerad kod (i nästlad komponentlista)
  PZExpressionPropValue = ^TZExpressionPropValue;
  TZExpressionPropValue = record
    {$ifndef minimal}
    Source : string;            //Expression source
    {$endif}
    Code : TZComponentList;     //Expression byte code
  end;

  //Datatypes in Zc-script
  TZcDataType = (zctVoid,zctFloat);

  PZBinaryPropValue = ^TZBinaryPropValue;
  TZBinaryPropValue = record
    Size : integer;
    Data : pointer;
  end;

  TZPropertyValue = record
    {$IFNDEF MINIMAL}
    //String-props kan ej ligga i case-switch för designer
    StringValue : string;
    ExpressionValue : TZExpressionPropValue;
    {$ENDIF}
    case integer of
      0 : (FloatValue : single);
      2 : (RectfValue : TZRectf);
      3 : (ColorfValue : TZColorf);
      4 : (IntegerValue : integer);
      5 : (ComponentValue : TZComponent); //även inlinecomponent
      {$IFDEF MINIMAL}6 : (StringValue : PChar);{$ENDIF}
      7 : (PropertyValue : TZPropertyRef);
      8 : (Vector3fValue : TZVector3f);
      9 : (ComponentListValue : TZComponentList);
     10 : (GenericValue : array[0..2] of integer); //för default-data test
     11 : (ByteValue : byte);
     12 : (BooleanValue : ByteBool);
     {$ifdef minimal}
     13 : (ExpressionValue : TZExpressionPropValue);
     {$endif}
     14 : (BinaryValue : TZBinaryPropValue);
  end;

  //zptScalar = float with 0..1 range
  TZPropertyType = (zptFloat,zptScalar,zptRectf,zptColorf,zptString,zptComponentRef,zptInteger,
    zptPropertyRef,zptVector3f,zptComponentList,zptByte,zptInlineComponent,zptBoolean,
    zptExpression,zptBinary);

  TZProperty = class
  public
    DefaultValue : TZPropertyValue;
    NeverPersist : boolean;
    DontClone : boolean;
    {$IFNDEF MINIMAL}public{$ELSE}private{$ENDIF}
    PropertyType : TZPropertyType;
    PropId : integer;             //Ordningsnr på denna property för en klass
    Offset : integer;
    {$IFNDEF MINIMAL}
    Name : string;              //Namn på property i designer 'Color'
    ExcludeFromBinary : boolean;  //Ta inte med denna prop i binärström (designer only)
    ExcludeFromXml : boolean; //Spara ej i xml-fil
    NeedRefreshNodeName : boolean;//True för propertys som kräver refresh i nodträd vid ändring av prop
    ChildClasses :               //För componentlists: krav på vilka klasser som kan ligga i listan
      array of TZComponentClass; //För componentref: krav på vilka klasser som går att referera till
    Options : array of string;  //För bytes: Valbara alternativ
    HideInGui : boolean;        //Visa inte denna prop i gui
    IsReadOnly : boolean;       //Prop kan ej tilldelas i expressions
    ReturnType : TZcDataType;      //For expresssions: return type of expression
    function IsDefaultValue(const Value : TZPropertyValue) : boolean;
    procedure SetChildClasses(const C : array of TZComponentClass);
    procedure SetOptions(const O : array of string);
    {$ENDIF}
  end;

  TZPropertyList = class(TZArrayList)
  private
    NextId : integer;
  public
    procedure AddProperty({$IFNDEF MINIMAL}Name : string; {$ENDIF} Offset : integer; PropType : TZPropertyType);
    {$IFNDEF MINIMAL}
    procedure SetDesignerProperty;
    function GetByName(Name : string) : TZProperty;
    function GetByType(Kind : TZPropertyType) : TZProperty;
    {$ENDIF}
    function GetLast : TZProperty;
    function GetById(PropId : integer) : TZProperty;
  end;

  //Datatyp som  zptString-properties ska deklareras som i components (se app.caption)
  TPropString = {$IFNDEF MINIMAL}string{$else}PChar{$ENDIF};
  PPropString = ^TPropString;

  //Baskomponentklass för allt som ska kunna redigeras i tool
  TZComponent = class
  private
    function DoClone(ObjIds,FixUps : TZArrayList) : TZComponent;
  protected
    ObjId : integer;    //only used in streaming
    IsChanged : boolean;
    procedure DefineProperties(List : TZPropertyList); virtual;
  public
    {$IFNDEF MINIMAL} Name,Comment : string; {$ENDIF}
    OwnerList : TZComponentList;
    constructor Create(OwnerList: TZComponentList); virtual;
    destructor Destroy; override;
    function GetProperties : TZPropertyList;
    procedure SetProperty(Prop : TZProperty; const Value : TZPropertyValue);
    procedure GetProperty(Prop : TZProperty; var Value : TZPropertyValue);
    function GetPropertyPtr(Prop : TZProperty; Index : integer) : pointer;
    procedure Update; virtual;
    procedure Change;
    function Clone : TZComponent;
    {$ifndef minimal}
    function GetDisplayName : string; virtual;
    procedure DesignerReset; virtual;  //Reset house-keeping state (such as localtime in animators)
    function GetOwner : TZComponent;
    {$endif}
  end;


  //Command är komponent som används i event-träd
  //Execute-metoden körs vid event-utvärdering
  TCommand = class(TZComponent)
  public
    procedure Execute; virtual; abstract;
  end;


  //Standardkomponent med en definierad property: 'Children'
  //Ärver TCommand så att den exekverar children när den ligger i eventlists.
  TLogicalGroup = class(TCommand)
  protected
    procedure DefineProperties(List : TZPropertyList); override;
  public
    Children : TZComponentList;
    procedure Update; override;
    procedure Execute; override;
    {$ifndef minimal}
    procedure DesignerReset; override;
    {$endif}
  end;



  //Info about one componentclass
  TZComponentInfo = class
    {$IFNDEF MINIMAL}public{$ELSE}private{$ENDIF}
    ZClass : TZComponentClass;
    ClassId : TZClassIds;
    Properties : TZPropertyList;
    {$IFNDEF MINIMAL}
    ZClassName : string;  //Klassnamn utan 'T'
    NoUserCreate : boolean;
    NoTopLevelCreate : boolean; //Tillåt ej användare att skapa denna på toppnivå
    ExcludeFromBinary : boolean; //Skippa hela komponenten i binärfilen
    ImageIndex : integer;  //Icon som ska visas i designerträdet
    HelpText : string;
    NeedParentComp : string;
    NeedParentList : string;
    AutoName : boolean;  //Give name automatically when inserted in designer
    {$ENDIF}
    {$if (not defined(MINIMAL)) or defined(zzdc_activex)}
    public
    HasGlobalData : boolean; //See audiomixer. Do not cache property-list.
    {$ifend}
  end;


  TComponentInfoArray = array[TZClassIds] of TZComponentInfo;
  PComponentInfoArray = ^TComponentInfoArray;

  //Singleton
  //Keeps track of all componentclasses
  TZComponentManager = class
  private
    ComponentInfos : TComponentInfoArray;
    constructor Create;
    function GetInfoFromId(ClassId : TZClassIds) : TZComponentInfo;
  {$IFNDEF MINIMAL}
    function GetInfoFromName(const ZClassName : string) : TZComponentInfo;
  {$ENDIF}
    procedure Register(C : TZComponentClass; ClassId : TZClassIds);
    function GetProperties(Component : TZComponent) : TZPropertyList;
    {$ifndef minimal}public
    destructor Destroy; override;{$else}private{$endif}
    function GetInfo(Component : TZComponent) : TZComponentInfo;
  public
    {$if (not defined(MINIMAL)) or defined(zzdc_activex)}
    LastAdded : TZComponentInfo;
    {$ifend}
  {$IFNDEF MINIMAL}
    function SaveBinaryToStream(Component : TZComponent) : TObject;
    function LoadXmlFromFile(FileName : string) : TZComponent;
    function LoadXmlFromString(const XmlData : string; SymTab : TSymbolTable) : TZComponent;
    procedure SaveXml(Component : TZComponent; FileName : string);
    function SaveXmlToStream(Component: TZComponent) : TObject;
    function GetAllInfos : PComponentInfoArray;
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
    constructor CreateFromFile(FileName : PChar; IsRelative : Boolean);
    constructor CreateFromMemory(M : pointer; Size : integer);
    destructor Destroy; override;
    procedure Read(var Buf; Count : integer);
    function GetMemory : PBytes;
    procedure BitsBegin;
    function ReadBit : boolean;
    procedure BitsEnd;
  end;


function GetPropertyRef(const Prop : TZPropertyRef) : PFloat;
function MakeColorf(const R,G,B,A : single) : TZColorf;

{.$IFNDEF MINIMAL}
function ComponentManager : TZComponentManager;
{.$ENDIF}

//Register componentclass
procedure Register(C : TZComponentClass; ClassId : TZClassIds);

//String functions
function ZStrLength(P : PChar) : integer;
procedure ZStrCopy(P : PChar; const Src : PChar);
procedure ZStrCat(P : PChar; const Src : PChar);
procedure ZStrConvertFloat(const S : single; Dest : PChar);

{$ifndef minimal}
const
  FloatTypes : set of TZPropertyType = [zptFloat,zptScalar,zptRectf,zptColorf,zptVector3f];
  FloatTextDecimals = 4;  //Nr of fraction digits when presenting float-values as text

function GetPropRefAsString(const PRef : TZPropertyRef) : string;
{$endif}

implementation

uses ZMath,ZLog,ZPlatform
  {$IFNDEF MINIMAL},Classes,LibXmlParser,SysUtils,Contnrs,Math,zlib{$ENDIF}
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
    FixUps,PropFixUps : TZArrayList;
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
    Xml : LibXmlParser.TXmlParser;
    FixUps : TZArrayList;
    SymTab : TSymbolTable;
    OldSeparator : char;
    ExternalSymTab : boolean;
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFile(const FileName: string);
    procedure LoadFromString(const XmlData: string; SymTab : TSymbolTable);
    function DoReadComponent(OwnerList : TZComponentList) : TZComponent; override;
    procedure OnDocumentStart; override;
    procedure OnDocumentEnd; override;
  end;
{$ENDIF}

const
  TBinaryNested : set of TZPropertyType = [zptComponentList,zptExpression,zptInlineComponent];

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


{ TZComponent }

constructor TZComponent.Create(OwnerList: TZComponentList);
var
  PropList : TZPropertyList;
  Prop : TZProperty;
  I : integer;
  List : TZComponentList;
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
          List := TZComponentList.Create;
          PZExpressionPropValue(GetPropertyPtr(Prop,0))^.Code := List;
        end;
    end;
    //Set defaultvalue for property
    //todo: Robustare sätt att testa ifall defaultvärde finns, generic4 är < sizeof(value)
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
          GetProperty(Prop,Value);
          Value.ComponentListValue.Free;
        end;
      zptExpression :
        begin
          GetProperty(Prop,Value);
          Value.ExpressionValue.Code.Free;
        end;
      zptInlineComponent :
        begin
          GetProperty(Prop,Value);
          Value.ComponentValue.Free;
        end;
      {$ifndef zminimal}
      zptBinary :
        begin
          //Frigör binary mem enbart i designer.
          //I minimal så klonas binary genom att peka på samma source, mem skulle
          //då frigöras flera gånger.
          GetProperty(Prop,Value);
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
  List.AddProperty('Name', integer(@Name) - integer(Self), zptString);
    List.SetDesignerProperty;
    List.GetLast.NeedRefreshNodeName := True;
  List.AddProperty('Comment', integer(@Comment) - integer(Self), zptString);
    List.SetDesignerProperty;
    List.GetLast.NeedRefreshNodeName := True;
  {$ENDIF}
  List.AddProperty({$IFNDEF MINIMAL}'ObjId',{$ENDIF}integer(@ObjId) - integer(Self), zptInteger);
    {$IFNDEF MINIMAL}
    List.GetLast.ExcludeFromXml := True;
    List.GetLast.IsReadOnly := True;
    {$ENDIF}
    List.GetLast.DontClone := True;
end;

procedure TZComponent.GetProperty(Prop: TZProperty; var Value: TZPropertyValue);
var
  P : pointer;
begin
  P := pointer(integer(Self) + Prop.Offset);
  case Prop.PropertyType of
    zptFloat,zptScalar :
      Value.FloatValue := PFloat(P)^;
    zptString :
      {$IFDEF MINIMAL}
      Value.StringValue := PChar(PPointer(P)^);
      {$ELSE}
      Value.StringValue := PString(P)^;
      {$ENDIF}
    zptComponentRef,zptInlineComponent :
      Value.ComponentValue := TZComponent(PPointer(P)^);
    zptInteger :
      Value.IntegerValue := PInteger(P)^;
    zptRectf :
      Value.RectfValue := PRectf(P)^;
    zptColorf :
      Value.ColorfValue := PColorf(P)^;
    zptPropertyRef :
      Value.PropertyValue := PZPropertyRef(PPointer(P))^;
    zptVector3f :
      Value.Vector3fValue := PZVector3f(P)^;
    zptComponentList :
      Value.ComponentListValue := TZComponentList(PPointer(P)^);
    zptByte :
      Value.ByteValue := PByte(P)^;
    zptBoolean :
      Value.BooleanValue := PBoolean(P)^;
    zptExpression :
      {$ifdef minimal}
      Value.ExpressionValue.Code := TZComponentList(PPointer(P)^);
      {$else}
      Value.ExpressionValue := PZExpressionPropValue(P)^;
      {$endif}
    zptBinary :
      Value.BinaryValue := PZBinaryPropValue(P)^;
    else
      ZHalt('GetProperty no handler');
  end;
end;

procedure TZComponent.SetProperty(Prop: TZProperty; const Value: TZPropertyValue);
var
  P : pointer;
begin
  P := pointer(integer(Self) + Prop.Offset);
  case Prop.PropertyType of
    zptFloat,zptScalar :
      PFloat(P)^ := Value.FloatValue;
    zptString :
      {$IFDEF MINIMAL}
      //todo copy characters? nix, string ska vara immutable.
      PPChar(P)^ := Value.StringValue;
      {$ELSE}
      PString(P)^ := Value.StringValue;
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
    zptPropertyRef :
      PZPropertyRef(P)^ := Value.PropertyValue;
    zptVector3f :
      PZVector3f(P)^ := Value.Vector3fValue;
    zptInlineComponent :
      begin
        if (PPointer(P)^<>nil) and (PPointer(P)^<>Value.ComponentValue) then
          TZComponent(PPointer(P)^).Free;
        PPointer(P)^ := pointer(Value.ComponentValue);
      end;
    zptComponentList :
      begin
        {$IFNDEF MINIMAL}
        //Måste tilldelas till samma värde, annars så ska vi göra free på äldre värdet
        Assert(PPointer(P)^ = pointer(Value.ComponentListValue));
        {$ENDIF}
        //Onödigt att tilldela samma värde
        //PPointer(P)^ := pointer(Value.ComponentListValue);
      end;
    zptBoolean :
      PBoolean(P)^ := Value.BooleanValue;
    zptExpression :
      {$ifdef minimal}
      //Tilldelas ej direkt i minimal. Skapas i create och fylls på i binary-load.
      ;
      {$else}
      PZExpressionPropValue(P)^.Source := Value.ExpressionValue.Source;  //Tool tilldelar source-strängen
      {$endif}
    zptBinary :
      begin
        {$ifndef minimal}
        if PZBinaryPropValue(P)^.Size>0 then
          FreeMem(PZBinaryPropValue(P)^.Data);
        {$endif}
        PZBinaryPropValue(P)^ := Value.BinaryValue;
      end
    else
      ZHalt('SetProperty no handler');
  end;
  Change;
end;

function TZComponent.GetProperties: TZPropertyList;
begin
  Result := ComponentManager.GetProperties(Self);
end;



//Returnerar pekare till property-value
//Används för att hitta target för propertyrefs
function TZComponent.GetPropertyPtr(Prop: TZProperty; Index: integer): pointer;
begin
  Result := pointer(integer(Self) + Prop.Offset);
  if Index>0 then
    Result := pointer(integer(Result) + Index*4);
end;

procedure TZComponent.Change;
begin
  IsChanged := True;
  //todo bättre hantering av change behövs nog
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
          C.GetProperty(Prop,Value);
          for J := 0 to Value.ComponentListValue.ComponentCount-1 do
            CloneAssignObjectIds(TZComponent(Value.ComponentListValue[J]),ObjIds,CleanUps);
        end;
      zptExpression :
        begin
          C.GetProperty(Prop,Value);
          for J := 0 to Value.ExpressionValue.Code.ComponentCount-1 do
            CloneAssignObjectIds(TZComponent(Value.ExpressionValue.Code[J]),ObjIds,CleanUps);
        end;
      zptInlineComponent :
        begin
          C.GetProperty(Prop,Value);
          CloneAssignObjectIds(Value.ComponentValue,ObjIds,CleanUps);
        end;
    end;
  end;
end;

function TZComponent.Clone: TZComponent;
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
  Result := DoClone(ObjIds,FixUps);

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
end;



function TZComponent.DoClone(ObjIds,FixUps : TZArrayList): TZComponent;
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
      DestList.AddComponent( C.DoClone(ObjIds,FixUps) );
    end;
  end;

begin
  Result := TZComponentClass(Self.ClassType).Create(nil);
  ObjIds[ Self.ObjId ] := Result;
  PropList := GetProperties;
  for I := 0 to PropList.Count-1 do
  begin
    Prop := TZProperty(PropList[I]);
    if Prop.DontClone then
      Continue; //Skip properties like: objid, model.personality
    GetProperty(Prop,Value);
    case Prop.PropertyType of
      zptInlineComponent :
        begin
          Value.ComponentValue := Value.ComponentValue.DoClone(ObjIds,FixUps);
          Result.SetProperty(Prop,Value);
        end;
      zptComponentRef :
        begin
          Result.SetProperty(Prop,Value);
          if (Value.ComponentValue<>nil) and (Value.ComponentValue.ObjId<>0) then
            FixUps.Add( TObject(PPointer(integer(Result) + Prop.Offset)) );
        end;
      zptPropertyRef :
        begin
          Result.SetProperty(Prop,Value);
          if (Value.PropertyValue.Component<>nil) and (Value.PropertyValue.Component.ObjId<>0) then
            FixUps.Add( TObject(PPointer(integer(Result) + Prop.Offset)) );
        end;
      zptComponentList :
        begin
          Result.GetProperty(Prop,Tmp);
          InCloneList(Value.ComponentListValue,Tmp.ComponentListValue);
        end;
      zptExpression :
        begin
          Result.GetProperty(Prop,Tmp);
          InCloneList(Value.ExpressionValue.Code,Tmp.ExpressionValue.Code);
          {$ifndef minimal}
          //Also copy source if in designer
          Tmp.ExpressionValue.Source := Value.ExpressionValue.Source;
          Result.SetProperty(Prop,Tmp);
          {$endif}
        end;
      {$ifndef zminimal}
      zptBinary :
        begin
          //Kopiera binary mem enbart i designer.
          //I minimal så klonas binary genom att peka på samma source
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
end;

{$ifndef minimal}
function TZComponent.GetDisplayName: string;
var
  S,Cn : string;
begin
  S := Self.Name;
  Cn := ComponentManager.GetInfo(Self).ZClassName;
  if Length(S)=0 then
    S := Cn
  else
    S := S + ' : ' + Cn;
  Result := S;
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
          GetProperty(Prop,Value);
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

function TZArrayList.GetItem(Index: Integer): TObject;
begin
  if (Index < 0) or (Index >= FCount) then
    ZHalt('ZArrayList bad index');
  Result := List^[Index];
end;

function TZArrayList.GetPtrToItem(Index: Integer): pointer;
begin
  {$IFNDEF MINIMAL}
  if (Index < 0) or (Index >= FCount) then
    ZHalt('ZArrayList bad index');
  {$ENDIF}
  Result := @List^[Index];
end;

procedure TZArrayList.SetItem(Index: Integer; const Value: TObject);
begin
  if (Index < 0) or (Index >= FCount) then
    ZHalt('ZArrayList bad index');
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
  while (Result < FCount) and (List^[Result] <> Item) do
    Inc(Result);
  if Result = FCount then
    Result := -1;
end;

procedure TZArrayList.RemoveAt(Index: integer);
var
  Temp: TObject;
begin
  if (Index < 0) or (Index >= FCount) then
    ZHalt('ZArrayList bad index');
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
  {$ifndef minimal}Assert(Count>0);{$endif}
  Result := Last;
  RemoveAt(Count-1);
end;

procedure TZArrayList.Push(Item: TObject);
begin
  Add(Item);
end;

function TZArrayList.PopFloat: single;
var
  T : TObject;
begin
  //FPC har en bug så att man inte kan skriva: f:=single(stack.pop)
  T:=Last;
  Result := single(T);
  RemoveAt(Count-1);
end;

constructor TZArrayList.CreateReferenced;
begin
  ReferenceOnly := True;
end;


{ TZComponentManager }

constructor TZComponentManager.Create;
begin
end;

function TZComponentManager.GetInfo(Component: TZComponent) : TZComponentInfo;
var
  C : TZComponentClass;
  I : TZClassIds;
  Ci : TZComponentInfo;
begin
  //todo borde ligga i hashlist
  C := TZComponentClass(Component.ClassType);

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
  {$ENDIF}
  {$if (not defined(MINIMAL)) or defined(zzdc_activex)}
  LastAdded := Ci;
  {$ifend}
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
    if (Ci.Properties<>nil) then
      Ci.Properties.Free;
    Ci.Free;
  end;
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
{$ENDIF}

function TZComponentManager.LoadBinary: TZComponent;
var
  Stream : TZInputStream;
  Reader : TZBinaryReader;

  function InLoadPiggyback : TZInputStream;
  var
    FileName : PChar;
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

begin
  //First check linked data, returns nil if not present
  Stream := Platform_LoadLinkedResource;
  //Second: check piggyback data
  if Stream=nil then
    Stream := InLoadPiggyback;
  //Last try: load from file
  if Stream=nil then
    Stream := TZInputStream.CreateFromFile('zzdc.dat',True);
  {$ifdef zlog}
  if Stream=nil then
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
  //Returnerar propertylista för en komponent
  //Listan ligger i classinfon, initieras första gången genom att anropa component.defineproperties
  //Listan kan ej skapas vid klassregistrering för att prop-adressoffsets endast kan beräknas när instans finns.
  //Den här metoden är private, App-kod ska anropa c.GetProperties
  Ci := GetInfo(Component);
  Result := Ci.Properties;
  if Result=nil then
  begin
    Result := TZPropertyList.Create;
    Component.DefineProperties(Result);
    Ci.Properties := Result;
  end
  {$if (not defined(MINIMAL)) or defined(zzdc_activex)}
  else if Ci.HasGlobalData then
  begin
    //Components that use global variables must be single instance
    //and redefines their properties each time (AudioMixer).
    Result.Clear;
    Result.NextId := 0;
    Component.DefineProperties(Result);
  end
  {$ifend};
end;


{ TZPropertyList }

function TZPropertyList.GetLast;
begin
  Result := TZProperty(Self.Last);
end;

{$IFNDEF MINIMAL}
procedure TZPropertyList.SetDesignerProperty;
begin
  //Sätt senaste prop till bara ska användas i designer (t.ex. Name)
  GetLast.ExcludeFromBinary := True;
  //Avallokera senaste id, dessa måste vara konstanta för alla binärprops
  Dec(NextId);
end;

function TZPropertyList.GetByName(Name: string): TZProperty;
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

//Returnerar den första propertyn av en viss typ
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

procedure TZPropertyList.AddProperty({$IFNDEF MINIMAL}Name: string;{$ENDIF} Offset: integer; PropType : TZPropertyType);
var
  P : TZProperty;
begin
  P := TZProperty.Create;
  P.PropertyType := PropType;
  P.Offset := Offset;
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
    C.GetProperty(Prop,Value);
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
            PStream.Write(Value.StringValue[1],Temp);
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
          //todo: should not need to test for nil, när vi har defaultfiltrering
          WriteNulls(PStream,4)
        else
          PStream.Write(Value.ComponentValue.ObjId,4);
      zptPropertyRef :
        if Value.PropertyValue.Component=nil then
          //todo: should not need to test for nil, när vi har defaultfiltrering
          WriteNulls(PStream,6)
        else
        begin
          PStream.Write(Value.PropertyValue.Component.ObjId,4);
          WriteVarLength(PStream,Value.PropertyValue.Prop.PropId);
          B := Value.PropertyValue.Index;
          PStream.Write(B,1);
        end;
      zptVector3f :
        PStream.Write(Value.Vector3fValue,SizeOf(TZVector3f));
      zptByte :
        PStream.Write(Value.ByteValue,SizeOf(byte));
      zptBoolean :
        PStream.Write(Value.BooleanValue,SizeOf(ByteBool));
      zptInlineComponent,zptComponentList,zptExpression :
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
  //Skriv nästlade componenter efteråt så att alla propbits
  //hamnar i main-stream först.
  for I := 0 to AfterList.Count-1 do
  begin
    Prop := TZProperty(AfterList[I]);
    C.GetProperty(Prop,Value);
    case Prop.PropertyType of
      zptInlineComponent :
        DoWriteComponent(Value.ComponentValue);
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
  //Appenda alla propertystreams, samt gör free på dessa
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
  //Skriv dictionary med sizes för varje pstream
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
            C.GetProperty(Prop,Value);
            InGiveOne(Value.ComponentValue);
          end;
        zptInlineComponent :
          begin
            C.GetProperty(Prop,Value);
            InGiveObjIds(Value.ComponentValue);
          end;
        zptPropertyRef :
          begin
            C.GetProperty(Prop,Value);
            InGiveOne(Value.PropertyValue.Component);
          end;
        zptComponentList :
          begin
            C.GetProperty(Prop,Value);
            for J := 0 to Value.ComponentListValue.ComponentCount-1 do
              InGiveObjIds(Value.ComponentListValue.GetComponent(J));
          end;
        zptExpression :
          begin
            C.GetProperty(Prop,Value);
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

  function InAttrValue(const S : string) : string;
  begin
    Result := S;
    Result := StringReplace(Result,'&','&amp;',[rfReplaceAll]);
    Result := StringReplace(Result,'"','&quot;',[rfReplaceAll]);
    Result := StringReplace(Result,'<','&lt;',[rfReplaceAll]);
    Result := StringReplace(Result,'>','&gt;',[rfReplaceAll]);
    Result := StringReplace(Result,'''','&apos;',[rfReplaceAll]);
  end;

  function InGetBinary(const BinaryValue : TZBinaryPropValue) : string;
  var
    Zs : zlib.TCompressionStream;
    Mem : TMemoryStream;
  begin
    Mem := TMemoryStream.Create;
    try
      Zs := TCompressionStream.Create(clMax,Mem);
      try
        Zs.Write(BinaryValue.Data^,BinaryValue.Size)
      finally
        Zs.Free;
      end;
      Mem.Position:=0;
      SetLength(Result,Mem.Size*2);
      Classes.BinToHex(PChar(Mem.Memory),PChar(Result),Mem.Size);
    finally
      Mem.Free;
    end;
  end;

begin
  Ci := ComponentManager.GetInfo(C);

  NormalProps := TObjectList.Create(False);
  NestedProps := TObjectList.Create(False);
  try
    //Gå igenom props för att ta reda på vilka som ska skrivas
    //Skilj på props som skrivs som attributes, och de som skrivs nested som elements
    PropList := C.GetProperties;
    for I := 0 to PropList.Count-1 do
    begin
      Prop := TZProperty(PropList[I]);
      C.GetProperty(Prop,Value);
      if Prop.NeverPersist or Prop.ExcludeFromXml or Prop.IsDefaultValue(Value) then
        Continue;
      case Prop.PropertyType of
        zptString :
          if (Pos(#13,Value.StringValue)=0) {and
            (Pos('<',Value.StringValue)=0) and
            (Pos('>',Value.StringValue)=0)} then
            NormalProps.Add(Prop)
          else
            NestedProps.Add(Prop);
        zptInlineComponent,zptComponentList :
          NestedProps.Add(Prop);
        zptBinary :
          if Value.BinaryValue.Size>0 then
            NestedProps.Add(Prop);
        zptExpression :
          if (Pos(#13,Value.ExpressionValue.Source)=0) then
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
      C.GetProperty(Prop,Value);
      case Prop.PropertyType of
        zptString : V := InAttrValue( Value.StringValue );
        zptFloat,zptScalar : V := FloatToStr( RoundTo( Value.FloatValue ,-FloatTextDecimals) );
        zptRectf : V := InArray(Value.RectfValue.Area);
        zptColorf : V := InArray(Value.ColorfValue.V);
        zptInteger : V := IntToStr(Value.IntegerValue);
        zptComponentRef : V := Value.ComponentValue.Name;
        zptPropertyRef :
          begin
            V := Value.PropertyValue.Component.Name + ' ' + Value.PropertyValue.Prop.Name;
            if Value.PropertyValue.Index>0 then
              V := V + ' ' + IntToStr(Value.PropertyValue.Index);
          end;
        zptVector3f : V := InArray(Value.Vector3fValue);
        zptByte : V := IntToStr(Value.ByteValue);
        zptBoolean : V := IntToStr( byte(Value.BooleanValue) );
        zptExpression : V := InAttrValue( Value.ExpressionValue.Source );
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
        C.GetProperty(Prop,Value);
        LevelDown;
        WriteLine('<' + Prop.Name + '>');
        case Prop.PropertyType of
          zptString :
            WriteString('<![CDATA[' + Value.StringValue + ']]>'#13#10);
          zptExpression :
            WriteString('<![CDATA[' + Value.ExpressionValue.Source + ']]>'#13#10);
          zptInlineComponent :
            begin
              LevelDown;
              DoWriteComponent(Value.ComponentValue);
              LevelUp;
            end;
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
        S := S + ' <!-- ' + C.Name + ' -->'#13#10;
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
  SysUtils.DecimalSeparator := Self.OldSeparator;
end;

procedure TZXmlWriter.OnDocumentStart;
begin
  WriteLine('<?xml version="1.0" encoding="iso-8859-1" ?>');
  Self.OldSeparator := SysUtils.DecimalSeparator;
  SysUtils.DecimalSeparator := '.';
end;

procedure TZXmlWriter.WriteString(const S: string);
begin
  Write(S[1],Length(S));
end;

procedure TZXmlWriter.WriteLine(const S: string);
var
  Spaces : string;
begin
  if Length(S)>0 then
  begin
    if IndentLevel>0 then
    begin
      SetLength(Spaces,IndentLevel*2);
      FillChar(Spaces[1],Length(Spaces),' ');
      WriteString(Spaces);
    end;
    WriteString(S);
    WriteString(#13#10);
  end;
end;

{$ENDIF}

{ TZProperty }

{$IFNDEF MINIMAL}
function TZProperty.IsDefaultValue(const Value: TZPropertyValue): boolean;
begin
  Result := False;
  //true ifall value.equals(self.defaultvalue)
  //eller null. döp om till ShouldStreamValue?
  case PropertyType of
    zptString : Result := {$ifdef minimal}Value.StringValue=nil;{$else}Value.StringValue=DefaultValue.StringValue;{$endif}
    zptByte : Result := Value.ByteValue=DefaultValue.ByteValue;
    zptInteger : Result := Value.IntegerValue=DefaultValue.IntegerValue;
    zptComponentRef : Result := Value.ComponentValue=nil;
    zptPropertyRef : Result := Value.PropertyValue.Component=nil;
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
    zptBinary :
      Result := Value.BinaryValue.Size=0;
  end;
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

{ TZInputStream }

constructor TZInputStream.CreateFromFile(FileName: PChar; IsRelative : Boolean);
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
          Temp := ZStrLength(PChar(PStream.GetMemory));
          if Temp>0 then
          begin
            {$IFDEF MINIMAL}
            //Value.StringValue := PChar(PStream.GetMemory);
            //Inc(PStream.Position,Temp+1);
            GetMem(Value.StringValue,Temp+1);
            PStream.Read(Value.StringValue^,Temp+1);
            //Value.StringValue[Temp] := #0;
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
            FixUps.Add( TObject(PPointer(integer(C) + Prop.Offset)) );
        end;
      zptPropertyRef :
        begin
          PStream.Read(Value.PropertyValue.Component,4);
          PInteger(@Value.PropertyValue.Prop)^ := ReadVarLength(PStream);
          //PStream.Read(Value.PropertyValue.Prop,1);
          PStream.Read(B,1);
          Value.PropertyValue.Index := B;
          if Value.PropertyValue.Component<>nil then
            PropFixUps.Add( TObject(PPointer(integer(C) + Prop.Offset)) );
        end;
      zptVector3f :
        PStream.Read(Value.Vector3fValue,SizeOf(TZVector3f));
      zptInlineComponent,zptComponentList,zptExpression :
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

  //Ta nästlade komponenter separat
  for I := 0 to AfterList.Count-1 do
  begin
    Prop := TZProperty(AfterList[I]);
    case Prop.PropertyType of
      zptInlineComponent :
        Value.ComponentValue := DoReadComponent(nil);
      zptComponentList :
        begin
          //läs först så att vi får samma pekare (listan ägs av komponenten)
          C.GetProperty(Prop,Value);
          InReadList(Value.ComponentListValue);
        end;
      zptExpression :
        begin
          //läs först så att vi får samma pekare (listan ägs av komponenten)
          C.GetProperty(Prop,Value);
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
  Result := C;
end;

procedure TZBinaryReader.OnDocumentEnd;
var
  I,ObjId : integer;
  P : PPointer;
  PRef : PZPropertyRef;
  PropId : integer;
begin
  //component references
  for I := 0 to FixUps.Count-1 do
  begin
    P := PPointer(FixUps[I]);
    ObjId := integer(P^);
    P^ := ObjIds[ObjId];
  end;
  FixUps.Free;

  //property references
  for I := 0 to PropFixUps.Count-1 do
  begin
    PRef := PZPropertyRef(PropFixUps[I]);
    ObjId := integer(PRef^.Component);
    PRef^.Component := TZComponent(ObjIds[ObjId]);
    PropId := PInteger(@PRef^.Prop)^;
    PRef^.Prop := PRef^.Component.GetProperties.GetById(PropId);
  end;
  PropFixUps.Free;

  //nolla ut tilldelade objids så att de kan användas runtime för clone
  for I := 0 to ObjIds.Count-1 do
    if ObjIds[I]<>nil then
      TZComponent(ObjIds[I]).ObjId:=0;

  ObjIds.Free;

  //todo: gör free på pstreams?
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
  //Loopa i omvänd ordning och backa i stream
  //för att få tag på varje propstream.
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
  PropFixUps := TZArrayList.Create;
  PropFixUps.ReferenceOnly := True;
  ObjIds := TZArrayList.Create;
  ObjIds.ReferenceOnly := True;
end;

{ TZXmlReader }

{$IFNDEF MINIMAL}
constructor TZXmlReader.Create;
begin
  Xml := TXmlParser.Create;
  FixUps := TZArrayList.Create;
  Self.OldSeparator := SysUtils.DecimalSeparator;
  SysUtils.DecimalSeparator := '.';
end;

procedure TZXmlReader.LoadFromFile(const FileName: string);
begin
  ZLog.GetLog(Self.ClassName).Write('Loading: ' + FileName);
  ExternalSymTab := False;
  SymTab := TSymbolTable.Create;
  Xml.LoadFromFile(FileName);
end;

procedure TZXmlReader.LoadFromString(const XmlData: string; SymTab : TSymbolTable);
begin
  ExternalSymTab := True;
  Self.SymTab := SymTab;
  //Use the global symbol table
  //Let the locals be defines in a local scope
  SymTab.PushScope;
  Xml.LoadFromBuffer(PChar(XmlData));;
end;

destructor TZXmlReader.Destroy;
begin
  SysUtils.DecimalSeparator := Self.OldSeparator;
  Xml.Free;
  FixUps.Free;
  if ExternalSymTab then
    SymTab.PopScope
  else
    SymTab.Free;
  inherited;
end;

function TZXmlReader.DoReadComponent(OwnerList: TZComponentList): TZComponent;
var
  ZClassName : string;
  C : TZComponent;
  Ci : TZComponentInfo;
  I,J : integer;
  PropList : TZPropertyList;
  Value : TZPropertyValue;
  Prop,NestedProp : TZProperty;
  S : string;
  L : TStringList;
  Fix : TZXmlFixUp;

  procedure InDecodeBinary(const HexS : string; var BinaryValue : TZBinaryPropValue);
  var
    CompMem,DecompMem : TMemoryStream;
    Zs : zlib.TDecompressionStream;
    S : string;
    Buf : array[0..1023] of byte;
    I : integer;
  begin
    CompMem := TMemoryStream.Create;
    DecompMem := TMemoryStream.Create;
    try
      SetLength(S,Length(HexS) div 2);
      Classes.HexToBin(PChar(HexS),PChar(S),Length(S));

      CompMem.Write(S[1],Length(S));
      CompMem.Position := 0;

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

begin
  ZClassName := Xml.CurName;

  Ci := ComponentManager.GetInfoFromName(ZClassName);
  C := Ci.ZClass.Create(OwnerList);

  L := TStringList.Create;
  try
    L.Delimiter := ' ';
    //read properties
    PropList := C.GetProperties;

    for I := 0 to Xml.CurAttr.Count-1 do
    begin
      S:=Xml.CurAttr.Name(I);
      for J := 0 to PropList.Count-1 do
      begin
        Prop := TZProperty(PropList[J]);
        if SameText(Prop.Name,S) then
        begin
          S := Xml.CurAttr.Value(I);
          case Prop.PropertyType of
            zptString :
              Value.StringValue := S;
            zptFloat,zptScalar :
              Value.FloatValue := StrToFloat(S);
            zptRectf :
              begin
                L.DelimitedText := S;
                Value.RectfValue.Area[0] := StrToFloat(L[0]);
                Value.RectfValue.Area[1] := StrToFloat(L[1]);
                Value.RectfValue.Area[2] := StrToFloat(L[2]);
                Value.RectfValue.Area[3] := StrToFloat(L[3]);
              end;
            zptColorf :
              begin
                L.DelimitedText := S;
                Value.ColorfValue.V[0] := StrToFloat(L[0]);
                Value.ColorfValue.V[1] := StrToFloat(L[1]);
                Value.ColorfValue.V[2] := StrToFloat(L[2]);
                Value.ColorfValue.V[3] := StrToFloat(L[3]);
              end;
            zptVector3f :
              begin
                L.DelimitedText := S;
                Value.Vector3fValue[0] := StrToFloat(L[0]);
                //Allow a single value to be specified, this is copied to all three elements
                //Used when switching type from float to vector3d (material.texturescale)
                if L.Count>1 then
                  Value.Vector3fValue[1] := StrToFloat(L[1])
                else
                  Value.Vector3fValue[1] := Value.Vector3fValue[0];
                if L.Count>2 then
                  Value.Vector3fValue[2] := StrToFloat(L[2])
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
            zptPropertyRef :
              begin
                L.DelimitedText := S;
                if L.Count<2 then
                  raise Exception.Create('TZXmlReader: Bad property ref ' + S);
                Fix := TZXmlFixUp.Create;
                Fix.Name := LowerCase(L[0]);
                Fix.PropName := LowerCase(L[1]);
                if L.Count>2 then
                  Value.PropertyValue.Index := StrToIntDef(L[2],0);
                Fix.Prop := Prop;
                Fix.Obj := C;
                FixUps.Add( Fix );
              end;
            zptExpression :
              Value.ExpressionValue.Source := S;
          else
            ZHalt('TZXmlReader: No readhandler');
          end;
          C.SetProperty(Prop,Value);

          Break;
        end;
      end;
    end;

  if Xml.CurPartType=ptStartTag then
  begin
    while Xml.Scan do
      case Xml.CurPartType of
        ptStartTag :
          begin
            //Hantera nästlade komponnenter
            //Det gäller componentlists och inlinecomponent
            S := Xml.CurName;
            NestedProp:=nil;
            for I := 0 to PropList.Count-1 do
            begin
              Prop := TZProperty(PropList[I]);
              if SameText(Prop.Name,Xml.CurName) and
                (Prop.PropertyType in [zptComponentList,zptInlineComponent,zptString,zptExpression,zptBinary]) then
              begin
                NestedProp := Prop;
                Break;
              end;
            end;
            if NestedProp=nil then
              raise Exception.Create('TZXmlReader: Unknown nested property ' + Xml.CurName);
            C.GetProperty(NestedProp,Value);
            while Xml.Scan do
              case Xml.CurPartType of
                ptStartTag,ptEmptyTag,ptCData  :
                  case NestedProp.PropertyType of
                    zptComponentList : DoReadComponent(Value.ComponentListValue);
                    zptInlineComponent :
                      begin
                        Value.ComponentValue := DoReadComponent(nil);
                        C.SetProperty(NestedProp,Value);
                      end;
                    zptString :
                      begin
                        Value.StringValue := Trim(Xml.CurContent);
                        C.SetProperty(NestedProp,Value);
                      end;
                    zptExpression :
                      begin
                        Value.ExpressionValue.Source := Trim(Xml.CurContent);
                        C.SetProperty(NestedProp,Value);
                      end;
                    zptBinary :
                      begin
                        try
                          InDecodeBinary(Xml.CurContent,Value.BinaryValue);
                          C.SetProperty(NestedProp,Value);
                        except
                          ZLog.GetLog(Self.ClassName).Write('*** Failed to read binary property: ' + C.Name);
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
  end;

  if C.Name<>'' then
    SymTab.Add(LowerCase(C.Name),C);

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
        Assert(C<>nil,'Unknown reference: ' + Fix.Name);
    end;
    case Fix.Prop.PropertyType of
      zptComponentRef :
        Value.ComponentValue := C;
      zptPropertyRef :
        begin
          Fix.Obj.GetProperty(Fix.Prop,Value);
          Value.PropertyValue.Component := C;
          Value.PropertyValue.Prop := C.GetProperties.GetByName(Fix.PropName);
          Assert(Value.PropertyValue.Prop<>nil,'Unknown reference: ' + Fix.PropName);
        end;
    end;
    Fix.Obj.SetProperty(Fix.Prop,Value);
  end;
end;

procedure TZXmlReader.OnDocumentStart;
begin
  while Xml.Scan do
    if Xml.CurPartType in [ptStartTag,ptEmptyTag] then
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
    if C is TCommand then
      TCommand(C).Execute
    else
      //Anropa Update på allt som inte är kommandon (expressions)
      C.Update;
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
begin
  for I := 0 to Count-1 do
    TZComponent(Self[I]).Update;
end;

procedure TZComponentList.Clear;
var
  Instance: TZComponent;
begin
  //Destroy childcomponents
  while Count>0 do
  begin
    Instance := TZComponent(Last);
    RemoveComponent(Instance);
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
{$endif}



{ TLogicalGroup }

procedure TLogicalGroup.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Children',{$ENDIF}integer(@Children) - integer(Self), zptComponentList);
end;

{$ifndef minimal}
procedure TLogicalGroup.DesignerReset;
begin
  inherited;
  Children.DesignerReset;
end;
{$endif}

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
  List.AddProperty({$IFNDEF MINIMAL}'Producers',{$ENDIF}integer(@Producers) - integer(Self), zptComponentList);
end;

var
  GlobalContent :
    record
      Content : TContent;
      Stack : TZArrayList;
    end;

procedure TContent.RefreshFromProducers;
var
  Stack : TZArrayList;
begin
  {$ifdef zlog}
  if Producers.Count>0 then
    ZLog.GetLog(Self.ClassName).Write('Refresh: ' + GetDisplayName);
  {$endif}

  Stack := TZArrayList.Create;
    Stack.ReferenceOnly := True;

    GlobalContent.Content := Self;
    GlobalContent.Stack := Stack;

    //Execute producers as commands
    //This way Repeat and Condition-statements can be used
    Producers.ExecuteCommands;

    if Stack.Count>0 then
      CopyAndDestroy(TContent(Stack.Pop));
    while(Stack.Count>0) do
      Stack.Pop().Free;
    IsChanged := False;
    Producers.IsChanged := False;
  Stack.Free;

  FillChar(GlobalContent,SizeOf(GlobalContent),0);
end;

///////////////////

function GetPropertyRef(const Prop : TZPropertyRef) : PFloat;
begin
  Result := PFloat(Prop.Component.GetPropertyPtr(Prop.Prop,Prop.Index));
end;

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
  List.AddProperty({$IFNDEF MINIMAL}'OnStart',{$ENDIF}integer(@OnStart) - integer(Self), zptComponentList);
  List.AddProperty({$IFNDEF MINIMAL}'OnUpdate',{$ENDIF}integer(@OnUpdate) - integer(Self), zptComponentList);
  List.AddProperty({$IFNDEF MINIMAL}'OnLeave',{$ENDIF}integer(@OnLeave) - integer(Self), zptComponentList);
  List.AddProperty({$IFNDEF MINIMAL}'OnRender',{$ENDIF}integer(@OnRender) - integer(Self), zptComponentList);
  List.AddProperty({$IFNDEF MINIMAL}'Definitions',{$ENDIF}integer(@Definitions) - integer(Self), zptComponentList);
end;


//String functions

function ZStrFindEnd(P : PChar) : PChar;
begin
  while P^<>#0 do Inc(P);
  Result := P;
end;

function ZStrLength(P : PChar) : integer;
begin
  Result := ZStrFindEnd(P) - P;
end;

procedure ZStrCopy(P : PChar; const Src : PChar);
var
  Len : integer;
begin
  Len := ZStrLength(Src);
  System.Move(Src^,P^,Len+1);
end;

procedure ZStrCat(P : PChar; const Src : PChar);
begin
  P := ZStrFindEnd(P);
  ZStrCopy(P,Src);
end;

procedure ZStrConvertFloat(const S : single; Dest : PChar);
var
  Value : integer;
  Tmp : PChar;
  Buf : array[0..15] of char;
begin
  Value := Abs(Trunc(S));
  Tmp := @Buf[High(Buf)];
  Tmp^ := #0;
  Dec(Tmp);
  while (Value>9) and (Tmp>@Buf) do
  begin
    Tmp^:=Chr(Value mod 10 + 48);
    Dec(Tmp);
    Value := Value div 10;
  end;
  Tmp^ := Chr(Value + 48);
  if S<0 then
  begin
    Dec(Tmp);
    Tmp^ := '-';
  end;
  ZStrCopy(Dest,Tmp);
end;

{$ifndef minimal}
function GetPropRefAsString(const PRef : TZPropertyRef) : string;
begin
  Result := PRef.Component.Name + '.' + PRef.Prop.Name;
  case PRef.Component.GetProperties.GetByName(PRef.Prop.Name).PropertyType of
    zptColorf : Result := Result + '.' + Copy('RGBA',PRef.Index+1,1);
    zptVector3f : Result := Result + '.' + Copy('XYZ',PRef.Index+1,1);
    zptRectf : Result := Result + '.' + Copy('XYZW',PRef.Index+1,1);
  end;
end;
{$endif}

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

initialization

  //todo really need to register TLogicalGroup?
  Register(TLogicalGroup,LogicalGroupClassId);
    {$ifndef minimal}ComponentManager.LastAdded.ImageIndex:=4;{$endif}
    {$ifndef minimal}ComponentManager.LastAdded.ZClassName := 'Group';{$endif}

{$ifndef minimal}
finalization

  if Assigned(_ComponentManager) then
    _ComponentManager.Free;
{$endif}

end.
