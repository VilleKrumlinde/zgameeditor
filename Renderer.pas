{Copyright (c) Ville Krumlinde

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

unit Renderer;

{$include zzdc_globalopt.inc}

interface

uses {$ifndef minimal}SysUtils,{$endif}Meshes,ZClasses,ZBitmap,ZExpressions;

type
  TRenderCommand = class(TCommand);

  TFont = class;
  TShader = class;
  TRenderTarget = class;

  TMaterialShading = (msSmooth,msFlat,msWireframe,msPoint);
  TMaterialTexCoords = (tcGenerated,tcModelDefined);
  TMaterial = class(TZComponent)
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Textures : TZComponentList;
    Shading : TMaterialShading;
    Color : TZColorf;
    Light : boolean;
    Blend : (mbNoBlend,mbA_1MSA,mbA_1,mbC_1MSC,mbAlphaSat_1,mb1MSA_A);
    ZBuffer : boolean;
    DrawBackFace : boolean;
    Font : TFont;
    Shader : TShader;
    WireframeWidth : single;
    SpecularColor,EmissionColor : TZColorf;
    Shininess : integer;
    SkipSetColor : boolean;  //True if Color should not be used
  end;

  TMaterialTexture = class(TZComponent)
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Texture : TZBitmap;
    RenderTarget : TRenderTarget;
    TextureScale : TZVector3f;
    TextureX,TextureY : single;
    TextureRotate : single;
    TextureWrapMode : (tmMirror,tmTile,tmClamp);
    TexCoords : TMaterialTexCoords;
    Origin : TZVector3f;
    {$ifndef minimal}function GetDisplayName: AnsiString; override;{$endif}
  end;

  TShaderVariable = class(TZComponent)
  private
    Location : integer;
    TextureHandle : integer;
    TexWidth : integer;
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    VariableName : TPropString;
    Value : single;
    ValuePropRef : TZExpressionPropValue;
    ValueArrayRef : TDefineArray;
    ArrayKind : (sakTexture2D,sakMat4);
    VariableRef : TDefineVariable;
    procedure ResetGpuResources; override;
    {$ifndef minimal}
    function GetDisplayName: AnsiString; override;
    {$endif}
    destructor Destroy; override;
  end;

  {$ifndef minimal}
  EShaderException = class(Exception);
  {$endif}
  TShader = class(TZComponent)
  strict private
    VShaderHandle,FShaderHandle,GShaderHandle : cardinal;
    LastVariableUpdate : single;
    TexCount,FirstTexIndex : integer;
    procedure ReInit;
    procedure CleanUp;
    procedure UpdateVariableLocations;
    procedure UpdateVariableValues;
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    VertexShaderSource : TPropString;
    FragmentShaderSource : TPropString;
    GeometryShaderSource : TPropString;
    UniformVariables : TZComponentList;
    UpdateVarsOnEachUse : boolean;
    ProgHandle : integer;
    BeforeLinkExpression : TZExpressionPropValue;
    MvpLoc,MvLoc,ProjLoc,TexMatLoc,NormMatLoc,GlobColLoc : Integer;
    destructor Destroy; override;
    procedure ResetGpuResources; override;
    procedure DetachArrayVariables;
    procedure UseShader;
  end;

  //Render-commands
  TUseMaterial = class(TRenderCommand)
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Material : TMaterial;
    procedure Execute; override;
    {$ifndef minimal}function GetDisplayName: AnsiString; override;{$endif}
  end;

  TRenderMesh = class(TRenderCommand)
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Mesh : TMesh;
    procedure Execute; override;
    {$ifndef minimal}function GetDisplayName: AnsiString; override;{$endif}
  end;

  TRenderTransform = class(TRenderCommand)
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Scale : TZVector3f;
    Translate : TZVector3f;
    Rotate : TZVector3f;
    procedure Execute; override;
  end;

  TSpriteSheet = class;

  TRenderSprite = class(TRenderCommand)
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    SpriteSheet : TSpriteSheet;
    SpriteIndex : integer;
    MirrorHorizontal,MirrorVertical : boolean;
    procedure Execute; override;
  end;

  TSpriteSheet = class(TZComponent)
  strict private
  type
    PSpriteInfo = ^TSpriteInfo;
    TSpriteInfo = packed record
      SheetX,SheetY,SizeX,SizeY,OriginX,OriginY : smallint;
    end;
  var
    Material : TMaterial;
    function GetSpriteInfo(const Index : integer) : PSpriteInfo;
  protected
    procedure DefineProperties(List: TZPropertyList); override;
    procedure Render(Rs : TRenderSprite);
  public
    Bitmap : TZBitmap;
    SpriteData : TZBinaryPropValue;
    {$ifndef minimal}
    destructor Destroy; override;
    {$endif}
  end;

  TTileSet = class(TZComponent)
  private
    Material : TMaterial;
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Bitmap : TZBitmap;
    TileWidth,TileHeight,TileBorder : integer;
    {$ifndef minimal}
    constructor Create(OwnerList: TZComponentList); override;
    destructor Destroy; override;
    {$endif}
  end;

  TRenderTile = class(TRenderCommand)
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    TileSet : TTileSet;
    TileIndex,OriginX,OriginY : integer;
    MirrorHorizontal,MirrorVertical : boolean;
    procedure Execute; override;
  end;

  TRenderBeams = class(TRenderCommand)
  private
    Beams : TZArrayList;
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Count : integer;
    Length : single;
    Width : single;
    Speed : single;
    procedure Execute; override;
    procedure Update; override;
    constructor Create(OwnerList: TZComponentList); override;
    destructor Destroy; override;
  end;

  TRenderTransformGroup = class(TRenderCommand)
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Scale : TZVector3f;
    Translate : TZVector3f;
    Rotate : TZVector3f;
    Children : TZComponentList;
    procedure Execute; override;
    procedure Update; override;
  end;

  TFont = class(TZComponent)
  {$ifndef fpc}strict{$endif}private
    IsInit : boolean;
    FontSizes : array[0..2] of TZComponentList;
    BmStruct :
      record
        ScaleX,ScaleY : single;
        TransX,TransY : single;
        TopLeftY : single;
        CharsPerRow : integer;
      end;
    procedure InitFromBitmap;
    procedure InitBuiltIn;
  private
    procedure Prepare;
  protected
    procedure RenderCharacter(Char : integer; Size : integer; const IsFirst : boolean);
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Bitmap : TZBitmap;
    CharPixelWidth : integer;
    CharPixelHeight : integer;
    FirstChar : integer;
    BorderPixels : integer;
    destructor Destroy; override;
  end;

  TRenderText = class(TRenderCommand)
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Text : TPropString;               //Text att printa
    TextFloatRef : TZExpressionPropValue;     //Propvalue att printa, en av dessa gäller
    TextArrayRef : pointer;           //DefineArray to print
    X,Y,Scale : single;
    Align : (alCenter,alLeft);
    CharX,CharY,CharRotate,CharScale : single;
    CharI : integer;
    RenderCharExpression : TZExpressionPropValue;
    FloatMultiply : single;  //Multiplicera floatref innan print, för att visa decimaltal
    UseModelSpace : boolean;
    StretchY : single;
    procedure Execute; override;
    procedure ResetGpuResources; override;
    {$ifndef minimal}
    function GetDisplayName: AnsiString; override;
    {$endif}
  end;

  //Set the current GL-color
  TRenderSetColor = class(TRenderCommand)
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Color : TZColorf;
    procedure Execute; override;
  end;

  TRenderNet = class(TRenderCommand)
  private
    Mesh : TMesh;
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    XCount,YCount : integer;
    RenderVertexExpression : TZExpressionPropValue;
    Vertex : TZVector3f;
    TexCoord : TZVector3f;
    Color : TZColorf;
    VertexColors : boolean;
    procedure Execute; override;
    destructor Destroy; override;
  end;

  TRenderParticles = class(TRenderCommand)
  private
    RenderBuffer : PFloat;
    RenderBufferSize : integer;
    Particles : TZArrayList;
    EmitTime,Time : single;
    PColor : TZColorf;
    PAngle : single;
    function EmitParticle : TObject;
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    ParticlesPerSecond : single;
    Direction : single;
    Spread : single;
    ParticleWidth : single;
    ParticleHeight : single;
    ParticleLifetime : single;
    Speed,SpeedRange : single;
    Radius : single;
    AnimateAlpha,AnimateSize,Damping : single;
    Duration : single;
    BeginTime : single;
    Gravity : TZVector3f;
    FollowModel : boolean;
    OnEmitExpression : TZExpressionPropValue;
    procedure Execute; override;
    constructor Create(OwnerList: TZComponentList); override;
    destructor Destroy; override;
    procedure Update; override;
    {$ifndef minimal}
    procedure DesignerReset; override;
    {$endif}
  end;

  TRenderQuality = (rtsScreenSize,rtsHalfScreen,rtsQuartScreen,rts128,rts256,rts512,rts1024);
  TRenderTarget = class(TZComponent)
  strict private
    RboId : integer;
    {$ifndef minimal}
    LastW,LastH : integer;
    {$endif}
    procedure CleanUp;
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    ClearBeforeUse : boolean;
    AutoPowerOfTwo : boolean;
    ClearColor : TZColorf;
    Width,Height : TRenderQuality;
    CustomWidth,CustomHeight : integer;
    TexId,FboId : integer; //read by zgeviz
    Filter : TBitmapFilterType;
    InternalFormat : (infRGBA,infRGBA16);
    destructor Destroy; override;
    procedure ResetGpuResources; override;
    procedure UseTextureBegin;
    procedure Activate{$ifndef minimal}(const NoApp : boolean = False){$endif};
  end;

  TSetRenderTarget = class(TRenderCommand)
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    RenderTarget : TRenderTarget;
    procedure Execute; override;
    {$ifndef minimal}function GetDisplayName: AnsiString; override;{$endif}
  end;

  TLight = class(TZComponent)
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Position : TZVector4f;
    Color : TZColorf;
    Enabled : boolean;
    Kind : (likDirectional,likPoint,likSpot);
    SpotDirection : TZVector3f;
    SpotExponent : single;
    SpotCutoff : single;
    procedure ApplyLight(const LightId : integer);
    procedure RemoveLight(const LightId : integer);
  end;

procedure RenderModel(Model : TModel);
procedure ApplyModelTransform(Model : TModel);

{$ifndef minimal}
procedure CleanUp;
procedure DesignerRenderStop;

var
  IsRendering : boolean;
  NormalsVisible : boolean;
  CollisionBoundsVisible : boolean;
{$endif}

var
  CurrentRenderTarget : TRenderTarget;
  DefaultMaterial : TMaterial = nil;
  DefaultMaterialTexture : TMaterialTexture = nil;


implementation

{$POINTERMATH ON}

uses ZOpenGL, ZMath, ZApplication, ZPlatform, ZLog, GLDrivers;

var
  DefaultFont : TFont = nil;


{ TRenderer }

{$ifndef minimal}
//Draw area around collision bounds
//Code originally in Z-script by Kjell
procedure RenderCollisionBounds(const Style : integer;
   const Rx,Ry,Rz : single;
   const Sx,Sy,Sz : single;
   const Bx,By,Bz : single;
   const Ox,Oy,Oz : single);

  procedure renderRect(const W,H : single);
  begin
    glBegin(2);
    glVertex2f(W   ,H   );
    glVertex2f(W*-1,H   );
    glVertex2f(W*-1,H*-1);
    glVertex2f(W   ,H*-1);
    glEnd();
  end;

  procedure renderCircle(const S : single);
  var
    X,R : single;
    I : integer;
  begin
    glBegin(2);
    X := 0;
    for I := 0 to 31 do
    begin
      R := X*PI*2;
      glVertex2f(sin(R)*S,cos(R)*S);
      X := X + 1/32;
    end;
    glEnd();
  end;


begin
  glPushAttrib(GL_ALL_ATTRIB_BITS);

  glColor3f(1,0,0);
  glLineWidth(1);

  glDisable(GL_LIGHTING);
  glDisable(GL_DEPTH_TEST);
  glDisable(GL_BLEND);
  glPushMatrix();

//  glScalef(1/SX,1/SY,1/SZ);
//  glRotatef(RZ*360,0,0,-1);
//  glRotatef(RY*360,0,-1,0);
//  glRotatef(RX*360,-1,0,0);
  glTranslatef(OX,OY,OZ);

  case Style of
    0: // Rect2D
      renderRect(BX*0.5,BY*0.5);

    1: // Sphere3D
      begin
        renderCircle(BX);
        glRotatef(90,1,0,0);
        renderCircle(BX);
        glRotatef(90,0,1,0);
        renderCircle(BX);
      end;

    2: // Box3D
      begin
        glTranslatef(0,0,BZ);
        renderRect(BX,BY);
        glTranslatef(0,0,BZ*-2);
        renderRect(BX,BY);
        glBegin(1);
        glVertex3f(BX   ,BY   ,0   );
        glVertex3f(BX   ,BY   ,BZ*2);
        glVertex3f(BX*-1,BY   ,0   );
        glVertex3f(BX*-1,BY   ,BZ*2);
        glVertex3f(BX*-1,BY*-1,0   );
        glVertex3f(BX*-1,BY*-1,BZ*2);
        glVertex3f(BX   ,BY*-1,0   );
        glVertex3f(BX   ,BY*-1,BZ*2);
        glEnd();
      end;

    3: // Rect2D_OBB
      begin
        glRotatef(RZ*360,0,0,1);
        glScalef(SX,SY,0);
        renderRect(BX*0.5,BY*0.5);
      end;

    4: // Circle2D
      renderCircle(BX);
  end;

  glPopMatrix();
  glPopAttrib;
end;
{$endif}

procedure ApplyModelTransform(Model : TModel);
var
  Driver : TGLDriverBase;
begin
  Driver := Model.ZApp.Driver;
  Driver.Translate(Model.Position[0],Model.Position[1],Model.Position[2]);
  Driver.ApplyRotation(Model.Rotation);
  Driver.UpdateNormalMatrix;
  if not VecIsIdentity3(Model.Scale) then
    Driver.Scale(Model.Scale[0],Model.Scale[1],Model.Scale[2]);
end;

procedure RenderModel(Model : TModel);
var
  Driver : TGLDriverBase;
begin
  Driver := Model.ZApp.Driver;

  Driver.PushMatrix();

  ApplyModelTransform(Model);

  Model.RunRenderCommands;

  Driver.PopMatrix();

  {$ifndef minimal}
  if CollisionBoundsVisible then
  begin
    Driver.PushMatrix();
    Driver.Translate(Model.Position[0],Model.Position[1],Model.Position[2]);
    RenderCollisionBounds( Ord(Model.CollisionStyle),
             Model.Rotation[0],
             Model.Rotation[1],
             Model.Rotation[2],
             Model.Scale[0],
             Model.Scale[1],
             Model.Scale[2],
             Model.CollisionBounds.Area[0],
             Model.CollisionBounds.Area[1],
             Model.CollisionBounds.Area[2],
             Model.CollisionOffset[0],
             Model.CollisionOffset[1],
             Model.CollisionOffset[2]);
    Driver.PopMatrix();
  end;
  {$endif}
end;


{ TMaterial }

procedure TMaterial.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Textures',{$ENDIF}(@Textures), zptComponentList);
    {$ifndef minimal}List.GetLast.SetChildClasses([TMaterialTexture]);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'WireframeWidth',{$ENDIF}(@WireframeWidth), zptFloat);
    List.GetLast.DefaultValue.FloatValue:=1.0;
  List.AddProperty({$IFNDEF MINIMAL}'Shading',{$ENDIF}(@Shading), zptByte);
    {$ifndef minimal}List.GetLast.SetOptions(['Smooth','Flat','Wireframe','Point']);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'Color',{$ENDIF}(@Color), zptColorf);
    List.GetLast.DefaultValue.ColorfValue := TZColorf(ZMath.UNIT_XYZ4);
  List.AddProperty({$IFNDEF MINIMAL}'Light',{$ENDIF}(@Light), zptBoolean);
    List.GetLast.DefaultValue.BooleanValue:=True;
  List.AddProperty({$IFNDEF MINIMAL}'SpecularColor',{$ENDIF}(@SpecularColor), zptColorf);
  List.AddProperty({$IFNDEF MINIMAL}'EmissionColor',{$ENDIF}(@EmissionColor), zptColorf);
  List.AddProperty({$IFNDEF MINIMAL}'Shininess',{$ENDIF}(@Shininess), zptInteger);
  List.AddProperty({$IFNDEF MINIMAL}'Blend',{$ENDIF}(@Blend), zptByte);
    {$ifndef minimal}List.GetLast.SetOptions(['None','Alpha/OneMinusSourceAlpha','Alpha/One','Color/OneMinusSourceColor','AlphaSaturate/One','OneMinusSourceAlpha/Alpha']);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'ZBuffer',{$ENDIF}(@ZBuffer), zptBoolean);
    List.GetLast.DefaultValue.BooleanValue:=True;
  List.AddProperty({$IFNDEF MINIMAL}'DrawBackFace',{$ENDIF}(@DrawBackFace), zptBoolean);
  List.AddProperty({$IFNDEF MINIMAL}'Font',{$ENDIF}(@Font), zptComponentRef);
    {$ifndef minimal}List.GetLast.SetChildClasses([TFont]);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'Shader',{$ENDIF}(@Shader), zptComponentRef);
    {$ifndef minimal}List.GetLast.SetChildClasses([TShader]);{$endif}
end;


{ TUseMaterial }

procedure TUseMaterial.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Material',{$ENDIF}(@Material), zptComponentRef);
    {$ifndef minimal}List.GetLast.SetChildClasses([TMaterial]);{$endif}
    {$ifndef minimal}List.GetLast.NeedRefreshNodeName := True;{$endif}
end;

procedure TUseMaterial.Execute;
begin
  Self.ZApp.Driver.EnableMaterial(Self.Material);
end;

{$ifndef minimal}
function TUseMaterial.GetDisplayName: AnsiString;
begin
  Result := inherited GetDisplayName;
  if Assigned(Material) then
    Result := Result + '  ' + Material.Name;
end;
{$endif}

{ TRenderMesh }

procedure TRenderMesh.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Mesh',{$ENDIF}(@Mesh), zptComponentRef);
    {$ifndef minimal}List.GetLast.SetChildClasses([TMesh]);{$endif}
    {$ifndef minimal}List.GetLast.NeedRefreshNodeName := True;{$endif}
end;

procedure TRenderMesh.Execute;
begin
  {$ifndef minimal}
  if Mesh=nil then
    Exit;
  {$endif}
  Self.ZApp.Driver.RenderMesh(Mesh);
end;

{$ifndef minimal}
function TRenderMesh.GetDisplayName: AnsiString;
begin
  Result := inherited GetDisplayName;
  if Assigned(Mesh) then
    Result := Result + '  ' + Mesh.Name;
end;
{$endif}

{ TRenderTransform }

procedure TRenderTransform.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Scale',{$ENDIF}(@Scale), zptVector3f);
    List.GetLast.DefaultValue.Vector3fValue := ZMath.UNIT_XYZ3;
  List.AddProperty({$IFNDEF MINIMAL}'Translate',{$ENDIF}(@Translate), zptVector3f);
  List.AddProperty({$IFNDEF MINIMAL}'Rotate',{$ENDIF}(@Rotate), zptVector3f);
end;

procedure TRenderTransform.Execute;
//Transforms the current matrix
var
  Driver : TGLDriverBase;
begin
  Driver := Self.ZApp.Driver;
  Driver.Translate(Translate[0],Translate[1],Translate[2]);
  Driver.ApplyRotation(Rotate);
  if not VecIsIdentity3(Scale) then
    Driver.Scale(Scale[0],Scale[1],Scale[2]);
end;

{ TRenderTransformGroup }

procedure TRenderTransformGroup.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Scale',{$ENDIF}(@Scale), zptVector3f);
    List.GetLast.DefaultValue.Vector3fValue := ZMath.UNIT_XYZ3;
  List.AddProperty({$IFNDEF MINIMAL}'Translate',{$ENDIF}(@Translate), zptVector3f);
  List.AddProperty({$IFNDEF MINIMAL}'Rotate',{$ENDIF}(@Rotate), zptVector3f);
  List.AddProperty({$IFNDEF MINIMAL}'Children',{$ENDIF}(@Children), zptComponentList);
end;

procedure TRenderTransformGroup.Execute;
var
  Driver : TGLDriverBase;
begin
  Driver := Self.ZApp.Driver;
  Driver.PushMatrix();
    Driver.Translate(Translate[0],Translate[1],Translate[2]);
    Driver.ApplyRotation(Rotate);
    if not VecIsIdentity3(Scale) then
      Driver.Scale(Scale[0],Scale[1],Scale[2]);
    Children.ExecuteCommands;
  Driver.PopMatrix();
end;

procedure TRenderTransformGroup.Update;
begin
  inherited;
  Children.Update;
end;

{ TRenderSprite }


procedure TRenderSprite.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'SpriteSheet',{$ENDIF}(@SpriteSheet), zptComponentRef);
    {$ifndef minimal}List.GetLast.SetChildClasses([TSpriteSheet]);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'SpriteIndex',{$ENDIF}(@SpriteIndex), zptInteger);
  List.AddProperty({$IFNDEF MINIMAL}'MirrorHorizontal',{$ENDIF}(@MirrorHorizontal), zptBoolean);
  List.AddProperty({$IFNDEF MINIMAL}'MirrorVertical',{$ENDIF}(@MirrorVertical), zptBoolean);
end;

procedure TRenderSprite.Execute;
var
  Driver : TGLDriverBase;
begin
  Driver := Self.ZApp.Driver;

  if Assigned(Self.SpriteSheet) then
    Self.SpriteSheet.Render(Self)
  else
    Driver.RenderUnitQuad;
end;

{ TRenderBeams }

type
  TBeam = class
  private
    Width,Angle : single;
    AngleInc : single;
  end;


constructor TRenderBeams.Create(OwnerList: TZComponentList);
begin
  inherited Create(OwnerList);
  Beams := TZArrayList.Create;
end;

destructor TRenderBeams.Destroy;
begin
  Beams.Free;
  inherited;
end;

procedure TRenderBeams.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Count',{$ENDIF}(@Count), zptInteger);
  List.AddProperty({$IFNDEF MINIMAL}'Length',{$ENDIF}(@Length), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}'Width',{$ENDIF}(@Width), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}'Speed',{$ENDIF}(@Speed), zptFloat);
    List.GetLast.DefaultValue.FloatValue := 2;
end;


procedure TRenderBeams.Execute;
var
  I : integer;
  B : TBeam;
  Angle,X,Y,C,S : single;
  Mem : pointer;
  Vp,Tp,V,T : PFloat;
  procedure Vert(const T1,T2,V1,V2 : single);
  begin
    Tp^ := T1; Inc(Tp);
    Tp^ := T2; Inc(Tp);
    Vp^ := V1; Inc(Vp);
    Vp^ := V2; Inc(Vp);
  end;
begin
  GetMem(Mem,Beams.Count * 3 * ((2+2)*SizeOf(single)));
  Vp := Mem;
  Tp := pointer(NativeInt(Vp) + Beams.Count * 3 * (2*SizeOf(single)));

  V := Vp; T := Tp;

  for I := 0 to Beams.Count-1 do
  begin
    B := TBeam(Beams[I]);
    Vert(0.52,0.52,0,0);

    Angle := B.Angle-B.Width/2;
    C := cos(Angle);
    S := sin(Angle);
    X := C * Length;
    Y := S * Length;
    Vert(0.5 + C/2 , 0.5 + S/2, X,Y);

    Angle := B.Angle+B.Width/2;
    C := cos(Angle);
    S := sin(Angle);
    X := C * Length;
    Y := S * Length;
    Vert(0.5 + C/2 , 0.5 + S/2, X,Y);
  end;

  Self.ZApp.Driver.RenderArrays(GL_TRIANGLES,Beams.Count*3,2,V,T,nil);

  FreeMem(Mem);
end;

procedure TRenderBeams.Update;
var
  B : TBeam;
  I : integer;
  WRange : single;
begin
  {$ifndef minimal}
  if IsChanged then
  begin
    //Nollställ ifall ändrad i designer
    Beams.Clear;
    IsChanged := False;
  end;
  {$endif}
  //Emit new beams
  while Beams.Count<Count do
  begin
    B := TBeam.Create;
    WRange := Width*0.1; //Use +/- 10 percent width diff
    B.Width := ZMath.Random(Width,WRange);
    B.Angle := System.Random * (2*PI);
    B.AngleInc := Random(Speed,Speed*0.5);
    if System.Random<0.5 then
      B.AngleInc := B.AngleInc * -1;
    Beams.Add(B);
  end;
  //Update beams
  for I := 0 to Beams.Count-1 do
  begin
    B := TBeam(Beams[I]);
    B.Angle := B.Angle + B.AngleInc * ZApp.DeltaTime;
  end;
end;


{ TRenderText }

procedure TRenderText.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Text',{$ENDIF}(@Text), zptString);
    List.GetLast.IsManagedTarget := True;
    {$ifndef minimal}List.GetLast.NeedRefreshNodeName:=True;{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'TextFloatRef',{$ENDIF}(@TextFloatRef), zptExpression);
    {$ifndef minimal}List.GetLast.ExpressionKind := ekiGetValue;{$endif}
    {$ifndef minimal}List.GetLast.NeedRefreshNodeName:=True;{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'TextArray',{$ENDIF}(@TextArrayRef), zptComponentRef);
    {$ifndef minimal}List.GetLast.SetChildClasses([TDefineArray]);{$endif}
    {$ifndef minimal}List.GetLast.NeedRefreshNodeName:=True;{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'X',{$ENDIF}(@X), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}'Y',{$ENDIF}(@Y), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}'Scale',{$ENDIF}(@Scale), zptFloat);
    List.GetLast.DefaultValue.FloatValue := 1;
  List.AddProperty({$IFNDEF MINIMAL}'Align',{$ENDIF}(@Align), zptByte);
    {$ifndef minimal}List.GetLast.SetOptions(['Center','Left']);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'RenderCharExpression',{$ENDIF}(@RenderCharExpression), zptExpression);
    {$ifndef minimal}
    List.GetLast.DefaultValue.ExpressionValue.Source :=
      '//Modify current character before render.'#13#10 +
      '//CharX,CharY : current coordinate'#13#10 +
      '//CharI : current character index (read only)'#13#10 +
      '//CharRotate : current character rotation in radians'#13#10 +
      '//CharScale : current character scale';
    {$endif}
  List.AddProperty({$IFNDEF MINIMAL}'FloatMultiply',{$ENDIF}(@FloatMultiply), zptFloat);
    List.GetLast.DefaultValue.FloatValue := 1;
  List.AddProperty({$IFNDEF MINIMAL}'UseModelSpace',{$ENDIF}(@UseModelSpace), zptBoolean);
  List.AddProperty({$IFNDEF MINIMAL}'StretchY',{$ENDIF}(@StretchY), zptFloat);
    List.GetLast.DefaultValue.FloatValue := 1;

  List.AddProperty({$IFNDEF MINIMAL}'CharX',{$ENDIF}(@CharX), zptFloat);
    List.GetLast.NeverPersist:=True;
  List.AddProperty({$IFNDEF MINIMAL}'CharY',{$ENDIF}(@CharY), zptFloat);
    List.GetLast.NeverPersist:=True;
  List.AddProperty({$IFNDEF MINIMAL}'CharI',{$ENDIF}(@CharI), zptInteger);
    List.GetLast.NeverPersist:=True;
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'CharRotate',{$ENDIF}(@CharRotate), zptFloat);
    List.GetLast.NeverPersist:=True;
  List.AddProperty({$IFNDEF MINIMAL}'CharScale',{$ENDIF}(@CharScale), zptFloat);
    List.GetLast.NeverPersist:=True;
end;

procedure TRenderText.Execute;
const
  BuiltInSpacing = -0.18; //Slight overlap to put chars closer to each other (for built-in font)
  CharsScreen = 20; //Scale 1 = 20 characters on screen
var
  CurChar,CharLen,I,ArrayLimit : integer;
  P : PByte;
  XStep,StartX : single;
  Spacing,YScaleFactor : single;
  ArrayRef : TDefineArray;
  PInt : ^integer;
  TheText : PByte;
  FontSize : integer;
  CurFont : TFont;
  UseBuiltInFont : boolean;
  Driver : TGLDriverBase;
  FloatBuf : array[0..19] of ansichar;
  TextBuf : array[0..(8*1024-1)] of ansichar;
begin
  Driver := Self.ZApp.Driver;
  UseBuiltInFont := (Driver.CurrentMaterial=nil) or (Driver.CurrentMaterial.Font=nil);

  if not UseBuiltInFont then
  begin
    CurFont := Driver.CurrentMaterial.Font;
    Spacing := 0;
  end
  else
  begin
    if DefaultFont=nil then
      DefaultFont := TFont.Create(nil);
    DefaultFont._ZApp := Self.ZApp;
    CurFont := DefaultFont;
    Spacing := BuiltInSpacing;
  end;
  CurFont.Prepare;

  if TextFloatRef.Code.Count>0 then
  begin
    //If textref is set then convert float-value to string
    ZStrConvertInt(
      Trunc(
        ExpGetValue(TextFloatRef.Code)
        * Self.FloatMultiply
      ),
      PAnsiChar(@FloatBuf));
    if pointer(Self.Text)<>nil then
      ZStrCopy(TextBuf,PAnsiChar(Self.Text))
    else
      TextBuf[0]:=#0;
    ZStrCat(TextBuf,PAnsiChar(@FloatBuf));
    TheText := @TextBuf;
  end else if TextArrayRef<>nil then
  begin
    ArrayRef := TDefineArray(TextArrayRef);
    ArrayLimit := ArrayRef.CalcLimit;
    PInt := pointer(ArrayRef.GetData);
    I := 0;
    while (I<High(TextBuf)-1) and (I<ArrayLimit) and (PInt^<>0) do
    begin
      TextBuf[I] := AnsiChar(PInt^);
      Inc(PInt);
      Inc(I);
    end;
    TextBuf[I] := #0;
    TheText := @TextBuf;
  end
  else
    TheText := PByte(Text);

  if TheText=nil then
    Exit;

  //todo move to Begin2D(), End2D()-procs
  if not Self.UseModelSpace then
  begin
    Driver.MatrixMode(GL_PROJECTION);
    Driver.PushMatrix();
    Driver.LoadIdentity();
    Driver.MatrixMode(GL_MODELVIEW);
    Driver.PushMatrix();
    Driver.LoadIdentity();
    //Must account for viewpointratio here because of the -1 .. 1 resolution
    YScaleFactor := ZApp.ActualViewportRatio;
  end
  else
  begin
    Driver.PushMatrix();
    YScaleFactor := 1;
  end;


  glPushAttrib(GL_TEXTURE_BIT);

  glEnable(GL_TEXTURE_2D);
  //Force disable automatic tex-coords
  glDisable(GL_TEXTURE_GEN_S);
  glDisable(GL_TEXTURE_GEN_T);

  Driver.Translate(Self.X,Self.Y,0);
  //Scale 1 = 20 characters width on screen
  Driver.Scale(Scale * 2/(CharsScreen+CharsScreen*Spacing),
    Scale * 2/(CharsScreen+CharsScreen*Spacing) * YScaleFactor * StretchY,
    1);

  //Font pixel size
  if not Self.UseModelSpace then
    FontSize := Round(Scale * (ZApp.ScreenWidth / CharsScreen))
  else
    FontSize := 16;

  //Get length of first line in string
  P := pointer(TheText);
  while not (P^ in [0,13]) do
    Inc(P);
  CharLen := NativeUInt(P) - NativeUInt(TheText);

  XStep := 1.0 + Spacing;

  case Align of
    alCenter : StartX := XStep*0.5 - CharLen*XStep*0.5;
  else
    StartX := XStep*0.5;  //alLeft
  end;
  CharX := StartX;

  CharY := 0;
  CharI := 0;
  CharRotate := 0;
  CharScale := 1;
  P := pointer(TheText);
  while (P^<>0) do
  begin
    if P^=13 then
    begin
      CharX := StartX;
      CharY := CharY - 1.0;
      Inc(P,1);
      if P^=10 then
        Inc(P);
      Continue;
    end;

    CurChar := P^;

    ZExpressions.RunCode(RenderCharExpression.Code);

    Driver.PushMatrix;
      Driver.Translate(CharX,CharY,0);
      Driver.Scale(CharScale,CharScale,1);
      Driver.Rotate(CharRotate*180/PI,0,0,1);
      CurFont.RenderCharacter(CurChar,FontSize, CharI=0);
    Driver.PopMatrix;

    CharX := CharX + XStep;
    CharI := CharI + 1;
    Inc(P);
  end;

  //Need to disable textures again because enable-bit is not saved in pushattrib
  glDisable(GL_TEXTURE_2D);

  glPopAttrib;

  // restore matrices, todo End2D()-proc
  if not Self.UseModelSpace then
  begin
    Driver.PopMatrix();
    Driver.MatrixMode(GL_PROJECTION);
    Driver.PopMatrix();
    Driver.MatrixMode(GL_MODELVIEW);
  end else
    Driver.PopMatrix();

end;

procedure TRenderText.ResetGpuResources;
begin
  if Assigned(DefaultFont) then
  begin
    DefaultFont.Free;
    DefaultFont := nil;
  end;
end;

{$ifndef minimal}
function TRenderText.GetDisplayName: AnsiString;
begin
  Result := inherited GetDisplayName;
  if Text<>'' then
    Result := Result + '  ' + Text;
  Result := Result + '  ' + AnsiString(TextFloatRef.Source);
end;
{$endif}


{ TFont }

const
  FontMinSize1 = 16;

procedure TFont.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Bitmap',{$ENDIF}(@Bitmap), zptComponentRef);
    {$ifndef minimal}List.GetLast.SetChildClasses([TZBitmap]);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'FirstChar',{$ENDIF}(@FirstChar), zptInteger);
  List.AddProperty({$IFNDEF MINIMAL}'CharPixelWidth',{$ENDIF}(@CharPixelWidth), zptInteger);
  List.AddProperty({$IFNDEF MINIMAL}'CharPixelHeight',{$ENDIF}(@CharPixelHeight), zptInteger);
  List.AddProperty({$IFNDEF MINIMAL}'BorderPixels',{$ENDIF}(@BorderPixels), zptInteger);
end;

destructor TFont.Destroy;
var
  I : integer;
begin
  for I:=0 to High(FontSizes) do
    FontSizes[I].Free;
  inherited;
end;

procedure TFont.Prepare;
begin
  if (not IsInit) {$ifndef minimal}or (IsChanged){$endif} then
  begin
    if Bitmap=nil then
      InitBuiltIn
    else
      InitFromBitmap;
    IsInit := True;
    {$ifndef minimal}
    IsChanged := False;
    {$endif}
  end;
end;

procedure TFont.InitFromBitmap;
var
  Px,Py : single;
begin
  Px := (1.0/Self.Bitmap.PixelWidth);
  Py := (1.0/Self.Bitmap.PixelHeight);
  BmStruct.CharsPerRow := Bitmap.PixelWidth div (Self.CharPixelWidth+Self.BorderPixels);
  BmStruct.ScaleX := Self.CharPixelWidth * Px;
  //NRows := Bitmap.PixelHeight div Self.CharPixelHeight;
  BmStruct.ScaleY := Self.CharPixelHeight * Py;
  BmStruct.TransX := (Self.CharPixelWidth+Self.BorderPixels) * Px;
  BmStruct.TransY := (Self.CharPixelHeight+Self.BorderPixels) * Py;
  //1.0 är topleft för textures, om det finns borderpixels i bitmappen
  //så ska dessa vara under bokstäverna.
  BmStruct.TopLeftY := 1.0 + Self.BorderPixels * Py;
end;

procedure TFont.RenderCharacter(Char, Size : integer; const IsFirst : boolean);
var
  Characters : TZComponentList;
  CurSize,FontSize : integer;
  IsBuiltIn : boolean;
  B : TZBitmap;
  Driver : TGLDriverBase;
begin
  IsBuiltIn := Self.Bitmap=nil;
  Driver := Self.ZApp.Driver;

  if IsBuiltIn then
  begin
    //Use builtin truetype font (win32 only)
    {$ifndef Win32}
    Exit;
    {$endif}
    Dec(Char,33);
    //Pick the right font for the size
    FontSize := 0;
    CurSize := FontMinSize1;
    while (FontSize<High(FontSizes)) and (CurSize<Size) do
    begin
      Inc(FontSize);
      Inc(CurSize,CurSize);
    end;

    Characters := FontSizes[FontSize];
    if (Char>=0) and (Char<Characters.Count) then
    begin
      B := TZBitmap(Characters[Char]);
      B.UseTextureBegin;
      Driver.RenderUnitQuad;
      //B.UseTextureEnd;
    end;
  end else
  begin
    //Use imported texture
    Dec(Char,Self.FirstChar);
    if Char>=0 then
    begin
      Driver.MatrixMode(GL_TEXTURE);
      Driver.PushMatrix();
      Driver.LoadIdentity();
      {$ifndef minimal} //Avoid potential div by zero
      if BmStruct.CharsPerRow>0 then
      {$endif}
        Driver.Translate( BmStruct.TransX*(Char mod BmStruct.CharsPerRow) ,
          BmStruct.TopLeftY - (BmStruct.TransY*((Char div BmStruct.CharsPerRow)+1)),0);
      Driver.Scale(BmStruct.ScaleX,BmStruct.ScaleY,1);
      Driver.MatrixMode(GL_MODELVIEW);
        if IsFirst then
          Bitmap.UseTextureBegin;  //Only switch texture on first call to reduce state changes
        Driver.RenderUnitQuad;
        //Bitmap.UseTextureEnd;
      Driver.MatrixMode(GL_TEXTURE);
      Driver.PopMatrix();
      Driver.MatrixMode(GL_MODELVIEW);
    end;
  end;
end;

procedure TFont.InitBuiltIn;
{
  Obs, ingen alpha verkar följa med tecknen när de kopieras från framebuffer.
  Måste blendas med color istället.
}
const
  FirstCharBuiltIn = 33; //Avoid name-clash with Self.FirstChar
  LastChar = 127;
var
  Lists : integer;
  B : TZBitmap;
  J,I,CharCount : integer;
  RasterPos : TZVector4f;
  CharWidth : single;
  CurSize1 : integer;
  CurSize2 : integer;
  Characters : TZComponentList;
begin
  {$ifndef minimal} //Undvik init fler gånger
  if Assigned(FontSizes[0]) then Exit;
  {$endif}

  //ladda font
  CharCount := LastChar-FirstCharBuiltIn+1;
  CurSize1 := FontMinSize1;
  CurSize2 := FontMinSize1;
  for J := 0 to High(FontSizes) do
  begin
    Lists := Platform_GenerateFontDisplayLists(CurSize2,FirstCharBuiltIn,LastChar);
    Characters := TZComponentList.Create;
    FontSizes[J] := Characters;
    {$ifndef Win32}
    Continue; //Built-in only work on win32
    {$endif}
    for I := 0 to CharCount-1 do
    begin
      B:=TZBitmap.Create(Characters);
      B.Height:=CurSize1;
      B.Width:=CurSize1;
      B.RenderTargetBegin;

      glClearColor(0,0,0,0);
      glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT or GL_STENCIL_BUFFER_BIT);

      //Draw each letter twice to get letter width
      glColor3f(1,1,1);

      glRasterPos2f(-1,0);
      glCallList(Lists + I);
      glGetFloatv(GL_CURRENT_RASTER_POSITION,@RasterPos);
      //Rasterpos is in view coords. Current view (texture-target) has width FontSize.
      CharWidth := RasterPos[0]/CurSize2;
      //CharWidths[ Char(I) ] := CharWidth;
      glRasterPos2f( Clamp(0 - CharWidth,-1,1),-0.7);

      glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT or GL_STENCIL_BUFFER_BIT);

      glCallList(Lists + I);

      B.RenderTargetEnd;
    end;

    glDeleteLists(Lists,CharCount);
    Inc(CurSize1,CurSize1);
    Inc(CurSize2,CurSize2);
  end;
end;

{ TRenderSetColor }

procedure TRenderSetColor.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Color',{$ENDIF}(@Color), zptColorf);
end;

procedure TRenderSetColor.Execute;
begin
  Self.ZApp.Driver.SetColor(Self.Color);
end;

{ TRenderNet }

procedure TRenderNet.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'XCount',{$ENDIF}(@XCount), zptInteger);
  List.AddProperty({$IFNDEF MINIMAL}'YCount',{$ENDIF}(@YCount), zptInteger);
  List.AddProperty({$IFNDEF MINIMAL}'RenderVertexExpression',{$ENDIF}(@RenderVertexExpression), zptExpression);
    {$ifndef minimal}
    List.GetLast.DefaultValue.ExpressionValue.Source :=
      '//Update each vertex.'#13#10 +
      '//Vertex : current vertex'#13#10 +
      '//TexCoord : current texture coordinate'#13#10 +
      '//Color : current vertex color';
    {$endif}
  List.AddProperty({$IFNDEF MINIMAL}'VertexColors',{$ENDIF}(@VertexColors), zptBoolean);
  List.AddProperty({$IFNDEF MINIMAL}'Vertex',{$ENDIF}(@Vertex), zptVector3f);
    List.GetLast.NeverPersist:=True;
  List.AddProperty({$IFNDEF MINIMAL}'TexCoord',{$ENDIF}(@TexCoord), zptVector3f);
    List.GetLast.NeverPersist:=True;
  List.AddProperty({$IFNDEF MINIMAL}'Color',{$ENDIF}(@Color), zptColorf);
    List.GetLast.NeverPersist := True;
end;

destructor TRenderNet.Destroy;
begin
  if Mesh<>nil then
    Mesh.Free;
  inherited;
end;

procedure TRenderNet.Execute;
var
  I : integer;
  P : PZVector3f;
  Tex : PZVector2f;
  PColor : PMeshVertexColor;
begin
  if (Mesh=nil) then
  begin
    Mesh := TMesh.Create(nil);
  end;

  Mesh.MakeNet(XCount,YCount);
  Mesh.IsDynamic := True;

  if VertexColors and (Mesh.Colors=nil) then
    GetMem(Mesh.Colors,Mesh.VerticesCount * 4);
  PColor := PMeshVertexColor(Mesh.Colors);

  if (RenderVertexExpression.Code<>nil) then
  begin
    P := pointer(Mesh.Vertices);
    Tex := pointer(Mesh.TexCoords);
    for I := 0 to Mesh.VerticesCount-1 do
    begin
      Vertex := P^;
      //TexCoord tvingas till en 2f, är 3f enligt properties pga 2f saknas i proptypes
      PZVector2f(@TexCoord)^ := Tex^;
      ZExpressions.RunCode(RenderVertexExpression.Code);
      Tex^ := PZVector2f(@TexCoord)^;
      P^ := Vertex;
      if VertexColors then
      begin
        PColor^ := ColorFtoB(Self.Color);
        Inc(PColor);
      end;
      Inc(P);
      Inc(Tex);
    end;
  end;

  //Recompute normals, the vertice have changed
  Mesh.ComputeNormals;

  Self.ZApp.Driver.RenderMesh(Mesh);
end;


{ TRenderParticles }

type
  TParticle = class
  private
    LifeTime : single;
    Position,Velocity: TZVector2f;
    Width,Height : single;
    Color : TZColorf;
  end;

constructor TRenderParticles.Create(OwnerList: TZComponentList);
begin
  inherited Create(OwnerList);
  Particles := TZArrayList.Create;
end;

destructor TRenderParticles.Destroy;
begin
  Particles.Free;
  FreeMem(RenderBuffer);
  inherited;
end;

procedure TRenderParticles.Execute;
var
  I : integer;
  P : TParticle;
  X,Y,W,H : single;
  Tmp : TZVector2f;
  ArV,ArT : PFloat;
  ArC : PInteger;
  MemSize,ColorB : integer;
  V,T,C : pointer;

  procedure Vert(const T1,T2,V1,V2 : single);
  begin
    ArT^ := T1; Inc(ArT);
    ArT^ := T2; Inc(ArT);
    ArV^ := V1; Inc(ArV);
    ArV^ := V2; Inc(ArV);
    ArC^ := ColorB; Inc(ArC);
  end;

begin
  if not Self.FollowModel then
  begin
    if CurrentModel<>nil then
    begin
      Self.ZApp.Driver.PushMatrix;
      Tmp := PZVector2f(@CurrentModel.Position)^;
      Self.ZApp.Driver.Translate(-Tmp[0],-Tmp[1],0);
    end;
  end;

  if Particles.Count>0 then
  begin
    //Every particle has 6 vertex (two tris). Per vertex: 2 floats coords, 2 float tex coords, 1 integer color
    MemSize := Particles.Count * 6 * (SizeOf(Single)*4 + SizeOf(Integer));
    if MemSize>Self.RenderBufferSize then
    begin
      ReAllocMem(Self.RenderBuffer,MemSize);
      Self.RenderBufferSize := MemSize;
    end;
    ArV := Self.RenderBuffer;
    ArT := Pointer(NativeInt(ArV) + (Particles.Count*6*SizeOf(Single)*2) );
    ArC := Pointer(NativeInt(ArT) + (Particles.Count*6*SizeOf(Single)*2) );

    V := ArV; T := ArT; C := ArC;

    for I := 0 to Particles.Count-1 do
    begin
      P := TParticle(Particles[I]);

      ColorB := ColorFtoB(P.Color);

      X := P.Position[0];
      Y := P.Position[1];
      W := P.Width * 0.5;
      H := P.Height * 0.5;

      //Triangle 1
      //..
      //x.
      Vert(0,0,X-W,Y-H);
      //..
      //.x
      Vert(1,0,X+W,Y-H);
      //.x
      //..
      Vert(1,1,X+W,Y+H);

      //Triangle 2
      //x.
      //..
      Vert(0,1,X-W,Y+H);
      Vert(0,0,X-W,Y-H);
      Vert(1,1,X+W,Y+H);
    end;

    Self.ZApp.Driver.RenderArrays(GL_TRIANGLES,Particles.Count*6,2,V,T,C);
  end;

  //Restore default color
  glColor3fv(@ZMath.UNIT_XYZ3);

  if not Self.FollowModel and (CurrentModel<>nil) then
     Self.ZApp.Driver.PopMatrix;
end;


function TRenderParticles.EmitParticle : TObject;
//Returns tobject to avoid having TParticle in interface-part
var
  P : TParticle;
  Angle,Tmp : single;
begin
  P := TParticle.Create;
  //Direction,Spread,Speed,SpeedRange controls velocity
  P.Width := ParticleWidth;
  P.Height := ParticleHeight;
  P.LifeTime := ParticleLifeTime;

  //Radius control startposition-randomness
  Angle := System.Random * (PI*2);
  Tmp := System.Random * Radius;
  P.Position[0] := Sin(Angle) * Tmp;
  P.Position[1] := Cos(Angle) * Tmp;

  if not Self.FollowModel then
  begin
    if CurrentModel<>nil then
      VecAdd2_Inplace(P.Position, PZVector2f(@CurrentModel.Position)^ );
  end;

  Angle := ZMath.Random(Direction,Spread);
  //Let emit expression modify particle properties
  Self.PColor := MakeColorf(1,1,1,1);
  Self.PAngle := Angle;
  if OnEmitExpression.Code<>nil then
    ZExpressions.RunCode(OnEmitExpression.Code);
  Angle := Self.PAngle;

  if SpeedRange=0 then
    Tmp := Speed
  else
    Tmp := ZMath.Random(Speed,Speed*SpeedRange);
  P.Velocity[0] := Sin(Angle) * Tmp;
  P.Velocity[1] := Cos(Angle) * Tmp;

  P.Color := Self.PColor;

  Result := P;
end;

{$ifndef minimal}
procedure TRenderParticles.DesignerReset;
begin
  inherited;
  Time := 0;
  EmitTime := 0;
  Particles.Clear;
  IsChanged := False;
end;
{$endif}

procedure TRenderParticles.Update;
var
  P : TParticle;
  I,EmitCount : integer;
  DSpeed,DAlpha,DSize,T,DT : single;
  ScaledGravity : TZVector2f;
  UseGravity : boolean;
begin
  if ZApp.FrameLoss and (System.Random<0.5) then
    Exit; //If we are losing frames then randomly skip update for trying to catch up

  //Copy to local for making the loop faster
  DT:=ZApp.DeltaTime;

  UseGravity := not VecIsNull3(Gravity);

  //Update positions and remove particles
  DAlpha := Self.AnimateAlpha * DT;
  DSize := Self.AnimateSize * DT;
  DSpeed := 1.0 - (Self.Damping * DT);

  if UseGravity then
    ScaledGravity := VecScalarMult2(PZVector2f(@Gravity)^,DT);

  I := 0;
  while I<Particles.Count do
  begin
    P := TParticle(Particles[I]);
    P.LifeTime := P.LifeTime - DT;
    if P.LifeTime<=0 then
      Particles.SwapRemoveAt(I)
    else
    begin
      P.Position[0] := P.Position[0] + P.Velocity[0] * DT;
      P.Position[1] := P.Position[1] + P.Velocity[1] * DT;

      P.Velocity[0] := P.Velocity[0] * DSpeed;
      P.Velocity[1] := P.Velocity[1] * DSpeed;

      if UseGravity then
        VecAdd2_Inplace( PZVector2f(@P.Velocity)^, ScaledGravity );

      P.Color.A := Clamp(P.Color.A + DAlpha,0,1);

      if DSize<>0 then
      begin
        P.Width := P.Width + DSize;
        P.Height := P.Height + DSize;
      end;

      Inc(I);
    end;
  end;
  //Emit new particles
  Time := Time + DT;
  T := Time-BeginTime;
  if (T>0) and ((Duration=0) or (T<Duration)) then
  begin
    EmitTime:=EmitTime + DT;
    EmitCount:=Trunc( ParticlesPerSecond*EmitTime );
    if EmitCount>0 then
    begin
      EmitTime:=EmitTime - (1/ParticlesPerSecond)*EmitCount;
      while (EmitCount>0) and (Particles.Count<10000) do
      begin
        Particles.Add( EmitParticle );
        Dec(EmitCount);
      end;
    end;
  end;
end;

procedure TRenderParticles.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'ParticlesPerSecond',{$ENDIF}(@ParticlesPerSecond), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}'Direction',{$ENDIF}(@Direction), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}'Spread',{$ENDIF}(@Spread), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}'ParticleWidth',{$ENDIF}(@ParticleWidth), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}'ParticleHeight',{$ENDIF}(@ParticleHeight), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}'Speed',{$ENDIF}(@Speed), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}'SpeedRange',{$ENDIF}(@SpeedRange), zptScalar);
  List.AddProperty({$IFNDEF MINIMAL}'Radius',{$ENDIF}(@Radius), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}'ParticleLifetime',{$ENDIF}(@ParticleLifetime), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}'AnimateAlpha',{$ENDIF}(@AnimateAlpha), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}'AnimateSize',{$ENDIF}(@AnimateSize), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}'Damping',{$ENDIF}(@Damping), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}'Duration',{$ENDIF}(@Duration), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}'BeginTime',{$ENDIF}(@BeginTime), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}'OnEmitExpression',{$ENDIF}(@OnEmitExpression), zptExpression);
    {$ifndef minimal}
    List.GetLast.DefaultValue.ExpressionValue.Source :=
      '//Emit particle.'#13#10 +
      '//PColor : particle color, PAngle : particle angle'#13#10 +
      '';
    {$endif}
  List.AddProperty({$IFNDEF MINIMAL}'Gravity',{$ENDIF}(@Gravity), zptVector3f);
  List.AddProperty({$IFNDEF MINIMAL}'FollowModel',{$ENDIF}(@FollowModel), zptBoolean);
    List.GetLast.DefaultValue.BooleanValue:=True;
  List.AddProperty({$IFNDEF MINIMAL}'PColor',{$ENDIF}(@PColor), zptColorf);
    List.GetLast.NeverPersist:=True;
  List.AddProperty({$IFNDEF MINIMAL}'PAngle',{$ENDIF}(@PAngle), zptFloat);
    List.GetLast.NeverPersist:=True;
end;


{ TShader }

procedure TShader.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'VertexShaderSource',{$ENDIF}(@VertexShaderSource), zptString);
  List.AddProperty({$IFNDEF MINIMAL}'GeometryShaderSource',{$ENDIF}(@GeometryShaderSource), zptString);
  List.AddProperty({$IFNDEF MINIMAL}'FragmentShaderSource',{$ENDIF}(@FragmentShaderSource), zptString);
  List.AddProperty({$IFNDEF MINIMAL}'UniformVariables',{$ENDIF}(@UniformVariables), zptComponentList);
    {$ifndef minimal}List.GetLast.SetChildClasses([TShaderVariable]);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'UpdateVarsOnEachUse',{$ENDIF}(@UpdateVarsOnEachUse), zptBoolean);
  List.AddProperty({$IFNDEF MINIMAL}'BeforeLinkExpression',{$ENDIF}(@BeforeLinkExpression), zptExpression);
  List.AddProperty({$IFNDEF MINIMAL}'Handle',{$ENDIF}(@ProgHandle), zptInteger);
    List.GetLast.NeverPersist:=True;
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
end;

destructor TShader.Destroy;
begin
  CleanUp;
  inherited;
end;

{$if (not defined(minimal)) or defined(android)}
  {$define glsl_error_check}
{$ifend}
procedure TShader.ReInit;
  {$ifdef glsl_error_check}
  procedure LogWrite(P : PAnsiChar);
  begin
    {$ifdef zlog}
    ZLog.GetLog(Self.ClassName).Write( String(P) );
    {$else}
    Platform_Error(P);
    {$endif}
  end;

  procedure LogWarn(P : PAnsiChar);
  begin
    {$ifdef zlog}
    ZLog.GetLog(Self.ClassName).Write( String(P) );
    {$else}
    Platform_Error(P);
    {$endif}
  end;

  procedure InDumpProgramLog(Prog : GLuint);
  var
    GlMess : array[0..511] of ansichar;
    MessLen : integer;
  begin
    glGetProgramInfoLog(Prog,SizeOf(GlMess),@MessLen,@GlMess);
    if MessLen>0 then
      LogWrite( PAnsiChar(@GlMess) );
  end;

  function InCheckShaderValid(Shader : PGLuint; Kind: GLEnum) : boolean;
  var
    Status : GLUInt;
    S : string;
    GlMess : array[0..16*1024-1] of ansichar;
    MessLen : integer;
  begin
    glGetShaderiv(Shader^,GL_COMPILE_STATUS,@Status);
    if Kind=GL_VERTEX_SHADER then
      S := 'Vertex'
    else
      S := 'Fragment';
    if Status=GL_TRUE then
    begin
      //ZLog.GetLog(Self.ClassName).Write( S + ' shader compiled OK' );
      Result := True
    end
    else
    begin
      {$ifndef minimal}
      LogWarn( PAnsiChar(AnsiString('Error in ' + S + ' shader compilation' {$ifndef minimal} + ' (' + String(Self.Name) + ')'{$endif})) );
      {$endif}
      glGetShaderInfoLog(Shader^,SizeOf(GlMess),@MessLen,@GlMess);
      if MessLen>0 then
        LogWrite( PAnsiChar(@GlMess) );
      //Remove the incorrect shader, otherwise it tries to unattach in cleanup
      glDeleteShader(Shader^);
      Shader^ := 0;
      {$ifndef minimal}
      raise EShaderException.Create(String(PAnsiChar(@GlMess)));
      {$else}
      Result := False;
      {$endif}
    end;
  end;

  procedure InCheckProgramStatus;
  var
    Status : GLUInt;
  begin
    glGetProgramiv(ProgHandle,GL_LINK_STATUS,@Status);
    if Status=GL_FALSE then
    begin
      LogWarn( 'Error when linking shader program' );
      InDumpProgramLog(ProgHandle);
    end;
    glValidateProgram(ProgHandle);
    glGetProgramiv(ProgHandle,GL_VALIDATE_STATUS,@Status);
    if Status=GL_FALSE then
    begin
      LogWarn( 'Error when linking shader program' );
      InDumpProgramLog(ProgHandle);
    end;
  end;
  {$endif}

  function InCreate(const Source : TPropString; const Kind: GLEnum) : cardinal;
  {$if not defined(minimal) and defined(macos)}
  var
    S : ansistring;
  {$ifend}
  begin
    Result := 0;
    if (pointer(Source)=nil) or (ZStrLength(PAnsiChar(Source))=0) then
      Exit;
    Result := glCreateShader(Kind);

    {$if not defined(minimal) and defined(macos)} //Give shaders a chance to ifdef around macos specifics (in zgeviz)
    S := ansistring('#define macos'#13) + Source;
    glShaderSource(Result,1, @S,nil);
    {$else}
    glShaderSource(Result,1,@Source,nil);
    {$ifend}

    glCompileShader(Result);
    {$ifdef glsl_error_check}
    if InCheckShaderValid(@Result,Kind) then
    {$endif}
    glAttachShader(ProgHandle,Result);
    {$ifndef minimal}CheckGLError;{$endif}
  end;

const
  TexVar : array[0..4] of ansichar = 'tex'#0#0;
var
  I,J : integer;
  Driver : TGLDriverBase;
begin
  CleanUp;

  ProgHandle := glCreateProgram;

  VShaderHandle := InCreate(VertexShaderSource,GL_VERTEX_SHADER);
  FShaderHandle := InCreate(FragmentShaderSource,GL_FRAGMENT_SHADER);
  GShaderHandle := InCreate(GeometryShaderSource,GL_GEOMETRY_SHADER);

  Driver := Self.ZApp.Driver;
  Driver.BeforeLinkShader(Self);
  ZExpressions.RunCode(BeforeLinkExpression.Code);

  glLinkProgram(ProgHandle);
  {$ifdef glsl_error_check}InCheckProgramStatus;{$endif}

  Driver.AfterLinkShader(Self);

  //Initialize uniform variables for accessing multi-textures
  glUseProgram(ProgHandle);
  if Driver.CurrentMaterial<>nil then
    for I := 0 to Driver.CurrentMaterial.Textures.Count-1 do
    begin
      TexVar[3]:=ansichar(ord('1') + I);
      J := glGetUniformLocation(ProgHandle,pansichar(@TexVar));
      if J>-1 then
        glUniform1i(J,I);
    end;

  IsChanged := False;
end;

procedure TShader.UpdateVariableValues;
var
  I : integer;
  Sv : TShaderVariable;
  V : single;

  procedure UpdateArrayVar;
  var
    Count : integer;
    P : pointer;
  begin
    Count := Sv.ValueArrayRef.CalcLimit;
    P := Sv.ValueArrayRef.GetData;
    case Sv.ArrayKind of
      sakTexture2D :
        begin
          glActiveTexture($84C0 + Self.FirstTexIndex + Self.TexCount);
          glEnable(GL_TEXTURE_2D);
          if Sv.TexWidth<>Count then
          begin //Resize texture if array size has changed
            Sv.TexWidth := Count;
            Sv.ResetGpuResources;
          end;
          if Sv.TextureHandle=0 then
          begin
            glGenTextures(1, @Sv.TextureHandle);
            glBindTexture(GL_TEXTURE_2D, Sv.TextureHandle);
            //Store array as 2d-texture with height 1 (because there seems to be problems with 1d textures)
            glTexImage2D(GL_TEXTURE_2D, 0, GL_R32F, Count, 1, 0, GL_RED, GL_FLOAT, P);
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE );
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE );
            glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST );
            glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
            //glTexParameteri(GL_TEXTURE_2D, GL_GENERATE_MIPMAP, GL_TRUE); // automatic mipmap
          end else
          begin
            glBindTexture(GL_TEXTURE_2D, Sv.TextureHandle);
            glTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, Count, 1, GL_RED, GL_FLOAT, P);
          end;
          glUniform1i(Sv.Location,Self.FirstTexIndex + Self.TexCount);
          Inc(Self.TexCount);
        end;
      sakMat4 :
        begin
          //Use count to allow mat4 arrays
          glUniformMatrix4fv(Sv.Location,Count,GL_FALSE,P);
        end;
    end;
  end;

  procedure UpdateVariableRef;
  var
    V : TDefineVariable;
  begin
    V := Sv.VariableRef;
    case V._Type of
      zctFloat : glUniform1f(Sv.Location,V.Value);
      zctVec2 : glUniform2fv(Sv.Location, 1, PGLFloat(TDefineArray(V.PointerValue).GetData));
      zctVec3 : glUniform3fv(Sv.Location, 1, PGLFloat(TDefineArray(V.PointerValue).GetData));
      zctVec4 : glUniform4fv(Sv.Location, 1, PGLFloat(TDefineArray(V.PointerValue).GetData));
      zctMat4 : glUniformMatrix4fv(Sv.Location, 1, GL_FALSE, PGLFloat(TDefineArray(V.PointerValue).GetData));
    end;
  end;

begin
  //Arrays are passed as sampler2d so update with the index of the texture
  if ZApp.Driver.CurrentMaterial<>nil then
    Self.FirstTexIndex := ZApp.Driver.CurrentMaterial.Textures.Count
  else
    FirstTexIndex := 0;
  Self.TexCount := 0;

  for I := 0 to UniformVariables.Count - 1 do
  begin
    Sv := TShaderVariable(UniformVariables[I]);
    {$ifndef minimal}
    if (Sv.Location=-1) or (Sv.DesignDisable) then Continue;
    {$endif}

    if Sv.ValueArrayRef<>nil then
      UpdateArrayVar
    else if Sv.VariableRef<>nil then
      UpdateVariableRef
    else
    begin
      if Sv.ValuePropRef.Code.Count>0 then
        V := ExpGetValue(Sv.ValuePropRef.Code)
      else
        V := Sv.Value;
      glUniform1f(Sv.Location,V);
    end;
  end;
end;

procedure TShader.UpdateVariableLocations;
var
  I : integer;
  Sv : TShaderVariable;
begin
  for I := 0 to UniformVariables.Count - 1 do
  begin
    Sv := TShaderVariable(UniformVariables[I]);
    {$ifndef minimal}
    if Sv.DesignDisable then
      Continue;
    if Length(Sv.VariableName)=0 then
    begin
      if Length(Sv.Name)>0 then
        ZLog.GetLog(Self.ClassName).Warning( 'Ignoring shader variable because VariableName property not set: ' + String(Sv.Name) );
      Continue;
    end;
    {$endif}
    Sv.Location := glGetUniformLocation(ProgHandle,pointer(Sv.VariableName));
    {$if (not defined(minimal)) and (not defined(zgeviz)) }
    if Sv.Location=-1 then
      ZLog.GetLog(Self.ClassName).Write( 'Shader variable missing/unused: ' + String(Sv.VariableName) );
    {$ifend}
  end;
end;

procedure TShader.DetachArrayVariables;
var
  I : integer;
begin
  if Self.TexCount>0 then
  begin
    for I := Self.TexCount-1 downto 0 do
    begin
      glActiveTexture($84C0 + Self.FirstTexIndex + I);
      glDisable(GL_TEXTURE_2D);
    end;
    glActiveTexture($84C0); //Make sure we leave with activetexture 0
  end;
end;

procedure TShader.UseShader;
var
  ReinitDone : boolean;
begin
  if (ProgHandle=0) or IsChanged then
  begin
    ReInit;
    ReinitDone := True;
  end
  else
    ReinitDone := False;

  glUseProgram(ProgHandle);

  //Update uniform variables once each frame
  if (UniformVariables.Count>0) and
    ((LastVariableUpdate=0) or (LastVariableUpdate<>ZApp.Time) or Self.UpdateVarsOnEachUse)
    then
  begin
    if ReinitDone then
      //Uniform var locations must be updated after new linkage, and after glUseProgram
      UpdateVariableLocations;
    UpdateVariableValues;
    LastVariableUpdate := ZApp.Time;
  end;

  Self.ZApp.Driver.SetCurrentShader(Self);
end;

procedure TShader.CleanUp;

  procedure InDestroy(H : PGLuint);
  begin
    if H^=0 then
      Exit;
    glDetachShader(ProgHandle,H^);
    glDeleteShader(H^);
    H^ := 0;
  end;

begin
  if ProgHandle=0 then
    Exit;

  InDestroy(@VShaderHandle);
  InDestroy(@FShaderHandle);
  InDestroy(@GShaderHandle);

  glDeleteProgram(ProgHandle);
  ProgHandle := 0;
end;

procedure TShader.ResetGpuResources;
begin
  CleanUp;
  inherited;
end;

{ TShaderVariable }

procedure TShaderVariable.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'VariableName',{$ENDIF}(@VariableName), zptString);
    {$ifndef minimal}List.GetLast.NeedRefreshNodeName := True;{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'Value',{$ENDIF}(@Value), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}'ValuePropRef',{$ENDIF}(@ValuePropRef), zptExpression);
    {$ifndef minimal}List.GetLast.ExpressionKind := ekiGetValue;{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'ValueArrayRef',{$ENDIF}(@ValueArrayRef), zptComponentRef);
    {$ifndef minimal}List.GetLast.SetChildClasses([TDefineArray]);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'ArrayKind',{$ENDIF}(@ArrayKind), zptByte);
    {$ifndef minimal}List.GetLast.SetOptions(['Texture2D','Mat4']);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'VariableRef',{$ENDIF}(@VariableRef), zptComponentRef);
    {$ifndef minimal}List.GetLast.SetChildClasses([TDefineVariable]);{$endif}
end;

{$ifndef minimal}
function TShaderVariable.GetDisplayName: AnsiString;
begin
  Result := inherited GetDisplayName;
  Result := Result + '  ' + VariableName;
  if Length(VariableName)=0 then
    Result := Result + '*VariableName not set*';
end;
{$endif}

procedure TShaderVariable.ResetGpuResources;
begin
  if TextureHandle<>0 then
  begin
    glDeleteTextures(1, @TextureHandle);
    TextureHandle := 0;
  end;
end;

destructor TShaderVariable.Destroy;
begin
  if TextureHandle<>0 then
    glDeleteTextures(1, @TextureHandle);
  inherited;
end;

{$ifndef minimal}
procedure CleanUp;
begin
  DefaultMaterial.Free;
  DefaultMaterialTexture.Free;
  if DefaultFont<>nil then
    DefaultFont.Free;
end;

procedure DesignerRenderStop;
begin
  //Make sure primary render buffer is restored
  if FbosSupported then
  begin
    if CurrentRenderTarget<>nil then
      glBindFramebuffer(GL_FRAMEBUFFER_EXT, 0);
  end;
  CurrentRenderTarget := nil;
end;
{$endif}

{ TMaterialTexture }

procedure TMaterialTexture.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Texture',{$ENDIF}(@Texture), zptComponentRef);
    {$ifndef minimal}List.GetLast.SetChildClasses([TZBitmap]);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'RenderTarget',{$ENDIF}(@RenderTarget), zptComponentRef);
    {$ifndef minimal}List.GetLast.SetChildClasses([TRenderTarget]);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'TextureScale',{$ENDIF}(@TextureScale), zptVector3f);
    List.GetLast.DefaultValue.Vector3fValue := ZMath.UNIT_XYZ3;
  List.AddProperty({$IFNDEF MINIMAL}'TextureX',{$ENDIF}(@TextureX), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}'TextureY',{$ENDIF}(@TextureY), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}'TextureRotate',{$ENDIF}(@TextureRotate), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}'TextureWrapMode',{$ENDIF}(@TextureWrapMode), zptByte);
    {$ifndef minimal}List.GetLast.SetOptions(['Mirror','Tile','Clamp']);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'TexCoords',{$ENDIF}(@TexCoords), zptByte);
    {$ifndef minimal}List.GetLast.SetOptions(['Generated','ModelDefined']);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'Origin',{$ENDIF}(@Origin), zptVector3f);
    List.GetLast.DefaultValue.Vector3fValue := Vector3f(0.5,0.5,0);
end;

{$ifndef minimal}
function TMaterialTexture.GetDisplayName: AnsiString;
begin
  Result := inherited GetDisplayName;
  if Assigned(RenderTarget) then
    Result := Result + '  ' + RenderTarget.Name
  else if Assigned(Texture) then
    Result := Result + '  ' + Texture.Name;
end;
{$endif}

{ TRenderTarget }

function NextPowerOfTwo(const I : integer) : integer;
begin
  Result := 1;
  while I>Result do
    Result := Result shl 1;
end;

procedure TRenderTarget.Activate{$ifndef minimal}(const NoApp : boolean = False){$endif};
//NoApp = does not require ZApp to be set
const
  FilterTypes : array[0..2] of integer = (GL_LINEAR,GL_NEAREST,GL_LINEAR_MIPMAP_LINEAR);
  InternalFormats : array[0..2] of integer = (GL_RGBA, GL_RGBA16, GL_R32F);
var
  W,H : integer;
  ActualW,ActualH,I : integer;
  A : TZApplication;
begin
  if not FbosSupported then
    Exit;

  {$ifndef minimal}
  if not NoApp then
  begin
  {$endif}
    A := ZApp;
    //Reset viewport so that ViewportW/H is the full size before the code below that reads from it
    A.UpdateViewport;
  {$ifndef minimal}
  end else
    A := nil;
  {$endif}

  W := Self.CustomWidth;
  if W=0 then
  begin
    if Self.Width>=rts128 then
      W := 128 shl (Ord(Self.Width)-Ord(rts128))
    else
      W := A.ViewportWidth shr Ord(Self.Width);
  end;
  H := Self.CustomHeight;
  if H=0 then
  begin
    if Self.Height>=rts128 then
      H := 128 shl (Ord(Self.Height)-Ord(rts128))
    else
      H := A.ViewportHeight shr Ord(Self.Height);
  end;

  if Self.AutoPowerOfTwo then
  begin
    ActualW := NextPowerOfTwo(W);
    ActualH := NextPowerOfTwo(H);
  end
  else
  begin
    ActualW := W;
    ActualH := H;
  end;

  {$ifndef minimal}
  if (LastW<>W) or (LastH<>H) then
    CleanUp;
  LastW := W; LastH := H;
  {$endif}

  if TexId=0 then
  begin
    //http://www.songho.ca/opengl/gl_fbo.html
    //For android: http://blog.shayanjaved.com/2011/05/13/android-opengl-es-2-0-render-to-texture/
    // create a texture object
    glGenTextures(1, @TexId);
    glBindTexture(GL_TEXTURE_2D, TexId);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

    {$ifdef android}
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    {$else}
    if Self.Filter=bmfMipmap then
      I := GL_LINEAR  //Mipmap is not a valid mag-filter
    else
      I := FilterTypes[Ord(Self.Filter)];
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, I );
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, FilterTypes[Ord(Self.Filter)] );
    glTexParameteri(GL_TEXTURE_2D, GL_GENERATE_MIPMAP, GL_TRUE); // automatic mipmap
    {$endif}

    glTexImage2D(GL_TEXTURE_2D, 0, InternalFormats[ Ord(Self.InternalFormat) ] , ActualW, ActualH, 0, GL_RGBA, GL_UNSIGNED_BYTE, nil);
    glBindTexture(GL_TEXTURE_2D, 0);

    // create a renderbuffer object to store depth info
    glGenRenderbuffers(1, @RboId);
    glBindRenderbuffer(GL_RENDERBUFFER_EXT, RboId);
    glRenderbufferStorage(GL_RENDERBUFFER_EXT, {$ifdef android}GL_DEPTH_COMPONENT16{$else}GL_DEPTH_COMPONENT{$endif},ActualW, ActualH);
    glBindRenderbuffer(GL_RENDERBUFFER_EXT, 0);

    // create a framebuffer object
    glGenFramebuffers(1, @FboId);
    glBindFramebuffer(GL_FRAMEBUFFER_EXT, FboId);

    // attach the texture to FBO color attachment point
    glFramebufferTexture2D(GL_FRAMEBUFFER_EXT, GL_COLOR_ATTACHMENT0_EXT, GL_TEXTURE_2D, TexId, 0);

    // attach the renderbuffer to depth attachment point
    glFramebufferRenderbuffer(GL_FRAMEBUFFER_EXT, GL_DEPTH_ATTACHMENT_EXT,GL_RENDERBUFFER_EXT, RboId);

    {$ifdef android}
    if glCheckFramebufferStatus(GL_FRAMEBUFFER_EXT)<>GL_FRAMEBUFFER_COMPLETE_EXT then
      AndroidLog('fbo fail')
    else
      AndroidLog('fbo ok');
    {$endif}

    // check FBO status
    {$ifndef minimal}
    if glCheckFramebufferStatus(GL_FRAMEBUFFER_EXT)<>GL_FRAMEBUFFER_COMPLETE_EXT then
    begin
      ZLog.GetLog(Self.ClassName).Warning( 'Fbo error: ' + IntToStr(glCheckFramebufferStatus(GL_FRAMEBUFFER_EXT)) );
    end;
    {$endif}
  end else
    glBindFramebuffer(GL_FRAMEBUFFER_EXT, FboId);

  {$ifndef minimal}
  if NoApp then
    glViewPort(0,0,W,H)
  else
  {$endif}
    A.UpdateViewport(W, H);

  if Self.ClearBeforeUse then
  begin
    glClearColor(ClearColor.V[0],ClearColor.V[1],ClearColor.V[2],0);
    glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  end;
end;

procedure TRenderTarget.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Width',{$ENDIF}(@Width), zptByte);
    {$ifndef minimal}List.GetLast.SetOptions(['Viewport width','Half viewport width','Quarter viewport width','128','256','512','1024']);{$endif}
    List.GetLast.DefaultValue.ByteValue := 4;
  List.AddProperty({$IFNDEF MINIMAL}'Height',{$ENDIF}(@Height), zptByte);
    {$ifndef minimal}List.GetLast.SetOptions(['Viewport height','Half viewport height','Quarter viewport height','128','256','512','1024']);{$endif}
    List.GetLast.DefaultValue.ByteValue := 4;
  List.AddProperty({$IFNDEF MINIMAL}'CustomWidth',{$ENDIF}(@CustomWidth), zptInteger);
  List.AddProperty({$IFNDEF MINIMAL}'CustomHeight',{$ENDIF}(@CustomHeight), zptInteger);
  List.AddProperty({$IFNDEF MINIMAL}'ClearBeforeUse',{$ENDIF}(@ClearBeforeUse), zptBoolean);
    List.GetLast.DefaultValue.BooleanValue := True;
  List.AddProperty({$IFNDEF MINIMAL}'AutoPowerOfTwo',{$ENDIF}(@AutoPowerOfTwo), zptBoolean);
  List.AddProperty({$IFNDEF MINIMAL}'ClearColor',{$ENDIF}(@ClearColor), zptColorf);
  List.AddProperty({$IFNDEF MINIMAL}'Filter',{$ENDIF}(@Filter), zptByte);
    {$ifndef minimal}List.GetLast.SetOptions(['Linear','Nearest','Mipmap']);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'InternalFormat',{$ENDIF}@InternalFormat, zptByte);
    {$ifndef minimal}List.GetLast.SetOptions(['RGBA','RGBA16','R32F']);{$endif}
end;

procedure TRenderTarget.CleanUp;
begin
  if FbosSupported and (TexId<>0) then
  begin
    glDeleteTextures(1, @TexId);
    glDeleteFramebuffers(1, @FboId);
    glDeleteRenderbuffers(1, @RboId);
    TexId := 0;
  end;
end;

destructor TRenderTarget.Destroy;
begin
  CleanUp;
  inherited;
end;

procedure TRenderTarget.UseTextureBegin;
begin
  glBindTexture(GL_TEXTURE_2D, TexId);
end;

procedure TRenderTarget.ResetGpuResources;
begin
  CleanUp;
  inherited;
end;

{ TSetRenderTarget }

procedure TSetRenderTarget.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'RenderTarget',{$ENDIF}(@RenderTarget), zptComponentRef);
    {$ifndef minimal}List.GetLast.SetChildClasses([TRenderTarget]);{$endif}
    {$ifndef minimal}List.GetLast.NeedRefreshNodeName := True;{$endif}
end;

procedure TSetRenderTarget.Execute;
var
  NewRt : TRenderTarget;
begin
  if not FbosSupported then
    Exit;

  {$ifndef zgeviz}  //For ZgeViz we want updateviewport to be called every time
  if CurrentRenderTarget=Self.RenderTarget then
    Exit;
  {$endif}

  if (CurrentRenderTarget<>nil) and (CurrentRenderTarget<>Self.RenderTarget) then
  begin
    //update texture mipmap of current render target
    CurrentRenderTarget.UseTextureBegin;
    glGenerateMipmap(GL_TEXTURE_2D);
  end;

  NewRt := Self.RenderTarget;
  {$ifdef zgeviz}
  if NewRt=nil then
    NewRt := Self.ZApp.MainRenderTarget;
  {$endif}

  if NewRt=nil then
  begin
    //Restore main framebuffer
    glBindFramebuffer(GL_FRAMEBUFFER_EXT, 0);
    CurrentRenderTarget := nil;
    ZApp.UpdateViewport;
  end
  else
  begin
    //CurrentRenderTarget must be set before viewport-change because of zgeviz-onviewcallback
    CurrentRenderTarget := NewRt;
    CurrentRenderTarget.Activate;
  end;
end;

{$ifndef minimal}
function TSetRenderTarget.GetDisplayName: AnsiString;
begin
  Result := inherited GetDisplayName;
  if Assigned(RenderTarget) then
    Result := Result + '  ' + RenderTarget.Name;
end;
{$endif}

{ TLight }

procedure TLight.ApplyLight(const LightId: integer);
var
  Id : integer;
  V : single;
begin
  Id := GL_LIGHT0 + LightId;
  if Self.Enabled then
    glEnable(Id)
  else
    glDisable(Id);

  if Kind=likDirectional then
    Self.Position[3] := 0
  else
  begin
    Self.Position[3] := 1;
    if Kind=likPoint then
      V := 180
    else
    begin
      V := Self.SpotCutoff;
      glLightfv(Id, GL_SPOT_DIRECTION, @Self.SpotDirection);
      glLightfv(Id, GL_SPOT_EXPONENT, @Self.SpotExponent);
    end;
    glLightfv(Id, GL_SPOT_CUTOFF,@V);
  end;
  glLightfv(Id, GL_POSITION, @Self.Position);

  glLightfv(Id, GL_DIFFUSE, @Self.Color);
  glLightfv(Id, GL_SPECULAR, @Self.Color);
end;

procedure TLight.RemoveLight(const LightId: integer);
var
  Id : integer;
begin
  //Leave light0 on as this is the default in ZGE
  //Turn the others off
  Id := GL_LIGHT0 + LightId;
  if LightId=0 then
  begin
    glEnable(Id);
    //Also restore diffuse color
    glLightfv(Id, GL_DIFFUSE, @ZMath.UNIT_XYZ4);
  end
  else
    glDisable(Id);
end;

procedure TLight.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Position',{$ENDIF}(@Position), zptVector3f);
  List.AddProperty({$IFNDEF MINIMAL}'Color',{$ENDIF}(@Color), zptColorf);
  List.AddProperty({$IFNDEF MINIMAL}'Enabled',{$ENDIF}(@Enabled), zptBoolean);
    List.GetLast.DefaultValue.BooleanValue := True;
  List.AddProperty({$IFNDEF MINIMAL}'Kind',{$ENDIF}(@Kind), zptByte);
    {$ifndef minimal}List.GetLast.SetOptions(['Directional','Point','Spot']);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'SpotDirection',{$ENDIF}(@SpotDirection), zptVector3f);
    List.GetLast.DefaultValue.Vector3fValue := Vector3f(0,0,-1);
  List.AddProperty({$IFNDEF MINIMAL}'SpotExponent',{$ENDIF}(@SpotExponent), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}'SpotCutoff',{$ENDIF}(@SpotCutoff), zptFloat);
    List.GetLast.DefaultValue.FloatValue := 45;
end;

{ TSpriteSheet }

{$ifndef minimal}
destructor TSpriteSheet.Destroy;
begin
  Material.Free;
  inherited;
end;
{$endif}

procedure TSpriteSheet.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Bitmap',{$ENDIF}(@Bitmap), zptComponentRef);
    {$ifndef minimal}List.GetLast.SetChildClasses([TZBitmap]);{$endif}
  List.AddProperty({$ifndef minimal}'SpriteData',{$ENDIF}@SpriteData, zptBinary);
end;

function TSpriteSheet.GetSpriteInfo(const Index: integer): PSpriteInfo;
begin
  Result := nil;
  if Assigned(Self.Bitmap) and (Index>=0) and (Index<SpriteData.Size div SizeOf(TSpriteInfo)) then
    Result := PSpriteInfo(PByte(Self.SpriteData.Data) + Index*SizeOf(TSpriteInfo));
end;

function CreateMaterialForSprites(B : TZBitmap) : TMaterial;
var
  M : TMaterial;
  MT : TMaterialTexture;
begin
  M := TMaterial.Create(nil);
  M.SkipSetColor := True;
  Mt := TMaterialTexture.Create(M.Textures);
  Mt.Texture := B;
  Mt.TextureWrapMode := tmClamp;
  Mt.TexCoords := tcModelDefined;
  Result := M;
end;

procedure CopySpriteMaterialProps(M1,M2 : TMaterial);
begin
  M1.Blend := M2.Blend;
  M1.Shader := M2.Shader;
  M1.Light := M2.Light;
  M1.Shading := M2.Shading;
end;


procedure TSpriteSheet.Render(Rs: TRenderSprite);
//This routine is based on a ZGE script by Kjell
var
  Info : PSpriteInfo;
  Driver : TGLDriverBase;
begin
  Info := GetSpriteInfo(Rs.SpriteIndex);
  if (Info=nil) or (Self.Bitmap=nil) then
    Exit;

  Driver := Self.ZApp.Driver;

  if Material=nil then
    Material := CreateMaterialForSprites(Self.Bitmap);
  if (Driver.CurrentMaterial<>Self.Material) and (Driver.CurrentMaterial<>nil) then
    CopySpriteMaterialProps(Self.Material,Driver.CurrentMaterial);

  TMaterialTexture(Self.Material.Textures[0]).Texture := Self.Bitmap;
  Driver.EnableMaterial(Self.Material);

  Driver.RenderQuad(Self.Bitmap,
    Info.OriginX, Info.OriginY,
    Info.SizeX, Info.SizeY,
    Info.SheetX, Info.SheetY,
    Rs.MirrorHorizontal, Rs.MirrorVertical);
end;

{ TTileSet }

{$ifndef minimal}
//Set some default values in designer
constructor TTileSet.Create(OwnerList: TZComponentList);
begin
  inherited;
  Self.TileWidth := 16;
  Self.TileHeight := 16;
end;

destructor TTileSet.Destroy;
begin
  Material.Free;
  inherited;
end;
{$endif}

procedure TTileSet.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Bitmap',{$ENDIF}(@Bitmap), zptComponentRef);
    {$ifndef minimal}List.GetLast.SetChildClasses([TZBitmap]);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'TileWidth',{$ENDIF}@TileWidth, zptInteger);
  List.AddProperty({$IFNDEF MINIMAL}'TileHeight',{$ENDIF}@TileHeight, zptInteger);
  List.AddProperty({$IFNDEF MINIMAL}'TileBorder',{$ENDIF}@TileBorder, zptInteger);
end;

{ TRenderTile }

procedure TRenderTile.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'TileSet',{$ENDIF}(@TileSet), zptComponentRef);
    {$ifndef minimal}List.GetLast.SetChildClasses([TTileSet]);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'TileIndex',{$ENDIF}(@TileIndex), zptInteger);
  List.AddProperty({$IFNDEF MINIMAL}'OriginX',{$ENDIF}(@OriginX), zptInteger);
  List.AddProperty({$IFNDEF MINIMAL}'OriginY',{$ENDIF}(@OriginY), zptInteger);
  List.AddProperty({$IFNDEF MINIMAL}'MirrorHorizontal',{$ENDIF}(@MirrorHorizontal), zptBoolean);
  List.AddProperty({$IFNDEF MINIMAL}'MirrorVertical',{$ENDIF}(@MirrorVertical), zptBoolean);
end;

procedure TRenderTile.Execute;
var
  Ts : TTileSet;
  TilesPerRow,X,Y : integer;
  Driver : TGLDriverBase;
begin
  Ts := Self.TileSet;
  if (Ts=nil) or (Ts.Bitmap=nil) then
    Exit;

  TilesPerRow := (Ts.Bitmap.PixelWidth+Ts.TileBorder) div (Ts.TileWidth+Ts.TileBorder);

  X := (Self.TileIndex mod TilesPerRow) * (Ts.TileWidth+Ts.TileBorder);
	Y := (Self.TileIndex div TilesPerRow) * (Ts.TileHeight+Ts.TileBorder);

  Driver := Self.ZApp.Driver;

  if Ts.Material=nil then
    Ts.Material := CreateMaterialForSprites(Ts.Bitmap);
  if (Driver.CurrentMaterial<>Ts.Material) and (Driver.CurrentMaterial<>nil) then
    CopySpriteMaterialProps(Ts.Material,Driver.CurrentMaterial);
  Driver.EnableMaterial(Ts.Material);

  Driver.RenderQuad(Ts.Bitmap,
    Self.OriginX, Self.OriginY,
    Ts.TileWidth, Ts.TileHeight,
    X, Y,
    Self.MirrorHorizontal, Self.MirrorVertical);
end;

initialization

  ZClasses.Register(TMaterial,MaterialClassId);
    {$ifndef minimal}ComponentManager.LastAdded.AutoName := True;{$endif}
    {$ifndef minimal}ComponentManager.LastAdded.ImageIndex:=32;{$endif}
  ZClasses.Register(TMaterialTexture,MaterialTextureClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NeedParentComp := 'Material';{$endif}
  ZClasses.Register(TShader,ShaderClassId);
    {$ifndef minimal}ComponentManager.LastAdded.AutoName := True;{$endif}
    {$ifndef minimal}ComponentManager.LastAdded.ImageIndex := 50;{$endif}
  ZClasses.Register(TShaderVariable,ShaderVariableClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NeedParentList := 'UniformVariables';{$endif}
    {$ifndef minimal}ComponentManager.LastAdded.NoTopLevelCreate := True;{$endif}
  ZClasses.Register(TUseMaterial,UseMaterialClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NeedParentList := 'OnRender';{$endif}
    {$ifndef minimal}ComponentManager.LastAdded.ImageIndex:=33;{$endif}
  ZClasses.Register(TRenderMesh,RenderMeshClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NeedParentList := 'OnRender';{$endif}
    {$ifndef minimal}ComponentManager.LastAdded.ImageIndex := 10;{$endif}
  ZClasses.Register(TRenderTransform,RenderTransformClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NeedParentList := 'OnRender';{$endif}
    {$ifndef minimal}ComponentManager.LastAdded.ImageIndex := 44;{$endif}
  ZClasses.Register(TRenderSprite,RenderSpriteClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NeedParentList := 'OnRender';{$endif}
    {$ifndef minimal}ComponentManager.LastAdded.ImageIndex := 51;{$endif}
  ZClasses.Register(TRenderBeams,RenderBeamsClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NeedParentList := 'OnRender';{$endif}
    {$ifndef minimal}ComponentManager.LastAdded.ImageIndex := 52;{$endif}
  ZClasses.Register(TRenderTransformGroup,RenderTransformGroupClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NeedParentList := 'OnRender';{$endif}
    {$ifndef minimal}ComponentManager.LastAdded.ImageIndex := 45;{$endif}
  ZClasses.Register(TRenderText,RenderTextClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NeedParentList := 'OnRender';{$endif}
    {$ifndef minimal}ComponentManager.LastAdded.ImageIndex := 12;{$endif}
  ZClasses.Register(TFont,FontClassId);
    {$ifndef minimal}ComponentManager.LastAdded.ImageIndex := 40;{$endif}
  ZClasses.Register(TRenderSetColor,RenderSetColorClassId);
    {$ifndef minimal}ComponentManager.LastAdded.HelpText := 'Set the current OpenGL color';{$endif}
    {$ifndef minimal}ComponentManager.LastAdded.NeedParentList := 'OnRender';{$endif}
    {$ifndef minimal}ComponentManager.LastAdded.ImageIndex := 11;{$endif}
  ZClasses.Register(TRenderNet,RenderNetClassId);
    {$ifndef minimal}ComponentManager.LastAdded.HelpText := 'Render a 2D net';{$endif}
    {$ifndef minimal}ComponentManager.LastAdded.NeedParentList := 'OnRender';{$endif}
    {$ifndef minimal}ComponentManager.LastAdded.ImageIndex := 48;{$endif}
  ZClasses.Register(TRenderParticles,RenderParticlesClassId);
    {$ifndef minimal}ComponentManager.LastAdded.HelpText := 'Simple 2D particlesystem';{$endif}
    {$ifndef minimal}ComponentManager.LastAdded.NeedParentList := 'OnRender';{$endif}
    {$ifndef minimal}ComponentManager.LastAdded.ImageIndex := 49;{$endif}

  ZClasses.Register(TRenderTarget,RenderTargetClassId);
    {$ifndef minimal}ComponentManager.LastAdded.AutoName := True;{$endif}
    {$ifndef minimal}ComponentManager.LastAdded.ImageIndex := 46;{$endif}
  ZClasses.Register(TSetRenderTarget,SetRenderTargetClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NeedParentList := 'OnRender';{$endif}
    {$ifndef minimal}ComponentManager.LastAdded.ImageIndex := 47;{$endif}

  ZClasses.Register(TLight,LightClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NeedParentList := 'Lights';{$endif}
    {$ifndef minimal}ComponentManager.LastAdded.ImageIndex:=25;{$endif}

  ZClasses.Register(TSpriteSheet,SpriteSheetClassId);
    {$ifndef minimal}ComponentManager.LastAdded.AutoName := True;{$endif}
  ZClasses.Register(TTileSet,TileSetClassId);
    {$ifndef minimal}ComponentManager.LastAdded.AutoName := True;{$endif}
  ZClasses.Register(TRenderTile,RenderTileClassId);

  DefaultMaterial := TMaterial.Create(nil);
  DefaultMaterialTexture := TMaterialTexture.Create(nil);

end.
