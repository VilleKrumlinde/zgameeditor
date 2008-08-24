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

unit Renderer;

interface

uses Meshes,ZClasses,ZBitmap;

type
  TRenderCommand = class(TCommand);

  TFont = class;
  TShader = class;

  TMaterialShading = (msSmooth,msFlat,msWireframe);
  TMaterialTexCoords = (tcGenerated,tcModelDefined);
  TMaterial = class(TZComponent)
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Texture : TZBitmap;
    TextureScale : TZVector3f;
    TextureX,TextureY : single;
    TextureRotate : single;
    TextureWrapMode : (tmMirror,tmTile,tmClamp);
    Shading : TMaterialShading;
    Color : TZColorf;
    Light : boolean;
    Blend : (mbNoBlend,mbA_1MSA,mbA_1,mbC_1MSC,mbAlphaSat_1);
    TexCoords : TMaterialTexCoords;
    ZBuffer : boolean;
    DrawBackFace : boolean;
    Font : TFont;
    Shader : TShader;
    WireframeWidth : single;
  end;

  TShaderVariable = class(TZComponent)
  private
    Location : integer;
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    VariableName : TPropString;
    Value : single;
    ValuePropRef : TZPropertyRef;
    {$ifndef minimal}function GetDisplayName: String; override;{$endif}
  end;

  TShader = class(TZComponent)
  strict private
    ProgHandle,VShaderHandle,FShaderHandle : cardinal;
    LastVariableUpdate : single;
    procedure ReInit;
    procedure CleanUp;
    procedure UpdateVariableLocations;
    procedure UpdateVariableValues;
  protected
    procedure DefineProperties(List: TZPropertyList); override;
    procedure UseShader;
  public
    VertexShaderSource : TPropString;
    FragmentShaderSource : TPropString;
    UniformVariables : TZComponentList;
    destructor Destroy; override;
  end;

  //Render-commands
  TUseMaterial = class(TRenderCommand)
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Material : TMaterial;
    procedure Execute; override;
    {$ifndef minimal}function GetDisplayName: String; override;{$endif}
  end;

  TRenderMesh = class(TRenderCommand)
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Mesh : TMesh;
    procedure Execute; override;
    {$ifndef minimal}function GetDisplayName: String; override;{$endif}
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

  TRenderSprite = class(TRenderCommand)
  public
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
    procedure RenderCharacter(Char : integer; Size : integer);
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
    Text : TPropString;                     //Text att printa
    TextFloatRef : TZPropertyRef;     //Propvalue att printa, en av dessa gäller
    X,Y,Scale : single;
    Align : (alCenter,alLeft);
    CharX,CharY,CharI,CharRotate,CharScale : single;
    RenderCharExpression : TZExpressionPropValue;
    FloatMultiply : single;  //Multiplicera floatref innan print, för att visa decimaltal
    UseModelSpace : boolean;
    StretchY : single;
    procedure Execute; override;
    {$ifndef minimal}
    function GetDisplayName: String; override;
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
    procedure Execute; override;
    destructor Destroy; override;
  end;

  TRenderParticles = class(TRenderCommand)
  private
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
    AnimateAlpha : single;
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

procedure InitRenderer;
procedure RenderMesh(Mesh : TMesh);

procedure Render_Begin;
procedure RenderModel(Model : TModel);
procedure Render_End;
procedure ApplyRotation(const Rotate : TZVector3f);


{$ifndef minimal}
procedure AssertNotRenderMode;

var
  IsRendering : boolean;
  NormalsVisible : boolean;
{$endif}

implementation

uses ZOpenGL, ZMath, ZApplication, ZPlatform, ZExpressions
  {$ifdef zlog},ZLog,SysUtils{$endif};

var
  DefaultMaterial : TMaterial;

procedure EnableMaterial(OldM,NewM : TMaterial); forward;

{$ifndef minimal}
procedure CheckGLError;
var
  Error : GLenum;
begin
  Error := glGetError;
  if Error<>0 then
  begin
    ZLog.GetLog('GL').Write( 'GL ERROR: ' + IntToStr(Error) );
  end;
end;
{$endif}

procedure ApplyRotation(const Rotate : TZVector3f);
begin
  //*180/PI
  if Rotate[0]<>0 then
    glRotatef( (Rotate[0]*360) , 1, 0, 0);
  if Rotate[1]<>0 then
    glRotatef( (Rotate[1]*360) , 0, 1, 0);
  if Rotate[2]<>0 then
    glRotatef( (Rotate[2]*360) , 0, 0, 1);
end;

{ TRenderer }

{$ifndef minimal}
procedure RenderNormals(Mesh : TMesh);
var
  EndPoint : TZVector3f;
  I : integer;
begin
  // draw red unlit lines
  glColor3f(1,0,0);
  glDisable(GL_LIGHTING);
  glBegin(GL_LINES);

  // loop through all verts
  for I := 0 to Mesh.VerticesCount-1 do
  begin
    glVertex3fv(@Mesh.Vertices^[I]);                  // from the vertex point...
    EndPoint := VecAdd3(Mesh.Vertices^[I],Mesh.Normals^[I]); // to the vertex plus the normal
    glVertex3fv(@EndPoint);
  end;
  glEnd();
  glEnable(GL_LIGHTING);
end;
{$endif}

procedure RenderMesh(Mesh: TMesh);
{$DEFINE USE_VERTEX_ARRAYS}
{$ifndef USE_VERTEX_ARRAYS}
var
  I : integer;
{$endif}
begin
  Mesh.BeforeRender;
  case Mesh.Style of
    msTris :
      begin
{$ifdef USE_VERTEX_ARRAYS}
        //Rita med vertex arrayer
        //Verticedata ligger kvar i arbetsminnet
        //Använd vertex buffer objects extension för att kopiera till grafikkortet
        glEnableClientState(GL_VERTEX_ARRAY);
        glVertexPointer(3,GL_FLOAT,0,Mesh.Vertices);
        glEnableClientState(GL_NORMAL_ARRAY);
        glNormalPointer(GL_FLOAT,0,Mesh.Normals);
        if Mesh.TexCoords<>nil then
        begin
          glEnableClientState(GL_TEXTURE_COORD_ARRAY);
          glTexCoordPointer(2,GL_FLOAT,0,Mesh.TexCoords);
        end;
        if Mesh.Colors<>nil then
        begin
          glEnableClientState(GL_COLOR_ARRAY);
          glColorPointer(4,GL_UNSIGNED_BYTE,0,Mesh.Colors);
        end;
        glDrawElements(GL_TRIANGLES,Mesh.IndicesCount,GL_UNSIGNED_SHORT,Mesh.Indices);
        glDisableClientState(GL_VERTEX_ARRAY);
        glDisableClientState(GL_NORMAL_ARRAY);
        glDisableClientState(GL_TEXTURE_COORD_ARRAY);
        glDisableClientState(GL_COLOR_ARRAY);
{$else}
        glBegin(GL_TRIANGLES);
        for I := 0 to Mesh.IndicesCount-1 do
        begin
          glNormal3fv( @Mesh.Normals^[ Mesh.Indices^[I] ] );
          glVertex3fv( @Mesh.Vertices^[ Mesh.Indices^[I] ] );
        end;
        glEnd;
{$endif}
      end;
  end;

  {$ifndef minimal}
  //Display normals for debugging
  if NormalsVisible then
    RenderNormals(Mesh);
  {$endif}
end;

{$ifndef minimal}
procedure AssertRenderMode;
begin
  if not IsRendering then
    raise EZHalted.Create('Render-components can only be used in OnRender');
end;

procedure AssertNotRenderMode;
begin
  if IsRendering then
    raise EZHalted.Create('This component can not be used in OnRender');
end;
{$endif}

var
  CurrentMaterial : TMaterial;

procedure EnableMaterial(OldM,NewM : TMaterial);
const
  //0x8370 GL_MIRRORED_REPEAT, enbart gl 1.4 och uppåt
  TexWrapModes : array[0..2] of integer = ($8370,GL_REPEAT,GL_CLAMP);
var
  NilOld : boolean;
  Tmp : integer;
begin
  {$ifndef minimal}
  AssertRenderMode;
  {$endif}

  if (NewM=nil) then
    Exit;

  if (not VecIsNull4(NewM.Color.V)) then
    glColor4fv(@NewM.Color)
  else
    //default color, alpha is set to 1 by default
    glColor3fv(@ZMath.UNIT_XYZ3);

  //Test for equal material after setting color
  //This is because rendersetcolor may have been called so we need reset material.color
  if NewM=OldM then
    Exit;

  NilOld := OldM=nil;

  CurrentMaterial := NewM;

  if NilOld or (NewM.Shading<>OldM.Shading) then
  begin
    if (not NilOld) and (OldM.Shading=msWireframe) then
    begin //todo: bökigt... slopa wireframe?
      glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
      glLineWidth(1.0);
    end;
    case NewM.Shading of
      msSmooth :
        glShadeModel(GL_SMOOTH);
      msFlat :
        glShadeModel(GL_FLAT);
      msWireframe :
        begin
          //Wireframe
          glPolygonMode(GL_FRONT_AND_BACK,GL_LINE);
          glLineWidth(NewM.WireframeWidth);
        end;
    end;
  end;

  if NilOld or (NewM.Light<>OldM.Light) then
  begin
    if NewM.Light then
      glEnable( GL_LIGHTING )
    else
      glDisable( GL_LIGHTING );
  end;

  if NilOld or (NewM.ZBuffer<>OldM.ZBuffer) then
  begin
    if NewM.ZBuffer then
    begin
      glDepthFunc(GL_LEQUAL);
      glDepthMask(1);
    end
    else
    begin
      //Disable z-buffer: Skip depth-test
      glDepthFunc(GL_ALWAYS);
      glDepthMask(0);
    end;
  end;

  if NilOld or (NewM.DrawBackFace<>OldM.DrawBackFace) then
  begin
    if NewM.DrawBackFace then
      glDisable(GL_CULL_FACE)
    else
      glEnable(GL_CULL_FACE);
  end;

  if NilOld or (NewM.Blend<>OldM.Blend) then
  begin
    if NewM.Blend=mbNoBlend then
      glDisable(GL_BLEND)
    else
    begin
      glEnable(GL_BLEND);
      case NewM.Blend of
        mbA_1MSA : glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
        mbA_1 : glBlendFunc(GL_SRC_ALPHA,GL_ONE);
        mbC_1MSC : glBlendFunc(GL_SRC_COLOR,GL_ONE_MINUS_SRC_COLOR);
        mbAlphaSat_1 : glBlendFunc(GL_SRC_ALPHA_SATURATE,GL_ONE);
      end;
    end;
  end;

  if NilOld or (NewM.Texture<>OldM.Texture) then
  begin
    if NewM.Texture<>nil then
    begin
      glEnable(GL_TEXTURE_2D);
      NewM.Texture.UseTextureBegin;
    end
    else
      glDisable(GL_TEXTURE_2D);
  end;

  if NilOld or
    (NewM.TextureScale[0]<>OldM.TextureScale[0]) or
    (NewM.TextureScale[1]<>OldM.TextureScale[1]) or
    (NewM.TextureX<>OldM.TextureX) or
    (NewM.TextureY<>OldM.TextureY) or
    (NewM.TextureRotate<>OldM.TextureRotate) then
  begin
    //Texture matrix
    //Denna ordning är nödvändig för att scale och rotate ska ske kring texture center (0.5)
    glMatrixMode(GL_TEXTURE);
    glLoadIdentity();
      glTranslatef(NewM.TextureX+0.5,NewM.TextureY+0.5,0);
      glScalef(NewM.TextureScale[0],NewM.TextureScale[1],1);
      glRotatef(NewM.TextureRotate*360,0,0,1);
      glTranslatef(-0.5,-0.5,0);
    glMatrixMode(GL_MODELVIEW);
  end;

  if NilOld or (NewM.TexCoords<>OldM.TexCoords) then
  begin
    //Tex coord generation
    if NewM.TexCoords=tcGenerated then
    begin
      glEnable(GL_TEXTURE_GEN_S);
      glEnable(GL_TEXTURE_GEN_T);
      glTexGeni(GL_S,GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
      glTexGeni(GL_T,GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
    end
    else
    begin
      glDisable(GL_TEXTURE_GEN_S);
      glDisable(GL_TEXTURE_GEN_T);
    end;
  end;

  if NilOld or (NewM.TextureWrapMode<>OldM.TextureWrapMode) then
  begin
    Tmp := TexWrapModes[Ord(NewM.TextureWrapMode)];
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, Tmp );
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, Tmp );
  end;

  if ShadersSupported and (NilOld or (NewM.Shader<>OldM.Shader)) then
  begin
    if NewM.Shader<>nil then
      NewM.Shader.UseShader
    else
      glUseProgram(0);
  end;

end;


procedure Render_Begin;
begin
  {$ifndef minimal}
  IsRendering:=true;
  {$endif}
  //Designer may have messed with default settings, restore
  //Must reset runtime also otherwise mirror_repeat fails
  //Not sure why this is needed
  EnableMaterial(nil,DefaultMaterial)
end;

procedure Render_End;
begin
  EnableMaterial(CurrentMaterial,DefaultMaterial);
  {$ifndef minimal}
  IsRendering:=false;
  {$endif}
end;

procedure RenderModel(Model : TModel);
begin
  glPushMatrix();
  glTranslatef(Model.Position[0],Model.Position[1],Model.Position[2]);
  ApplyRotation(Model.Rotation);
  if not VecIsIdentity3(Model.Scale) then
    glScalef(Model.Scale[0],Model.Scale[1],Model.Scale[2]);
  Model.RunRenderCommands;
  glPopMatrix();
end;

(*procedure _ShaderTest;
const
  VertexSource : pchar = 'void main(void){ gl_Position     = ftransform(); }';
  FragmentSource : pchar = 'void main(void){ gl_FragColor = vec4(1.0,0.5,0.5, 1.0); }';
var
  ProgHandle,VShaderHandle,FShaderHandle : GLuint;
begin
  VShaderHandle := glCreateShader(GL_VERTEX_SHADER);
  glShaderSource(VShaderHandle,1,@VertexSource,nil);
  glCompileShader(VShaderHandle);

  FShaderHandle := glCreateShader(GL_FRAGMENT_SHADER);
  glShaderSource(FShaderHandle,1,@FragmentSource,nil);
  glCompileShader(FShaderHandle);

  ProgHandle := glCreateProgram;
  glAttachShader(ProgHandle,VShaderHandle);
  glAttachShader(ProgHandle,FShaderHandle);
  glLinkProgram(ProgHandle);
end;*)


//Sätt standardvärden för open gl
procedure InitRenderer;
const
  AmbientLight : array[0..3] of single = (0.4, 0.4, 0.4, 1.0);
  //exempel från http://rush3d.com/reference/opengl-redbook-1.1/chapter06.html
//  no_mat : array[0..3] of single = (0.0, 0.0, 0.0, 1.0 );
//  mat_ambient_color : array[0..3] of single = (0.8, 0.8, 0.2, 1.0 );
//  mat_diffuse : packed array[0..3] of single = ( 0.1, 0.5, 0.8, 1.0 );
  //Specular är färg för highlights i direkt ljus
  mat_specular : array[0..3] of single = ( 0.1, 0.1, 0.1, 1.0 );
  no_shininess = 0;
  low_shininess = 5;
  high_shininess = 100;
//  mat_emission : array[0..3] of single = (0.3, 0.2, 0.2, 0.0);
begin
  glClearColor(0.0 , 0.0, 0.0, 0.0);       // Black Background

  glEnable(GL_DEPTH_TEST);

  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  glMatrixMode(GL_MODELVIEW);

  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  //Light
  //Use default position
  glLightModelfv( GL_LIGHT_MODEL_AMBIENT, @AmbientLight );
//  glEnable( GL_LIGHTING );
  glEnable( GL_LIGHT0 );

  //Färg på material följer anrop till glColor. Detta gör att man slipper
  //sätta glMaterial ambient och diffuse separat.
  glEnable ( GL_COLOR_MATERIAL );
  glColorMaterial ( GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE );

  glMaterialfv(GL_FRONT, GL_SPECULAR, @mat_specular);
  glMateriali(GL_FRONT, GL_SHININESS, low_shininess);

  //Viktigt: Annars blir ljus fel vid scaled vectors, t.ex. scale 0.1 då blir det för ljust
  //Detta p.g.a. gl skalar normals vid glScale
  glEnable(GL_NORMALIZE);

  //Extensions must be loaded after video-init, shaders are available on a
  //per-context basis in Win32
  LoadOpenGLExtensions;
//_ShaderTest;

  //Set other default properties using the material-handler
  {$ifndef minimal}IsRendering := True;{$endif}
     EnableMaterial(nil,DefaultMaterial);
  {$ifndef minimal}IsRendering := False;{$endif}
end;

{ TMaterial }

procedure TMaterial.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Texture',{$ENDIF}integer(@Texture) - integer(Self), zptComponentRef);
    {$ifndef minimal}List.GetLast.SetChildClasses([TZBitmap]);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'TextureScale',{$ENDIF}integer(@TextureScale) - integer(Self), zptVector3f);
    List.GetLast.DefaultValue.Vector3fValue := ZMath.UNIT_XYZ3;
  List.AddProperty({$IFNDEF MINIMAL}'TextureX',{$ENDIF}integer(@TextureX) - integer(Self), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}'TextureY',{$ENDIF}integer(@TextureY) - integer(Self), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}'TextureRotate',{$ENDIF}integer(@TextureRotate) - integer(Self), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}'TextureWrapMode',{$ENDIF}integer(@TextureWrapMode) - integer(Self), zptByte);
    {$ifndef minimal}List.GetLast.SetOptions(['Mirror','Tile','Clamp']);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'WireframeWidth',{$ENDIF}integer(@WireframeWidth) - integer(Self), zptFloat);
    List.GetLast.DefaultValue.FloatValue:=4.0;
  List.AddProperty({$IFNDEF MINIMAL}'Shading',{$ENDIF}integer(@Shading) - integer(Self), zptByte);
    {$ifndef minimal}List.GetLast.SetOptions(['Smooth','Flat','Wireframe']);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'Color',{$ENDIF}integer(@Color) - integer(Self), zptColorf);
  List.AddProperty({$IFNDEF MINIMAL}'Light',{$ENDIF}integer(@Light) - integer(Self), zptBoolean);
    List.GetLast.DefaultValue.BooleanValue:=True;
  List.AddProperty({$IFNDEF MINIMAL}'Blend',{$ENDIF}integer(@Blend) - integer(Self), zptByte);
    {$ifndef minimal}List.GetLast.SetOptions(['None','Alpha/OneMinusSourceAlpha','Alpha/One','Color/OneMinusSourceColor','AlphaSaturate/One']);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'TexCoords',{$ENDIF}integer(@TexCoords) - integer(Self), zptByte);
    {$ifndef minimal}List.GetLast.SetOptions(['Generated','ModelDefined']);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'ZBuffer',{$ENDIF}integer(@ZBuffer) - integer(Self), zptBoolean);
    List.GetLast.DefaultValue.BooleanValue:=True;
  List.AddProperty({$IFNDEF MINIMAL}'DrawBackFace',{$ENDIF}integer(@DrawBackFace) - integer(Self), zptBoolean);
  List.AddProperty({$IFNDEF MINIMAL}'Font',{$ENDIF}integer(@Font) - integer(Self), zptComponentRef);
    {$ifndef minimal}List.GetLast.SetChildClasses([TFont]);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'Shader',{$ENDIF}integer(@Shader) - integer(Self), zptComponentRef);
    {$ifndef minimal}List.GetLast.SetChildClasses([TShader]);{$endif}
end;


{ TUseMaterial }

procedure TUseMaterial.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Material',{$ENDIF}integer(@Material) - integer(Self), zptComponentRef);
    {$ifndef minimal}List.GetLast.SetChildClasses([TMaterial]);{$endif}
end;

procedure TUseMaterial.Execute;
begin
  Renderer.EnableMaterial(CurrentMaterial,Self.Material);
end;

{$ifndef minimal}
function TUseMaterial.GetDisplayName: String;
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
  List.AddProperty({$IFNDEF MINIMAL}'Mesh',{$ENDIF}integer(@Mesh) - integer(Self), zptComponentRef);
    {$ifndef minimal}List.GetLast.SetChildClasses([TMesh]);{$endif}
    {$ifndef minimal}List.GetLast.NeedRefreshNodeName := True;{$endif}
end;

procedure TRenderMesh.Execute;
begin
  {$ifndef minimal}
  AssertRenderMode;
  if Mesh=nil then
    Exit;
  {$endif}
  Renderer.RenderMesh(Mesh);
end;

{$ifndef minimal}
function TRenderMesh.GetDisplayName: String;
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
  List.AddProperty({$IFNDEF MINIMAL}'Scale',{$ENDIF}integer(@Scale) - integer(Self), zptVector3f);
    List.GetLast.DefaultValue.Vector3fValue := ZMath.UNIT_XYZ3;
  List.AddProperty({$IFNDEF MINIMAL}'Translate',{$ENDIF}integer(@Translate) - integer(Self), zptVector3f);
  List.AddProperty({$IFNDEF MINIMAL}'Rotate',{$ENDIF}integer(@Rotate) - integer(Self), zptVector3f);
end;

procedure TRenderTransform.Execute;
//Transforms the current matrix
begin
  glTranslatef(Translate[0],Translate[1],Translate[2]);
  ApplyRotation(Rotate);
  if not VecIsIdentity3(Scale) then
    glScalef(Scale[0],Scale[1],Scale[2]);
end;

{ TRenderTransformGroup }

procedure TRenderTransformGroup.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Scale',{$ENDIF}integer(@Scale) - integer(Self), zptVector3f);
    List.GetLast.DefaultValue.Vector3fValue := ZMath.UNIT_XYZ3;
  List.AddProperty({$IFNDEF MINIMAL}'Translate',{$ENDIF}integer(@Translate) - integer(Self), zptVector3f);
  List.AddProperty({$IFNDEF MINIMAL}'Rotate',{$ENDIF}integer(@Rotate) - integer(Self), zptVector3f);
  List.AddProperty({$IFNDEF MINIMAL}'Children',{$ENDIF}integer(@Children) - integer(Self), zptComponentList);
    {$ifndef minimal}List.GetLast.SetChildClasses([TCommand,TZExpression]);{$endif}
end;

procedure TRenderTransformGroup.Execute;
begin
  glPushMatrix();
    glTranslatef(Translate[0],Translate[1],Translate[2]);
    ApplyRotation(Rotate);
    if not VecIsIdentity3(Scale) then
      glScalef(Scale[0],Scale[1],Scale[2]);
    Children.ExecuteCommands;
  glPopMatrix();
end;

procedure TRenderTransformGroup.Update;
begin
  inherited;
  Children.Update;
end;

{ TRenderSprite }

procedure RenderUnitQuad(const X,Y : single);
const
  Width = 1;
  Height = 1;
var
  W,H : single;
begin
  //Y är positivt uppåt
  //Rita i CCW direction för att skapa en front-facing polygon
  //Behövs normals här? Default är 0,0,1.

  //För TexCoords gäller: Y=1 Top, Y=0 Bottom

  glBegin(GL_QUADS);
    W := Width / 2.0;
    H := Height / 2.0;

    //Normal towards camera
    glNormal3f(0,0,1);

    //..
    //x.
    glTexCoord2f(0.0, 0.0);
    glVertex2f(X-W,Y-H);

    //..
    //.x
    glTexCoord2f(1.0, 0.0);
    glVertex2f(X+W,Y-H);

    //.x
    //..
    glTexCoord2f(1.0, 1.0);
    glVertex2f(X+W,Y+H);

    //x.
    //..
    glTexCoord2f(0.0, 1.0);
    glVertex2f(X-W,Y+H);
  glEnd();
end;


procedure TRenderSprite.Execute;
begin
  {$ifndef minimal}
  AssertRenderMode;
  {$endif}
  //todo: ange texgen auto on/off i material?
  {
    TexCoords Generated,ModelDefined
    Z-Buffer on/off (depth)
    DrawBackFace on/off
  }
  RenderUnitQuad(0,0);
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
  List.AddProperty({$IFNDEF MINIMAL}'Count',{$ENDIF}integer(@Count) - integer(Self), zptInteger);
  List.AddProperty({$IFNDEF MINIMAL}'Length',{$ENDIF}integer(@Length) - integer(Self), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}'Width',{$ENDIF}integer(@Width) - integer(Self), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}'Speed',{$ENDIF}integer(@Speed) - integer(Self), zptFloat);
    List.GetLast.DefaultValue.FloatValue := 2;
end;


procedure TRenderBeams.Execute;
var
  I : integer;
  B : TBeam;
  Angle,X,Y,C,S : single;
begin
  {$ifndef minimal}
  AssertRenderMode;
  {$endif}

  glBegin(GL_TRIANGLES);

  for I := 0 to Beams.Count-1 do
  begin
    B := TBeam(Beams[I]);
    glTexCoord2f(0.52,0.52);
    glVertex3f(0,0,0);

    Angle := B.Angle-B.Width/2;
    C := cos(Angle);
    S := sin(Angle);
    X := C * Length;
    Y := S * Length;
    glTexCoord2f( 0.5 + C/2 , 0.5 + S/2);
    glVertex3f(X,Y,0);

    Angle := B.Angle+B.Width/2;
    C := cos(Angle);
    S := sin(Angle);
    X := C * Length;
    Y := S * Length;
    glTexCoord2f( 0.5 + C/2 , 0.5 + S/2);
    glVertex3f(X,Y,0);
  end;

  glEnd;
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
  List.AddProperty({$IFNDEF MINIMAL}'Text',{$ENDIF}integer(@Text) - integer(Self), zptString);
    {$ifndef minimal}List.GetLast.NeedRefreshNodeName:=True;{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'TextFloatRef',{$ENDIF}integer(@TextFloatRef) - integer(Self), zptPropertyRef);
    {$ifndef minimal}List.GetLast.NeedRefreshNodeName:=True;{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'X',{$ENDIF}integer(@X) - integer(Self), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}'Y',{$ENDIF}integer(@Y) - integer(Self), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}'Scale',{$ENDIF}integer(@Scale) - integer(Self), zptFloat);
    List.GetLast.DefaultValue.FloatValue := 1;
  List.AddProperty({$IFNDEF MINIMAL}'Align',{$ENDIF}integer(@Align) - integer(Self), zptByte);
    {$ifndef minimal}List.GetLast.SetOptions(['Center','Left']);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'RenderCharExpression',{$ENDIF}integer(@RenderCharExpression) - integer(Self), zptExpression);
    {$ifndef minimal}
    List.GetLast.DefaultValue.ExpressionValue.Source :=
      '//Modify current character before render.'#13#10 +
      '//CharX,CharY : current coordinate'#13#10 +
      '//CharI : current character index (read only)'#13#10 +
      '//CharRotate : current character rotation in radians'#13#10 +
      '//CharScale : current character scale';
    {$endif}
  List.AddProperty({$IFNDEF MINIMAL}'FloatMultiply',{$ENDIF}integer(@FloatMultiply) - integer(Self), zptFloat);
    List.GetLast.DefaultValue.FloatValue := 1;
  List.AddProperty({$IFNDEF MINIMAL}'UseModelSpace',{$ENDIF}integer(@UseModelSpace) - integer(Self), zptBoolean);
  List.AddProperty({$IFNDEF MINIMAL}'StretchY',{$ENDIF}integer(@StretchY) - integer(Self), zptFloat);
    List.GetLast.DefaultValue.FloatValue := 1;

  List.AddProperty({$IFNDEF MINIMAL}'CharX',{$ENDIF}integer(@CharX) - integer(Self), zptFloat);
    List.GetLast.NeverPersist:=True;
  List.AddProperty({$IFNDEF MINIMAL}'CharY',{$ENDIF}integer(@CharY) - integer(Self), zptFloat);
    List.GetLast.NeverPersist:=True;
  List.AddProperty({$IFNDEF MINIMAL}'CharI',{$ENDIF}integer(@CharI) - integer(Self), zptFloat);
    List.GetLast.NeverPersist:=True;
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'CharRotate',{$ENDIF}integer(@CharRotate) - integer(Self), zptFloat);
    List.GetLast.NeverPersist:=True;
  List.AddProperty({$IFNDEF MINIMAL}'CharScale',{$ENDIF}integer(@CharScale) - integer(Self), zptFloat);
    List.GetLast.NeverPersist:=True;
end;

procedure TRenderText.Execute;
const
  BuiltInSpacing = -0.18; //Slight overlap to put chars closer to each other (for built-in font)
  CharsScreen = 20; //Scale 1 = 20 characters on screen
var
  CurChar,CharLen : integer;
  P : PByte;
  XStep,StartX : single;
  Spacing,YScaleFactor : single;
  TheText : PByte;
  FontSize : integer;
  FloatBuf : array[0..19] of char;
  TextBuf : array[0..254] of char;
  CurFont : TFont;
  UseBuiltInFont : boolean;
begin
  {$ifndef minimal}
  AssertRenderMode;
  {$endif}

  //builtin font måste init innan rendering börjar

  UseBuiltInFont := (CurrentMaterial=nil) or (CurrentMaterial.Font=nil);

  if not UseBuiltInFont then
  begin
    CurFont := CurrentMaterial.Font;
    Spacing := 0;
  end
  else
  begin
    CurFont := ZApp.Font;
    Spacing := BuiltInSpacing;
  end;
  CurFont.Prepare;

  if TextFloatRef.Component<>nil then
  begin
    //If textref is set then convert float-value to string
    ZStrConvertFloat(
      PFloat(TextFloatRef.Component.GetPropertyPtr(TextFloatRef.Prop,TextFloatRef.Index))^
      * Self.FloatMultiply,
      PChar(@FloatBuf));
    if pointer(Self.Text)<>nil then
      ZStrCopy(TextBuf,PChar(Self.Text))
    else
      TextBuf[0]:=#0;
    ZStrCat(TextBuf,PChar(@FloatBuf));
    TheText := @TextBuf;
  end
  else
    TheText := PByte(Text);

  if TheText=nil then
    Exit;

  //todo move to Begin2D(), End2D()-procs
  if not Self.UseModelSpace then
  begin
    glMatrixMode(GL_PROJECTION);
    glPushMatrix();
    glLoadIdentity();
    glOrtho(-1, 1, -1, 1, 1, -1.0);
    glMatrixMode(GL_MODELVIEW);
    glPushMatrix();
    glLoadIdentity();
    //Must account for viewpointratio here because of the -1 .. 1 resolution
    YScaleFactor := ZApp.ActualViewportRatio;
  end
  else
  begin
    glPushMatrix();
    YScaleFactor := 1;
  end;


//glEnable(GL_BLEND);
//glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
//glDisable(GL_DEPTH_TEST);


  glPushAttrib(GL_TEXTURE_BIT);

  glEnable(GL_TEXTURE_2D);
  //Force disable automatic tex-coords
  glDisable(GL_TEXTURE_GEN_S);
  glDisable(GL_TEXTURE_GEN_T);

// glColor3f(1,1,1);

  glTranslatef(Self.X,Self.Y,0);
  //Scale 1 = 20 characters width on screen
  glScalef(Scale * 2/(CharsScreen+CharsScreen*Spacing),
    Scale * 2/(CharsScreen+CharsScreen*Spacing) * YScaleFactor * StretchY,
    1);

  //Font pixel size
  if not Self.UseModelSpace then
    FontSize := Round(Scale * (ScreenWidth / CharsScreen))
  else
    FontSize := 16;

  //Get length of first line in string
  P := pointer(TheText);
  while not (P^ in [0,13]) do
    Inc(P);
  CharLen := integer(P) - integer(TheText);

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
    while P^=13 do
    begin
      CharX := StartX;
      CharY := CharY - 1.0;
      Inc(P,2); //skip LF
    end;

    CurChar := P^;

    ZExpressions.RunCode(RenderCharExpression.Code);

    glPushMatrix;
      glTranslatef(CharX,CharY,0);
      glScalef(CharScale,CharScale,1);
      glRotatef(CharRotate*180/PI,0,0,1);
      CurFont.RenderCharacter(CurChar,FontSize);
    glPopMatrix;

    CharX := CharX + XStep;

    CharI := CharI + 1;
    Inc(P);
  end;

  glPopAttrib;

  // restore matrices, todo End2D()-proc
  if not Self.UseModelSpace then
  begin
    glPopMatrix();
    glMatrixMode(GL_PROJECTION);
    glPopMatrix();
    glMatrixMode(GL_MODELVIEW);
  end else
    glPopMatrix();

end;

{$ifndef minimal}
function TRenderText.GetDisplayName: String;
begin
  Result := inherited GetDisplayName;
  if Text<>'' then
    Result := Result + '  ' + Text;
  if TextFloatRef.Component<>nil then
    Result := Result + '  ' + GetPropRefAsString(TextFloatRef);
end;
{$endif}


{ TFont }

const
  FontMinSize1 = bs16;
  FontMinSize2 = 16;

procedure TFont.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Bitmap',{$ENDIF}integer(@Bitmap) - integer(Self), zptComponentRef);
    {$ifndef minimal}List.GetLast.SetChildClasses([TZBitmap]);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'FirstChar',{$ENDIF}integer(@FirstChar) - integer(Self), zptInteger);
  List.AddProperty({$IFNDEF MINIMAL}'CharPixelWidth',{$ENDIF}integer(@CharPixelWidth) - integer(Self), zptInteger);
  List.AddProperty({$IFNDEF MINIMAL}'CharPixelHeight',{$ENDIF}integer(@CharPixelHeight) - integer(Self), zptInteger);
  List.AddProperty({$IFNDEF MINIMAL}'BorderPixels',{$ENDIF}integer(@BorderPixels) - integer(Self), zptInteger);
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

procedure TFont.RenderCharacter(Char, Size : integer);
var
  Characters : TZComponentList;
  CurSize,FontSize : integer;
  IsBuiltIn : boolean;
  B : TZBitmap;
begin
  IsBuiltIn := Self.Bitmap=nil;

  if IsBuiltIn then
  begin
    //Use builtin truetype font (win32 only)
    Dec(Char,33);
    //Pick the right font for the size
    FontSize := 0;
    CurSize := FontMinSize2;
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
      RenderUnitQuad(0,0);
      //B.UseTextureEnd;
    end;
  end else
  begin
    //Use imported texture
    Dec(Char,Self.FirstChar);
    if Char>=0 then
    begin
      glMatrixMode(GL_TEXTURE);
      glPushMatrix();
      glLoadIdentity();
      glTranslatef( BmStruct.TransX*(Char mod BmStruct.CharsPerRow) ,
        BmStruct.TopLeftY - (BmStruct.TransY*((Char div BmStruct.CharsPerRow)+1)),0);
      glScalef(BmStruct.ScaleX,BmStruct.ScaleY,1);
      glMatrixMode(GL_MODELVIEW);
        Bitmap.UseTextureBegin;
        RenderUnitQuad(0,0);
        //Bitmap.UseTextureEnd;
      glMatrixMode(GL_TEXTURE);
      glPopMatrix();
      glMatrixMode(GL_MODELVIEW);
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
  CurSize1 : TBitmapSize;
  CurSize2 : integer;
  Characters : TZComponentList;
begin
  {$ifndef minimal} //Undvik init fler gånger
  if Assigned(FontSizes[0]) then Exit;
  {$endif}

  //ladda font
  CharCount := LastChar-FirstCharBuiltIn+1;
  CurSize1 := FontMinSize1;
  CurSize2 := FontMinSize2;
  for J := 0 to High(FontSizes) do
  begin
    Lists := Platform_GenerateFontDisplayLists(CurSize2,FirstCharBuiltIn,LastChar);
    Characters := TZComponentList.Create;
    FontSizes[J]:= Characters;
    for I := 0 to CharCount-1 do
    begin
      B:=TZBitmap.Create(Characters);
      B.PropHeight:=CurSize1;
      B.PropWidth:=CurSize1;
      B.RenderTargetBegin;

      glClearColor(0,0,0,0);
      glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT or GL_STENCIL_BUFFER_BIT);

  //    glColor4f(0.5,0,0,1);
  //    RenderUnitQuad(0,0);
  //    glColor4f(0,0.5,0,0.5);
   //   RenderUnitQuad(0.2,0.2);
  //     glColor4f(1,1,1,1);

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
  {  glColor3f(0.5,0.1,0.1);
    glRasterPos2f(0,0);

    glCallList(LargerLists + I);
    glColor3f(1,1,1);
    glRasterPos2f(0 - RasterPos[0]/FontSize,-0.7);}
  //glDisable(GL_BLEND);

      glCallList(Lists + I);

  {  glColor4f(1,1,1,0.5);
    RenderUnitQuad(-0.5,-0.5);
    glColor3f(1,1,1);}

      B.RenderTargetEnd;

  {    B.RenderTargetBegin;
      B.UseTextureBegin;
        glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
        glEnable(GL_TEXTURE_2D);
        glScalef(2,2,0);
        glColor3f(0.6,0.6,0.6);
        RenderUnitQuad(0,0);

        glEnable(GL_BLEND);
        glBlendFunc(GL_SRC_COLOR,GL_ONE_MINUS_SRC_COLOR);
        glScalef(1,1,0);
        glColor3f(1,1,1);
        RenderUnitQuad(0.05,0.05);

      B.UseTextureEnd;
      B.RenderTargetEnd;}
    end;

    glDeleteLists(Lists,CharCount);
    Inc(CurSize1);
    Inc(CurSize2,CurSize2);
  end;
  //glDeleteLists(LargerLists,CharCount);
end;

{ TRenderSetColor }

procedure TRenderSetColor.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Color',{$ENDIF}integer(@Color) - integer(Self), zptColorf);
end;

procedure TRenderSetColor.Execute;
begin
  glColor4fv(@Color);
end;

{ TRenderNet }

procedure TRenderNet.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'XCount',{$ENDIF}integer(@XCount) - integer(Self), zptInteger);
  List.AddProperty({$IFNDEF MINIMAL}'YCount',{$ENDIF}integer(@YCount) - integer(Self), zptInteger);
  List.AddProperty({$IFNDEF MINIMAL}'RenderVertexExpression',{$ENDIF}integer(@RenderVertexExpression) - integer(Self), zptExpression);
    {$ifndef minimal}
    List.GetLast.DefaultValue.ExpressionValue.Source :=
      '//Update each vertex.'#13#10 +
      '//Vertex : current vertex'#13#10 +
      '//TexCoord : current texture coordinate';
    {$endif}
  List.AddProperty({$IFNDEF MINIMAL}'Vertex',{$ENDIF}integer(@Vertex) - integer(Self), zptVector3f);
    List.GetLast.NeverPersist:=True;
  List.AddProperty({$IFNDEF MINIMAL}'TexCoord',{$ENDIF}integer(@TexCoord) - integer(Self), zptVector3f);
    List.GetLast.NeverPersist:=True;
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
begin
  {$ifndef minimal}
  AssertRenderMode;
  {$endif}

  if (Mesh=nil) then
    Mesh := TMesh.Create(nil);

  Mesh.MakeNet(XCount,YCount);

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
      Inc(P);
      Inc(Tex);
    end;
  end;

  //Recompute normals, the vertice have changed
  Mesh.ComputeNormals;

  RenderMesh(Mesh);
end;


{ TRenderParticles }

type
  TParticle = class
  private
    LifeTime : single;
    Position,Velocity,Acceleration : TZVector2f;
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
  inherited;
end;

//Define to use interleaved arrays
//Not worth the effort on nvidia
{.$define USE_PARTICLE_ARRAYS}
procedure TRenderParticles.Execute;
var
  I : integer;
  P : TParticle;
  X,Y,W,H : single;
  Tmp : TZVector2f;
  {$ifdef USE_PARTICLE_ARRAYS}
  Ar,PAr : PFloat;
  ColorB : integer;
  {$endif}
begin
  {$ifndef minimal}
  AssertRenderMode;
  {$endif}

  if not Self.FollowModel then
  begin
    if CurrentModel<>nil then
    begin
      Tmp := PZVector2f(@CurrentModel.Position)^;
      glTranslatef(-Tmp[0],-Tmp[1],0);
    end;
  end;

  //Uncomment to use interleaved arrays
  {$ifdef USE_PARTICLE_ARRAYS}
  if Particles.Count>0 then
  begin
    GetMem(Ar,Particles.Count * 4 * (SizeOf(Single)*6) );

    PAr := Ar;
    for I := 0 to Particles.Count-1 do
    begin
      P := TParticle(Particles[I]);

      X := P.Position[0];
      Y := P.Position[1];
      W := P.Width * 0.5;
      H := P.Height * 0.5;

      //T2F C4UB V3F

      ColorB := ColorFtoB(P.Color);

      //..
      //x.
      PAr^ := 0.0; Inc(PAr);
      PAr^ := 0.0; Inc(PAr);
      PInteger(PAr)^ := ColorB; Inc(PAr);
      PAr^ := X-W; Inc(PAr);
      PAr^ := Y-H; Inc(PAr);
      PAr^ := 0; Inc(PAr);

      //..
      //.x
      PAr^ := 1.0; Inc(PAr);
      PAr^ := 0.0; Inc(PAr);
      PInteger(PAr)^ := ColorB; Inc(PAr);
      PAr^ := X+W; Inc(PAr);
      PAr^ := Y-H; Inc(PAr);
      PAr^ := 0; Inc(PAr);

      //.x
      //..
      PAr^ := 1.0; Inc(PAr);
      PAr^ := 1.0; Inc(PAr);
      PInteger(PAr)^ := ColorB; Inc(PAr);
      PAr^ := X+W; Inc(PAr);
      PAr^ := Y+H; Inc(PAr);
      PAr^ := 0; Inc(PAr);

      //x.
      //..
      PAr^ := 0.0; Inc(PAr);
      PAr^ := 1.0; Inc(PAr);
      PInteger(PAr)^ := ColorB; Inc(PAr);
      PAr^ := X-W; Inc(PAr);
      PAr^ := Y+H; Inc(PAr);
      PAr^ := 0; Inc(PAr);
    end;

    glInterleavedArrays(GL_T2F_C4UB_V3F,0,Ar);
    glDrawArrays(GL_QUADS, 0, Particles.Count*4);

    FreeMem(Ar);
  end;
  {$endif}

  {$ifndef USE_PARTICLE_ARRAYS}
  glBegin(GL_QUADS);
  for I := 0 to Particles.Count-1 do
  begin
    P := TParticle(Particles[I]);

    X := P.Position[0];
    Y := P.Position[1];
    W := P.Width * 0.5;
    H := P.Height * 0.5;

    glColor4fv(@P.Color);

    //..
    //x.
    glTexCoord2f(0.0, 0.0);
    glVertex2f(X-W,Y-H);

    //..
    //.x
    glTexCoord2f(1.0, 0.0);
    glVertex2f(X+W,Y-H);

    //.x
    //..
    glTexCoord2f(1.0, 1.0);
    glVertex2f(X+W,Y+H);

    //x.
    //..
    glTexCoord2f(0.0, 1.0);
    glVertex2f(X-W,Y+H);
  end;
  glEnd;
  {$endif}

  //Restore default color
  glColor3fv(@ZMath.UNIT_XYZ3);
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

//  P.Acceleration[0] := Sin(Angle+0.5)*8;
//  P.Acceleration[1] := Cos(Angle+0.5)*8;

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
  DAlpha,T,DT : single;
  ScaledGravity : TZVector2f;
  UseGravity : boolean;
begin
  if ZApp.Clock.FrameLoss and (System.Random<0.5) then
    Exit; //If we are losing frames then randomly skip update for trying to catch up

  //Copy to local for making the loop faster
  DT:=ZApp.DeltaTime;

  UseGravity := not VecIsNull3(Gravity);

  //Update positions and remove particles
  DAlpha := Self.AnimateAlpha * DT;

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

      P.Velocity[0] := P.Velocity[0] + P.Acceleration[0] * DT;
      P.Velocity[1] := P.Velocity[1] + P.Acceleration[1] * DT;

      if UseGravity then
        VecAdd2_Inplace( PZVector2f(@P.Velocity)^, ScaledGravity );

      P.Color.A := Clamp(P.Color.A + DAlpha,0,1);

//      P.Rotation := P.Rotation + P.RotationSpeed * DT;
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
  List.AddProperty({$IFNDEF MINIMAL}'ParticlesPerSecond',{$ENDIF}integer(@ParticlesPerSecond) - integer(Self), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}'Direction',{$ENDIF}integer(@Direction) - integer(Self), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}'Spread',{$ENDIF}integer(@Spread) - integer(Self), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}'ParticleWidth',{$ENDIF}integer(@ParticleWidth) - integer(Self), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}'ParticleHeight',{$ENDIF}integer(@ParticleHeight) - integer(Self), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}'Speed',{$ENDIF}integer(@Speed) - integer(Self), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}'SpeedRange',{$ENDIF}integer(@SpeedRange) - integer(Self), zptScalar);
  List.AddProperty({$IFNDEF MINIMAL}'Radius',{$ENDIF}integer(@Radius) - integer(Self), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}'ParticleLifetime',{$ENDIF}integer(@ParticleLifetime) - integer(Self), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}'AnimateAlpha',{$ENDIF}integer(@AnimateAlpha) - integer(Self), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}'Duration',{$ENDIF}integer(@Duration) - integer(Self), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}'BeginTime',{$ENDIF}integer(@BeginTime) - integer(Self), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}'OnEmitExpression',{$ENDIF}integer(@OnEmitExpression) - integer(Self), zptExpression);
    {$ifndef minimal}
    List.GetLast.DefaultValue.ExpressionValue.Source :=
      '//Emit particle.'#13#10 +
      '//PColor : particle color, PAngle : particle angle'#13#10 +
      '';
    {$endif}
  List.AddProperty({$IFNDEF MINIMAL}'Gravity',{$ENDIF}integer(@Gravity) - integer(Self), zptVector3f);
  List.AddProperty({$IFNDEF MINIMAL}'FollowModel',{$ENDIF}integer(@FollowModel) - integer(Self), zptBoolean);
    List.GetLast.DefaultValue.BooleanValue:=True;
  List.AddProperty({$IFNDEF MINIMAL}'PColor',{$ENDIF}integer(@PColor) - integer(Self), zptColorf);
    List.GetLast.NeverPersist:=True;
  List.AddProperty({$IFNDEF MINIMAL}'PAngle',{$ENDIF}integer(@PAngle) - integer(Self), zptFloat);
    List.GetLast.NeverPersist:=True;
end;


{ TShader }

procedure TShader.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'VertexShaderSource',{$ENDIF}integer(@VertexShaderSource) - integer(Self), zptString);
  List.AddProperty({$IFNDEF MINIMAL}'FragmentShaderSource',{$ENDIF}integer(@FragmentShaderSource) - integer(Self), zptString);
  List.AddProperty({$IFNDEF MINIMAL}'UniformVariables',{$ENDIF}integer(@UniformVariables) - integer(Self), zptComponentList);
    {$ifndef minimal}List.GetLast.SetChildClasses([TShaderVariable]);{$endif}
end;

destructor TShader.Destroy;
begin
  CleanUp;
  inherited;
end;

procedure TShader.ReInit;

  {$ifndef minimal}
  function InCheckShaderValid(Shader : PGLuint; Kind: GLEnum) : boolean;
  var
    Status : GLUInt;
    S : string;
  begin
    glGetShaderiv(Shader^,GL_COMPILE_STATUS,@Status);
    if Kind=GL_VERTEX_SHADER then
      S := 'Vertex'
    else
      S := 'Fragment';
    if Status=GL_TRUE then
    begin
      ZLog.GetLog(Self.ClassName).Write( S + ' shader compiled OK' );
      Result := True
    end
    else
    begin
      ZLog.GetLog(Self.ClassName).Write( 'Error in ' + S + ' shader compilation' );
      //Remove the incorrect shader, otherwise it try to unattach in cleanup
      glDeleteShader(Shader^);
      Shader^ := 0;
      Result := False;
    end;
  end;

  procedure InCheckProgramStatus;
  var
    Status : GLUInt;
  begin
    glGetProgramiv(ProgHandle,GL_LINK_STATUS,@Status);
    if Status=GL_FALSE then
      ZLog.GetLog(Self.ClassName).Write( 'Error when linking shader program' );
    glValidateProgram(ProgHandle);
    glGetProgramiv(ProgHandle,GL_VALIDATE_STATUS,@Status);
    if Status=GL_FALSE then
      ZLog.GetLog(Self.ClassName).Write( 'Error when linking shader program' );
  end;
  {$endif}

  function InCreate(const Source : TPropString; const Kind: GLEnum) : cardinal;
  begin
    Result := 0;
    if (pointer(Source)=nil) or (ZStrLength(PChar(Source))=0) then
      Exit;
    Result := glCreateShader(Kind);
    glShaderSource(Result,1,@Source,nil);
    glCompileShader(Result);
    {$ifndef minimal}
    if InCheckShaderValid(@Result,Kind) then
    {$endif}
    glAttachShader(ProgHandle,Result);
    {$ifndef minimal}CheckGLError;{$endif}
  end;

begin
  CleanUp;

  ProgHandle := glCreateProgram;

  VShaderHandle := InCreate(VertexShaderSource,GL_VERTEX_SHADER);
  FShaderHandle := InCreate(FragmentShaderSource,GL_FRAGMENT_SHADER);

  glLinkProgram(ProgHandle);
  {$ifndef minimal}InCheckProgramStatus;{$endif}

  IsChanged := False;
end;

procedure TShader.UpdateVariableValues;
var
  I : integer;
  Sv : TShaderVariable;
  V : single;
begin
  for I := 0 to UniformVariables.Count - 1 do
  begin
    Sv := TShaderVariable(UniformVariables[I]);
    {$ifndef minimal}
    if Sv.Location=-1 then Continue;
    {$endif}

    if Sv.ValuePropRef.Component<>nil then
      V := PFloat(Sv.ValuePropRef.Component.GetPropertyPtr(Sv.ValuePropRef.Prop,Sv.ValuePropRef.Index))^
    else
      V := Sv.Value;

    glUniform1f(Sv.Location,V);
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
    if Length(Sv.VariableName)=0 then Continue;
    {$endif}
    Sv.Location := glGetUniformLocation(ProgHandle,pointer(Sv.VariableName));
    {$ifndef minimal}
    if Sv.Location=-1 then
      ZLog.GetLog(Self.ClassName).Write( 'Shader variable error: ' + Sv.VariableName );
    {$endif}
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

  //Update uniform variables
  if (UniformVariables.Count>0)
    //Time is not moving in designer, need to update anyway
    {$ifdef minimal}and (LastVariableUpdate<>ZApp.Time){$endif}
    then
  begin
    if ReinitDone then
      //Uniform var locations must be updated after new linkage, and after glUseProgram
      UpdateVariableLocations;
    UpdateVariableValues;
    LastVariableUpdate := ZApp.Time;
  end;
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

  glDeleteProgram(ProgHandle);
  ProgHandle := 0;
end;

{ TShaderVariable }

procedure TShaderVariable.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'VariableName',{$ENDIF}integer(@VariableName) - integer(Self), zptString);
    {$ifndef minimal}List.GetLast.NeedRefreshNodeName := True;{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'Value',{$ENDIF}integer(@Value) - integer(Self), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}'ValuePropRef',{$ENDIF}integer(@ValuePropRef) - integer(Self), zptPropertyRef);
end;

{$ifndef minimal}
function TShaderVariable.GetDisplayName: String;
begin
  Result := inherited GetDisplayName;
  Result := Result + '  ' + VariableName;
end;
{$endif}

initialization

  ZClasses.Register(TMaterial,MaterialClassId);
    {$ifndef minimal}ComponentManager.LastAdded.AutoName := True;{$endif}
  ZClasses.Register(TShader,ShaderClassId);
    {$ifndef minimal}ComponentManager.LastAdded.AutoName := True;{$endif}
  ZClasses.Register(TShaderVariable,ShaderVariableClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NeedParentList := 'UniformVariables';{$endif}
    {$ifndef minimal}ComponentManager.LastAdded.NoTopLevelCreate := True;{$endif}
  ZClasses.Register(TUseMaterial,UseMaterialClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NeedParentList := 'OnRender';{$endif}
  ZClasses.Register(TRenderMesh,RenderMeshClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NeedParentList := 'OnRender';{$endif}
    {$ifndef minimal}ComponentManager.LastAdded.ImageIndex := 10;{$endif}
  ZClasses.Register(TRenderTransform,RenderTransformClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NeedParentList := 'OnRender';{$endif}
  ZClasses.Register(TRenderSprite,RenderSpriteClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NeedParentList := 'OnRender';{$endif}
  ZClasses.Register(TRenderBeams,RenderBeamsClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NeedParentList := 'OnRender';{$endif}
  ZClasses.Register(TRenderTransformGroup,RenderTransformGroupClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NeedParentList := 'OnRender';{$endif}
  ZClasses.Register(TRenderText,RenderTextClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NeedParentList := 'OnRender';{$endif}
    {$ifndef minimal}ComponentManager.LastAdded.ImageIndex := 12;{$endif}
  ZClasses.Register(TFont,FontClassId);
    {$ifndef minimal}ComponentManager.LastAdded.ImageIndex := 12;{$endif}
  ZClasses.Register(TRenderSetColor,RenderSetColorClassId);
    {$ifndef minimal}ComponentManager.LastAdded.HelpText := 'Set the current OpenGL color';{$endif}
    {$ifndef minimal}ComponentManager.LastAdded.NeedParentList := 'OnRender';{$endif}
    {$ifndef minimal}ComponentManager.LastAdded.ImageIndex := 11;{$endif}
  ZClasses.Register(TRenderNet,RenderNetClassId);
    {$ifndef minimal}ComponentManager.LastAdded.HelpText := 'Render a 2D net';{$endif}
    {$ifndef minimal}ComponentManager.LastAdded.NeedParentList := 'OnRender';{$endif}
  ZClasses.Register(TRenderParticles,RenderParticlesClassId);
    {$ifndef minimal}ComponentManager.LastAdded.HelpText := 'Simple 2D particlesystem';{$endif}
    {$ifndef minimal}ComponentManager.LastAdded.NeedParentList := 'OnRender';{$endif}

  DefaultMaterial := TMaterial.Create(nil);

{$ifndef minimal}
finalization
  DefaultMaterial.Free;
{$endif}
end.
