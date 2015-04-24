unit GLDrivers;

interface

uses ZOpenGL, Meshes, ZClasses, Renderer, ZBitmap;

type
  TGLBase = (glbFixed,glbProgrammable);

  TGLDriverBase = class
  public
    CurrentMaterial : TMaterial;
    Kind : TGLBase;
    procedure Translate(const X,Y,Z : GLfloat); virtual; abstract;
    procedure Rotate(const Angle, X, Y, Z: GLfloat); virtual; abstract;
    procedure Scale(const X,Y,Z: GLfloat); virtual; abstract;
    procedure MatrixMode(const mode: GLenum); virtual; abstract;
    procedure LoadIdentity; virtual; abstract;
    procedure Ortho(const left, right, bottom, top, zNear, zFar: GLfloat); virtual; abstract;
    procedure PushMatrix; virtual; abstract;
    procedure PopMatrix; virtual; abstract;
    procedure RenderMesh(Mesh : TMesh); virtual; abstract;
    procedure RenderArrays(const Mode: GLenum; const Count,VertElements : integer; const Verts,Texc,Cols : pointer); virtual; abstract;
    procedure Perspective(const fovy,aspect,zNear,zFar : GLfloat); virtual; abstract;
    procedure ApplyRotation(const R : TZVector3f);
    procedure SetCurrentShader(Shader : TShader); virtual; abstract;
    procedure BeforeLinkShader(S : TShader); virtual; abstract;
    procedure AfterLinkShader(S : TShader); virtual; abstract;
    procedure SetColor(const C : TZColorf); virtual; abstract;
    procedure GetMatrix(const Index : integer; Mat : PZMatrix4f); virtual; abstract;
    procedure SetMatrix(const Index : integer; const Mat : TZMatrix4f); virtual; abstract;
    procedure UpdateNormalMatrix; virtual; abstract;
    procedure InitGL;
    procedure EnableMaterial(NewM : TMaterial);
    procedure RenderUnitQuad;
    procedure RenderQuad(Tex : TZBitmap; const x,y,w,h,TexX,TexY : integer; const FlipH,FlipV : boolean);
  end;

function CreateDriver(Kind : TGLBase) : TGLDriverBase;

implementation

uses ZMath, zplatform
{$ifndef minimal},ZLog{$endif}
;

const
  {$if SizeOf(TMeshVertexIndex)=2}
  TMeshVertexIndex_GL = GL_UNSIGNED_SHORT;
  {$ifend}
  {$if SizeOf(TMeshVertexIndex)=4}
  TMeshVertexIndex_GL = GL_UNSIGNED_INT;
  {$ifend}

type
  TGLDriverFixed = class(TGLDriverBase)
  public
    procedure Translate(const X,Y,Z: GLfloat); override;
    procedure Rotate(const Angle, X, Y, Z: GLfloat); override;
    procedure Scale(const X,Y,Z: GLfloat); override;
    procedure MatrixMode(const mode: GLenum); override;
    procedure LoadIdentity; override;
    procedure Ortho(const left, right, bottom, top, zNear, zFar: GLfloat); override;
    procedure PushMatrix; override;
    procedure PopMatrix; override;
    procedure RenderMesh(Mesh : TMesh); override;
    procedure RenderArrays(const Mode: GLenum; const Count,VertElements : integer; const Verts,Texc,Cols : pointer); override;
    procedure SetCurrentShader(Shader : TShader); override;
    procedure BeforeLinkShader(S : TShader); override;
    procedure AfterLinkShader(S : TShader); override;
    procedure Perspective(const fovy,aspect,zNear,zFar : GLfloat); override;
    procedure SetColor(const C : TZColorf); override;
    procedure GetMatrix(const Index : integer; Mat : PZMatrix4f); override;
    procedure SetMatrix(const Index : integer; const Mat : TZMatrix4f); override;
    procedure UpdateNormalMatrix; override;
  end;

  TGLDriverProgrammable = class(TGLDriverBase)
  strict private
    MStack : array[0..2, 0..15] of TZMatrix4f;
    MPtrs : array[0..2] of PZMatrix4f;
    MDirty : array[0..2] of boolean;
    MMode : integer;
    CurrentShader : TShader;
    MVP : TZMatrix4f;
    NormalM : array[0..2,0..2] of single;
    GlobalColor : TZColorf;
    procedure UpdateUniforms;
  public
    procedure Translate(const X,Y,Z: GLfloat); override;
    procedure Rotate(const Angle, X, Y, Z: GLfloat); override;
    procedure Scale(const X,Y,Z: GLfloat); override;
    procedure MatrixMode(const mode: GLenum); override;
    procedure LoadIdentity; override;
    procedure PushMatrix; override;
    procedure PopMatrix; override;
    procedure RenderMesh(Mesh : TMesh); override;
    procedure SetCurrentShader(Shader : TShader); override;
    procedure BeforeLinkShader(S : TShader); override;
    procedure AfterLinkShader(S : TShader); override;
    procedure Perspective(const fovy,aspect,zNear,zFar : GLfloat); override;
    procedure Ortho(const left, right, bottom, top, zNear, zFar: GLfloat); override;
    procedure RenderArrays(const Mode: GLenum; const Count,VertElements : integer; const Verts,Texc,Cols : pointer); override;
    procedure SetColor(const C : TZColorf); override;
    procedure GetMatrix(const Index : integer; Mat : PZMatrix4f); override;
    procedure SetMatrix(const Index : integer; const Mat : TZMatrix4f); override;
    procedure UpdateNormalMatrix; override;
    constructor Create;
  end;

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
    glVertex3f(Mesh.Vertices^[I][0],Mesh.Vertices^[I][1],Mesh.Vertices^[I][2]);                  // from the vertex point...
    EndPoint := VecAdd3(Mesh.Vertices^[I],Mesh.Normals^[I]); // to the vertex plus the normal
    glVertex3f(EndPoint[0],EndPoint[1],EndPoint[2]);
  end;
  glEnd();
  glEnable(GL_LIGHTING);
end;
{$endif}

{ TGLDriverBase }

procedure TGLDriverBase.ApplyRotation(const R: TZVector3f);
begin
  //*180/PI
  //Reverse order to make XYZ-rotation
  if R[2]<>0 then
    Rotate( (R[2]*360) , 0, 0, 1);
  if R[1]<>0 then
    Rotate( (R[1]*360) , 0, 1, 0);
  if R[0]<>0 then
    Rotate( (R[0]*360) , 1, 0, 0);
end;

procedure TGLDriverBase.InitGL;
const
  //exempel från http://rush3d.com/reference/opengl-redbook-1.1/chapter06.html
  //Specular är färg för highlights i direkt ljus
  Specular : array[0..3] of single = ( 0.1, 0.1, 0.1, 1.0 );
  LowShininess = 5;
begin
  LoadOpenGL(Ord(Self.Kind));

  glEnable(GL_DEPTH_TEST);

  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  if Self.Kind=glbFixed then
  begin
    //Light
    //Use default position
    glEnable(GL_LIGHTING);
    glEnable(GL_LIGHT0);

    //Färg på material följer anrop till glColor. Detta gör att man slipper
    //sätta glMaterial ambient och diffuse separat.
    glEnable(GL_COLOR_MATERIAL);
    glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);

    glMaterialfv(GL_FRONT, GL_SPECULAR, @Specular);
    glMaterialf(GL_FRONT, GL_SHININESS, LowShininess);
  end;

  //Viktigt: Annars blir ljus fel vid scaled vectors, t.ex. scale 0.1 då blir det för ljust
  //Detta p.g.a. gl skalar normals vid glScale
  glEnable(GL_NORMALIZE);

  //Extensions must be loaded after video-init, shaders are available on a
  //per-context basis in Win32
  LoadOpenGLExtensions;

  //Set other default properties using the material-handler
  Self.CurrentMaterial := nil;
  EnableMaterial(DefaultMaterial);
end;

procedure TGLDriverBase.EnableMaterial(NewM : TMaterial);
const
  //0x8370 GL_MIRRORED_REPEAT, enbart gl 1.4 och uppåt
  TexWrapModes : array[0..2] of integer = ($8370,GL_REPEAT,GL_CLAMP_TO_EDGE);
var
  NilOld : boolean;
  Tmp,I,TexCount : integer;
  Tex : TMaterialTexture;
  OldM : TMaterial;
begin
  OldM := Self.CurrentMaterial;
  if (NewM=nil) then
    Exit;

  Self.SetColor(NewM.Color);

  //Test for equal material after setting color
  //This is because rendersetcolor may have been called so we need reset material.color
  if NewM=OldM then
    Exit;

  NilOld := OldM=nil;

  CurrentMaterial := NewM;

  if Self.Kind=glbFixed then
  begin
    glMaterialfv(GL_FRONT, GL_SPECULAR, @NewM.SpecularColor);
    glMaterialfv(GL_FRONT, GL_EMISSION, @NewM.EmissionColor);
    glMaterialf(GL_FRONT, GL_SHININESS, NewM.Shininess);

    if NilOld or (NewM.Shading<>OldM.Shading) then
    begin
      if (not NilOld) and (OldM.Shading=msWireframe) then
      begin
        glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
        glLineWidth(1.0);
      end;
      case NewM.Shading of
        msSmooth :
          glShadeModel(GL_SMOOTH);
        msFlat :
          glShadeModel(GL_FLAT);
        msWireframe :
          //Wireframe
          glPolygonMode(GL_FRONT_AND_BACK,GL_LINE);
      end;
    end;

    if NilOld or (NewM.WireframeWidth<>OldM.WireFrameWidth) then
      glLineWidth(NewM.WireframeWidth);

    if NilOld or (NewM.Light<>OldM.Light) then
    begin
      if NewM.Light then
        glEnable( GL_LIGHTING )
      else
        glDisable( GL_LIGHTING );
    end;
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
        mb1MSA_A : glBlendFunc(GL_ONE_MINUS_SRC_ALPHA, GL_SRC_ALPHA);
      end;
    end;
  end;

  TexCount := NewM.Textures.Count;
  if (not NilOld) and (OldM.Textures.Count>TexCount) then
    TexCount := OldM.Textures.Count;
  //Count backwards so that activetexture is zero on loop exit
  for I := TexCount-1 downto 0 do
  begin
    if MultiTextureSupported then
      glActiveTexture($84C0 + I)
    else if I>0 then
      Continue;

    if I<NewM.Textures.Count then
      Tex := TMaterialTexture(NewM.Textures[I])
    else
      Tex := DefaultMaterialTexture;

    if Tex.Texture<>nil then
    begin
      glEnable(GL_TEXTURE_2D);
      Tex.Texture.UseTextureBegin;
    end else if Tex.RenderTarget<>nil then
    begin
      glEnable(GL_TEXTURE_2D);
      Tex.RenderTarget.UseTextureBegin;
    end else
    begin
      glDisable(GL_TEXTURE_2D);
    end;

    //Texture matrix
    //Denna ordning är nödvändig för att scale och rotate ska ske kring texture center (0.5)
    Self.MatrixMode(GL_TEXTURE);
    Self.LoadIdentity();
      Self.Translate(Tex.TextureX+Tex.Origin[0],Tex.TextureY+Tex.Origin[1],0);
      Self.Scale(Tex.TextureScale[0],Tex.TextureScale[1],1);
      Self.Rotate(Tex.TextureRotate*360,0,0,1);
      Self.Translate(-Tex.Origin[0],-Tex.Origin[1],0);
    Self.MatrixMode(GL_MODELVIEW);

    if Self.Kind=glbFixed then
    begin
      if Tex.TexCoords=tcGenerated then
      begin
        {$ifndef Android}
        glEnable(GL_TEXTURE_GEN_S);
        glEnable(GL_TEXTURE_GEN_T);
        glTexGeni(GL_S,GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
        glTexGeni(GL_T,GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
        {$endif}
      end
      else
      begin
        glDisable(GL_TEXTURE_GEN_S);
        glDisable(GL_TEXTURE_GEN_T);
      end;
    end;

    //This is a local parameter for every texture
    Tmp := TexWrapModes[Ord(Tex.TextureWrapMode)];
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, Tmp );
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, Tmp );
  end;

  if ShadersSupported and (NilOld or (NewM.Shader<>OldM.Shader)) then
  begin
    if (not NilOld) and (OldM.Shader<>nil) then
      OldM.Shader.DetachArrayVariables;
    if NewM.Shader<>nil then
      NewM.Shader.UseShader
    else
      glUseProgram(0);
  end;
end;

procedure TGLDriverBase.RenderQuad(Tex : TZBitmap; const x,y,w,h,TexX,TexY : integer; const FlipH,FlipV : boolean);
//This routine is based on a ZGE script by Kjell
var
  x1, y1, x2, y2, s1, t1, s2, t2 : single;
  M : TZMatrix4f;
  Verts : array[0..3] of TZVector2f;
  Texc : array[0..3] of TZVector2f;
begin
  if FlipH then
  begin
    x1 := (0 - w + x);
    s2 := TexX;
    s1 := s2 + w;
  end
  else
  begin
    x1 := (0 - x);
    s1 := TexX;
    s2 := s1 + w;
  end;

  if FlipV then
  begin
    y1 := (0 - y);
    t2 := TexY;
    t1 := t2 + h;
  end
  else
  begin
    y1 := (y - h);
    t1 := TexY;
    t2 := t1 + h;
  end;

  x2 := x1 + w;
  y2 := y1 + h;

  FillChar(M,SizeOf(M),0);
  m[0,0] :=  1 / Tex.PixelWidth; // Texture width
  m[1,1] := -1 / Tex.PixelHeight; // Texture height
  m[2,2] :=  1;
  m[3,1] :=  1;
  m[3,3] :=  1;

  Self.SetMatrix(2, m);

  glPushAttrib(GL_TEXTURE_BIT);
    glEnable(GL_TEXTURE_2D);
    glDisable(GL_TEXTURE_GEN_S);
    glDisable(GL_TEXTURE_GEN_T);
    Tex.UseTextureBegin;
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);

    Verts[0] := Vector2f(X1,Y1); Texc[0] := Vector2f(s1,t2);
    Verts[1] := Vector2f(X2,Y1); Texc[1] := Vector2f(s2,t2);
    Verts[2] := Vector2f(X2,Y2); Texc[2] := Vector2f(s2,t1);
    Verts[3] := Vector2f(X1,Y2); Texc[3] := Vector2f(s1,t1);
    Self.RenderArrays(GL_TRIANGLE_FAN,4,2,@Verts,@Texc,nil);

    glDisable(GL_TEXTURE_2D);
  glPopAttrib();
end;

procedure TGLDriverBase.RenderUnitQuad;
const
  Width = 1;
  Height = 1;
  X = 0;
  Y = 0;
var
  W,H : single;
  Verts : array[0..3] of TZVector2f;
  Texc : array[0..3] of TZVector2f;
begin
  //Y är positivt uppåt
  //Rita i CCW direction för att skapa en front-facing polygon
  //Behövs normals här? Default är 0,0,1.

  //För TexCoords gäller: Y=1 Top, Y=0 Bottom

  W := Width / 2.0;
  H := Height / 2.0;

  //Normal towards camera
  glNormal3f(0,0,1);

  Verts[0] := Vector2f(X-W,Y-H); Texc[0] := Vector2f(0.0, 0.0);
  Verts[1] := Vector2f(X+W,Y-H); Texc[1] := Vector2f(1.0, 0.0);
  Verts[2] := Vector2f(X+W,Y+H); Texc[2] := Vector2f(1.0, 1.0);
  Verts[3] := Vector2f(X-W,Y+H); Texc[3] := Vector2f(0.0, 1.0);
  Self.RenderArrays(GL_TRIANGLE_FAN,4,2,@Verts,@Texc,nil);
end;


{ TGLDriverFixed }

procedure TGLDriverFixed.Perspective(const fovy, aspect, zNear, zFar: GLfloat);
//http://steinsoft.net/index.php?site=Programming/Code%20Snippets/OpenGL/gluperspective
var
   xmin, xmax, ymin, ymax : GLfloat;
begin
  ymax := zNear * Tan(fovy * PI / 360.0);
  ymin := -ymax;
  xmin := ymin * aspect;
  xmax := ymax * aspect;
  glFrustum(xmin, xmax, ymin, ymax, zNear, zFar);
end;


procedure TGLDriverFixed.AfterLinkShader(S: TShader);
begin

end;

procedure TGLDriverFixed.BeforeLinkShader(S: TShader);
begin

end;

procedure TGLDriverFixed.GetMatrix(const Index: integer; Mat: PZMatrix4f);
begin
  glGetFloatv(GL_MODELVIEW_MATRIX + Index, PGLFloat(Mat));
end;

procedure TGLDriverFixed.SetMatrix(const Index: integer; const Mat: TZMatrix4f);
var
  OldMode : integer;
begin
  glGetIntegerv(GL_MATRIX_MODE, @OldMode);
  glMatrixMode(GL_MODELVIEW + Index);
  glLoadMatrixf(@Mat);
  glMatrixMode(OldMode);
end;

procedure TGLDriverFixed.LoadIdentity;
begin
  glLoadIdentity;
end;

procedure TGLDriverFixed.MatrixMode(const mode: GLenum);
begin
  glMatrixMode(mode);
end;

procedure TGLDriverFixed.Ortho(const left, right, bottom, top, zNear,
  zFar: GLfloat);
begin
  glOrtho(left,right,bottom,top,znear,zfar);
end;

procedure TGLDriverFixed.PopMatrix;
begin
  glPopMatrix;
end;

procedure TGLDriverFixed.PushMatrix;
begin
  glPushMatrix;
end;

procedure TGLDriverFixed.RenderArrays(const Mode: GLenum; const Count,
  VertElements: integer; const Verts, Texc, Cols: pointer);
begin
  glEnableClientState(GL_VERTEX_ARRAY);
  glEnableClientState(GL_TEXTURE_COORD_ARRAY);
  glVertexPointer(VertElements,GL_FLOAT,0,Verts);
  glTexCoordPointer(2,GL_FLOAT,0,Texc);
  if Cols<>nil then
  begin
    glEnableClientState(GL_COLOR_ARRAY);
    glColorPointer(4,GL_UNSIGNED_BYTE,0,Cols);
  end;
  glDrawArrays(Mode,0,Count);
  glDisableClientState(GL_VERTEX_ARRAY);
  glDisableClientState(GL_TEXTURE_COORD_ARRAY);
  if Cols<>nil then
    glDisableClientState(GL_COLOR_ARRAY);
end;

procedure TGLDriverFixed.RenderMesh(Mesh: TMesh);
begin
  Mesh.BeforeRender;

  if VbosSupported and (not Mesh.IsDynamic) then
  begin
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, Mesh.VboHandles[1]);

    glBindBuffer(GL_ARRAY_BUFFER, Mesh.VboHandles[0]);
    glEnableClientState(GL_NORMAL_ARRAY);
    glNormalPointer(GL_FLOAT,0,pointer(Mesh.VboOffsets[0]));

    if Mesh.Colors<>nil then
    begin
      glEnableClientState(GL_COLOR_ARRAY);
      glColorPointer(4,GL_UNSIGNED_BYTE,0,pointer(Mesh.VboOffsets[1]));
    end;

    if Mesh.TexCoords<>nil then
    begin
      glEnableClientState(GL_TEXTURE_COORD_ARRAY);
      glTexCoordPointer(2,GL_FLOAT,0,pointer(Mesh.VboOffsets[2]));
    end;

    glEnableClientState(GL_VERTEX_ARRAY);
    glVertexPointer(3,GL_FLOAT,0,nil);

    glDrawElements(GL_TRIANGLES,Mesh.IndicesCount,TMeshVertexIndex_GL,nil);
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);
  end else
  begin
    //Use vertex arrays
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
    glDrawElements(GL_TRIANGLES,Mesh.IndicesCount,TMeshVertexIndex_GL,Mesh.Indices);
  end;

  glDisableClientState(GL_VERTEX_ARRAY);
  glDisableClientState(GL_NORMAL_ARRAY);
  glDisableClientState(GL_TEXTURE_COORD_ARRAY);
  glDisableClientState(GL_COLOR_ARRAY);

  {$ifndef minimal}
  //Display normals for debugging
  if NormalsVisible then
    RenderNormals(Mesh);
  {$endif}
end;

procedure TGLDriverFixed.Rotate(const Angle, X, Y, Z: GLfloat);
begin
  glRotatef(Angle,X,Y,Z);
end;

procedure TGLDriverFixed.Scale(const X, Y, Z: GLfloat);
begin
  glScalef(X,Y,Z);
end;

procedure TGLDriverFixed.SetColor(const C: TZColorf);
begin
  glColor4fv(@C);
end;

procedure TGLDriverFixed.SetCurrentShader(Shader: TShader);
begin

end;

procedure TGLDriverFixed.Translate(const X, Y, Z: GLfloat);
begin
  glTranslatef(X,Y,Z);
end;


procedure TGLDriverFixed.UpdateNormalMatrix;
begin

end;

{ TGLDriverProgrammable }

constructor TGLDriverProgrammable.Create;
var
  I : integer;
begin
  for I := 0 to High(MPtrs) do
    MPtrs[I] := @MStack[I,0];
end;

procedure TGLDriverProgrammable.GetMatrix(const Index: integer; Mat: PZMatrix4f);
begin
  Mat^ := MPtrs[Index]^;
end;

procedure TGLDriverProgrammable.SetMatrix(const Index: integer;
  const Mat: TZMatrix4f);
begin
  MPtrs[Index]^ := Mat;
  MDirty[Index] := True;
end;

procedure TGLDriverProgrammable.Perspective(const fovy, aspect, zNear, zFar: GLfloat);
var
  xymax,ymin,xmin,width,height,depth,q,qn,w,h : single;
  M  : TZMatrix4f;
begin
  //http://www.geeks3d.com/20090729/howto-perspective-projection-matrix-in-opengl/
  xymax := znear * tan(fovy * PI / 360.0);
  ymin := -xymax;
  xmin := -xymax;

  width := xymax - xmin;
  height := xymax - ymin;

  depth := zfar - znear;
  q := -(zfar + znear) / depth;
  qn := -2 * (zfar * znear) / depth;

  w := 2 * znear / width;
  w := w / aspect;
  h := 2 * znear / height;

  M[0] := Vector4f(w,0,0,0);
  M[1] := Vector4f(0,h,0,0);
  M[2] := Vector4f(0,0,q,-1);
  M[3] := Vector4f(0,0,qn,0);
  Self.MPtrs[ Self.MMode ]^ := M;
  MDirty[Self.MMode] := True;
end;

procedure TGLDriverProgrammable.Ortho(const left, right, bottom, top, zNear,
  zFar: GLfloat);
var
  M : TZMatrix4f;
  p_fn,m_nf,p_rl,m_rl,p_tb,m_tb,m_lr,m_bt : single;
begin
  //http://forums.4fips.com/viewtopic.php?f=3&t=1059

  p_fn := zfar + znear;
  m_nf := znear - zfar; // ~ -m_fn

  p_rl := right + left;
  m_rl := right - left;
  p_tb := top + bottom;
  m_tb := top - bottom;

  m_lr := -m_rl;
  m_bt := -m_tb;

  M[0] := Vector4f(2/m_rl,       0,          0,         p_rl/m_lr);
  M[1] := Vector4f(0,            2/m_tb,     0,         p_tb/m_bt);
  M[2] := Vector4f(0,            0,          2/m_nf,    0);
  M[3] := Vector4f(0,            0,          p_fn/m_nf, 1);

  Self.MPtrs[ Self.MMode ]^ := M;
  MDirty[Self.MMode] := True;
end;


procedure TGLDriverProgrammable.LoadIdentity;
begin
  Self.MPtrs[ Self.MMode ]^ := IdentityHmgMatrix;
  MDirty[Self.MMode] := True;
end;

procedure TGLDriverProgrammable.MatrixMode(const mode: GLenum);
begin
  Self.MMode := Mode-GL_MODELVIEW;
end;

procedure TGLDriverProgrammable.PopMatrix;
begin
  Dec(Self.MPtrs[ Self.MMode ]);
  MDirty[Self.MMode] := True;
end;

procedure TGLDriverProgrammable.PushMatrix;
var
  P : PZMatrix4f;
begin
  P := Self.MPtrs[ Self.MMode ];
  Inc(Self.MPtrs[ Self.MMode ]);
  Self.MPtrs[ Self.MMode ]^ := P^;
  MDirty[Self.MMode] := True;
end;

procedure TGLDriverProgrammable.Rotate(const Angle, X, Y, Z: GLfloat);
var
  P : PZMatrix4f;
  M : TZMatrix4f;
  A :  single;
begin
  if Angle=0 then
    Exit;
  P := Self.MPtrs[ Self.MMode ];
  A := Angle * PI/180;
  if X<>0 then
  begin
    CreateRotationMatrixX( A , M);
    P^ := MatrixMultiply(M,P^);
  end;
  if Y<>0 then
  begin
    CreateRotationMatrixY( A , M);
    P^ := MatrixMultiply(M,P^);
  end;
  if Z<>0 then
  begin
    CreateRotationMatrixZ( A, M);
    P^ := MatrixMultiply(M,P^);
  end;
  MDirty[Self.MMode] := True;
end;

procedure TGLDriverProgrammable.Scale(const X, Y, Z: GLfloat);
var
  P : PZMatrix4f;
  M : TZMatrix4f;
begin
  P := Self.MPtrs[ Self.MMode ];
  CreateScaleAndTranslationMatrix(Vector3f(X,Y,Z), Vector3f(0,0,0), M);
  P^ := MatrixMultiply(M,P^);
  MDirty[Self.MMode] := True;
end;

procedure TGLDriverProgrammable.SetColor(const C: TZColorf);
begin
  Self.GlobalColor := C;
end;

procedure TGLDriverProgrammable.SetCurrentShader(Shader: TShader);
begin
  Self.CurrentShader := Shader;
  //Must update matrices directly on use to work with scripted OpenGL calls
  //Possibly add a Shader.MatricesUpdatedTime to avoid redundant updates
  UpdateUniforms;
end;

procedure TGLDriverProgrammable.Translate(const X, Y, Z: GLfloat);
var
  P : PZMatrix4f;
  M : TZMatrix4f;
begin
  P := Self.MPtrs[ Self.MMode ];
  CreateScaleAndTranslationMatrix(UNIT_XYZ3 , Vector3f(X,Y,Z), M);
  P^ := MatrixMultiply(M,P^);
  MDirty[Self.MMode] := True;
end;

procedure TGLDriverProgrammable.UpdateNormalMatrix;
var
  P : PZMatrix4f;
  I,J : integer;
begin
  P := Self.MPtrs[ 0 ];
  for I := 0 to 2 do
    for J := 0 to 2 do
      Self.NormalM[I,J] := P^[I,J];
end;

procedure TGLDriverProgrammable.UpdateUniforms;
begin
  {$ifndef minimal}
  ZAssert(Self.CurrentShader<>nil, 'No shader set before render');
  {$endif}

  if (CurrentShader.MvpLoc>-1) then
  begin
    if (Self.MDirty[0] or Self.MDirty[1]) then
    begin
      MVP := MatrixMultiply(Self.MPtrs[ 0 ]^, Self.MPtrs[ 1 ]^);
      Self.MDirty[0]:=False;
      Self.MDirty[1]:=False;
    end;
    glUniformMatrix4fv(CurrentShader.MvpLoc,1,GL_FALSE,@Self.MVP);
  end;

  if CurrentShader.MvLoc>-1 then
    glUniformMatrix4fv(CurrentShader.MvLoc,1,GL_FALSE,PGLfloat(Self.MPtrs[ 0 ]));

  if CurrentShader.ProjLoc>-1 then
    glUniformMatrix4fv(CurrentShader.ProjLoc,1,GL_FALSE,PGLfloat(Self.MPtrs[ 1 ]));

  if CurrentShader.TexMatLoc>-1 then
    glUniformMatrix4fv(CurrentShader.TexMatLoc,1,GL_FALSE,PGLfloat(Self.MPtrs[ 2 ]));

  if CurrentShader.NormMatLoc>-1 then
    glUniformMatrix3fv(CurrentShader.NormMatLoc,1,GL_FALSE,PGLfloat(@Self.NormalM));

  if CurrentShader.GlobColLoc>-1 then
    glUniform4fv(CurrentShader.GlobColLoc,1,PGLfloat(@Self.GlobalColor));
end;

procedure TGLDriverProgrammable.BeforeLinkShader(S: TShader);
begin
  glBindAttribLocation(S.ProgHandle, 0, 'position');
  glBindAttribLocation(S.ProgHandle, 1, 'normal');
  glBindAttribLocation(S.ProgHandle, 2, 'color');
  glBindAttribLocation(S.ProgHandle, 3, 'texCoord');
end;

procedure TGLDriverProgrammable.AfterLinkShader(S: TShader);
begin
  S.MvpLoc := glGetUniformLocation(S.ProgHandle, PAnsiChar('modelViewProjectionMatrix'));
  S.MvLoc := glGetUniformLocation(S.ProgHandle, PAnsiChar('modelViewMatrix'));
  S.ProjLoc := glGetUniformLocation(S.ProgHandle, PAnsiChar('projectionMatrix'));
  S.TexMatLoc := glGetUniformLocation(S.ProgHandle, PAnsiChar('textureMatrix'));
  S.NormMatLoc := glGetUniformLocation(S.ProgHandle, PAnsiChar('normalMatrix'));
  S.GlobColLoc := glGetUniformLocation(S.ProgHandle, PAnsiChar('globalColor'));
end;

procedure TGLDriverProgrammable.RenderArrays(const Mode: GLenum; const Count,
  VertElements: integer; const Verts, Texc, Cols: pointer);
begin
  UpdateUniforms;

  //Vertex
  glEnableVertexAttribArray(0);
  glVertexAttribPointer(0, VertElements, GL_FLOAT, GL_FALSE, 0, Verts);

  //TODO: Normals
//  glEnableVertexAttribArray(1);
//  glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, 0, pointer(Mesh.VboOffsets[0]));

  //Colors
  if Cols<>nil then
  begin
    glEnableVertexAttribArray(2);
    glVertexAttribPointer(2, 4, GL_UNSIGNED_BYTE, GL_TRUE, 0, Cols);
  end;

  //Texcoords
  if Texc<>nil then
  begin
    glEnableVertexAttribArray(3);
    glVertexAttribPointer(3, 2, GL_FLOAT, GL_FALSE, 0, Texc);
  end;

  glDrawArrays(Mode,0,Count);

  glDisableVertexAttribArray(0);
  glDisableVertexAttribArray(1);
  glDisableVertexAttribArray(2);
  glDisableVertexAttribArray(3);
end;

procedure TGLDriverProgrammable.RenderMesh(Mesh: TMesh);
begin
  Mesh.BeforeRender;
  UpdateUniforms;

  glBindBuffer(GL_ARRAY_BUFFER, Mesh.VboHandles[0]);

  //Vertex
  glEnableVertexAttribArray(0);
  glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, nil);

  //Normals
  glEnableVertexAttribArray(1);
  glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, 0, pointer(Mesh.VboOffsets[0]));

  //Colors
  if Mesh.Colors<>nil then
  begin
    glEnableVertexAttribArray(2);
    glVertexAttribPointer(2, 4, GL_UNSIGNED_BYTE, GL_TRUE, 0, pointer(Mesh.VboOffsets[1]));
  end;

  //Texcoords
  if Mesh.TexCoords<>nil then
  begin
    glEnableVertexAttribArray(3);
    glVertexAttribPointer(3, 2, GL_FLOAT, GL_FALSE, 0, pointer(Mesh.VboOffsets[2]));
  end;

  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, Mesh.VboHandles[1]);
  glDrawElements(GL_TRIANGLES,Mesh.IndicesCount,TMeshVertexIndex_GL,nil);
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, 0);

  glDisableVertexAttribArray(0);
  glDisableVertexAttribArray(1);
  glDisableVertexAttribArray(2);
  glDisableVertexAttribArray(3);

  {$ifndef minimal}
  //Display normals for debugging
  if NormalsVisible then
    RenderNormals(Mesh);
  {$endif}
end;

function CreateDriver(Kind : TGLBase) : TGLDriverBase;
begin
  case Kind of
    glbProgrammable : Result := TGLDriverProgrammable.Create;
  else
    Result := TGLDriverFixed.Create
  end;
  Result.Kind := Kind;
end;


end.
