unit GLDrivers;

interface

uses ZOpenGL, Meshes, ZClasses;

type
  TGLDriverBase = class
  public
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
    procedure SetCurrentShader(Shader : pointer); virtual; abstract;
    procedure OnCompileShader(Shader : pointer); virtual; abstract;
  end;

function CreateDriver(Kind : integer) : TGLDriverBase;

implementation

uses ZMath, Renderer
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
    procedure SetCurrentShader(Shader : pointer); override;
    procedure OnCompileShader(Shader : pointer); override;
    procedure Perspective(const fovy,aspect,zNear,zFar : GLfloat); override;
  end;

  TGLDriverProgrammable = class(TGLDriverBase)
  strict private
    MStack : array[0..2, 0..15] of TZMatrix4f;
    MPtrs : array[0..2] of PZMatrix4f;
    MDirty : array[0..2] of boolean;
    MMode : integer;
    CurrentShader : TShader;
    MVP : TZMatrix4f;
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
    procedure SetCurrentShader(Shader : pointer); override;
    procedure OnCompileShader(Shader : pointer); override;
    procedure Perspective(const fovy,aspect,zNear,zFar : GLfloat); override;
    procedure Ortho(const left, right, bottom, top, zNear, zFar: GLfloat); override;
    procedure RenderArrays(const Mode: GLenum; const Count,VertElements : integer; const Verts,Texc,Cols : pointer); override;
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

procedure TGLDriverFixed.LoadIdentity;
begin
  glLoadIdentity;
end;

procedure TGLDriverFixed.MatrixMode(const mode: GLenum);
begin
  glMatrixMode(mode);
end;

procedure TGLDriverFixed.OnCompileShader(Shader: pointer);
begin

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
    glBindBufferARB(GL_ELEMENT_ARRAY_BUFFER_ARB, Mesh.VboHandles[1]);

    glBindBufferARB(GL_ARRAY_BUFFER_ARB, Mesh.VboHandles[0]);
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
    glBindBufferARB(GL_ARRAY_BUFFER_ARB, 0);
    glBindBufferARB(GL_ELEMENT_ARRAY_BUFFER_ARB, 0);
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

procedure TGLDriverFixed.SetCurrentShader(Shader: pointer);
begin

end;

procedure TGLDriverFixed.Translate(const X, Y, Z: GLfloat);
begin
  glTranslatef(X,Y,Z);
end;


{ TGLDriverProgrammable }

constructor TGLDriverProgrammable.Create;
var
  I : integer;
begin
  for I := 0 to High(MPtrs) do
    MPtrs[I] := @MStack[I,0];
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
end;

procedure TGLDriverProgrammable.Ortho(const left, right, bottom, top, zNear,
  zFar: GLfloat);
var
  M : TZMatrix4f;
  P : PZMatrix4f;
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

  M[0] := Vector4f(2/m_rl,       0,          0,       p_rl/m_lr);
  M[1] := Vector4f(0,            2/m_tb,     0,       p_tb/m_bt);
  M[2] := Vector4f(0,            0,          2/m_nf,  p_fn/m_nf);
  M[3] := Vector4f(0,            0,          0,       1);

  P := Self.MPtrs[ Self.MMode ];
  P^ := MatrixMultiply(P^, M);
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

procedure TGLDriverProgrammable.SetCurrentShader(Shader: pointer);
begin
  Self.CurrentShader := Shader;
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

procedure TGLDriverProgrammable.UpdateUniforms;
begin
  ZAssert(Self.CurrentShader<>nil, 'No shader set before render');

  if Self.MDirty[0] or Self.MDirty[1] then
  begin
//    MVP := MatrixMultiply(Self.MPtrs[ 1 ]^, Self.MPtrs[ 0 ]^);
//    MVP := MatrixMultiply(IdentityHmgMatrix, Self.MPtrs[ 0 ]^);
//    MVP := IdentityHmgMatrix;
    MVP := MatrixMultiply(Self.MPtrs[ 0 ]^, Self.MPtrs[ 1 ]^);
    MVP := Self.MPtrs[ 1 ]^;

    Self.MDirty[0]:=False;
    Self.MDirty[1]:=False;
  end;

//  glUniformMatrix4fv(CurrentShader.MvpLoc,1,GL_FALSE,@Self.MVP);
  glUniformMatrix4fv(CurrentShader.MvLoc,1,GL_FALSE,Pointer(Self.MPtrs[ 0 ]));
  glUniformMatrix4fv(CurrentShader.PrLoc,1,GL_FALSE,Pointer(Self.MPtrs[ 1 ]));
end;

procedure TGLDriverProgrammable.OnCompileShader(Shader: pointer);
var
  S : TShader;
begin
  S := TShader(Shader);
  S.MvLoc := glGetUniformLocation(S.ProgHandle, PAnsiChar('modelView'));
  S.PrLoc := glGetUniformLocation(S.ProgHandle, PAnsiChar('projection'));
  glBindAttribLocation(S.ProgHandle, 0, 'position');
  glBindAttribLocation(S.ProgHandle, 1, 'normal');
  glBindAttribLocation(S.ProgHandle, 2, 'color');
  glBindAttribLocation(S.ProgHandle, 3, 'texCoord');
end;

procedure TGLDriverProgrammable.RenderArrays(const Mode: GLenum; const Count,
  VertElements: integer; const Verts, Texc, Cols: pointer);
begin
  UpdateUniforms;

  //Vertex
  glEnableVertexAttribArray(0);
  glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, Verts);

  //TODO: Normals
//  glEnableVertexAttribArray(1);
//  glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, 0, pointer(Mesh.VboOffsets[0]));

  //Colors
  if Cols<>nil then
  begin
    glEnableVertexAttribArray(2);
    glVertexAttribPointer(2, 4, GL_UNSIGNED_BYTE, GL_FALSE, 0, Cols);
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

  glBindBufferARB(GL_ARRAY_BUFFER_ARB, Mesh.VboHandles[0]);

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
//    glVertexAttribIPointer(2, 4, GL_UNSIGNED_BYTE, 0, pointer(Mesh.VboOffsets[1]));
    glVertexAttribPointer(2, 4, GL_UNSIGNED_BYTE, GL_TRUE, 0, pointer(Mesh.VboOffsets[1]));
  end;

  //Texcoords
  if Mesh.TexCoords<>nil then
  begin
    glEnableVertexAttribArray(3);
    glVertexAttribPointer(3, 2, GL_FLOAT, GL_FALSE, 0, pointer(Mesh.VboOffsets[2]));
  end;

  glBindBufferARB(GL_ELEMENT_ARRAY_BUFFER_ARB, Mesh.VboHandles[1]);
  glDrawElements(GL_TRIANGLES,Mesh.IndicesCount,TMeshVertexIndex_GL,nil);
  glBindBufferARB(GL_ARRAY_BUFFER_ARB, 0);
  glBindBufferARB(GL_ELEMENT_ARRAY_BUFFER_ARB, 0);

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

function CreateDriver(Kind : integer) : TGLDriverBase;
begin
//  Result := TGLDriverFixed.Create;
  Result := TGLDriverProgrammable.Create;
end;


end.
