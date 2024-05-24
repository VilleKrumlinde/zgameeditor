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

//Import OBJ-files

unit uObjFile;

interface

uses ZClasses, Classes, Generics.Collections, Meshes;

{$IFDEF ZGEVIZ}
  {$DEFINE HugeMeshes}
{$ENDIF}

type
  TObjFace = record
    Index : array[0..2] of integer;
  end;

  TObjMaterial = class
    Diffuse : TZVector3f;
  end;

  TObjImport = class
  strict private
    Lines : TStringList;
    Verts,Normals,Colors,TexCoords : TList<TZVector3f>;
    Faces : TList<TObjFace>;
    FileName : string;
    Materials : TObjectDictionary<string,TObjMaterial>;
    HasVertexColors : boolean;
    procedure UpdateMeshImp(MeshImp: TMeshImport);
    function GenerateMesh : TZComponent;
    procedure AddMaterialLib(const MatFileName : string);
    procedure ScaleAndCenter;
  public
    AutoScale,AutoCenter,IncludeVertexColors,IncludeTextureCoords : boolean;
    MeshImpToUpdate : TMeshImport;
    ResultMesh : TMesh;
    constructor Create(const FileName : string);
    destructor Destroy; override;
    procedure Import;
  end;

implementation

uses SysUtils, ZMath, ZLog;

{ TObjImport }

constructor TObjImport.Create(const FileName: string);
begin
  Lines := TStringList.Create;
  Lines.LoadFromFile(FileName);
  Verts := TList<TZVector3f>.Create;
  Colors := TList<TZVector3f>.Create;
  Normals := TList<TZVector3f>.Create;
  TexCoords := TList<TZVector3f>.Create;
  Faces := TList<TObjFace>.Create;
  Materials := TObjectDictionary<string,TObjMaterial>.Create([doOwnsValues]);
  Self.FileName := FileName;
  Self.IncludeVertexColors := True;
  Self.IncludeTextureCoords := True;
end;

destructor TObjImport.Destroy;
begin
  Lines.Free;
  Verts.Free;
  Colors.Free;
  Normals.Free;
  TexCoords.Free;
  Faces.Free;
  Materials.Free;
  inherited;
end;

procedure TObjImport.ScaleAndCenter;
var
  MinV,MaxV,DV,ScaleV,TransV,V : TZVector3f;
  I : integer;
  Mat : TZMatrix4f;
  Scale : single;
begin
  MinV := Vector3f(10000,10000,10000);
  MaxV := Vector3f(-10000,-10000,-10000);

  for I := 0 to Self.Verts.Count-1 do
  begin
    MinV[0] := Min(MinV[0],Self.Verts[I][0]);
    MinV[1] := Min(MinV[1],Self.Verts[I][1]);
    MinV[2] := Min(MinV[2],Self.Verts[I][2]);
    MaxV[0] := Max(MaxV[0],Self.Verts[I][0]);
    MaxV[1] := Max(MaxV[1],Self.Verts[I][1]);
    MaxV[2] := Max(MaxV[2],Self.Verts[I][2]);
  end;
  VecSub3(MaxV,MinV,DV);

  if AutoScale then
  begin
    Scale := 1 / DV[0];
    ScaleV := Vector3f(Scale,Scale,Scale);
  end
  else
    ScaleV := Vector3f(1,1,1);

  if AutoCenter then
  begin
    VecScalarMult3(MinV,-1,V);
    VecSub3(V, VecScalarMult3(DV,0.5) ,TransV);
    VecMult3(TransV,ScaleV);
  end
  else
    TransV := Vector3f(0,0,0);

  CreateScaleAndTranslationMatrix(ScaleV, TransV, Mat);

  for I := 0 to Self.Verts.Count-1 do
  begin
    VectorTransform(Self.Verts[I],Mat,V);
    Self.Verts[I] := V;
  end;
end;

procedure TObjImport.AddMaterialLib(const MatFileName : string);
var
  S : string;
  L,L2 : TStringList;
  Mat : TObjMaterial;
  I : integer;
begin
  S := ExtractFilePath(Self.FileName) + MatFileName;
  if not FileExists(S) then
    Exit;
  L := TStringList.Create;
  L.LoadFromFile(S);

  L2 := TStringList.Create;
  L2.Delimiter := ' ';
  Mat := nil;
  for S in L do
  begin
    if Trim(S)='' then
      Continue;
    L2.DelimitedText := Trim(S);
    if L2[0]='newmtl' then
    begin
      Mat := TObjMaterial.Create;
      Self.Materials.Add(L2[1], Mat);
    end
    else if L2[0]='Kd' then
    begin
      for I := 0 to 2 do
        Mat.Diffuse[I] := StrToFloat(L2[I+1]);
    end;
  end;

  L.Free;
  L2.Free;
end;

procedure TObjImport.Import;
var
  S : string;
  L,L2 : TStringList;
  Face : TObjFace;
  I,J : integer;
  Mat : TObjMaterial;

  function FaceIndex(I : integer) : integer;
  begin
    if I<0 then
      //Negative index means relative postion of vertex
      Exit(Self.Verts.Count+I)
    else
      Exit(I-1);
  end;

var
  TexCoordReMapping : TList<TZVector3f>;
begin
  L := TStringList.Create;
  L.Delimiter := ' ';

  L2 := TStringList.Create;
  L2.Delimiter := '/';

  TexCoordReMapping := TList<TZVector3f>.Create;

  Mat := nil;
  for S in Lines do
  begin
    if Trim(S)='' then
      Continue;
    L.DelimitedText := S;

    if L[0][1]='#' then
      Continue
    else if L[0]='v' then
    begin
      Verts.Add(  Vector3f( StrToFloatDef(L[1],0),StrToFloatDef(L[2],0),StrToFloatDef(L[3],0)  )  );
      if L.Count >= 7 then
      begin
        // vertex RGB color specified
        Colors.Add( Vector3f( StrToFloatDef(L[4],0),StrToFloatDef(L[5],0),StrToFloatDef(L[6],0)  ) );
        HasVertexColors := True;
      end
      else
        Colors.Add( Vector3f(0.8,0.8,0.8) );
    end
    else if L[0]='vn' then
    begin
      Normals.Add(  Vector3f( StrToFloatDef(L[1],0),StrToFloatDef(L[2],0),StrToFloatDef(L[3],0)  )  )
    end
    else if L[0]='vt' then
    begin
      if L.Count>=3 then
        TexCoords.Add(  Vector3f( StrToFloatDef(L[1],0),StrToFloatDef(L[2],0),0  )  );
    end
    else if L[0]='f' then
    begin
      if L.Count in [4,5] then
      begin
        for I := 0 to 2 do
        begin
          L2.DelimitedText := L[1+I];
          Face.Index[I] := FaceIndex(StrToInt(L2[0]));
          if L2.Count > 1 then
          begin
            // When faces specify texcoord index then use this for vert -> texcoord mapping
            if TexCoordReMapping.Count=0 then
              TexCoordReMapping.AddRange(TexCoords);
            TexCoordReMapping[ Face.Index[I] ] := TexCoords[ StrToInt(L2[1]) - 1 ];
          end;
          if Assigned(Mat) then
          begin
            Colors[ Face.Index[I] ] := Mat.Diffuse;
            Self.HasVertexColors := True;
          end;
        end;
        Self.Faces.Add(Face);
        if L.Count=5 then
        begin  //Split quad into two tris
          //3 4 1
          J := Face.Index[2];
          Face.Index[2] := Face.Index[0];
          Face.Index[0] := J;

          L2.DelimitedText := L[1+3];
          Face.Index[1] := FaceIndex(StrToInt(L2[0]));
          if Assigned(Mat) then
          begin
            Colors[ Face.Index[1] ] := Mat.Diffuse;
            Self.HasVertexColors := True;
          end;
          Self.Faces.Add(Face);
        end;
      end else
        raise Exception.Create('OBJ-reader: Only triangle surfaces supported');
    end
    else if L[0]='mtllib' then
    begin
      AddMaterialLib(L[1]);
    end
    else if L[0]='usemtl' then
    begin
      Self.Materials.TryGetValue(L[1],Mat);
    end;

  end;
  L.Free;
  L2.Free;

  if TexCoordReMapping.Count>0 then
  begin
    Self.TexCoords.Clear;
    Self.TexCoords.AddRange(TexCoordReMapping);
  end;
  TexCoordReMapping.Free;

  if AutoScale or AutoCenter then
    ScaleAndCenter;

  if Assigned(Self.MeshImpToUpdate) then
    UpdateMeshImp(Self.MeshImpToUpdate)
  else
    Self.ResultMesh := GenerateMesh as TMesh;
end;

procedure TObjImport.UpdateMeshImp(MeshImp: TMeshImport);
//Write to MeshImp from 3dsMesh
var
  I,Color : integer;
  Stream : TMemoryStream;
  MinV,MaxV,DiffV,V : TZVector3f;
  W : word;
  {$IFDEF HugeMeshes}
  J : integer;
  {$ELSE}
  Sm : smallint;
  {$ENDIF}
begin
  ZLog.GetLog(Self.ClassName).Write('Obj-file vertcount: ' + IntToStr(Self.Verts.Count) );

  Stream := TMemoryStream.Create;
  try
    Stream.Write(Self.Verts.Count,4);
    Stream.Write(Self.Faces.Count,4);

    //Quantize vertices to 16-bit values
    MinV := Vector3f(100000,100000,100000);
    MaxV := Vector3f(0,0,0);
    for I := 0 to Self.Verts.Count - 1 do
    begin
      MinV[0] := Min(MinV[0],Self.Verts[I][0]);
      MinV[1] := Min(MinV[1],Self.Verts[I][1]);
      MinV[2] := Min(MinV[2],Self.Verts[I][2]);
      MaxV[0] := Max(MaxV[0],Self.Verts[I][0]);
      MaxV[1] := Max(MaxV[1],Self.Verts[I][1]);
      MaxV[2] := Max(MaxV[2],Self.Verts[I][2]);
    end;

    Stream.Write(MinV,3*4);
    ZMath.VecSub3(MaxV,MinV,DiffV);
    Stream.Write(DiffV,3*4);

    for I := 0 to Self.Verts.Count - 1 do
    begin
      W := Round( (Self.Verts[I][0] - MinV[0]) / DiffV[0] * High(Word) );
      Stream.Write(W,2);
      W := Round( (Self.Verts[I][1] - MinV[1]) / DiffV[1] * High(Word) );
      Stream.Write(W,2);
      W := Round( (Self.Verts[I][2] - MinV[2]) / DiffV[2] * High(Word) );
      Stream.Write(W,2);
    end;

    {$IFDEF HugeMeshes}
    MeshImp.AreIndicesUncompressed := True;
    for I := 0 to Self.Faces.Count - 1 do
    begin
      J := Self.Faces[I].Index[0];
      Stream.Write(J,4);
      J := Self.Faces[I].Index[1];
      Stream.Write(J,4);
      J := Self.Faces[I].Index[2];
      Stream.Write(J,4);
    end;
    {$ELSE}
    //Delta-encode indices
    Sm := Self.Faces[0].Index[0];
    Stream.Write(Sm,2);
    Sm := Self.Faces[0].Index[1];
    Stream.Write(Sm,2);
    Sm := Self.Faces[0].Index[2];
    Stream.Write(Sm,2);
    for I := 1 to Self.Faces.Count - 1 do
    begin
      Sm := Self.Faces[I].Index[0] - Self.Faces[I-1].Index[0];
      Stream.Write(Sm,2);
      Sm := Self.Faces[I].Index[1] - Self.Faces[I-1].Index[1];
      Stream.Write(Sm,2);
      Sm := Self.Faces[I].Index[2] - Self.Faces[I-1].Index[2];
      Stream.Write(Sm,2);
    end;
    {$ENDIF}

    if Self.IncludeVertexColors and Self.HasVertexColors then
    begin
      MeshImp.HasVertexColors := True;
      for I := 0 to Self.Colors.Count - 1 do
      begin
        Color :=
          ($ff shl 24) or
          (Round(Self.Colors[I][2] * 255) shl 16) or
          (Round(Self.Colors[I][1] * 255) shl 8) or
          Round(Self.Colors[I][0] * 255);
        Stream.Write(Color,4);
      end;
    end else
      MeshImp.HasVertexColors := False;

    if Self.IncludeTextureCoords and (Self.TexCoords.Count>=Self.Verts.Count) then
    begin
      MeshImp.HasTextureCoords := True;
      MeshImp.AreTexCoordsUncompressed := True;
      for I := 0 to Self.Verts.Count-1 do
      begin
        V := texcoords[i];
        Stream.Write(v, 8);
      end;
    end else
      MeshImp.HasTextureCoords := False;

    //Write data to binary property
    if MeshImp.MeshData.Data<>nil then
      FreeMem(MeshImp.MeshData.Data,MeshImp.MeshData.Size);
    GetMem(MeshImp.MeshData.Data,Stream.Size);
    MeshImp.MeshData.Size := Stream.Size;
    Stream.Position := 0;
    Stream.Read(MeshImp.MeshData.Data^,Stream.Size)
  finally
    Stream.Free;
  end;
end;

function TObjImport.GenerateMesh : TZComponent;
var
  OutMesh : TMesh;
  MeshImp : TMeshImport;
begin
  OutMesh := TMesh.Create(nil);
  OutMesh.SetString('Comment','Imported from ' + AnsiString(ExtractFileName(Self.FileName)));

  MeshImp := TMeshImport.Create(OutMesh.Producers);
  UpdateMeshImp(MeshImp);

  Result := OutMesh;
end;

end.
