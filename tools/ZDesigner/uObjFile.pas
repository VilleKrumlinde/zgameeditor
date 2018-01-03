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
    Verts,Normals,Colors : TList<TZVector3f>;
    Faces : TList<TObjFace>;
    FileName : string;
    Materials : TObjectDictionary<string,TObjMaterial>;
    procedure UpdateMeshImp(MeshImp: TMeshImport);
    function GenerateMesh : TZComponent;
    procedure AddMaterialLib(const MatFileName : string);
  public
    IncludeVertexColors : boolean;
    MeshImpToUpdate : TMeshImport;
    ResultMesh : TMesh;
    constructor Create(const FileName : string);
    destructor Destroy; override;
    procedure Import;
  end;

implementation

uses SysUtils, ZMath;

{ TObjImport }

constructor TObjImport.Create(const FileName: string);
begin
  Lines := TStringList.Create;
  Lines.LoadFromFile(FileName);
  Verts := TList<TZVector3f>.Create;
  Colors := TList<TZVector3f>.Create;
  Normals := TList<TZVector3f>.Create;
  Faces := TList<TObjFace>.Create;
  Materials := TObjectDictionary<string,TObjMaterial>.Create([doOwnsValues]);
  Self.FileName := FileName;
  Self.IncludeVertexColors := True;
end;

destructor TObjImport.Destroy;
begin
  Lines.Free;
  Verts.Free;
  Colors.Free;
  Normals.Free;
  Faces.Free;
  Materials.Free;
  inherited;
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
  I : integer;
  Mat : TObjMaterial;
begin
  L := TStringList.Create;
  L.Delimiter := ' ';

  L2 := TStringList.Create;
  L2.Delimiter := '/';

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
      Colors.Add( Vector3f(0.8,0.8,0.8) );
    end
    else if L[0]='vn' then
    begin
      Normals.Add(  Vector3f( StrToFloatDef(L[1],0),StrToFloatDef(L[2],0),StrToFloatDef(L[3],0)  )  )
    end
    else if L[0]='f' then
    begin
      if L.Count<>4 then
        raise Exception.Create('OBJ-reader: Only triangle surfaces supported');
      for I := 0 to 2 do
      begin
        L2.DelimitedText := L[1+I];
        Face.Index[I] := StrToInt(L2[0])-1;
        if Assigned(Mat) then
          Colors[ Face.Index[I] ] := Mat.Diffuse;
      end;
      Self.Faces.Add(Face);
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

  if Assigned(Self.MeshImpToUpdate) then
    UpdateMeshImp(Self.MeshImpToUpdate)
  else
    Self.ResultMesh := GenerateMesh as TMesh;
end;

procedure TObjImport.UpdateMeshImp(MeshImp: TMeshImport);
//Write to MeshImp from 3dsMesh
var
  I,Color : integer;
  Stream{,StU,StV} : TMemoryStream;
  MinV,MaxV,DiffV : TZVector3f;
  W : word;
  Sm : smallint;
begin
//  MeshImp.Scale := Vector3f(Self.MeshScale,Self.MeshScale,Self.MeshScale);

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

   if Self.IncludeVertexColors then
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
    end;

{    if DataFile.IncludeTextureCoords and (Length(InMesh.TextureCoords)>0) then
    begin
      MeshImp.HasTextureCoords := True;
      StU := TMemoryStream.Create;
      StV := TMemoryStream.Create;

      //Delta-encode in separate streams
      W := Round( Frac(InMesh.TextureCoords[0][0]) * High(Smallint) );
      StU.Write(W,2);
      W := Round( Frac(InMesh.TextureCoords[0][1]) * High(Smallint) );
      StV.Write(W,2);

      for I := 1 to High(InMesh.TextureCoords) do
      begin
        Sm := Round( (InMesh.TextureCoords[I][0]-InMesh.TextureCoords[I-1][0]) * High(SmallInt) );
        StU.Write(Sm,2);
        Sm := Round( (InMesh.TextureCoords[I][1]-InMesh.TextureCoords[I-1][1]) * High(SmallInt) );
        StV.Write(Sm,2);
      end;
      StU.SaveToStream(Stream);
      StV.SaveToStream(Stream);
      StU.Free;
      StV.Free;
      //Stream.Write(InMesh.TextureCoords[0],8 * InMesh.NVertices);
    end;}

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
//  OutMesh.SetString('Comment',AnsiString(InMesh.Name));
//  OutMesh.SetString('Name',AnsiString(NamePrefix + 'Mesh' + IntToStr(DataFile.MeshList.IndexOf(M))));

  MeshImp := TMeshImport.Create(OutMesh.Producers);
  UpdateMeshImp(MeshImp);

  Result := OutMesh;
end;

end.
