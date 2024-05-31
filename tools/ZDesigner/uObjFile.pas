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
    Verts,Normals,Colors,TexCoords : TList<TZVector3f>;
    Faces : TList<TObjFace>;
    FileName : string;
    Materials : TObjectDictionary<string,TObjMaterial>;
    HasVertexColors : boolean;
    RequireFloatTexCoords : boolean;
    procedure UpdateMeshImp(MeshImp: TMeshImport);
    function GenerateMesh : TZComponent;
    procedure AddMaterialLib(const MatFileName : string);
    procedure ScaleAndCenter;
  public
    AutoScale,AutoCenter,IncludeVertexColors,IncludeTextureCoords,IncludeNormals : boolean;
    MeshImpToUpdate : TMeshImport;
    ResultMesh : TMesh;
    constructor Create(const FileName : string);
    destructor Destroy; override;
    procedure Import;
  end;

implementation

uses SysUtils, ZMath, ZLog, Math;

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
  Self.IncludeNormals := True;
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
    MinV[0] := Math.Min(MinV[0],Self.Verts[I][0]);
    MinV[1] := Math.Min(MinV[1],Self.Verts[I][1]);
    MinV[2] := Math.Min(MinV[2],Self.Verts[I][2]);
    MaxV[0] := Math.Max(MaxV[0],Self.Verts[I][0]);
    MaxV[1] := Math.Max(MaxV[1],Self.Verts[I][1]);
    MaxV[2] := Math.Max(MaxV[2],Self.Verts[I][2]);
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

  function FaceIndex(Count, I : integer) : integer;
  begin
    if I < 0 then
      //Negative index means relative postion of vertex
      Exit(Count + I)
    else
      Exit(I - 1);
  end;

type
  TVertEntry = record
    VertIndex, TexCoordIndex, NormalIndex : integer;
  end;
var
  S : string;
  L,L2 : TStringList;
  I : integer;
  Mat : TObjMaterial;
  Face,OtherFace : TObjFace;
  V: TZVector3f;
  AllColors, AllVerts, AllNormals, AllTexCoords : TList<TZVector3f>;
  Entry : TVertEntry;
  EntryCache : TDictionary<TVertEntry, integer>;
  NewVertIndex : integer;
begin
  L := TStringList.Create;
  L.Delimiter := ' ';

  L2 := TStringList.Create;
  L2.Delimiter := '/';

  AllVerts := TList<TZVector3f>.Create;
  AllNormals := TList<TZVector3f>.Create;
  AllTexCoords := TList<TZVector3f>.Create;
  AllColors := TList<TZVector3f>.Create;

  EntryCache := TDictionary<TVertEntry, integer>.Create;

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
      AllVerts.Add(  Vector3f( StrToFloatDef(L[1],0),StrToFloatDef(L[2],0),StrToFloatDef(L[3],0)  )  );
      if L.Count >= 7 then
        // vertex RGB color specified
        AllColors.Add( Vector3f( StrToFloatDef(L[4],0),StrToFloatDef(L[5],0),StrToFloatDef(L[6],0)  ) );
    end
    else if L[0]='vn' then
    begin
      AllNormals.Add(  Vector3f( StrToFloatDef(L[1],0),StrToFloatDef(L[2],0),StrToFloatDef(L[3],0)  )  )
    end
    else if L[0]='vt' then
    begin
      if L.Count>=3 then
      begin
        V := Vector3f( StrToFloatDef(L[1],0),StrToFloatDef(L[2],0),0  );
        AllTexCoords.Add( V );
        if (V[0]<0) or (V[0]>1) or (V[1]<0) or (V[1]>1) then
          RequireFloatTexCoords := True;
      end;
    end
    else if L[0]='f' then
    begin
      if L.Count in [4,5] then
      begin
        for I := 0 to L.Count - 2 do
        begin
          L2.DelimitedText := L[1+I];
          Entry.VertIndex := FaceIndex(AllVerts.Count, StrToInt(L2[0]));
          if (L2.Count > 1) and (AllTexCoords.Count > 0) then
            Entry.TexCoordIndex := FaceIndex(AllTexCoords.Count, StrToIntDef(L2[1],0))
          else
            Entry.TexCoordIndex := 0;
          if (L2.Count > 2) and (AllNormals.Count > 0) then
            Entry.NormalIndex := FaceIndex(AllNormals.Count, StrToInt(L2[2]))
          else
            Entry.NormalIndex := 0;

          if not EntryCache.TryGetValue(Entry, NewVertIndex) then
          begin
            NewVertIndex := Verts.Count;
            EntryCache.Add(Entry, NewVertIndex);
            Verts.Add( AllVerts[Entry.VertIndex] );
            if AllTexCoords.Count > 0 then
              TexCoords.Add( AllTexCoords[ EnsureRange(Entry.TexCoordIndex, 0, AllTexCoords.Count-1) ] );
            if AllNormals.Count > 0 then
              Normals.Add( AllNormals[ EnsureRange(Entry.NormalIndex, 0, AllNormals.Count-1) ] );
            if AllColors.Count > 0 then
              Colors.Add( AllColors[ EnsureRange(Entry.VertIndex, 0, AllColors.Count-1) ] )
            else if Assigned(Mat) then
              Colors.Add( Mat.Diffuse );
          end;

          if I = 3 then
          begin  //Split quad into two tris
            //3 4 1
            OtherFace.Index[2] := Face.Index[0];
            OtherFace.Index[0] := Face.Index[2];
            OtherFace.Index[1] := NewVertIndex;
            Self.Faces.Add(OtherFace);
          end else
            Face.Index[I] := NewVertIndex;

        end;
        Self.Faces.Add(Face);
      end else
        raise Exception.Create('OBJ-reader: Only triangle or quad surfaces supported');
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

  AllVerts.Free;
  AllNormals.Free;
  AllTexCoords.Free;
  AllColors.Free;

  EntryCache.Free;

  Self.HasVertexColors := Colors.Count > 0;

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
  Stream,StU,StV : TMemoryStream;
  MinV,MaxV,DiffV,V : TZVector3f;
  W : word;
  J : integer;
  Sm : smallint;
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
      MinV[0] := Math.Min(MinV[0],Self.Verts[I][0]);
      MinV[1] := Math.Min(MinV[1],Self.Verts[I][1]);
      MinV[2] := Math.Min(MinV[2],Self.Verts[I][2]);
      MaxV[0] := Math.Max(MaxV[0],Self.Verts[I][0]);
      MaxV[1] := Math.Max(MaxV[1],Self.Verts[I][1]);
      MaxV[2] := Math.Max(MaxV[2],Self.Verts[I][2]);
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

    if Verts.Count > High(Word) then
    begin
      MeshImp.IndicesFormat := mifInteger;
      for I := 0 to Self.Faces.Count - 1 do
      begin
        J := Self.Faces[I].Index[0];
        Stream.Write(J,4);
        J := Self.Faces[I].Index[1];
        Stream.Write(J,4);
        J := Self.Faces[I].Index[2];
        Stream.Write(J,4);
      end;
    end
    else
    begin
      MeshImp.IndicesFormat := mifWord;
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
    end;

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
      if RequireFloatTexCoords then
      begin
        MeshImp.HasTextureCoords := mtcFloat;
        for I := 0 to Self.Verts.Count-1 do
        begin
          V := TexCoords[I];
          Stream.Write(V, 8);
        end;
      end else
      begin
        MeshImp.HasTextureCoords := mtcDeltaS16;
        StU := TMemoryStream.Create;
        StV := TMemoryStream.Create;

        //Delta-encode in separate streams
        W := Round( Frac(Self.TexCoords[0][0]) * High(Smallint) );
        StU.Write(W,2);
        W := Round( Frac(Self.TexCoords[0][1]) * High(Smallint) );
        StV.Write(W,2);
        V := Self.TexCoords[0];
        for I := 1 to Self.Verts.Count-1 do
        begin
          //Idea from Kjell: decode while encoding and take the delta from the decoded value
          //to prevent precision problems. Helps when there are many (100k+) texcoords.
          Sm := Round( (Self.TexCoords[I][0]-V[0]) * High(SmallInt) );
          StU.Write(Sm,2);
          V[0] := V[0] + (Sm / High(Smallint));
          Sm := Round( (Self.TexCoords[I][1]-V[1]) * High(SmallInt) );
          StV.Write(Sm,2);
          V[1] := V[1] + (Sm / High(Smallint));
        end;
        StU.SaveToStream(Stream);
        StV.SaveToStream(Stream);
        StU.Free;
        StV.Free;
      end;
    end else
      MeshImp.HasTextureCoords := mtcNone;

    if Self.IncludeNormals and (Self.Normals.Count>=Self.Verts.Count) then
    begin
      MeshImp.NormalsFormat := mnfFloat;
      for I := 0 to Self.Verts.Count-1 do
      begin
        V := Normals[I];
        Stream.Write(V, SizeOf(V));
      end;
    end else
      MeshImp.NormalsFormat := mnfNone;

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
