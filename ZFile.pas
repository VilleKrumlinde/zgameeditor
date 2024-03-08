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

unit ZFile;

interface

{$include zzdc_globalopt.inc}

uses ZClasses, ZExpressions;

resourcestring
  strFileName = 'FileName';
  strFileNameFloatRef = 'FileNameFloatRef';
  strFileEmbedded = 'FileEmbedded';
  strEncoding = 'Encoding';
  strTargetArray = 'TargetArray';
  strOnRead = 'OnRead';
  strOnWrite = 'OnWrite';
  strPosition = 'Position';
  strSize = 'Size';
  strFile = 'File';
  strAction = 'Action';
  strProperty = 'Property';

type
  TZFile = class(TZComponent)
  private
    WriteFileName : array[0..254] of AnsiChar;
  protected
    procedure DefineProperties(List: TZPropertyList); override;
    procedure InitAfterPropsAreSet; override;
  public
    FileName : TPropString;
    FileNameFloatRef : TZExpressionPropValue;
    FileEmbedded : TZBinaryPropValue;
    FilePosition,FileSize : integer;
    Encoding : (feChar,feBinary);
    TargetArray : TDefineArray;
    OnRead : TZComponentList;
    OnWrite : TZComponentList;
    procedure Update; override;
  end;

  TFileAction = class(TCommand)
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Action : (faRead,faWrite);
    ZFile : TZFile;
    procedure Execute; override;
  end;

  TFileMoveData = class(TCommand)
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    ZProperty : TZExpressionPropValue;
    procedure Execute; override;
  end;

implementation

uses ZLog,ZPlatform;


const
  WriteBufSize=1024;
var
  CurFileState : (fsNone,fsReading,fsWriting);
  CurInStream : TZInputStream;
  CurFile : TZFile;
  CurWriteBuf : record
      Position,ByteSize : integer;
      Append : boolean;
      case Boolean of
        True : (BBuf : array[0..WriteBufSize-1] of byte);
        False : (FBuf : array[0..(WriteBufSize div 4)-1] of single);
    end;


procedure FlushWriteBuf;
begin
  Platform_WriteFile(CurFile.WriteFileName,@CurWriteBuf.BBuf,
    CurWriteBuf.ByteSize,CurWriteBuf.Append);
  CurWriteBuf.Position := 0;
  CurWriteBuf.ByteSize := 0;
  CurWriteBuf.Append := True;
end;

{ TZFile }

procedure TZFile.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}strFileName,{$ENDIF}(@FileName), zptString);
    List.GetLast.IsManagedTarget := True;
  List.AddProperty({$IFNDEF MINIMAL}strFileNameFloatRef,{$ENDIF}(@FileNameFloatRef), zptExpression);
    {$ifndef minimal}List.GetLast.ExpressionKind := ekiGetValue;{$endif}
  List.AddProperty({$IFNDEF MINIMAL}strFileEmbedded,{$ENDIF}(@FileEmbedded), zptBinary);
  List.AddProperty({$IFNDEF MINIMAL}strEncoding,{$ENDIF}(@Encoding), zptByte);
    {$ifndef minimal}List.GetLast.SetOptions(['Char','Binary']);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}strTargetArray,{$ENDIF}(@TargetArray), zptComponentRef);
    {$ifndef minimal}List.GetLast.SetChildClasses([TDefineArray]);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}strOnRead,{$ENDIF}(@OnRead), zptComponentList);
  List.AddProperty({$IFNDEF MINIMAL}strOnWrite,{$ENDIF}(@OnWrite), zptComponentList);

  List.AddProperty({$IFNDEF MINIMAL}strPosition,{$ENDIF}(@FilePosition), zptInteger);
    List.GetLast.NeverPersist := True;
  List.AddProperty({$IFNDEF MINIMAL}strSize,{$ENDIF}(@FileSize), zptInteger);
    List.GetLast.NeverPersist := True;
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
end;

{ TFileAction }

{$ifndef minimal}
const
  FileActionNames : array[0..1] of string = ('Read','Write');
{$endif}

procedure TFileAction.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}strFile,{$ENDIF}(@ZFile), zptComponentRef);
    {$ifndef minimal}List.GetLast.SetChildClasses([TZFile]);{$endif}
    {$ifndef minimal}List.GetLast.NeedRefreshNodeName := True;{$endif}
  List.AddProperty({$IFNDEF MINIMAL}strAction,{$ENDIF}(@Action), zptByte);
    {$ifndef minimal}List.GetLast.SetOptions(FileActionNames);{$endif}
    {$ifndef minimal}List.GetLast.NeedRefreshNodeName := True;{$endif}
end;

procedure TFileAction.Execute;

  procedure CopyToArray(A : TDefineArray);
  var
    I,BufPos : integer;
    B : byte;
    P : PInteger;
    Dest : PAnsiChar;
    DestP : ppointer;
    Buf : array[0..2047] of ansichar;
  begin
    {$ifndef minimal}
    if not (A._Type.Kind in [zctInt,zctFloat,zctByte,zctString]) then
    begin
      ZLog.GetLog(Self.ClassName).Warning('TargetArray must be of type string, int, float or byte.');
      Exit;
    end;
    {$endif}
    if A._Type.Kind=zctString then
    begin
      BufPos := 0;
      A.SizeDim1 := 0;
      A.GetData;
      for I := 0 to CurInStream.Size-1 do
      begin
        CurInStream.Read(B,1);
        if (B<>13) and (B<>10) then
        begin
          Buf[BufPos]:=AnsiChar(B);
          Inc(BufPos);
        end;
        if (B=10) or (BufPos>=High(Buf)-2) or (I=CurInStream.Size-1) and (BufPos>0) then
        begin
          Buf[BufPos]:=#0;
          A.SizeDim1 := A.SizeDim1 + 1;
          Dest := ManagedHeap_Alloc(BufPos+1);
          ZStrCopy(Dest,@Buf);
          DestP := PPointer(A.GetData);
          Inc(DestP,A.SizeDim1-1);
          DestP^ := Dest;
          BufPos:=0;
        end;
      end;
    end
    else if (CurFile.Encoding=feBinary) or (A._Type.Kind in [zctByte,zctFloat]) then
    begin
      A.SizeDim1 := CurInStream.Size div A.GetElementSize;
      CurInStream.Read(A.GetData^,CurInStream.Size);
    end else
    begin
      A.SizeDim1 := CurInStream.Size;
      P := PInteger(A.GetData);
      for I := 0 to CurInStream.Size-1 do
      begin
        CurInStream.Read(B,1);
        P^ := B;
        Inc(P);
      end;
    end;
    CurInStream.Position := 0;
  end;

  procedure WriteFromArray(A : TDefineArray);
  begin
    Platform_WriteFile(CurFile.WriteFileName,A.GetData,
      A.CalcLimit * A.GetElementSize,
      CurWriteBuf.Append);
    CurWriteBuf.Append := True;
  end;

var
  S : PAnsiChar;
  FloatBuf : array[0..15] of ansichar;
  NameBuf : array[0..254] of ansichar;
begin
  {$ifndef minimal}
  ZAssert(ZFile<>nil,'File property not set');
  if CurFileState<>fsNone then
    //Only allow a single file-operation to be active at one time
    ZHalt('FileAction failed. CurFileState<>fsNone.');
  {$endif}

  if ZFile.FileEmbedded.Size>0 then
  begin
    {$ifndef minimal}
    if Action=faWrite then
      ZHalt('FileAction failed. Cannot write to embedded file.');
    {$endif}
    //Open file embedded in exe-file
    CurInStream := TZInputStream.CreateFromMemory(ZFile.FileEmbedded.Data,ZFile.FileEmbedded.Size);
  end
  else
  begin
    //Open external file
    if ZFile.FileNameFloatRef.Code.Count>0 then
    begin
      //If ref is set then convert float-value to string
      ZStrConvertInt(
        Trunc( ExpGetValue(ZFile.FileNameFloatRef.Code) ),
        PAnsiChar(@FloatBuf));
      ZStrCopy(NameBuf,PAnsiChar(ZFile.FileName));
      ZStrCat(NameBuf,PAnsiChar(@FloatBuf));
      S := PAnsiChar(@NameBuf);
    end
    else
      S := PAnsiChar(ZFile.FileName);

    {$ifdef zlog}
    ZLog.GetLog(Self.ClassName).Write('ZFile Open: ' + S);
    {$endif}

    if Action=faRead then
      CurInStream := TZInputStream.CreateFromFile(S,True)
    else
    begin
      ZStrCopy(Self.ZFile.WriteFileName,S);
      CurWriteBuf.Position := 0;
    end;
  end;

  CurFile := Self.ZFile;

  CurFile.FilePosition := 0;
  if Action=faRead then
    CurFile.FileSize := CurInStream.Size;

  case Action of
    faRead :
      begin
        if CurFile.TargetArray<>nil then
        begin
          //Copy content directly to an array
          //(this avoids the need of FileMoveData components)
          CopyToArray(CurFile.TargetArray);
        end;
      {$ifndef minimal}try{$endif}
        CurFileState := fsReading;
        ZFile.OnRead.ExecuteCommands;
      {$ifndef minimal}finally{$endif}
        CurInStream.Free;
        CurFileState := fsNone;
      {$ifndef minimal}end;{$endif}
      end;
    faWrite :
      begin
      {$ifndef minimal}try{$endif}
        CurFileState := fsWriting;
        FillChar(CurWriteBuf,SizeOf(CurWriteBuf),0);
        if CurFile.TargetArray<>nil then
          WriteFromArray(CurFile.TargetArray);
        ZFile.OnWrite.ExecuteCommands;
        FlushWriteBuf;
      {$ifndef minimal}finally{$endif}
        CurFileState := fsNone;
      {$ifndef minimal}end;{$endif}
      end;
  end;

  {$ifndef minimal}
  CurFile := nil;
  CurInStream := nil;
  {$endif}
end;

procedure TZFile.InitAfterPropsAreSet;
begin
  inherited;
  Self.FileSize := Self.FileEmbedded.Size;
end;

procedure TZFile.Update;
begin
  inherited;
  //Set size so that xptr calls can read FileSize correctly
  if FileEmbedded.Size>0 then
    Self.FileSize := FileEmbedded.Size;
end;

{ TFileMoveData }

procedure TFileMoveData.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}strProperty,{$ENDIF}(@ZProperty), zptExpression);
    {$ifndef minimal}List.GetLast.ExpressionKind := ekiGetPointer;{$endif}
    {$ifndef minimal}List.GetLast.NeedRefreshNodeName := True;{$endif}
end;

procedure TFileMoveData.Execute;
var
  PropValuePtr : PFloat;
  V : single;
  B : byte;
begin
  {$ifndef minimal}
  if (CurFileState=fsNone) then
    ZHalt('FileMoveData failed. No file active.');
  if (ZProperty.Code.Count=0) then
    ZHalt('FileMoveData failed. No target property set.');
  {$endif}
  PropValuePtr := ExpGetPointer(Self.ZProperty.Code);
  case CurFileState of
    fsReading :
      begin
        CurInStream.Position := CurFile.FilePosition;
        case CurFile.Encoding of
          feChar :
            begin
              CurInStream.Read(B,SizeOf(B));
              V := B;
            end;
          feBinary :
            CurInStream.Read(V,SizeOf(V));
        end;
        PropValuePtr^ := V;
        CurFile.FilePosition := CurInStream.Position;
      end;
    fsWriting :
      begin
        V := PropValuePtr^;
        case CurFile.Encoding of
          feChar :
            begin
              B := Trunc(V);
              if CurWriteBuf.Position>High(CurWriteBuf.BBuf) then
                FlushWriteBuf;
              CurWriteBuf.BBuf[CurWriteBuf.Position] := B;
              Inc(CurWriteBuf.ByteSize);
            end;
          feBinary :
            begin
              if CurWriteBuf.Position>High(CurWriteBuf.FBuf) then
                FlushWriteBuf;
              CurWriteBuf.FBuf[CurWriteBuf.Position] := V;
              Inc(CurWriteBuf.ByteSize,4);
            end;
        end;
        Inc(CurWriteBuf.Position);
        CurFile.FilePosition := CurWriteBuf.ByteSize;
      end;
  end;
end;

initialization

  ZClasses.Register(TZFile,ZFileClassId);
    {$ifndef minimal}ComponentManager.LastAdded.ZClassName := 'File';{$endif}
    {$ifndef minimal}ComponentManager.LastAdded.HelpText := 'A file on disk for reading and writing of data';{$endif}
    {$ifndef minimal}ComponentManager.LastAdded.AutoName := True;{$endif}
    {$ifndef minimal}ComponentManager.LastAdded.ImageIndex := 55;{$endif}
  ZClasses.Register(TFileAction,FileActionClassId);
    {$ifndef minimal}ComponentManager.LastAdded.HelpText := 'Controls when to read/write from a File';{$endif}
    {$ifndef minimal}ComponentManager.LastAdded.ImageIndex := 57;{$endif}
  ZClasses.Register(TFileMoveData,FileMoveDataClassId);
    {$ifndef minimal}ComponentManager.LastAdded.HelpText := 'Move data from/to a File. Use only in File.OnRead or OnWrite.';{$endif}
    {$ifndef minimal}ComponentManager.LastAdded.ImageIndex := 56;{$endif}

end.