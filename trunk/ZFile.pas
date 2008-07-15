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

uses ZClasses;

type
  TZFile = class(TZComponent)
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    FileName : TPropString;
    FileNameFloatRef : TZPropertyRef;
    FileEmbedded : TZBinaryPropValue;
    Encoding : (feChar,feBinary);
    OnRead : TZComponentList;
  end;

  TFileAction = class(TCommand)
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Action : (faRead,faWrite);
    ZFile : TZFile;
    procedure Execute; override;
    {$ifndef minimal}
    function GetDisplayName: String; override;
    {$endif}
  end;

  TFileMoveData = class(TCommand)
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    ZProperty : TZPropertyRef;
    procedure Execute; override;
    {$ifndef minimal}
    function GetDisplayName: String; override;
    {$endif}
  end;

implementation

uses ZLog;

var
  CurFileState : (fsNone,fsReading,fsWriting);
  CurInStream : TZInputStream;
  CurFile : TZFile;

{ TZFile }

procedure TZFile.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'FileName',{$ENDIF}integer(@FileName) - integer(Self), zptString);
  List.AddProperty({$IFNDEF MINIMAL}'FileNameFloatRef',{$ENDIF}integer(@FileNameFloatRef) - integer(Self), zptPropertyRef);
  List.AddProperty({$IFNDEF MINIMAL}'FileEmbedded',{$ENDIF}integer(@FileEmbedded) - integer(Self), zptBinary);
  List.AddProperty({$IFNDEF MINIMAL}'Encoding',{$ENDIF}integer(@Encoding) - integer(Self), zptByte);
    {$ifndef minimal}List.GetLast.SetOptions(['Char','Binary']);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'OnRead',{$ENDIF}integer(@OnRead) - integer(Self), zptComponentList);
end;

{ TFileAction }

{$ifndef minimal}
const
  FileActionNames : array[0..0] of string = ('Read');
{$endif}

procedure TFileAction.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'File',{$ENDIF}integer(@ZFile) - integer(Self), zptComponentRef);
    {$ifndef minimal}List.GetLast.SetChildClasses([TZFile]);{$endif}
    {$ifndef minimal}List.GetLast.NeedRefreshNodeName := True;{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'Action',{$ENDIF}integer(@Action) - integer(Self), zptByte);
    {$ifndef minimal}List.GetLast.SetOptions(FileActionNames);{$endif}
    {$ifndef minimal}List.GetLast.NeedRefreshNodeName := True;{$endif}
end;

procedure TFileAction.Execute;
var
  S : PChar;
  FloatBuf : array[0..15] of char;
  NameBuf : array[0..254] of char;
begin
  {$ifndef minimal}
  if CurFileState<>fsNone then
    ZHalt('FileAction failed. CurFileState<>fsNone.');
  {$endif}

  if ZFile.FileEmbedded.Size>0 then
  begin
    //Open file embedded in exe-file
    CurInStream := TZInputStream.CreateFromMemory(ZFile.FileEmbedded.Data,ZFile.FileEmbedded.Size);
  end
  else
  begin
    //Open external file
    if ZFile.FileNameFloatRef.Component<>nil then
    begin
      //Om textref är satt så konvertera float-värde till sträng
      ZStrConvertFloat(
        PFloat(ZFile.FileNameFloatRef.Component.GetPropertyPtr(ZFile.FileNameFloatRef.Prop,ZFile.FileNameFloatRef.Index))^,
        PChar(@FloatBuf));
      ZStrCopy(NameBuf,PChar(ZFile.FileName));
      ZStrCat(NameBuf,PChar(@FloatBuf));
      S := PChar(@NameBuf);
    end
    else
      S := PChar(ZFile.FileName);

    {$ifdef zlog}
    ZLog.GetLog(Self.ClassName).Write('ZFile Open: ' + S);
    {$endif}

    CurInStream := TZInputStream.CreateFromFile(S,True);
  end;

  CurFileState := fsReading;
  CurFile := Self.ZFile;

  {$ifndef minimal}
  try
  {$endif}
    ZFile.OnRead.ExecuteCommands;
  {$ifndef minimal}
  finally
  {$endif}
    CurInStream.Free;
    CurFileState := fsNone;
  {$ifndef minimal}
  end;
  CurFile := nil;
  CurInStream := nil;
  {$endif}

end;

{$ifndef minimal}
function TFileAction.GetDisplayName: String;
begin
  Result := inherited GetDisplayName;
  if Assigned(Self.ZFile) then
    Result := Result + '  ' + FileActionNames[ Ord(Action) ] + ' ' + ZFile.Name;
end;
{$endif}


{ TFileMoveData }

procedure TFileMoveData.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Property',{$ENDIF}integer(@ZProperty) - integer(Self), zptPropertyRef);
    {$ifndef minimal}List.GetLast.NeedRefreshNodeName := True;{$endif}
end;

procedure TFileMoveData.Execute;
var
  PropValuePtr : PFloat;
  V : single;
  B : byte;
begin
  {$ifndef minimal}
  if (CurFileState=fsNone) or (CurFile=nil) or (CurInStream=nil) then
    ZHalt('FileMoveData failed. No file opened for reading.');
  if (ZProperty.Component=nil) then
    ZHalt('FileMoveData failed. No target property set.');
  {$endif}
  case CurFileState of
    fsReading :
      begin
        PropValuePtr := ZProperty.Component.GetPropertyPtr(ZProperty.Prop,ZProperty.Index);
        case CurFile.Encoding of
          feChar :
            begin
              CurInStream.Read(B,SizeOf(B));
              V := Trunc(B);
            end;
          feBinary :
            CurInStream.Read(V,SizeOf(V));
        end;
        PropValuePtr^ := V;
      end;
  end;
end;

{$ifndef minimal}
function TFileMoveData.GetDisplayName: String;
begin
  Result := inherited GetDisplayName;
  if Assigned(ZProperty.Component) then
    Result := Result + '  ' + ZProperty.Component.Name + '.' + ZProperty.Prop.Name;
end;
{$endif}

initialization

  ZClasses.Register(TZFile,ZFileClassId);
    {$ifndef minimal}ComponentManager.LastAdded.ZClassName := 'File';{$endif}
    {$ifndef minimal}ComponentManager.LastAdded.HelpText := 'A file on disk for reading and writing of data';{$endif}
    {$ifndef minimal}ComponentManager.LastAdded.AutoName := True;{$endif}
  ZClasses.Register(TFileAction,FileActionClassId);
    {$ifndef minimal}ComponentManager.LastAdded.HelpText := 'Controls when to read/write from a File';{$endif}
  ZClasses.Register(TFileMoveData,FileMoveDataClassId);
    {$ifndef minimal}ComponentManager.LastAdded.HelpText := 'Move data from/to a File. Use only in File.OnRead.';{$endif}

end.
