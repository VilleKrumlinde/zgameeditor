{ Copyright (C) 2009, Serge Voloshenyuk
  
  This file is Free Software and part of DCocoR
  
  It is licensed under the following three licenses as alternatives:
    1. GNU Lesser General Public License (LGPL) V2.1 or any newer version
    2. GNU General Public License (GPL) V2 or any newer version
    3. Apache License, V2.0 or any newer version
  
  You may not use this file except in compliance with at least one of
  the above three licenses.
  
  See LICENSE.txt at the top of this package for the
  complete terms and further detail along with the license texts for
  the licenses in COPYING-LIB.txt, COPYING.txt and LICENSE-2.0.txt respectively.
} 

unit CocoAncestor;

interface
uses Classes, CocoSets;

const
  minErrDist = 2; { minimal distance (good tokens) between two errors }
  LookBackDefaultCount = 1;
  LookAheadDefaultCount = 3;

  { Standard Error Types }
  etSyntax = 0;
  etSymantic = 1;
  etWarning = 2;

  _EOFSYMB = 0;

type
{$IFDEF UNICODE}
   CocoChar = Char;
{$ELSE}
   CocoChar = WideChar;
{$ENDIF}
  TCocoRGrammar = class;

  TSetArray = array of TCocoSet;

  PSymbol = ^TSymbolRec;
  TSymbolRec = record
    Beg,  Len,
    Line, Col,
    Id: Integer;
  end;

  TGetCH = function(var pos: Integer): CocoChar of object;
  TErrorEvent = procedure(Sender: TObject; ErrorType,ErrorCode, line,col: Integer;
    const Msg, data: string) of object;

  TStartTableEntry = packed record
    Ch, State: Word;
  end;

  TStartTable = class
  private
    fEnterPoints: array[0..127] of array of TStartTableEntry;
    function getState(const aCh: Integer): Integer;
    procedure setState(const aCh: Integer; const Value: Integer);
  public
    constructor Create;
    procedure FillRange(start_, end_, value: Integer);
    property States[const aCh: Integer]: Integer read getState write setState; default;
  end;

  TBaseScanner = class(TComponent)
  private
    fSourceBegin, fSourceLen,
    fStartLine, fStartCol: Integer;
  protected
    fSource: String;
    fUTF8, fCaseInsensitive: Boolean;
    procedure setCaseInsensitive(const Value: Boolean); virtual;
    procedure setUTF8(const Value: Boolean); virtual;
    function  GetText(pos,len: Integer): String;
  public
    noSym: Integer;

    procedure Reset; virtual; abstract;
    procedure Get(var sym: TSymbolRec); virtual; abstract;

    procedure SetSource(const Src: String; pos: PSymbol=nil); virtual;
    procedure SetSourceFile(const FileName: String); virtual;
    procedure GetSourcePosition(var pos: TSymbolRec);
    procedure GetPosition(var pos: TSymbolRec); virtual; abstract;
    procedure GotoPosition(var pos: TSymbolRec); virtual; abstract;

    function  getSymbolPtr(pos: PSymbol): PChar;
    function  getSymbolText(pos: PSymbol): string;
    function  GetLine(var pos: Integer; var line: string): Boolean;

    property CaseInsensitive: Boolean read fCaseInsensitive write setCaseInsensitive;
    property UTF8: Boolean read fUTF8 write setUTF8;
    property Source: String read fSource;
    property SourceBegin: Integer read fSourceBegin;
    property SourceLen: Integer read fSourceLen;
    property StartLine: Integer read fStartLine;
    property StartCol: Integer read fStartCol;
  end;

  TCocoRGrammar = class(TComponent)
  private
    fScanner: TBaseScanner;
    fLookAheadCount, fLookBackCount, fLookAroundGap,
    fCurSymbolIndex, fNextSymbolIndex, fRealAheadCount: Integer;
    fSymbols: array of TSymbolRec;
    fErrDist: Integer;
    fOnError: TErrorEvent;
    fErrorCount, fWarnCount: Integer;
    fSymSets: TSetArray;
    function SymbolOffset(Idx: Integer): Integer;
    function getLexName(const Index: Integer): string;
    function getLexString(const Index: Integer): string;
    function GetSuccessful: Boolean;
    function getSymbol(const Ind: Integer): PSymbol;
    function getCurSymbol: PSymbol;
    function getCurLine: Integer;
    function getNextSymbol: PSymbol;
    procedure setCurSymbolIndex(const Value: Integer);
    function getCurrentInputSymbol: Integer;
  protected
    property CurSymbolIndex: Integer read fCurSymbolIndex write setCurSymbolIndex;
  public
    class procedure ClearSymSet(arr: TSetArray);
    class procedure InitSymSets( var aSymSets: TSetArray; vals: array of Integer);
    class function DequotedStr(const str: String): String;

    procedure LookAroundGap(Back,Ahead: Word);
    function  CreateScanner: TBaseScanner; virtual;
    function  TokenToString(n: Integer): String; virtual;

    procedure DoError(Sender: TObject; ErrorType,ErrorCode, line,col: Integer; const Data: string);
    function  ErrorMessage(ErrorType,ErrorCode: Integer; const data: string): String; virtual;
    function  ErrorPrefix(ErrorType: Integer): String; virtual;

    procedure Get;
    procedure ProcessPragmas; virtual;
    procedure CheckHomograph(var sym: Integer); virtual;
    procedure RestoreHomograph(Id: Integer);

    procedure Expect(n: Integer);
    function  InSet(Symbol, SetIndex: Integer): Boolean;
    procedure ExpectWeak(n, expectedSetIndex: Integer);
    function  WeakSeparator(n, sySuccIdx, iterSuccIdx: Integer) : boolean;

    procedure SetSource(const Src: String); virtual;
    procedure SetSourceFileName(const Filename: String); virtual;
    function  Bookmark: String;
    procedure GotoBookmark(const aBookmark: String);

    procedure SemError(const errNo : integer; const Data : string='');
    procedure SemErrorInLine(const errNo,line: integer; const Data : string='');
    procedure SynError(const errNo : integer);
    procedure SynErrorExpect(const aId: integer);
    procedure Warning(line: Integer; const Msg: string);

    procedure Reinit; virtual;
    function Execute: Boolean; virtual;

    constructor Create(AOwner: TComponent); override;

    property ErrorCount: Integer read fErrorCount write fErrorCount;
    property WarnCount: Integer read fWarnCount write fWarnCount;

    property Scanner: TBaseScanner read fScanner write fScanner;
    property LookBackCount: Integer read fLookBackCount;
    property LookAheadCount: Integer read fLookAheadCount;
    property CurrentInputSymbol: Integer read getCurrentInputSymbol;
    property ErrDist: Integer read fErrDist;

    property CurSymbol: PSymbol read getCurSymbol;
    property CurLine: Integer read getCurLine;
    property NextSymbol: PSymbol read getNextSymbol;
    property Symbols[const Ind: Integer]: PSymbol read getSymbol;
    property LexName : string index 0 read getLexName;
    property LexString:  string index 0 read getLexString;
    property LexNames[const Ind: Integer]: string read getLexName;
    property LexStrings[const Ind: Integer]: string read getLexString;
    property Successful : Boolean read GetSuccessful;
    property SymSets: TSetArray read fSymSets write fSymSets;
  published
    property OnError: TErrorEvent read fOnError write fOnError;
  end;

  TCocoRScanner = class(TBaseScanner)
  private
    fTabLength: Integer;
    fGetCh: TGetCh;
    fCurrInputCh: CocoChar;

    fSymbolStartPos, fBufferPosition,fNextPosition: Integer;
    fCurrLine, fCurrCol: integer;

    fLiterals: TStringList;
    fStartState: TStartTable;
    procedure AdjustGetter;
  protected
    procedure setCaseInsensitive(const Value: Boolean); override;
    procedure setUTF8(const Value: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Reset; override;
    procedure GetPosition(var pos: TSymbolRec); override;
    procedure GotoPosition(var pos: TSymbolRec); override;
    procedure BeginContext(var pos: TSymbolRec);
    procedure EndContext(var pos: TSymbolRec);

    function  CharAt(var pos: Integer): CocoChar;
    function  CharUTF8At(var pos: Integer): CocoChar;
    function  CapChAt(var pos: Integer): CocoChar;
    function  CapChUTF8At(var pos: Integer): CocoChar;
    procedure NextCh;

    procedure Get(var sym: TSymbolRec); override;
    procedure ScanSym(state: Integer; var sym: Integer); virtual;

    function SkipComments(ind: Integer): Boolean; virtual;
    procedure SkipTo(const Str: String);
    procedure SkipIgnoreSet; virtual;
    procedure SkipCommentTo(const str: String);
    procedure SkipNestedComment(const str1,str2: String);

    procedure CheckLiteral(var Sym: Integer);

    property GetCh: TGetCh read fGetCh;
    property BufferPosition: Integer read fBufferPosition write fBufferPosition;
    property CurrInputCh: CocoChar read fCurrInputCh;
    property CurrLine: Integer read fCurrLine;
    property CurrCol: Integer read fCurrCol;
    property TabLength: Integer read fTabLength write fTabLength;
    property StartState: TStartTable read fStartState write fStartState;
    property Literals: TStringList read fLiterals write fLiterals;
  end;

  TResorceSystem = class
  public
    function AbsoluteURL(const BaseURL, RelativeURL: String): String; virtual; abstract;
    function ResourceExists(const URL: String; const mime: String=''): Boolean; virtual; abstract;
    function ResourceAge(const URL: string; out ResDateTime: TDateTime; const mime: String=''): Boolean; virtual; abstract;
    function ResourceForReading(const URL: String; const mime: String=''): TStream; virtual; abstract;
    function GetResString(const URL: String; const mime: String=''): String; virtual; abstract;
    function ResourceForWriting(const URL: String; const mime: String=''): TStream; virtual; abstract;
  end;

procedure ClearSymbol(pos: PSymbol);
function LoadFileToString(const FileName: String): String;
function CreateLiterals(aCaseSensitive: Boolean; const Names: array of String; const Ids: array of Integer): TStringList;

var LineBreak: String = sLineBreak;
    LineBreakLen: Integer;
    ResorceSystem: TResorceSystem;

implementation

uses SysUtils,Windows,Generics.Collections;

var
  _GlobalCleanUps : TObjectList<TObject>;


function LoadFileToString(const FileName: String): String;
var fstrm: TFileStream;
    strm: TStringStream;
begin
  fstrm := TFileStream.Create(FileName,fmOpenRead);
  strm := TStringStream.Create('');
  try
    strm.CopyFrom(fstrm,fstrm.Size);
    Result := strm.DataString;
  finally
    strm.Free;
    fstrm.Free;
  end;
end;

function CreateLiterals(aCaseSensitive: Boolean; const Names: array of String; const Ids: array of Integer): TStringList;
var I: Integer;
begin
  Result := TStringList.Create;
  with Result do
  begin
    CaseSensitive := aCaseSensitive;
    Sorted := True;
    Duplicates := dupIgnore;
  end;
  for I := Low(Names) to High(Names) do
    Result.AddObject(Names[I], TObject(Ids[I]));
  _GlobalCleanUps.Add(Result);
end;

{ TSymbolRec }

procedure ClearSymbol(pos: PSymbol);
begin
  with pos^ do begin
    Beg  := -1;
    Len  := 0;
    Line := 0;
    Col  := 0;
    Id   := _EOFSYMB;
  end;
end;

{ TBaseScanner }

procedure TBaseScanner.setCaseInsensitive(const Value: Boolean);
begin
  fCaseInsensitive := Value;
end;

procedure TBaseScanner.setUTF8(const Value: Boolean);
begin
  fUTF8 := Value;
end;

procedure TBaseScanner.SetSource(const Src: String; pos: PSymbol);
begin
  fSource := Src;
  if pos<>nil then
  with pos^ do
  begin
    fSourceBegin := Beg;
    fSourceLen := Len;
    fStartLine := Line;
    fStartCol := Col;
  end else
  begin
    fSourceBegin := 0;
    fSourceLen := Length(fSource);
    fStartLine := 1;
    fStartCol := 0;
  end;
  Reset;
end;

procedure TBaseScanner.SetSourceFile(const FileName: String);
begin
  SetSource(ResorceSystem.GetResString(FileName));
end;

procedure TBaseScanner.GetSourcePosition(var pos: TSymbolRec);
begin
  with pos do
  begin
    Beg := fSourceBegin;
    Len := fSourceLen;
    Line := fStartLine;
    Col := fStartCol;
  end;
end;

function TBaseScanner.GetSymbolPtr(pos: PSymbol): PChar;
begin
  if (pos<>nil)and(pos.Beg>=0) then
    Result := PChar(fSource)+SourceBegin+pos.Beg
  else Result := nil;
end;


function TBaseScanner.getSymbolText(pos: PSymbol): string;
begin
  if pos<>nil  then
  with pos^ do
  if (Beg>=0)and(Len>0) then
    SetString(Result, PChar(fSource)+SourceBegin+Beg, Len)
  else Result := '';
end;

function TBaseScanner.GetText(pos, len: Integer): String;
begin
  SetString(Result, PChar(fSource)+SourceBegin+pos, len);
end;

function TBaseScanner.GetLine(var pos: Integer; var line: string): Boolean;
var s,e,p: PChar;
begin
  s := PChar(fSource)+SourceBegin+pos;
  e := PChar(fSource)+SourceBegin+SourceLen;
  if s>=e then
  begin
    Result := False; Exit;
  end;
  Result := True;
  p := s;
  while (p<e)and(p^<>#10)and(p^<>#13) do
    Inc(p);
  if s<>p then
    SetString(line, s, p-s)
  else line := '';
  if p^=#13 then Inc(p);
  Inc(p);
  Inc(pos,p-s);
end;


{ TCocoRScanner }

constructor TCocoRScanner.Create(AOwner: TComponent);
begin
  fGetCh := CharAt;
  fTabLength := 8;
  inherited;
  fCurrInputCh := #0;
  fBufferPosition := -1;
  fNextPosition := 0;
end;

procedure TCocoRScanner.setCaseInsensitive(const Value: Boolean);
begin
  if fCaseInsensitive <> Value then
  begin
    fCaseInsensitive := Value;
    AdjustGetter;
  end;
end;

procedure TCocoRScanner.setUTF8(const Value: Boolean);
begin
  if fUTF8<>Value then
  begin
    fUTF8 := Value;
    AdjustGetter;
  end;
end;

procedure TCocoRScanner.AdjustGetter;
begin
  if fCaseInsensitive then
  begin
    if UTF8 then
      fGetCh := CapChUTF8At
    else fGetCh := CapChAt;
  end else begin
    if UTF8 then fGetCh := CharUTF8At
    else fGetCh := CharAt;
  end;
end;

procedure TCocoRScanner.Reset;
begin
  fCurrInputCh := #0;
  fBufferPosition := -1;
  fNextPosition := 0;
  fCurrLine    := StartLine;
  fCurrCol     := StartCol;
  NextCh;
end;

procedure TCocoRScanner.GetPosition(var pos: TSymbolRec);
begin
  with pos do
  begin
    Beg := fBufferPosition;
    Line := fCurrLine;
    Col := fCurrCol;
    Id := Ord(fCurrInputCh);
  end;
end;

procedure TCocoRScanner.GotoPosition(var pos: TSymbolRec);
begin
  with pos do
  begin
    fNextPosition := Beg;
    fCurrLine    := Line;
    fCurrCol     := Col;
    fCurrInputCh := CocoChar(Id);
    NextCh;
  end;
end;

procedure TCocoRScanner.NextCh;
var tmpCh: CocoChar;
begin
  tmpCh := fCurrInputCh;
  fBufferPosition := fNextPosition;
  fCurrInputCh := GetCh(fNextPosition);
  if tmpCh = #10 then
  begin
    Inc(fCurrLine);
    fCurrCol := 0;
  end;
  if fCurrInputCh=#9 then
    fCurrCol := ((fCurrCol + fTabLength - 1) div fTabLength)*fTabLength+1
  else Inc(fCurrCol);
end;

function TCocoRScanner.CharUTF8At(var pos: Integer): CocoChar;
var ch: Integer;
begin
  ch := 0;
  if (pos<0)or(pos>=SourceLen) then
        Result := #0
  else begin
     while True do
     begin
       ch := Ord(fSource[pos+SourceBegin+1]);
       if (ch<127)or((ch and $C0)=$C0) then Break;
       Inc(pos);
     end;
     if (ch and $F0)=$F0 then
     begin
      Result := CocoChar(((((((ch and $7) shl 6) or (Ord(fSource[pos+SourceBegin+2])and $3F)) shl 6)
                  or (Ord(fSource[pos+SourceBegin+3])and $3F)) shl 6)
                  or (Ord(fSource[pos+SourceBegin+4]) and $3F));
      Inc(pos,4);
     end else if (ch and $E0)=$E0 then
     begin
      Result := CocoChar(((((ch and $F) shl 6) or (Ord(fSource[pos+SourceBegin+2])and $3F)) shl 6)
                or (Ord(fSource[pos+SourceBegin+3])and $3F));
      Inc(pos,3);
     end else if (ch and $C0)=$C0 then
     begin
       Result := CocoChar(((ch and $1F) shl 6) or (Ord(fSource[pos+SourceBegin+2])and $3F));
       Inc(pos,2);
     end else begin
       Result := CocoChar(ch);
       Inc(pos);
     end;
   end;
end;

function TCocoRScanner.CharAt(var pos: Integer): CocoChar;
begin
  if (pos<0)or(pos>=SourceLen) then
        Result := #0
  else  Result := CocoChar(fSource[pos+SourceBegin+1]);
  Inc(pos);
end;

procedure TCocoRScanner.CheckLiteral(var Sym: Integer);
var I: Integer; str: String;
begin
  str := GetText(fSymbolStartPos,BufferPosition-fSymbolStartPos);
  if (fLiterals<>nil)and fLiterals.Find(str,I) then
    Sym := Integer(fLiterals.Objects[I]);
end;


procedure TCocoRScanner.SkipIgnoreSet;
begin
  while CurrInputCh = ' ' do NextCh;
end;

type  TScanProc = procedure(const str1,str2: String) of object;

function TCocoRScanner.SkipComments(ind: Integer): Boolean;
begin
  Result := False;
end;

procedure TCocoRScanner.SkipTo(const Str: String);
  function AtStr: Boolean;
  var I,P: Integer;
  begin
    Result := False;
    P := fNextPosition;
    for I := 2 to Length(Str) do
      if GetCh(P)<>CocoChar(Str[I]) then Exit;
    Result := True;
  end;
begin
 while true do
 begin
   while (CurrInputCh<>#0)and(CurrInputCh<>CocoChar(Str[1])) do NextCh;
   if (CurrInputCh=#0) or AtStr then Break
   else NextCh;
 end;
end;

procedure TCocoRScanner.SkipNestedComment(const str1,str2: String);

  function AtStr(const Str: String): Boolean;
  var I,P: Integer;
  begin
    Result := False;
    P := fNextPosition;
    for I := 2 to Length(Str) do
      if GetCh(P)<>CocoChar(Str[I]) then Exit;
    Result := True;
  end;

  var level: Integer;
begin
 level := 1;
 while True do
 begin
    while (CurrInputCh<>#0)and((CurrInputCh<>CocoChar(str1[1]))and(CurrInputCh<>CocoChar(str2[1]))) do NextCh;
    if CurrInputCh=#0 then Exit;
    if (CurrInputCh=CocoChar(str1[1]))and AtStr(str1) then
      Inc(Level)
    else if (CurrInputCh=CocoChar(str2[1]))and AtStr(str2) then
      Dec(Level);
    NextCh;
    if Level=0 then
    begin
      if CurrInputCh<>#0 then
      begin
        level := Length(str2)-1;
        while level>0 do begin NextCh; Dec(level); end;
      end;
      Exit;
    end;
 end;
end;

procedure TCocoRScanner.SkipCommentTo(const str: String);
var I: Integer;
begin
  SkipTo(str);
  if CurrInputCh<>#0 then
  begin
    I := Length(str);
    while I>0 do begin NextCh; Dec(I); end;
  end;
end;

procedure TCocoRScanner.BeginContext(var pos: TSymbolRec);
begin
  if pos.Beg = -1 then
    GetPosition(pos);
end;

procedure TCocoRScanner.EndContext(var pos: TSymbolRec);
begin
  if pos.Beg>=0 then
    GotoPosition(pos);
end;

function TCocoRScanner.CapChAt(var pos: Integer): CocoChar;
begin
  Result := CocoChar(UpCase(Char(CharAt(pos))));
end;

function TCocoRScanner.CapChUTF8At(var pos: Integer): CocoChar;
begin
  Result := CharAt(pos);
  if Result<>#0 then
    CharUpperBuffW(Pointer(@Result),1);
end;

procedure TCocoRScanner.Get(var sym: TSymbolRec);
begin
  while True do
  begin
    SkipIgnoreSet;
    with sym do
    begin
      fSymbolStartPos := BufferPosition;
      Beg := BufferPosition;
      Line := CurrLine;
      Col := CurrCol;
      Len := 0;
    end;

    ScanSym(StartState[Ord(CurrInputCh)],sym.Id);

    if sym.Id = _EOFSYMB then
    begin
      fCurrInputCh := #0;
      Dec(fBufferPosition);
      Dec(fNextPosition);
    end else if sym.Id<>noSym then
      sym.Len := BufferPosition-sym.Beg;
    if (sym.Id>noSym) and SkipComments(sym.Id) then Continue;
    Exit;
  end;
end;

procedure TCocoRScanner.ScanSym(state: Integer; var sym: Integer);
begin
  sym := noSym;
end;

{ TCocoRGrammar }

constructor TCocoRGrammar.Create(AOwner: TComponent);
begin
  inherited;
  LookAroundGap(LookBackDefaultCount, LookAheadDefaultCount);
  fScanner := CreateScanner;
  Reinit;
end;

procedure TCocoRGrammar.LookAroundGap(Back, Ahead: Word);
begin
  fLookAheadCount := Ahead;
  fLookBackCount := Back;
  fLookAroundGap := Ahead+Back+1;
  SetLength(fSymbols,fLookAroundGap);
end;

function TCocoRGrammar.SymbolOffset(Idx: Integer): Integer;
begin
  Result := Idx mod (fLookAroundGap);
  if Result<0 then
     Inc(Result,fLookAroundGap);
end;

function TCocoRGrammar.CreateScanner: TBaseScanner;
begin
  Result := TCocoRScanner.Create(Self);
end;

class procedure TCocoRGrammar.ClearSymSet(arr: TSetArray);
var I: Integer;
begin
  for I := Low(arr) to High(arr) do
    arr[I].Free;
end;

function TCocoRGrammar.getSymbol(const Ind: Integer): PSymbol;
var I: Integer;
begin
  if ((Ind<0)and(-Ind>fLookBackCount)) or
     ((Ind>0)and(Ind>fLookAheadCount)) then
     raise Exception.Create('Look Gap Range');
  if Ind>fRealAheadCount then
  begin
    I := CurSymbolIndex+fRealAheadCount;
    while fRealAheadCount < Ind do
    begin
      I := SymbolOffset(I+1);
      Scanner.Get(fSymbols[I]);
      Inc(fRealAheadCount);
    end;
  end;
  Result := @fSymbols[SymbolOffset(CurSymbolIndex+Ind)];
end;

function TCocoRGrammar.getCurLine: Integer;
begin
  Result := CurSymbol^.Line;
end;

function TCocoRGrammar.getCurrentInputSymbol: Integer;
begin
  Result := fSymbols[fNextSymbolIndex].Id;
  if Result<0 then
  begin
    Result := -Result;
    CheckHomograph(Result);
  end;
end;

function TCocoRGrammar.getCurSymbol: PSymbol;
begin
  Result := @fSymbols[fCurSymbolIndex];
end;

function TCocoRGrammar.getNextSymbol: PSymbol;
begin
  Result := getSymbol(1);
end;

function TCocoRGrammar.getLexName(const Index: Integer): string;
begin
  Result := getLexString(Index);
   if Scanner.CaseInsensitive then
    Result := UpperCase(Result);
    //FIXME doesn't work for utf8, but LexName's used for ident and therefore generally is ansi
end;

function TCocoRGrammar.getLexString(const Index: Integer): string;
begin
  Result := Scanner.getSymbolText(getSymbol(Index));
end;

class function TCocoRGrammar.DequotedStr(const str: String): String;
begin
  if Length(str)>=2 then
    Result := AnsiDequotedStr(str,str[1])
  else Result := '';
end;

procedure TCocoRGrammar.SetSource(const Src: String);
begin
  Scanner.SetSource(Src);
  Reinit;
end;

procedure TCocoRGrammar.SetSourceFileName(const Filename: String);
begin
  Scanner.SetSourceFile(Filename);
  Reinit;
end;

function TCocoRGrammar.Bookmark: string;
var writer: TWriter;
    stream: TStringStream;
    pos: TSymbolRec;
begin
 stream := TStringStream.Create('');
 try
  writer := TWriter.Create(stream,1024);
  try
   Scanner.GetPosition(pos);
   writer.Write(pos,Sizeof(pos));
   writer.Write(fSymbols[0],Sizeof(TSymbolRec)*Length(fSymbols));
   writer.WriteInteger(fCurSymbolIndex);
   writer.WriteInteger(fNextSymbolIndex);
   writer.WriteInteger(fRealAheadCount);
  finally
    writer.Free;
  end;
  Result := stream.DataString;
 finally
   stream.Free;
 end;
end;

procedure TCocoRGrammar.GotoBookmark(const aBookmark: string);
var reader: TReader;
    stream: TStringStream;
    pos: TSymbolRec;
begin
 stream := TStringStream.Create(aBookmark);
 try
  reader := TReader.Create(stream,1024);
  try
   reader.Read(pos,Sizeof(pos));
   Scanner.GotoPosition(pos);
   reader.Read(fSymbols[0],Sizeof(TSymbolRec)*Length(fSymbols));
   CurSymbolIndex := reader.ReadInteger;
   fNextSymbolIndex := reader.ReadInteger;
   fRealAheadCount := reader.ReadInteger;
  finally
    reader.Free;
  end;
 finally
   stream.Free;
 end;
end;

class procedure TCocoRGrammar.InitSymSets( var aSymSets: TSetArray; vals: array of Integer);
var I,C,mI: Integer;
begin
  if Length(vals)=0 then Exit;
  C := 1;
  mI := -1;
  for I := Low(vals) to High(vals) do
  if vals[I]<0 then Inc(C)
  else if mI<vals[I] then mI := vals[I];

  SetLength(aSymSets,C);
  for I := 0 to C-1 do
  begin
    aSymSets[I] := TCocoSet.Create(mI+1);
    _GlobalCleanUps.Add(aSymSets[I]);
  end;

  C := 0;
  for I := Low(vals) to High(vals) do
  if vals[I]<0 then Inc(C)
  else aSymSets[C][vals[I]] := True;
end;

function TCocoRGrammar.InSet(Symbol, SetIndex: Integer): Boolean;
begin
  Result := fSymSets[SetIndex].Bits[Symbol];
end;

procedure TCocoRGrammar.ProcessPragmas;
begin
end;

function TCocoRGrammar.GetSuccessful: Boolean;
begin
  Result := ErrorCount=0;
end;

procedure TCocoRGrammar.DoError(Sender: TObject; ErrorType, ErrorCode, line,
  col: Integer; const Data: string);
begin
  if ErrorType=etWarning then
     Inc(fWarnCount)
  else Inc(fErrorCount);
  if Assigned(OnError) then
    OnError(Sender,ErrorType, ErrorCode,line,col,
     ErrorMessage(ErrorType, ErrorCode,data), Data);
end;

function TCocoRGrammar.ErrorMessage(ErrorType, ErrorCode: Integer; const data: string): String;
begin
  if (ErrorType=etSyntax)and(ErrorCode=0) then
    Result := Format('%s expected',[data])
  else begin
    if ErrorCode<>-1 then
    begin
      Result := Format('%s: %d',[ErrorPrefix(ErrorType),ErrorCode]);
      if Trim(Data)<>'' then
        Result := Format('%s (%s)',[Result,Data])
    end else Result := Format('%s: %s',[ErrorPrefix(ErrorType),Data]);
  end;
end;

function TCocoRGrammar.ErrorPrefix(ErrorType: Integer): String;
const errTypeNames: array[etSyntax..etWarning+1] of String = (
  'Syntax Error', 'Symantic Error', 'Warning', 'Error' );
begin
  if ErrorType>etWarning then ErrorType := etWarning+1;
  Result := errTypeNames[ErrorType];
end;

procedure TCocoRGrammar.SemErrorInLine(const errNo, line: integer;
  const Data: string);
begin
  DoError(Self,etSymantic,errNo, line,0,Data);
end;

procedure TCocoRGrammar.setCurSymbolIndex(const Value: Integer);
begin
  fCurSymbolIndex := SymbolOffset(Value);
  fNextSymbolIndex := SymbolOffset(Value+1);
end;

procedure TCocoRGrammar.SemError(const errNo: integer; const Data: string);
begin
  if ferrDist >= minErrDist then
  with Scanner, fSymbols[CurSymbolIndex] do
    DoError(Self,etSymantic,errNo,Line,Col,Data);
  ferrDist := 0;
end;

procedure TCocoRGrammar.SynError(const errNo: integer);
begin
  if ferrDist >= minErrDist then
  with Scanner,fSymbols[fNextSymbolIndex] do
    DoError(Self,etSyntax,errNo,Line,Col,'');
  ferrDist := 0;
end;

procedure TCocoRGrammar.SynErrorExpect(const aId: integer);
begin
  if ferrDist >= minErrDist then
  with Scanner,fSymbols[fNextSymbolIndex] do
    DoError(Self,etSyntax,0,Line,Col,TokenToString(aId));
  ferrDist := 0;
end;

procedure TCocoRGrammar.Warning(line: Integer; const Msg: string);
begin
  DoError(Self,etWarning,-1,line,0,Msg);
end;

function TCocoRGrammar.TokenToString(n: Integer): String;
begin
  Result := 'sym'+IntToStr(n);
end;

procedure TCocoRGrammar.Reinit;
var I: Integer;
begin
  fErrDist := minErrDist;
  fWarnCount := 0;
  fErrorCount := 0;
  Scanner.Reset;
  for I := Low(fSymbols) to High(fSymbols) do
    ClearSymbol(@fSymbols[I]);
  fCurSymbolIndex := -1;
  fRealAheadCount := 1;
  Get;
end;

procedure TCocoRGrammar.RestoreHomograph(Id: Integer);
var I,sym: Integer;
begin
  if Scanner is TCocoRScanner then
  with TCocoRScanner(Scanner) do
  if (fLiterals<>nil) and fLiterals.Find(LexNames[1],I) then
  begin
    sym := Integer(fLiterals.Objects[I]);
    if sym=-Id then
    fSymbols[fNextSymbolIndex].Id := Id;
  end;
end;

function TCocoRGrammar.Execute: Boolean;
begin
  Reinit;
  Result := False;
end;

procedure TCocoRGrammar.Expect(n: Integer);
begin
  if CurrentInputSymbol = n then
    Get
  else SynErrorExpect(n);
end;

procedure TCocoRGrammar.ExpectWeak(n, expectedSetIndex: Integer);
begin
  if CurrentInputSymbol = n then
    Get
  else begin
    SynErrorExpect(n);
    while (CurrentInputSymbol > _EOFSYMB) and (not InSet(CurrentInputSymbol,expectedSetIndex)) do
      Get;
  end
end;

function TCocoRGrammar.WeakSeparator(n, sySuccIdx, iterSuccIdx: Integer): boolean;
begin
  if CurrentInputSymbol=n then
  begin
    Get;
    Result := True;
  end
  else if InSet(CurrentInputSymbol, iterSuccIdx) then
    Result := False
  else begin
    SynErrorExpect(n);
    while not( InSet(CurrentInputSymbol,sySuccIdx)or InSet(CurrentInputSymbol,iterSuccIdx)or
             InSet(CurrentInputSymbol,0)
             ) do Get;
    Result := InSet(CurrentInputSymbol, sySuccIdx);
  end;
end;

procedure TCocoRGrammar.Get;
begin
  while True do
  begin
    CurSymbolIndex := CurSymbolIndex+1;
    Dec(fRealAheadCount);
    with Symbols[1]^ do
    begin
      if Id<=Scanner.noSym then
      begin
       INC(fErrDist); Break;
      end else begin
        ProcessPragmas;
        CurSymbolIndex := CurSymbolIndex-1;
      end;
    end;
  end;
end;

procedure TCocoRGrammar.CheckHomograph(var sym: Integer);
begin
end;

{ TStartTable }

constructor TStartTable.Create;
begin
  _GlobalCleanUps.Add(Self);
end;

procedure TStartTable.FillRange(start_, end_, value: Integer);
var I,J,K,C: Integer;
begin
  for I := start_ to end_ do
  begin
    J := I mod 127;
    C := Length(fEnterPoints[J]);
    for K := 0 to C-1 do
    with fEnterPoints[J][K] do
      if Ch=I then
      begin
        State := Value; Exit;
      end;
    SetLength(fEnterPoints[J],C+1);
    with fEnterPoints[J][C] do
    begin
      Ch := I;
      State := Value;
    end;
  end;
end;

function TStartTable.getState(const aCh: Integer): Integer;
var I,J,C: Integer;
begin
  I := aCh mod 127;
  C := Length(fEnterPoints[I]);
  for J := 0 to C-1 do
  with fEnterPoints[I][J] do
    if Ch=aCh then
    begin
      Result := State; Exit;
    end;
  Result := -1;
end;

procedure TStartTable.setState(const aCh: Integer; const Value: Integer);
var I,J,C: Integer;
begin
  I := aCh mod 127;
  C := Length(fEnterPoints[I]);
  for J := 0 to C-1 do
  with fEnterPoints[I][J] do
    if Ch=aCh then
    begin
      State := Value; Exit;
    end;
  SetLength(fEnterPoints[I],C+1);
  with fEnterPoints[I][C] do
  begin
    Ch := aCh;
    State := Value;
  end;
end;

initialization
   LineBreakLen := Length(LineBreak);
   _GlobalCleanUps := TObjectList<TObject>.Create(True);
finalization
   _GlobalCleanUps.Free;
end.
