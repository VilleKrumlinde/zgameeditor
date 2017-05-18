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

{
  Import midi-files.

  Based on code by:

  F.Bouwmans
  fbouwmans@spiditel.nl

  Z.wan
  ziziiwan@hotmail.com
}

unit uMidiFile;

interface

uses
  SysUtils, Classes, Contnrs, AudioComponents;

type
  TChunkType = (ctIllegal, ctHeader, ctTrack);
  TFileType = (ftSingle, ftMultiSynch, ftMultiAsynch);
  TMidiFile = class;

  TMidiEvent = class
  private
    iTrack: Byte;
    iEvent: Byte;
    iData1: Byte;
    iData2: Byte;
    sLetter: string;
    iPulses: Integer;
    Position: Integer;
    iSize: Integer;
    TimeMsec,LengthMsec : integer;
  end;

  TMidiHead = record
    FileType: TFileType;
    NumberTracks: Integer;
    PulsesPerQuarter: Integer;
  end;
  PMidiHead = ^TMidiHead;

  TMidiTrack = class
  private
    EventList: TObjectList;
    FTrackName: string;
    FTrackKeyword: string;
    FTrackCopyright: string;
    FInstrument: string;
    FPosition: Integer;
    FTrackSize: Integer;
    CurEventI : integer;
    CurTimeMsec : integer;
  protected
    function GetTrackLength: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddEvent(Event: TMidiEvent; Mid : TMidiFile);
  end;

  TMidiFile = class
  private
    FMidiFile: TMemoryStream;
    FChunkType: TChunkType;
    FChunkLength: Integer;
    FChunkData: PByte;
    FChunkIndex: PByte;
    FChunkEnd: PByte;
    FMidiHead: TMidiHead;
    FMidiTrack: TMidiTrack;
    TrackList: TList;
    SecondsPerTick : single;
    LastWrittenPosition : integer;
    MidiWarnings,UsedPatchesList : TStringList;
    UsedChannels : array[0..15] of boolean;
    Tickrate : single;
    procedure ReadChunkHeader;
    procedure ReadChunkContent;
    procedure ProcessHeaderChunk;
    procedure ProcessTrackChunk;
    procedure ReadFile;
    function WriteTrackEvents(Stream: TMemoryStream; Time: integer) : integer;
    procedure WriteOneEvent(Stream: TMemoryStream; Track : TMidiTrack; Event: TMidiEvent);
    function TicksToTime(const I: integer): integer;
    procedure MidiWarning(const S: string);
    procedure GenerateInstruments(Music : TMusic);
    procedure GenerateChannels;
  public
    constructor Create(const FileName : string);
    destructor Destroy; override;
    procedure WriteZzdcResource(Music : TMusic; Stream : TMemoryStream);
  end;

implementation

uses ZLog,ZClasses,Vcl.Forms,ZApplication,Vcl.Dialogs,Winapi.Windows,AudioPlayer,DesignerGui;

procedure WriteVarLength(Stream : TMemoryStream; Value: LongInt; Mask: Byte = 0);
var
  B: byte;
  I: Longint;
begin
  B := Value and $7F;
  I := Value shr 7;
  if I > 0 then
    WriteVarLength(Stream, I, $80);
  B := B or Mask;
  Stream.Write(B,1);
end;

procedure WriteTime(Stream : TMemoryStream; Value: LongInt);
begin
  //Convert from msecs to zge-miditiming
  WriteVarLength(Stream,Value div 10);
end;

function ReadVarLength(var PIndex: PByte): Integer;
var
  i: integer;
  b: byte;
begin
  b := $80;
  i := $0;
  while b > $7F do
  begin
    i := i shl 7;
    b := PIndex^;
    i := i + b and $7F;
    Inc(PIndex);
  end;
  Result := i;
end;

function ReadString(Len: Integer; var PIndex: PByte): string;
var
  i: Integer;
begin
  SetLength(Result,Len);
  for i := 0 to Len - 1 do
  begin
    Result[i+1] := Chr(PIndex^);
    inc(PIndex);
  end;
end;

procedure LengthToByte(Value: Integer; var b1, b2, b3, b4: Byte);
begin
  b1 := Value div $1000000;
  b2 := Value div $10000;
  b3 := Value div $100;
  b4 := Value;
end;

procedure ByteToLength(b1, b2, b3, b4: Byte; var Value: Integer);
begin
  Value := b4 + b3 * $100 + b2 * $10000 + b1 * $1000000;
end;

{ TMidiTrack }

constructor TMidiTrack.Create;
begin
  inherited Create;
  EventList := TObjectList.Create(True);
end;

destructor TMidiTrack.Destroy;
begin
  EventList.Free;
  inherited;
end;

procedure TMidiTrack.AddEvent(Event: TMidiEvent; Mid : TMidiFile);
begin
  if (Event.iEvent = $FF) then
  begin
    case Event.iData1 of
      $1: FTrackKeyword := FTrackKeyword + Event.sLetter;
      $2: FTrackCopyright := FTrackCopyright + Event.sLetter;
      $3: FTrackName := FTrackName + Event.sLetter;
      $4: FInstrument := FInstrument + Event.sLetter;
    end;
  end else
  begin
//    case Event^.iEvent of
//      $B0..$BF, $C0..$CF: // control change, program change
//        FChannels[Event^.iEvent and $F] := True;
//    end;
  end;

  FPosition := FPosition + Event.iPulses;
  Event.Position := FPosition;

  Event.LengthMsec := Mid.TicksToTime(Event.iPulses);
  CurTimeMsec := CurTimeMsec + Event.LengthMsec;
  Event.TimeMsec := CurTimeMsec;

  EventList.Add(Event);
end;

function TMidiTrack.GetTrackLength: Integer;
begin
  Result := TMidiEvent(EventList[EventList.Count - 1]).Position;
end;

{ TMidiFile }

constructor TMidiFile.Create(const FileName : string);
begin
  FChunkType := ctIllegal;
  FChunkLength := -1;
  FChunkData := nil;
  FChunkIndex := nil;
  FChunkEnd := nil;

  TrackList := TList.Create;

  Self.FMidiFile := TMemorySTream.Create;
  Self.FMidiFile.LoadFromFile(FileName);

  MidiWarnings := TStringList.Create;
  MidiWarnings.Sorted := True;
  MidiWarnings.Duplicates := dupIgnore;

  UsedPatchesList := TStringList.Create;

  ZLog.GetLog(Self.ClassName).Write( 'Importing: ' + ExtractFileName(FileName) );

  Self.ReadFile;
end;

destructor TMidiFile.Destroy;
var
  i: Integer;
begin
  if not (FChunkData = nil) then
    FreeMem(FChunkData);

  for i := 0 to TrackList.Count - 1 do
    TMidiTrack(TrackList[i]).Free;
  TrackList.Free;

  FMidiFile.Free;
  MidiWarnings.Free;
  UsedPatchesList.Free;

  inherited;
end;

procedure TMidiFile.ReadChunkHeader;
var
  tmpByte: array[0..7] of Byte;
begin
  FChunkType := ctIllegal;
  FChunkLength := -1;
  // read "4D 54 68 64", follow "00 00 00 06", Head
  // read "4D 54 72 6B", follow "00 00 0C DF", Track
  FMidiFile.Read(tmpByte,8);
  if (tmpByte[0] = $4D) and (tmpByte[1] = $54) then // MT
  begin
    if (tmpByte[2] = $68) and (tmpByte[3] = $64) then // hd, mean header
      FChunkType := ctHeader;
    if (tmpByte[2] = $72) and (tmpByte[3] = $6B) then // rk, mean track
      FChunkType := ctTrack;
  end;
  if FChunkType <> ctIllegal then
    ByteToLength(tmpByte[4], tmpByte[5], tmpByte[6], tmpByte[7], FChunkLength)
end;

procedure TMidiFile.ReadChunkContent;
begin
  if not (FChunkData = nil) then
    FreeMem(FChunkData);
  GetMem(FChunkData, FChunkLength + 1);
  FMidiFile.Read(FChunkData^, FChunkLength);
  FChunkIndex := FChunkData;
  FChunkEnd := PByte(Integer(FChunkIndex) + Integer(FChunkLength) - 1);
end;

procedure TMidiFile.ProcessHeaderChunk;
var
  i: Integer;
  W : word;
  B : byte;
begin
  ReadChunkHeader;
  if FChunkType <> ctHeader then
    raise Exception.Create('Invalid midi format!');
  ReadChunkContent;

  inc(FChunkIndex); // ff ff
  case FChunkIndex^ of
    0: FMidiHead.FileType := ftSingle;
    1: FMidiHead.FileType := ftMultiSynch;
    2: FMidiHead.FileType := ftMultiAsynch;
  end;
  inc(FChunkIndex); // nn nn
  i := FChunkIndex^ * $100;
  inc(FChunkIndex);
  FMidiHead.numberTracks := i + FChunkIndex^;

  //Parse time-division

  inc(FChunkIndex); // dd dd
  i := FChunkIndex^ * $100;
  inc(FChunkIndex);
  W := i + FChunkIndex^;
  FMidiHead.PulsesPerQuarter := W;

  if (W and $8000)>0 then
  begin
    //Frames per second
    B := (W and $7f00) shr 8; //SMPTE frames
    Tickrate := B;         //Frames per second
    if Tickrate=29.0 then  //29 -> 29.97
      Tickrate := 29.97;
    //Lower 8 bits=Ticks per frame
    Tickrate := Tickrate * (W and $00ff);
    Self.SecondsPerTick := 1.0 / Tickrate;
  end
  else
  begin
    //Ticks per quarter note  = ticks per beat
    Tickrate := (W and $7FFF);
    //Se STK
    //Default tempo is 120 beats per minute = 2 per second
    Self.SecondsPerTick := (0.5 / Tickrate);
  end;

end;


procedure TMidiFile.ProcessTrackChunk;
var
  iEvent: Integer;
  iLength, I : Integer;
  pEvent: TMidiEvent;
  pStart: PByte;
begin
  ReadChunkHeader;
  if FChunkType <> ctTrack then
  begin
    if (FMidiFile.Size-FMidiFile.Position)<256 then
    begin
      MidiWarning('Ignoring junk at end of file');
      FMidiFile.Position := FMidiFile.Size;
    end
    else
      raise Exception.Create('Invalid midi format!');
  end;
  ReadChunkContent;
  iEvent := 0;

  FMidiTrack := TMidiTrack.Create;
  FMidiTrack.FTrackSize := FChunkLength;
  TrackList.Add(FMidiTrack); // Add to track list
  while Integer(FChunkIndex) < Integer(FChunkEnd) do
  begin
    pEvent := TMidiEvent.Create;
    pStart := FChunkIndex;
    pEvent.iTrack := TrackList.Count;
    // Each event starts with var length delta time
    iLength := ReadVarLength(FChunkIndex);
    if FChunkIndex^ >= $80 then
    begin
      iEvent := FChunkIndex^;
      inc(FChunkIndex);
    end;
    // Else it is a running status event (just the same event as before)
    pEvent.iEvent := iEvent;
    pEvent.iPulses := iLength;
    if iEvent = $FF then
    begin
      pEvent.iData1 := FChunkIndex^; // Type is stored in data1
      inc(FChunkIndex);
      iLength := ReadVarLength(FChunkIndex);
      pEvent.sLetter := ReadString(iLength, FChunkIndex);
      if pEvent.iData1=81 then
      begin //tempo change
        I := Byte( pEvent.sLetter[1] ) * $10000 +
          Byte( pEvent.sLetter[2] ) * $100 +
          Byte( pEvent.sLetter[3] );
        //http://www.piclist.com/techref/io/serial/midi/midifile.html
        Self.SecondsPerTick := I / Tickrate / 1000000;
      end;
    end else
    begin
      // These are all midi events
      case iEvent of
        $80..$8F, // Note off
          $90..$9F, // Note on
          $A0..$AF, // Key aftertouch
          $B0..$BF, // Control change
          $E0..$EF: // Pitch wheel change
          begin
            pEvent.iData1 := FChunkIndex^;
            inc(FChunkIndex);
            pEvent.iData2 := FChunkIndex^;
            inc(FChunkIndex);
            //Some files use "note on" with velocity zero instead of note-off
            //Convert them to note-off events
            if (iEvent shr 4=$9) and (pEvent.iData2=0) then
               pEvent.iEvent := pEvent.iEvent and (255- (1 shl 4));
          end;
        $C0..$CF, // Program change
          $D0..$DF: // Channel aftertouch
          begin
            pEvent.iData1 := FChunkIndex^;
            inc(FChunkIndex);
          end;
      end;
    end;
    pEvent.iSize := Integer(FChunkIndex) - Integer(pStart);
    FMidiTrack.AddEvent(pEvent,Self);
  end; // End while
end;

procedure TMidiFile.ReadFile;
begin
  ProcessHeaderChunk;
  while FMidiFile.Position<FMidiFile.Size do
    ProcessTrackChunk;
end;

function TMidiFile.TicksToTime(const I : integer) : integer;
begin
  //Returns ticks recalculated to msecs
  Result := Round(I * Self.SecondsPerTick * 1000) ;
end;

procedure TMidiFile.MidiWarning(const S : string);
begin
  if MidiWarnings.IndexOf(S)>-1 then
    Exit;
  MidiWarnings.Add(S);
  ZLog.GetLog(Self.ClassName).Write( S );
end;

procedure TMidiFile.WriteOneEvent(Stream : TMemoryStream; Track : TMidiTrack; Event : TMidiEvent);
var
  I,NoteLength,TimeToNextNote : integer;
  InstrumentNr : byte;
  EndEvent,E : TMidiEvent;
  S : string;
begin
  case Event.iEvent shr 4 of
    $8 : ; //note off, ignore
    $9 : //Note on
      begin
        //find matching note off-event to get note length
        NoteLength := 0;
        EndEvent := nil;
        TimeToNextNote := 0;
        for I := Track.CurEventI+1 to Track.EventList.Count - 1 do
        begin
          E := TMidiEvent(Track.EventList[I]);
          Inc(NoteLength,E.LengthMsec);
          if (E.iEvent shr 4=$8) and //note off
            (E.iEvent and 15=Event.iEvent and 15) and  //same channel
            (E.iData1=Event.iData1) //same note
          then
          begin
            EndEvent := E;
            Break;
          end;
          if (TimeToNextNote=0) and (E.iEvent shr 4=$9) and //note on
            (E.iEvent and 15=Event.iEvent and 15) then  //same channel
            TimeToNextNote := NoteLength;
        end;
        if EndEvent=nil then
        begin
          MidiWarning('Note off missing');
          //Hur lång ska den då vara?
          //Prova först med längd till nästa note-on
          NoteLength := TimeToNextNote;
          if (NoteLength=0) or (NoteLength>10) then
            NoteLength := 2;
        end;
        UsedChannels[Event.iEvent and 15] := True;
        //ZLog.GetLog(Self.ClassName).Write( Format('Write %d : %d : %d : %d',
        //  [Event.iEvent,Event.iPulses,Event.iData1,NoteLength]) );
        WriteTime(Stream, Event.TimeMsec - Self.LastWrittenPosition );  //delta-time
        Stream.Write(Event.iEvent,1); //playnote id
        Stream.Write(Event.iData1,1); //note nr
        WriteTime(Stream,NoteLength);  //length
        if Event.iData2=0 then
          MidiWarning('Note on with velocity zero');
        Stream.Write(Event.iData2,1); //velocity
        Self.LastWrittenPosition := Event.TimeMsec;  //event is written: reset delta for next event
      end;
    $B : //controller event
      begin
        case Event.iData1 of
          7 : //Channel volume
            begin
              WriteTime(Stream, Event.TimeMsec - Self.LastWrittenPosition );  //delta-time
              Stream.Write(Event.iEvent,1); //command
              Stream.Write(Event.iData2,1); //volume
              Self.LastWrittenPosition := Event.TimeMsec;  //event is written: reset delta for next event
            end;
        else
          MidiWarning(Format('Ignore ctrl event %d', [Event.iData1]));
        end
      end;
    $C : //program change
      begin
        S := IntToStr(Event.iData1);
        //Convert patch-nr to instrument nr
        if UsedPatchesList.IndexOf(S)=-1 then
          UsedPatchesList.Add(S);
        InstrumentNr := UsedPatchesList.IndexOf(S);
        //ZLog.GetLog(Self.ClassName).Write( Format('Patch change %d : %d', [Event.iEvent,Event.iData1]) );
        WriteTime(Stream, Event.TimeMsec - Self.LastWrittenPosition );  //delta-time
        Stream.Write(Event.iEvent,1); //program change
        Stream.Write(InstrumentNr,1); //instrument nr
        Self.LastWrittenPosition := Event.TimeMsec;  //event is written: reset delta for next event
      end;
    $F : //meta event FF
      begin
        case Event.iData1 of
          1 : ZLog.GetLog(Self.ClassName).Write( 'Text: ' +  Event.sLetter );
          3 : //ZLog.GetLog(Self.ClassName).Write( 'Track name: ' +  Event.sLetter );
        else
          MidiWarning(Format('Ignore meta cmd %d', [Event.iData1]));
        end;
      end
    else
      MidiWarning(Format('Ignore cmd %d', [Event.iEvent shr 4]));
  end;
end;


//Skriv de events som inträffar nu
function TMidiFile.WriteTrackEvents(Stream : TMemoryStream; Time : integer) : integer;
var
  I : integer;
  Track : TMidiTrack;
  Event : TMidiEvent;
begin
  Result := High(Integer);
  for I := 0 to TrackList.Count - 1 do
  begin
    Track := TMidiTrack(TrackList[I]);
    while Track.CurEventI<Track.EventList.Count - 1 do
    begin
      Event := TMidiEvent(Track.EventList[Track.CurEventI]);
      if Event.TimeMsec<=Time then
        WriteOneEvent(Stream,Track,Event)
      else
      begin
        //Returnera tidpunkt för närmaste event från något track
        if Event.TimeMsec<Result then
          Result := Event.TimeMsec;
        Break;
      end;
      Inc(Track.CurEventI);
    end;
  end;
end;

procedure TMidiFile.GenerateInstruments(Music : TMusic);
var
  Root,Inst : TZComponent;
  InstGroup : TLogicalGroup;
  S : string;
  I,PatchNr : integer;
begin
  ZLog.GetLog(Self.ClassName).Write( 'Instrument count: ' + IntToStr(UsedPatchesList.Count) );
  S := ExtractFilePath(Application.ExeName) + 'MidiInstruments.xml';
  Root := ComponentManager.LoadXmlFromFile(S);
  try
    InstGroup := (Root as TZApplication).Content[0] as TLogicalGroup;
    if Music.Instruments.Count>0 then
    begin
      case Application.MessageBox('Music->Instruments list property will be overwritten with instruments of the imported midi-file. Is this ok?'#13#13 +
        'Select YES to OVERWRITE instruments'#13+
        'Select NO to KEEP the instruments that are already there'#13 +
        'Select Cancel to cancel midi-import' ,
        'Midi import', MB_YESNOCANCEL) of
        IDYES :  ;
        IDNO : Exit;
        IDCANCEL : Abort;
      end;
    end;
    Music.Instruments.Clear;
    if UsedPatchesList.Count=0 then
      //Some files do not contain patch-changes, just add a default instrument
      Music.Instruments.AddComponent((InstGroup.Children[0] as TSound).Clone)
    else for I := 0 to UsedPatchesList.Count - 1 do
    begin
      PatchNr := StrToInt(UsedPatchesList[I]);
      Inst := InstGroup.Children[PatchNr] as TSound;
      Music.Instruments.AddComponent(Inst.Clone);
    end;
  finally
    Root.Free;
  end;
end;

procedure TMidiFile.GenerateChannels;
var
  S : string;
  Channel : PChannel;
  Changed : boolean;
  I : integer;
begin
  Changed := False;
  S := '';
  for I := 0 to High(UsedChannels) do
  begin
    if not UsedChannels[I] then
      Continue;
    S := S + IntToStr(I) + ',';

    if I=9 then
      Continue; //todo: skip drum track for now

    Channel := AudioPlayer.GetChannel(I);
    if Channel.Active then
      Continue;
    Channel.Active := True;
    Channel.Volume := 0.5;
    Changed := True;
  end;
  S := Copy(S,1,Length(S)-1);
  ZLog.GetLog(Self.ClassName).Write( 'Used channels: ' + S );
  if Changed then
  begin
//    if FindInstanceOf(ZApp,TAudioMixer)=nil then
//      ShowMessage('Audio-channels were activated. Add a AudioMixer-component to your project to save mixer settings.');
  end;
end;

procedure TMidiFile.WriteZzdcResource(Music : TMusic; Stream : TMemoryStream);
var
  Time : integer;
begin
  Time := 0;
  repeat
    Time := WriteTrackEvents(Stream,Time);
  until Time=High(Integer);
  try
    GenerateInstruments(Music);
  except
    on E : EAbort do raise;
    on E : Exception do
      ShowMessage('Failed to assign midi instruments: ' + E.Message);
  end;
  GenerateChannels;
end;


end.

