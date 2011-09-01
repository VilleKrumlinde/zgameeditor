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

unit ZLog;

//Log, debug and error unit

interface

{$ifndef minimal}
uses SysUtils;
{$endif}

type
  TLogString = {$ifdef zlog}string{$else}PAnsiChar{$endif};

  {$ifdef zlog}
  TLogLevel = (lleNormal,lleWarning,lleError,lleUserTrace);

  TLog = class
  private
    LastTime : single;
  public
    ID : integer;
    Name : TLogString;
    procedure Write(S : TLogString); overload;
    procedure Write(S : TLogString; Level : TLogLevel); overload;
    procedure Warning(S : TLogString);
    procedure Error(S : TLogString);
    procedure BeginTimer;
    procedure EndTimer(S: TLogString);
  end;

  TLogReceiverFunc = procedure(Log : TLog; Mess : TLogString; Level : TLogLevel) of object;
  {$endif}

  {$ifndef minimal}
  EZHalted = class(Exception);
  {$endif}


//Fatal error, display message and exit program
//Use instead of raise exception
procedure ZHalt(ErrorMessage : TLogString);
procedure ZAssert(B : boolean; const ErrorMessage : TLogString);

{$ifdef zlog}
procedure SetReceiverFunc(F : TLogReceiverFunc);
function GetLog(const LogName : TLogString) : TLog;
{$endif}


implementation

uses ZPlatform
  {$ifdef zlog},Contnrs,Classes{$endif};

procedure ZHalt(ErrorMessage : TLogString);
begin
 {$ifdef minimal}
  Platform_Error(PAnsiChar(ErrorMessage));
  System.Halt;
 {$else}
  raise EZHalted.Create(ErrorMessage);
 {$endif}
end;

procedure ZAssert(B : boolean; const ErrorMessage : TLogString);
begin
  if not B then
    ZHalt(ErrorMessage);
end;

{$ifdef zlog}
var
  Logs : TObjectList;
  LogLookup : TStringList;
  ReceiverFunc : TLogReceiverFunc;

procedure InitLog;
begin
  Logs := TObjectList.Create(True);
  LogLookup := TStringList.Create;
  LogLookup.Sorted := True;
end;


procedure SetReceiverFunc(F : TLogReceiverFunc);
begin
  Assert( not Assigned(ReceiverFunc) );
  ReceiverFunc := F;
end;

function GetLog(const LogName : TLogString) : TLog;
var
  I,J,LogId : integer;
  Log : TLog;
begin
  if LogLookup=nil then
    InitLog;
  I := LogLookup.IndexOf(LogName);
  if I=-1 then
  begin
    //Give each a unique id by hashing logname
    LogId := Length(LogName);
    for J := 1 to Length(LogName) do
      Inc(LogId,Ord(LogName[J]));
    Log := TLog.Create;
    Log.ID := LogId;
    Log.Name := LogName;
    LogLookup.AddObject(LogName,Log);
    Logs.Add(Log);
  end else
    Log := TLog(LogLookup.Objects[I]);
  Result := Log;
end;

{ TLog }

procedure TLog.Write(S: TLogString);
begin
  Write(S,lleNormal);
end;

procedure TLog.Write(S : TLogString; Level : TLogLevel);
begin
  if Assigned(ReceiverFunc) then
    ReceiverFunc(Self,S,Level);
end;

procedure TLog.Warning(S: TLogString);
begin
  Write(S,lleWarning);
end;

procedure TLog.Error(S: TLogString);
begin
  Write(S,lleError);
end;

procedure TLog.BeginTimer;
begin
  LastTime := Platform_GetTime;
end;

procedure TLog.EndTimer(S: TLogString);
begin
  {$ifndef minimal}
  Write( S + ': ' + SysUtils.FormatFloat('0.0###',Platform_GetTime - LastTime) );
  {$endif}
end;

{$endif}


initialization

finalization
  {$ifdef zlog}
  Logs.Free;
  LogLookup.Free;
  {$endif}
end.
