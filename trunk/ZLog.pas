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
  TLogString = {$ifdef zlog}string{$else}PChar{$endif};

  {$ifdef zlog}
  TLog = class
  private
    LastTime : single;
  public
    ID : integer;
    Name : TLogString;
    procedure Write(S : TLogString);
    procedure BeginTimer;
    procedure EndTimer(S: TLogString);
  end;

  TLogReceiverFunc = procedure(Log : TLog; Mess : TLogString) of object;
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
  Platform_Error(PChar(ErrorMessage));
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
  if Assigned(ReceiverFunc) then
    ReceiverFunc(Self,S);
end;

procedure TLog.BeginTimer;
begin
  LastTime := Platform_GetTime;
end;

procedure TLog.EndTimer(S: TLogString);
begin
  {$ifndef minimal}
  Write( S + ': ' + SysUtils.FloatToStr(Platform_GetTime - LastTime) );
  {$endif}
end;

{$endif}


initialization
  {$ifdef zlog}
  Logs := TObjectList.Create(True);
  LogLookup := TStringList.Create;
  LogLookup.Sorted := True;
  {$endif}
finalization
  {$ifdef zlog}
  Logs.Free;
  LogLookup.Free;
  {$endif}
end.
