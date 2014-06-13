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

unit Commands;

interface

uses ZClasses,ZExpressions;

//General purpose commands for eventtree

type
  TRepeat = class(TCommand)
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Count : integer;                   //Set for a fixed nr of iterations
    OnIteration : TZComponentList;
    WhileExp : TZExpressionPropValue;  //Set to use a expression to test against
    Iteration : integer;
    procedure Execute; override;
    procedure Update; override;
  end;

  TCondition = class(TCommand)
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Expression : TZExpressionPropValue;
    OnTrue,OnFalse : TZComponentList;
    procedure Execute; override;
    procedure Update; override;
  end;

  TKeyPress = class(TCommand)
  private
    LastPressedAt : single;
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Keys : TPropString;
    RepeatDelay : single;
    OnPressed : TZComponentList;
    KeyIndex : integer;
    CharCode : word;
    procedure Execute; override;
  end;

  //Force a component to refresh itself by calling .Change
  TRefreshContent = class(TCommand)
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Component : TZComponent;
    procedure Execute; override;
    {$ifndef minimal}
    function GetDisplayName: AnsiString; override;
    {$endif}
  end;

  TZTimer = class(TCommand)
  strict private
    Time : single;
    CurrentIteration : integer;
    Stopped : boolean;
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    OnTimer : TZComponentList;
    Interval : single;
    RepeatCount : integer;
    procedure Execute; override;
    {$ifndef minimal}
    procedure DesignerReset; override;
    {$endif}
  end;

  TWebOpen = class(TCommand)
  strict private
    Buffer : pointer;
    BufferSize : integer;
    IsWaiting : boolean;
  private
    procedure AddToResultList;
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Url : TPropString;
    ParamArray,ResultArray : TDefineArray;
    ResultString : TPropString;
    OnResult : TZComponentList;
    InBrowser : boolean;
    Handle : pointer;
    class procedure FlushResultList;
    procedure Execute; override;
    procedure StartReading;
    destructor Destroy; override;
    {$ifndef minimal}
    function GetDisplayName: AnsiString; override;
    {$endif}
  end;

  TCallComponent = class(TCommand)
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Component : TCommand;
    procedure Execute; override;
    {$ifndef minimal}
    function GetDisplayName: AnsiString; override;
    {$endif}
  end;

var
  //Used by TWebOpen
  NetMutex : pointer;
  NetResultList : TZArrayList;

implementation

uses ZPlatform,ZApplication
  {$ifndef minimal},ZLog{$endif};

{ TRepeat }

procedure TRepeat.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Count',{$ENDIF}(@Count), zptInteger);
  List.AddProperty({$IFNDEF MINIMAL}'OnIteration',{$ENDIF}(@OnIteration), zptComponentList);
  List.AddProperty({$IFNDEF MINIMAL}'WhileExp',{$ENDIF}(@WhileExp), zptExpression);
    {$ifndef minimal}
    List.GetLast.DefaultValue.ExpressionValue.Source:='//this.Iteration=current iteration nr. Return false to end loop.';
    List.GetLast.ReturnType.Kind := zctFloat;
    {$endif}
  List.AddProperty({$IFNDEF MINIMAL}'Iteration',{$ENDIF}(@Iteration), zptInteger);
    List.GetLast.NeverPersist:=True;
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
end;

procedure TRepeat.Execute;
var
  I : integer;
  {$ifndef minimal}BailOutTime : single;{$endif}
begin
  {$ifndef minimal}BailOutTime:=Platform_GetTime + 3.0;{$endif}
  Iteration := 0;
  if WhileExp.Code.Count>0 then
  begin
    while True do
    begin
      ZExpressions.RunCode(WhileExp.Code);
      if PSingle(@ZExpressions.gReturnValue)^=0 then
        Break;
      OnIteration.ExecuteCommands;
      Inc(Iteration);
      {$ifndef minimal}
      if (Platform_GetTime>BailoutTime) and (Iteration>1) then
        ZHalt('Repeat-loop timeout. Check your repeat-components for infinite loops.');
      {$endif}
    end;
  end
  else
    for I := 0 to Count-1 do
    begin
      OnIteration.ExecuteCommands;
      Inc(Iteration);
    end;
end;

procedure TRepeat.Update;
begin
  inherited;
  OnIteration.Update;
end;

{ TCondition }

procedure TCondition.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Expression',{$ENDIF}(@Expression), zptExpression);
    {$ifndef minimal}List.GetLast.ReturnType.Kind := zctFloat;{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'OnTrue',{$ENDIF}(@OnTrue), zptComponentList);
    {$ifndef minimal}{List.GetLast.SetChildClasses([TCommand,TZExpression]);}{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'OnFalse',{$ENDIF}(@OnFalse), zptComponentList);
    {$ifndef minimal}{List.GetLast.SetChildClasses([TCommand,TZExpression]);}{$endif}
end;

procedure TCondition.Execute;
begin
  ZExpressions.RunCode(Expression.Code);
  if PSingle(@ZExpressions.gReturnValue)^<>0 then
    OnTrue.ExecuteCommands
  else
    OnFalse.ExecuteCommands;
end;

procedure TCondition.Update;
begin
  inherited;
  OnTrue.Update;
  OnFalse.Update;
end;

{ TKeyPress }

procedure TKeyPress.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Keys',{$ENDIF}(@Keys), zptString);
    List.GetLast.IsManagedTarget := True;
  List.AddProperty({$IFNDEF MINIMAL}'CharCode',{$ENDIF}(@CharCode), zptByte);
  List.AddProperty({$IFNDEF MINIMAL}'RepeatDelay',{$ENDIF}(@RepeatDelay), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}'OnPressed',{$ENDIF}(@OnPressed), zptComponentList);
  List.AddProperty({$IFNDEF MINIMAL}'KeyIndex',{$ENDIF}(@KeyIndex), zptInteger);
    List.GetLast.NeverPersist := True;
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
end;

procedure TKeyPress.Execute;
var
  P : PBytes;
  Index : integer;
begin
  if Keys^=#0 then
    //Charcode is byte-sized but declared as word so it is zero-terminated
    P := @CharCode
  else
    P := pointer(Keys);

  if (P=nil) or
    ((ZApp.Time<LastPressedAt+RepeatDelay)
      //Time gets reset in designer every time app is started, validate LastPressedAt
      {$ifndef minimal}and (LastPressedAt<ZApp.Time){$endif}
     )
    then
    Exit;
  Index := 0;
  while P^[Index]<>0 do
  begin
    if Platform_IsKeyPressed(AnsiChar(P^[Index])) then
    begin
      Self.KeyIndex := Index;
      OnPressed.ExecuteCommands;
      LastPressedAt := ZApp.Time;
      Break;
    end;
    Inc(Index);
  end;
end;

{ TRefreshContent }

procedure TRefreshContent.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Component',{$ENDIF}(@Component), zptComponentRef);
end;

procedure TRefreshContent.Execute;
begin
  {$ifndef minimal}
  if not Assigned(Component) then Exit;
  {$endif}
  Component.Change;
  Component.Update;
end;

{$ifndef minimal}
function TRefreshContent.GetDisplayName: AnsiString;
begin
  Result := inherited GetDisplayName;
  if Assigned(Component) then
    Result := Result + '  ' + Component.Name;
end;
{$endif}

{ TZTimer }

procedure TZTimer.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'OnTimer',{$ENDIF}(@OnTimer), zptComponentList);
    {$ifndef minimal}{List.GetLast.SetChildClasses([TCommand,TZExpression]);}{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'Interval',{$ENDIF}(@Interval), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}'RepeatCount',{$ENDIF}(@RepeatCount), zptInteger);
    List.GetLast.DefaultValue.IntegerValue := -1;
  List.AddProperty({$IFNDEF MINIMAL}'CurrentRelativeTime',{$ENDIF}(@Time), zptFloat);
    {$ifndef minimal}List.GetLast.HideInGui := True;{$endif}
    List.GetLast.NeverPersist := True;
end;

{$ifndef minimal}
procedure TZTimer.DesignerReset;
begin
  inherited;
  CurrentIteration := 0;
  Time := 0;
  Stopped := False;
end;
{$endif}

procedure TZTimer.Execute;
begin
  if (Interval<>0) and (not Stopped) then
  begin
    Time := Time + ZApp.DeltaTime;
    if Time>=Interval then
    begin
      Time := Time - Interval;
      OnTimer.ExecuteCommands;
      if RepeatCount<>-1 then
      begin
        Inc(CurrentIteration);
        Stopped := CurrentIteration>RepeatCount;
      end;
    end;
  end;
end;


{ TWebOpen }

procedure TWebOpen.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Url',{$ENDIF}(@Url), zptString);
    List.GetLast.IsManagedTarget := True;
  List.AddProperty({$IFNDEF MINIMAL}'ResultArray',{$ENDIF}(@ResultArray), zptComponentRef);
    {$ifndef minimal}List.GetLast.SetChildClasses([TDefineArray]);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'ResultString',{$ENDIF}(@ResultString), zptString);
    List.GetLast.NeverPersist := True;
    List.GetLast.IsManagedTarget := True;
  List.AddProperty({$IFNDEF MINIMAL}'ParamArray',{$ENDIF}(@ParamArray), zptComponentRef);
    {$ifndef minimal}List.GetLast.SetChildClasses([TDefineArray]);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'InBrowser',{$ENDIF}(@InBrowser), zptBoolean);
  List.AddProperty({$IFNDEF MINIMAL}'OnResult',{$ENDIF}(@OnResult), zptComponentList);
end;

destructor TWebOpen.Destroy;
begin
  FreeMem(Buffer);
  inherited;
end;

procedure TWebOpen.Execute;
var
  Ps,Pd : PByte;
  I,Limit : integer;
  Buf : array[0..1024-1] of byte;
begin
  if not Self.IsWaiting then
  begin
    if not Self.InBrowser then
      IsWaiting := True;

    Pd := @Buf;
    ZStrCopy(@Buf,PAnsiChar(Self.Url));
    Inc(Pd,ZStrLength(@Buf));

    if ParamArray<>nil then
    begin
      {$ifndef minimal}
      if ParamArray._Type<>zctByte then
        GetLog(Self.ClassName).Warning('ParamArray must be of byte-type');
      {$endif}
      Limit := ParamArray.CalcLimit;
      Ps := PByte(ParamArray.GetData);
      I := 0;
      while (I<Limit) and (I<High(Buf)-1) and (Ps^<>0) do
      begin
        Pd^ := Ps^;
        Inc(Pd);
        Inc(Ps);
        Inc(I);
      end;
      Pd^ := 0;
    end;

    Platform_NetOpen(PAnsiChar(@Buf),Self.InBrowser,Self);
  end;
end;

class procedure TWebOpen.FlushResultList;
var
  I : integer;
begin
  Platform_EnterMutex(NetMutex);
    for I := 0 to NetResultList.Count - 1 do
    begin
      TWebOpen(NetResultList[I]).OnResult.ExecuteCommands;
    end;
    NetResultList.Clear;
  Platform_LeaveMutex(NetMutex);
end;

{$ifndef minimal}
function TWebOpen.GetDisplayName: AnsiString;
begin
  Result := inherited GetDisplayName;
  Result := Result + '  ' + Url;
end;
{$endif}

procedure TWebOpen.StartReading;
var
  Ps,Pd : PByte;
  BytesRead : integer;
begin
  if not Self.IsWaiting then
    Exit;
  Self.IsWaiting := False;

  BufferSize := 512 * 1024;

  ReAllocMem(Self.Buffer,BufferSize);
  BytesRead := Platform_NetRead(Self.Handle,Self.Buffer,Self.BufferSize);

  if BytesRead>0 then
  begin
    if ResultArray<>nil then
    begin
      {$ifndef minimal}
      if ResultArray._Type<>zctByte then
        GetLog(Self.ClassName).Warning('ResultArray must be of byte-type');
      {$endif}
      ResultArray.SizeDim1 := BytesRead;
      Ps := Self.Buffer;
      Pd := PByte(ResultArray.GetData);
      Move(Ps^,Pd^,BytesRead);
    end else
    begin
      Self.ResultString := ManagedHeap_Alloc(BytesRead+1);
      Move(Self.Buffer^,Self.ResultString^,BytesRead);
      PBytes(Self.ResultString)^[BytesRead+1] := 0; //Zero terminate
    end;
  end;
  AddToResultList;
end;

procedure TWebOpen.AddToResultList;
begin
  Platform_EnterMutex(NetMutex);
    NetResultList.Add(Self);
  Platform_LeaveMutex(NetMutex);
end;

{ TCallComponent }

procedure TCallComponent.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Component',{$ENDIF}(@Component), zptComponentRef);
    {$ifndef minimal}List.GetLast.SetChildClasses([TCommand]);{$endif}
end;

procedure TCallComponent.Execute;
begin
  if Component<>nil then
    Component.Execute;
end;

{$ifndef minimal}
function TCallComponent.GetDisplayName: AnsiString;
begin
  Result := inherited GetDisplayName;
  if Component<>nil then
    Result := Result + '  ' + Component.Name;
end;
{$endif}

initialization

  ZClasses.Register(TRepeat,RepeatClassId);
    {$ifndef minimal}ComponentManager.LastAdded.ImageIndex:=7;{$endif}
  ZClasses.Register(TCondition,ConditionClassId);
    {$ifndef minimal}ComponentManager.LastAdded.ImageIndex:=27;{$endif}
  ZClasses.Register(TKeyPress,KeyPressClassId);
    {$ifndef minimal}ComponentManager.LastAdded.NeedParentList := 'OnUpdate';{$endif}
    {$ifndef minimal}ComponentManager.LastAdded.ImageIndex:=34;{$endif}
  ZClasses.Register(TRefreshContent,RefreshContentClassId);
    {$ifndef minimal}ComponentManager.LastAdded.ImageIndex:=39;{$endif}
  ZClasses.Register(TZTimer,ZTimerClassId);
    {$ifndef minimal}ComponentManager.LastAdded.ZClassName := 'Timer';{$endif}
    {$ifndef minimal}ComponentManager.LastAdded.NeedParentList := 'OnUpdate';{$endif}
    {$ifndef minimal}ComponentManager.LastAdded.ImageIndex:=5;{$endif}
  ZClasses.Register(TWebOpen,WebOpenClassId);
    {$ifndef minimal}ComponentManager.LastAdded.ImageIndex:=37;{$endif}
  ZClasses.Register(TCallComponent,CallComponentClassId);
    {$ifndef minimal}ComponentManager.LastAdded.ImageIndex:=28;{$endif}

  NetResultList := TZArrayList.CreateReferenced;
  NetMutex := Platform_CreateMutex;
{$ifndef minimal}
finalization
  NetResultList.Free;
  Platform_FreeMutex( NetMutex );
{$endif}

end.
