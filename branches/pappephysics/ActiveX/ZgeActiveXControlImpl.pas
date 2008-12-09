unit ZgeActiveXControlImpl;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ActiveX, AxCtrls, ZgeActiveX_TLB, StdVcl, StdCtrls;

type
  TZgeActiveXControl = class(TActiveForm, IZgeActiveXControl)
  private
    { Private declarations }
    procedure ActivateEvent(Sender: TObject);
    procedure ClickEvent(Sender: TObject);
    procedure CreateEvent(Sender: TObject);
    procedure DblClickEvent(Sender: TObject);
    procedure DeactivateEvent(Sender: TObject);
    procedure DestroyEvent(Sender: TObject);
    procedure KeyPressEvent(Sender: TObject; var Key: Char);
    procedure MouseEnterEvent(Sender: TObject);
    procedure MouseLeaveEvent(Sender: TObject);
    procedure PaintEvent(Sender: TObject);
    procedure InitZge;
    procedure CloseZge;
  protected
    { Protected declarations }
    procedure DefinePropertyPages(DefinePropertyPage: TDefinePropertyPage); override;
    procedure EventSinkChanged(const EventSink: IUnknown); override;
  public
    { Public declarations }
    procedure Initialize; override;
  end;

implementation

uses ComObj, ComServ, ZApplication, ZPlatform;

{$R *.DFM}

var
  ZgePlayerThreadHandle : THandle;
  ZgePlayerThreadId : DWORD;


function ZgePlayerThread_Execute(Parameter: Pointer): Integer; stdcall;
begin
  ZApplication.ZApp := ZApplication.LoadApplicationComponent;
  try
    ZApp.Run;
    ZApp.Terminate;
  finally
    ZApp.Free;
    ZApp := nil;
  end;
  Result := 0;
end;

{ TZgeActiveXControl }

procedure TZgeActiveXControl.DefinePropertyPages(DefinePropertyPage: TDefinePropertyPage);
begin
end;

procedure TZgeActiveXControl.EventSinkChanged(const EventSink: IUnknown);
begin
  inherited EventSinkChanged(EventSink);
end;

procedure TZgeActiveXControl.Initialize;
begin
  inherited Initialize;
  OnActivate := ActivateEvent;
  OnClick := ClickEvent;
  OnCreate := CreateEvent;
  OnDblClick := DblClickEvent;
  OnDeactivate := DeactivateEvent;
  OnDestroy := DestroyEvent;
  OnKeyPress := KeyPressEvent;
  OnMouseEnter := MouseEnterEvent;
  OnMouseLeave := MouseLeaveEvent;
  OnPaint := PaintEvent;

  //todo: make window resizable
  Height := 600;
  Width := 800;

  InitZGE;
end;

procedure TZgeActiveXControl.InitZge;
begin
  if ZgePlayerThreadHandle<>0 then
    Exit;
  ZPlatform.PreviewParentHWnd := Self.Handle;
  ZPlatform.ZgeActiveXFinished := False; //todo: synchronze
  //Create and start thread
  ZgePlayerThreadHandle := CreateThread(nil, 0, @ZgePlayerThread_Execute, nil, 0, ZgePlayerThreadID);
  SetThreadPriority(ZgePlayerThreadHandle,THREAD_PRIORITY_ABOVE_NORMAL);
end;

procedure TZgeActiveXControl.CloseZge;
begin
  ZPlatform.ZgeActiveXFinished := True;  //todo: synchronize
  if ZgePlayerThreadHandle<>0 then
  begin
    WaitForSingleObject(ZgePlayerThreadHandle, 1000);
    CloseHandle(ZgePlayerThreadHandle);
    ZgePlayerThreadHandle := 0;
  end;
end;


procedure TZgeActiveXControl.ActivateEvent(Sender: TObject);
begin
end;


procedure TZgeActiveXControl.ClickEvent(Sender: TObject);
begin
end;

procedure TZgeActiveXControl.CreateEvent(Sender: TObject);
begin
end;

procedure TZgeActiveXControl.DblClickEvent(Sender: TObject);
begin
end;

procedure TZgeActiveXControl.DeactivateEvent(Sender: TObject);
begin
end;

procedure TZgeActiveXControl.DestroyEvent(Sender: TObject);
begin
//  PostMessage(Self.HAndle,WM_QUIT,0,0);  //zgeexit
  CloseZge;
end;

procedure TZgeActiveXControl.KeyPressEvent(Sender: TObject; var Key: Char);
begin
end;

procedure TZgeActiveXControl.MouseEnterEvent(Sender: TObject);
begin
end;

procedure TZgeActiveXControl.MouseLeaveEvent(Sender: TObject);
begin
end;

procedure TZgeActiveXControl.PaintEvent(Sender: TObject);
begin
end;


initialization
  TActiveFormFactory.Create(
    ComServer,
    TActiveFormControl,
    TZgeActiveXControl,
    Class_ZgeActiveXControl,
    1,
    '',
    OLEMISC_SIMPLEFRAME or OLEMISC_ACTSLIKELABEL,
    tmSingle);
end.
