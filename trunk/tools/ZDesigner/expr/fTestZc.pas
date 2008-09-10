unit fTestZc;

interface

uses
  Classes,
  ComCtrls,
  Forms,
  Buttons,
  ExtCtrls,
  StdCtrls,
  Dialogs,
  Controls,
  SysUtils,
  Zc;

type
  TfmTestZc = class(TForm)
    pnlButtons: TPanel;
    pnlSource: TPanel;
    pnlOutput: TPanel;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    memSource: TMemo;
    memOutput: TMemo;
    splSourceOutput: TSplitter;
    btnOpen: TButton;
    btnSave: TButton;
    btnExecute: TButton;
    procedure btnExecuteClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure memSourceChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  end;

var
  fmTestZc: TfmTestZc;

implementation

{$R *.DFM}

procedure TfmTestZc.btnOpenClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    memSource.Clear;
    memSource.Lines.LoadFromFile(OpenDialog.FileName);
  end;
end;

procedure TfmTestZc.btnSaveClick(Sender: TObject);
begin
  if SaveDialog.Execute then
    memSource.Lines.SaveToFile(SaveDialog.FileName);
end;

procedure TfmTestZc.btnExecuteClick(Sender: TObject);
var
  Zc : TZc;
begin
  Zc := TZc.Create(nil);
  memSource.Lines.SaveToStream(Zc.SourceStream);
  Zc.Execute;
  memOutput.Clear;
  memOutput.Lines.LoadFromStream(Zc.ListStream);
  Zc.Free;
end;                                

procedure TfmTestZc.memSourceChange(Sender: TObject);
begin
  btnSave.Enabled := memSource.Text > '';
end;

procedure TfmTestZc.FormCreate(Sender: TObject);
begin
  DecimalSeparator := '.';
end;

end.
