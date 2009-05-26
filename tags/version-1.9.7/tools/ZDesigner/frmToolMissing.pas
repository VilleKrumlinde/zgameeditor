unit frmToolMissing;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TToolMissingForm = class(TForm)
    Button1: TButton;
    DownloadURLLabel: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    ExeNameLabel: TLabel;
    ToolPathLabel: TLabel;
    ToolNameLabel: TLabel;
    procedure DownloadURLLabelClick(Sender: TObject);
    procedure ToolPathLabelMouseEnter(Sender: TObject);
    procedure ToolPathLabelMouseLeave(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ToolMissingForm: TToolMissingForm;

implementation

{$R *.dfm}

uses ShellApi,uHelp;

procedure TToolMissingForm.DownloadURLLabelClick(Sender: TObject);
begin
  GoUrl( (Sender as TLabel).Caption );
end;

procedure TToolMissingForm.ToolPathLabelMouseEnter(Sender: TObject);
begin
  (Sender as TLabel).Font.Color := clBlue;
end;

procedure TToolMissingForm.ToolPathLabelMouseLeave(Sender: TObject);
begin
  (Sender as TLabel).Font.Color := clBlack;
end;

end.
