unit frmRawAudioImportOptions;

interface

uses
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TImportRawAudioForm = class(TForm)
    sampleFormatListBox: TListBox;
    sampleRateComboBox: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Button1: TButton;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ImportRawAudioForm: TImportRawAudioForm;

implementation

{$R *.dfm}

procedure TImportRawAudioForm.FormCreate(Sender: TObject);
begin
  sampleFormatListBox.ItemIndex := 0;
end;

end.
