unit frmAndroidApk;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TAndroidApkForm = class(TForm)
    PackageNameEdit: TLabeledEdit;
    AppNameEdit: TLabeledEdit;
    Button1: TButton;
    Button2: TButton;
    VersionNameEdit: TLabeledEdit;
    VersionNumberEdit: TEdit;
    VersionNumberLabel: TLabel;
    OrientationComboBox: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    BuildTypeComboBox: TComboBox;
    Arm32CheckBox: TCheckBox;
    Arm64CheckBox: TCheckBox;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AndroidApkForm: TAndroidApkForm;

implementation

{$R *.dfm}

procedure TAndroidApkForm.Button1Click(Sender: TObject);
begin
  if (not Arm32CheckBox.Checked) and (not Arm64CheckBox.Checked) then
  begin
    ModalResult := mrNone;
    ShowMessage('Select at least one native library (32-bit or 64-bit).');
  end;
end;

end.
