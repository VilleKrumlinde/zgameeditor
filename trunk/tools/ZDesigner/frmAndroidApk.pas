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
    VersionNumberEdit: TLabeledEdit;
    OrientationComboBox: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    AndroidVersionComboBox: TComboBox;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AndroidApkForm: TAndroidApkForm;

implementation

{$R *.dfm}

end.
