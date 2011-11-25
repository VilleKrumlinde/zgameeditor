unit frmSettings;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls;

type
  TSettingsForm = class(TForm)
    OkButton: TButton;
    Button2: TButton;
    PackerEdit: TEdit;
    PackerParamsEdit: TEdit;
    PackerPresetCombo: TComboBox;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    GroupBox2: TGroupBox;
    ShellCheck: TCheckBox;
    GroupBox3: TGroupBox;
    Label6: TLabel;
    GuiLayoutCombo: TComboBox;
    GroupBox4: TGroupBox;
    UpDown1: TUpDown;
    CompDelayEdit: TEdit;
    Label7: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ShellCheckClick(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
    procedure PackerPresetComboChange(Sender: TObject);
  private
    { Private declarations }
    ShellChanged : boolean;
    procedure ReadSettings;
    procedure SaveSettings;
  public
    { Public declarations }
  end;

implementation

uses frmEditor, Registry;

{$R *.DFM}

const
  ZgeRegName = 'ZgeProjFile';

procedure RegZgeExt;
begin
  with TRegIniFile.Create('') do
    try
      RootKey := HKEY_CURRENT_USER;
      OpenKey('Software\Classes',True);
      WriteString(ZgeProjExtension, '', ZgeRegName);
      WriteString(ZgeRegName, '', 'ZGameEditor Project');
      WriteString(ZgeRegName + '\DefaultIcon', '', Application.ExeName + ',0');
//      WriteString(ZgeRegName + '\Shell', '', 'This_Is_Our_Default_Action');
//      WriteString(ZgeRegName + '\Shell\open', '','This is our first action');
      WriteString(ZgeRegName + '\Shell\open\command', '', '"' + Application.ExeName + '" "%1"');
   finally
      Free;
   end;
end;

procedure UnRegZgeExt;
begin
  with TRegIniFile.Create('') do
    try
      RootKey := HKEY_CURRENT_USER;
      OpenKey('Software\Classes',True);
      EraseSection(ZgeProjExtension);
      EraseSection(ZgeRegName);
   finally
      Free;
   end;
end;

function IsRegZgeExt : boolean;
begin
  with TRegIniFile.Create('') do
    try
      RootKey := HKEY_CURRENT_USER;
      OpenKey('Software\Classes',True);
      Result := KeyExists(ZgeProjExtension);
   finally
      Free;
   end;
end;



procedure TSettingsForm.FormCreate(Sender: TObject);
begin
  ShellChanged := False;
  ReadSettings;
end;

procedure TSettingsForm.ShellCheckClick(Sender: TObject);
begin
  ShellChanged := True;
end;

procedure TSettingsForm.OkButtonClick(Sender: TObject);
begin
  SaveSettings;
  ModalResult := mrOK;
end;

procedure TSettingsForm.PackerPresetComboChange(Sender: TObject);
type
  PPreset = ^TPreset;
  TPreset =
    record
      P,A : string;
    end;
const
  Presets : array[0..5] of TPreset = (
(P:'{$toolpath}upx.exe'; A:'{$exename}'),
(P:'{$toolpath}upx.exe'; A:'--lzma {$exename}'),
(P:'{$toolpath}upx.exe'; A:'--best {$exename}'),
(P:'{$toolpath}upx.exe'; A:'--brute {$exename}'),
(P:'{$toolpath}kkrunchy.exe'; A:'{$exename}'),
(P:'{$toolpath}kkrunchy.exe'; A:'--best {$exename}')
//(P:'{$toolpath}apk2.exe'; A:'{$exename}')
);
begin
  PackerEdit.Text := PPreset( @Presets[TComboBox(Sender).ItemIndex] )^.P;
  PackerParamsEdit.Text := PPreset( @Presets[TComboBox(Sender).ItemIndex] )^.A;
end;

procedure TSettingsForm.ReadSettings;
begin
  ShellCheck.Checked := IsRegZgeExt;
  ShellChanged := False;
end;

procedure TSettingsForm.SaveSettings;
begin
  if ShellChanged then
  begin
    if ShellCheck.Checked then
      RegZgeExt
    else
      UnRegZgeExt;
  end;
end;

end.
