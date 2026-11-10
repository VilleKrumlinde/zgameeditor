object AndroidApkForm: TAndroidApkForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Build Android APK'
  ClientHeight = 388
  ClientWidth = 252
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  DesignSize = (
    252
    388)
  PixelsPerInch = 96
  TextHeight = 13
  object VersionNumberLabel: TLabel
    Left = 16
    Top = 169
    Width = 74
    Height = 13
    Caption = 'Version number'
  end
  object Label3: TLabel
    Left = 16
    Top = 202
    Width = 28
    Height = 13
    Caption = 'Build'
  end
  object Label1: TLabel
    Left = 16
    Top = 235
    Width = 54
    Height = 13
    Caption = 'Orientation'
  end
  object Label2: TLabel
    Left = 16
    Top = 268
    Width = 81
    Height = 13
    Caption = 'Native libraries'
  end
  object PackageNameEdit: TLabeledEdit
    Left = 16
    Top = 32
    Width = 220
    Height = 21
    EditLabel.Width = 172
    EditLabel.Height = 13
    EditLabel.Caption = 'Package name (needs to be unique)'
    TabOrder = 0
  end
  object AppNameEdit: TLabeledEdit
    Left = 16
    Top = 80
    Width = 220
    Height = 21
    EditLabel.Width = 73
    EditLabel.Height = 13
    EditLabel.Caption = 'Application title'
    TabOrder = 1
  end
  object VersionNameEdit: TLabeledEdit
    Left = 16
    Top = 128
    Width = 220
    Height = 21
    EditLabel.Width = 64
    EditLabel.Height = 13
    EditLabel.Caption = 'Version name'
    TabOrder = 2
  end
  object VersionNumberEdit: TEdit
    Left = 108
    Top = 165
    Width = 128
    Height = 21
    NumbersOnly = True
    TabOrder = 3
  end
  object BuildTypeComboBox: TComboBox
    Left = 108
    Top = 198
    Width = 128
    Height = 21
    Style = csDropDownList
    ItemIndex = 0
    TabOrder = 4
    Text = 'Debug'
    Items.Strings = (
      'Debug'
      'Release')
  end
  object OrientationComboBox: TComboBox
    Left = 108
    Top = 231
    Width = 128
    Height = 21
    Style = csDropDownList
    ItemIndex = 0
    TabOrder = 5
    Text = 'Landscape'
    Items.Strings = (
      'Landscape'
      'Portrait'
      'sensorLandscape')
  end
  object Arm32CheckBox: TCheckBox
    Left = 16
    Top = 289
    Width = 220
    Height = 17
    Caption = '32-bit (armeabi-v7a)'
    Checked = True
    State = cbChecked
    TabOrder = 6
  end
  object Arm64CheckBox: TCheckBox
    Left = 16
    Top = 314
    Width = 220
    Height = 17
    Caption = '64-bit (arm64-v8a)'
    Checked = True
    State = cbChecked
    TabOrder = 7
  end
  object Button1: TButton
    Left = 78
    Top = 347
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 8
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 161
    Top = 347
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 9
  end
end
