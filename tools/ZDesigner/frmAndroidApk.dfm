object AndroidApkForm: TAndroidApkForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Build Android APK'
  ClientHeight = 334
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
    334)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 219
    Width = 54
    Height = 13
    Caption = 'Orientation'
  end
  object Label2: TLabel
    Left = 16
    Top = 251
    Width = 75
    Height = 13
    Caption = 'Android version'
  end
  object PackageNameEdit: TLabeledEdit
    Left = 16
    Top = 32
    Width = 217
    Height = 21
    EditLabel.Width = 172
    EditLabel.Height = 13
    EditLabel.Caption = 'Package name (needs to be unique)'
    TabOrder = 0
  end
  object AppNameEdit: TLabeledEdit
    Left = 16
    Top = 80
    Width = 217
    Height = 21
    EditLabel.Width = 73
    EditLabel.Height = 13
    EditLabel.Caption = 'Application title'
    TabOrder = 1
  end
  object Button1: TButton
    Left = 86
    Top = 301
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 2
    ExplicitTop = 277
  end
  object Button2: TButton
    Left = 169
    Top = 301
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
    ExplicitTop = 277
  end
  object VersionNameEdit: TLabeledEdit
    Left = 16
    Top = 128
    Width = 217
    Height = 21
    EditLabel.Width = 64
    EditLabel.Height = 13
    EditLabel.Caption = 'Version name'
    TabOrder = 4
  end
  object VersionNumberEdit: TLabeledEdit
    Left = 16
    Top = 176
    Width = 73
    Height = 21
    EditLabel.Width = 74
    EditLabel.Height = 13
    EditLabel.Caption = 'Version number'
    NumbersOnly = True
    TabOrder = 5
  end
  object OrientationComboBox: TComboBox
    Left = 96
    Top = 216
    Width = 105
    Height = 21
    Style = csDropDownList
    ItemIndex = 0
    TabOrder = 6
    Text = 'Landscape'
    Items.Strings = (
      'Landscape'
      'Portrait')
  end
  object AndroidVersionComboBox: TComboBox
    Left = 96
    Top = 248
    Width = 148
    Height = 21
    Style = csDropDownList
    TabOrder = 7
  end
end
