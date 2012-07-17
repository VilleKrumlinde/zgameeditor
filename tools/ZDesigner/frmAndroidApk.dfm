object AndroidApkForm: TAndroidApkForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Build Android APK'
  ClientHeight = 181
  ClientWidth = 287
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  DesignSize = (
    287
    181)
  PixelsPerInch = 96
  TextHeight = 13
  object PackageNameEdit: TLabeledEdit
    Left = 16
    Top = 32
    Width = 217
    Height = 21
    EditLabel.Width = 69
    EditLabel.Height = 13
    EditLabel.Caption = 'Package name'
    TabOrder = 0
  end
  object AppNameEdit: TLabeledEdit
    Left = 16
    Top = 88
    Width = 217
    Height = 21
    EditLabel.Width = 73
    EditLabel.Height = 13
    EditLabel.Caption = 'Application title'
    TabOrder = 1
  end
  object Button1: TButton
    Left = 121
    Top = 148
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 2
    ExplicitLeft = 360
    ExplicitTop = 143
  end
  object Button2: TButton
    Left = 204
    Top = 148
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
    ExplicitLeft = 443
    ExplicitTop = 143
  end
end
