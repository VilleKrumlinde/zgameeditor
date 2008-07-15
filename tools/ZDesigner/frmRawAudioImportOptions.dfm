object ImportRawAudioForm: TImportRawAudioForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Import raw audio'
  ClientHeight = 167
  ClientWidth = 310
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  DesignSize = (
    310
    167)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 84
    Height = 13
    Caption = 'Sample rate (Hz):'
  end
  object Label2: TLabel
    Left = 120
    Top = 8
    Width = 73
    Height = 13
    Caption = 'Sample format:'
  end
  object sampleFormatListBox: TListBox
    Left = 120
    Top = 27
    Width = 169
    Height = 49
    ItemHeight = 13
    Items.Strings = (
      '8 bit signed mono'
      '16 bit signed little endian mono')
    TabOrder = 0
  end
  object sampleRateComboBox: TComboBox
    Left = 8
    Top = 27
    Width = 89
    Height = 21
    ItemHeight = 13
    TabOrder = 1
    Text = '8000'
    Items.Strings = (
      '8000'
      '8363'
      '16000'
      '22050'
      '44100')
  end
  object Button1: TButton
    Left = 146
    Top = 134
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
    ExplicitLeft = 264
    ExplicitTop = 185
  end
  object Button2: TButton
    Left = 227
    Top = 134
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
    ExplicitLeft = 345
    ExplicitTop = 185
  end
end
