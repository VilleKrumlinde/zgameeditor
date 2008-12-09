object ToolMissingForm: TToolMissingForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'External tool needed'
  ClientHeight = 230
  ClientWidth = 502
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  DesignSize = (
    502
    230)
  PixelsPerInch = 96
  TextHeight = 13
  object DownloadURLLabel: TLabel
    Left = 48
    Top = 209
    Width = 107
    Height = 13
    Caption = 'DownloadURLLabel'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clActiveCaption
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    Visible = False
    OnClick = DownloadURLLabelClick
  end
  object Label1: TLabel
    Left = 24
    Top = 24
    Width = 104
    Height = 13
    Caption = 'External tool needed:'
  end
  object Label2: TLabel
    Left = 24
    Top = 177
    Width = 151
    Height = 13
    Caption = 'Please download from this URL:'
    Visible = False
  end
  object Label3: TLabel
    Left = 24
    Top = 56
    Width = 56
    Height = 13
    Caption = 'Put the file:'
  end
  object Label4: TLabel
    Left = 24
    Top = 88
    Width = 64
    Height = 13
    Caption = 'In the folder:'
  end
  object ExeNameLabel: TLabel
    Left = 120
    Top = 56
    Width = 52
    Height = 13
    Caption = 'ExeName'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object ToolPathLabel: TLabel
    Left = 120
    Top = 88
    Width = 374
    Height = 25
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 'ToolPathLabel'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    WordWrap = True
    OnClick = DownloadURLLabelClick
    OnMouseEnter = ToolPathLabelMouseEnter
    OnMouseLeave = ToolPathLabelMouseLeave
  end
  object ToolNameLabel: TLabel
    Left = 144
    Top = 24
    Width = 86
    Height = 13
    Caption = 'ToolNameLabel'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Button1: TButton
    Left = 419
    Top = 197
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Close'
    ModalResult = 1
    TabOrder = 0
  end
end
