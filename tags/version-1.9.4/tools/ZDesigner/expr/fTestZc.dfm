object fmTestZc: TfmTestZc
  Left = 141
  Top = 134
  ActiveControl = memSource
  Caption = 'Test Zc'
  ClientHeight = 341
  ClientWidth = 536
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object splSourceOutput: TSplitter
    Left = 0
    Top = 174
    Width = 536
    Height = 6
    Cursor = crVSplit
    Align = alBottom
    Beveled = True
    ExplicitTop = 170
  end
  object pnlButtons: TPanel
    Left = 0
    Top = 0
    Width = 536
    Height = 29
    Align = alTop
    TabOrder = 0
    object btnOpen: TButton
      Left = 4
      Top = 2
      Width = 75
      Height = 25
      Hint = 'Load a source file'
      Caption = '&Open'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      OnClick = btnOpenClick
    end
    object btnExecute: TButton
      Left = 161
      Top = 2
      Width = 75
      Height = 25
      Hint = 'Parse the source file'
      Caption = '&Execute'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = btnExecuteClick
    end
    object btnSave: TButton
      Left = 83
      Top = 2
      Width = 75
      Height = 25
      Hint = 'Save this source file'
      Caption = '&Save'
      Enabled = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      OnClick = btnSaveClick
    end
  end
  object pnlSource: TPanel
    Left = 0
    Top = 29
    Width = 536
    Height = 145
    Align = alClient
    TabOrder = 1
    ExplicitHeight = 141
    object memSource: TMemo
      Left = 1
      Top = 1
      Width = 534
      Height = 139
      Align = alClient
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Courier New'
      Font.Style = []
      ParentFont = False
      ScrollBars = ssBoth
      TabOrder = 0
      WordWrap = False
      OnChange = memSourceChange
    end
  end
  object pnlOutput: TPanel
    Left = 0
    Top = 180
    Width = 536
    Height = 161
    Align = alBottom
    TabOrder = 2
    ExplicitTop = 176
    object memOutput: TMemo
      Left = 1
      Top = 1
      Width = 534
      Height = 159
      Align = alClient
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Courier New'
      Font.Style = []
      ParentFont = False
      ScrollBars = ssBoth
      TabOrder = 0
      WordWrap = False
    end
  end
  object OpenDialog: TOpenDialog
    Left = 100
    Top = 50
  end
  object SaveDialog: TSaveDialog
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 200
    Top = 50
  end
end
