object ArrayEditForm: TArrayEditForm
  Left = 0
  Top = 0
  Caption = 'Edit array values'
  ClientHeight = 286
  ClientWidth = 426
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  DesignSize = (
    426
    286)
  PixelsPerInch = 96
  TextHeight = 13
  object OkButton: TButton
    Left = 343
    Top = 253
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    TabOrder = 0
    OnClick = OkButtonClick
  end
  object Grid: TStringGrid
    Left = 16
    Top = 16
    Width = 393
    Height = 217
    Anchors = [akLeft, akTop, akRight, akBottom]
    RowCount = 2
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goTabs, goAlwaysShowEditor]
    TabOrder = 1
    OnSetEditText = GridSetEditText
  end
  object UpDown1: TUpDown
    Left = 57
    Top = 246
    Width = 24
    Height = 21
    Anchors = [akLeft, akBottom]
    Associate = Dim3Edit
    Orientation = udHorizontal
    TabOrder = 2
    OnChangingEx = UpDown1ChangingEx
  end
  object Dim3Edit: TEdit
    Left = 16
    Top = 246
    Width = 41
    Height = 21
    Anchors = [akLeft, akBottom]
    ReadOnly = True
    TabOrder = 3
    Text = '0'
  end
  object CopyAllButton: TButton
    Left = 143
    Top = 253
    Width = 86
    Height = 25
    Hint = 'Copy all values as comma-seperated text to clipboard'
    Anchors = [akRight, akBottom]
    Caption = 'Copy all values'
    TabOrder = 4
    OnClick = CopyAllButtonClick
  end
  object PasteAllButton: TButton
    Left = 235
    Top = 253
    Width = 86
    Height = 25
    Hint = 'Paste all values from clipboard'
    Anchors = [akRight, akBottom]
    Caption = 'Paste all values'
    TabOrder = 5
    OnClick = PasteAllButtonClick
  end
end
