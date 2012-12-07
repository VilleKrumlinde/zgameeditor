object ArrayEditForm: TArrayEditForm
  Left = 0
  Top = 0
  Caption = 'Edit array values'
  ClientHeight = 374
  ClientWidth = 557
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  DesignSize = (
    557
    374)
  PixelsPerInch = 120
  TextHeight = 17
  object OkButton: TButton
    Left = 449
    Top = 331
    Width = 98
    Height = 33
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    TabOrder = 0
    OnClick = OkButtonClick
  end
  object Grid: TStringGrid
    Left = 21
    Top = 21
    Width = 514
    Height = 284
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akLeft, akTop, akRight, akBottom]
    RowCount = 2
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goTabs, goAlwaysShowEditor]
    TabOrder = 1
    OnSetEditText = GridSetEditText
  end
  object UpDown1: TUpDown
    Left = 75
    Top = 322
    Width = 31
    Height = 25
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akLeft, akBottom]
    Associate = Dim3Edit
    Orientation = udHorizontal
    TabOrder = 2
    OnChangingEx = UpDown1ChangingEx
  end
  object Dim3Edit: TEdit
    Left = 21
    Top = 322
    Width = 54
    Height = 25
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akLeft, akBottom]
    ReadOnly = True
    TabOrder = 3
    Text = '0'
  end
  object CopyAllButton: TButton
    Left = 187
    Top = 331
    Width = 112
    Height = 33
    Hint = 'Copy all values as comma-seperated text to clipboard'
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akRight, akBottom]
    Caption = 'Copy all values'
    TabOrder = 4
    OnClick = CopyAllButtonClick
  end
  object PasteAllButton: TButton
    Left = 307
    Top = 331
    Width = 113
    Height = 33
    Hint = 'Paste all values from clipboard'
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akRight, akBottom]
    Caption = 'Paste all values'
    TabOrder = 5
    OnClick = PasteAllButtonClick
  end
end
