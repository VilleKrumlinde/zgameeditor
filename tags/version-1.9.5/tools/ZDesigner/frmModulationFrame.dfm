object ModulationFrame: TModulationFrame
  Left = 0
  Top = 0
  Width = 513
  Height = 24
  TabOrder = 0
  DesignSize = (
    513
    24)
  object Label1: TLabel
    Left = 261
    Top = 6
    Width = 36
    Height = 13
    Caption = 'Amount'
  end
  object NrLabel: TLabel
    Left = 8
    Top = 6
    Width = 6
    Height = 13
    Caption = '1'
  end
  object DestComboBox: TComboBox
    Left = 159
    Top = 2
    Width = 97
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 1
    OnChange = DestComboBoxChange
  end
  object AmountTrackBar: TTrackBar
    Left = 301
    Top = 1
    Width = 209
    Height = 23
    Anchors = [akLeft, akTop, akRight]
    Max = 100
    Orientation = trHorizontal
    Frequency = 1
    Position = 0
    SelEnd = 0
    SelStart = 0
    TabOrder = 2
    TickMarks = tmBottomRight
    TickStyle = tsNone
    OnChange = AmountTrackBarChange
  end
  object ActiveCheckBox: TCheckBox
    Left = 23
    Top = 4
    Width = 57
    Height = 17
    Caption = 'Active'
    TabOrder = 3
    OnClick = ActiveCheckBoxClick
  end
  object SourceComboBox: TComboBox
    Left = 79
    Top = 2
    Width = 73
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
    OnChange = SourceComboBoxChange
  end
end
