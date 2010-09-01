object LfoFrame: TLfoFrame
  Left = 0
  Top = 0
  Width = 529
  Height = 24
  TabOrder = 0
  DesignSize = (
    529
    24)
  object Label1: TLabel
    Left = 232
    Top = 5
    Width = 31
    Height = 13
    Caption = 'Speed'
  end
  object NrLabel: TLabel
    Left = 8
    Top = 5
    Width = 6
    Height = 13
    Caption = '1'
  end
  object ActiveCheckBox: TCheckBox
    Left = 24
    Top = 3
    Width = 57
    Height = 17
    Caption = 'Active'
    TabOrder = 0
    OnClick = ActiveCheckBoxClick
  end
  object StyleComboBox: TComboBox
    Left = 88
    Top = 1
    Width = 73
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 1
    OnChange = StyleComboBoxChange
  end
  object SpeedTrackBar: TTrackBar
    Left = 272
    Top = 0
    Width = 239
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
    OnChange = SpeedTrackBarChange
  end
  object BipolarCheckBox: TCheckBox
    Left = 168
    Top = 3
    Width = 57
    Height = 17
    Caption = 'Bipolar'
    TabOrder = 3
    OnClick = BipolarCheckBoxClick
  end
end
