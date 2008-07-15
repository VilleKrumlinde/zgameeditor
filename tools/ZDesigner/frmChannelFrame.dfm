object ChannelFrame: TChannelFrame
  Left = 0
  Top = 0
  Width = 131
  Height = 244
  TabOrder = 0
  DesignSize = (
    131
    244)
  object NrLabel: TLabel
    Left = 0
    Top = 8
    Width = 131
    Height = 13
    Alignment = taCenter
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = '1'
  end
  object VolumeTrackBar: TTrackBar
    Left = 6
    Top = 40
    Width = 29
    Height = 121
    Hint = 'Volume'
    Max = 100
    Orientation = trVertical
    Frequency = 1
    Position = 0
    SelEnd = 0
    SelStart = 0
    TabOrder = 0
    TickMarks = tmBottomRight
    TickStyle = tsNone
    OnChange = VolumeTrackBarChange
  end
  object ActiveCheckBox: TCheckBox
    Left = 9
    Top = 26
    Width = 15
    Height = 17
    Hint = 'Active'
    TabOrder = 1
    OnClick = ActiveCheckBoxClick
  end
  object DelayActiveCheckBox: TCheckBox
    Left = 10
    Top = 159
    Width = 17
    Height = 17
    Hint = 'Delay active'
    TabOrder = 2
    OnClick = DelayActiveCheckBoxClick
  end
  object DelayLengthTrackBar: TTrackBar
    Left = 6
    Top = 175
    Width = 29
    Height = 65
    Hint = 'Delay length'
    Max = 100
    Orientation = trVertical
    Frequency = 1
    Position = 0
    SelEnd = 0
    SelStart = 0
    TabOrder = 3
    TickMarks = tmBottomRight
    TickStyle = tsNone
    OnChange = DelayLengthTrackBarChange
  end
end
