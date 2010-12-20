object ChannelFrame: TChannelFrame
  Left = 0
  Top = 0
  Width = 26
  Height = 244
  TabOrder = 0
  DesignSize = (
    26
    244)
  object NrLabel: TLabel
    Left = 0
    Top = 8
    Width = 26
    Height = 13
    Alignment = taCenter
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = '1'
    ExplicitWidth = 131
  end
  object VolumeTrackBar: TTrackBar
    Left = 4
    Top = 40
    Width = 29
    Height = 121
    Hint = 'Volume'
    Max = 100
    Orientation = trVertical
    TabOrder = 0
    TickStyle = tsNone
    OnChange = VolumeTrackBarChange
  end
  object ActiveCheckBox: TCheckBox
    Left = 7
    Top = 26
    Width = 15
    Height = 17
    Hint = 'Active'
    TabOrder = 1
    OnClick = ActiveCheckBoxClick
  end
  object DelayActiveCheckBox: TCheckBox
    Left = 8
    Top = 159
    Width = 17
    Height = 17
    Hint = 'Delay active'
    TabOrder = 2
    OnClick = DelayActiveCheckBoxClick
  end
  object DelayLengthTrackBar: TTrackBar
    Left = 4
    Top = 177
    Width = 29
    Height = 65
    Hint = 'Delay length'
    Max = 100
    Orientation = trVertical
    TabOrder = 3
    TickStyle = tsNone
    OnChange = DelayLengthTrackBarChange
  end
end
