object EnvelopeFrame: TEnvelopeFrame
  Left = 0
  Top = 0
  Width = 635
  Height = 27
  TabOrder = 0
  object NrLabel: TLabel
    Left = 8
    Top = 4
    Width = 6
    Height = 13
    Caption = '1'
  end
  object Label2: TLabel
    Left = 87
    Top = 4
    Width = 7
    Height = 13
    Caption = 'A'
  end
  object Label4: TLabel
    Left = 450
    Top = 4
    Width = 7
    Height = 13
    Caption = 'R'
  end
  object Label1: TLabel
    Left = 215
    Top = 4
    Width = 7
    Height = 13
    Caption = 'D'
  end
  object Label3: TLabel
    Left = 351
    Top = 4
    Width = 6
    Height = 13
    Caption = 'S'
  end
  object Image1: TImage
    Left = 582
    Top = 2
    Width = 45
    Height = 20
  end
  object ActiveCheckBox: TCheckBox
    Left = 24
    Top = 2
    Width = 57
    Height = 17
    Caption = 'Active'
    TabOrder = 0
    OnClick = ActiveCheckBoxClick
  end
  object AttackTrackBar: TTrackBar
    Left = 96
    Top = 0
    Width = 117
    Height = 23
    Max = 250
    TabOrder = 1
    TickStyle = tsNone
    OnContextPopup = TrackBarContextPopup
    OnChange = AttackTrackBarChange
  end
  object ReleaseTrackBar: TTrackBar
    Left = 459
    Top = 0
    Width = 117
    Height = 23
    Max = 250
    TabOrder = 2
    TickStyle = tsNone
    OnContextPopup = TrackBarContextPopup
    OnChange = ReleaseTrackBarChange
  end
  object DecayTrackBar: TTrackBar
    Left = 228
    Top = 0
    Width = 117
    Height = 23
    Max = 250
    TabOrder = 3
    TickStyle = tsNone
    OnContextPopup = TrackBarContextPopup
    OnChange = DecayTrackBarChange
  end
  object SustainTrackBar: TTrackBar
    Left = 360
    Top = 0
    Width = 81
    Height = 23
    Max = 250
    TabOrder = 4
    TickStyle = tsNone
    OnContextPopup = TrackBarContextPopup
    OnChange = SustainTrackBarChange
  end
end
