inherited FloatPropEditForm: TFloatPropEditForm
  Caption = 'FloatPropEditForm'
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel [0]
    Left = 9
    Top = 9
    Width = 47
    Height = 13
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = '&Edit value'
    FocusControl = TrackBar1
  end
  inherited DetachButton: TButton
    ExplicitTop = 234
  end
  object TrackBar1: TTrackBar
    Left = 0
    Top = 30
    Width = 558
    Height = 34
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akLeft, akTop, akRight]
    Max = 1000
    TabOrder = 1
    TickStyle = tsNone
    OnChange = TrackBar1Change
  end
end
