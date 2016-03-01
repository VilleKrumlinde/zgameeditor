inherited ScriptedCompEditFrameBase: TScriptedCompEditFrameBase
  object Panel3: TPanel
    Left = 0
    Top = 208
    Width = 320
    Height = 32
    Align = alBottom
    Alignment = taLeftJustify
    BevelOuter = bvNone
    TabOrder = 0
    object InfoLabel: TLabel
      Left = 84
      Top = 9
      Width = 45
      Height = 13
      Caption = 'InfoLabel'
    end
    object HelpButton: TButton
      Left = 4
      Top = 3
      Width = 75
      Height = 25
      Caption = 'Help'
      TabOrder = 0
      OnClick = HelpButtonClick
    end
  end
end
