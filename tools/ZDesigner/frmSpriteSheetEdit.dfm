inherited SpriteSheetEditFrame: TSpriteSheetEditFrame
  inherited Panel3: TPanel
    inherited InfoLabel: TLabel
      Left = 166
      Width = 89
      Hint = 'SpriteSheet'
      Caption = 'Sprite sheet editor'
      ExplicitLeft = 166
      ExplicitWidth = 89
    end
    object ImportButton: TButton
      Left = 85
      Top = 3
      Width = 75
      Height = 25
      Hint = 'Import sprite frame from DragonBones'
      Caption = 'Import'
      TabOrder = 1
      OnClick = ImportButtonClick
    end
  end
  object OpenJsonDialog: TOpenDialog
    DefaultExt = '*.json'
    Filter = 'Json (*.json)|*.json'
    Title = 'Import sprite frame from DragonBones'
    Left = 8
    Top = 140
  end
end
