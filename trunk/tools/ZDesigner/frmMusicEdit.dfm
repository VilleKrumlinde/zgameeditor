inherited MusicEditFrame: TMusicEditFrame
  Width = 886
  Height = 586
  ExplicitWidth = 886
  ExplicitHeight = 586
  object PlayButton: TButton
    Left = 24
    Top = 24
    Width = 75
    Height = 25
    Caption = '&Play music'
    TabOrder = 0
    OnClick = PlayButtonClick
  end
  object Memo1: TMemo
    Left = 24
    Top = 80
    Width = 465
    Height = 209
    Lines.Strings = (
      
        'To import a midi-file click on the "Import" button next to the M' +
        'usicFile property in the property '
      'editor to the left.'
      ''
      
        'For playing music in your application use the MusicControl compo' +
        'nent.'
      ''
      'Note that midi-import is still in development:'
      '- percussions are not imported'
      
        '- general midi (GM) instruments are recognized but they currentl' +
        'y all sound the same')
    ReadOnly = True
    TabOrder = 1
  end
end
