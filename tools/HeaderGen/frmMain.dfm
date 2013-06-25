object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'OpenGL headers generator'
  ClientHeight = 489
  ClientWidth = 766
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    766
    489)
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 383
    Top = 8
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Generate'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 8
    Top = 8
    Width = 369
    Height = 473
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'Memo1')
    ScrollBars = ssVertical
    TabOrder = 1
    WordWrap = False
  end
end
