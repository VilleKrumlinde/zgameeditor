object ArrayEditForm: TArrayEditForm
  Left = 0
  Top = 0
  Caption = 'Edit array values'
  ClientHeight = 286
  ClientWidth = 426
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    426
    286)
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 343
    Top = 253
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 0
  end
  object Grid: TStringGrid
    Left = 16
    Top = 16
    Width = 393
    Height = 217
    Anchors = [akLeft, akTop, akRight, akBottom]
    RowCount = 2
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goTabs, goAlwaysShowEditor]
    TabOrder = 1
    OnSetEditText = GridSetEditText
  end
end
