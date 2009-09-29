object SelectComponentForm: TSelectComponentForm
  Left = 787
  Top = 88
  Caption = 'Select component to add'
  ClientHeight = 481
  ClientWidth = 785
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  ShowHint = True
  OnCreate = FormCreate
  DesignSize = (
    785
    481)
  PixelsPerInch = 96
  TextHeight = 13
  object CompListView: TListView
    Left = 8
    Top = 8
    Width = 768
    Height = 427
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <>
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    SmallImages = CommonModule.CompIconsImageList
    SortType = stText
    TabOrder = 0
    ViewStyle = vsList
    OnCustomDrawItem = CompListViewCustomDrawItem
    OnDblClick = CompListViewDblClick
    OnInfoTip = CompListViewInfoTip
    OnSelectItem = CompListViewSelectItem
  end
  object OkButton: TButton
    Left = 618
    Top = 443
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    Enabled = False
    ModalResult = 1
    TabOrder = 1
  end
  object Button2: TButton
    Left = 701
    Top = 443
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object HelpButton: TButton
    Left = 8
    Top = 443
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Help'
    TabOrder = 3
    OnClick = HelpButtonClick
  end
end
