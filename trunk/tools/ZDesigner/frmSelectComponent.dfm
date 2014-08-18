object SelectComponentForm: TSelectComponentForm
  Left = 787
  Top = 88
  Caption = 'Select component to add'
  ClientHeight = 592
  ClientWidth = 966
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
    966
    592)
  PixelsPerInch = 96
  TextHeight = 16
  object CompListView: TListView
    Left = 10
    Top = 10
    Width = 945
    Height = 525
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
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
    Left = 761
    Top = 545
    Width = 92
    Height = 31
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    Enabled = False
    ModalResult = 1
    TabOrder = 1
  end
  object Button2: TButton
    Left = 863
    Top = 545
    Width = 92
    Height = 31
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object HelpButton: TButton
    Left = 10
    Top = 545
    Width = 92
    Height = 31
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akLeft, akBottom]
    Caption = '&Help'
    TabOrder = 3
    OnClick = HelpButtonClick
  end
end
