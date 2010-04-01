inherited BitmapEditFrame: TBitmapEditFrame
  Width = 438
  Height = 354
  OnResize = FrameResize
  ExplicitWidth = 438
  ExplicitHeight = 354
  object Splitter1: TSplitter
    Left = 249
    Top = 0
    Width = 4
    Height = 354
    ExplicitLeft = 253
    ExplicitTop = 3
  end
  object LeftPanel: TGroupBox
    Left = 0
    Top = 0
    Width = 249
    Height = 354
    Align = alLeft
    Caption = 'Graph'
    TabOrder = 0
    object ScrollBox1: TScrollBox
      Left = 2
      Top = 15
      Width = 245
      Height = 337
      Align = alClient
      BevelInner = bvNone
      BevelOuter = bvNone
      BorderStyle = bsNone
      PopupMenu = PopupMenu1
      TabOrder = 0
      object Image: TImage
        Left = 0
        Top = 0
        Width = 161
        Height = 270
        PopupMenu = PopupMenu1
        OnMouseDown = ImageMouseDown
        OnMouseMove = ImageMouseMove
        OnMouseUp = ImageMouseUp
      end
    end
  end
  object RightPanel: TGroupBox
    Left = 253
    Top = 0
    Width = 185
    Height = 354
    Align = alClient
    Caption = 'Preview'
    TabOrder = 1
    object PreviewPanel: TPanel
      Left = 2
      Top = 15
      Width = 181
      Height = 337
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      object Panel1: TPanel
        Left = 0
        Top = 315
        Width = 181
        Height = 22
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 0
        object DisablePreviewCheckBox: TCheckBox
          Left = 2
          Top = 1
          Width = 100
          Height = 18
          Caption = 'Hide preview'
          TabOrder = 0
          OnClick = DisablePreviewCheckBoxClick
        end
        object SaveToFileButton: TButton
          Left = 104
          Top = 1
          Width = 75
          Height = 20
          Caption = 'Save to file'
          TabOrder = 1
          OnClick = SaveToFileButtonClick
        end
      end
    end
  end
  object PopupMenu1: TPopupMenu
    OnPopup = PopupMenu1Popup
    Left = 63
    Top = 153
    object AddMenuItem: TMenuItem
      Caption = 'Add'
    end
    object DeleteMenuItem: TMenuItem
      Caption = 'Delete'
      OnClick = DeleteMenuItemClick
    end
    object PreviewMenuItem: TMenuItem
      Caption = 'Preview'
      OnClick = PreviewMenuItemClick
    end
  end
end
