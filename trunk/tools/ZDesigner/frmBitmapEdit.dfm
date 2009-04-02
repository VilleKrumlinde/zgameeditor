inherited BitmapEditFrame: TBitmapEditFrame
  Width = 421
  Height = 354
  OnResize = FrameResize
  ExplicitWidth = 421
  ExplicitHeight = 354
  DesignSize = (
    421
    354)
  object Image: TImage
    Left = 5
    Top = 5
    Width = 223
    Height = 333
    Anchors = [akLeft, akTop, akRight, akBottom]
    PopupMenu = PopupMenu1
    OnMouseDown = ImageMouseDown
    OnMouseMove = ImageMouseMove
    OnMouseUp = ImageMouseUp
    ExplicitHeight = 330
  end
  object Panel1: TPanel
    Left = 229
    Top = 5
    Width = 180
    Height = 164
    Anchors = [akTop, akRight]
    BevelOuter = bvNone
    FullRepaint = False
    ParentBackground = False
    TabOrder = 0
    object PaintBox: TPaintBox
      Left = 0
      Top = 0
      Width = 180
      Height = 164
      Align = alClient
      OnPaint = PaintBoxPaint
      ExplicitLeft = 9
      ExplicitWidth = 178
      ExplicitHeight = 159
    end
  end
  object PopupMenu1: TPopupMenu
    OnPopup = PopupMenu1Popup
    Left = 65535
    Top = 1
    object AddMenuItem: TMenuItem
      Caption = 'Add'
    end
    object DeleteMenuItem: TMenuItem
      Caption = 'Delete'
      OnClick = DeleteMenuItemClick
    end
  end
end
