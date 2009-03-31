inherited BitmapEditFrame: TBitmapEditFrame
  Width = 421
  Height = 351
  ExplicitWidth = 421
  ExplicitHeight = 351
  DesignSize = (
    421
    351)
  object Image: TImage
    Left = 16
    Top = 16
    Width = 223
    Height = 316
    Anchors = [akLeft, akTop, akRight, akBottom]
    PopupMenu = PopupMenu1
    OnMouseDown = ImageMouseDown
    OnMouseMove = ImageMouseMove
    OnMouseUp = ImageMouseUp
    ExplicitHeight = 329
  end
  object PaintBox: TPaintBox
    Left = 245
    Top = 16
    Width = 162
    Height = 169
    Anchors = [akTop, akRight]
    OnPaint = PaintBoxPaint
    ExplicitLeft = 487
  end
  object Button1: TButton
    Left = 245
    Top = 307
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Layout'
    TabOrder = 0
    OnClick = Button1Click
    ExplicitTop = 320
  end
  object PopupMenu1: TPopupMenu
    Left = 10
    Top = 10
    object AddMenuItem: TMenuItem
      Caption = 'Add'
    end
    object DeleteMenuItem: TMenuItem
      Caption = 'Delete'
      OnClick = DeleteMenuItemClick
    end
  end
end
