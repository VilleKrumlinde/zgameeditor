inherited BitmapEditFrame: TBitmapEditFrame
  Width = 421
  Height = 373
  ExplicitWidth = 421
  ExplicitHeight = 373
  object Image: TImage
    Left = 16
    Top = 16
    Width = 223
    Height = 317
    Anchors = [akLeft, akTop, akRight, akBottom]
    PopupMenu = PopupMenu1
    OnMouseDown = ImageMouseDown
    OnMouseMove = ImageMouseMove
    OnMouseUp = ImageMouseUp
    ExplicitWidth = 465
    ExplicitHeight = 513
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
    Top = 308
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Layout'
    TabOrder = 0
    OnClick = Button1Click
    ExplicitTop = 387
  end
  object PopupMenu1: TPopupMenu
    Left = 520
    Top = 256
    object AddMenuItem: TMenuItem
      Caption = 'Add'
    end
  end
end
