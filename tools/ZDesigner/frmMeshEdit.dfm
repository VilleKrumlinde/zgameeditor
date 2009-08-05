inherited MeshEditFrame: TMeshEditFrame
  Width = 438
  Height = 354
  OnResize = FrameResize
  ExplicitWidth = 438
  ExplicitHeight = 354
  object Splitter1: TSplitter
    Left = 200
    Top = 0
    Width = 4
    Height = 354
    ExplicitLeft = 233
  end
  object LeftPanel: TGroupBox
    Left = 0
    Top = 0
    Width = 200
    Height = 354
    Align = alLeft
    Caption = 'Graph'
    TabOrder = 0
    object ScrollBox1: TScrollBox
      Left = 2
      Top = 15
      Width = 196
      Height = 337
      Align = alClient
      BevelInner = bvNone
      BevelOuter = bvNone
      BorderStyle = bsNone
      PopupMenu = PopupMenu1
      TabOrder = 0
      ExplicitWidth = 245
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
    Left = 204
    Top = 0
    Width = 234
    Height = 354
    Align = alClient
    Caption = 'Preview'
    TabOrder = 1
    ExplicitLeft = 253
    ExplicitWidth = 185
    object PreviewPanel: TPanel
      Left = 2
      Top = 49
      Width = 230
      Height = 285
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitWidth = 181
    end
    object DisablePreviewCheckBox: TCheckBox
      Left = 2
      Top = 334
      Width = 230
      Height = 18
      Align = alBottom
      Caption = 'Hide preview'
      TabOrder = 1
      OnClick = DisablePreviewCheckBoxClick
      ExplicitWidth = 181
    end
    object WireframeCheckBox: TCheckBox
      Left = 2
      Top = 32
      Width = 230
      Height = 17
      Align = alTop
      Caption = 'Wireframe'
      TabOrder = 2
      ExplicitWidth = 181
    end
    object NormalsCheckBox: TCheckBox
      Left = 2
      Top = 15
      Width = 230
      Height = 17
      Align = alTop
      Caption = 'Normals'
      TabOrder = 3
      OnClick = MeshNormalsCheckBoxClick
      ExplicitWidth = 181
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
