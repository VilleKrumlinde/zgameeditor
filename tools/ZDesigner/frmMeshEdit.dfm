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
    object WireframeCheckBox: TCheckBox
      Left = 2
      Top = 32
      Width = 230
      Height = 17
      Align = alTop
      Caption = 'Wireframe'
      TabOrder = 0
      OnClick = WireframeCheckBoxClick
    end
    object NormalsCheckBox: TCheckBox
      Left = 2
      Top = 15
      Width = 230
      Height = 17
      Align = alTop
      Caption = 'Normals'
      TabOrder = 1
      OnClick = MeshNormalsCheckBoxClick
    end
    object Panel1: TPanel
      Left = 2
      Top = 330
      Width = 230
      Height = 22
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 2
      object SaveMeshToFileButton: TButton
        Left = 104
        Top = 1
        Width = 75
        Height = 20
        Caption = 'Save to file'
        TabOrder = 0
        OnClick = SaveMeshToFileButtonClick
      end
      object DisablePreviewCheckBox: TCheckBox
        Left = 2
        Top = 2
        Width = 100
        Height = 18
        Caption = 'Hide preview'
        TabOrder = 1
        OnClick = DisablePreviewCheckBoxClick
      end
    end
    object PreviewPanel: TPanel
      Left = 2
      Top = 49
      Width = 230
      Height = 281
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 3
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
