inherited ExprPropEditForm: TExprPropEditForm
  Caption = 'ExprPropEditForm'
  ClientWidth = 604
  OnCreate = FormCreate
  ExplicitWidth = 604
  PixelsPerInch = 96
  TextHeight = 13
  inherited DetachButton: TButton
    ExplicitTop = 234
  end
  object ExprPanel: TGroupBox
    Left = 0
    Top = 0
    Width = 604
    Height = 220
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = '&Code editor'
    TabOrder = 1
    object Splitter: TSplitter
      Left = 2
      Top = 183
      Width = 600
      Height = 3
      Cursor = crVSplit
      Align = alBottom
      Visible = False
      ExplicitTop = 182
    end
    object CompileErrorLabel: TStaticText
      Left = 2
      Top = 186
      Width = 600
      Height = 32
      Align = alBottom
      AutoSize = False
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -20
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      TabOrder = 0
      Transparent = False
      Visible = False
    end
  end
  object ExprCompileButton: TButton
    Left = 403
    Top = 228
    Width = 92
    Height = 30
    Anchors = [akRight, akBottom]
    Caption = 'C&ompile'
    Enabled = False
    TabOrder = 2
  end
  object ExprHelpButton: TButton
    Left = 503
    Top = 228
    Width = 92
    Height = 30
    Anchors = [akRight, akBottom]
    Caption = '&Help'
    TabOrder = 3
    OnClick = ExprHelpButtonClick
  end
end
