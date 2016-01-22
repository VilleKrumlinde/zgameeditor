inherited ShaderPropEditForm: TShaderPropEditForm
  Caption = 'ShaderPropEditForm'
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  inherited DetachButton: TButton
    ExplicitTop = 234
  end
  object ShaderPanel: TGroupBox
    Left = 0
    Top = 0
    Width = 558
    Height = 226
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = '&Shader editor'
    TabOrder = 1
    object CompileErrorLabel: TStaticText
      Left = 2
      Top = 192
      Width = 554
      Height = 32
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
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
      ExplicitLeft = 90
      ExplicitTop = 188
      ExplicitWidth = 305
    end
  end
  object CompileShaderButton: TButton
    Left = 457
    Top = 234
    Width = 92
    Height = 26
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Anchors = [akRight, akBottom]
    Caption = 'C&ompile'
    Enabled = False
    TabOrder = 2
  end
end
