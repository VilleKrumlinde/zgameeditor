object Import3dsForm: TImport3dsForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = '3DS import'
  ClientHeight = 256
  ClientWidth = 450
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  ShowHint = True
  DesignSize = (
    450
    256)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 19
    Width = 58
    Height = 13
    Caption = 'Name prefix'
  end
  object Label2: TLabel
    Left = 168
    Top = 78
    Width = 52
    Height = 13
    Caption = 'Mesh scale'
  end
  object Label3: TLabel
    Left = 280
    Top = 78
    Width = 11
    Height = 13
    Caption = '%'
  end
  object Label4: TLabel
    Left = 8
    Top = 43
    Width = 187
    Height = 13
    Caption = 'Generated compontents will be named:'
  end
  object NameExampleLabel: TLabel
    Left = 202
    Top = 43
    Width = 231
    Height = 13
    AutoSize = False
  end
  object DownloadURLLabel: TLabel
    Left = 8
    Top = 237
    Width = 140
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'How to: Import a 3ds-file'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clActiveCaption
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    OnClick = DownloadURLLabelClick
    ExplicitTop = 244
  end
  object Button1: TButton
    Left = 286
    Top = 223
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
    ExplicitTop = 206
  end
  object Button2: TButton
    Left = 367
    Top = 223
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
    ExplicitTop = 206
  end
  object NamePrefixEdit: TEdit
    Left = 72
    Top = 16
    Width = 121
    Height = 21
    TabOrder = 2
    OnChange = NamePrefixEditChange
  end
  object UpDown1: TUpDown
    Left = 261
    Top = 75
    Width = 16
    Height = 21
    Associate = MeshScaleEdit
    Min = 1
    Max = 1000
    Increment = 10
    Position = 100
    TabOrder = 3
  end
  object MeshScaleEdit: TEdit
    Left = 226
    Top = 75
    Width = 35
    Height = 21
    Enabled = False
    TabOrder = 4
    Text = '100'
  end
  object ColorsCheckBox: TCheckBox
    Left = 8
    Top = 136
    Width = 153
    Height = 17
    Caption = 'Import vertex colors'
    Checked = True
    State = cbChecked
    TabOrder = 5
  end
  object AutoCenterCheckBox: TCheckBox
    Left = 8
    Top = 97
    Width = 153
    Height = 17
    Caption = 'Auto center model'
    Checked = True
    State = cbChecked
    TabOrder = 6
  end
  object AutoScaleCheckBox: TCheckBox
    Left = 8
    Top = 77
    Width = 137
    Height = 17
    Caption = 'Auto scale model'
    Checked = True
    State = cbChecked
    TabOrder = 7
    OnClick = AutoScaleCheckBoxClick
  end
  object InvertNormalsCheckBox: TCheckBox
    Left = 8
    Top = 176
    Width = 153
    Height = 17
    Hint = 
      'If the model appears transparent after import, try setting this ' +
      'checkbox'
    Caption = 'Invert normals'
    TabOrder = 8
  end
  object TexCoordsCheckBox: TCheckBox
    Left = 8
    Top = 156
    Width = 153
    Height = 17
    Caption = 'Import texture coordinates'
    TabOrder = 9
  end
  object SingleMeshCheckBox: TCheckBox
    Left = 8
    Top = 199
    Width = 153
    Height = 17
    Hint = 
      'Make a single Mesh-component even if the 3ds-file contains sever' +
      'al meshes'
    Caption = 'Single mesh'
    TabOrder = 10
  end
end
