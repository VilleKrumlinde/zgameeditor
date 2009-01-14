object SettingsForm: TSettingsForm
  Left = 206
  Top = 110
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Settings'
  ClientHeight = 322
  ClientWidth = 358
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    358
    322)
  PixelsPerInch = 96
  TextHeight = 13
  object OkButton: TButton
    Left = 195
    Top = 290
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    TabOrder = 0
    OnClick = OkButtonClick
  end
  object Button2: TButton
    Left = 277
    Top = 290
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 118
    Width = 339
    Height = 154
    Caption = 'Packer settings'
    TabOrder = 2
    object Label1: TLabel
      Left = 14
      Top = 32
      Width = 35
      Height = 13
      Caption = 'Presets'
    end
    object Label2: TLabel
      Left = 14
      Top = 59
      Width = 39
      Height = 13
      Caption = 'Program'
    end
    object Label3: TLabel
      Left = 14
      Top = 101
      Width = 50
      Height = 13
      Caption = 'Arguments'
    end
    object Label4: TLabel
      Left = 70
      Top = 80
      Width = 194
      Height = 13
      Caption = '{$toolpath} = Path to ZGameEditor\Tools'
    end
    object Label5: TLabel
      Left = 70
      Top = 122
      Width = 164
      Height = 13
      Caption = '{$exename} = Name of executable'
    end
    object PackerEdit: TEdit
      Left = 70
      Top = 56
      Width = 235
      Height = 21
      TabOrder = 0
      Text = 'PackerEdit'
    end
    object PackerParamsEdit: TEdit
      Left = 70
      Top = 99
      Width = 235
      Height = 21
      TabOrder = 1
      Text = 'PackerParamsEdit'
    end
    object PackerPresetCombo: TComboBox
      Left = 70
      Top = 29
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 2
      OnChange = PackerPresetComboChange
      Items.Strings = (
        'Upx default'
        'Upx best'
        'Upx brute'
        'kkrunchy default'
        'kkrunchy best')
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 63
    Width = 339
    Height = 49
    Caption = 'Explorer'
    TabOrder = 3
    object ShellCheck: TCheckBox
      Left = 10
      Top = 16
      Width = 255
      Height = 17
      Caption = 'Associate .zgeproj file extension with explorer'
      TabOrder = 0
      OnClick = ShellCheckClick
    end
  end
  object GroupBox3: TGroupBox
    Left = 8
    Top = 8
    Width = 339
    Height = 49
    Caption = 'Designer layout'
    TabOrder = 4
    object Label6: TLabel
      Left = 14
      Top = 19
      Width = 185
      Height = 13
      Caption = 'Property editor position (requires restart)'
    end
    object GuiLayoutCombo: TComboBox
      Left = 211
      Top = 16
      Width = 89
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
      Items.Strings = (
        'Left'
        'Middle')
    end
  end
end
