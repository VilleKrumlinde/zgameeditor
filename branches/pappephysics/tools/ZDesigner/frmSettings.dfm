object SettingsForm: TSettingsForm
  Left = 206
  Top = 110
  BorderIcons = [biSystemMenu]
  Caption = 'Settings'
  ClientHeight = 374
  ClientWidth = 333
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
    333
    374)
  PixelsPerInch = 96
  TextHeight = 13
  object OkButton: TButton
    Left = 170
    Top = 342
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    TabOrder = 0
    OnClick = OkButtonClick
    ExplicitLeft = 165
    ExplicitTop = 352
  end
  object Button2: TButton
    Left = 252
    Top = 342
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
    ExplicitLeft = 247
    ExplicitTop = 352
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 118
    Width = 317
    Height = 154
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Packer settings'
    TabOrder = 2
    ExplicitWidth = 312
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
      TabOrder = 2
      OnChange = PackerPresetComboChange
      Items.Strings = (
        'Upx default'
        'Upx lzma'
        'Upx best'
        'Upx brute'
        'kkrunchy default'
        'kkrunchy best')
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 63
    Width = 317
    Height = 49
    Anchors = [akLeft, akTop, akRight]
    Caption = 'File association'
    TabOrder = 3
    ExplicitWidth = 312
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
    Width = 317
    Height = 49
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Designer layout'
    TabOrder = 4
    ExplicitWidth = 312
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
      TabOrder = 0
      Items.Strings = (
        'Left'
        'Middle')
    end
  end
  object GroupBox4: TGroupBox
    Left = 8
    Top = 278
    Width = 317
    Height = 59
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Code editor'
    TabOrder = 5
    ExplicitWidth = 312
    DesignSize = (
      317
      59)
    object Label7: TLabel
      Left = 14
      Top = 24
      Width = 188
      Height = 13
      Caption = 'Delay for code completion (milliseconds)'
    end
    object UpDown1: TUpDown
      Left = 265
      Top = 22
      Width = 24
      Height = 21
      Anchors = [akLeft, akBottom]
      Associate = CompDelayEdit
      Min = 100
      Max = 10000
      Increment = 100
      Orientation = udHorizontal
      Position = 100
      TabOrder = 0
    end
    object CompDelayEdit: TEdit
      Left = 224
      Top = 22
      Width = 41
      Height = 21
      Anchors = [akLeft, akBottom]
      ReadOnly = True
      TabOrder = 1
      Text = '100'
    end
  end
end
