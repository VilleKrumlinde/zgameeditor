object AboutForm: TAboutForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'AboutForm'
  ClientHeight = 327
  ClientWidth = 690
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  DesignSize = (
    690
    327)
  PixelsPerInch = 96
  TextHeight = 13
  object NameLabel: TLabel
    Left = 8
    Top = 3
    Width = 84
    Height = 18
    Caption = 'NameLabel'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object VersionLabel: TLabel
    Left = 256
    Top = 8
    Width = 117
    Height = 13
    Alignment = taRightJustify
    AutoSize = False
    Caption = 'VersionLabel'
  end
  object SplashPanel: TPanel
    Left = 8
    Top = 24
    Width = 365
    Height = 295
    Anchors = [akLeft, akTop, akBottom]
    TabOrder = 0
  end
  object Button1: TButton
    Left = 607
    Top = 295
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 378
    Top = 24
    Width = 304
    Height = 265
    BevelOuter = bvLowered
    TabOrder = 2
    object Label1: TLabel
      Left = 11
      Top = 8
      Width = 107
      Height = 13
      Caption = 'Author: Ville Krumlinde'
    end
    object Label2: TLabel
      Left = 11
      Top = 172
      Width = 108
      Height = 13
      Caption = 'Application icons from:'
    end
    object Label3: TLabel
      Left = 11
      Top = 27
      Width = 226
      Height = 13
      Caption = '(this animation source: "About" sample project)'
    end
    object Label4: TLabel
      Left = 125
      Top = 172
      Width = 141
      Height = 13
      Caption = 'http://www.famfamfam.com/'
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clHotLight
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      OnClick = Label6Click
    end
    object Label5: TLabel
      Left = 11
      Top = 54
      Width = 118
      Height = 13
      Caption = 'ZGameEditor Home Page'
    end
    object Label6: TLabel
      Left = 11
      Top = 73
      Width = 143
      Height = 13
      Caption = 'http://www.zgameeditor.org/'
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clHotLight
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      OnClick = Label6Click
    end
    object Label7: TLabel
      Left = 11
      Top = 153
      Width = 151
      Height = 13
      Caption = 'Resources code by Colin Wilson'
    end
    object Label8: TLabel
      Left = 11
      Top = 134
      Width = 118
      Height = 13
      Caption = 'Third party components:'
    end
    object Label9: TLabel
      Left = 11
      Top = 191
      Width = 158
      Height = 13
      Caption = 'LibXmlParser by Stefan Heymann'
    end
    object Label10: TLabel
      Left = 11
      Top = 210
      Width = 165
      Height = 13
      Caption = 'Coco/R for Delphi by Michael Reith'
    end
  end
end
