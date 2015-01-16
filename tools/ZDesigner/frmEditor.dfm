object EditorForm: TEditorForm
  Left = 0
  Top = 0
  Caption = 'ZDesigner'
  ClientHeight = 1022
  ClientWidth = 1344
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  ShowHint = True
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnKeyPress = FormKeyPress
  PixelsPerInch = 96
  TextHeight = 16
  object Splitter2: TSplitter
    Left = 345
    Top = 22
    Width = 5
    Height = 1000
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    ExplicitTop = 27
    ExplicitHeight = 1013
  end
  object LeftPanel: TPanel
    Left = 0
    Top = 22
    Width = 345
    Height = 1000
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alLeft
    BevelOuter = bvNone
    Constraints.MinWidth = 123
    TabOrder = 0
    object TreePanel: TGroupBox
      Left = 0
      Top = 0
      Width = 345
      Height = 1000
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Align = alClient
      Caption = 'Project &tree'
      Constraints.MinHeight = 185
      TabOrder = 0
      object GamutImage: TImage
        Left = 176
        Top = 256
        Width = 105
        Height = 105
        Picture.Data = {
          0954506E67496D61676589504E470D0A1A0A0000000D49484452000000100000
          00100802000000909168360000000467414D410000B18E7CFB51930000000970
          48597300000B1200000B1201D2DD7EFC0000001A74455874536F667477617265
          005061696E742E4E45542076332E352E313147F34237000000274944415478DA
          63FCFFFF3F033670F6EC59ACE28CA31A68A2E1CC993358258C8D8D4735D04F03
          00238343190EE1CA360000000049454E44AE426082}
        Visible = False
      end
    end
  end
  object ViewerPanel: TPanel
    Left = 350
    Top = 22
    Width = 994
    Height = 1000
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object Splitter1: TSplitter
      Left = 0
      Top = 803
      Width = 994
      Height = 5
      Cursor = crVSplit
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Align = alBottom
      ExplicitTop = 816
    end
    object LowerRightPanel: TPanel
      Left = 0
      Top = 808
      Width = 994
      Height = 192
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Align = alBottom
      BevelOuter = bvNone
      Constraints.MinHeight = 20
      TabOrder = 0
      object Splitter4: TSplitter
        Left = 780
        Top = 0
        Width = 5
        Height = 192
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Align = alRight
      end
      object LogPanel: TPanel
        Left = 785
        Top = 0
        Width = 209
        Height = 192
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Align = alRight
        BevelOuter = bvNone
        Constraints.MinWidth = 5
        DoubleBuffered = True
        ParentDoubleBuffered = False
        TabOrder = 1
        object LogListBox: TListBox
          Left = 0
          Top = 0
          Width = 209
          Height = 192
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Style = lbOwnerDrawFixed
          Align = alClient
          BevelInner = bvNone
          BevelOuter = bvNone
          Color = 2976796
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Arial'
          Font.Style = []
          ItemHeight = 14
          ParentFont = False
          PopupMenu = LogPopupMenu
          TabOrder = 0
          OnDrawItem = LogListBoxDrawItem
          OnMouseMove = LogListBoxMouseMove
        end
      end
      object CustomPropEditorsPageControl: TPageControl
        Left = 0
        Top = 0
        Width = 780
        Height = 192
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        ActivePage = TabSheet3
        Align = alClient
        Constraints.MinWidth = 123
        TabOrder = 0
        object TabSheet2: TTabSheet
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'TabSheet2'
          ImageIndex = 1
          TabVisible = False
        end
        object TabSheet1: TTabSheet
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Float/Int'
          TabVisible = False
          DesignSize = (
            772
            182)
          object Label1: TLabel
            Left = 20
            Top = 10
            Width = 59
            Height = 16
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Caption = '&Edit value'
            FocusControl = TrackBar1
          end
          object TrackBar1: TTrackBar
            Left = 10
            Top = 31
            Width = 757
            Height = 55
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Anchors = [akLeft, akTop, akRight]
            Max = 1000
            TabOrder = 0
            TickStyle = tsNone
            OnChange = TrackBar1Change
          end
        end
        object TabSheet3: TTabSheet
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Expression'
          ImageIndex = 2
          TabVisible = False
          DesignSize = (
            772
            182)
          object ExprPanel: TGroupBox
            Left = 0
            Top = 0
            Width = 772
            Height = 132
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Align = alTop
            Anchors = [akLeft, akTop, akRight, akBottom]
            Caption = '&Code editor'
            TabOrder = 0
            OnClick = ExprPanelClick
          end
          object ExprCompileButton: TButton
            Left = 10
            Top = 142
            Width = 92
            Height = 30
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Anchors = [akLeft, akBottom]
            Caption = '&OK'
            Enabled = False
            TabOrder = 1
            OnClick = ExprCompileButtonClick
          end
          object CompileErrorLabel: TStaticText
            Left = 207
            Top = 139
            Width = 560
            Height = 32
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Anchors = [akLeft, akRight, akBottom]
            AutoSize = False
            Color = clWhite
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clRed
            Font.Height = -20
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentColor = False
            ParentFont = False
            TabOrder = 2
            Transparent = False
          end
          object ExprHelpButton: TButton
            Left = 107
            Top = 142
            Width = 92
            Height = 30
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Anchors = [akLeft, akBottom]
            Caption = '&Help'
            TabOrder = 3
            OnClick = ExprHelpButtonClick
          end
        end
        object ShaderTabSheet: TTabSheet
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'ShaderTabSheet'
          ImageIndex = 3
          TabVisible = False
          DesignSize = (
            772
            182)
          object Label6: TLabel
            Left = 118
            Top = 150
            Width = 117
            Height = 16
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Anchors = [akLeft, akBottom]
            Caption = 'GLSL Shader editor'
          end
          object CompileShaderButton: TButton
            Left = 10
            Top = 142
            Width = 92
            Height = 30
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Anchors = [akLeft, akBottom]
            Caption = '&OK'
            Enabled = False
            TabOrder = 0
            OnClick = CompileShaderButtonClick
          end
          object ShaderPanel: TGroupBox
            Left = 0
            Top = 0
            Width = 772
            Height = 132
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Align = alTop
            Anchors = [akLeft, akTop, akRight, akBottom]
            Caption = '&Shader editor'
            TabOrder = 1
            OnClick = ExprPanelClick
          end
        end
      end
    end
    object Panel2: TPanel
      Left = 0
      Top = 0
      Width = 994
      Height = 803
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object Splitter3: TSplitter
        Left = 358
        Top = 0
        Width = 5
        Height = 803
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        ExplicitHeight = 816
      end
      object PropEditorPanel: TGroupBox
        Left = 0
        Top = 0
        Width = 358
        Height = 803
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Align = alLeft
        Caption = '&Properties'
        Constraints.MinWidth = 100
        TabOrder = 1
      end
      object ViewerPageControl: TPageControl
        Left = 363
        Top = 0
        Width = 631
        Height = 803
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        ActivePage = ViewerCompTabSheet
        Align = alClient
        TabOrder = 0
        object ViewerGlTabSheet: TTabSheet
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'ViewerGlTabSheet'
          TabVisible = False
          object RotateModelPanel: TPanel
            Left = 0
            Top = 0
            Width = 623
            Height = 62
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Align = alTop
            TabOrder = 0
            object ViewTranslateLabel: TLabel
              Left = 512
              Top = 6
              Width = 39
              Height = 37
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Alignment = taRightJustify
              AutoSize = False
              Caption = 'ViewTranslateLabel'
              Font.Charset = ANSI_CHARSET
              Font.Color = clWindowText
              Font.Height = -10
              Font.Name = 'Arial'
              Font.Style = []
              ParentFont = False
              WordWrap = True
            end
            object Label2: TLabel
              Left = 9
              Top = 27
              Width = 51
              Height = 16
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Caption = 'Rotate X'
            end
            object Label3: TLabel
              Left = 138
              Top = 27
              Width = 52
              Height = 16
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Caption = 'Rotate Y'
            end
            object Label4: TLabel
              Left = 267
              Top = 27
              Width = 51
              Height = 16
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Caption = 'Rotate Z'
            end
            object Label5: TLabel
              Left = 399
              Top = 27
              Width = 35
              Height = 16
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Caption = 'Zoom'
            end
            object ViewRotateXTrackBar: TTrackBar
              Left = 0
              Top = 0
              Width = 123
              Height = 30
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Max = 100
              TabOrder = 0
              TickStyle = tsNone
              OnChange = ViewRotateXTrackBarChange
            end
            object TrackBar2: TTrackBar
              Tag = 1
              Left = 128
              Top = 0
              Width = 123
              Height = 30
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Max = 100
              TabOrder = 1
              TickStyle = tsNone
              OnChange = ViewRotateXTrackBarChange
            end
            object TrackBar3: TTrackBar
              Tag = 2
              Left = 257
              Top = 0
              Width = 123
              Height = 30
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Max = 100
              TabOrder = 2
              TickStyle = tsNone
              OnChange = ViewRotateXTrackBarChange
            end
            object ZoomTrackBar: TTrackBar
              Left = 389
              Top = 0
              Width = 123
              Height = 30
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              TabOrder = 3
              TickStyle = tsNone
              OnChange = ZoomTrackBarChange
            end
            object Panel4: TPanel
              Left = 310
              Top = 1
              Width = 312
              Height = 60
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Align = alRight
              BevelOuter = bvNone
              ParentBackground = False
              TabOrder = 4
              DesignSize = (
                312
                60)
              object NormalsCheckBox: TCheckBox
                Left = 11
                Top = 6
                Width = 75
                Height = 21
                Margins.Left = 4
                Margins.Top = 4
                Margins.Right = 4
                Margins.Bottom = 4
                Anchors = [akTop, akRight]
                Caption = 'Normals'
                TabOrder = 0
                OnClick = NormalsCheckBoxClick
              end
              object ResetCameraButton: TButton
                Left = 212
                Top = 31
                Width = 90
                Height = 21
                Margins.Left = 4
                Margins.Top = 4
                Margins.Right = 4
                Margins.Bottom = 4
                Anchors = [akTop, akRight]
                Caption = 'Reset camera'
                TabOrder = 1
                OnClick = ResetCameraButtonClick
              end
              object ResetModelButton: TButton
                Left = 212
                Top = 6
                Width = 90
                Height = 21
                Margins.Left = 4
                Margins.Top = 4
                Margins.Right = 4
                Margins.Bottom = 4
                Anchors = [akTop, akRight]
                Caption = '&Reset time'
                Enabled = False
                TabOrder = 2
                OnClick = ResetModelButtonClick
              end
              object UpdateTimeCheckBox: TCheckBox
                Left = 89
                Top = 6
                Width = 99
                Height = 21
                Margins.Left = 4
                Margins.Top = 4
                Margins.Right = 4
                Margins.Bottom = 4
                Anchors = [akTop, akRight]
                Caption = 'Update time'
                TabOrder = 3
                OnClick = UpdateTimeCheckBoxClick
              end
              object WireframeCheckBox: TCheckBox
                Left = 89
                Top = 27
                Width = 91
                Height = 21
                Margins.Left = 4
                Margins.Top = 4
                Margins.Right = 4
                Margins.Bottom = 4
                Anchors = [akTop, akRight]
                Caption = 'Wireframe'
                TabOrder = 4
              end
              object BoundsCheckBox: TCheckBox
                Left = 11
                Top = 27
                Width = 75
                Height = 21
                Hint = 'Click to display collision bounds'
                Margins.Left = 4
                Margins.Top = 4
                Margins.Right = 4
                Margins.Bottom = 4
                Anchors = [akTop, akRight]
                Caption = 'Bounds'
                TabOrder = 5
                OnClick = BoundsCheckBoxClick
              end
            end
          end
          object AppControlPanel: TPanel
            Left = 0
            Top = 62
            Width = 623
            Height = 50
            Margins.Left = 4
            Margins.Top = 4
            Margins.Right = 4
            Margins.Bottom = 4
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 1
            object DisableShadersCheckBox: TCheckBox
              Left = 236
              Top = 2
              Width = 120
              Height = 21
              Hint = 'Click to disable OpenGL shaders'
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Caption = 'Disable shaders'
              TabOrder = 0
              OnClick = DisableShadersCheckBoxClick
            end
            object DisableFBOCheckBox: TCheckBox
              Left = 236
              Top = 22
              Width = 120
              Height = 21
              Hint = 'Click to disable OpenGL FBO (render to texture)'
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Caption = 'Disable FBO'
              TabOrder = 1
              OnClick = DisableFBOCheckBoxClick
            end
            object AppStartButton: TButton
              Left = 2
              Top = 7
              Width = 71
              Height = 31
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Action = AppPreviewStartAction
              Images = ActionImageList
              TabOrder = 2
            end
            object AppStopButton: TButton
              Left = 71
              Top = 7
              Width = 71
              Height = 31
              Margins.Left = 4
              Margins.Top = 4
              Margins.Right = 4
              Margins.Bottom = 4
              Action = AppPreviewStopAction
              Images = ActionImageList
              TabOrder = 3
            end
          end
        end
        object ViewerCompTabSheet: TTabSheet
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          ImageIndex = 1
          TabVisible = False
          object Panel1: TPanel
            Left = 0
            Top = 0
            Width = 623
            Height = 24
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 0
            object DetachCompEditorButton: TButton
              Left = 2
              Top = 1
              Width = 111
              Height = 20
              Caption = 'Detach editor'
              TabOrder = 0
              OnClick = DetachCompEditorButtonClick
            end
          end
          object CompEditorParentPanel: TPanel
            Left = 0
            Top = 24
            Width = 623
            Height = 769
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 1
          end
        end
        object ViewerBlankTabSheet: TTabSheet
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          ImageIndex = 3
          TabVisible = False
        end
      end
    end
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 1344
    Height = 22
    AutoSize = True
    ButtonWidth = 178
    Caption = 'ToolBar1'
    DisabledImages = ActionDisabledImageList
    DrawingStyle = dsGradient
    Images = ActionImageList
    List = True
    AllowTextButtons = True
    TabOrder = 2
    Transparent = False
    object ToolButton8: TToolButton
      Left = 0
      Top = 0
      Action = FileOpenAction
    end
    object ToolButton1: TToolButton
      Left = 24
      Top = 0
      Action = SaveProjectAction
    end
    object ToolButton6: TToolButton
      Left = 48
      Top = 0
      Width = 8
      Caption = 'ToolButton6'
      ImageIndex = 3
      Style = tbsSeparator
    end
    object ToolButton2: TToolButton
      Left = 56
      Top = 0
      Action = AddComponentAction
    end
    object ToolButton3: TToolButton
      Left = 80
      Top = 0
      Action = DeleteComponentAction
    end
    object ToolButton7: TToolButton
      Left = 104
      Top = 0
      Width = 8
      Caption = 'ToolButton7'
      ImageIndex = 3
      Style = tbsSeparator
    end
    object ToolButton4: TToolButton
      Left = 112
      Top = 0
      Action = MoveUpComponentAction
    end
    object ToolButton5: TToolButton
      Left = 136
      Top = 0
      Action = MoveDownComponentAction
    end
    object ToolButton9: TToolButton
      Left = 160
      Top = 0
      Width = 8
      Caption = 'ToolButton9'
      ImageIndex = 3
      Style = tbsSeparator
    end
    object ToolButton10: TToolButton
      Left = 168
      Top = 0
      Action = EditCopyAction
    end
    object ToolButton11: TToolButton
      Left = 192
      Top = 0
      Action = EditPasteAction
    end
    object ToolButton12: TToolButton
      Left = 216
      Top = 0
      Width = 8
      Caption = 'ToolButton12'
      ImageIndex = 8
      Style = tbsSeparator
    end
    object ToolButton15: TToolButton
      Left = 224
      Top = 0
      Action = LockShowAction
    end
    object ToolButton16: TToolButton
      Left = 248
      Top = 0
      Width = 8
      Caption = 'ToolButton16'
      ImageIndex = 11
      Style = tbsSeparator
    end
    object ToolButton13: TToolButton
      Left = 256
      Top = 0
      Action = RunExeAction
    end
    object ToolButton14: TToolButton
      Left = 280
      Top = 0
      Width = 8
      Caption = 'ToolButton14'
      ImageIndex = 9
      Style = tbsSeparator
    end
    object ToolButton17: TToolButton
      Left = 288
      Top = 0
      Action = EditXmlAction
    end
  end
  object SaveDialog: TSaveDialog
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 48
    Top = 40
  end
  object Timer1: TTimer
    Interval = 15
    OnTimer = Timer1Timer
    Left = 88
    Top = 40
  end
  object ActionList1: TActionList
    Images = ActionImageList
    Left = 48
    Top = 128
    object AddComponentAction: TAction
      Caption = 'Add component'
      Hint = 'Add component'
      ImageIndex = 0
      ShortCut = 16449
      OnExecute = AddComponentActionExecute
      OnUpdate = AddComponentActionUpdate
    end
    object ResetComponentAction: TAction
      Caption = 'Reset component'
      OnExecute = ResetComponentActionExecute
    end
    object DeleteComponentAction: TAction
      Caption = 'Delete component'
      Hint = 'Delete component'
      ImageIndex = 4
      ShortCut = 8238
      OnExecute = DeleteComponentActionExecute
      OnUpdate = DeleteComponentActionUpdate
    end
    object CopyComponentAction: TAction
      Caption = 'Copy component'
      ShortCut = 16451
      OnExecute = CopyComponentActionExecute
      OnUpdate = CopyComponentActionUpdate
    end
    object PasteComponentAction: TAction
      Caption = 'Paste component'
      ShortCut = 16470
      OnExecute = PasteComponentActionExecute
      OnUpdate = PasteComponentActionUpdate
    end
    object MoveUpComponentAction: TAction
      Caption = 'Move up'
      Hint = 'Move up'
      ImageIndex = 1
      SecondaryShortCuts.Strings = (
        'Ctrl-Up')
      ShortCut = 16422
      OnExecute = MoveUpComponentActionExecute
      OnUpdate = MoveUpComponentActionUpdate
    end
    object MoveDownComponentAction: TAction
      Caption = 'Move down'
      Hint = 'Move down'
      ImageIndex = 2
      ShortCut = 16424
      OnExecute = MoveDownComponentActionExecute
      OnUpdate = MoveDownComponentActionUpdate
    end
    object SaveProjectAction: TAction
      Caption = 'Save'
      Hint = 'Save'
      ImageIndex = 3
      ShortCut = 16467
      OnExecute = SaveProjectActionExecute
    end
    object LockShowAction: TAction
      Caption = 'Lock preview to this component'
      Hint = 'Toggle lock preview selected component'
      ImageIndex = 10
      ShortCut = 16460
      OnExecute = LockShowActionExecute
      OnUpdate = LockShowActionUpdate
    end
    object NewProjectAction: TAction
      Caption = 'New project'
      ShortCut = 16462
      OnExecute = NewProjectActionExecute
    end
    object FileExitAction: TFileExit
      Category = 'File'
      Caption = 'E&xit'
      Hint = 'Exit|Quits the application'
      ImageIndex = 14
    end
    object FileOpenAction: TFileOpen
      Category = 'File'
      Caption = '&Open...'
      Dialog.DefaultExt = '*.zgeproj'
      Dialog.Filter = 'Project files (*.zgeproj)|*.zgeproj|Any file (*.*)|*.*'
      Hint = 'Open|Opens an existing file'
      ImageIndex = 5
      ShortCut = 16463
      BeforeExecute = FileOpenActionBeforeExecute
      OnAccept = FileOpenActionAccept
    end
    object EditCopyAction: TEditCopy
      Category = 'Edit'
      Caption = '&Copy component'
      Enabled = False
      Hint = 'Copy|Copies the selection and puts it on the Clipboard'
      ImageIndex = 6
      ShortCut = 16451
      OnExecute = CopyComponentActionExecute
      OnUpdate = CopyComponentActionUpdate
    end
    object EditPasteAction: TEditPaste
      Category = 'Edit'
      Caption = '&Paste component'
      Hint = 'Paste|Inserts Clipboard contents'
      ImageIndex = 7
      ShortCut = 16470
      OnExecute = PasteComponentActionExecute
      OnUpdate = PasteComponentActionUpdate
    end
    object FileSaveAsAction: TFileSaveAs
      Category = 'File'
      Caption = 'Save &As...'
      Dialog.DefaultExt = '*.zgeproj'
      Dialog.Filter = 'Project files (*.zgeproj)|*.zgeproj'
      Dialog.Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
      Hint = 'Save As|Saves the active file with a new name'
      ShortCut = 24659
      BeforeExecute = FileSaveAsActionBeforeExecute
      OnAccept = FileSaveAsActionAccept
    end
    object RunExeAction: TAction
      Caption = 'Build and run'
      Hint = 'Run'
      ImageIndex = 8
      ShortCut = 120
      OnExecute = GenerateEXEClick
    end
    object GenerateReleaseLinuxAction: TAction
      Caption = 'Build Linux x86 binary'
      OnExecute = GenerateReleaseLinuxActionExecute
    end
    object FileSaveBinaryAsAction: TAction
      Category = 'File'
      Caption = 'Save binary as...'
      Visible = False
      OnExecute = SaveBinaryMenuItemClick
    end
    object GenerateScreenSaverAction: TAction
      Caption = 'Build Screensaver'
      OnExecute = GenerateScreenSaverActionExecute
    end
    object GenerateReleaseAction: TAction
      Caption = 'Build and compress Windows exe-file'
      OnExecute = GenerateReleaseActionExecute
    end
    object AboutAction: TAction
      Caption = 'About...'
      OnExecute = AboutActionExecute
    end
    object GenerateReleaseSSAction: TAction
      Caption = 'Build and compress Screensaver'
      OnExecute = GenerateReleaseSSActionExecute
    end
    object AppPreviewStartAction: TAction
      Caption = 'Start'
      Hint = 'Run application in preview window'
      ImageIndex = 8
      ShortCut = 32781
      OnExecute = AppPreviewStartActionExecute
    end
    object AppPreviewStopAction: TAction
      Caption = 'Stop'
      Enabled = False
      Hint = 'Stop application'
      ImageIndex = 9
      OnExecute = AppPreviewStopActionExecute
    end
    object FileNewWindowAction: TAction
      Category = 'File'
      Caption = 'New &Window'
      OnExecute = FileNewWindowActionExecute
    end
    object Import3dsAction: TAction
      Caption = 'Import 3D-Model...'
      OnExecute = Import3dsActionExecute
    end
    object GenerateReleaseOsx86Action: TAction
      Caption = 'Build Mac OS X Intel binary'
      OnExecute = GenerateReleaseOsx86ActionExecute
    end
    object ShowSettingsAction: TAction
      Caption = 'Settings...'
      OnExecute = ShowSettingsActionExecute
    end
    object AndroidBuildReleaseApkAction: TAction
      Caption = 'Android: Build APK (release)'
      OnExecute = AndroidBuildReleaseApkActionExecute
    end
    object FindComponentAction: TAction
      Category = 'Edit'
      Caption = 'Find component'
      ShortCut = 16454
      OnExecute = FindComponentActionExecute
    end
    object ShowCompilerDetailsAction: TAction
      Caption = 'Show compiler details'
      OnExecute = ShowCompilerDetailsActionExecute
    end
    object UndoDeleteAction: TAction
      Caption = 'Undo delete'
      OnExecute = UndoDeleteActionExecute
      OnUpdate = UndoDeleteActionUpdate
    end
    object ForceRefreshAction: TAction
      Caption = 'Refresh content'
      ShortCut = 116
      OnExecute = ForceRefreshActionExecute
    end
    object HelpContentsAction: TAction
      Caption = '&Contents'
      ShortCut = 112
      OnExecute = HelpContentsActionExecute
    end
    object EditXmlAction: TAction
      Category = 'Edit'
      Caption = 'Edit project as XML'
      Hint = 'Edit as XML'
      ImageIndex = 13
      ShortCut = 16461
      OnExecute = EditXmlActionExecute
    end
    object DisableComponentAction: TAction
      Caption = 'Disable component'
      ShortCut = 16452
      OnExecute = DisableComponentActionExecute
      OnUpdate = DisableComponentActionUpdate
    end
    object GenerateAndroidAction: TAction
      Caption = 'Android: Build zzdc.dat file'
      OnExecute = GenerateAndroidActionExecute
    end
    object AndroidRunAction: TAction
      Caption = 'Android: Run project'
      OnExecute = AndroidRunActionExecute
    end
    object AndroidBuildDebugApkAction: TAction
      Caption = 'Android: Build APK (debug)'
      OnExecute = AndroidBuildDebugApkActionExecute
    end
    object ImportBitmapAction: TAction
      Caption = 'Import Bitmaps...'
      OnExecute = ImportBitmapActionExecute
    end
    object ImportAudioAction: TAction
      Caption = 'Import Audio...'
      OnExecute = ImportAudioActionExecute
    end
  end
  object TreePopupMenu: TPopupMenu
    Images = ActionImageList
    Left = 168
    Top = 40
    object Addcomponent2: TMenuItem
      Action = AddComponentAction
    end
    object AddFromLibraryMenuItem: TMenuItem
      Caption = 'Add from library...'
      OnClick = AddFromLibraryMenuItemClick
      object TMenuItem
      end
    end
    object Import3DSfile1: TMenuItem
      Action = Import3dsAction
    end
    object N12: TMenuItem
      Caption = '-'
    end
    object DeleteComponentAction1: TMenuItem
      Action = DeleteComponentAction
    end
    object Undodelete1: TMenuItem
      Action = UndoDeleteAction
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object Findcomponent1: TMenuItem
      Action = FindComponentAction
    end
    object Copy1: TMenuItem
      Action = CopyComponentAction
      ImageIndex = 6
    end
    object Paste1: TMenuItem
      Action = PasteComponentAction
      ImageIndex = 7
    end
    object Moveup1: TMenuItem
      Action = MoveUpComponentAction
    end
    object Movedown1: TMenuItem
      Action = MoveDownComponentAction
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Refresh1: TMenuItem
      Action = ForceRefreshAction
    end
    object ResetComponentAction1: TMenuItem
      Action = ResetComponentAction
    end
    object Disablecomponent1: TMenuItem
      Action = DisableComponentAction
    end
    object Lockshow1: TMenuItem
      Action = LockShowAction
    end
  end
  object ActionImageList: TImageList
    ColorDepth = cd32Bit
    Left = 208
    Top = 96
    Bitmap = {
      494C01010F001100040010001000FFFFFFFF2000FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000004000000001002000000000000040
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000002177EFFF1A71E2FF2475ECFF2776
      F6FF2375E4FF2476E7FF226FE2FF206CE9FF1F6EF3FF226CEDFF1D5BDAFF245C
      E5FF1E67E3FF1E65DDFF195FDAFF2046D3FF0000000000000000000000000000
      000000000000000000000000000000000000000000000000011601040739020D
      15620418288805233AA412588DFC0A4A7BED0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000FF000000FF000000000000000000000000000000000000
      0000000000000000000000000000000000002875DDFF1258B2FF266DCEFF2677
      EBFF1B5BAFFF2B6BCAFF1E59B4FF1F62D9FF1E6FF0FF276CDCFF1D4CA5FF275A
      D6FF1D56B5FF1E4EA2FF16489FFF1441B7FF989898FF6F6F6FFF525252FF4F4F
      4FFF4D4D4DFF4A4A4AFF484848FF454545FF434343FF23659CFF3072A7FF3B7A
      AEFF4583B4FF4C89B9FF3C7CACFF0A4877EA0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000FF000000FF000000000000000000000000000000000000
      000000000000000000000000000000000000599FF1FF92B4DBFF225FB4FF5292
      E2FF97C0E0FF5F97D8FF8FB1DDFF1F60D1FF176DF5FF5B94EBFF8CA7D5FF3A67
      CDFF9BBDE9FFACBDDCFFACBBD9FF4160BAFF0000000000000000565656FFA1A1
      A1FFA1A1A1FFA2A2A2FFA3A3A3FFA3A3A3FFA4A4A4FF2D6DA4FF76AAD2FF76AA
      D3FF71A6D1FF679FCDFF3E7DADFF0C4A79EA0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000FF000000FF00000000000000000000000000000000840000FF8400
      00FF840000FF0000000000000000000000005698EDFFF0F6F8FF3F6DAFFF92B8
      E2FFDBEEF6FF70A2D6FFDDEAF7FF2664CAFF1665E4FF7DA9E9FFD2DCEDFF4772
      C7FFEDF9FEFFF9FBFEFFF7F7FFFF5C77C7FF00000000000000005A5A5AFF9F9F
      9FFF3A713EFFA1A1A1FFA2A2A2FFA2A2A2FFA3A3A3FF3472A9FF7BAED4FF5999
      C9FF5294C7FF5695C8FF3F7EADFF0F4C7CEA0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000FF00000000000000000000
      0000000000FF000000FF00000000000000000000000000000000840000FF8400
      00FF840000FF0000000000000000000000003781E9FFEAF1F6FF7293BAFFBFD2
      E6FFBFD0E6FF699BDCFFE4EDF2FF3065BEFF1F56B1FF81A4D9FFDAE2EAFF4677
      C9FFF5FAFAFFBFD5EBFFB1C9F0FF4672D8FF00000000000000005E5E5EFF3875
      3DFF3B743FFFA0A0A0FFA1A1A1FFA1A1A1FFA2A2A2FF3B77AFFF81B2D7FF609E
      CCFF5899C9FF5C9ACAFF4180AEFF144F7FEA0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000FF000000FF000000000000
      00FF000000FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000002374F1FFBDD5F5FFB7C8D8FFE3ED
      F7FF88A8E2FF6197E8FFE0ECF7FF5680C1FF8DAAD3FF7CA0CBFFD7E3EDFF4375
      CCFFF1F9FCFF4D7CBEFF2E65D3FF1E58DDFF000000000000000037743CFF4B94
      52FF47904EFF266C2BFF246828FF216425FF1F6123FF437CB3FF87B6D9FF65A2
      CFFF5F9DCCFF619ECCFF4382B0FF195382EA0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000FF000000FF000000FF0000
      00FF000000FF0000000000000000000000000000000000000000840000FF8400
      00FF840000FF0000000000000000000000002070F4FF89B3EDFFF2F9F9FFEBF9
      FCFF4B85E0FF5E97E9FFE0EBF8FF839CC6FFD8E5F1FF809EC1FFD3E1EFFF4674
      CCFFF2F8FEFF4074C8FF1F5BE3FF1D57E7FF0000000026592BD4549C5BFF7EC5
      87FF79C282FF75C07DFF70BD77FF6DBB73FF226526FF4A83B9FF8CBADBFF6CA7
      D1FF64A5D1FF5DB3DFFF4584B0FF1E5687EA0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF0000000000000000840000FF8400
      00FF840000FF0000000000000000000000002271F3FF5B93E5FFF4FAF6FFDAEB
      F3FF296CD8FF5F98E8FFE5EDF7FFA8B7CFFFF0F7F8FF95ADC6FFD2E0EFFF4875
      CAFFF4F8FEFF4271C5FF275DE4FF2358E6FF18361BA15CA464FF8ACC93FF7BC4
      85FF71BF7AFF6CBC75FF67BA6FFF73BE7AFF256A2AFF5288BEFF93BEDDFF73AC
      D4FF61B7E1FF49D4FFFF408AB7FF235B8AEA0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000000000000000000000000000008400
      00FF840000FF840000FF00000000000000001B6FF0FF377ADFFFEDF5FCFFB7CA
      DDFF205ECCFF5D96ECFFE5ECF3FFC7D5DDFFEFF6F9FFBAC6D5FFD4E0EDFF3F76
      C9FFEFF9FEFF4170C6FF215EE7FF215AE4FF00000000295D2FD25DA565FF8CCD
      95FF88CB91FF83C88CFF7EC587FF79C282FF286E2EFF588DC3FF97C2E0FF7AB2
      D7FF72AED6FF5CC3EDFF4987B2FF2A5F8FEA0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000FF000000FF000000FF0000
      00FF000000FF000000FF00000000000000000000000000000000000000000000
      0000840000FF840000FF840000FF000000001B6FF0FF518CDEFFF7F9F7FFD5DF
      EBFF2056B6FF5D96EAFFEFF1F5FFE6EFF3FFB7CEE6FFE4EBF1FFD9E2EDFF3F76
      CAFFF0FAFEFF4371C5FF2261E8FF205CE6FF000000000000000045884DFF5EA6
      67FF5BA363FF35813CFF327C39FF2F7735FF2C7332FF5E91C9FF9DC7E2FF82B7
      DAFF7BB3D7FF7CB2D7FF4D88B3FF306494EA0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000FF000000FF000000FF0000
      00FF000000FF0000000000000000840000FF840000FF840000FF000000000000
      000000000000840000FF840000FF840000FF1D6EF0FF83ACE5FFFAF9F8FFF1F7
      F8FF3B64AFFF5B94E4FFF8F8F9FFE9F1F7FF76A7DFFFF9FBF8FFDDE3ECFF3F74
      CDFFF2FAFDFF4371C6FF2365E8FF205EE8FF0000000000000000757575FF4B8F
      52FF3B8943FF9A9A9AFF9B9B9BFF9C9C9CFF9C9C9CFF6495CCFFA1CBE3FF88BC
      DCFF82B8DAFF83B8DAFF4F8AB4FF376998EA0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000FF000000FF000000FF0000
      00FF000000000000000000000000840000FF840000FF840000FF000000000000
      000000000000840000FF840000FF840000FF246DEBFFB1CEEEFFD6DBE9FFE6F2
      F7FF6F8EBEFF5A91DCFFF9FAFBFFCBD8EDFF4F93E8FFF7FAF9FFDCE3EDFF3E74
      CFFFF2FAFDFF4171C8FF2166E9FF2060E9FF0000000000000000787878FF9798
      97FF509057FF999999FF9A9A9AFF9B9B9BFF9B9B9BFF6A99D0FFA6CEE5FF8EC0
      DFFF88BCDCFF8ABCDCFF518CB5FF3E6D9EEA0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000FF000000FF000000FF0000
      0000000000000000000000000000840000FF840000FF840000FF000000000000
      000000000000840000FF840000FF840000FF3070E2FFD7EDFAFF97AFDDFFBCD9
      F1FFAABDD9FF558BD0FFF5FCFEFFA2BCE5FF3280EAFFE8F1FBFFD7E1F0FF3C76
      D1FFEEFAFDFF4173CFFF1D68EBFF1F62EBFF00000000000000007B7B7BFF9898
      98FF989898FF999999FF999999FF9A9A9AFF9A9A9AFF6D9CD3FFA9D1E7FFAAD1
      E7FF97C7E1FF90C1DEFF548EB6FF4372A1EA0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000FF000000FF000000000000
      000000000000000000000000000000000000840000FF840000FF840000FF8400
      00FF840000FF840000FF840000FF00000000538BEAFFE5F8F9FF608FE4FF85B6
      EAFFDEEBF9FF5F94D2FFECFAFEFF7AA7EAFF2672EBFFC8DBF8FFD2E4F6FF3F80
      D8FFEBF9FEFF467CDDFF1B6BEFFF1F64EDFF00000000000000007E7E7EFF7C7C
      7CFF7A7A7AFF787878FF757575FF737373FF707070FF6F9DD4FF6D9DD6FF86B1
      DCFFAAD3E8FFA8D0E6FF568FB7FF4A76A6EA0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000FF00000000000000000000
      00000000000000000000000000000000000000000000840000FF840000FF8400
      00FF840000FF840000FF0000000000000000538CF0FFA2C5EFFF3977EBFF4C8F
      E7FFA5C5F2FF5B98E7FFA2C8F3FF4F8DECFF236FEEFF80A8EDFF91B8EEFF3B84
      E3FFA1C8F4FF3F81EAFF1D6CF3FF1F66EFFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000005174
      9EDB6B9BD4FF84B0DAFF5890B8FF4F7BAAEA0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000002666EBFF296BEBFF236DF0FF2471
      ECFF2774E8FF2976EDFF2976EDFF2370EEFF216EF2FF286FEEFF2571E6FF2A7D
      EBFF277BEDFF2473F3FF226CF5FF2068F1FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000001F2C3D896495CBFB5783B4EE0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000349DD9FF2F99D8FF2A94
      D7FF2690D6FF218CD5FF1C88D4FF1884D3FF1380D2FF0F7AD1FF0C77D1FF0874
      D0FF0571CFFF026ECFFF006CCEFF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000010201230E22117D296531DB307C38F32C7A35F3226027DB0A1E0C7D0002
      0023000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000003BA3DAFFBCEBFAFFBCEB
      FCFFBFEEFEFFC6F4FFFFCEF8FFFFD3FAFFFFD0F8FFFFC7F2FFFFBAE9FCFFB3E4
      F9FFB0E2F8FFB0E2F8FF036FCFFF000000000000000000000000000000000000
      000003020123291A0F7D7C512DDB976135F3965F33F3794C27DB27180C7D0301
      002300000000000000000000000000000000000000000000000000000000070F
      0953367942E63F984EFF7BC18EFF95D0A5FF95CFA5FF76BD88FF348C40FF2367
      29E6040D04530000000000000000000000000000000000000000000000006E5E
      D0FF1E0EC0FF1E0EC0FF1E0EC0FF1E0EC0FF1E0EC0FF1E0EC0FF1E0EC0FF1E0E
      C0FF1E0EC0FF6E5ED0FF00000000000000000000000041A8DBFFBFECFBFF57CF
      F5FF3FB0ECFF4CBAEFFF58C2EFFF5EC6EFFF5AC4EFFF4AB6EFFF35A5E6FF289A
      E1FF36B8EEFFB1E3F8FF0773D0FF00000000000000000000000000000000130D
      085391633DE6D7BAA2FFE9DACAFFECE0D1FFECE0D1FFE8D8C8FFD3B49BFF8757
      2DE6110B0553000000000000000000000000000000000000000008100A53458E
      53F462B376FFA7DBB4FF86CC97FF64BB7BFF62B97AFF85CB97FFA4D9B3FF56A9
      69FF27742EF4040D045300000000000000000000000000000000000000001E0E
      C0FFA8A8F8FFA8A8F8FFA8A8F8FFA8A8F8FFA8A8F8FFA8A8F8FFA8A8F8FFA8A8
      F8FFA8A8F8FF1E0EC0FF00000000000000000000000047ADDCFFC1EEFBFF5DD3
      F7FF6ADBFCFF7DE5FFFF8FEDFFFF97F2FFFF93EDFFFF7ADFFFFF59CCF8FF44BE
      EFFF3ABAEEFFB3E3F9FF0C77D1FF000000000000000000000000130E0A53AA7A
      51F4E7D5C3FFE5D2BEFFC9A584FFB78D65FFB58963FFC4A07EFFE0CCB9FFE3D0
      BDFF9C6539F4110B065300000000000000000000000001020122438251E568B8
      7BFFA7DBB1FF5EBB75FF3D794CFFFFFFFFFF57B46DFF56B46DFF59B672FFA4D9
      B2FF58A96AFF226729E500020022000000000000000000000000000000001E0E
      C0FF9898D8FF3E36E8FF3E36E8FF3E36E8FF3E36E8FF3E36E8FF3E36E8FF3E36
      E8FF9898D8FF1E0EC0FF0000000000000000000000004CB2DDFFC3EFFBFF63D6
      F8FF4AB6ECFF58BDEFFF95EBFFFF2E97DDFF4B82ABFF84E1FFFF3FA9E9FF309F
      E1FF40BEEFFFB4E5F9FF117CD2FF0000000000000000030201229C7554E5EAD8
      C9FFE3CDB9FFBF9369FFB98B60FFCFAF93FFCFAF93FFB6885DFFB1865FFFDABF
      A9FFE4D1BFFF8B5D37E503010122000000000000000016281B7E51AA66FFA9DD
      B3FF62C077FF5DBD6FFF3C7A48FFFFFFFFFFFFFFFFFF57B76EFF56B46CFF5AB6
      72FFA5DAB3FF368E41FF0A1F0C7E000000000000000000000000000000003626
      C8FF7676F0FF362EE8FF362EE8FF362EE8FF362EE8FF362EE8FF362EE8FF362E
      E8FF7676F0FF3626C8FF00000000000000000000000051B7DEFFC6F0FCFF68D9
      F8FF7AE2FDFF90E8FFFF99E9FFFF309FDFFF528BB2FF8AE2FFFF68D0F9FF4EC5
      F1FF44C1F0FFB6E7F9FF1683D3FF000000000000000031261D7EE4CCB8FFEAD6
      C4FFC7986FFFBE8F64FFBE8F64FFF7F1ECFFF6F0EAFFB6885DFFB6885DFFB488
      61FFE2CEBAFFD9BCA5FF2A1D127E0000000000000000467E55DB89CC97FF88D3
      95FF69C578FF61C06EFF3F7D47FFFFFFFFFFFFFFFFFFFFFFFFFF57B76EFF59B8
      70FF84CC96FF79BD8CFF226029DB000000000000000000000000000000003636
      B8FF6E66F0FF2E1EE8FF2E1EE8FF2E1EE8FF2E1EE8FF2E1EE8FF2E1EE8FF2E1E
      E8FF6E66F0FF3636B8FF00000000000000000000000056BBDFFFC7F1FCFF6DDC
      F9FF54BBEDFF5FBDEFFF9BE7FFFF33A6E2FF49A4E1FF90E2FFFF47ADE9FF36A4
      E3FF47C4F0FFB8E8F9FF1C88D4FF00000000000000009B7B60DBEFE1D3FFD9B4
      94FFC7976AFFC29467FFC09265FFBE8F64FFBE8F64FFBA8A61FFB88961FFB789
      60FFCBA685FFEADCCCFF875F3FDB00000000000000005CA270F6A8DDB2FF7BCF
      89FF73CC80FF73CC80FF4B8552FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF57B7
      6EFF65BD7BFF9BD4AAFF2F7D37F6000000000000000000000000000000003636
      B8FF7676F0FF362EE8FF362EE8FF362EE8FF362EE8FF362EE8FF362EE8FF362E
      E8FF7676F0FF363EC8FF0000000000000000000000005ABFE0FFC8F3FCFF73DF
      F9FF89E6FDFF95E7FFFF9AE5FFFFAAEEFFFFA8EDFFFF99E3FFFF72D5F9FF57CC
      F3FF4DC8F1FFBBE9FAFF228DD5FF0000000000000000C9A384F6F2E4D9FFD1A4
      78FFC49869FFC39668FFC39567FFFAF6F2FFF3EAE1FFC1946BFFBD8E63FFBD8E
      62FFBF946BFFEFE3D5FFB08159F6000000000000000060A574F6B5E2BDFF8AD5
      96FF78C985FF78C985FF4E8356FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF57B7
      6EFF67C07CFF9CD4A9FF32803CF600000000000000000000000000000000363E
      C8FF9898D8FF463EE8FF463EE8FF463EE8FF463EE8FF463EE8FF463EE8FF463E
      E8FF9898D8FF363EC8FF0000000000000000000000005EC2E1FFC9F3FCFFCBF3
      FDFFD4F6FEFFD7F6FFFFD8F4FFFFE0F8FFFFDFF8FFFFDAF5FFFFCDF1FCFFC2ED
      FAFFBDEBFAFFBDEBFAFF2993D6FF0000000000000000D0AC8EF6F2E5DAFFD1A5
      7CFFCC9C6FFFC7996AFFC49769FFE2CCB5FFF8F3EEFFF6EEE8FFD9BCA0FFC193
      66FFC49A6FFFF0E2D6FFB68A62F600000000000000004F855FDBABDDB5FFA5DF
      AEFF80CB8BFF7AC985FF4F8356FFFFFFFFFFFFFFFFFFFFFFFFFF5EBB75FF5AB9
      71FF8AD198FF7EC491FF2B6733DB00000000000000000000000000000000363E
      C8FF9898D8FF4E46E8FF4E46E8FF4E46E8FF4E46E8FF4E46E8FF4E46E8FF4E46
      E8FF9898D8FF363EC8FF0000000000000000000000005FC3E1FF88A0A8FF9191
      91FF8E8E8EFF58B9DCFF53B8DFFF4FB5DEFF4BB1DDFF47ADDCFF44A8D7FF7676
      76FF747474FF637C8DFF2F99D8FF0000000000000000AA8E77DBF3E5D9FFDFBA
      9DFFCF9F73FFCD9D70FFF5EBE3FFE4CBB3FFE7D3BEFFFBF8F6FFE5D3BEFFC397
      69FFD6B390FFEEE0D2FF967457DB00000000000000001B2C207E84C796FFD2EE
      D7FF94D99FFF89D393FF58895FFFFFFFFFFFFFFFFFFF77CD84FF69C27AFF6DC7
      7CFFABDFB4FF439D55FF0F22127E000000000000000000000000000000003E4E
      C8FFA8A8F8FF4E4EE8FF4E4EE8FF4E4EE8FF4E4EE8FF4E4EE8FF4E4EE8FF4E4E
      E8FFA8A8F8FF3E4EC8FF00000000000000000000000000000000999999FFC6C6
      C6FF949494FF8F8F8FFF000000000000000000000000000000007C7C7CFF7B7B
      7BFFABABABFF747474FF0000000000000000000000003930297EF4E3D4FFEFDC
      CDFFD5A77CFFD09F75FFFBF8F5FFFCF8F5FFFCF8F5FFFBF8F5FFD1A780FFCFA3
      79FFEAD5C2FFEAD4C1FF33281F7E00000000000000000103022259936BE5A9DA
      B6FFD8F1DCFF91D89CFF5E8D65FFFFFFFFFF8AD495FF89D494FF82D28DFFAEE0
      B6FF69B87BFF397A43E501020122000000000000000000000000000000003E4E
      C8FFA8B0F8FFA8B0F8FFA8B0F8FFA8B0F8FFA8B0F8FFA8B0F8FFA8B0F8FFA8B0
      F8FFA8B0F8FF3E4EC8FF000000000000000000000000000000009D9D9DFFC4C4
      C4FFA1A1A1FF939393FF00000000000000000000000000000000828282FF8989
      89FFA9A9A9FF777777FF00000000000000000000000004030322C1A690E5F6E9
      DDFFECD8C5FFD7AB80FFDCBA99FFF6ECE3FFF5ECE2FFE4C8ADFFD2A679FFE6CE
      B9FFF1E2D5FFB19074E5030302220000000000000000000000000B130E5365A7
      7AF4AEDCBAFFDCF2E0FF7D9F83FF9ADBA4FF95D99FFFA4DFAEFFBFE8C4FF77C1
      89FF488F55F40710095300000000000000000000000000000000000000009898
      D8FF4656C8FF4656C8FF4656C8FF4656C8FF4656C8FF4656C8FF4656C8FF4656
      C8FF4656C8FF9898D8FF00000000000000000000000000000000A1A1A1FFBABA
      BAFFBFBFBFFF989898FF949494FF919191FF8E8E8EFF8A8A8AFF878787FFA8A8
      A8FF9E9E9EFF7B7B7BFF0000000000000000000000000000000019161353DEC0
      A7F4F7EADFFFEEDED0FFE3C0A6FFD8AD88FFD7AB85FFDDBA9BFFEBD6C7FFF3E6
      D9FFCFAD90F41713105300000000000000000000000000000000000000000B13
      0E535A956CE693CEA3FFC2E6CBFFCFEBD4FFC9E9CEFFAEDDB7FF6BB87DFF4685
      54E608100A530000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000A4A4A4FFA1A1
      A1FFC4C4C4FFBEBEBEFFA1A1A1FF969696FF939393FF979797FFAEAEAEFFAEAE
      AEFF848484FF818181FF00000000000000000000000000000000000000001916
      1453C7AE99E6F9E9DCFFF6E8DDFFF3E5DAFFF3E5DAFFF5E7DCFFF5E4D6FFBFA2
      8BE6181411530000000000000000000000000000000000000000000000000000
      0000020302231A2B207D508560DB5EA272F35CA06FF3478056DB16281B7D0103
      0223000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000A4A4
      A4FFA2A2A2FFBCBCBCFFCACACAFFCCCCCCFFCACACAFFC2C2C2FFADADADFF8C8C
      8CFF898989FF0000000000000000000000000000000000000000000000000000
      0000040403233B342E7DB69F8DDBDFC3ABF3DEC1A9F3B39B88DB39312B7D0403
      0323000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000A5A5A5FFA3A3A3FFA0A0A0FF9D9D9DFF9A9A9AFF979797FF949494FF9090
      90FF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000919898FF919898FF919898FF919898FF919898FF919898FF9198
      98FF919898FF919898FF919898FF919898FF0000000000000000000000006590
      B5FF618DB3FF5A89B1FF5484AFFFAE9487FFAE9587FFAD9587FFAD9487FFAD94
      87FFAD9587FFAD9487FFAD9487FF000000000000000000000000000000000403
      0F41000000080000000000000000000000000000000000000000000000000000
      000801010E4100000000000000000000000074848FFF65808FFF5F7887FF566D
      7AFF4D616EFF435560FF384751FF2D3942FF242D34FF1A2128FF13181DFF0D11
      15FF0D1217FF00000000000000000000000000000000919898FF919898FF9198
      98FF919898FF90999AFFF7E7DCFFF7E7DCFFF7E7DDFFF7E7DCFFF7E7DDFFF7E7
      DCFFF7E7DDFFF7E7DDFFF7E7DCFF909A9AFF00000000000000006896BCFF6AAE
      D8FF6FC5F1FF6FC6F2FF6FC6F1FFAA988CFFF8E8DEFFF7E9DFFFF8E8DFFFF7E9
      DEFFF7E9DEFFF7E8DEFFAA988DFF00000000000000000000000004040F414D4A
      F2FF3E3CE9FD0000000800000000000000000000000000000000000000082220
      DEFC2F2DEAFF02010E410000000000000000768792FF89A1ABFF69B2D4FF008F
      CDFF008FCDFF008FCDFF038CC7FF0788BEFF0E82B4FF147BA9FF1A769FFF1E71
      96FF20495AFE0204044A00000000000000000000000090999AFFF7E7DCFFF7E7
      DCFFF7E7DDFF8D9C9DFFF8E9DFFFC7A591FFC19E89FFBA937CFFB1886FFFA87C
      64FFF7E8DFFFF7E8DEFFF8E8DEFF8D9B9EFF00000000000000006998BEFF77C8
      F1FF77C9F2FF77C8F2FF77C9F2FFA69C92FFF8EAE1FFF7EBE2FFF8EAE2FFF8EB
      E2FFF8EAE1FFF8EAE2FFA69B92FF000000000000000005050F415654F5FF615F
      FAFF5653F6FF3E3CE7FC000000080000000000000000000000082928E0FC3F3D
      F1FF4A48F6FF2F2DEAFF02010E4100000000798A95FF7DBED3FF8AA4AEFF7DDC
      FFFF5ECFFFFF54CBFFFF4BC4FAFF40BCF5FF36B3F0FF2DAAEBFF23A0E5FF128C
      D4FF226680FF0D1517B40000000000000000000000008D9C9DFFF8E9DFFFC7A5
      91FFC19E89FF8A9EA1FFF8EAE1FFF8EAE1FFF8EAE1FFF8EAE1FFF8EAE1FFF8EA
      E1FFF8EAE1FFF8EAE1FFF8EAE1FF8A9EA2FF00000000000000006A9BC1FF83CC
      F2FF83CDF2FF83CCF3FF83CCF3FFA1A099FFF8EDE6FFC7A591FFBC9781FFAD84
      6BFFA3745CFFF8EDE5FFA19F9AFF00000000000000000202062B5956F6FF6360
      FAFF6F6EFFFF5754F6FF3F3DE8FC0000000800000008322FE3FC4543F2FF6160
      FFFF4846F4FF2D2BE9FF0100062B000000007C8E98FF78D2ECFF8BA4ADFF89C2
      CEFF70D8FFFF64D3FFFF5BCEFFFF50C9FEFF48C1FAFF3EB9F5FF33B0EEFF28A8
      E9FF0F85CDFF214A5AFF0000012700000000000000008A9EA2FFF8EAE1FFF8EA
      E1FFF8EAE1FF87A0A6FFF9ECE4FFC7A591FFC3A08CFFBF9A84FFB8927AFFB289
      71FFAC8268FFA77861FFF9ECE4FF87A1A6FF00000000000000006D9EC4FF8ED1
      F2FF8ED0F2FF8ED0F3FF8ED0F3FF9CA4A1FFF9F0EAFFF9F0EAFFF9F0EAFFF9F0
      EAFFF9F0EAFFF9F0EAFF9CA4A1FF0000000000000000000000000202062B5957
      F6FF6461FAFF726FFFFF5856F6FF3F3EE8FC3C3AE8FD4E4BF4FF6665FFFF4E4C
      F5FF3432EBFF0101062B000000000000000080919CFF81D7EFFF7CC5E0FF8CA6
      B0FF80DDFEFF67D3FFFF66D4FFFF61D1FFFF57CDFFFF4DC7FCFF45BEF7FF3AB6
      F2FF30ACECFF246881FF06151C90000000000000000087A1A6FFF9ECE4FFC7A5
      91FFC3A08CFF85A3AAFFF9EEE6FFF9EEE7FFF9EEE7FFF9EEE7FFF9EEE7FFF9EE
      E7FFF9EEE7FFF9EEE6FFF9EEE7FF84A3ABFF000000000000000070A2C7FF9AD5
      F3FF9AD4F3FF99D5F4FF99D4F4FF97A8A8FFFAF3EFFFC7A591FFBC9781FFAD84
      6BFFA3745CFFF9F2EEFF97A8A8FF000000000000000000000000000000000202
      062B5A58F6FF6562FAFF7270FFFF716EFFFF6E6CFFFF6C6AFFFF5553F7FF3D3B
      EEFF0101062B00000000000000000000000083959FFF89DCF1FF8CE2FFFF8DA8
      B1FF8CBAC7FF73D8FFFF66D4FFFF66D4FFFF66D4FFFF5ED0FFFF53CDFFFF4AC5
      FCFF40BBF7FF2DA2DBFF3F5360F10202032B0000000084A4AAFFF9EEE6FFF9EE
      E7FFF9EEE7FF81A6B0FFF9F0EAFFC7A591FFC3A08CFFBF9A84FFB8927AFFB289
      71FFAC8268FFA77861FFF9F0EAFF81A7B0FF000000000000000072A5CAFFA5D8
      F5FFA5D8F4FFA5D9F4FFA5D9F4FF92ACB0FFFBF6F3FFFBF5F2FFFAF6F2FFFAF6
      F3FFFAF6F3FFFAF5F3FF92ADAFFF000000000000000000000000000000000000
      00000202062B5B59F7FF7774FFFF5754FFFF5552FFFF706EFFFF4644F0FF0101
      062B00000000000000000000000000000000869AA3FF92E1F2FF98E8FDFF80C4
      DEFF8EA7B0FF81DEFDFF84E0FFFF84E0FFFF84E0FFFF84E0FFFF81DFFFFF7ADD
      FFFF73D8FFFF6AD6FFFF55A9D1FF1F272CA20000000081A7B0FFF9F0EAFFC7A5
      91FFC3A08CFF7BAAB4FFFAF2EDFFFAF2EDFFFAF2EEFFFAF2EDFFFAF2EDFFFAF2
      EDFFFAF2EDFFFAF2EDFFFAF2EDFF7CAAB5FF000000000000000074A9CEFFB0DD
      F4FFB0DDF5FFB0DDF4FFB0DDF5FF8EB0B6FFFAF8F5FFC7A591FFA3745CFFFAF7
      F6FF8FC8E0FF7ABDDAFF8EB0B6FF000000000000000000000000000000000000
      0000000000085956F2FD7B77FFFF5C59FFFF5956FFFF7472FFFF4340EBFD0000
      000800000000000000000000000000000000889CA5FF9AE6F3FF9FEBFBFF98E8
      FEFF8BACB9FF8BACB9FF8AAAB7FF88A6B3FF86A3AFFF839FAAFF819AA6FF7E95
      A1FF7B919DFF798E99FF788B95FF768893FF000000007CAAB5FFFAF2EDFFFAF2
      EDFFFAF2EEFF78ACB9FFFBF5F0FFC7A591FFC3A08CFFBF9A84FFB8927AFFB289
      71FFAC8268FFA77861FFFBF4F0FF78ACB9FF000000000000000076ACD0FFBAE1
      F5FFBAE1F5FFBAE1F6FFBAE1F5FF8AB4BBFFFBF9F8FFFBF9F8FFFBF9F8FFFBF9
      F8FF7ABDDAFF8EB0B6FF00000000000000000000000000000000000000000000
      0008625FF3FC6E6BFBFF7E7CFFFF7C79FFFF7A77FFFF7775FFFF5C5AF7FF4340
      E9FC000000080000000000000000000000008BA0A8FFA0EAF6FFA6EEF9FF9FEB
      FBFF98E8FEFF79DAFFFF66D4FFFF66D4FFFF66D4FFFF66D4FFFF66D4FFFF66D4
      FFFF768893FF0000000000000000000000000000000079ADBAFFFBF5F0FFC7A5
      91FFC3A08CFF76AFBEFFFBF5F2FFFBF6F3FFFBF6F2FFFBF6F3FFFBF5F3FFFBF6
      F3FFFBF6F3FFFBF6F3FFFBF6F3FF75AFBEFF000000000000000078AED3FFC2E4
      F6FFC2E3F6FFC2E4F5FFC2E4F6FF8AB4BBFF8AB4BBFF8AB4BBFF8AB4BBFF8AB4
      BBFF8AB4BBFF629AC3FF00000000000000000000000000000000000000086A67
      F6FC7572FDFF8581FFFF7471FCFF6260F8FF5E5BF7FF6B68FAFF7977FFFF5E5B
      F7FF4441E9FC0000000800000000000000008EA2ABFFA7EEF6FFABF0F7FFA6EE
      F9FF9FEBFBFF98E8FDFF70D4FBFF899EA7FF8699A3FF82949FFF7D909AFF798C
      97FF768893FF0000000000000000000000000000000075AFBDFFFBF5F2FFFBF6
      F3FFFBF6F2FF73B2C1FFFBF8F5FFC7A591FFBC9781FFAD846BFFA3745CFFFBF8
      F6FF8FC8E0FF7ABDDAFF66B2D4FF73B1C1FF000000000000000079B0D4FFC8E6
      F6FFC8E6F6FFC8E6F6FFC8E6F6FFC8E6F6FFC8E6F6FFC8E6F6FFC8E6F6FFC8E6
      F6FFC8E6F6FF73A8CCFF00000000000000000000000000000008706DFAFD7B78
      FEFF8986FFFF7A77FDFF6A67FBFF0202072B0202072B5F5CF8FF6C6AFAFF7B78
      FFFF5F5DF7FF4542EAFC00000005000000008FA4ACFFA0D2DAFFABF0F7FFABF0
      F7FFA6EEF9FF9FEBFBFF8DA1AAFF222A2D820000000000000000000000000000
      0000000000008E6952FF91674EFF93664BFF0000000072B2C2FFFBF8F5FFC7A5
      91FFBC9781FF71B4C4FFFBF9F7FFFCF9F8FFFBF9F8FFFBF9F7FFFCF9F8FFFCF9
      F8FF7ABDDAFF67B2D5FF71B4C4FF00000000000000000000000079B0D4FFC8E6
      F6FF96715EFFB49481FFB49481FFB49480FFB49481FFB49481FFB49481FF9671
      5DFFC8E6F6FF77ACCEFF0000000000000000000000000101031F7875FFFF807C
      FFFF807CFEFF726FFDFF0303072B00000000000000000202072B605DF8FF6D6B
      FBFF7C7AFFFF605DF8FF0D0D2D6F00000002101314578FA4ACFF8FA4ACFF8FA4
      ACFF8FA4ACFF8FA4ACFF2933378D000000000000000000000000000000000000
      0000000000000D0B094D8D6A53FF90684EFF0000000071B3C4FFFBF9F7FFFCF9
      F8FFFBF9F8FF6FB5C7FFFCFAF9FFFCFAF9FFFCFAF9FFFCFAF9FFFCFAF9FFFCFA
      F9FF67B2D4FF6FB5C7FF00000000000000000000000000000000000000005F9A
      C5FF5F9AC5FFA0826FFFC0A798FFC0A797FFC0A797FFC0A798FFA0826FFF5081
      ADFF5F9AC5FF00000000000000000000000000000000000000000101031F7875
      FFFF7774FEFF0303072B000000000000000000000000000000000202072B625F
      F8FF6866F9FF242269A802010629000000000000000000000000000000000000
      0000000000000000000000000000000000000000000095735FFF120D0A5C0000
      000001000015A69182FF110E0C5594735EFF000000006FB5C7FFFCFAF9FFFCFA
      F9FFFCFAF9FF6FB5C7FF6FB5C7FF6FB5C7FF6FB5C7FF6FB5C7FF6FB5C7FF6FB5
      C7FF6FB5C7FF0000000000000000000000000000000000000000000000000000
      000000000000AA9182FF00000000000000000000000000000000AA9182FF0000
      0000000000000000000000000000000000000000000000000000000000000101
      031F0303072B0000000000000000000000000000000000000000000000000202
      072B11102E6F05050E3E00000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000B0A0943A69181FFA189
      77FFAA9587FF25201D7C0000000000000000000000006FB5C7FF6FB5C7FF6FB5
      C7FF6FB5C7FF6FB5C7FF6FB5C7FF6FB5C7FF6FB5C7FF6FB5C7FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000B09B8EFF0000000000000000B09B8EFF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000A0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000003A20108F5D341AB59F5A2DEEB666
      33FFB46633FFB36532FFB16432FFAF6331FFAD6231FFAB6130FFA96030FFA85F
      30FFA75E2FFFA45E2FFE93542AF162381CC40000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000E69A6FF0E69A6FF0E69A6FF0E69A6FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000E69A6FF0E69A6FF00000000000000000000
      0000000000000000000000000000000000008C4E27DEEBC5ACFFEAC4ACFFFEFB
      F8FFFEFBF8FFFEFBF8FFFEFBF8FFFEFBF8FFFEFBF8FFFEFBF8FFFEFBF8FFFEFB
      F8FFFEFBF8FFC8997AFFC79777FF8F5129ED0000000000000000000000000000
      00000000000000000000527433FF527433FF527433FF527433FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000E6AA7FF83DDFBFF83DDFBFF83DDFBFF83DDFBFF0E6AA7FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000E6AA7FF83DDFBFF83DDFBFF0E6AA7FF000000000000
      000000000000000000000000000000000000B76935FEEDCAB2FFE0A178FFFEFA
      F7FF60BF87FF60BF87FF60BF87FF60BF87FF60BF87FF60BF87FF60BF87FF60BF
      87FFFDF9F6FFCA8C63FFC99A7AFFA45E2FFE0000000000000000000000000000
      00000000000000000000557836FF52B48DFF51B48DFF557736FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000F6CA9FF83DDFBFF83DDFBFF83DDFBFF83DDFBFF0E6CA9FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000E6CA8FF83DDFBFF83DDFBFF83DDFBFF83DDFBFF0E6CA8FF0000
      000000000000000000000000000000000000BA6A36FFEECCB5FFE1A178FFFEFA
      F7FFBEDCC1FFBEDCC1FFBEDCC1FFBEDCC1FFBEDCC1FFBEDCC1FFBEDCC1FFBEDC
      C1FFFDF9F6FFCD8F66FFCC9D80FFA75F30FF0000000000000000000000000000
      00000000000000000000587C3AFF52B48DFF51B48DFF587C39FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000F6EAAFF83DDFBFF83DDFBFF83DDFBFF83DDFBFF0F6EABFF0000
      0000000000000000000000000000000000000000000000000000000000000000
      00000F6EABFF8ADFFBFF8ADFFBFF8ADFFBFF8ADFFCFF8ADFFBFF8ADFFCFF106E
      ABFF00000000000000000000000000000000BA6936FFEFCEB7FFE1A177FFFEFA
      F7FF60BF87FF60BF87FF60BF87FF60BF87FF60BF87FF60BF87FF60BF87FF60BF
      87FFFDF9F6FFCF9268FFCEA283FFA95F30FF0000000000000000000000000000
      000000000000000000005C853EFF53B996FF54B996FF5C853EFF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000001070ADFF83DDFBFF83DDFBFF83DDFBFF83DDFBFF1070ADFF0000
      0000000000000000000000000000000000000000000000000000000000000F71
      ACFF93E2FCFF94E2FCFF93E1FBFF94E2FCFF94E2FBFF93E2FBFF93E2FBFF94E2
      FCFF1070ADFF000000000000000000000000B96834FFEFD0BAFFE2A178FFFEFB
      F8FFFEFBF8FFFEFBF8FFFEFBF8FFFEFBF8FFFEFBF8FFFEFBF8FFFEFBF8FFFEFB
      F8FFFEFBF8FFD3956BFFD2A689FFAA6030FF0000000000000000608B42FF618B
      43FF608C43FF608C43FF56C0A0FF56C0A1FF56C0A0FF56C0A0FF608C43FF608B
      43FF618C43FF618C43FF00000000000000000000000000000000000000000000
      0000000000001173AFFF83DDFBFF83DDFBFF83DDFBFF83DDFBFF1073AFFF0000
      00000000000000000000000000000000000000000000000000001173AFFF9FE5
      FBFF9FE5FBFF9FE5FBFF9FE5FCFF9FE5FCFF9FE6FCFF9FE5FCFF9FE6FBFF9FE5
      FCFF9FE5FBFF1073AFFF0000000000000000BA6834FFF0D2BDFFE2A278FFE2A2
      78FFE1A278FFE2A279FFE1A279FFE0A076FFDE9E75FFDD9E74FFDC9C72FFD99A
      70FFD8986FFFD6986EFFD5AA8DFFAC6131FF0000000000000000669348FF59C7
      ADFF59C7ADFF58C6ADFF58C7ACFF58C6ADFF59C7ADFF59C7ACFF58C7ADFF58C7
      ADFF58C7ADFF659348FF00000000000000000000000000000000000000000000
      0000000000001176B1FF8EE0FCFF8EE1FBFF8EE0FBFF8EE0FCFF1176B1FF0000
      00000000000000000000000000000000000000000000000000001175B1FF1176
      B2FF1175B2FF1176B2FFACE9FCFFABE9FCFFABE9FCFFABE9FCFF1176B1FF1175
      B1FF1175B2FF1176B2FF0000000000000000BA6834FFF2D5C1FFE3A278FFE3A2
      78FFE2A279FFE2A279FFE2A379FFE1A177FFE0A076FFDE9F75FFDE9D73FFDC9C
      72FFDA9A71FFD99A71FFDAAF94FFAE6231FF00000000000000006A9A4EFF5ED6
      C5FF5ED6C5FF5FD5C5FF5ED5C5FF5ED6C5FF5FD5C5FF5ED5C6FF5ED6C6FF5ED6
      C5FF5ED6C5FF6A9A4DFF000000000000000000000000000000001278B4FF1178
      B4FF1178B4FF1278B4FF9DE4FBFF9DE4FBFF9DE5FBFF9DE5FCFF1178B4FF1278
      B4FF1279B4FF1278B4FF00000000000000000000000000000000000000000000
      0000000000001178B4FFB9EDFCFFB9EDFCFFB9EDFCFFB9EDFCFF1278B4FF0000
      000000000000000000000000000000000000BA6834FFF2D8C4FFE3A379FFE3A2
      78FFE3A378FFE2A379FFE2A279FFE1A279FFE1A177FFDF9F75FFDE9E74FFDD9D
      72FFDB9B70FFDC9C72FFDDB499FFB06332FF00000000000000006FA253FF6EA1
      52FF6FA153FF6FA152FF61DBD0FF61DBD0FF61DBD0FF61DBD0FF6FA152FF6FA1
      52FF6FA152FF6FA152FF00000000000000000000000000000000127BB6FFB0EA
      FCFFB0EAFDFFAFEAFCFFB0EAFDFFB0EAFCFFB0EAFDFFB0EAFCFFAFEAFDFFB0EA
      FCFFAFEAFDFF127AB6FF00000000000000000000000000000000000000000000
      000000000000127BB6FFC7F1FCFFC6F1FDFFC7F1FDFFC7F1FDFF127AB7FF0000
      000000000000000000000000000000000000BA6934FFF4D9C7FFE6A57BFFC88B
      62FFC98C63FFC98D65FFCB916AFFCB916BFFCA8F67FFC88B63FFC88B62FFC88B
      62FFC88B62FFDA9B72FFE1B99EFFB26432FF0000000000000000000000000000
      0000000000000000000073A756FF63E1DAFF63E2DAFF73A756FF000000000000
      000000000000000000000000000000000000000000000000000000000000137D
      B8FFC3F0FDFFC3F0FDFFC3F0FDFFC3F0FCFFC2F0FDFFC2F0FDFFC3F0FDFFC3F0
      FDFF137DB9FF0000000000000000000000000000000000000000000000000000
      000000000000127DB9FFD3F5FDFFD3F4FDFFD3F5FDFFD3F4FEFF127CB8FF0000
      000000000000000000000000000000000000B86934FEF4DCC9FFE7A67BFFF9EC
      E1FFF9ECE1FFF9EDE3FFFCF4EEFFFDFAF7FFFDF7F3FFFAEDE5FFF7E7DBFFF7E5
      D9FFF6E5D8FFDE9F75FFE4BDA3FFB36532FF0000000000000000000000000000
      0000000000000000000077AC5AFF63E1DAFF63E2DAFF77AC5BFF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00001481BAFFD4F5FEFFD5F6FDFFD5F6FDFFD5F5FEFFD4F5FDFFD4F5FDFF1481
      BBFF000000000000000000000000000000000000000000000000000000000000
      0000000000001381BBFFDFF8FDFFDFF8FDFFDEF9FEFFDFF8FDFF1381BAFF0000
      000000000000000000000000000000000000B36532FAF5DDCCFFE7A77CFFFAF0
      E8FFFAF0E8FFC98C64FFFAF0E9FFFDF8F3FFFEFAF8FFFCF4EFFFF9E9DFFFF7E7
      DBFFF7E5D9FFE0A176FFE7C1A8FFB56633FF0000000000000000000000000000
      0000000000000000000079B05DFF64E5E0FF64E5E0FF79B05DFF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000001482BCFFE5F9FEFFE5F9FEFFE4FAFEFFE5FAFEFF1483BCFF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000001482BCFFE8FBFEFFE8FBFEFFE8FAFDFFE9FAFEFF1482BCFF0000
      000000000000000000000000000000000000A55D2EF0F6DFD0FFE8A77CFFFCF6
      F1FFFCF6F1FFC88B62FFFAF1E9FFFBF4EEFFFDFAF7FFFDF9F6FFFAF0E8FFF8E8
      DDFFF7E6DBFFE1A278FFEFD5C2FFB46733FE0000000000000000000000000000
      0000000000000000000079B05DFF79B05DFF79B05DFF79B05DFF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000001484BDFFEFFDFEFFEFFDFEFF1484BDFF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000001484BDFFE8FBFEFFE8FBFEFFE8FAFDFFE9FAFEFF1484BDFF0000
      000000000000000000000000000000000000864B25D8F6DFD1FFE9A97EFFFEFA
      F6FFFDFAF6FFC88B62FFFBF3EEFFFBF1EAFFFCF6F2FFFEFBF8FFFCF6F1FFF9EC
      E2FFF8E7DBFFEED0B9FFECD0BCFFB06839F80000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000001484BDFF1484BDFF00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000001484BDFF1484BDFF1484BDFF1484BDFF000000000000
      0000000000000000000000000000000000004526139BF6E0D1FFF7E0D1FFFEFB
      F8FFFEFBF7FFFDF9F6FFFCF5F0FFFAF0EAFFFBF2EDFFFDF9F6FFFDFAF7FFFBF1
      EBFFF6E7DDFEE4C9B6FBAC744FEC1B0F07630000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000024140A713B211090784321CCA35C
      2DEEB36532FAB86934FEBA6934FFBA6834FFBA6834FFBB6A37FFBC6C39FFBA6B
      38FFA35C30EF764526CB130B055400000000}
  end
  object MainMenu1: TMainMenu
    Images = ActionImageList
    Left = 40
    Top = 88
    object File1: TMenuItem
      Caption = '&File'
      object NewProjectMenuItem: TMenuItem
        Caption = 'New project'
        object emptyproject1: TMenuItem
          Action = NewProjectAction
          Caption = '(empty project)'
        end
        object N16: TMenuItem
          Caption = '-'
        end
      end
      object Open1: TMenuItem
        Action = FileOpenAction
      end
      object ReopenMenuItem: TMenuItem
        Caption = 'Reopen'
      end
      object NewWindow1: TMenuItem
        Action = FileNewWindowAction
      end
      object N8: TMenuItem
        Caption = '-'
      end
      object Import3dsAction1: TMenuItem
        Action = Import3dsAction
      end
      object Importbitmap1: TMenuItem
        Action = ImportBitmapAction
      end
      object ImportAudio1: TMenuItem
        Action = ImportAudioAction
      end
      object N7: TMenuItem
        Caption = '-'
      end
      object Save1: TMenuItem
        Action = SaveProjectAction
        Caption = 'Save Project'
      end
      object SaveAs1: TMenuItem
        Action = FileSaveAsAction
        Caption = 'Save Project &As...'
      end
      object SaveBinaryMenuItem: TMenuItem
        Action = FileSaveBinaryAsAction
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Action = FileExitAction
      end
    end
    object Edit1: TMenuItem
      Caption = 'Edit'
      object Undodelete2: TMenuItem
        Action = UndoDeleteAction
      end
      object N14: TMenuItem
        Caption = '-'
      end
      object EditasXML1: TMenuItem
        Action = EditXmlAction
      end
    end
    object Run1: TMenuItem
      Caption = 'Project'
      object Run2: TMenuItem
        Action = RunExeAction
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object Buildreleaseversionsmallest1: TMenuItem
        Action = GenerateReleaseAction
      end
      object RemoveUnusedMenuItem: TMenuItem
        AutoCheck = True
        Caption = 'Remove unused code'
        Checked = True
      end
      object DetailedBuildReportMenuItem: TMenuItem
        AutoCheck = True
        Caption = 'Display detailed build report'
      end
      object N9: TMenuItem
        Caption = '-'
      end
      object BuildandcompressLinuxbinary1: TMenuItem
        Action = GenerateReleaseLinuxAction
      end
      object BuildMacOSXIntelbinary1: TMenuItem
        Action = GenerateReleaseOsx86Action
      end
      object N17: TMenuItem
        Caption = '-'
      end
      object BuildAndroidbinaryexperimental1: TMenuItem
        Action = GenerateAndroidAction
      end
      object AndroidRunproject1: TMenuItem
        Action = AndroidRunAction
      end
      object AndroidBuildAPK1: TMenuItem
        Action = AndroidBuildDebugApkAction
      end
      object AndroidBuildAPKrelease1: TMenuItem
        Action = AndroidBuildReleaseApkAction
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object GenerateScreenSaverAction1: TMenuItem
        Action = GenerateScreenSaverAction
      end
      object Buildreleasescreensaverversionsmallest1: TMenuItem
        Action = GenerateReleaseSSAction
      end
    end
    object ools1: TMenuItem
      Caption = 'Tools'
      object Settings2: TMenuItem
        Action = ShowSettingsAction
      end
      object N11: TMenuItem
        Caption = '-'
      end
      object N10: TMenuItem
        Action = ShowCompilerDetailsAction
      end
      object EnableThreadedProcessingMenuItem: TMenuItem
        AutoCheck = True
        Caption = 'Enable threaded processing'
        Checked = True
        OnClick = EnableThreadedProcessingMenuItemClick
      end
    end
    object StyleMenuItem: TMenuItem
      Caption = 'Style'
      object OpenStyleMenuItem: TMenuItem
        Caption = 'Open style...'
        OnClick = OpenStyleMenuItemClick
      end
      object N15: TMenuItem
        Caption = '-'
      end
    end
    object Help1: TMenuItem
      Caption = 'Help'
      object Contents1: TMenuItem
        Action = HelpContentsAction
      end
      object N13: TMenuItem
        Caption = '-'
      end
      object Onlinehelp1: TMenuItem
        Caption = 'ZGameEditor Home Page'
        OnClick = Onlinehelp1Click
      end
      object ForumsMenuItems: TMenuItem
        Caption = 'Get help on the Online Forums'
        OnClick = ForumsMenuItemsClick
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object About1: TMenuItem
        Action = AboutAction
      end
    end
  end
  object ActionDisabledImageList: TImageList
    ColorDepth = cd32Bit
    Left = 208
    Top = 152
    Bitmap = {
      494C01010C001100040010001000FFFFFFFF2000FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000004000000001002000000000000040
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A3A3A3FF9F9F9FFF9C9C
      9CFF999999FF959595FF929292FF8F8F8FFF8C8C8CFF898989FF878787FF8484
      84FF828282FF808080FF7C7C7CFF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000D5E0D5FF75AD64FF3F8824FF3F8024FF3F8024FF3F8824FF75AD64FFD5E0
      D5FF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A7A7A7FFEAEAEAFFEAEA
      EAFFEDEDEDFFF2F2F2FFF5F5F5FFF7F7F7FFF5F5F5FFF0F0F0FFE9E9E9FFE4E4
      E4FFE2E2E2FFE2E2E2FF808080FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000075AD
      64FF3F882BFF559A46FF5CA455FF6DAD64FF6DAD64FF5CA455FF559A46FF3F88
      2BFF75AD64FF000000000000000000000000000000000000000000000000C0C0
      C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0
      C0FFC0C0C0FFC0C0C0FF000000000000000000000000ABABABFFEBEBEBFFCCCC
      CCFFB5B5B5FFBDBDBDFFC3C3C3FFC6C6C6FFC5C5C5FFBABABAFFABABABFFA2A2
      A2FFB9B9B9FFE3E3E3FF838383FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000075AD64FF4691
      38FF5CA455FFADCBADFFADCBADFF388831FF388838FF469A3FFF55A455FF5CA4
      55FF469138FF75AD64FF0000000000000000000000000000000000000000C0C0
      C0FFD8D8E8FFD8D8E8FFD8D8E8FFD8D8E8FFD8D8E8FFD8D8E8FFD8D8E8FFD8D8
      E8FFD8D8E8FFC0C0C0FF000000000000000000000000AFAFAFFFECECECFFD0D0
      D0FFD8D8D8FFE1E1E1FFE7E7E7FFEBEBEBFFE8E8E8FFDDDDDDFFCCCCCCFFBEBE
      BEFFBBBBBBFFE4E4E4FF878787FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000D5E0D5FF3F882BFF559A
      4DFF3F913FFFADCBADFFFFFFFFFFADCBADFF3F8838FF3F8838FF3F913FFF4D9A
      46FF5CA455FF3F882BFFD5E0D5FF00000000000000000000000000000000C0C0
      C0FFD0D0D8FFB0B0C0FFB0B0C0FFB0B0C0FFB0B0C0FFB0B0C0FFB0B0C0FFB0B0
      C0FFD0D0D8FFC0C0C0FF000000000000000000000000B3B3B3FFEDEDEDFFD3D3
      D3FFBABABAFFC0C0C0FFE7E7E7FFA0A0A0FF888888FFDFDFDFFFB0B0B0FFA6A6
      A6FFBEBEBEFFE5E5E5FF8A8A8AFF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000075AD64FF4D913FFF4D91
      46FF4D9146FFADCBADFFFFFFFFFFFFFFFFFFADCBADFF4D9146FF4D9146FF4D9A
      46FF559A4DFF55913FFF75AD64FF00000000000000000000000000000000C0C0
      C0FFC8C8D8FFB0A8C0FFB0A8C0FFB0A8C0FFB0A8C0FFB0A8C0FFB0A8C0FFB0A8
      C0FFC8C8D8FFC0C0C0FF000000000000000000000000B7B7B7FFEEEEEEFFD5D5
      D5FFDEDEDEFFE4E4E4FFE6E6E6FFA5A5A5FF909090FFE0E0E0FFD0D0D0FFC4C4
      C4FFC1C1C1FFE6E6E6FF8E8E8EFF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000003F8024FF31802BFF3180
      2BFF31802BFFADCBADFFFFFFFFFFFFFFFFFFFFFFFFFFADCBADFF31802BFF3180
      2BFF46883FFF5CA455FF3F8024FF00000000000000000000000000000000C0C0
      C0FFC0C0D0FFA8A8C0FFA8A8C0FFA8A8C0FFA8A8C0FFA8A8C0FFA8A8C0FFA8A8
      C0FFC0C0D0FFC0C0C0FF000000000000000000000000BABABAFFEFEFEFFFD8D8
      D8FFBEBEBEFFC1C1C1FFE5E5E5FFABABABFFACACACFFE1E1E1FFB3B3B3FFAAAA
      AAFFC3C3C3FFE7E7E7FF929292FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000003F801EFF387531FF3875
      31FF387531FFADCBADFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFBAD2BAFF3875
      31FF387531FF6D9A5CFF3F801EFF00000000000000000000000000000000C0C0
      C0FFC8C8D8FFB0A8C0FFB0A8C0FFB0A8C0FFB0A8C0FFB0A8C0FFB0A8C0FFB0A8
      C0FFC8C8D8FFC0C0C0FF000000000000000000000000BDBDBDFFF0F0F0FFDADA
      DAFFE2E2E2FFE4E4E4FFE4E4E4FFEBEBEBFFEAEAEAFFE2E2E2FFD4D4D4FFCACA
      CAFFC6C6C6FFE8E8E8FF969696FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000468831FF468046FF4680
      46FF468046FFADCBADFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF76E9
      C2FF468046FF75AD75FF3F8024FF00000000000000000000000000000000C0C0
      C0FFD0D0D8FFB0B0C8FFB0B0C8FFB0B0C8FFB0B0C8FFB0B0C8FFB0B0C8FFB0B0
      C8FFD0D0D8FFC0C0C0FF000000000000000000000000C0C0C0FFF0F0F0FFF1F1
      F1FFF4F4F4FFF5F5F5FFF4F4F4FFF7F7F7FFF7F7F7FFF4F4F4FFF0F0F0FFEBEB
      EBFFEAEAEAFFEAEAEAFF9B9B9BFF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000468831FF559A5CFF559A
      5CFF559A5CFFBAD2BAFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF24CC80FF559A
      5CFF559A5CFF75AD75FF468831FF00000000000000000000000000000000C0C0
      C0FFD0D0D8FFB8B0C8FFB8B0C8FFB8B0C8FFB8B0C8FFB8B0C8FFB8B0C8FFB8B0
      C8FFD0D0D8FFC0C0C0FF000000000000000000000000C0C0C0FF9F9F9FFF9191
      91FF8E8E8EFFB8B8B8FFB8B8B8FFB5B5B5FFB2B2B2FFAFAFAFFFAAAAAAFF7676
      76FF747474FF7D7D7DFF9F9F9FFF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000075AD64FF559A4DFF5CA4
      6DFF5CA46DFFBAD2BAFFFFFFFFFFFFFFFFFFFFFFFFFF24CC80FF5CA46DFF5CA4
      6DFF5CA46DFF5CA46DFF75AD64FF00000000000000000000000000000000C0C0
      C0FFD8D8E8FFB8B8C8FFB8B8C8FFB8B8C8FFB8B8C8FFB8B8C8FFB8B8C8FFB8B8
      C8FFD8D8E8FFC0C0C0FF00000000000000000000000000000000999999FFC6C6
      C6FF949494FF8F8F8FFF000000000000000000000000000000007C7C7CFF7B7B
      7BFFABABABFF747474FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000D5E0D5FF468831FF6DAD
      80FF6DB788FFBAD2BAFFFFFFFFFFFFFFFFFF24CC80FF6DB788FF6DB788FF6DB7
      88FF6DB788FF468831FFD5E0D5FF00000000000000000000000000000000C0C0
      C0FFD8D8E8FFD8D8E8FFD8D8E8FFD8D8E8FFD8D8E8FFD8D8E8FFD8D8E8FFD8D8
      E8FFD8D8E8FFC0C0C0FF000000000000000000000000000000009D9D9DFFC4C4
      C4FFA1A1A1FF939393FF00000000000000000000000000000000828282FF8989
      89FFA9A9A9FF777777FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000075AD64FF5591
      46FF75B788FF24CC80FFFFFFFFFF24CC80FF6DB791FF6DB791FF75B791FF75B7
      91FF559146FF75AD64FF0000000000000000000000000000000000000000C0C0
      C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0
      C0FFC0C0C0FFC0C0C0FF00000000000000000000000000000000A1A1A1FFBABA
      BAFFBFBFBFFF989898FF949494FF919191FF8E8E8EFF8A8A8AFF878787FFA8A8
      A8FF9E9E9EFF7B7B7BFF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000075AD
      64FF468831FF24CC80FF24CC80FF88C19AFF88C1A4FF80B788FF64A464FF4D88
      38FF75AD64FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000A4A4A4FFA1A1
      A1FFC4C4C4FFBEBEBEFFA1A1A1FF969696FF939393FF979797FFAEAEAEFFAEAE
      AEFF848484FF818181FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000D5E0D5FF75AD64FF4D8838FF3F8024FF3F8024FF4D8838FF75AD64FFD5E0
      D5FF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000A4A4
      A4FFA2A2A2FFBCBCBCFFCACACAFFCCCCCCFFCACACAFFC2C2C2FFADADADFF8C8C
      8CFF898989FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000A5A5A5FFA3A3A3FFA0A0A0FF9D9D9DFF9A9A9AFF979797FF949494FF9090
      90FF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008B8B8BFF8B8B8BFF8B8B8BFF8B8B8BFF8B8B8BFF8B8B8BFF8B8B
      8BFF8B8B8BFF8B8B8BFF8B8B8BFF8B8B8BFF0000000000000000A3A3A3FF8B8B
      8BFF898989FF878787FF858585FF898989FF898989FF898989FF898989FF8989
      89FF898989FF898989FF898989FF000000000000000000000000000000000707
      0741000000080000000000000000000000000000000000000000000000000000
      0008050505410000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000008B8B8BFF8B8B8BFF8B8B
      8BFF8B8B8BFF8C8C8CFFB5B5B5FFB5B5B5FFB5B5B5FFB5B5B5FFB5B5B5FFB5B5
      B5FFB5B5B5FFB5B5B5FFB5B5B5FF8C8C8CFF00000000000000008E8E8EFF9A9A
      9AFFA5A5A5FFA6A6A6FFA6A6A6FF8B8B8BFFB5B5B5FFB6B6B6FFB6B6B6FFB6B6
      B6FFB6B6B6FFB5B5B5FF8B8B8BFF000000000000000000000000070707417B7B
      7BFF6E6E6EFD0000000800000000000000000000000000000000000000085858
      58FC656565FF050505410000000000000000878787FF818181FF757575FF6F6F
      6FFF606060FF555555FF474747FF373737FF2C2C2CFF222222FF171717FF0E0E
      0EFF121212FF000000000000000000000000000000008C8C8CFFB5B5B5FFB5B5
      B5FFB5B5B5FF8D8D8DFFB6B6B6FF919191FF8D8D8DFF888888FF828282FF7B7B
      7BFFB6B6B6FFB5B5B5FFB5B5B5FF8D8D8DFF00000000000000008F8F8FFFA7A7
      A7FFA7A7A7FFA7A7A7FFA7A7A7FF8D8D8DFFB6B6B6FFB7B7B7FFB7B7B7FFB7B7
      B7FFB6B6B6FFB7B7B7FF8C8C8CFF000000000000000008080841848484FF8D8D
      8DFF848484FF6F6F6FFC000000080000000000000000000000085E5E5EFC7272
      72FF7B7B7BFF656565FF0505054100000000878787FFA2A2A2FFB2B2B2FF9292
      92FF929292FF929292FF8D8D8DFF8B8B8BFF818181FF818181FF777777FF6E6E
      6EFF484848FF939393FF0000000000000000000000008D8D8DFFB6B6B6FF9191
      91FF8D8D8DFF8E8E8EFFB6B6B6FFB6B6B6FFB6B6B6FFB6B6B6FFB6B6B6FFB6B6
      B6FFB6B6B6FFB6B6B6FFB6B6B6FF8E8E8EFF0000000000000000919191FFA9A9
      A9FFA9A9A9FFA9A9A9FFA9A9A9FF8F8F8FFFB8B8B8FF919191FF8A8A8AFF8080
      80FF777777FFB8B8B8FF8E8E8EFF00000000000000000303032B868686FF8E8E
      8EFF999999FF858585FF707070FC0000000800000008656565FC767676FF9090
      90FF797979FF636363FF0202022B00000000898989FFBDBDBDFFA7A7A7FFDEDE
      DEFFD1D1D1FFCACACAFFCACACAFFBFBFBFFFB4B4B4FFAFAFAFFFA4A4A4FF9494
      94FF646464FF575757FF0000000000000000000000008E8E8EFFB6B6B6FFB6B6
      B6FFB6B6B6FF8F8F8FFFB8B8B8FF919191FF8F8F8FFF8B8B8BFF878787FF8383
      83FF7D7D7DFF7A7A7AFFB8B8B8FF8F8F8FFF0000000000000000929292FFABAB
      ABFFAAAAAAFFAAAAAAFFAAAAAAFF919191FFBABABAFFBABABAFFBABABAFFBABA
      BAFFBABABAFFBABABAFF919191FF0000000000000000000000000303032B8787
      87FF8F8F8FFF9A9A9AFF868686FF707070FC6C6C6CFD7D7D7DFF939393FF7E7E
      7EFF686868FF0202022B0000000000000000929292FFD0D0D0FFA7A7A7FFBFBF
      BFFFD7D7D7FFD1D1D1FFD1D1D1FFCACACAFFC6C6C6FFBBBBBBFFB4B4B4FFAFAF
      AFFF8F8F8FFF484848FFB7B7B7FF00000000000000008F8F8FFFB8B8B8FF9191
      91FF8F8F8FFF919191FFB8B8B8FFB9B9B9FFB9B9B9FFB9B9B9FFB9B9B9FFB9B9
      B9FFB9B9B9FFB8B8B8FFB9B9B9FF919191FF0000000000000000949494FFADAD
      ADFFADADADFFADADADFFADADADFF939393FFBBBBBBFF919191FF8A8A8AFF8080
      80FF777777FFBBBBBBFF939393FF000000000000000000000000000000000303
      032B878787FF909090FF9B9B9BFF9A9A9AFF989898FF979797FF848484FF7070
      70FF0202022B000000000000000000000000929292FFD5D5D5FFC6C6C6FFA7A7
      A7FFDEDEDEFFD3D3D3FFD5D5D5FFD1D1D1FFD1D1D1FFCACACAFFBFBFBFFFBBBB
      BBFFB4B4B4FF646464FF838383FF0000000000000000919191FFB8B8B8FFB9B9
      B9FFB9B9B9FF929292FFBABABAFF919191FF8F8F8FFF8B8B8BFF878787FF8383
      83FF7D7D7DFF7A7A7AFFBABABAFF939393FF0000000000000000959595FFAFAF
      AFFFAFAFAFFFAFAFAFFFAFAFAFFF959595FFBDBDBDFFBDBDBDFFBDBDBDFFBDBD
      BDFFBDBDBDFFBDBDBDFF959595FF000000000000000000000000000000000000
      00000303032B888888FF9E9E9EFF878787FF868686FF999999FF777777FF0303
      032B00000000000000000000000000000000979797FFDBDBDBFFE0E0E0FFA7A7
      A7FFB5B5B5FFD7D7D7FFD5D5D5FFD5D5D5FFD5D5D5FFD1D1D1FFCFCFCFFFCACA
      CAFFBBBBBBFFA6A6A6FF656565FFC1C1C1FF00000000939393FFBABABAFF9191
      91FF8F8F8FFF949494FFBBBBBBFFBBBBBBFFBBBBBBFFBBBBBBFFBBBBBBFFBBBB
      BBFFBBBBBBFFBBBBBBFFBBBBBBFF949494FF0000000000000000979797FFB1B1
      B1FFB1B1B1FFB1B1B1FFB1B1B1FF979797FFBEBEBEFF919191FF777777FFBEBE
      BEFFA5A5A5FFA0A0A0FF979797FF000000000000000000000000000000000000
      000000000008858585FDA0A0A0FF8B8B8BFF898989FF9C9C9CFF727272FD0000
      000800000000000000000000000000000000979797FFDBDBDBFFE6E6E6FFC6C6
      C6FFA7A7A7FFDEDEDEFFDEDEDEFFDEDEDEFFDEDEDEFFDEDEDEFFDEDEDEFFDEDE
      DEFFD7D7D7FFD7D7D7FFA9A9A9FF909090FF00000000949494FFBBBBBBFFBBBB
      BBFFBBBBBBFF959595FFBDBDBDFF919191FF8F8F8FFF8B8B8BFF878787FF8383
      83FF7D7D7DFF7A7A7AFFBCBCBCFF959595FF0000000000000000989898FFB3B3
      B3FFB3B3B3FFB3B3B3FFB3B3B3FF999999FFBFBFBFFFBFBFBFFFBFBFBFFFBFBF
      BFFFA0A0A0FF979797FF00000000000000000000000000000000000000000000
      00088B8B8BFC969696FFA3A3A3FFA1A1A1FFA0A0A0FF9E9E9EFF898989FF7373
      73FC000000080000000000000000000000009E9E9EFFE2E2E2FFE6E6E6FFE6E6
      E6FFB1B1B1FFB1B1B1FFA7A7A7FFA7A7A7FFA1A1A1FFA1A1A1FF979797FF9797
      97FF929292FF929292FF898989FF878787FF00000000969696FFBDBDBDFF9191
      91FF8F8F8FFF979797FFBDBDBDFFBDBDBDFFBDBDBDFFBDBDBDFFBDBDBDFFBDBD
      BDFFBDBDBDFFBDBDBDFFBDBDBDFF979797FF00000000000000009A9A9AFFB5B5
      B5FFB4B4B4FFB5B5B5FFB5B5B5FF999999FF999999FF999999FF999999FF9999
      99FF999999FF909090FF00000000000000000000000000000000000000089191
      91FC9C9C9CFFA6A6A6FF9B9B9BFF8E8E8EFF8A8A8AFF949494FFA0A0A0FF8A8A
      8AFF747474FC000000080000000000000000A2A2A2FFE2E2E2FFEBEBEBFFE6E6
      E6FFE6E6E6FFD9D9D9FFD5D5D5FFD5D5D5FFD5D5D5FFD5D5D5FFD5D5D5FFD5D5
      D5FF878787FF00000000000000000000000000000000969696FFBDBDBDFFBDBD
      BDFFBDBDBDFF989898FFBEBEBEFF919191FF8A8A8AFF808080FF777777FFBEBE
      BEFFA5A5A5FFA0A0A0FF9A9A9AFF979797FF00000000000000009B9B9BFFB6B6
      B6FFB6B6B6FFB6B6B6FFB6B6B6FFB6B6B6FFB6B6B6FFB6B6B6FFB6B6B6FFB6B6
      B6FFB6B6B6FF969696FF00000000000000000000000000000008989898FDA0A0
      A0FFAAAAAAFF9F9F9FFF939393FF0404042B0303032B8B8B8BFF959595FFA1A1
      A1FF8B8B8BFF757575FC0000000500000000A2A2A2FFE6E6E6FFE8E8E8FFEBEB
      EBFFE6E6E6FFE6E6E6FFD7D7D7FF9E9E9EFF979797FF979797FF929292FF8E8E
      8EFF878787FF00000000000000000000000000000000989898FFBEBEBEFF9191
      91FF8A8A8AFF999999FFBFBFBFFFBFBFBFFFBFBFBFFFBFBFBFFFBFBFBFFFBFBF
      BFFFA0A0A0FF9B9B9BFF999999FF0000000000000000000000009B9B9BFFB6B6
      B6FF767676FF888888FF888888FF888888FF888888FF888888FF888888FF7676
      76FFB6B6B6FF989898FF0000000000000000000000000202021F9F9F9FFFA3A3
      A3FFA3A3A3FF9A9A9AFF0404042B00000000000000000303032B8C8C8CFF9696
      96FFA2A2A2FF8C8C8CFF1616166F00000002A7A7A7FFCFCFCFFFE8E8E8FFE8E8
      E8FFEBEBEBFFE6E6E6FFA2A2A2FFC2C2C2FF0000000000000000000000000000
      000000000000636363FF636363FF636363FF00000000989898FFBFBFBFFFBFBF
      BFFFBFBFBFFF9A9A9AFFBFBFBFFFBFBFBFFFBFBFBFFFBFBFBFFFBFBFBFFFBFBF
      BFFF9A9A9AFF9A9A9AFF00000000000000000000000000000000AEAEAEFF9090
      90FF909090FF7D7D7DFF939393FF939393FF939393FF939393FF7D7D7DFF8383
      83FF909090FFAEAEAEFF000000000000000000000000000000000202021F9F9F
      9FFF9E9E9EFF0404042B000000000000000000000000000000000303032B8D8D
      8DFF929292FF373737A80303032900000000CECECEFFA7A7A7FFA7A7A7FFA7A7
      A7FFA7A7A7FFA7A7A7FFC0C0C0FF000000000000000000000000000000000000
      00000000000000000000636363FF636363FF000000009A9A9AFFBFBFBFFFBFBF
      BFFFBFBFBFFF9A9A9AFF9A9A9AFF9A9A9AFF9A9A9AFF9A9A9AFF9A9A9AFF9A9A
      9AFF9A9A9AFF0000000000000000000000000000000000000000000000000000
      000000000000878787FF9D9D9DFF9D9D9DFF9D9D9DFF9D9D9DFF878787FF0000
      0000000000000000000000000000000000000000000000000000000000000202
      021F0404042B0000000000000000000000000000000000000000000000000404
      042B1919196F0707073E00000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000717171FFB3B3B3FF0000
      0000000000008D8D8DFF00000000717171FF000000009A9A9AFF9A9A9AFF9A9A
      9AFF9A9A9AFF9A9A9AFF9A9A9AFF9A9A9AFF9A9A9AFF9A9A9AFF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000008C8C8CFF9F9F9FFF9F9F9FFF8C8C8CFF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000A0000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000008D8D8DFF8888
      88FF939393FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C1C1C2FFC1C1C2FFC1C1
      C2FFC1C1C2FFC1C1C2FFC1C1C2FFC1C1C2FFC1C1C2FFC1C1C2FFC1C1C2FFC1C1
      C2FFC1C1C2FFC1C1C2FFC1C1C2FFC1C1C2FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000C1C1C2FFC1C1C2FFC1C1C2FFC1C1C2FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000C1C1C2FFC1C1C2FF00000000000000000000
      00000000000000000000000000000000000000000000C1C1C2FFE3E0DEFFE3E0
      DEFFE3E0DEFFBAB5B2FFD5DBDEFFD5DBDEFFD5DBDEFFAFABA8FFD5DBDEFFBAB5
      B2FFE3E0DEFFE3E0DEFFE3E0DEFFC1C1C2FF0000000000000000000000000000
      00000000000000000000C1C1C2FFC1C1C2FFC1C1C2FFC1C1C2FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C1C1C2FFD1DCE0FFD1DCE0FFD1DCE0FFD1DCE0FFC1C1C2FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000C1C1C2FFD1DCE0FFD1DCE0FFC1C1C2FF000000000000
      00000000000000000000000000000000000000000000C1C1C2FFE5E2E0FFE5E2
      E0FFE5E2E0FFBAB5B2FFE8E4E1FFE8E4E1FFE8E4E1FFCDCECCFFCDCECCFFBAB5
      B2FFE5E2E0FFE5E2E0FFE5E2E0FFC1C1C2FF0000000000000000000000000000
      00000000000000000000C1C1C2FFAFBBB6FFAFBBB6FFC1C1C2FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C1C1C2FFD1DCE0FFD1DCE0FFD1DCE0FFD1DCE0FFC1C1C2FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C1C1C2FFD1DCE0FFD1DCE0FFD1DCE0FFD1DCE0FFC1C1C2FF0000
      00000000000000000000000000000000000000000000C1C1C2FFE6E3E1FFE6E3
      E1FFE6E4E2FFBAB5B2FFD8D3D0FFD8D3D0FFD8D3D0FFBFBFBDFFBFBFBDFFBAB5
      B2FFE6E4E2FFE6E3E1FFE6E3E1FFC1C1C2FF0000000000000000000000000000
      00000000000000000000C1C1C2FFAFBBB6FFAFBBB6FFC1C1C2FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C1C1C2FFD1DCE0FFD1DCE0FFD1DCE0FFD1DCE0FFC1C1C2FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C1C1C2FFD4DEE2FFD4DEE2FFD4DEE2FFD4DEE2FFD4DEE2FFD4DEE2FFC1C1
      C2FF0000000000000000000000000000000000000000C1C1C2FFE8E6E4FFE7E5
      E3FFE8E6E4FFBAB5B2FFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFD5DBDEFFBAB5
      B2FFE8E6E4FFE8E6E4FFE8E6E4FFC1C1C2FF0000000000000000000000000000
      00000000000000000000C1C1C2FFB1BDB9FFB1BDB9FFC1C1C2FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C1C1C2FFD1DCE0FFD1DCE0FFD1DCE0FFD1DCE0FFC1C1C2FF0000
      000000000000000000000000000000000000000000000000000000000000C1C1
      C2FFD7E1E4FFD7E1E4FFD7E1E4FFD7E1E4FFD7E1E4FFD7E1E4FFD7E1E4FFD7E1
      E4FFC1C1C2FF00000000000000000000000000000000C1C1C2FFEAE8E6FFEAE7
      E6FFEAE8E6FFEAE7E6FFBAB5B2FFBAB5B2FFBAB5B2FFBAB5B2FFBAB5B2FFEAE8
      E6FFE9E7E5FFEAE8E6FFEAE8E6FFC1C1C2FF0000000000000000C1C1C2FFC1C1
      C2FFC1C1C2FFC1C1C2FFB3C0BCFFB3C0BCFFB3C0BCFFB3C0BCFFC1C1C2FFC1C1
      C2FFC1C1C2FFC1C1C2FF00000000000000000000000000000000000000000000
      000000000000C1C1C2FFD1DCE0FFD1DCE0FFD1DCE0FFD1DCE0FFC1C1C2FF0000
      0000000000000000000000000000000000000000000000000000C1C1C2FFDBE3
      E6FFDBE3E6FFDBE3E6FFDBE3E6FFDBE3E6FFDBE3E6FFDBE3E6FFDBE3E6FFDBE3
      E6FFDBE3E6FFC1C1C2FF000000000000000000000000C1C1C2FFEBE9E8FFECEA
      E9FFECEAE9FFEBE9E8FFEBE9E8FFEBE9E8FFECEAE9FFECEAE9FFECEAE9FFECEA
      E9FFECEAE9FFECEAE9FFEBE9E8FFC1C1C2FF0000000000000000C1C1C2FFB6C3
      C0FFB6C3C0FFB6C3C0FFB6C3C0FFB6C3C0FFB6C3C0FFB6C3C0FFB6C3C0FFB6C3
      C0FFB6C3C0FFC1C1C2FF00000000000000000000000000000000000000000000
      000000000000C1C1C2FFD5DFE2FFD5DFE2FFD5DFE2FFD5DFE2FFC1C1C2FF0000
      0000000000000000000000000000000000000000000000000000C1C1C2FFC1C1
      C2FFC1C1C2FFC1C1C2FFE0E8EAFFE0E8EAFFE0E8EAFFE0E8EAFFC1C1C2FFC1C1
      C2FFC1C1C2FFC1C1C2FF000000000000000000000000C1C1C2FFEEECEBFFEEEC
      EBFFDFDCDAFFC0C0BEFFC0C0BEFFC0C0BEFFC0C0BEFFC0C0BEFFC0C0BEFFC0C0
      BEFFDFDCDAFFEEECEBFFEEECEBFFC1C1C2FF0000000000000000C1C1C2FFBCCA
      C8FFBCCAC8FFBCCAC8FFBCCAC8FFBCCAC8FFBCCAC8FFBCCAC8FFBCCAC8FFBCCA
      C8FFBCCAC8FFC1C1C2FF00000000000000000000000000000000C1C1C2FFC1C1
      C2FFC1C1C2FFC1C1C2FFDAE3E6FFDAE3E6FFDAE3E6FFDAE3E6FFC1C1C2FFC1C1
      C2FFC1C1C2FFC1C1C2FF00000000000000000000000000000000000000000000
      000000000000C1C1C2FFE5EBEDFFE5EBEDFFE5EBEDFFE5EBEDFFC1C1C2FF0000
      00000000000000000000000000000000000000000000C1C1C2FFF0EFEEFFF0EF
      EEFFC0C0BEFFD5DBDEFFD5DBDEFFD5DBDEFFD5DBDEFFD5DBDEFFD5DBDEFFD5DB
      DEFFC0C0BEFFF0EFEEFFF0EFEEFFC1C1C2FF0000000000000000C1C1C2FFC1C1
      C2FFC1C1C2FFC1C1C2FFBDCCCBFFBDCCCBFFBDCCCBFFBDCCCBFFC1C1C2FFC1C1
      C2FFC1C1C2FFC1C1C2FF00000000000000000000000000000000C1C1C2FFE2E9
      EBFFE2E9EBFFE1E8EAFFE2E9EBFFE2E9EBFFE2E9EBFFE2E9EBFFE1E8EBFFE2E9
      EBFFE1E8EBFFC1C1C2FF00000000000000000000000000000000000000000000
      000000000000C1C1C2FFEAEFF0FFEAEFF1FFEAEFF1FFEAEFF1FFC1C1C2FF0000
      00000000000000000000000000000000000000000000C1C1C2FFF3F2F1FFF2F1
      F0FFC0C0BEFFF1F0EEFFF1F0EEFFF1F0EEFFF2F1EFFFF1F0EEFFF2F1EFFFF1F0
      EEFFC0C0BEFFF2F1F0FFF2F1F0FFC1C1C2FF0000000000000000000000000000
      00000000000000000000C1C1C2FFC0CFCEFFC1D0CFFFC1C1C2FF000000000000
      000000000000000000000000000000000000000000000000000000000000C1C1
      C2FFE9EEF0FFE9EEF0FFE9EEF0FFE8EEEFFFE8EDEFFFE8EDEFFFE9EEF0FFE9EE
      F0FFC1C1C2FF0000000000000000000000000000000000000000000000000000
      000000000000C1C1C2FFEFF3F4FFEFF3F4FFEFF3F4FFEFF3F4FFC1C1C2FF0000
      00000000000000000000000000000000000000000000C1C1C2FFF5F4F3FFF4F3
      F3FFC0C0BEFFF5F4F2FFF5F4F2FFF5F4F2FFF5F4F2FFF5F4F3FFF5F4F2FFF5F4
      F2FFC0C0BEFFF4F3F3FFF4F3F3FFC1C1C2FF0000000000000000000000000000
      00000000000000000000C1C1C2FFC0CFCEFFC1D0CFFFC1C1C2FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C1C1C2FFEFF3F4FFEFF3F4FFEFF3F4FFEFF3F4FFEFF3F4FFEFF3F4FFC1C1
      C2FF000000000000000000000000000000000000000000000000000000000000
      000000000000C1C1C2FFF3F6F7FFF3F6F7FFF2F5F6FFF3F6F7FFC1C1C2FF0000
      00000000000000000000000000000000000000000000C1C1C2FFF6F5F5FFF6F5
      F5FFC0C0BEFFF8F8F7FFF8F8F7FFF8F8F7FFF8F8F7FFF8F7F6FFF8F8F7FFF8F8
      F7FFC0C0BEFFF6F5F5FFF6F5F5FFC1C1C2FF0000000000000000000000000000
      00000000000000000000C1C1C2FFC1D0CFFFC1D0CFFFC1C1C2FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C1C1C2FFF5F7F8FFF5F7F8FFF5F8F8FFF5F8F8FFC1C1C2FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C1C1C2FFF6F9F9FFF6F9F9FFF6F9F9FFF7F9F9FFC1C1C2FF0000
      00000000000000000000000000000000000000000000C1C1C2FFF8F7F7FFF8F8
      F7FFC0C0BEFFFAFAFAFFFAFAFAFFFAFAFAFFFAFAFAFFFAFAFAFFFAFAFAFFFAFA
      FAFFC0C0BEFFF8F7F7FFF8F8F7FFC1C1C2FF0000000000000000000000000000
      00000000000000000000C1C1C2FFC1C1C2FFC1C1C2FFC1C1C2FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000C1C1C2FFF8FAFAFFF8FAFAFFC1C1C2FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C1C1C2FFF6F9F9FFF6F9F9FFF6F9F9FFF7F9F9FFC1C1C2FF0000
      00000000000000000000000000000000000000000000C1C1C2FFF9F9F9FFF9F9
      F9FFD9D9D7FFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
      FDFFD9D9D7FFF9F9F9FFF9F9F9FFC1C1C2FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000C1C1C2FFC1C1C2FF00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000C1C1C2FFC1C1C2FFC1C1C2FFC1C1C2FF000000000000
      00000000000000000000000000000000000000000000C1C1C2FFC1C1C2FFC1C1
      C2FFC1C1C2FFC1C1C2FFC1C1C2FFC1C1C2FFC1C1C2FFC1C1C2FFC1C1C2FFC1C1
      C2FFC1C1C2FFC1C1C2FFC1C1C2FFC1C1C2FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000}
  end
  object LogPopupMenu: TPopupMenu
    Left = 40
    Top = 312
    object LogCopytoclipboardMenuItem: TMenuItem
      Caption = 'Copy to clipboard'
      OnClick = LogCopytoclipboardMenuItemClick
    end
    object LogClearMenuItem: TMenuItem
      Caption = 'Clear'
      OnClick = LogClearMenuItemClick
    end
  end
  object OpenStyleDialog: TOpenDialog
    DefaultExt = '*.vsf'
    Filter = 'Style (*.vsf)|*.vsf'
    Title = 'Open style'
    Left = 56
    Top = 368
  end
end
