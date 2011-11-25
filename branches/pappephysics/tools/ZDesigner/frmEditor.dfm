object EditorForm: TEditorForm
  Left = 0
  Top = 0
  Caption = 'ZDesigner'
  ClientHeight = 767
  ClientWidth = 1092
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
  TextHeight = 13
  object Splitter2: TSplitter
    Left = 280
    Top = 22
    Width = 4
    Height = 745
    ExplicitLeft = 433
    ExplicitTop = 30
    ExplicitHeight = 537
  end
  object LeftPanel: TPanel
    Left = 0
    Top = 22
    Width = 280
    Height = 745
    Align = alLeft
    BevelOuter = bvNone
    Constraints.MinWidth = 100
    TabOrder = 0
    ExplicitHeight = 823
    object TreePanel: TGroupBox
      Left = 0
      Top = 0
      Width = 280
      Height = 823
      Align = alClient
      Caption = 'Project &tree'
      Constraints.MinHeight = 150
      TabOrder = 0
    end
  end
  object ViewerPanel: TPanel
    Left = 284
    Top = 22
    Width = 808
    Height = 745
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitHeight = 823
    object Splitter1: TSplitter
      Left = 0
      Top = 585
      Width = 808
      Height = 4
      Cursor = crVSplit
      Align = alBottom
      ExplicitLeft = 1
      ExplicitTop = 376
      ExplicitWidth = 475
    end
    object LowerRightPanel: TPanel
      Left = 0
      Top = 589
      Width = 808
      Height = 156
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitTop = 667
      object Splitter4: TSplitter
        Left = 634
        Top = 0
        Width = 4
        Height = 156
        Align = alRight
        ExplicitLeft = 0
      end
      object LogPanel: TPanel
        Left = 638
        Top = 0
        Width = 170
        Height = 156
        Align = alRight
        BevelOuter = bvNone
        Constraints.MinWidth = 4
        TabOrder = 1
        object LogListBox: TListBox
          Left = 0
          Top = 0
          Width = 170
          Height = 156
          Style = lbOwnerDrawFixed
          Align = alClient
          BevelInner = bvNone
          BevelOuter = bvNone
          Color = 2976796
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
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
        Width = 634
        Height = 156
        ActivePage = TabSheet3
        Align = alClient
        Constraints.MinWidth = 100
        TabOrder = 0
        object TabSheet2: TTabSheet
          Caption = 'TabSheet2'
          ImageIndex = 1
          TabVisible = False
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
        end
        object TabSheet1: TTabSheet
          Caption = 'Float/Int'
          TabVisible = False
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          DesignSize = (
            626
            146)
          object Label1: TLabel
            Left = 16
            Top = 8
            Width = 47
            Height = 13
            Caption = '&Edit value'
            FocusControl = TrackBar1
          end
          object TrackBar1: TTrackBar
            Left = 8
            Top = 25
            Width = 615
            Height = 45
            Anchors = [akLeft, akTop, akRight]
            Max = 1000
            TabOrder = 0
            TickStyle = tsNone
            OnChange = TrackBar1Change
          end
        end
        object TabSheet3: TTabSheet
          Caption = 'Expression'
          ImageIndex = 2
          TabVisible = False
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          DesignSize = (
            626
            146)
          object ExprPanel: TGroupBox
            Left = 0
            Top = 0
            Width = 626
            Height = 107
            Align = alTop
            Anchors = [akLeft, akTop, akRight, akBottom]
            Caption = '&Code editor'
            TabOrder = 0
            OnClick = ExprPanelClick
          end
          object ExprCompileButton: TButton
            Left = 8
            Top = 115
            Width = 75
            Height = 25
            Anchors = [akLeft, akBottom]
            Caption = '&OK'
            Enabled = False
            TabOrder = 1
            OnClick = ExprCompileButtonClick
          end
          object CompileErrorLabel: TStaticText
            Left = 168
            Top = 113
            Width = 455
            Height = 26
            Anchors = [akLeft, akRight, akBottom]
            AutoSize = False
            Color = clWhite
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clRed
            Font.Height = -16
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentColor = False
            ParentFont = False
            TabOrder = 2
            Transparent = False
          end
          object ExprHelpButton: TButton
            Left = 87
            Top = 115
            Width = 75
            Height = 25
            Anchors = [akLeft, akBottom]
            Caption = '&Help'
            TabOrder = 3
            OnClick = ExprHelpButtonClick
          end
        end
        object ShaderTabSheet: TTabSheet
          Caption = 'ShaderTabSheet'
          ImageIndex = 3
          TabVisible = False
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          DesignSize = (
            626
            146)
          object Label6: TLabel
            Left = 96
            Top = 122
            Width = 93
            Height = 13
            Anchors = [akLeft, akBottom]
            Caption = 'GLSL Shader editor'
            ExplicitTop = 120
          end
          object CompileShaderButton: TButton
            Left = 8
            Top = 115
            Width = 75
            Height = 25
            Anchors = [akLeft, akBottom]
            Caption = '&OK'
            Enabled = False
            TabOrder = 0
            OnClick = CompileShaderButtonClick
          end
          object ShaderPanel: TGroupBox
            Left = 0
            Top = 0
            Width = 626
            Height = 107
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
      Width = 808
      Height = 585
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitHeight = 663
      object Splitter3: TSplitter
        Left = 291
        Top = 0
        Width = 4
        Height = 585
        ExplicitLeft = 100
        ExplicitHeight = 574
      end
      object PropEditorPanel: TGroupBox
        Left = 0
        Top = 0
        Width = 291
        Height = 585
        Align = alLeft
        Caption = '&Properties'
        TabOrder = 1
        ExplicitHeight = 663
      end
      object ViewerPageControl: TPageControl
        Left = 295
        Top = 0
        Width = 513
        Height = 585
        ActivePage = ViewerGlTabSheet
        Align = alClient
        TabOrder = 0
        ExplicitHeight = 663
        object ViewerGlTabSheet: TTabSheet
          Caption = 'ViewerGlTabSheet'
          TabVisible = False
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
          object RotateModelPanel: TPanel
            Left = 0
            Top = 0
            Width = 505
            Height = 50
            Align = alTop
            TabOrder = 0
            object ViewTranslateLabel: TLabel
              Left = 416
              Top = 5
              Width = 32
              Height = 30
              Alignment = taRightJustify
              AutoSize = False
              Caption = 'ViewTranslateLabel'
              Font.Charset = ANSI_CHARSET
              Font.Color = clWindowText
              Font.Height = -8
              Font.Name = 'Arial'
              Font.Style = []
              ParentFont = False
              WordWrap = True
            end
            object Label2: TLabel
              Left = 7
              Top = 22
              Width = 42
              Height = 13
              Caption = 'Rotate X'
            end
            object Label3: TLabel
              Left = 112
              Top = 22
              Width = 42
              Height = 13
              Caption = 'Rotate Y'
            end
            object Label4: TLabel
              Left = 217
              Top = 22
              Width = 42
              Height = 13
              Caption = 'Rotate Z'
            end
            object Label5: TLabel
              Left = 324
              Top = 22
              Width = 27
              Height = 13
              Caption = 'Zoom'
            end
            object ViewRotateXTrackBar: TTrackBar
              Left = 0
              Top = 0
              Width = 100
              Height = 24
              Max = 100
              TabOrder = 0
              TickStyle = tsNone
              OnChange = ViewRotateXTrackBarChange
            end
            object TrackBar2: TTrackBar
              Tag = 1
              Left = 104
              Top = 0
              Width = 100
              Height = 24
              Max = 100
              TabOrder = 1
              TickStyle = tsNone
              OnChange = ViewRotateXTrackBarChange
            end
            object TrackBar3: TTrackBar
              Tag = 2
              Left = 209
              Top = 0
              Width = 100
              Height = 24
              Max = 100
              TabOrder = 2
              TickStyle = tsNone
              OnChange = ViewRotateXTrackBarChange
            end
            object ZoomTrackBar: TTrackBar
              Left = 316
              Top = 0
              Width = 100
              Height = 24
              TabOrder = 3
              TickStyle = tsNone
              OnChange = ZoomTrackBarChange
            end
            object Panel4: TPanel
              Left = 250
              Top = 1
              Width = 254
              Height = 48
              Align = alRight
              BevelOuter = bvNone
              ParentBackground = False
              TabOrder = 4
              DesignSize = (
                254
                48)
              object NormalsCheckBox: TCheckBox
                Left = 9
                Top = 5
                Width = 61
                Height = 17
                Anchors = [akTop, akRight]
                Caption = 'Normals'
                TabOrder = 0
                OnClick = NormalsCheckBoxClick
              end
              object ResetCameraButton: TButton
                Left = 172
                Top = 25
                Width = 73
                Height = 17
                Anchors = [akTop, akRight]
                Caption = 'Reset camera'
                TabOrder = 1
                OnClick = ResetCameraButtonClick
              end
              object ResetModelButton: TButton
                Left = 172
                Top = 5
                Width = 73
                Height = 17
                Anchors = [akTop, akRight]
                Caption = '&Reset time'
                Enabled = False
                TabOrder = 2
                OnClick = ResetModelButtonClick
              end
              object UpdateTimeCheckBox: TCheckBox
                Left = 72
                Top = 5
                Width = 81
                Height = 17
                Anchors = [akTop, akRight]
                Caption = 'Update time'
                TabOrder = 3
                OnClick = UpdateTimeCheckBoxClick
              end
              object WireframeCheckBox: TCheckBox
                Left = 72
                Top = 22
                Width = 74
                Height = 17
                Anchors = [akTop, akRight]
                Caption = 'Wireframe'
                TabOrder = 4
              end
              object BoundsCheckBox: TCheckBox
                Left = 9
                Top = 22
                Width = 61
                Height = 17
                Hint = 'Click to display collision bounds'
                Anchors = [akTop, akRight]
                Caption = 'Bounds'
                TabOrder = 5
                OnClick = BoundsCheckBoxClick
              end
            end
          end
          object AppControlPanel: TPanel
            Left = 0
            Top = 50
            Width = 505
            Height = 41
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 1
            object AppStartButton: TBitBtn
              Left = 7
              Top = 6
              Width = 75
              Height = 25
              Action = AppPreviewStartAction
              Caption = 'Start'
              TabOrder = 0
            end
            object AppStopButton: TBitBtn
              Left = 88
              Top = 6
              Width = 75
              Height = 25
              Action = AppPreviewStopAction
              Caption = 'Stop'
              Glyph.Data = {
                36040000424D3604000000000000360000002800000010000000100000000100
                2000000000000004000000000000000000000000000000000000FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF007060D0002010C0002010C0002010C0002010C0002010C0002010
                C0002010C0002010C0002010C0007060D000FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF002010C000A8A8F800A8A8F800A8A8F800A8A8F800A8A8F800A8A8
                F800A8A8F800A8A8F800A8A8F8002010C000FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF002010C0009898D8004038E8004038E8004038E8004038E8004038
                E8004038E8004038E8009898D8002010C000FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF003828C8007878F0003830E8003830E8003830E8003830E8003830
                E8003830E8003830E8007878F0003828C800FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF003838B8007068F0003020E8003020E8003020E8003020E8003020
                E8003020E8003020E8007068F0003838B800FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF003838B8007878F0003830E8003830E8003830E8003830E8003830
                E8003830E8003830E8007878F0003840C800FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF003840C8009898D8004840E8004840E8004840E8004840E8004840
                E8004840E8004840E8009898D8003840C800FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF003840C8009898D8005048E8005048E8005048E8005048E8005048
                E8005048E8005048E8009898D8003840C800FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF004050C800A8A8F8005050E8005050E8005050E8005050E8005050
                E8005050E8005050E800A8A8F8004050C800FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF004050C800A8B0F800A8B0F800A8B0F800A8B0F800A8B0F800A8B0
                F800A8B0F800A8B0F800A8B0F8004050C800FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF009898D8004858C8004858C8004858C8004858C8004858C8004858
                C8004858C8004858C8004858C8009898D800FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
                FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
              TabOrder = 1
            end
            object DisableShadersCheckBox: TCheckBox
              Left = 192
              Top = 2
              Width = 97
              Height = 17
              Hint = 'Click to disable OpenGL shaders'
              Caption = 'Disable shaders'
              TabOrder = 2
              OnClick = DisableShadersCheckBoxClick
            end
            object DisableFBOCheckBox: TCheckBox
              Left = 192
              Top = 18
              Width = 97
              Height = 17
              Hint = 'Click to disable OpenGL FBO (render to texture)'
              Caption = 'Disable FBO'
              TabOrder = 3
              OnClick = DisableFBOCheckBoxClick
            end
          end
        end
        object ViewerSoundTabSheet: TTabSheet
          Caption = 'ViewerSoundTabSheet'
          ImageIndex = 1
          TabVisible = False
          inline SoundEditFrame1: TSoundEditFrame
            Left = 0
            Top = 0
            Width = 505
            Height = 575
            Align = alClient
            ParentShowHint = False
            ShowHint = True
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 505
            ExplicitHeight = 575
            inherited PageControl1: TPageControl
              Width = 505
              Height = 575
              ExplicitWidth = 505
              ExplicitHeight = 575
              inherited TabSheet1: TTabSheet
                ExplicitLeft = 4
                ExplicitTop = 24
                ExplicitWidth = 497
                ExplicitHeight = 547
                inherited Label8: TLabel
                  Width = 55
                  ExplicitWidth = 55
                end
                inherited Label6: TLabel
                  Width = 25
                  ExplicitWidth = 25
                end
                inherited Label12: TLabel
                  Width = 50
                  ExplicitWidth = 50
                end
                inherited Label1: TLabel
                  Width = 82
                  ExplicitWidth = 82
                end
                inherited Label16: TLabel
                  Width = 35
                  ExplicitWidth = 35
                end
                inherited Label17: TLabel
                  Width = 19
                  ExplicitWidth = 19
                end
                inherited Panel1: TPanel
                  Width = 468
                  ExplicitWidth = 468
                  inherited SoundGraphPaintBox: TPaintBox
                    Width = 468
                    ExplicitWidth = 245
                  end
                end
                inherited GroupBox4: TGroupBox
                  inherited Label5: TLabel
                    Width = 49
                    ExplicitWidth = 49
                  end
                  inherited Label11: TLabel
                    Width = 33
                    ExplicitWidth = 33
                  end
                end
                inherited GroupBox2: TGroupBox
                  inherited Label3: TLabel
                    Width = 28
                    ExplicitWidth = 28
                  end
                end
                inherited GroupBox1: TGroupBox
                  inherited Label2: TLabel
                    Width = 62
                    ExplicitWidth = 62
                  end
                  inherited Label18: TLabel
                    Width = 62
                    ExplicitWidth = 62
                  end
                end
              end
              inherited TabSheet2: TTabSheet
                ExplicitLeft = 0
                ExplicitTop = 0
                ExplicitWidth = 0
                ExplicitHeight = 0
                inherited Label15: TLabel
                  Width = 58
                  ExplicitWidth = 58
                end
                inherited Label19: TLabel
                  Width = 69
                  ExplicitWidth = 69
                end
              end
            end
          end
        end
        object ViewerMusicTabSheet: TTabSheet
          Caption = 'ViewerMusicTabSheet'
          ImageIndex = 2
          TabVisible = False
          inline MusicEditFrame1: TMusicEditFrame
            Left = 0
            Top = 0
            Width = 505
            Height = 575
            Align = alClient
            TabOrder = 0
            TabStop = True
            ExplicitWidth = 505
            ExplicitHeight = 575
          end
        end
        object ViewerBlankTabSheet: TTabSheet
          Caption = 'ViewerBlankTabSheet'
          ImageIndex = 3
          TabVisible = False
          ExplicitLeft = 0
          ExplicitTop = 0
          ExplicitWidth = 0
          ExplicitHeight = 0
        end
        object ViewerBitmapTabSheet: TTabSheet
          ImageIndex = 4
          TabVisible = False
          inline BitmapEditFrame1: TBitmapEditFrame
            Left = 0
            Top = 0
            Width = 505
            Height = 575
            Align = alClient
            TabOrder = 0
            ExplicitWidth = 505
            ExplicitHeight = 575
            inherited Splitter1: TSplitter
              Left = 314
              Height = 575
              ExplicitLeft = 223
              ExplicitHeight = 463
            end
            inherited LeftPanel: TGroupBox
              Width = 314
              Height = 575
              ExplicitWidth = 314
              ExplicitHeight = 653
              inherited ScrollBox1: TScrollBox
                Width = 310
                Height = 636
                ExplicitWidth = 310
                ExplicitHeight = 636
              end
            end
            inherited RightPanel: TGroupBox
              Left = 318
              Width = 187
              Height = 575
              ExplicitLeft = 318
              ExplicitWidth = 187
              ExplicitHeight = 653
              inherited PreviewPanel: TPanel
                Width = 183
                Height = 636
                ExplicitWidth = 183
                ExplicitHeight = 636
                inherited Panel1: TPanel
                  Top = 614
                  Width = 183
                  ExplicitTop = 614
                  ExplicitWidth = 183
                end
              end
            end
          end
        end
        object ViewerMeshTabSheet: TTabSheet
          ImageIndex = 5
          TabVisible = False
          inline MeshEditFrame1: TMeshEditFrame
            Left = 0
            Top = 0
            Width = 505
            Height = 575
            Align = alClient
            TabOrder = 0
            ExplicitWidth = 505
            ExplicitHeight = 575
            inherited Splitter1: TSplitter
              Left = 214
              Height = 575
              ExplicitLeft = 223
              ExplicitHeight = 483
            end
            inherited LeftPanel: TGroupBox
              Width = 214
              Height = 575
              ExplicitWidth = 214
              ExplicitHeight = 653
              inherited ScrollBox1: TScrollBox
                Width = 210
                Height = 636
                ExplicitWidth = 210
                ExplicitHeight = 636
              end
            end
            inherited RightPanel: TGroupBox
              Left = 218
              Width = 287
              Height = 575
              ExplicitLeft = 218
              ExplicitWidth = 287
              ExplicitHeight = 653
              inherited WireframeCheckBox: TCheckBox
                Width = 283
                ExplicitWidth = 283
              end
              inherited NormalsCheckBox: TCheckBox
                Width = 283
                ExplicitWidth = 283
              end
              inherited Panel1: TPanel
                Top = 629
                Width = 283
                ExplicitTop = 629
                ExplicitWidth = 283
              end
              inherited PreviewPanel: TPanel
                Width = 283
                Height = 580
                ExplicitWidth = 283
                ExplicitHeight = 580
              end
            end
          end
        end
      end
    end
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 1092
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
    Interval = 20
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
      ShortCut = 16422
      SecondaryShortCuts.Strings = (
        'Ctrl-Up')
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
      OnExecute = NewProjectActionExecute
    end
    object FileExitAction: TFileExit
      Category = 'File'
      Caption = 'E&xit'
      Hint = 'Exit|Quits the application'
    end
    object FileOpenAction: TFileOpen
      Category = 'File'
      Caption = '&Open...'
      Dialog.DefaultExt = '*.zgeproj'
      Dialog.Filter = 'Project files (*.zgeproj)|*.zgeproj|Any file (*.*)|*.*'
      Hint = 'Open|Opens an existing file'
      ImageIndex = 5
      ShortCut = 16463
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
      Caption = 'Import 3DS-file'
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
    BlendColor = clBlack
    BkColor = clBlack
    DrawingStyle = dsTransparent
    Masked = False
    Left = 208
    Top = 96
    Bitmap = {
      494C01010E0008014C011000100000000000FF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000004000000001002000000000000040
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000002379EFFF1C73E2FF2677ECFF2978
      F6FF2577E4FF2678E7FF2471E2FF226EE9FF2170F3FF246EEDFF1F5DDAFF265E
      E5FF2069E3FF2067DDFF1B61DAFF2248D3FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000002A77DDFF145AB2FF286FCEFF2879
      EBFF1D5DAFFF2D6DCAFF205BB4FF2164D9FF2071F0FF296EDCFF1F4EA5FF295C
      D6FF1F58B5FF2050A2FF184A9FFF1643B7FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000005B9FF1FF92B4DBFF2461B4FF5492
      E2FF97C0E0FF6197D8FF8FB1DDFF2162D1FF196FF5FF5D94EBFF8CA7D5FF3C69
      CDFF9BBDE9FFACBDDCFFACBBD9FF4362BAFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000840000008400
      0000840000000000000000000000000000005898EDFFF0F6F8FF416FAFFF92B8
      E2FFDBEEF6FF72A2D6FFDDEAF7FF2866CAFF1867E4FF7FA9E9FFD2DCEDFF4974
      C7FFEDF9FEFFF9FBFEFFF7F7FFFF5E79C7FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000840000008400
      0000840000000000000000000000000000003981E9FFEAF1F6FF7493BAFFBFD2
      E6FFBFD0E6FF6B9BDCFFE4EDF2FF3267BEFF2158B1FF81A4D9FFDAE2EAFF4879
      C9FFF5FAFAFFBFD5EBFFB1C9F0FF4874D8FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000002576F1FFBDD5F5FFB7C8D8EEE3ED
      F7FF88A8E2FF6397E8FFE0ECF7FF5880C1FF8DAAD3FF7EA0CBFFD7E3EDFF4577
      CCFFF1F9FCFF4F7EBEFF3067D3FF205ADDFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000840000008400
      0000840000000000000000000000000000002272F4FF89B3EDFFF2F9F9FDEBF9
      FCFF4D85E0FF6097E9FFE0EBF8FF839CC6FFD8E5F1FF809EC1FFD3E1EFFF4876
      CCFFF2F8FEFF4276C8FF215DE3FF1F59E7FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000840000008400
      0000840000000000000000000000000000002473F3FF5D93E5FFF4FAF6FFDAEB
      F3FF2B6ED8FF6198E8FFE5EDF7FFA8B7CFFFF0F7F8FF95ADC6FFD2E0EFFF4A77
      CAFFF4F8FEFF4473C5FF295FE4FF255AE6FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008400
      0000840000008400000000000000000000001D71F0FF397CDFFFEDF5FCFFB7CA
      DDFF2260CCFF5F96ECFFE5ECF3FFC7D5DDFFEFF6F9FFBAC6D5FFD4E0EDFF4178
      C9FFEFF9FEFF4372C6FF2360E7FF235CE4FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000840000008400000084000000000000001D71F0FF538CDEFFF7F9F7FFD5DF
      EBFF2258B6FF5F96EAFFEFF1F5FFE6EFF3FFB7CEE6FFE4EBF1EBD9E2EDFF4178
      CAFFF0FAFEFF4573C5FF2463E8FF225EE6FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000840000008400000084000000000000000000
      0000000000008400000084000000840000001F70F0FF83ACE5FFFAF9F8FFF1F7
      F8FF3D66AFFF5D94E4FFF8F8F9FFE9F1F7FF78A7DFFFF9FBF8FFDDE3ECFF4176
      CDFFF2FAFDFF4573C6FF2567E8FF2260E8FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000840000008400000084000000000000000000
      000000000000840000008400000084000000266FEBFFB1CEEEFFD6DBE9FFE6F2
      F7FF718EBEFF5C91DCFFF9FAFBFFCBD8EDFF5193E8FFF7FAF9FFDCE3EDFF4076
      CFFFF2FAFDFF4373C8FF2368E9FF2262E9FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000840000008400000084000000000000000000
      0000000000008400000084000000840000003272E2FFD7EDFAFF97AFDDFFBCD9
      F1FFAABDD9FF578BD0FFF5FCFEFFA2BCE5FF3480EAFFE8F1FBFFD7E1F0FF3E78
      D1FFEEFAFDFF4375CFFF1F6AEBFF2164EBFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400000084000000840000008400
      000084000000840000008400000000000000558BEAFFE5F8F9FF628FE4FF85B6
      EAFFDEEBF9FF6194D2FFECFAFEFF7CA7EAFF2874EBFFC8DBF8FFD2E4F6FF4180
      D8FFEBF9FEFF487EDDFF1D6DEFFF2166EDFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084000000840000008400
      000084000000840000000000000000000000558CF0FFA2C5EFFF3B79EBFF4E8F
      E7FFA5C5F2FF5D98E7FFA2C8F3FF518DECFF2571EEFF80A8EDFF91B8EEFF3D84
      E3FFA1C8F4FF4181EAFF1F6EF3FF2168EFFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000002868EBFF2B6DEBFF256FF0FF2673
      ECFF2976E8FF2B78EDFF2B78EDFF2572EEFF2370F2FF2A71EEFF2773E6FF2C7F
      EBFF297DEDFF2675F3FF246EF5FF226AF1FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000369DD9003199D8002C94
      D7002890D600238CD5001E88D4001A84D3001580D200117CD1000E79D1000A76
      D0000773CF000470CF00016ECE00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000041882600418026004180260041882600000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000003DA3DA00BCEBFA00BCEB
      FC00BFEEFE00C6F4FF00CEF8FF00D3FAFF00D0F8FF00C7F2FF00BAE9FC00B3E4
      F900B0E2F800B0E2F8000571CF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000041882D00579A48005EA457006FAD66006FAD66005EA45700579A48004188
      2D00000000000000000000000000000000000000000000000000000000007060
      D0002010C0002010C0002010C0002010C0002010C0002010C0002010C0002010
      C0002010C0007060D00000000000000000000000000043A8DB00BFECFB0059CF
      F50041B0EC004EBAEF005AC2EF0060C6EF005CC4EF004CB6EF0037A5E6002A9A
      E10038B8EE00B1E3F8000975D000000000000000000000000000000000000000
      0000000000000000000000000000840084008400840084848400000000000000
      0000000000000000000000000000000000000000000000000000000000004891
      3A005EA45700ADCBAD00ADCBAD003A8833003A883A00489A410057A457005EA4
      570048913A000000000000000000000000000000000000000000000000002010
      C000A8A8F800A8A8F800A8A8F800A8A8F800A8A8F800A8A8F800A8A8F800A8A8
      F800A8A8F8002010C00000000000000000000000000049ADDC00C1EEFB005FD3
      F7006CDBFC007FE5FF008FEDFF0097F2FF0093EDFF007CDFFF005BCCF80046BE
      EF003CBAEE00B3E3F9000E79D100000000000000000000000000000000000000
      0000000000008400840084008400FFFFFF00FFFFFF00C6C6C600848484000000
      000000000000000000000000000000000000000000000000000041882D00579A
      4F0041914100ADCBAD00FFFFFF00ADCBAD0041883A0041883A00419141004F9A
      48005EA4570041882D0000000000000000000000000000000000000000002010
      C0009898D8004038E8004038E8004038E8004038E8004038E8004038E8004038
      E8009898D8002010C0000000000000000000000000004EB2DD00C3EFFB0065D6
      F8004CB6EC005ABDEF0095EBFF003097DD004D82AB0084E1FF0041A9E900329F
      E10042BEEF00B4E5F900137ED200000000000000000000000000000000008400
      840084008400FFFFFF00FFFFFF000000000000000000C6C6C600C6C6C6008484
      84000000000000000000000000000000000000000000000000004F9141004F91
      48004F914800ADCBAD00FFFFFF00FFFFFF00ADCBAD004F9148004F9148004F9A
      4800579A4F005791410000000000000000000000000000000000000000003828
      C8007878F0003830E8003830E8003830E8003830E8003830E8003830E8003830
      E8007878F0003828C80000000000000000000000000053B7DE00C6F0FC006AD9
      F8007CE2FD0090E8FF0099E9FF00329FDF00548BB2008AE2FF006AD0F90050C5
      F10046C1F000B6E7F9001883D30000000000848484008400840084008400FFFF
      FF00FFFFFF000000000000000000840084008400840000000000C6C6C600C6C6
      C60084848400000000000000000000000000000000004180260033802D003380
      2D0033802D00ADCBAD00FFFFFF00FFFFFF00FFFFFF00ADCBAD0033802D003380
      2D00488841005EA4570041802600000000000000000000000000000000003838
      B8007068F0003020E8003020E8003020E8003020E8003020E8003020E8003020
      E8007068F0003838B80000000000000000000000000058BBDF00C7F1FC006FDC
      F90056BBED0061BDEF009BE7FF0035A6E2004BA4E10090E2FF0049ADE90038A4
      E30049C4F000B8E8F9001E88D400000000008484840084008400FFFFFF000000
      000000000000840084008400840084008400840084008400840000000000C6C6
      C600C6C6C60084848400000000000000000000000000418020003A7733003A77
      33003A773300ADCBAD00FFFFFF00FFFFFF00FFFFFF00FFFFFF00BAD2BA003A77
      33003A7733006F9A5E0041802000000000000000000000000000000000003838
      B8007878F0003830E8003830E8003830E8003830E8003830E8003830E8003830
      E8007878F0003840C8000000000000000000000000005CBFE000C8F3FC0075DF
      F90089E6FD0095E7FF009AE5FF00AAEEFF00A8EDFF0099E3FF0074D5F90059CC
      F3004FC8F100BBE9FA00248DD500000000008484840000000000000000008400
      840084008400840084000084840000FFFF008400840084008400840084000000
      0000C6C6C600C6C6C60084848400000000000000000048883300488048004880
      480048804800ADCBAD00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0078E9
      C2004880480077AD770041802600000000000000000000000000000000003840
      C8009898D8004840E8004840E8004840E8004840E8004840E8004840E8004840
      E8009898D8003840C80000000000000000000000000060C2E100C9F3FC00CBF3
      FD00D4F6FE00D7F6FF00D8F4FF00E0F8FF00DFF8FF00DAF5FF00CDF1FC00C2ED
      FA00BDEBFA00BDEBFA002B93D600000000008484840084008400840084008400
      8400840084008400840084008400008484008400840084008400840084008400
      840000000000C6C6C60000000000000000000000000048883300579A5E00579A
      5E00579A5E00BAD2BA00FFFFFF00FFFFFF00FFFFFF00FFFFFF0026CC8000579A
      5E00579A5E0077AD770048883300000000000000000000000000000000003840
      C8009898D8005048E8005048E8005048E8005048E8005048E8005048E8005048
      E8009898D8003840C80000000000000000000000000061C3E10088A0A8009191
      91008E8E8E005AB9DC0055B8DF0051B5DE004DB1DD0049ADDC0046A8D7007878
      780076767600657E8D003199D800000000000000000084008400FFFFFF008400
      84008400840084008400840084008400840000FFFF0000FFFF00840084008400
      8400840084000000000000000000000000000000000000000000579A4F005EA4
      6F005EA46F00BAD2BA00FFFFFF00FFFFFF00FFFFFF0026CC80005EA46F005EA4
      6F005EA46F005EA46F0000000000000000000000000000000000000000004050
      C800A8A8F8005050E8005050E8005050E8005050E8005050E8005050E8005050
      E800A8A8F8004050C8000000000000000000000000000000000099999900C6C6
      C600949494008F8F8F00000000000000000000000000000000007E7E7E007D7D
      7D00ABABAB00767676000000000000000000000000000000000084008400FFFF
      FF0084008400840084008400840084008400840084000084840000FFFF0000FF
      FF00840084008400840000000000000000000000000000000000488833006FAD
      80006FB78800BAD2BA00FFFFFF00FFFFFF0026CC80006FB788006FB788006FB7
      88006FB788004888330000000000000000000000000000000000000000004050
      C800A8B0F800A8B0F800A8B0F800A8B0F800A8B0F800A8B0F800A8B0F800A8B0
      F800A8B0F8004050C800000000000000000000000000000000009D9D9D00C4C4
      C400A1A1A1009393930000000000000000000000000000000000828282008989
      8900A9A9A9007979790000000000000000000000000000000000000000008400
      8400FFFFFF00840084008400840084008400008484008400840000FFFF0000FF
      FF00840084008400840084008400000000000000000000000000000000005791
      480077B7880026CC8000FFFFFF0026CC80006FB791006FB7910077B7910077B7
      9100579148000000000000000000000000000000000000000000000000009898
      D8004858C8004858C8004858C8004858C8004858C8004858C8004858C8004858
      C8004858C8009898D80000000000000000000000000000000000A1A1A100BABA
      BA00BFBFBF009898980094949400919191008E8E8E008A8A8A0087878700A8A8
      A8009E9E9E007D7D7D0000000000000000000000000000000000000000000000
      000084008400FFFFFF00840084008400840000FFFF0000FFFF0000FFFF008400
      8400840084008400840000000000000000000000000000000000000000000000
      00004888330026CC800026CC800088C19A0088C1A40080B7880066A466004F88
      3A00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000A4A4A400A1A1
      A100C4C4C400BEBEBE00A1A1A100969696009393930097979700AEAEAE00AEAE
      AE00848484008181810000000000000000000000000000000000000000000000
      00000000000084008400FFFFFF00840084008400840084008400840084008400
      8400000000000000000000000000000000000000000000000000000000000000
      000000000000000000004F883A0041802600418026004F883A00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000A4A4
      A400A2A2A200BCBCBC00CACACA00CCCCCC00CACACA00C2C2C200ADADAD008C8C
      8C00898989000000000000000000000000000000000000000000000000000000
      0000000000000000000084008400FFFFFF008400840084008400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000A5A5A500A3A3A300A0A0A0009D9D9D009A9A9A0097979700949494009090
      9000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000840084008400840000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000009198980091989800919898009198980091989800919898009198
      9800919898009198980091989800919898000000000000000000AFC1D4006790
      B500638DB3005C89B1005684AF00AE948700AE958700AD958700AD948700AD94
      8700AD958700AD948700AD948700000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000091989800919898009198
      98009198980090999A00F7E7DC00F7E7DC00F7E7DD00F7E7DC00F7E7DD00F7E7
      DC00F7E7DD00F7E7DD00F7E7DC00909A9A0000000000000000006A96BC006CAE
      D80071C5F10071C6F20071C6F100AA988C00F8E8DE00F7E9DF00F8E8DF00F7E9
      DE00F7E9DE00F7E8DE00AA988D0000000000000000000000000000000000C4C4
      E3002513C5007465D40000000000000000000000000000000000000000007465
      D4002513C500C4C4E30000000000000000007088900060809000607880005070
      8000506070004058600040485000303840002030300020203000101820001010
      1000101020000000000000000000000000000000000090999A00F7E7DC00F7E7
      DC00F7E7DD008D9C9D00F8E9DF00C7A59100C19E8900BA937E00B1887100A87E
      6600F7E8DF00F7E8DE00F8E8DE008D9B9E0000000000000000006B98BE0079C8
      F10079C9F20079C8F20079C9F200A69C9200F8EAE100F7EBE200F8EAE200F8EB
      E200F8EAE100F8EAE200A69B9200000000000000000000000000C4C4E3002513
      C500342CE0002513C5007465D4000000000000000000000000007465D4002513
      C500332CDF002513C500C4C4E300000000007088900090A0B00070B0D0000090
      D0000090D0000090D0000090C0001088C0001080B0001080B0002078A0002070
      900020486000AE9783000000000000000000000000008D9C9D00F8E9DF00C7A5
      9100C19E89008A9EA100F8EAE100F8EAE100F8EAE100F8EAE100F8EAE100F8EA
      E100F8EAE100F8EAE100F8EAE1008A9EA20000000000000000006C9BC10083CC
      F20083CDF20083CCF30083CCF300A1A09900F8EDE600C7A59100BC978100AD84
      6D00A3765E00F8EDE500A19F9A00000000000000000000000000A89CDF002513
      C500523FF100513FF0002513C5007465D400A89CDF007465D4002513C500523F
      F100513FF0002513C500A89CDF00000000008088900080C0D00090A8B00080E0
      FF0060D0FF0050C8FF0050C8FF0040C0F00030B0F00030A8F00020A0E0001090
      D00020688000615A57000000000000000000000000008A9EA200F8EAE100F8EA
      E100F8EAE10087A0A600F9ECE400C7A59100C3A08C00BF9A8400B8927C00B289
      7300AC826A00A77A6300F9ECE40087A1A60000000000000000006F9EC4008ED1
      F2008ED0F2008ED0F3008ED0F3009CA4A100F9F0EA00F9F0EA00F9F0EA00F9F0
      EA00F9F0EA00F9F0EA009CA4A100000000000000000000000000000000007465
      D4002819C600523FF100523FF1002F2AD8002718C5002F2AD800523FF100513F
      F0002719C5007465D40000000000000000008090A00080D0F00090A8B00090C0
      D00070D8FF0060D0FF0060D0FF0050C8FF0050C0FF0040B8F00030B0F00030A8
      F0001088D00020486000D8BBA500000000000000000087A1A600F9ECE400C7A5
      9100C3A08C0085A3AA00F9EEE600F9EEE700F9EEE700F9EEE700F9EEE700F9EE
      E700F9EEE700F9EEE600F9EEE70084A3AB00000000000000000072A2C7009AD5
      F3009AD4F30099D5F40099D4F40097A8A800FAF3EF00C7A59100BC978100AD84
      6D00A3765E00F9F2EE0097A8A800000000000000000000000000000000000000
      00007465D4002C20C600523FF100523FF100523FF100523FF100523FF1002B20
      C5007465D4000000000000000000000000008090A00080D8F00080C8E00090A8
      B00080E0FF0070D0FF0060D8FF0060D0FF0060D0FF0050C8FF0040C0F00040B8
      F00030B0F0002068800074858800000000000000000084A4AA00F9EEE600F9EE
      E700F9EEE70081A6B000F9F0EA00C7A59100C3A08C00BF9A8400B8927C00B289
      7300AC826A00A77A6300F9F0EA0081A7B000000000000000000074A5CA00A5D8
      F500A5D8F400A5D9F400A5D9F40092ACB000FBF6F300FBF5F200FAF6F200FAF6
      F300FAF6F300FAF5F30092ADAF00000000000000000000000000000000000000
      0000000000003029C7003930E5005848F1005848F2005848F2003930E5003029
      C700000000000000000000000000000000008098A00090E0F00090E0FF0090A8
      B00090B8C00070D8FF0060D8FF0060D8FF0060D8FF0060D0FF0050D0FF0050C8
      FF0040B8F00030A0E0004A677400D9C3B4000000000081A7B000F9F0EA00C7A5
      9100C3A08C007DAAB400FAF2ED00FAF2ED00FAF2EE00FAF2ED00FAF2ED00FAF2
      ED00FAF2ED00FAF2ED00FAF2ED007EAAB500000000000000000076A9CE00B0DD
      F400B0DDF500B0DDF400B0DDF5008EB0B600FAF8F500C7A59100A3765E00FAF7
      F6008FC8E0007CBDDA008EB0B600000000000000000000000000000000000000
      0000000000003533C7003E36E5006055F2006055F2006054F2003E36E6003533
      C700000000000000000000000000000000008098A00090E0F000A0E8FF0080C8
      E00090A8B00080E0FF0080E0FF0080E0FF0080E0FF0080E0FF0080E0FF0080E0
      FF0070D8FF0070D8FF0050A8D00090909100000000007EAAB500FAF2ED00FAF2
      ED00FAF2EE007AACB900FBF5F000C7A59100C3A08C00BF9A8400B8927C00B289
      7300AC826A00A77A6300FBF4F0007AACB900000000000000000078ACD000BAE1
      F500BAE1F500BAE1F600BAE1F5008AB4BB00FBF9F800FBF9F800FBF9F800FBF9
      F8007CBDDA008EB0B60000000000000000000000000000000000000000000000
      00007465D4003A3DC7006B64F3006B63F3006A63F3006B63F4006A63F300393D
      C7007465D40000000000000000000000000090A0A000A0E8F000A0E8FF00A0E8
      FF0090B0C00090B0C00090A8B00090A8B00080A0B00080A0B0008098A0008098
      A0008090A0008090A0008088900070889000000000007BADBA00FBF5F000C7A5
      9100C3A08C0078AFBE00FBF5F200FBF6F300FBF6F200FBF6F300FBF5F300FBF6
      F300FBF6F300FBF6F300FBF6F30077AFBE0000000000000000007AAED300C2E4
      F600C2E3F600C2E4F500C2E4F6008AB4BB008AB4BB008AB4BB008AB4BB008AB4
      BB008AB4BB00649AC30000000000000000000000000000000000000000007C88
      D5003F47C8007573F3007473F4005B61E0003E47C9005B61E0007472F3007471
      F4003F47C9007C88D500000000000000000090A0B000A0E8F000A0F0FF00A0E8
      FF00A0E8FF0080D8FF0060D8FF0060D8FF0060D8FF0060D8FF0060D8FF0060D8
      FF00708890000000000000000000000000000000000077AFBD00FBF5F200FBF6
      F300FBF6F20075B2C100FBF8F500C7A59100BC978100AD846D00A3765E00FBF8
      F6008FC8E0007CBDDA0068B2D40075B1C10000000000000000007BB0D400C8E6
      F600C8E6F600C8E6F600C8E6F600C8E6F600C8E6F600C8E6F600C8E6F600C8E6
      F600C8E6F60075A8CC0000000000000000000000000000000000A0A6DC004350
      C9007E80F5007D81F5004350C9005E6ECE00A0A6DC005E6ECE004350C9007E80
      F4007D81F5004350C900A0A6DC000000000090A0B000A0F0F000B0F0F000A0F0
      FF00A0E8FF00A0E8FF0070D8FF0090A0A0008098A0008098A0008090A0008090
      9000708890000000000000000000000000000000000074B2C200FBF8F500C7A5
      9100BC97810073B4C400FBF9F700FCF9F800FBF9F800FBF9F700FCF9F800FCF9
      F8007CBDDA0069B2D50073B4C4000000000000000000000000007BB0D400C8E6
      F60096736000B4948100B4948100B4948000B4948100B4948100B49481009673
      5F00C8E6F60079ACCE0000000000000000000000000000000000C4C4E3004658
      CA004F52E3004658CA007C88D5000000000000000000000000007C88D5004658
      C9004F52E3004657CA00C4C4E3000000000090A8B000A0D0E000B0F0F000B0F0
      F000A0F0FF00A0E8FF0090A0B000BDC5BF000000000000000000000000000000
      0000000000009068500090685000906850000000000073B3C400FBF9F700FCF9
      F800FBF9F80071B5C700FCFAF900FCFAF900FCFAF900FCFAF900FCFAF900FCFA
      F90069B2D40071B5C70000000000000000000000000000000000C6D6E400619A
      C500619AC500A0827100C0A79800C0A79700C0A79700C0A79800A08271005281
      AD00619AC500C6D6E4000000000000000000000000000000000000000000C4C4
      E300495DCA007C88D50000000000000000000000000000000000000000007C88
      D500495DCA00C4C4E3000000000000000000DAD1C60090A8B00090A8B00090A8
      B00090A8B00090A8B000BFC2BE00000000000000000000000000000000000000
      0000000000000000000090685000906850000000000071B5C700FCFAF900FCFA
      F900FCFAF90071B5C70071B5C70071B5C70071B5C70071B5C70071B5C70071B5
      C70071B5C7000000000000000000000000000000000000000000000000000000
      000000000000AA918200CCBAAE00CCBAAE00CCBAAF00CCBAAE00AA9182000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000090786000D3B6A1000000
      000000000000A090800000000000907860000000000071B5C70071B5C70071B5
      C70071B5C70071B5C70071B5C70071B5C70071B5C70071B5C700000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000B09B8E00D4C1AB00D4C1AB00B09B8E00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000A0908000A088
      8000B09880000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000D3C5B900AB7A5B00AA79
      5B00AA7A5B00AB795B00AB795B00AA795B00AB795A00AB795B00AA7A5B00AA7A
      5B00AA795B00AB795B00AA7A5B00AA795B000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000106BA600106BA600106BA600106BA600000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000106BA600106BA60000000000000000000000
      00000000000000000000000000000000000000000000AA7B5E00E3C9B700E3C9
      B700E3C9B700A88064009DCFE4009DCFE4009DCFE400917558009DCFE400A880
      6400E3C9B700E3C9B700E3C9B700AA7B5E000000000000000000000000000000
      0000000000000000000054763500547635005476350054763500000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000106CA70083DDFB0083DDFB0083DDFB0083DDFB00106CA7000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000106CA70083DDFB0083DDFB00106CA700000000000000
      00000000000000000000000000000000000000000000A97D6100E4CBBA00E4CB
      BA00E4CCBA00A8806400F2CEB500F2CEB500F2CEB500AEB3A500AEB3A500A880
      6400E4CBBA00E4CBBA00E4CCBB00A97E60000000000000000000000000000000
      00000000000000000000577A380054B48D0053B48D0057793800000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000116EA90083DDFB0083DDFB0083DDFB0083DDFB00106EA9000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000106EA80083DDFB0083DDFB0083DDFB0083DDFB00106EA8000000
      00000000000000000000000000000000000000000000A87F6400E5CEBE00E5CE
      BE00E5CFBF00A8806400D9B29700D9B29700D9B297009C9B8A009C9B8A00A880
      6400E5CFBF00E6CFBE00E5CEBE00A88064000000000000000000000000000000
      000000000000000000005A7E3C0054B48D0053B48D005A7E3B00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000001170AA0083DDFB0083DDFB0083DDFB0083DDFB001170AB000000
      0000000000000000000000000000000000000000000000000000000000000000
      00001170AB008ADFFB008ADFFB008ADFFB008ADFFC008ADFFB008ADFFC001270
      AB000000000000000000000000000000000000000000A7826800E7D2C400E6D2
      C300E7D2C300A8806400FCF8F800FCF8F800FCF8F800FCF8F8009DCFE400A880
      6400E7D2C300E7D2C300E7D2C300A78367000000000000000000000000000000
      000000000000000000005E85400055B9960056B996005E854000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000001272AD0083DDFB0083DDFB0083DDFB0083DDFB001272AD000000
      0000000000000000000000000000000000000000000000000000000000001173
      AC0093E2FC0094E2FC0093E1FB0094E2FC0094E2FB0093E2FB0093E2FB0094E2
      FC001272AD0000000000000000000000000000000000A6856C00E8D6C900E9D6
      C900E8D6C900E9D6C900A8806400A8806400A8806400A8806400A8806400E8D6
      C900E8D6C800E9D6C800E8D6C900A6856B000000000000000000628B4400638B
      4500628C4500628C450058C0A00058C0A10058C0A00058C0A000628C4500628B
      4500638C4500638C450000000000000000000000000000000000000000000000
      0000000000001375AF0083DDFB0083DDFB0083DDFB0083DDFB001275AF000000
      00000000000000000000000000000000000000000000000000001375AF009FE5
      FB009FE5FB009FE5FB009FE5FC009FE5FC009FE6FC009FE5FC009FE6FB009FE5
      FC009FE5FB001275AF00000000000000000000000000A5887000EADBCF00EBDA
      CF00EBDACF00EADBCF00EADACF00EADACE00EBDACF00EBDACF00EBDACF00EBDA
      CF00EBDACE00EBDBCF00EADACE00A5887000000000000000000068934A005BC7
      AD005BC7AD005AC6AD005AC7AC005AC6AD005BC7AD005BC7AC005AC7AD005AC7
      AD005AC7AD0067934A0000000000000000000000000000000000000000000000
      0000000000001378B1008EE0FC008EE1FB008EE0FB008EE0FC001378B1000000
      00000000000000000000000000000000000000000000000000001377B1001378
      B2001377B2001378B200ACE9FC00ABE9FC00ABE9FC00ABE9FC001378B1001377
      B1001377B2001378B200000000000000000000000000A48B7300EDDFD500ECDF
      D500DAC3B2009E9B8A009E9B8A009E9B8A009E9B8A009E9B8A009E9B8A009E9B
      8A00DBC3B200ECDFD500EDDFD500A48B730000000000000000006C9A500060D6
      C50060D6C50061D5C50060D5C50060D6C50061D5C50060D5C60060D6C60060D6
      C50060D6C5006C9A4F0000000000000000000000000000000000147AB400137A
      B400137AB400147AB4009DE4FB009DE4FB009DE5FB009DE5FC00137AB400147A
      B400147BB400147AB40000000000000000000000000000000000000000000000
      000000000000137AB400B9EDFC00B9EDFC00B9EDFC00B9EDFC00147AB4000000
      00000000000000000000000000000000000000000000A38D7700EEE4DB00EEE4
      DB009E9B8A009DCFE4009DCFE4009DCFE4009DCFE4009DCFE4009DCFE4009DCF
      E4009E9B8A00EFE3DC00EEE3DB00A38E7700000000000000000071A2550070A1
      540071A1550071A1540063DBD00063DBD00063DBD00063DBD00071A1540071A1
      540071A1540071A1540000000000000000000000000000000000147DB600B0EA
      FC00B0EAFD00AFEAFC00B0EAFD00B0EAFC00B0EAFD00B0EAFC00AFEAFD00B0EA
      FC00AFEAFD00147CB60000000000000000000000000000000000000000000000
      000000000000147DB600C7F1FC00C6F1FD00C7F1FD00C7F1FD00147CB7000000
      00000000000000000000000000000000000000000000A2907B00F0E8E200F0E8
      E1009E9B8A00F2E9DB00F2E8DB00F2E9DB00F2E9DC00F2E8DB00F2E9DC00F2E9
      DB009E9B8A00F0E8E100F0E8E100A2907C000000000000000000000000000000
      0000000000000000000075A7580065E1DA0065E2DA0075A75800000000000000
      000000000000000000000000000000000000000000000000000000000000157F
      B800C3F0FD00C3F0FD00C3F0FD00C3F0FC00C2F0FD00C2F0FD00C3F0FD00C3F0
      FD00157FB9000000000000000000000000000000000000000000000000000000
      000000000000147FB900D3F5FD00D3F4FD00D3F5FD00D3F4FE00147EB8000000
      00000000000000000000000000000000000000000000A1937F00F3ECE600F2EC
      E7009E9B8A00F7EDE100F7EDE100F7EDE100F7EDE100F7EDE200F6EDE100F6ED
      E1009E9B8A00F2ECE700F2ECE700A1937F000000000000000000000000000000
      0000000000000000000079AC5C0065E1DA0065E2DA0079AC5D00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00001681BA00D4F5FE00D5F6FD00D5F6FD00D5F5FE00D4F5FD00D4F5FD001681
      BB00000000000000000000000000000000000000000000000000000000000000
      0000000000001581BB00DFF8FD00DFF8FD00DEF9FE00DFF8FD001581BA000000
      00000000000000000000000000000000000000000000A0968300F4F0EC00F4EF
      EC009E9B8A00F9F3EC00F9F3EC00F9F3EC00F8F3EC00F8F3EB00F9F3EC00F9F3
      EC009E9B8A00F4F0EC00F4F0EC00A09582000000000000000000000000000000
      000000000000000000007BB05F0066E5E00066E5E0007BB05F00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000001682BC00E5F9FE00E5F9FE00E4FAFE00E5FAFE001683BC000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000001682BC00E8FBFE00E8FBFE00E8FAFD00E9FAFE001682BC000000
      000000000000000000000000000000000000000000009F978500F5F2F000F5F3
      F0009E9B8A00F9F9F700F9F9F700F9F9F700F9F9F700F9F9F600F9F9F700F9F9
      F7009E9B8A00F5F2F000F5F3F0009F9886000000000000000000000000000000
      000000000000000000007BB05F007BB05F007BB05F007BB05F00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000001684BD00EFFDFE00EFFDFE001684BD00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000001684BD00E8FBFE00E8FBFE00E8FAFD00E9FAFE001684BD000000
      000000000000000000000000000000000000000000009E998800F6F5F300F6F5
      F300C6C3B700FAFDFE00FAFDFE00FAFDFE00FAFDFE00FAFDFE00FAFDFE00FAFD
      FE00C6C3B700F6F5F300F6F5F3009F9988000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000001684BD001684BD0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000001684BD001684BD001684BD001684BD00000000000000
      00000000000000000000000000000000000000000000D8D3CF009E9B8A009E9B
      8A009E9B8A0086837100BFBDB100BFBDB100BFBDB100BFBDB100BFBDB1008683
      71009E9B8A009E9B8A009E9B8A00D8D3CF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000400000000100010000000000000200000000000000000000
      000000000000000000000000FFFFFF00FFFF000000000000F9FF000000000000
      F9FF000000000000F3C700000000000073C700000000000027FF000000000000
      07C700000000000000C700000000000001E300000000000003F1000000000000
      06380000000000000E380000000000001E380000000000003F01000000000000
      7F83000000000000FFFF000000000000FFFFFFFF8001FFFFFC3FFFFF8001FE3F
      F00FE0038001F81FE007E0038001E00FC003E00380018007C003E00380010003
      8001E003800100018001E003800100008001E003800100018001E00380018001
      C003E003C3C3C001C003E003C3C3E000E007E003C003F000F00FFFFFC003F803
      FC3FFFFFE007FC0FFFFFFFFFF00FFE3FFFFFFFFFF800C001FFFFFFFF8000C001
      E3E300078000C001C1C100038000C001C00100038000C001E00300018000C001
      F00700018000C001F80F00008000C001F80F00008000C003F00700008000C003
      E00300078000C003C00100078001C003C1C100F88003C003E3E301FC8007F81F
      FFFFFF9A803FFC3FFFFFFFC7FFFFFFFFFFFFFFFFFFFF8000FFFFFC3FFE7F8000
      FC3FF81FFC3F8000FC3FF81FF81F8000FC3FF81FF00F8000FC3FF81FE0078000
      C003F81FC0038000C003F81FC0038000C003C003F81F8000C003C003F81F8000
      FC3FE007F81F8000FC3FF00FF81F8000FC3FF81FF81F8000FC3FFC3FF81F8000
      FFFFFE7FFC3F8000FFFFFFFFFFFFFFFF00000000000000000000000000000000
      000000000000}
  end
  object MainMenu1: TMainMenu
    Images = ActionImageList
    Left = 40
    Top = 88
    object File1: TMenuItem
      Caption = '&File'
      object Newproject1: TMenuItem
        Action = NewProjectAction
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
    BlendColor = 16185075
    BkColor = 16185075
    DrawingStyle = dsTransparent
    Masked = False
    Left = 208
    Top = 152
    Bitmap = {
      494C01010B000E002C0110001000F3F6F600FF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000003000000001002000000000000030
      000000000000000000000000000000000000F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300A3A3A3009F9F9F009C9C
      9C009999990095959500929292008F8F8F008C8C8C0089898900878787008484
      840082828200808080007E7E7E00F6F6F3000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000F6F6F300F6F6F300F6F6F300F6F6
      F300D5E0D50077AD66004188260041802600418026004188260077AD6600D5E0
      D500F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300A7A7A700EAEAEA00EAEA
      EA00EDEDED00F2F2F200F5F5F500F7F7F700F5F5F500F0F0F000E9E9E900E4E4
      E400E2E2E200E2E2E20080808000F6F6F3000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000F6F6F300F6F6F300F6F6F30077AD
      660041882D00579A48005EA457006FAD66006FAD66005EA45700579A48004188
      2D0077AD6600F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000C0C0C000C0C0C000F6F6F300F6F6F300F6F6F300ABABAB00EBEBEB00CCCC
      CC00B5B5B500BDBDBD00C3C3C300C6C6C600C5C5C500BABABA00ABABAB00A2A2
      A200B9B9B900E3E3E30083838300F6F6F3000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000F6F6F300F6F6F30077AD66004891
      3A005EA45700ADCBAD00ADCBAD003A8833003A883A00489A410057A457005EA4
      570048913A0077AD6600F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300C0C0
      C000D8D8E800D8D8E800D8D8E800D8D8E800D8D8E800D8D8E800D8D8E800D8D8
      E800D8D8E800C0C0C000F6F6F300F6F6F300F6F6F300AFAFAF00ECECEC00D0D0
      D000D8D8D800E1E1E100E7E7E700EBEBEB00E8E8E800DDDDDD00CCCCCC00BEBE
      BE00BBBBBB00E4E4E40087878700F6F6F3000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000F6F6F300D5E0D50041882D00579A
      4F0041914100ADCBAD00FFFFFF00ADCBAD0041883A0041883A00419141004F9A
      48005EA4570041882D00D5E0D500F6F6F300F6F6F300F6F6F300F6F6F300C0C0
      C000D0D0D800B0B0C000B0B0C000B0B0C000B0B0C000B0B0C000B0B0C000B0B0
      C000D0D0D800C0C0C000F6F6F300F6F6F300F6F6F300B3B3B300EDEDED00D3D3
      D300BABABA00C0C0C000E7E7E700A0A0A00088888800DFDFDF00B0B0B000A6A6
      A600BEBEBE00E5E5E5008A8A8A00F6F6F3000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000F6F6F30077AD66004F9141004F91
      48004F914800ADCBAD00FFFFFF00FFFFFF00ADCBAD004F9148004F9148004F9A
      4800579A4F005791410077AD6600F6F6F300F6F6F300F6F6F300F6F6F300C0C0
      C000C8C8D800B0A8C000B0A8C000B0A8C000B0A8C000B0A8C000B0A8C000B0A8
      C000C8C8D800C0C0C000F6F6F300F6F6F300F6F6F300B7B7B700EEEEEE00D5D5
      D500DEDEDE00E4E4E400E6E6E600A5A5A50090909000E0E0E000D0D0D000C4C4
      C400C1C1C100E6E6E6008E8E8E00F6F6F3000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000F6F6F3004180260033802D003380
      2D0033802D00ADCBAD00FFFFFF00FFFFFF00FFFFFF00ADCBAD0033802D003380
      2D00488841005EA4570041802600F6F6F300F6F6F300F6F6F300F6F6F300C0C0
      C000C0C0D000A8A8C000A8A8C000A8A8C000A8A8C000A8A8C000A8A8C000A8A8
      C000C0C0D000C0C0C000F6F6F300F6F6F300F6F6F300BABABA00EFEFEF00D8D8
      D800BEBEBE00C1C1C100E5E5E500ABABAB00ACACAC00E1E1E100B3B3B300AAAA
      AA00C3C3C300E7E7E70092929200F6F6F3000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000F6F6F300418020003A7733003A77
      33003A773300ADCBAD00FFFFFF00FFFFFF00FFFFFF00FFFFFF00BAD2BA003A77
      33003A7733006F9A5E0041802000F6F6F300F6F6F300F6F6F300F6F6F300C0C0
      C000C8C8D800B0A8C000B0A8C000B0A8C000B0A8C000B0A8C000B0A8C000B0A8
      C000C8C8D800C0C0C000F6F6F300F6F6F300F6F6F300BDBDBD00F0F0F000DADA
      DA00E2E2E200E4E4E400E4E4E400EBEBEB00EAEAEA00E2E2E200D4D4D400CACA
      CA00C6C6C600E8E8E80096969600F6F6F3000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000F6F6F30048883300488048004880
      480048804800ADCBAD00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0078E9
      C2004880480077AD770041802600F6F6F300F6F6F300F6F6F300F6F6F300C0C0
      C000D0D0D800B0B0C800B0B0C800B0B0C800B0B0C800B0B0C800B0B0C800B0B0
      C800D0D0D800C0C0C000F6F6F300F6F6F300F6F6F300C0C0C000F0F0F000F1F1
      F100F4F4F400F5F5F500F4F4F400F7F7F700F7F7F700F4F4F400F0F0F000EBEB
      EB00EAEAEA00EAEAEA009B9B9B00F6F6F3000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000F6F6F30048883300579A5E00579A
      5E00579A5E00BAD2BA00FFFFFF00FFFFFF00FFFFFF00FFFFFF0026CC8000579A
      5E00579A5E0077AD770048883300F6F6F300F6F6F300F6F6F300F6F6F300C0C0
      C000D0D0D800B8B0C800B8B0C800B8B0C800B8B0C800B8B0C800B8B0C800B8B0
      C800D0D0D800C0C0C000F6F6F300F6F6F300F6F6F300C0C0C0009F9F9F009191
      91008E8E8E00B8B8B800B8B8B800B5B5B500B2B2B200AFAFAF00AAAAAA007878
      7800767676007F7F7F009F9F9F00F6F6F3000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000F6F6F30077AD6600579A4F005EA4
      6F005EA46F00BAD2BA00FFFFFF00FFFFFF00FFFFFF0026CC80005EA46F005EA4
      6F005EA46F005EA46F0077AD6600F6F6F300F6F6F300F6F6F300F6F6F300C0C0
      C000D8D8E800B8B8C800B8B8C800B8B8C800B8B8C800B8B8C800B8B8C800B8B8
      C800D8D8E800C0C0C000F6F6F300F6F6F300F6F6F300F6F6F30099999900C6C6
      C600949494008F8F8F00F6F6F300F6F6F300F6F6F300F6F6F3007E7E7E007D7D
      7D00ABABAB0076767600F6F6F300F6F6F3000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000F6F6F300D5E0D500488833006FAD
      80006FB78800BAD2BA00FFFFFF00FFFFFF0026CC80006FB788006FB788006FB7
      88006FB7880048883300D5E0D500F6F6F300F6F6F300F6F6F300F6F6F300C0C0
      C000D8D8E800D8D8E800D8D8E800D8D8E800D8D8E800D8D8E800D8D8E800D8D8
      E800D8D8E800C0C0C000F6F6F300F6F6F300F6F6F300F6F6F3009D9D9D00C4C4
      C400A1A1A10093939300F6F6F300F6F6F300F6F6F300F6F6F300828282008989
      8900A9A9A90079797900F6F6F300F6F6F3000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000F6F6F300F6F6F30077AD66005791
      480077B7880026CC8000FFFFFF0026CC80006FB791006FB7910077B7910077B7
      91005791480077AD6600F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300C0C0
      C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0C000C0C0
      C000C0C0C000C0C0C000F6F6F300F6F6F300F6F6F300F6F6F300A1A1A100BABA
      BA00BFBFBF009898980094949400919191008E8E8E008A8A8A0087878700A8A8
      A8009E9E9E007D7D7D00F6F6F300F6F6F3000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000F6F6F300F6F6F300F6F6F30077AD
      66004888330026CC800026CC800088C19A0088C1A40080B7880066A466004F88
      3A0077AD6600F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300A4A4A400A1A1
      A100C4C4C400BEBEBE00A1A1A100969696009393930097979700AEAEAE00AEAE
      AE008484840081818100F6F6F300F6F6F3000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000F6F6F300F6F6F300F6F6F300F6F6
      F300D5E0D50077AD66004F883A0041802600418026004F883A0077AD6600D5E0
      D500F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300A4A4
      A400A2A2A200BCBCBC00CACACA00CCCCCC00CACACA00C2C2C200ADADAD008C8C
      8C0089898900F6F6F300F6F6F300F6F6F3000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6
      F300A5A5A500A3A3A300A0A0A0009D9D9D009A9A9A0097979700949494009090
      9000F6F6F300F6F6F300F6F6F300F6F6F3000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F3008B8B8B008B8B8B008B8B8B008B8B8B008B8B8B008B8B8B008B8B
      8B008B8B8B008B8B8B008B8B8B008B8B8B00F6F6F300F6F6F300A3A3A3008B8B
      8B00898989008787870085858500898989008989890089898900898989008989
      8900898989008989890089898900F6F6F300F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F3008B8B8B008B8B8B008B8B
      8B008B8B8B008C8C8C00B5B5B500B5B5B500B5B5B500B5B5B500B5B5B500B5B5
      B500B5B5B500B5B5B500B5B5B5008C8C8C00F6F6F300F6F6F3008E8E8E009A9A
      9A00A5A5A500A6A6A600A6A6A6008B8B8B00B5B5B500B6B6B600B6B6B600B6B6
      B600B6B6B600B5B5B5008B8B8B00F6F6F300F6F6F300F6F6F300F6F6F300CDCD
      CD004A4A4A0087878700F6F6F300F6F6F300F6F6F300F6F6F300F6F6F3008787
      87004A4A4A00CDCDCD00F6F6F300F6F6F3008787870081818100777777007171
      7100626262005757570049494900393939002E2E2E0024242400191919001010
      100014141400F6F6F300F6F6F300F6F6F300F6F6F3008C8C8C00B5B5B500B5B5
      B500B5B5B5008D8D8D00B6B6B600919191008D8D8D0088888800828282007D7D
      7D00B6B6B600B5B5B500B5B5B5008D8D8D00F6F6F300F6F6F3008F8F8F00A7A7
      A700A7A7A700A7A7A700A7A7A7008D8D8D00B6B6B600B7B7B700B7B7B700B7B7
      B700B6B6B600B7B7B7008C8C8C00F6F6F300F6F6F300F6F6F300CDCDCD004A4A
      4A00626262004A4A4A0087878700F6F6F300F6F6F300F6F6F300878787004A4A
      4A00626262004A4A4A00CDCDCD00F6F6F30087878700A2A2A200B2B2B2009292
      920092929200929292008D8D8D008B8B8B008181810081818100797979007070
      70004A4A4A0093939300F6F6F300F6F6F300F6F6F3008D8D8D00B6B6B6009191
      91008D8D8D008E8E8E00B6B6B600B6B6B600B6B6B600B6B6B600B6B6B600B6B6
      B600B6B6B600B6B6B600B6B6B6008E8E8E00F6F6F300F6F6F30091919100A9A9
      A900A9A9A900A9A9A900A9A9A9008F8F8F00B8B8B800919191008A8A8A008080
      800079797900B8B8B8008E8E8E00F6F6F300F6F6F300F6F6F300B1B1B1004A4A
      4A0076767600757575004A4A4A0087878700B1B1B100878787004A4A4A007676
      7600757575004A4A4A00B1B1B100F6F6F30089898900BDBDBD00A7A7A700DEDE
      DE00D1D1D100CACACA00CACACA00BFBFBF00B4B4B400AFAFAF00A4A4A4009494
      94006666660059595900F6F6F300F6F6F300F6F6F3008E8E8E00B6B6B600B6B6
      B600B6B6B6008F8F8F00B8B8B800919191008F8F8F008B8B8B00878787008383
      83007F7F7F007C7C7C00B8B8B8008F8F8F00F6F6F300F6F6F30092929200ABAB
      AB00AAAAAA00AAAAAA00AAAAAA0091919100BABABA00BABABA00BABABA00BABA
      BA00BABABA00BABABA0091919100F6F6F300F6F6F300F6F6F300F6F6F3008787
      87004E4E4E0076767600767676005E5E5E004D4D4D005E5E5E00767676007575
      75004E4E4E0087878700F6F6F300F6F6F30092929200D0D0D000A7A7A700BFBF
      BF00D7D7D700D1D1D100D1D1D100CACACA00C6C6C600BBBBBB00B4B4B400AFAF
      AF008F8F8F004A4A4A00B7B7B700F6F6F300F6F6F3008F8F8F00B8B8B8009191
      91008F8F8F0091919100B8B8B800B9B9B900B9B9B900B9B9B900B9B9B900B9B9
      B900B9B9B900B8B8B800B9B9B90091919100F6F6F300F6F6F30094949400ADAD
      AD00ADADAD00ADADAD00ADADAD0093939300BBBBBB00919191008A8A8A008080
      800079797900BBBBBB0093939300F6F6F300F6F6F300F6F6F300F6F6F300F6F6
      F300878787005353530076767600767676007676760076767600767676005252
      520087878700F6F6F300F6F6F300F6F6F30092929200D5D5D500C6C6C600A7A7
      A700DEDEDE00D3D3D300D5D5D500D1D1D100D1D1D100CACACA00BFBFBF00BBBB
      BB00B4B4B4006666660083838300F6F6F300F6F6F30091919100B8B8B800B9B9
      B900B9B9B90092929200BABABA00919191008F8F8F008B8B8B00878787008383
      83007F7F7F007C7C7C00BABABA0093939300F6F6F300F6F6F30095959500AFAF
      AF00AFAFAF00AFAFAF00AFAFAF0095959500BDBDBD00BDBDBD00BDBDBD00BDBD
      BD00BDBDBD00BDBDBD0095959500F6F6F300F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F30059595900676767007C7C7C007C7C7C007C7C7C00676767005959
      5900F6F6F300F6F6F300F6F6F300F6F6F30097979700DBDBDB00E0E0E000A7A7
      A700B5B5B500D7D7D700D5D5D500D5D5D500D5D5D500D1D1D100CFCFCF00CACA
      CA00BBBBBB00A6A6A60067676700C1C1C100F6F6F30093939300BABABA009191
      91008F8F8F0094949400BBBBBB00BBBBBB00BBBBBB00BBBBBB00BBBBBB00BBBB
      BB00BBBBBB00BBBBBB00BBBBBB0094949400F6F6F300F6F6F30097979700B1B1
      B100B1B1B100B1B1B100B1B1B10097979700BEBEBE009191910079797900BEBE
      BE00A5A5A500A0A0A00097979700F6F6F300F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F3005F5F5F006B6B6B008585850085858500848484006B6B6B005F5F
      5F00F6F6F300F6F6F300F6F6F300F6F6F30097979700DBDBDB00E6E6E600C6C6
      C600A7A7A700DEDEDE00DEDEDE00DEDEDE00DEDEDE00DEDEDE00DEDEDE00DEDE
      DE00D7D7D700D7D7D700A9A9A90090909000F6F6F30094949400BBBBBB00BBBB
      BB00BBBBBB0095959500BDBDBD00919191008F8F8F008B8B8B00878787008383
      83007F7F7F007C7C7C00BCBCBC0095959500F6F6F300F6F6F30098989800B3B3
      B300B3B3B300B3B3B300B3B3B30099999900BFBFBF00BFBFBF00BFBFBF00BFBF
      BF00A0A0A00097979700F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6
      F30087878700656565008F8F8F008E8E8E008E8E8E008F8F8F008E8E8E006565
      650087878700F6F6F300F6F6F300F6F6F3009E9E9E00E2E2E200E6E6E600E6E6
      E600B1B1B100B1B1B100A7A7A700A7A7A700A1A1A100A1A1A100979797009797
      970092929200929292008989890087878700F6F6F30096969600BDBDBD009191
      91008F8F8F0097979700BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBD
      BD00BDBDBD00BDBDBD00BDBDBD0097979700F6F6F300F6F6F3009A9A9A00B5B5
      B500B4B4B400B5B5B500B5B5B500999999009999990099999900999999009999
      99009999990090909000F6F6F300F6F6F300F6F6F300F6F6F300F6F6F3009D9D
      9D006C6C6C009999990099999900868686006C6C6C0086868600989898009898
      98006C6C6C009D9D9D00F6F6F300F6F6F300A2A2A200E2E2E200EBEBEB00E6E6
      E600E6E6E600D9D9D900D5D5D500D5D5D500D5D5D500D5D5D500D5D5D500D5D5
      D50087878700F6F6F300F6F6F300F6F6F300F6F6F30096969600BDBDBD00BDBD
      BD00BDBDBD0098989800BEBEBE00919191008A8A8A008080800079797900BEBE
      BE00A5A5A500A0A0A0009A9A9A0097979700F6F6F300F6F6F3009B9B9B00B6B6
      B600B6B6B600B6B6B600B6B6B600B6B6B600B6B6B600B6B6B600B6B6B600B6B6
      B600B6B6B60096969600F6F6F300F6F6F300F6F6F300F6F6F300B5B5B5007272
      7200A2A2A200A3A3A3007272720088888800B5B5B5008888880072727200A2A2
      A200A3A3A30072727200B5B5B500F6F6F300A2A2A200E6E6E600E8E8E800EBEB
      EB00E6E6E600E6E6E600D7D7D7009E9E9E009797970097979700929292008E8E
      8E0087878700F6F6F300F6F6F300F6F6F300F6F6F30098989800BEBEBE009191
      91008A8A8A0099999900BFBFBF00BFBFBF00BFBFBF00BFBFBF00BFBFBF00BFBF
      BF00A0A0A0009B9B9B0099999900F6F6F300F6F6F300F6F6F3009B9B9B00B6B6
      B600787878008888880088888800888888008888880088888800888888007878
      7800B6B6B60098989800F6F6F300F6F6F300F6F6F300F6F6F300CDCDCD007878
      78007D7D7D00787878009D9D9D00F6F6F300F6F6F300F6F6F3009D9D9D007777
      77007D7D7D0077777700CDCDCD00F6F6F300A7A7A700CFCFCF00E8E8E800E8E8
      E800EBEBEB00E6E6E600A2A2A200C2C2C200F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300656565006565650065656500F6F6F30098989800BFBFBF00BFBF
      BF00BFBFBF009A9A9A00BFBFBF00BFBFBF00BFBFBF00BFBFBF00BFBFBF00BFBF
      BF009A9A9A009A9A9A00F6F6F300F6F6F300F6F6F300F6F6F300AEAEAE009090
      9000909090007F7F7F00939393009393930093939300939393007F7F7F008383
      830090909000AEAEAE00F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300CDCD
      CD007B7B7B009D9D9D00F6F6F300F6F6F300F6F6F300F6F6F300F6F6F3009D9D
      9D007B7B7B00CDCDCD00F6F6F300F6F6F300CECECE00A7A7A700A7A7A700A7A7
      A700A7A7A700A7A7A700C0C0C000F6F6F300F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300F6F6F3006565650065656500F6F6F3009A9A9A00BFBFBF00BFBF
      BF00BFBFBF009A9A9A009A9A9A009A9A9A009A9A9A009A9A9A009A9A9A009A9A
      9A009A9A9A00F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300878787009D9D9D009D9D9D009D9D9D009D9D9D0087878700F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F30073737300B3B3B300F6F6
      F300F6F6F3008D8D8D00F6F6F30073737300F6F6F3009A9A9A009A9A9A009A9A
      9A009A9A9A009A9A9A009A9A9A009A9A9A009A9A9A009A9A9A00F6F6F300F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300F6F6F3008C8C8C009F9F9F009F9F9F008C8C8C00F6F6F300F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F3008D8D8D008888
      880093939300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300C1C1C200C1C1C200C1C1
      C200C1C1C200C1C1C200C1C1C200C1C1C200C1C1C200C1C1C200C1C1C200C1C1
      C200C1C1C200C1C1C200C1C1C200C1C1C200F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300F6F6F300C1C1C200C1C1C200C1C1C200C1C1C200F6F6F300F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300F6F6F300F6F6F300C1C1C200C1C1C200F6F6F300F6F6F300F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300C1C1C200E3E0DE00E3E0
      DE00E3E0DE00BAB5B200D5DBDE00D5DBDE00D5DBDE00AFABA800D5DBDE00BAB5
      B200E3E0DE00E3E0DE00E3E0DE00C1C1C200F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300F6F6F300C1C1C200C1C1C200C1C1C200C1C1C200F6F6F300F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300C1C1C200D1DCE000D1DCE000D1DCE000D1DCE000C1C1C200F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300F6F6F300C1C1C200D1DCE000D1DCE000C1C1C200F6F6F300F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300C1C1C200E5E2E000E5E2
      E000E5E2E000BAB5B200E8E4E100E8E4E100E8E4E100CDCECC00CDCECC00BAB5
      B200E5E2E000E5E2E000E5E2E000C1C1C200F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300F6F6F300C1C1C200AFBBB600AFBBB600C1C1C200F6F6F300F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300C1C1C200D1DCE000D1DCE000D1DCE000D1DCE000C1C1C200F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300C1C1C200D1DCE000D1DCE000D1DCE000D1DCE000C1C1C200F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300C1C1C200E6E3E100E6E3
      E100E6E4E200BAB5B200D8D3D000D8D3D000D8D3D000BFBFBD00BFBFBD00BAB5
      B200E6E4E200E6E3E100E6E3E100C1C1C200F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300F6F6F300C1C1C200AFBBB600AFBBB600C1C1C200F6F6F300F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300C1C1C200D1DCE000D1DCE000D1DCE000D1DCE000C1C1C200F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6
      F300C1C1C200D4DEE200D4DEE200D4DEE200D4DEE200D4DEE200D4DEE200C1C1
      C200F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300C1C1C200E8E6E400E7E5
      E300E8E6E400BAB5B200FCFCFC00FCFCFC00FCFCFC00FCFCFC00D5DBDE00BAB5
      B200E8E6E400E8E6E400E8E6E400C1C1C200F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300F6F6F300C1C1C200B1BDB900B1BDB900C1C1C200F6F6F300F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300C1C1C200D1DCE000D1DCE000D1DCE000D1DCE000C1C1C200F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300C1C1
      C200D7E1E400D7E1E400D7E1E400D7E1E400D7E1E400D7E1E400D7E1E400D7E1
      E400C1C1C200F6F6F300F6F6F300F6F6F300F6F6F300C1C1C200EAE8E600EAE7
      E600EAE8E600EAE7E600BAB5B200BAB5B200BAB5B200BAB5B200BAB5B200EAE8
      E600E9E7E500EAE8E600EAE8E600C1C1C200F6F6F300F6F6F300C1C1C200C1C1
      C200C1C1C200C1C1C200B3C0BC00B3C0BC00B3C0BC00B3C0BC00C1C1C200C1C1
      C200C1C1C200C1C1C200F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300C1C1C200D1DCE000D1DCE000D1DCE000D1DCE000C1C1C200F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300C1C1C200DBE3
      E600DBE3E600DBE3E600DBE3E600DBE3E600DBE3E600DBE3E600DBE3E600DBE3
      E600DBE3E600C1C1C200F6F6F300F6F6F300F6F6F300C1C1C200EBE9E800ECEA
      E900ECEAE900EBE9E800EBE9E800EBE9E800ECEAE900ECEAE900ECEAE900ECEA
      E900ECEAE900ECEAE900EBE9E800C1C1C200F6F6F300F6F6F300C1C1C200B6C3
      C000B6C3C000B6C3C000B6C3C000B6C3C000B6C3C000B6C3C000B6C3C000B6C3
      C000B6C3C000C1C1C200F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300C1C1C200D5DFE200D5DFE200D5DFE200D5DFE200C1C1C200F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300C1C1C200C1C1
      C200C1C1C200C1C1C200E0E8EA00E0E8EA00E0E8EA00E0E8EA00C1C1C200C1C1
      C200C1C1C200C1C1C200F6F6F300F6F6F300F6F6F300C1C1C200EEECEB00EEEC
      EB00DFDCDA00C0C0BE00C0C0BE00C0C0BE00C0C0BE00C0C0BE00C0C0BE00C0C0
      BE00DFDCDA00EEECEB00EEECEB00C1C1C200F6F6F300F6F6F300C1C1C200BCCA
      C800BCCAC800BCCAC800BCCAC800BCCAC800BCCAC800BCCAC800BCCAC800BCCA
      C800BCCAC800C1C1C200F6F6F300F6F6F300F6F6F300F6F6F300C1C1C200C1C1
      C200C1C1C200C1C1C200DAE3E600DAE3E600DAE3E600DAE3E600C1C1C200C1C1
      C200C1C1C200C1C1C200F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300C1C1C200E5EBED00E5EBED00E5EBED00E5EBED00C1C1C200F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300C1C1C200F0EFEE00F0EF
      EE00C0C0BE00D5DBDE00D5DBDE00D5DBDE00D5DBDE00D5DBDE00D5DBDE00D5DB
      DE00C0C0BE00F0EFEE00F0EFEE00C1C1C200F6F6F300F6F6F300C1C1C200C1C1
      C200C1C1C200C1C1C200BDCCCB00BDCCCB00BDCCCB00BDCCCB00C1C1C200C1C1
      C200C1C1C200C1C1C200F6F6F300F6F6F300F6F6F300F6F6F300C1C1C200E2E9
      EB00E2E9EB00E1E8EA00E2E9EB00E2E9EB00E2E9EB00E2E9EB00E1E8EB00E2E9
      EB00E1E8EB00C1C1C200F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300C1C1C200EAEFF000EAEFF100EAEFF100EAEFF100C1C1C200F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300C1C1C200F3F2F100F2F1
      F000C0C0BE00F1F0EE00F1F0EE00F1F0EE00F2F1EF00F1F0EE00F2F1EF00F1F0
      EE00C0C0BE00F2F1F000F2F1F000C1C1C200F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300F6F6F300C1C1C200C0CFCE00C1D0CF00C1C1C200F6F6F300F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300C1C1
      C200E9EEF000E9EEF000E9EEF000E8EEEF00E8EDEF00E8EDEF00E9EEF000E9EE
      F000C1C1C200F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300C1C1C200EFF3F400EFF3F400EFF3F400EFF3F400C1C1C200F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300C1C1C200F5F4F300F4F3
      F300C0C0BE00F5F4F200F5F4F200F5F4F200F5F4F200F5F4F300F5F4F200F5F4
      F200C0C0BE00F4F3F300F4F3F300C1C1C200F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300F6F6F300C1C1C200C0CFCE00C1D0CF00C1C1C200F6F6F300F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6
      F300C1C1C200EFF3F400EFF3F400EFF3F400EFF3F400EFF3F400EFF3F400C1C1
      C200F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300C1C1C200F3F6F700F3F6F700F2F5F600F3F6F700C1C1C200F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300C1C1C200F6F5F500F6F5
      F500C0C0BE00F8F8F700F8F8F700F8F8F700F8F8F700F8F7F600F8F8F700F8F8
      F700C0C0BE00F6F5F500F6F5F500C1C1C200F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300F6F6F300C1C1C200C1D0CF00C1D0CF00C1C1C200F6F6F300F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300C1C1C200F5F7F800F5F7F800F5F8F800F5F8F800C1C1C200F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300C1C1C200F6F9F900F6F9F900F6F9F900F7F9F900C1C1C200F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300C1C1C200F8F7F700F8F8
      F700C0C0BE00FAFAFA00FAFAFA00FAFAFA00FAFAFA00FAFAFA00FAFAFA00FAFA
      FA00C0C0BE00F8F7F700F8F8F700C1C1C200F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300F6F6F300C1C1C200C1C1C200C1C1C200C1C1C200F6F6F300F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300F6F6F300C1C1C200F8FAFA00F8FAFA00C1C1C200F6F6F300F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300C1C1C200F6F9F900F6F9F900F6F9F900F7F9F900C1C1C200F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300C1C1C200F9F9F900F9F9
      F900D9D9D700FDFDFD00FDFDFD00FDFDFD00FDFDFD00FDFDFD00FDFDFD00FDFD
      FD00D9D9D700F9F9F900F9F9F900C1C1C200F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300F6F6F300F6F6F300C1C1C200C1C1C200F6F6F300F6F6F300F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300F6F6F300C1C1C200C1C1C200C1C1C200C1C1C200F6F6F300F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300C1C1C200C1C1C200C1C1
      C200C1C1C200C1C1C200C1C1C200C1C1C200C1C1C200C1C1C200C1C1C200C1C1
      C200C1C1C200C1C1C200C1C1C200C1C1C200F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6F300F6F6
      F300F6F6F300F6F6F300F6F6F300F6F6F300424D3E000000000000003E000000
      2800000040000000300000000100010000000000800100000000000000000000
      000000000000000000000000FFFFFF00FFFFFFFF80010000F00FFFFF80010000
      E007E00380010000C003E003800100008001E003800100008001E00380010000
      8001E003800100008001E003800100008001E003800100008001E00380010000
      8001E003C3C300008001E003C3C30000C003E003C0030000E007FFFFC0030000
      F00FFFFFE0070000FFFFFFFFF00F0000FFFFFFFFF800C001FFFFFFFF8000C001
      E3E300078000C001C1C100038000C001C00100038000C001E00300018000C001
      F00700018000C001F80F00008000C001F80F00008000C003F00700008000C003
      E00300078000C003C00100078001C003C1C100F88003C003E3E301FC8007F81F
      FFFFFF9A803FFC3FFFFFFFC7FFFFFFFFFFFFFFFFFFFF8000FFFFFC3FFE7F8000
      FC3FF81FFC3F8000FC3FF81FF81F8000FC3FF81FF00F8000FC3FF81FE0078000
      C003F81FC0038000C003F81FC0038000C003C003F81F8000C003C003F81F8000
      FC3FE007F81F8000FC3FF00FF81F8000FC3FF81FF81F8000FC3FFC3FF81F8000
      FFFFFE7FFC3F8000FFFFFFFFFFFFFFFF00000000000000000000000000000000
      000000000000}
  end
  object LogPopupMenu: TPopupMenu
    Left = 40
    Top = 312
    object LogCopytoclipboardMenuItem: TMenuItem
      Caption = 'Copy to clipboard'
      OnClick = LogCopytoclipboardMenuItemClick
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
