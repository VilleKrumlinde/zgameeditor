inherited SoundEditFrame: TSoundEditFrame
  Width = 801
  Height = 597
  ParentShowHint = False
  ShowHint = True
  ExplicitWidth = 801
  ExplicitHeight = 597
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 801
    Height = 597
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Sound'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        793
        569)
      object Label9: TLabel
        Left = 216
        Top = 38
        Width = 39
        Height = 13
        Caption = 'Channel'
      end
      object Label8: TLabel
        Left = 103
        Top = 38
        Width = 56
        Height = 13
        Caption = 'Note length'
      end
      object Label7: TLabel
        Left = 8
        Top = 38
        Width = 35
        Height = 13
        Caption = 'Octave'
      end
      object Label6: TLabel
        Left = 8
        Top = 372
        Width = 24
        Height = 13
        Caption = 'LFOs'
      end
      object Label13: TLabel
        Left = 8
        Top = 5
        Width = 564
        Height = 20
        Caption = 
          'Point mouse here and use keys a,w,s,e,d,f,t,g,y,h,u,j to play no' +
          'tes of an octave'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object Label12: TLabel
        Left = 8
        Top = 452
        Width = 49
        Height = 13
        Caption = 'Envelopes'
      end
      object Label1: TLabel
        Left = 8
        Top = 240
        Width = 85
        Height = 13
        Caption = 'Modulation matrix'
      end
      object NoteNrLabel: TLabel
        Left = 624
        Top = 38
        Width = 57
        Height = 13
        AutoSize = False
        Caption = 'Note nr'
      end
      object Label16: TLabel
        Left = 320
        Top = 38
        Width = 34
        Height = 13
        Caption = 'Volume'
      end
      object Label17: TLabel
        Left = 480
        Top = 38
        Width = 18
        Height = 13
        Caption = 'Pan'
      end
      object Panel1: TPanel
        Left = 415
        Top = 63
        Width = 375
        Height = 169
        Anchors = [akLeft, akTop, akRight]
        BevelOuter = bvNone
        Constraints.MinWidth = 100
        FullRepaint = False
        ParentBackground = False
        TabOrder = 15
        object SoundGraphPaintBox: TPaintBox
          Left = 0
          Top = 0
          Width = 375
          Height = 169
          Align = alClient
          Color = clBtnFace
          ParentColor = False
          OnPaint = SoundGraphPaintBoxPaint
          ExplicitWidth = 105
          ExplicitHeight = 105
        end
      end
      object ModulationsParent: TScrollBox
        Left = 8
        Top = 256
        Width = 521
        Height = 113
        VertScrollBar.Visible = False
        BevelInner = bvNone
        BevelOuter = bvNone
        BorderStyle = bsNone
        TabOrder = 0
      end
      object OctaveEdit: TEdit
        Left = 48
        Top = 33
        Width = 35
        Height = 21
        TabOrder = 1
        Text = '5'
      end
      object NoteLengthEdit: TEdit
        Left = 167
        Top = 34
        Width = 33
        Height = 21
        TabOrder = 2
      end
      object LfosParent: TScrollBox
        Left = 8
        Top = 388
        Width = 521
        Height = 61
        VertScrollBar.Visible = False
        BevelInner = bvNone
        BevelOuter = bvNone
        BorderStyle = bsNone
        TabOrder = 3
      end
      object GroupBox4: TGroupBox
        Left = 8
        Top = 56
        Width = 209
        Height = 89
        Caption = 'Pattern'
        TabOrder = 4
        object Label5: TLabel
          Left = 8
          Top = 19
          Width = 47
          Height = 13
          Caption = 'Sequence'
        end
        object Label11: TLabel
          Left = 8
          Top = 43
          Width = 32
          Height = 13
          Caption = 'Tempo'
        end
        object NotesEdit: TComboBox
          Left = 64
          Top = 16
          Width = 134
          Height = 21
          ItemIndex = 0
          TabOrder = 0
          Text = 'k dfh fda afk hfd  fh k f a a   '
          OnChange = NotesEditChange
          OnKeyPress = NotesEditKeyPress
          Items.Strings = (
            'k dfh fda afk hfd  fh k f a a   '
            'aasaadaafaagaahaajaak')
        end
        object AutoPlayCheckBox: TCheckBox
          Left = 8
          Top = 65
          Width = 97
          Height = 17
          Caption = 'Auto play'
          TabOrder = 1
          OnClick = AutoPlayCheckBoxClick
        end
        object TempoTrackBar: TTrackBar
          Left = 48
          Top = 42
          Width = 105
          Height = 25
          Position = 5
          TabOrder = 2
          TickStyle = tsNone
          OnChange = TempoTrackBarChange
        end
        object CreateMidiButton: TButton
          Left = 128
          Top = 64
          Width = 81
          Height = 22
          Caption = 'Copy to Music'
          TabOrder = 3
          OnClick = CreateMidiButtonClick
        end
      end
      object GroupBox3: TGroupBox
        Left = 224
        Top = 61
        Width = 185
        Height = 44
        Caption = 'Oscillator 1'
        TabOrder = 5
        object Osc1WaveformCombo: TComboBox
          Left = 7
          Top = 16
          Width = 82
          Height = 21
          Style = csDropDownList
          TabOrder = 0
          Items.Strings = (
            'Square'
            'Saw'
            'Noise'
            'Sine')
        end
        object Osc1PWTrackBar: TTrackBar
          Left = 95
          Top = 17
          Width = 70
          Height = 24
          Hint = 'Square Pulse Width'
          Max = 100
          Min = -100
          TabOrder = 1
          TickStyle = tsNone
        end
      end
      object GroupBox2: TGroupBox
        Left = 8
        Top = 145
        Width = 209
        Height = 88
        Caption = 'Filter'
        TabOrder = 6
        object Label3: TLabel
          Left = 8
          Top = 40
          Width = 31
          Height = 13
          Caption = 'Cutoff'
        end
        object Label4: TLabel
          Left = 8
          Top = 64
          Width = 8
          Height = 13
          Caption = 'Q'
        end
        object FilterActiveCheckBox: TCheckBox
          Left = 8
          Top = 16
          Width = 57
          Height = 17
          Caption = 'Active'
          TabOrder = 0
        end
        object FilterCutoffTrackBar: TTrackBar
          Left = 48
          Top = 32
          Width = 150
          Height = 33
          Max = 100
          Position = 25
          TabOrder = 1
          TickStyle = tsNone
        end
        object FilterQTrackBar: TTrackBar
          Left = 48
          Top = 56
          Width = 150
          Height = 25
          Max = 100
          Position = 25
          TabOrder = 2
          TickStyle = tsNone
        end
      end
      object GroupBox1: TGroupBox
        Left = 223
        Top = 111
        Width = 185
        Height = 121
        Caption = 'Oscillator 2'
        TabOrder = 7
        object Label2: TLabel
          Left = 8
          Top = 40
          Width = 64
          Height = 13
          Caption = 'Note modifier'
        end
        object Label18: TLabel
          Left = 8
          Top = 95
          Width = 61
          Height = 13
          Caption = 'Osc2 volume'
        end
        object Osc2ActiveCheckBox: TCheckBox
          Left = 8
          Top = 16
          Width = 57
          Height = 17
          Caption = 'Active'
          TabOrder = 0
        end
        object Osc2FreqEdit: TEdit
          Left = 80
          Top = 35
          Width = 49
          Height = 21
          Hint = '10=one note, 120=one octave, 240 two octaves etc.'
          TabOrder = 1
        end
        object Osc2WaveformCombo: TComboBox
          Left = 8
          Top = 64
          Width = 121
          Height = 21
          Style = csDropDownList
          ItemIndex = 0
          TabOrder = 2
          Text = 'Square'
          Items.Strings = (
            'Square'
            'Saw')
        end
        object HardSyncCheckBox: TCheckBox
          Left = 64
          Top = 16
          Width = 81
          Height = 17
          Caption = 'Sync with 1'
          TabOrder = 3
          OnClick = HardSyncCheckBoxClick
        end
        object Osc2VolumeTrackBar: TTrackBar
          Left = 75
          Top = 91
          Width = 107
          Height = 24
          Max = 100
          Position = 25
          TabOrder = 4
          TickStyle = tsNone
        end
      end
      object EnvelopesParent: TScrollBox
        Left = 8
        Top = 468
        Width = 640
        Height = 69
        BevelInner = bvNone
        BevelOuter = bvNone
        BorderStyle = bsNone
        Ctl3D = True
        ParentCtl3D = False
        TabOrder = 8
      end
      object ChannelEdit: TEdit
        Left = 264
        Top = 33
        Width = 41
        Height = 21
        TabOrder = 9
        Text = '0'
      end
      object DumpButton: TButton
        Left = 544
        Top = 256
        Width = 104
        Height = 25
        Caption = 'Save as raw-file'
        TabOrder = 10
        OnClick = DumpButtonClick
      end
      object VolumeTrackBar: TTrackBar
        Left = 358
        Top = 32
        Width = 121
        Height = 29
        Hint = 'Volume'
        Max = 100
        TabOrder = 11
        TickStyle = tsNone
        OnChange = VolumeTrackBarChange
      end
      object PanTrackBar: TTrackBar
        Left = 502
        Top = 24
        Width = 121
        Height = 33
        Hint = 'Pan'
        Max = 100
        Frequency = 50
        TabOrder = 12
        TickMarks = tmBoth
        OnChange = PanTrackBarChange
      end
      object HelpButton: TButton
        Left = 544
        Top = 287
        Width = 75
        Height = 25
        Caption = 'Help'
        TabOrder = 13
        OnClick = HelpButtonClick
      end
      object UpDown1: TUpDown
        Left = 83
        Top = 33
        Width = 16
        Height = 21
        Associate = OctaveEdit
        Position = 5
        TabOrder = 14
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Global mixer'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      DesignSize = (
        793
        569)
      object Label10: TLabel
        Left = 8
        Top = 51
        Width = 44
        Height = 13
        Caption = 'Channels'
      end
      object Label14: TLabel
        Left = 8
        Top = 5
        Width = 624
        Height = 20
        Caption = 
          'Global mixer settings. To save settings: Add a AudioMixer-compon' +
          'ent to your project.'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object Label15: TLabel
        Left = 8
        Top = 343
        Width = 56
        Height = 13
        Caption = 'Global LFOs'
      end
      object Label19: TLabel
        Left = 8
        Top = 31
        Width = 70
        Height = 13
        Caption = 'Master volume'
      end
      object ChannelsParent: TScrollBox
        Left = 8
        Top = 67
        Width = 777
        Height = 265
        Anchors = [akLeft, akTop, akRight]
        BorderStyle = bsNone
        TabOrder = 0
      end
      object GlobalLfosParent: TScrollBox
        Left = 8
        Top = 359
        Width = 521
        Height = 61
        VertScrollBar.Visible = False
        BorderStyle = bsNone
        TabOrder = 1
      end
      object HelpMixerButton: TButton
        Left = 8
        Top = 447
        Width = 75
        Height = 25
        Caption = 'Help'
        TabOrder = 2
        OnClick = HelpMixerButtonClick
      end
      object MasterVolumeTrackBar: TTrackBar
        Left = 80
        Top = 27
        Width = 121
        Height = 29
        Hint = 'Master Volume'
        Max = 100
        TabOrder = 3
        TickStyle = tsNone
        OnChange = MasterVolumeTrackBarChange
      end
    end
  end
  object AutoPlayTimer: TTimer
    Enabled = False
    Interval = 250
    OnTimer = AutoPlayTimerTimer
    Left = 216
    Top = 1
  end
  object SoundGraphTimer: TTimer
    Interval = 100
    OnTimer = SoundGraphTimerTimer
    Left = 248
  end
end
