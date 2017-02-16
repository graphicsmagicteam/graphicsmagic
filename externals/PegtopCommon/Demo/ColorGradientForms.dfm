object ColorGradientForm: TColorGradientForm
  Left = 287
  Top = 257
  Width = 649
  Height = 470
  Caption = 'PegtopColorGradient Demo'
  Color = clBtnFace
  Constraints.MinHeight = 150
  Constraints.MinWidth = 200
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefault
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PaintBox1: TPaintBox
    Left = 185
    Top = 0
    Width = 456
    Height = 423
    Align = alClient
    OnPaint = PaintBox1Paint
  end
  object Panel1: TPegtopPanel
    Left = 0
    Top = 0
    Width = 185
    Height = 423
    Align = alLeft
    DoubleBuffered = False
    FullRepaint = False
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 8
      Width = 169
      Height = 13
      AutoSize = False
      Caption = 'Gradient definition'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Shell Dlg 2'
      Font.Style = [fsBold]
      ParentFont = False
      Transparent = True
    end
    object Label2: TLabel
      Left = 8
      Top = 176
      Width = 169
      Height = 13
      AutoSize = False
      Caption = 'Drawing options'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Shell Dlg 2'
      Font.Style = [fsBold]
      ParentFont = False
      Transparent = True
    end
    object Label3: TLabel
      Left = 8
      Top = 120
      Width = 169
      Height = 49
      AutoSize = False
      Caption = 
        'To test dithering maximize the form and try similar colors (good' +
        ' display and eyes required).'
      Transparent = True
      WordWrap = True
    end
    object StyleComboBox: TComboBox
      Left = 8
      Top = 192
      Width = 169
      Height = 21
      Style = csDropDownList
      DropDownCount = 20
      ItemHeight = 13
      TabOrder = 3
      OnClick = PropertyChange
      Items.Strings = (
        'Linear'
        'Linear repetitive'
        'Radial'
        'Radial repetitive'
        'Polar'
        'Diamond'
        'Diamand repetitive'
        'Star'
        'Tapered'
        'Tapered repetitive'
        'Spherical'
        'Spiral clockwise'
        'Spiral anticlockwise'
        'Tunnel'
        'Helical clockwise'
        'Helical anticlockwise')
    end
    object IterationsTrackBar: TPegtopTrackBar
      Left = 8
      Top = 264
      Width = 169
      Height = 32
      Cursor = crDefault
      TabOrder = 7
      LabelCaption = 'Iterations: <pos>'
      LabelMode = plmPos
      LabelParam = 1.000000000000000000
      LabelOptions = [ploVisible]
      Min = 1
      Max = 10
      OnChange = PropertyChange
      Position = 1
    end
    object ReverseCheckBox: TCheckBox
      Left = 8
      Top = 216
      Width = 169
      Height = 17
      Caption = 'Reverse'
      TabOrder = 4
      OnClick = PropertyChange
    end
    object SymmetricalCheckBox: TCheckBox
      Left = 8
      Top = 232
      Width = 169
      Height = 17
      Caption = 'Symmetrical'
      TabOrder = 5
      OnClick = PropertyChange
    end
    object DitherCheckBox: TCheckBox
      Left = 8
      Top = 248
      Width = 169
      Height = 17
      Caption = 'Dithered'
      TabOrder = 6
      OnClick = PropertyChange
    end
    object Button1: TButton
      Left = 8
      Top = 56
      Width = 169
      Height = 25
      Caption = 'Random'
      TabOrder = 1
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 8
      Top = 88
      Width = 169
      Height = 25
      Caption = 'Use similar colors'
      TabOrder = 2
      OnClick = Button2Click
    end
    object BlendCheckBox: TCheckBox
      Left = 8
      Top = 312
      Width = 169
      Height = 17
      Caption = 'Blend second gradient'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Shell Dlg 2'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 8
      OnClick = PropertyChange
    end
    object PegtopColorGradientBox1: TPegtopColorGradientBox
      Left = 8
      Top = 24
      Width = 169
      Height = 25
      Gradient.Name = 'Demo gradient'
      Gradient.Seamless = True
      Gradient.Color.Noise.Frequency = 2
      Gradient.Color.KeyData = {
        4B45595348000000434B45591000000000000000000000000000000000000000
        434B455910000000D001000000000000000000001443F300434B455910000000
        E70300000000000000000000FFFFFF00}
      Gradient.Opacity.Noise.Frequency = 2
      Gradient.Opacity.KeyData = {
        4B455953300000004F4B45591000000000000000000000000000000000010000
        4F4B455910000000E7030000000000000000000000010000}
      GradientLibrary = pgtpclrgrdntlbry1
      GradientLibraryIndex = 0
      GradientOptions = []
      OnChange = PegtopColorGradientBox1Change
      Caption = 'Base'
      TabOrder = 0
      TabStop = True
    end
    object PegtopColorGradientBox2: TPegtopColorGradientBox
      Left = 8
      Top = 330
      Width = 169
      Height = 25
      Gradient.Name = 'Blended gradient'
      Gradient.Seamless = True
      Gradient.Color.Smoothness = 0
      Gradient.Color.Noise.Frequency = 2
      Gradient.Color.KeyData = {
        4B45595330000000434B455910000000FA000000000000000000000000000000
        434B455910000000EE0200000000000000000000FFFFFF00}
      Gradient.Opacity.Frequency = 10
      Gradient.Opacity.Noise.Frequency = 2
      Gradient.Opacity.KeyData = {
        4B455953300000004F4B455910000000FA000000000000000000000000010000
        4F4B455910000000EE020000000000000000000000000000}
      GradientLibraryIndex = 0
      GradientOptions = []
      OnChange = PegtopColorGradientBox1Change
      Caption = 'Blended'
      TabOrder = 9
      TabStop = True
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 423
    Width = 641
    Height = 19
    Panels = <>
    SimplePanel = True
    SimpleText = 'Resize window to set gradient vector.'
  end
  object pgtpclrgrdntlst1: TPegtopColorGradientList
    Items = <
      item
        Gradient.Name = 'Untitled'
        Gradient.Seamless = False
        Gradient.Color.Noise.Frequency = 2
        Gradient.Color.KeyData = {
          4B45595330000000434B45591000000000000000000000000000000000000000
          434B455910000000E80300000000000000000000FFFFFF00}
        Gradient.Opacity.Noise.Frequency = 2
        Gradient.Opacity.KeyData = {
          4B455953300000004F4B45591000000000000000000000000000000000010000
          4F4B455910000000E8030000000000000000000000010000}
      end>
    Left = 288
    Top = 24
  end
  object pgtpclrgrdntlbry1: TPegtopColorGradientLibrary
    Items = <
      item
        Items = <
          item
            Gradient.Name = 'Untitled'
            Gradient.Seamless = False
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595330000000434B45591000000000000000000000000000000000000000
              434B455910000000E80300000000000000000000FFFFFF00}
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E8030000000000000000000000010000}
          end>
      end
      item
        Name = 'Edited XFader gradients'
        Items = <
          item
            Gradient.Name = 'Clouds: Blue Streaks'
            Gradient.Seamless = False
            Gradient.Color.Smoothness = 0
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595378000000434B45591000000000000000000000000000000068687300
              434B45591000000095010000000000000000000080809000434B455910000000
              D90100000000000000000000000A6800434B4559100000002202000000000000
              0000000080809000434B455910000000E70300000000000000000000B0A7A700}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Clouds: Default'
            Gradient.Seamless = False
            Gradient.Color.Smoothness = 0
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595330000000434B45591000000000000000000000000000000000209000
              434B455910000000E70300000000000000000000FFFFFF00}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Clouds: Glass'
            Gradient.Seamless = False
            Gradient.Color.Smoothness = 0
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595330000000434B455910000000000000000000000000000000FFFFFF00
              434B455910000000E70300000000000000000000FFFFFF00}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953480000004F4B45591000000000000000000000000000000033000000
              4F4B455910000000F40100000000000000000000000000004F4B455910000000
              E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Clouds: Gray'
            Gradient.Seamless = False
            Gradient.Color.Smoothness = 128
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595360000000434B455910000000DC000000000000000000000000209000
              434B455910000000A60200000000000000000000FFFFFF00434B455910000000
              F90200000000000000000000EFEFEF00434B455910000000E703000000000000
              0000000081818100}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Clouds Lightning'
            Gradient.Seamless = False
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595348000000434B4559100000007701000000000000000000000000F000
              434B455910000000F40100000000000000000000FFFFFF00434B455910000000
              7102000000000000000000000000F000}
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953480000004F4B45591000000077010000000000000000000000000000
              4F4B455910000000F40100000000000000000000000100004F4B455910000000
              71020000000000000000000000000000}
          end
          item
            Gradient.Name = 'Clouds: Purple Haze'
            Gradient.Seamless = False
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595348000000434B455910000000000000000000000000000000B600C700
              434B455910000000F40100000000000000000000B600C700434B455910000000
              E70300000000000000000000FFFFFF00}
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000000000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Clouds: Red/Blue'
            Gradient.Seamless = False
            Gradient.Color.Smoothness = 128
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595378000000434B455910000000640000000000000000000000A9A9A900
              434B4559100000002C01000000000000000000006867A500434B455910000000
              F401000000000000000000008F4EA100434B455910000000BC02000000000000
              00000000AB717100434B455910000000840300000000000000000000A9A9A900}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Clouds: Smoke'
            Gradient.Seamless = False
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595330000000434B45591000000096010000000000000000000000000000
              434B4559100000005A0300000000000000000000FFFFFF00}
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953480000004F4B45591000000086010000000000000000000000000000
              4F4B455910000000E301000000000000000000008B0000004F4B455910000000
              2A03000000000000000000004C000000}
          end
          item
            Gradient.Name = 'Clouds: Two Areas'
            Gradient.Seamless = False
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595378000000434B45591000000000000000000000000000000080809000
              434B455910000000CF010000000000000000000068687300434B455910000000
              D9010000000000000000000018181800434B455910000000E301000000000000
              00000000B0A7A700434B455910000000E7030000000000000000000080809000}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Colors: Alien'
            Gradient.Seamless = False
            Gradient.Color.Smoothness = 128
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B455953C0000000434B45591000000000000000000000000000000008020000
              434B455910000000AD0000000000000000000000A53A0000434B455910000000
              A00100000000000000000000E1BC0000434B455910000000F101000000000000
              0000000039BA0C00434B4559100000002402000000000000000000002590F200
              434B455910000000800200000000000000000000555FFF00434B455910000000
              B503000000000000000000009AE4FF00434B455910000000E703000000000000
              00000000FAFEFF00}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Colors: Blue water'
            Gradient.Seamless = False
            Gradient.Color.Smoothness = 0
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595348000000434B45591000000000000000000000000000000020208000
              434B4559100000002C03000000000000000000003362FF00434B455910000000
              E7030000000000000000000000D4FF00}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Colors: Cream'
            Gradient.Seamless = False
            Gradient.Color.Smoothness = 128
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595348000000434B455910000000000000000000000000000000452E1C00
              434B4559100000005A0300000000000000000000E8C67600434B455910000000
              E70300000000000000000000FFFAD500}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Colors: Orient'
            Gradient.Seamless = False
            Gradient.Color.Smoothness = 0
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595330000000434B45591000000000000000000000000000000027005300
              434B455910000000E70300000000000000000000FFC37800}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Colors: Fire'
            Gradient.Seamless = False
            Gradient.Color.Smoothness = 0
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595378000000434B4559100000000000000000000000000000005E000000
              434B455910000000540100000000000000000000C0000000434B455910000000
              6B0200000000000000000000DD810000434B4559100000004B03000000000000
              00000000FFD40000434B455910000000E70300000000000000000000FFFECC00}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Colors: Forest'
            Gradient.Seamless = False
            Gradient.Color.Smoothness = 0
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595330000000434B455910000000000000000000000000000000322C0000
              434B455910000000E7030000000000000000000000C8A800}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Colors: Gold'
            Gradient.Seamless = False
            Gradient.Color.Smoothness = 0
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595348000000434B455910000000000000000000000000000000110E0000
              434B455910000000F40100000000000000000000AA840000434B455910000000
              E70300000000000000000000FFDF6F00}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Colors: Grayscale'
            Gradient.Seamless = False
            Gradient.Color.Smoothness = 0
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595330000000434B45591000000000000000000000000000000000000000
              434B455910000000E70300000000000000000000FFFFFF00}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Colors: Heat'
            Gradient.Seamless = False
            Gradient.Color.Smoothness = 128
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B455953A8000000434B45591000000005000000000000000000000000009900
              434B4559100000009300000000000000000000006100EE00434B455910000000
              E400000000000000000000009900EE00434B4559100000004501000000000000
              00000000AA008B00434B455910000000BF0100000000000000000000AA3B0000
              434B4559100000002C0300000000000000000000EEC30000434B455910000000
              E70300000000000000000000FFFFFF00}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Colors: Icy'
            Gradient.Seamless = False
            Gradient.Color.Smoothness = 0
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595330000000434B45591000000000000000000000000000000000487000
              434B455910000000E70300000000000000000000FFFFFF00}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Colors: Iron'
            Gradient.Seamless = False
            Gradient.Color.Smoothness = 0
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595330000000434B4559100000000000000000000000000000000C0C3200
              434B455910000000E70300000000000000000000FFFFFF00}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Colors: Florid'
            Gradient.Seamless = False
            Gradient.Color.Smoothness = 0
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595360000000434B4559100000007D0000000000000000000000E36D0000
              434B455910000000770100000000000000000000F9FF0900434B455910000000
              7102000000000000000000004BFF5200434B4559100000006B03000000000000
              000000000023A100}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Colors: Metal'
            Gradient.Seamless = True
            Gradient.Color.Frequency = 3
            Gradient.Color.Smoothness = 0
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595330000000434B45591000000000000000000000000000000000000000
              434B455910000000F40100000000000000000000FFFFFF00}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Colors: Navy/Spring'
            Gradient.Seamless = False
            Gradient.Color.Smoothness = 0
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595360000000434B45591000000000000000000000000000000000000000
              434B4559100000004D01000000000000000000000028C700434B455910000000
              9B020000000000000000000000FF6700434B455910000000E703000000000000
              00000000FFFFFF00}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Colors: Old Photo'
            Gradient.Seamless = False
            Gradient.Color.Smoothness = 0
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595330000000434B455910000000000000000000000000000000190C0000
              434B455910000000E70300000000000000000000FFF3E600}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Colors: Overexpose'
            Gradient.Seamless = False
            Gradient.Color.Smoothness = 0
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595330000000434B45591000000000000000000000000000000000000000
              434B455910000000910100000000000000000000FFFFFF00}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Colors: Pastels'
            Gradient.Seamless = False
            Gradient.Color.Smoothness = 0
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595330000000434B455910000000000000000000000000000000552ED400
              434B455910000000E70300000000000000000000FFBDB600}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Colors: Phosphorus'
            Gradient.Seamless = False
            Gradient.Color.Smoothness = 0
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595330000000434B45591000000000000000000000000000000000245800
              434B455910000000E70300000000000000000000F0FFC000}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Colors: Peru'
            Gradient.Seamless = False
            Gradient.Color.Smoothness = 0
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595348000000434B455910000000000000000000000000000000110E0000
              434B455910000000F40100000000000000000000A2684600434B455910000000
              E70300000000000000000000FFA56F00}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Colors: Plum'
            Gradient.Seamless = False
            Gradient.Color.Smoothness = 0
            Gradient.Color.Mode = pgmHSB
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595348000000434B45591000000000000000000000000000000000000000
              434B455910000000F401000000000000000000005800A300434B455910000000
              E70300000000000000000000FFFFFF00}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Colors: Radioactive'
            Gradient.Seamless = False
            Gradient.Color.Smoothness = 0
            Gradient.Color.Mode = pgmHSB
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595330000000434B45591000000000000000000000000000000000245800
              434B455910000000E70300000000000000000000F0FFC000}
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Colors: Rainbow'
            Gradient.Seamless = True
            Gradient.Color.Smoothness = 0
            Gradient.Color.Mode = pgmHSB
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595348000000434B455910000000A600000000000000000000004C4CFF00
              434B455910000000F40100000000000000000000F1000000434B455910000000
              41030000000000000000000000E50000}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Colors: Slime'
            Gradient.Seamless = False
            Gradient.Color.Smoothness = 128
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595330000000434B45591000000000000000000000000000000000424900
              434B455910000000E70300000000000000000000AAD12B00}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Colors: Sky/Lawn'
            Gradient.Seamless = False
            Gradient.Color.Smoothness = 0
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595360000000434B45591000000000000000000000000000000000000000
              434B4559100000004D01000000000000000000000098F000434B455910000000
              9B020000000000000000000096FF2F00434B455910000000E703000000000000
              00000000FFFFFF00}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Colors: Snow'
            Gradient.Seamless = False
            Gradient.Color.Smoothness = 0
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595330000000434B4559100000000000000000000000000000003030C000
              434B455910000000E70300000000000000000000FFFFFF00}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Colors: Space (1)'
            Gradient.Seamless = False
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595348000000434B45591000000000000000000000000000000008009900
              434B455910000000940200000000000000000000DDBC0000434B455910000000
              E7030000000000000000000022E5FF00}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Colors: Space (2)'
            Gradient.Seamless = False
            Gradient.Color.Smoothness = 172
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595348000000434B45591000000000000000000000000000000057008700
              434B455910000000940200000000000000000000DDBC0000434B455910000000
              E7030000000000000000000022E5FF00}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Colors: Steel'
            Gradient.Seamless = False
            Gradient.Color.Smoothness = 0
            Gradient.Color.Mode = pgmHSB
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595330000000434B4559100000000000000000000000000000000C0C3200
              434B455910000000E70300000000000000000000FFFFFF00}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Colors: Wood (dark)'
            Gradient.Seamless = False
            Gradient.Color.Smoothness = 102
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595348000000434B45591000000000000000000000000000000000000000
              434B45591000000059010000000000000000000040140000434B455910000000
              E703000000000000000000009F782800}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Colors: Wood (red)'
            Gradient.Seamless = False
            Gradient.Color.Smoothness = 0
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595378000000434B45591000000000000000000000000000000032080000
              434B455910000000A001000000000000000000006B400000434B455910000000
              EC0100000000000000000000844F0000434B455910000000EA02000000000000
              00000000A1600000434B455910000000E70300000000000000000000B2820000}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Colors: Wood (erratic)'
            Gradient.Seamless = False
            Gradient.Color.Smoothness = 192
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B455953A8000000434B45591000000000000000000000000000000032080000
              434B4559100000003601000000000000000000006B400000434B455910000000
              680100000000000000000000805D0000434B455910000000A001000000000000
              000000006B400000434B455910000000EC0100000000000000000000844F0000
              434B455910000000EA0200000000000000000000A1600000434B455910000000
              E70300000000000000000000B2820000}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Lightning'
            Gradient.Seamless = False
            Gradient.Color.Smoothness = 0
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B455953A8000000434B4559100000005E010000000000000000000000000000
              434B4559100000009001000000000000000000000014C500434B455910000000
              C20100000000000000000000257CFF00434B455910000000F401000000000000
              00000000FFFFFF00434B455910000000260200000000000000000000257CFF00
              434B4559100000005802000000000000000000000014C500434B455910000000
              8A020000000000000000000000000000}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Marble: Beach'
            Gradient.Seamless = True
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595360000000434B455910000000C30000000000000000000000E0D49E00
              434B4559100000000C01000000000000000000008D8D9800434B455910000000
              8B0100000000000000000000B0B0BB00434B455910000000F601000000000000
              00000000006BD000}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Marble: Black Glass'
            Gradient.Seamless = True
            Gradient.Color.Smoothness = 0
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595330000000434B455910000000FA000000000000000000000000000000
              434B455910000000EE0200000000000000000000FFFFFF00}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B455910000000FA000000000000000000000000010000
              4F4B455910000000EE020000000000000000000000000000}
          end
          item
            Gradient.Name = 'Marble: Blue'
            Gradient.Seamless = True
            Gradient.Color.Smoothness = 102
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595348000000434B455910000000CD00000000000000000000000049DA00
              434B4559100000008902000000000000000000007BB8FF00434B455910000000
              4703000000000000000000002555FF00}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Marble: Default'
            Gradient.Seamless = True
            Gradient.Color.Smoothness = 0
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595378000000434B4559100000002C00000000000000000000006D686600
              434B4559100000006B000000000000000000000068687300434B455910000000
              A8010000000000000000000080809000434B455910000000A102000000000000
              00000000B0A7A700434B4559100000002F030000000000000000000080809000}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Marble: Fire'
            Gradient.Seamless = True
            Gradient.Color.Smoothness = 0
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595378000000434B455910000000D7000000000000000000000000000000
              434B4559100000004C0100000000000000000000711B0000434B455910000000
              95010000000000000000000084470000434B455910000000DE01000000000000
              00000000711B0000434B45591000000062020000000000000000000000000000}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Marble: Flame'
            Gradient.Seamless = True
            Gradient.Color.Smoothness = 0
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595360000000434B4559100000004B0000000000000000000000DA000000
              434B4559100000009B0200000000000000000000E5350D00434B455910000000
              3C0300000000000000000000FF8F4B00434B455910000000A803000000000000
              00000000FFCA4200}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953780000004F4B4559100000004B000000000000000000000000000000
              4F4B455910000000B101000000000000000000007F0000004F4B455910000000
              9B0200000000000000000000C00000004F4B4559100000003C03000000000000
              00000000000100004F4B455910000000A8030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Marble: Lemon'
            Gradient.Seamless = True
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595330000000434B455910000000FA000000000000000000000091F0B100
              434B455910000000EE0200000000000000000000F3D35800}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Marble: Plum'
            Gradient.Seamless = True
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595360000000434B4559100000002C00000000000000000000006D686600
              434B4559100000009F0100000000000000000000A070B100434B455910000000
              050200000000000000000000B0A7A700434B4559100000009A03000000000000
              0000000080809000}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Marble: Noble'
            Gradient.Seamless = True
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595348000000434B4559100000004E000000000000000000000049449E00
              434B455910000000970200000000000000000000A8877900434B455910000000
              0C03000000000000000000007D989D00}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Marble: Orange/Gray'
            Gradient.Seamless = True
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595348000000434B45591000000000000000000000000000000080809000
              434B455910000000F40100000000000000000000D0C0C000434B455910000000
              2A0300000000000000000000D07D0000}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Marble: Orange'
            Gradient.Seamless = True
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595348000000434B455910000000740000000000000000000000D88C2800
              434B4559100000009D0300000000000000000000D07D0000434B455910000000
              E70300000000000000000000D0BCC000}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Marble: Pastels'
            Gradient.Seamless = True
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B455953D8000000434B455910000000C30000000000000000000000D0C0C000
              434B4559100000000C01000000000000000000008D8D9800434B455910000000
              8B0100000000000000000000B0B0BB00434B455910000000CB01000000000000
              00000000BCD2CA00434B455910000000F20100000000000000000000AFAFB500
              434B4559100000001D0200000000000000000000D0C0C000434B455910000000
              E5020000000000000000000080809000434B4559100000002A03000000000000
              00000000C1BCD200434B455910000000D403000000000000000000008D8D9800}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Marble: Pink'
            Gradient.Seamless = True
            Gradient.Color.Smoothness = 102
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595348000000434B45591000000000000000000000000000000080809000
              434B455910000000F40100000000000000000000D0C0C000434B455910000000
              2A0300000000000000000000D000BB00}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Marbe: Red Stone'
            Gradient.Seamless = True
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595330000000434B4559100000004E000000000000000000000055485A00
              434B4559100000009702000000000000000000008F7E7800}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Marble: Red Streaks'
            Gradient.Seamless = True
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B455953A8000000434B4559100000007A000000000000000000000068687300
              434B455910000000BE0000000000000000000000BD380000434B455910000000
              DC000000000000000000000068687300434B455910000000E801000000000000
              00000000B0A7A700434B455910000000E1020000000000000000000080809000
              434B4559100000000C0300000000000000000000D0890000434B455910000000
              6E030000000000000000000080809000}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Marble: Relief'
            Gradient.Seamless = True
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595348000000434B455910000000620200000000000000000000FFFFFF00
              434B455910000000F4020000000000000000000000000000434B455910000000
              8B0300000000000000000000FFFFFF00}
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953480000004F4B45591000000062020000000000000000000000000000
              4F4B455910000000F40200000000000000000000000100004F4B455910000000
              8B030000000000000000000000000000}
          end
          item
            Gradient.Name = 'Marble: Sand'
            Gradient.Seamless = True
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595330000000434B455910000000FA0000000000000000000000DA941C00
              434B455910000000EE0200000000000000000000FFD05E00}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Marble: Space'
            Gradient.Seamless = True
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595360000000434B455910000000580000000000000000000000C75F0000
              434B4559100000008B01000000000000000000000000F000434B455910000000
              E30100000000000000000000FFFFFF00434B4559100000005302000000000000
              000000001C92FF00}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Marble: Teal Streaks'
            Gradient.Seamless = True
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B455953A8000000434B4559100000007A000000000000000000000068687300
              434B455910000000BE0000000000000000000000009BBD00434B455910000000
              DC000000000000000000000068687300434B455910000000E801000000000000
              00000000B0A7A700434B455910000000E1020000000000000000000080809000
              434B4559100000000C030000000000000000000000D0B700434B455910000000
              6E030000000000000000000080809000}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Marble: Warm Colors'
            Gradient.Seamless = True
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595360000000434B4559100000007D0000000000000000000000A35D5D00
              434B455910000000770100000000000000000000A78B6100434B455910000000
              710200000000000000000000AA5C5C00434B4559100000006B03000000000000
              00000000995D8300}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Skin: Beige'
            Gradient.Seamless = False
            Gradient.Color.Smoothness = 128
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595348000000434B455910000000000000000000000000000000F4EEE300
              434B455910000000860100000000000000000000C9B9AA00434B455910000000
              E703000000000000000000009F653000}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Skin: Black & White'
            Gradient.Seamless = False
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595330000000434B455910000000C10100000000000000000000FFFFFF00
              434B455910000000D9010000000000000000000000000000}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Skin: Blue'
            Gradient.Seamless = False
            Gradient.Color.Smoothness = 0
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595330000000434B4559100000000000000000000000000000000300A100
              434B455910000000E703000000000000000000005598FF00}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Skin: Church'
            Gradient.Seamless = False
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595360000000434B455910000000E50000000000000000000000ED630000
              434B4559100000002A01000000000000000000005000A100434B455910000000
              8E02000000000000000000005000A100434B455910000000D702000000000000
              00000000DA00C300}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Skin: Default'
            Gradient.Seamless = False
            Gradient.Color.Smoothness = 0
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595330000000434B455910000000000000000000000000000000A7846A00
              434B455910000000E70300000000000000000000D7B59800}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Skin: Green & Black'
            Gradient.Seamless = False
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595330000000434B455910000000F4010000000000000000000000000000
              434B455910000000E7030000000000000000000000971700}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Skin: Kashmir'
            Gradient.Seamless = False
            Gradient.Color.Smoothness = 128
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595378000000434B45591000000000000000000000000000000055000000
              434B4559100000008B0100000000000000000000A8745D00434B455910000000
              F4010000000000000000000000000000434B4559100000005802000000000000
              00000000A8745D00434B455910000000E70300000000000000000000FFD98E00}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Skin: Map'
            Gradient.Seamless = False
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B455953F0000000434B4559100000000000000000000000000000001A1FBA00
              434B4559100000006F00000000000000000000004C43FC00434B455910000000
              DE00000000000000000000008087F600434B4559100000000701000000000000
              0000000048774900434B455910000000A801000000000000000000009CBA5800
              434B455910000000490200000000000000000000C7A65B00434B455910000000
              E10200000000000000000000A7745900434B4559100000004203000000000000
              00000000713B1100434B455910000000A403000000000000000000007A789000
              434B455910000000E70300000000000000000000F8F8FA00}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Skin: Marble'
            Gradient.Seamless = False
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595348000000434B45591000000000000000000000000000000068687300
              434B4559100000008D000000000000000000000080809000434B455910000000
              E70300000000000000000000B0A7A700}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Skin: Red & Black'
            Gradient.Seamless = False
            Gradient.Color.Smoothness = 0
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595348000000434B455910000000C1010000000000000000000000000000
              434B4559100000004E02000000000000000000008E000000434B455910000000
              CD0200000000000000000000FF7B7B00}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Skin: Redstreaks'
            Gradient.Seamless = False
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595330000000434B45591000000000000000000000000000000097000000
              434B45591000000062000000000000000000000042000000}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B45591000000062000000000000000000000000000000}
          end
          item
            Gradient.Name = 'Skin: Reptile'
            Gradient.Seamless = False
            Gradient.Color.Smoothness = 0
            Gradient.Color.Mode = pgmHSB
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595360000000434B45591000000000000000000000000000000003A10000
              434B4559100000007A00000000000000000000001D970000434B455910000000
              530200000000000000000000EEC30000434B455910000000E703000000000000
              00000000E37A0000}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Skin: Stone'
            Gradient.Seamless = False
            Gradient.Color.Smoothness = 0
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595348000000434B45591000000000000000000000000000000068687300
              434B4559100000006E0300000000000000000000A7A7A700434B455910000000
              E70300000000000000000000DDDDDD00}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Corroded Bronce'
            Gradient.Seamless = False
            Gradient.Color.Smoothness = 0
            Gradient.Color.Mode = pgmHSB
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595330000000434B45591000000000000000000000000000000000000000
              434B455910000000E70300000000000000000000A3CC8F00}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Wood: Default'
            Gradient.Seamless = True
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595308010000434B455910000000440000000000000000000000B66D4800
              434B45591000000092000000000000000000000070432C00434B455910000000
              C80000000000000000000000A0604000434B4559100000001101000000000000
              00000000B66D4800434B455910000000950100000000000000000000B66D4800
              434B455910000000F60100000000000000000000A0604000434B455910000000
              27020000000000000000000040200000434B4559100000007F02000000000000
              00000000A0604000434B455910000000470300000000000000000000B66D4800
              434B45591000000082030000000000000000000070432C00434B455910000000
              AA0300000000000000000000B66D4800}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Wood: Lava'
            Gradient.Seamless = True
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595348000000434B455910000000660000000000000000000000AA002800
              434B455910000000F60100000000000000000000E36F0000434B455910000000
              9F0300000000000000000000E3B40000}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Wood: Petrification'
            Gradient.Seamless = True
            Gradient.Color.Smoothness = 0
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595360000000434B4559100000003B000000000000000000000068687300
              434B45591000000089000000000000000000000080809000434B455910000000
              77010000000000000000000080809000434B4559100000000C03000000000000
              0000000065666D00}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Wood: Red/Transparent'
            Gradient.Seamless = True
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595390000000434B455910000000440000000000000000000000A1A1A100
              434B455910000000B0000000000000000000000032323200434B455910000000
              A001000000000000000000006B6B6B00434B455910000000EC01000000000000
              0000000084848400434B455910000000F40200000000000000000000B93C3C00
              434B455910000000E70300000000000000000000B2B2B200}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953900000004F4B455910000000440000000000000000000000B3000000
              4F4B455910000000B00000000000000000000000000000004F4B455910000000
              A00100000000000000000000A40000004F4B455910000000EC01000000000000
              00000000000100004F4B455910000000F4020000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Wood: Regular'
            Gradient.Seamless = True
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595390000000434B455910000000440000000000000000000000A1600000
              434B455910000000B0000000000000000000000032080000434B455910000000
              A001000000000000000000006B400000434B455910000000EC01000000000000
              00000000844F0000434B455910000000EA0200000000000000000000A1600000
              434B455910000000E70300000000000000000000B2820000}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Wood: Space (1)'
            Gradient.Seamless = True
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595348000000434B455910000000A7000000000000000000000002007B00
              434B455910000000AB0200000000000000000000FFC22500434B455910000000
              410300000000000000000000DDDDDD00}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Wood: Space (2)'
            Gradient.Seamless = True
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595348000000434B455910000000B0000000000000000000000057008700
              434B455910000000940200000000000000000000CA950B00434B455910000000
              E7030000000000000000000022C1FF00}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Wood: Water (1)'
            Gradient.Seamless = True
            Gradient.Color.Smoothness = 0
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595360000000434B455910000000E801000000000000000000000E38CB00
              434B455910000000400200000000000000000000276CE500434B455910000000
              750200000000000000000000276CE500434B455910000000CD02000000000000
              000000000E38CB00}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Wood: Water (2)'
            Gradient.Seamless = True
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B455953C0000000434B4559100000007000000000000000000000002555FF00
              434B455910000000A100000000000000000000007BB8FF00434B455910000000
              E0000000000000000000000070A8E900434B4559100000002E01000000000000
              000000002555FF00434B455910000000E801000000000000000000002555FF00
              434B4559100000002C020000000000000000000070A8E900434B455910000000
              8902000000000000000000007BB8FF00434B455910000000C802000000000000
              000000002555FF00}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Wood: Water (3)'
            Gradient.Seamless = False
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B455953C0000000434B4559100000007000000000000000000000002555FF00
              434B455910000000A100000000000000000000007BB8FF00434B455910000000
              E0000000000000000000000070A8E900434B4559100000002E01000000000000
              000000002596FF00434B455910000000C10100000000000000000000387CFF00
              434B4559100000002C020000000000000000000070A8E900434B455910000000
              8902000000000000000000007BB8FF00434B455910000000C802000000000000
              000000002555FF00}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Pegtop'
            Gradient.Seamless = False
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595330000000434B4559100000000000000000000000000000002060A000
              434B455910000000E70300000000000000000000F0B81800}
            Gradient.Opacity.Smoothness = 0
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E7030000000000000000000000010000}
          end>
      end
      item
        Name = 'Normal gradients'
        Items = <
          item
            Gradient.Name = 'Red'
            Gradient.Seamless = False
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595348000000434B45591000000000000000000000000000000000000000
              434B455910000000DC0100000000000000000000FF000000434B455910000000
              E80300000000000000000000FFFFFF00}
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E8030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Test'
            Gradient.Seamless = False
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595330000000434B45591000000000000000000000000000000000000000
              434B455910000000E80300000000000000000000FFFFFF00}
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E8030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Blue'
            Gradient.Seamless = False
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595348000000434B45591000000000000000000000000000000000000000
              434B4559100000000302000000000000000000000017FF00434B455910000000
              E80300000000000000000000FFFFFF00}
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E8030000000000000000000000010000}
          end>
      end
      item
        Name = 'Noise gradients'
        Items = <
          item
            Gradient.Name = 'Brown and blue'
            Gradient.Seamless = False
            Gradient.Color.Noise.Strength = 256
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.KeyData = {
              4B45595330000000434B45591000000000000000000000000000000000000000
              434B455910000000E80300000000000000000000FFFFFF00}
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E8030000000000000000000000010000}
          end
          item
            Gradient.Name = 'Pink and green'
            Gradient.Seamless = False
            Gradient.Color.Noise.Strength = 256
            Gradient.Color.Noise.Frequency = 2
            Gradient.Color.Noise.RandomKey = 242457
            Gradient.Color.KeyData = {
              4B45595330000000434B45591000000000000000000000000000000000000000
              434B455910000000E80300000000000000000000FFFFFF00}
            Gradient.Opacity.Noise.Frequency = 2
            Gradient.Opacity.KeyData = {
              4B455953300000004F4B45591000000000000000000000000000000000010000
              4F4B455910000000E8030000000000000000000000010000}
          end>
      end>
    Left = 352
    Top = 48
  end
end
