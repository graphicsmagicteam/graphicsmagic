object frmMain: TfrmMain
  Left = 190
  Top = 129
  Width = 800
  Height = 500
  Caption = 'Image Effects Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  WindowState = wsMaximized
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlImageFiles: TPanel
    Left = 0
    Top = 0
    Width = 180
    Height = 456
    Align = alLeft
    TabOrder = 0
    object Splitter1: TSplitter
      Left = 1
      Top = 120
      Width = 178
      Height = 3
      Cursor = crVSplit
      Align = alTop
    end
    object Splitter2: TSplitter
      Left = 1
      Top = 223
      Width = 178
      Height = 3
      Cursor = crVSplit
      Align = alTop
    end
    object pnlDriveBoxHolder: TPanel
      Left = 1
      Top = 1
      Width = 178
      Height = 19
      Align = alTop
      AutoSize = True
      BevelOuter = bvNone
      TabOrder = 0
      object DriveComboBox1: TDriveComboBox
        Left = 0
        Top = 0
        Width = 177
        Height = 19
        DirList = DirectoryListBox1
        TabOrder = 0
      end
    end
    object DirectoryListBox1: TDirectoryListBox
      Left = 1
      Top = 20
      Width = 178
      Height = 100
      Align = alTop
      FileList = FileListBox1
      ItemHeight = 16
      TabOrder = 1
    end
    object FileListBox1: TFileListBox
      Left = 1
      Top = 123
      Width = 178
      Height = 100
      Align = alTop
      ItemHeight = 16
      Mask = '*.jpg;*.bmp'
      ShowGlyphs = True
      TabOrder = 2
      OnChange = FileListBox1Change
      OnDblClick = FileListBox1DblClick
    end
    object Panel1: TPanel
      Left = 1
      Top = 226
      Width = 178
      Height = 168
      Align = alTop
      BevelInner = bvLowered
      TabOrder = 3
      object imgThumbnail: TImage32
        Left = 2
        Top = 2
        Width = 174
        Height = 164
        Align = alClient
        Bitmap.ResamplerClassName = 'TNearestResampler'
        BitmapAlign = baCenter
        Scale = 1.000000000000000000
        ScaleMode = smScale
        TabOrder = 0
      end
    end
    object Panel2: TPanel
      Left = 1
      Top = 394
      Width = 178
      Height = 61
      Align = alClient
      BevelOuter = bvLowered
      TabOrder = 4
      object cmbbxZoomer: TComboBox
        Left = 16
        Top = 20
        Width = 145
        Height = 21
        Cursor = crHandPoint
        ItemHeight = 13
        ItemIndex = 4
        TabOrder = 0
        Text = '100%'
        OnChange = cmbbxZoomerChange
        Items.Strings = (
          '1600%'
          '800%'
          '400%'
          '200%'
          '100%'
          '50%'
          '25%'
          '12.5%'
          '6.25%')
      end
    end
  end
  object pnlFilters: TPanel
    Left = 612
    Top = 0
    Width = 180
    Height = 456
    Align = alRight
    TabOrder = 1
    object rdbtnBoxBlur: TRadioButton
      Left = 32
      Top = 16
      Width = 113
      Height = 17
      Cursor = crHandPoint
      Caption = 'Box Blur'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = ChangeFilterClick
    end
    object rdbtnGaussianBlur: TRadioButton
      Left = 32
      Top = 40
      Width = 113
      Height = 17
      Cursor = crHandPoint
      Caption = 'Gaussian Blur'
      TabOrder = 1
      OnClick = ChangeFilterClick
    end
    object rdbtnArtFragmentation: TRadioButton
      Left = 32
      Top = 88
      Width = 113
      Height = 17
      Cursor = crHandPoint
      Caption = 'Art Fragmentation'
      TabOrder = 2
      OnClick = ChangeFilterClick
    end
    object rdbtnSelectiveColor: TRadioButton
      Left = 32
      Top = 112
      Width = 113
      Height = 17
      Caption = 'Selective Color'
      TabOrder = 3
      OnClick = ChangeFilterClick
    end
    object rdbtnMotionBlur: TRadioButton
      Left = 32
      Top = 64
      Width = 113
      Height = 17
      Cursor = crHandPoint
      Caption = 'Motion Blur'
      TabOrder = 4
      OnClick = ChangeFilterClick
    end
  end
  object pnlView: TPanel
    Left = 195
    Top = 0
    Width = 402
    Height = 456
    Align = alClient
    TabOrder = 2
    object imgvwDisplay: TImgView32
      Left = 1
      Top = 21
      Width = 400
      Height = 157
      Align = alClient
      Bitmap.ResamplerClassName = 'TNearestResampler'
      BitmapAlign = baCustom
      Scale = 1.000000000000000000
      ScaleMode = smScale
      ScrollBars.ShowHandleGrip = True
      ScrollBars.Style = rbsDefault
      OverSize = 0
      TabOrder = 0
    end
    object pnlOptions: TPanel
      Left = 1
      Top = 193
      Width = 400
      Height = 262
      Align = alBottom
      TabOrder = 1
      Visible = False
      object ntbkFilterOptions: TNotebook
        Left = 1
        Top = 1
        Width = 398
        Height = 260
        Align = alClient
        PageIndex = 3
        TabOrder = 0
        object TPage
          Left = 0
          Top = 0
          Caption = 'Default'
        end
        object TPage
          Left = 0
          Top = 0
          Caption = 'BoxBlur'
          object scrlbxBoxBlurOptions: TScrollBox
            Left = 0
            Top = 0
            Width = 398
            Height = 148
            Align = alClient
            TabOrder = 0
            object lblBoxBlurHorzRadius: TLabel
              Left = 16
              Top = 16
              Width = 86
              Height = 13
              Caption = 'Horizontal Radius:'
            end
            object lblBoxBlurVertRadius: TLabel
              Left = 16
              Top = 56
              Width = 74
              Height = 13
              Caption = 'Vertical Radius:'
            end
            object lblBoxBlurIterations: TLabel
              Left = 16
              Top = 96
              Width = 46
              Height = 13
              Caption = 'Iterations:'
            end
            object ggbrBoxBlurHorzRadius: TGaugeBar
              Left = 16
              Top = 30
              Width = 340
              Height = 16
              Cursor = crHandPoint
              Backgnd = bgPattern
              ShowHandleGrip = True
              Style = rbsMac
              Position = 0
              OnChange = ggbrBoxBlurHorzRadiusChange
              OnMouseUp = ggbrBoxBlurHorzRadiusMouseUp
            end
            object ggbrBoxBlurVertRadius: TGaugeBar
              Left = 16
              Top = 70
              Width = 340
              Height = 16
              Cursor = crHandPoint
              Backgnd = bgPattern
              ShowHandleGrip = True
              Style = rbsMac
              Position = 0
              OnChange = ggbrBoxBlurVertRadiusChange
              OnMouseUp = ggbrBoxBlurVertRadiusMouseUp
            end
            object ggbrBoxBlurIterations: TGaugeBar
              Left = 16
              Top = 112
              Width = 340
              Height = 16
              Cursor = crHandPoint
              Backgnd = bgPattern
              Max = 10
              ShowHandleGrip = True
              Style = rbsMac
              Position = 1
              OnChange = ggbrBoxBlurIterationsChange
              OnMouseUp = ggbrBoxBlurIterationsMouseUp
            end
          end
        end
        object TPage
          Left = 0
          Top = 0
          Caption = 'GaussianBlur'
          object lblGaussianBlurRadius: TLabel
            Left = 16
            Top = 56
            Width = 104
            Height = 13
            Caption = 'Gaussian Blur Radius:'
          end
          object ggbrGaussianBlurRadius: TGaugeBar
            Left = 16
            Top = 72
            Width = 340
            Height = 16
            Cursor = crHandPoint
            Backgnd = bgPattern
            Max = 1000
            Min = 1
            ShowHandleGrip = True
            Style = rbsMac
            Position = 10
            OnChange = ggbrGaussianBlurRadiusChange
            OnMouseUp = ggbrGaussianBlurRadiusMouseUp
          end
        end
        object TPage
          Left = 0
          Top = 0
          Caption = 'MotionBlur'
          object lblMotionBlurAngle: TLabel
            Left = 24
            Top = 32
            Width = 27
            Height = 13
            Caption = 'Angle'
          end
          object lblMotionBlurDistance: TLabel
            Left = 24
            Top = 58
            Width = 42
            Height = 13
            Caption = 'Distance'
          end
          object lblMotionBlurRotation: TLabel
            Left = 24
            Top = 82
            Width = 40
            Height = 13
            Caption = 'Rotation'
          end
          object lblMotionBlurZoom: TLabel
            Left = 24
            Top = 106
            Width = 27
            Height = 13
            Caption = 'Zoom'
          end
          object lblMotionBlurAngleValue: TLabel
            Left = 280
            Top = 34
            Width = 6
            Height = 13
            Caption = '0'
          end
          object lblMotionBlurDistanceValue: TLabel
            Left = 280
            Top = 58
            Width = 6
            Height = 13
            Caption = '0'
          end
          object lblMotionBlurRotationValue: TLabel
            Left = 280
            Top = 82
            Width = 6
            Height = 13
            Caption = '0'
          end
          object lblMotionBlurZoomValue: TLabel
            Left = 280
            Top = 106
            Width = 6
            Height = 13
            Caption = '0'
          end
          object ggbrMotionBlurAngle: TGaugeBar
            Left = 72
            Top = 32
            Width = 200
            Height = 16
            Cursor = crHandPoint
            Backgnd = bgPattern
            Max = 360
            ShowHandleGrip = True
            Style = rbsMac
            Position = 0
            OnChange = ggbrMotionBlurAngleChange
            OnMouseUp = ggbrMotionBlurAngleMouseUp
          end
          object ggbrMotionBlurDistance: TGaugeBar
            Left = 72
            Top = 56
            Width = 200
            Height = 16
            Cursor = crHandPoint
            Backgnd = bgPattern
            Max = 200
            ShowHandleGrip = True
            Style = rbsMac
            Position = 0
            OnChange = ggbrMotionBlurDistanceChange
            OnMouseUp = ggbrMotionBlurDistanceMouseUp
          end
          object ggbrMotionBlurRotation: TGaugeBar
            Left = 72
            Top = 80
            Width = 200
            Height = 16
            Cursor = crHandPoint
            Backgnd = bgPattern
            Max = 360
            ShowHandleGrip = True
            Style = rbsMac
            Position = 180
            OnChange = ggbrMotionBlurRotationChange
            OnMouseUp = ggbrMotionBlurRotationMouseUp
          end
          object ggbrMotionBlurZoom: TGaugeBar
            Left = 72
            Top = 104
            Width = 200
            Height = 16
            Cursor = crHandPoint
            Backgnd = bgPattern
            ShowHandleGrip = True
            Style = rbsMac
            Position = 0
            OnChange = ggbrMotionBlurZoomChange
            OnMouseUp = ggbrMotionBlurZoomMouseUp
          end
          object chckbxMotionBlurWrapEdges: TCheckBox
            Left = 24
            Top = 136
            Width = 97
            Height = 17
            Cursor = crHandPoint
            Caption = 'Wrap Edges'
            TabOrder = 4
            OnClick = chckbxMotionBlurWrapEdgesClick
          end
        end
        object TPage
          Left = 0
          Top = 0
          Caption = 'ArtFragmentation'
          object Label1: TLabel
            Left = 32
            Top = 32
            Width = 61
            Height = 13
            Caption = 'Border Color:'
          end
          object shpArtFragBorderColor: TShape
            Left = 32
            Top = 48
            Width = 321
            Height = 25
            Cursor = crHandPoint
            OnMouseUp = shpArtFragBorderColorMouseUp
          end
          object chckbxArtFragAutoBorder: TCheckBox
            Left = 32
            Top = 96
            Width = 113
            Height = 17
            Cursor = crHandPoint
            Caption = 'Auto Border Color'
            TabOrder = 0
            OnClick = chckbxArtFragAutoBorderClick
          end
        end
        object TPage
          Left = 0
          Top = 0
          Caption = 'SelectiveColor'
          object lblCyanScale: TLabel
            Left = 32
            Top = 48
            Width = 44
            Height = 13
            Caption = 'Cyan: 0%'
          end
          object lblMagentaScale: TLabel
            Left = 32
            Top = 88
            Width = 62
            Height = 13
            Caption = 'Magenta: 0%'
          end
          object lblYellowScale: TLabel
            Left = 32
            Top = 128
            Width = 51
            Height = 13
            Caption = 'Yellow: 0%'
          end
          object lblBlackScale: TLabel
            Left = 32
            Top = 168
            Width = 47
            Height = 13
            Caption = 'Black: 0%'
          end
          object lblSelectiveColors: TLabel
            Left = 72
            Top = 16
            Width = 32
            Height = 13
            Caption = 'Colors:'
          end
          object Label2: TLabel
            Left = 32
            Top = 224
            Width = 39
            Height = 13
            Caption = 'Method:'
          end
          object ggbrCyanScale: TGaugeBar
            Left = 32
            Top = 64
            Width = 289
            Height = 16
            Cursor = crHandPoint
            Backgnd = bgPattern
            Max = 200
            ShowHandleGrip = True
            Style = rbsMac
            Position = 100
            OnChange = ggbrCyanScaleChange
            OnMouseUp = ggbrCyanScaleMouseUp
          end
          object ggbrMagentaScale: TGaugeBar
            Left = 32
            Top = 104
            Width = 289
            Height = 16
            Cursor = crHandPoint
            Backgnd = bgPattern
            Max = 200
            ShowHandleGrip = True
            Style = rbsMac
            Position = 100
            OnChange = ggbrMagentaScaleChange
            OnMouseUp = ggbrMagentaScaleMouseUp
          end
          object ggbrYellowScale: TGaugeBar
            Left = 32
            Top = 144
            Width = 289
            Height = 16
            Cursor = crHandPoint
            Backgnd = bgPattern
            Max = 200
            ShowHandleGrip = True
            Style = rbsMac
            Position = 100
            OnChange = ggbrYellowScaleChange
            OnMouseUp = ggbrYellowScaleMouseUp
          end
          object ggbrBlackScale: TGaugeBar
            Left = 32
            Top = 184
            Width = 289
            Height = 16
            Cursor = crHandPoint
            Backgnd = bgPattern
            Max = 200
            ShowHandleGrip = True
            Style = rbsMac
            Position = 100
            OnChange = ggbrBlackScaleChange
            OnMouseUp = ggbrBlackScaleMouseUp
          end
          object cmbbxSelectiveColors: TComboBox
            Left = 112
            Top = 12
            Width = 145
            Height = 21
            ItemHeight = 13
            ItemIndex = 0
            TabOrder = 4
            Text = 'Reds'
            OnChange = cmbbxSelectiveColorsChange
            Items.Strings = (
              'Reds'
              'Yellows'
              'Greens'
              'Cyans'
              'Blues'
              'Magentas')
          end
          object rdbtnSelectiveColorRelative: TRadioButton
            Left = 96
            Top = 224
            Width = 105
            Height = 17
            Cursor = crHandPoint
            Caption = 'Relative'
            Checked = True
            TabOrder = 5
            TabStop = True
            OnClick = ChangeSelectiveColorMethod
          end
          object rdbtnSelectiveColorAbsolute: TRadioButton
            Left = 216
            Top = 224
            Width = 105
            Height = 17
            Cursor = crHandPoint
            Caption = 'Absolute'
            TabOrder = 6
            OnClick = ChangeSelectiveColorMethod
          end
        end
      end
    end
    object pnlBottomCollapse: TPanel
      Left = 1
      Top = 178
      Width = 400
      Height = 15
      Align = alBottom
      TabOrder = 2
      OnResize = pnlBottomCollapseResize
      object pnlLeftPad: TPanel
        Left = 1
        Top = 1
        Width = 185
        Height = 13
        Align = alLeft
        TabOrder = 0
      end
      object pnlBottomArrowHolder: TPanel
        Left = 186
        Top = 1
        Width = 50
        Height = 13
        Align = alLeft
        TabOrder = 1
        object imgBottomCollapse: TImage32
          Left = 1
          Top = 1
          Width = 48
          Height = 11
          Cursor = crHandPoint
          Align = alClient
          Bitmap.ResamplerClassName = 'TNearestResampler'
          BitmapAlign = baCenter
          Scale = 1.000000000000000000
          ScaleMode = smNormal
          TabOrder = 0
          OnMouseDown = imgBottomCollapseMouseDown
          OnMouseUp = imgBottomCollapseMouseUp
        end
      end
      object pnlRightPad: TPanel
        Left = 236
        Top = 1
        Width = 163
        Height = 13
        Align = alClient
        TabOrder = 2
      end
    end
    object pnlTitle: TPanel
      Left = 1
      Top = 1
      Width = 400
      Height = 20
      Align = alTop
      BevelOuter = bvNone
      Color = clBtnShadow
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindow
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
    end
  end
  object pnlLeftCollapse: TPanel
    Left = 180
    Top = 0
    Width = 15
    Height = 456
    Align = alLeft
    TabOrder = 3
    OnResize = pnlLeftCollapseResize
    object pnlLeftArrowHolder: TPanel
      Left = 1
      Top = 185
      Width = 13
      Height = 50
      Align = alTop
      TabOrder = 0
      object imgLeftCollapse: TImage32
        Left = 1
        Top = 1
        Width = 11
        Height = 48
        Cursor = crHandPoint
        Align = alClient
        Bitmap.ResamplerClassName = 'TNearestResampler'
        BitmapAlign = baCenter
        Scale = 1.000000000000000000
        ScaleMode = smNormal
        TabOrder = 0
        OnMouseDown = imgLeftCollapseMouseDown
        OnMouseUp = imgLeftCollapseMouseUp
      end
    end
    object pnlTopPad1: TPanel
      Left = 1
      Top = 1
      Width = 13
      Height = 184
      Align = alTop
      TabOrder = 1
    end
    object pnlBottomPad1: TPanel
      Left = 1
      Top = 235
      Width = 13
      Height = 220
      Align = alClient
      TabOrder = 2
    end
  end
  object pnlRightCollapse: TPanel
    Left = 597
    Top = 0
    Width = 15
    Height = 456
    Align = alRight
    TabOrder = 4
    OnResize = pnlRightCollapseResize
    object pnlTopPad2: TPanel
      Left = 1
      Top = 1
      Width = 13
      Height = 184
      Align = alTop
      TabOrder = 0
    end
    object pnlRightArrowHolder: TPanel
      Left = 1
      Top = 185
      Width = 13
      Height = 50
      Align = alTop
      TabOrder = 1
      object imgRightCollapse: TImage32
        Left = 1
        Top = 1
        Width = 11
        Height = 48
        Cursor = crHandPoint
        Align = alClient
        Bitmap.ResamplerClassName = 'TNearestResampler'
        BitmapAlign = baCenter
        Scale = 1.000000000000000000
        ScaleMode = smNormal
        TabOrder = 0
        OnMouseDown = imgRightCollapseMouseDown
        OnMouseUp = imgRightCollapseMouseUp
      end
    end
    object pnlBottomPad2: TPanel
      Left = 1
      Top = 235
      Width = 13
      Height = 220
      Align = alClient
      TabOrder = 2
    end
  end
  object Bitmap32List1: TBitmap32List
    Bitmaps = <
      item
        Bitmap.ResamplerClassName = 'TNearestResampler'
        Bitmap.Data = {
          0A00000009000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFF808080FF000000FFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFF808080FF000000FF000000FF000000FFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFF808080FF000000FF000000FF000000FF000000FF
          000000FFFFFFFFFFFFFFFFFF808080FF000000FF000000FF000000FF000000FF
          000000FF000000FF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF808080FF
          000000FF000000FF000000FF000000FF000000FFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFF808080FF000000FF000000FF000000FFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF808080FF
          000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
      end
      item
        Bitmap.ResamplerClassName = 'TNearestResampler'
        Bitmap.Data = {
          0A00000009000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF808080FFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF
          000000FF000000FF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFF000000FF000000FF000000FF000000FF000000FF808080FFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFF000000FF000000FF000000FF000000FF000000FF
          000000FF000000FF808080FFFFFFFFFFFFFFFFFF000000FF000000FF000000FF
          000000FF000000FF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF
          000000FF000000FF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFF000000FF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
      end
      item
        Bitmap.ResamplerClassName = 'TNearestResampler'
        Bitmap.Data = {
          090000000A000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF808080FF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          808080FF000000FF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFF000000FF000000FF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFF808080FF000000FF000000FF000000FF808080FFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFF000000FF000000FF000000FF000000FF000000FFFFFFFFFF
          FFFFFFFFFFFFFFFF808080FF000000FF000000FF000000FF000000FF000000FF
          808080FFFFFFFFFFFFFFFFFF000000FF000000FF000000FF000000FF000000FF
          000000FF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
      end
      item
        Bitmap.ResamplerClassName = 'TNearestResampler'
        Bitmap.Data = {
          090000000A000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF000000FF000000FF000000FF
          000000FF000000FF000000FFFFFFFFFFFFFFFFFF808080FF000000FF000000FF
          000000FF000000FF000000FF808080FFFFFFFFFFFFFFFFFFFFFFFFFF000000FF
          000000FF000000FF000000FF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          808080FF000000FF000000FF000000FF808080FFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFF000000FF000000FF000000FFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFF808080FF000000FF808080FFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF808080FFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
      end>
    Left = 40
    Top = 248
  end
  object ColorDialog: TColorDialog
    Left = 72
    Top = 248
  end
end
