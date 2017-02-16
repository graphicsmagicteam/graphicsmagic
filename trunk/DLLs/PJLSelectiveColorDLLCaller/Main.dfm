object frmMain: TfrmMain
  Left = 191
  Top = 135
  Width = 862
  Height = 640
  Caption = 'Peng Jia Le -- Selective Color'
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
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 854
    Height = 41
    Align = alTop
    TabOrder = 0
    object lblZoom: TLabel
      Left = 136
      Top = 12
      Width = 30
      Height = 13
      Caption = 'Zoom:'
      Enabled = False
    end
    object btnLoadImage: TButton
      Left = 8
      Top = 8
      Width = 105
      Height = 25
      Cursor = crHandPoint
      Caption = 'Load Image...'
      TabOrder = 0
      OnClick = btnLoadImageClick
    end
    object cmbbxZoom: TComboBox
      Left = 184
      Top = 8
      Width = 145
      Height = 21
      Cursor = crHandPoint
      Enabled = False
      ItemHeight = 13
      ItemIndex = 3
      TabOrder = 1
      Text = '100%'
      OnChange = cmbbxZoomChange
      Items.Strings = (
        '25%'
        '50%'
        '75%'
        '100%'
        '125%'
        '150%'
        '175%'
        '200%')
    end
  end
  object ImgView: TImgView32
    Left = 0
    Top = 41
    Width = 624
    Height = 555
    Align = alClient
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baCustom
    Scale = 1.000000000000000000
    ScaleMode = smScale
    ScrollBars.ShowHandleGrip = True
    ScrollBars.Style = rbsDefault
    ScrollBars.Size = 21
    OverSize = 0
    TabOrder = 1
  end
  object Panel2: TPanel
    Left = 624
    Top = 41
    Width = 230
    Height = 555
    Align = alRight
    TabOrder = 2
    object lblColors: TLabel
      Left = 16
      Top = 24
      Width = 32
      Height = 13
      Caption = 'Colors:'
      Enabled = False
    end
    object lblCyan: TLabel
      Left = 16
      Top = 72
      Width = 27
      Height = 13
      Caption = 'Cyan:'
      Enabled = False
    end
    object lblMagenta: TLabel
      Left = 16
      Top = 112
      Width = 45
      Height = 13
      Caption = 'Magenta:'
      Enabled = False
    end
    object lblYellow: TLabel
      Left = 16
      Top = 152
      Width = 34
      Height = 13
      Caption = 'Yellow:'
      Enabled = False
    end
    object lblBlack: TLabel
      Left = 16
      Top = 192
      Width = 30
      Height = 13
      Caption = 'Black:'
      Enabled = False
    end
    object cmbbxColors: TComboBox
      Left = 64
      Top = 20
      Width = 145
      Height = 21
      Cursor = crHandPoint
      DropDownCount = 10
      Enabled = False
      ItemHeight = 13
      TabOrder = 0
      OnChange = cmbbxColorsChange
    end
    object ggbrCyan: TGaugeBar
      Left = 16
      Top = 88
      Width = 200
      Height = 16
      Cursor = crHandPoint
      Backgnd = bgPattern
      Enabled = False
      Max = 200
      ShowHandleGrip = True
      Style = rbsMac
      Position = 0
      OnChange = ggbrCyanChange
    end
    object ggbrMagenta: TGaugeBar
      Left = 16
      Top = 128
      Width = 200
      Height = 16
      Cursor = crHandPoint
      Backgnd = bgPattern
      Enabled = False
      Max = 200
      ShowHandleGrip = True
      Style = rbsMac
      Position = 0
      OnChange = ggbrMagentaChange
    end
    object ggbrYellow: TGaugeBar
      Left = 16
      Top = 168
      Width = 200
      Height = 16
      Cursor = crHandPoint
      Backgnd = bgPattern
      Enabled = False
      Max = 200
      ShowHandleGrip = True
      Style = rbsMac
      Position = 0
      OnChange = ggbrYellowChange
    end
    object ggbrBlack: TGaugeBar
      Left = 16
      Top = 208
      Width = 200
      Height = 16
      Cursor = crHandPoint
      Backgnd = bgPattern
      Enabled = False
      Max = 200
      ShowHandleGrip = True
      Style = rbsMac
      Position = 0
      OnChange = ggbrBlackChange
    end
    object rdbtnRelative: TRadioButton
      Left = 40
      Top = 256
      Width = 80
      Height = 17
      Cursor = crHandPoint
      Caption = 'Relative'
      Enabled = False
      TabOrder = 5
      OnClick = rdbtnRelativeClick
    end
    object rdbtnAbsolute: TRadioButton
      Left = 136
      Top = 256
      Width = 80
      Height = 17
      Cursor = crHandPoint
      Caption = 'Absolute'
      Enabled = False
      TabOrder = 6
      OnClick = rdbtnAbsoluteClick
    end
  end
  object OpenPictureDialog: TOpenPictureDialog
    Left = 8
    Top = 49
  end
end
