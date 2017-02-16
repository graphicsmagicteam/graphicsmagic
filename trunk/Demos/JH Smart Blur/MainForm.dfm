object frmMain: TfrmMain
  Left = 184
  Top = 131
  Width = 870
  Height = 640
  Caption = 'Jerry Huxtable -- Smart Blur'
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
  object Splitter1: TSplitter
    Left = 537
    Top = 65
    Width = 5
    Height = 512
    Color = clBtnShadow
    ParentColor = False
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 862
    Height = 65
    Align = alTop
    BevelOuter = bvNone
    Ctl3D = False
    ParentCtl3D = False
    TabOrder = 0
    object lblRadius: TLabel
      Left = 344
      Top = 16
      Width = 54
      Height = 13
      Caption = 'Radius: 5.0'
      Enabled = False
    end
    object lblThreshold: TLabel
      Left = 560
      Top = 16
      Width = 65
      Height = 13
      Caption = 'Threshold: 10'
      Enabled = False
    end
    object lblZoom: TLabel
      Left = 136
      Top = 27
      Width = 30
      Height = 13
      Caption = 'Zoom:'
      Enabled = False
    end
    object ggbrRadius: TGaugeBar
      Left = 344
      Top = 32
      Width = 200
      Height = 16
      Cursor = crHandPoint
      Backgnd = bgPattern
      Enabled = False
      Max = 10000
      Min = 1
      ShowHandleGrip = True
      Style = rbsMac
      Position = 500
      OnChange = ggbrRadiusChange
      OnMouseUp = ggbrRadiusMouseUp
    end
    object ggbrThreshold: TGaugeBar
      Left = 560
      Top = 32
      Width = 200
      Height = 16
      Cursor = crHandPoint
      Backgnd = bgPattern
      Enabled = False
      Max = 255
      ShowHandleGrip = True
      Style = rbsMac
      Position = 10
      OnChange = ggbrThresholdChange
      OnMouseUp = ggbrThresholdMouseUp
    end
    object btnLoadImage: TButton
      Left = 16
      Top = 18
      Width = 100
      Height = 30
      Cursor = crHandPoint
      Caption = 'Load Image...'
      TabOrder = 2
      OnClick = btnLoadImageClick
    end
    object cmbbxZoom: TComboBox
      Left = 176
      Top = 24
      Width = 145
      Height = 21
      Cursor = crHandPoint
      Enabled = False
      ItemHeight = 13
      ItemIndex = 3
      TabOrder = 3
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
  object StatusBar: TStatusBar
    Left = 0
    Top = 577
    Width = 862
    Height = 19
    Panels = <
      item
        Width = 50
      end>
  end
  object pnlOriginal: TPanel
    Left = 0
    Top = 65
    Width = 537
    Height = 512
    Align = alLeft
    BevelOuter = bvNone
    Ctl3D = False
    ParentCtl3D = False
    TabOrder = 2
    object pnlOriginalHeader: TPanel
      Left = 0
      Top = 0
      Width = 537
      Height = 25
      Align = alTop
      BevelOuter = bvNone
      Caption = 'Original'
      Color = clBtnShadow
      Ctl3D = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clHighlightText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentCtl3D = False
      ParentFont = False
      TabOrder = 0
    end
    object imgOriginal: TImgView32
      Left = 0
      Top = 25
      Width = 537
      Height = 487
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
  end
  object pnlResult: TPanel
    Left = 542
    Top = 65
    Width = 320
    Height = 512
    Align = alClient
    BevelOuter = bvNone
    Ctl3D = False
    ParentCtl3D = False
    TabOrder = 3
    object pnlResultHeader: TPanel
      Left = 0
      Top = 0
      Width = 320
      Height = 25
      Align = alTop
      BevelOuter = bvNone
      Caption = 'Result'
      Color = clBtnShadow
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clHighlightText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
    object imgResult: TImgView32
      Left = 0
      Top = 25
      Width = 320
      Height = 487
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
  end
  object OpenPictureDialog: TOpenPictureDialog
    Left = 8
    Top = 73
  end
end
