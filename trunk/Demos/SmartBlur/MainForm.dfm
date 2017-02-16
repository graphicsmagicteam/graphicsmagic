object frmMain: TfrmMain
  Left = 188
  Top = 126
  Width = 870
  Height = 640
  Caption = 'Smart Blur Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 393
    Top = 60
    Width = 8
    Height = 536
    Beveled = True
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 862
    Height = 60
    Align = alTop
    TabOrder = 0
    object lblRadius: TLabel
      Left = 96
      Top = 12
      Width = 36
      Height = 13
      Caption = 'Radius:'
      Enabled = False
    end
    object lblThreshold: TLabel
      Left = 312
      Top = 12
      Width = 50
      Height = 13
      Caption = 'Threshold:'
      Enabled = False
    end
    object ggbrRadius: TGaugeBar
      Left = 96
      Top = 28
      Width = 200
      Height = 16
      Cursor = crHandPoint
      Backgnd = bgPattern
      Enabled = False
      Max = 1000
      Min = 1
      ShowHandleGrip = True
      Style = rbsMac
      Position = 1
      OnChange = ggbrRadiusChange
      OnMouseUp = ggbrRadiusMouseUp
    end
    object ggbrThreshold: TGaugeBar
      Left = 312
      Top = 28
      Width = 200
      Height = 16
      Cursor = crHandPoint
      Backgnd = bgPattern
      Enabled = False
      Max = 1000
      Min = 1
      ShowHandleGrip = True
      Style = rbsMac
      Position = 1
      OnChange = ggbrThresholdChange
      OnMouseUp = ggbrThresholdMouseUp
    end
    object btnOpenImage: TButton
      Left = 8
      Top = 16
      Width = 75
      Height = 25
      Cursor = crHandPoint
      Caption = 'Open...'
      TabOrder = 2
      OnClick = btnOpenImageClick
    end
  end
  object pnlOriginal: TPanel
    Left = 0
    Top = 60
    Width = 393
    Height = 536
    Align = alLeft
    TabOrder = 1
    object Panel3: TPanel
      Left = 1
      Top = 1
      Width = 391
      Height = 25
      Align = alTop
      Caption = 'Original'
      Color = clBtnShadow
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
    object imgvwOriginal: TImgView32
      Left = 1
      Top = 26
      Width = 391
      Height = 509
      Align = alClient
      Bitmap.ResamplerClassName = 'TNearestResampler'
      BitmapAlign = baCustom
      Color = clBtnFace
      ParentColor = False
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
    Left = 401
    Top = 60
    Width = 461
    Height = 536
    Align = alClient
    TabOrder = 2
    object Panel2: TPanel
      Left = 1
      Top = 1
      Width = 459
      Height = 25
      Align = alTop
      Caption = 'Result'
      Color = clBtnShadow
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
    object imgvwResult: TImgView32
      Left = 1
      Top = 26
      Width = 459
      Height = 509
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
    Filter = 'JPEG Image File (*.jpg)|*.jpg'
    Left = 8
    Top = 104
  end
end
