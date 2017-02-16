object frmMain: TfrmMain
  Left = 192
  Top = 133
  Width = 666
  Height = 364
  Caption = 'Unsharp Mask Demo'
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
    Left = 324
    Top = 0
    Width = 334
    Height = 320
    Align = alRight
    TabOrder = 0
    object btnLoadImage: TButton
      Left = 104
      Top = 16
      Width = 107
      Height = 33
      Cursor = crHandPoint
      Caption = 'Load Image...'
      TabOrder = 0
      OnClick = LoadImageClick
    end
    object grpbxOptions: TGroupBox
      Left = 8
      Top = 64
      Width = 313
      Height = 177
      Caption = 'Unsharp Mask Options'
      TabOrder = 1
      Visible = False
      object lblAmount: TLabel
        Left = 16
        Top = 24
        Width = 39
        Height = 13
        Caption = 'Amount:'
      end
      object lblRadius: TLabel
        Left = 16
        Top = 72
        Width = 36
        Height = 13
        Caption = 'Radius:'
      end
      object lblThreshold: TLabel
        Left = 16
        Top = 120
        Width = 50
        Height = 13
        Caption = 'Threshold:'
      end
      object ggbrAmount: TGaugeBar
        Left = 8
        Top = 40
        Width = 289
        Height = 16
        Cursor = crHandPoint
        Backgnd = bgPattern
        Max = 500
        Min = 1
        ShowHandleGrip = True
        Style = rbsMac
        Position = 50
        OnChange = ggbrAmountChange
        OnMouseUp = GaugeBarMouseUp
      end
      object ggbrRadius: TGaugeBar
        Left = 8
        Top = 88
        Width = 289
        Height = 16
        Cursor = crHandPoint
        Backgnd = bgPattern
        Max = 2500
        Min = 1
        ShowHandleGrip = True
        Style = rbsMac
        Position = 10
        OnChange = ggbrRadiusChange
        OnMouseUp = GaugeBarMouseUp
      end
      object ggbrThreshold: TGaugeBar
        Left = 8
        Top = 144
        Width = 289
        Height = 16
        Cursor = crHandPoint
        Backgnd = bgPattern
        Max = 255
        ShowHandleGrip = True
        Style = rbsMac
        Position = 0
        OnChange = ggbrThresholdChange
        OnMouseUp = GaugeBarMouseUp
      end
    end
  end
  object imgvwWorkArea: TImgView32
    Left = 0
    Top = 0
    Width = 324
    Height = 320
    Align = alClient
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baCustom
    Scale = 1.000000000000000000
    ScaleMode = smScale
    ScrollBars.ShowHandleGrip = True
    ScrollBars.Style = rbsMac
    ScrollBars.Size = 21
    OverSize = 0
    TabOrder = 1
  end
  object OpenPictureDialog: TOpenPictureDialog
    Filter = 'JPEG Image File (*.jpg)|*.jpg'
    Left = 24
    Top = 32
  end
end
