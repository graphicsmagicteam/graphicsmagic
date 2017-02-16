object frmUnsharpMask: TfrmUnsharpMask
  Left = 190
  Top = 131
  BorderStyle = bsDialog
  Caption = 'Unsharp Mask'
  ClientHeight = 434
  ClientWidth = 335
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lblAmount: TLabel
    Left = 35
    Top = 268
    Width = 39
    Height = 13
    Caption = 'Amount:'
  end
  object lblRadius: TLabel
    Left = 38
    Top = 324
    Width = 36
    Height = 13
    Caption = 'Radius:'
  end
  object lblThreshold: TLabel
    Left = 24
    Top = 380
    Width = 50
    Height = 13
    Caption = 'Threshold:'
  end
  object Label4: TLabel
    Left = 136
    Top = 268
    Width = 8
    Height = 13
    Caption = '%'
  end
  object Label5: TLabel
    Left = 136
    Top = 324
    Width = 26
    Height = 13
    Caption = 'pixels'
  end
  object Label6: TLabel
    Left = 136
    Top = 380
    Width = 27
    Height = 13
    Caption = 'levels'
  end
  object chckbxPreview: TCheckBox
    Left = 248
    Top = 104
    Width = 73
    Height = 17
    Cursor = crHandPoint
    Caption = 'Preview'
    TabOrder = 0
    OnClick = chckbxPreviewClick
  end
  object btbtnOK: TBitBtn
    Left = 248
    Top = 24
    Width = 75
    Height = 25
    Cursor = crHandPoint
    TabOrder = 1
    OnClick = btbtnOKClick
    Kind = bkOK
  end
  object btbtnCancel: TBitBtn
    Left = 248
    Top = 64
    Width = 75
    Height = 25
    Cursor = crHandPoint
    TabOrder = 2
    Kind = bkCancel
  end
  object ggbrAmount: TGaugeBar
    Left = 16
    Top = 290
    Width = 305
    Height = 16
    Cursor = crHandPoint
    Backgnd = bgPattern
    Max = 500
    Min = 1
    ShowHandleGrip = True
    Style = rbsMac
    Position = 50
    OnChange = ggbrAmountChange
    OnMouseDown = GaugeBarMouseDown
    OnMouseUp = GaugeBarMouseUp
  end
  object ggbrRadius: TGaugeBar
    Left = 16
    Top = 346
    Width = 305
    Height = 16
    Cursor = crHandPoint
    Backgnd = bgPattern
    Max = 2500
    Min = 1
    ShowHandleGrip = True
    Style = rbsMac
    Position = 10
    OnChange = ggbrRadiusChange
    OnMouseDown = GaugeBarMouseDown
    OnMouseUp = GaugeBarMouseUp
  end
  object ggbrThreshold: TGaugeBar
    Left = 16
    Top = 402
    Width = 305
    Height = 16
    Cursor = crHandPoint
    Backgnd = bgPattern
    Max = 255
    ShowHandleGrip = True
    Style = rbsMac
    Position = 0
    OnChange = ggbrThresholdChange
    OnMouseDown = GaugeBarMouseDown
    OnMouseUp = GaugeBarMouseUp
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 9
    Width = 225
    Height = 240
    TabOrder = 6
    object pnlThumbnail: TPanel
      Left = 8
      Top = 16
      Width = 192
      Height = 192
      BevelInner = bvLowered
      TabOrder = 0
      object imgThumbnail: TImage32
        Left = 2
        Top = 2
        Width = 188
        Height = 188
        Align = alClient
        Bitmap.ResamplerClassName = 'TNearestResampler'
        BitmapAlign = baTopLeft
        Scale = 1.000000000000000000
        ScaleMode = smNormal
        TabOrder = 0
        OnMouseDown = imgThumbnailMouseDown
        OnMouseMove = imgThumbnailMouseMove
        OnMouseUp = imgThumbnailMouseUp
        OnPaintStage = imgThumbnailPaintStage
      end
    end
    object ggbrHorz: TGaugeBar
      Left = 8
      Top = 208
      Width = 193
      Height = 16
      Cursor = crHandPoint
      Backgnd = bgPattern
      ShowHandleGrip = True
      Style = rbsMac
      Visible = False
      Position = 0
      OnChange = ggbrHorzChange
      OnMouseDown = ThumbnailGaugeBarMouseDown
      OnMouseUp = ThumbnailGaugeBarMouseUp
    end
    object ggbrVert: TGaugeBar
      Left = 200
      Top = 16
      Width = 16
      Height = 193
      Cursor = crHandPoint
      Backgnd = bgPattern
      Kind = sbVertical
      ShowHandleGrip = True
      Style = rbsMac
      Visible = False
      Position = 0
      OnChange = ggbrVertChange
      OnMouseDown = ThumbnailGaugeBarMouseDown
      OnMouseUp = ThumbnailGaugeBarMouseUp
    end
  end
  object edtAmount: TEdit
    Left = 80
    Top = 264
    Width = 50
    Height = 21
    TabOrder = 7
    OnChange = edtAmountChange
  end
  object edtRadius: TEdit
    Left = 80
    Top = 320
    Width = 50
    Height = 21
    TabOrder = 8
    OnChange = edtRadiusChange
  end
  object edtThreshold: TEdit
    Left = 80
    Top = 376
    Width = 50
    Height = 21
    TabOrder = 9
    OnChange = edtThresholdChange
  end
end
