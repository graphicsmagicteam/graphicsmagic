object frmPencilSketch: TfrmPencilSketch
  Left = 191
  Top = 130
  BorderStyle = bsDialog
  Caption = 'Pencil Sketch'
  ClientHeight = 323
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
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 225
    Height = 305
    TabOrder = 0
    object lblRadius: TLabel
      Left = 8
      Top = 248
      Width = 36
      Height = 13
      Caption = 'Radius:'
    end
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
    object ggbrRadius: TGaugeBar
      Left = 8
      Top = 272
      Width = 201
      Height = 16
      Cursor = crHandPoint
      Backgnd = bgPattern
      Max = 20
      Min = 1
      ShowHandleGrip = True
      Style = rbsMac
      Position = 2
      OnChange = ggbrRadiusChange
      OnMouseDown = ggbrRadiusMouseDown
      OnMouseUp = ggbrRadiusMouseUp
    end
    object edtRadius: TEdit
      Left = 159
      Top = 244
      Width = 50
      Height = 21
      TabOrder = 4
      OnChange = edtRadiusChange
    end
  end
  object btbtnOK: TBitBtn
    Left = 248
    Top = 16
    Width = 75
    Height = 25
    Cursor = crHandPoint
    TabOrder = 1
    OnClick = btbtnOKClick
    Kind = bkOK
  end
  object btbtnCancel: TBitBtn
    Left = 248
    Top = 56
    Width = 75
    Height = 25
    Cursor = crHandPoint
    TabOrder = 2
    Kind = bkCancel
  end
  object chckbxPreview: TCheckBox
    Left = 248
    Top = 96
    Width = 75
    Height = 17
    Cursor = crHandPoint
    Caption = 'Preview'
    TabOrder = 3
    OnClick = chckbxPreviewClick
  end
end
