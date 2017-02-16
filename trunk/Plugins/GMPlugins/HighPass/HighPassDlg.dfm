object frmHighPass: TfrmHighPass
  Left = 192
  Top = 125
  BorderStyle = bsDialog
  Caption = 'High Pass'
  ClientHeight = 311
  ClientWidth = 325
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object grpbxHighPassOptions: TGroupBox
    Left = 8
    Top = 8
    Width = 225
    Height = 297
    TabOrder = 0
    object lblRadius: TLabel
      Left = 16
      Top = 248
      Width = 36
      Height = 13
      Caption = 'Radius:'
    end
    object lblPixels: TLabel
      Left = 128
      Top = 248
      Width = 27
      Height = 13
      Caption = 'Pixels'
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
      Position = 0
      OnChange = ggbrVertChange
      OnMouseDown = ggbrVertMouseDown
      OnMouseUp = ggbrVertMouseUp
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
      Position = 0
      OnChange = ggbrHorzChange
      OnMouseDown = ggbrHorzMouseDown
      OnMouseUp = ggbrHorzMouseUp
    end
    object edtRadius: TEdit
      Left = 64
      Top = 244
      Width = 50
      Height = 21
      TabOrder = 3
      OnChange = edtRadiusChange
    end
    object ggbrRadius: TGaugeBar
      Left = 8
      Top = 272
      Width = 201
      Height = 16
      Cursor = crHandPoint
      Backgnd = bgPattern
      Max = 250
      ShowHandleGrip = True
      Style = rbsMac
      Position = 0
      OnChange = ggbrRadiusChange
      OnMouseUp = ggbrRadiusMouseUp
    end
  end
  object btbtnOK: TBitBtn
    Left = 240
    Top = 24
    Width = 75
    Height = 25
    Cursor = crHandPoint
    TabOrder = 1
    OnClick = btbtnOKClick
    Kind = bkOK
  end
  object btbtnCancel: TBitBtn
    Left = 240
    Top = 64
    Width = 75
    Height = 25
    Cursor = crHandPoint
    TabOrder = 2
    Kind = bkCancel
  end
  object chckbxPreview: TCheckBox
    Left = 240
    Top = 112
    Width = 75
    Height = 17
    Cursor = crHandPoint
    Caption = 'Preview'
    TabOrder = 3
    OnClick = chckbxPreviewClick
  end
end
