object frmAddNoise: TfrmAddNoise
  Left = 199
  Top = 140
  BorderStyle = bsDialog
  Caption = 'Add Noise'
  ClientHeight = 349
  ClientWidth = 324
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
  object grpbxOptions: TGroupBox
    Left = 8
    Top = 8
    Width = 225
    Height = 337
    TabOrder = 0
    object lblAmount: TLabel
      Left = 8
      Top = 248
      Width = 39
      Height = 13
      Caption = 'Amount:'
    end
    object edtAmount: TEdit
      Left = 56
      Top = 244
      Width = 50
      Height = 21
      TabOrder = 0
      OnChange = edtAmountChange
    end
    object ggbrAmount: TGaugeBar
      Left = 8
      Top = 272
      Width = 209
      Height = 16
      Cursor = crHandPoint
      Backgnd = bgPattern
      Max = 400
      ShowHandleGrip = True
      Style = rbsMac
      Position = 1
      OnChange = ggbrAmountChange
      OnMouseUp = ggbrAmountMouseUp
    end
    object chckbxMonochromatic: TCheckBox
      Left = 8
      Top = 304
      Width = 97
      Height = 17
      Cursor = crHandPoint
      Caption = 'Monochromatic'
      TabOrder = 2
      OnClick = chckbxMonochromaticClick
    end
    object pnlThumbnail: TPanel
      Left = 8
      Top = 16
      Width = 192
      Height = 192
      BevelInner = bvLowered
      TabOrder = 3
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
      Position = 0
      OnChange = ggbrHorzChange
      OnMouseDown = ggbrHorzMouseDown
      OnMouseUp = ggbrHorzMouseUp
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
  end
  object btbtnOK: TBitBtn
    Left = 240
    Top = 16
    Width = 75
    Height = 25
    Cursor = crHandPoint
    TabOrder = 1
    OnClick = btbtnOKClick
    Kind = bkOK
  end
  object btbtnCancel: TBitBtn
    Left = 240
    Top = 56
    Width = 75
    Height = 25
    Cursor = crHandPoint
    TabOrder = 2
    Kind = bkCancel
  end
  object chckbxPreview: TCheckBox
    Left = 240
    Top = 104
    Width = 75
    Height = 17
    Cursor = crHandPoint
    Caption = 'Preview'
    Checked = True
    State = cbChecked
    TabOrder = 3
    OnClick = chckbxPreviewClick
  end
end
