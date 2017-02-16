object frmGradientName: TfrmGradientName
  Left = 192
  Top = 128
  BorderStyle = bsDialog
  Caption = 'Gradient Name'
  ClientHeight = 69
  ClientWidth = 445
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lblName: TLabel
    Left = 120
    Top = 28
    Width = 31
    Height = 13
    Caption = 'Name:'
  end
  object edtName: TEdit
    Left = 160
    Top = 22
    Width = 200
    Height = 21
    TabOrder = 0
  end
  object btbtnOK: TBitBtn
    Left = 376
    Top = 8
    Width = 61
    Height = 20
    Cursor = crHandPoint
    TabOrder = 1
    OnClick = btbtnOKClick
    Kind = bkOK
  end
  object btbtnCancel: TBitBtn
    Left = 376
    Top = 40
    Width = 61
    Height = 20
    Cursor = crHandPoint
    TabOrder = 2
    Kind = bkCancel
  end
  object Panel1: TPanel
    Left = 8
    Top = 18
    Width = 100
    Height = 32
    BevelInner = bvLowered
    TabOrder = 3
    object imgSelectedGradient: TImage32
      Left = 2
      Top = 2
      Width = 96
      Height = 28
      Align = alClient
      Bitmap.ResamplerClassName = 'TNearestResampler'
      BitmapAlign = baTopLeft
      Scale = 1.000000000000000000
      ScaleMode = smNormal
      TabOrder = 0
      OnPaintStage = imgSelectedGradientPaintStage
    end
  end
end
