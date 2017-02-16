object frmPatternName: TfrmPatternName
  Left = 187
  Top = 128
  BorderStyle = bsDialog
  Caption = 'Pattern Name'
  ClientHeight = 97
  ClientWidth = 355
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lblPatternName: TLabel
    Left = 98
    Top = 39
    Width = 31
    Height = 13
    Caption = 'Name:'
  end
  object pnlPatternView: TPanel
    Left = 7
    Top = 7
    Width = 84
    Height = 84
    BevelInner = bvLowered
    TabOrder = 0
    object imgPatternView: TImage32
      Left = 2
      Top = 2
      Width = 81
      Height = 81
      AutoSize = True
      Bitmap.ResamplerClassName = 'TNearestResampler'
      BitmapAlign = baTopLeft
      Scale = 1.000000000000000000
      ScaleMode = smNormal
      TabOrder = 0
    end
  end
  object edtPatternName: TEdit
    Left = 137
    Top = 36
    Width = 137
    Height = 21
    TabOrder = 1
  end
  object btbtnOK: TBitBtn
    Left = 286
    Top = 20
    Width = 61
    Height = 20
    Cursor = crHandPoint
    TabOrder = 2
    Kind = bkOK
  end
  object btbtnCancel: TBitBtn
    Left = 286
    Top = 52
    Width = 61
    Height = 20
    Cursor = crHandPoint
    TabOrder = 3
    Kind = bkCancel
  end
end
