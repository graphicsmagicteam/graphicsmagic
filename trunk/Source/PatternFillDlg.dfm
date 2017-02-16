object frmPatternFill: TfrmPatternFill
  Left = 188
  Top = 129
  BorderStyle = bsDialog
  Caption = 'Pattern Fill'
  ClientHeight = 82
  ClientWidth = 325
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object spdbtnOpenPattern: TSpeedButton
    Left = 72
    Top = 7
    Width = 13
    Height = 65
    Glyph.Data = {
      F6000000424DF600000000000000360000002800000008000000080000000100
      180000000000C000000000000000000000000000000000000000FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF404040404040FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFF404040000000000000404040FFFFFFFFFFFFFFFFFF404040
      000000000000000000000000404040FFFFFF4040400000000000000000000000
      00000000000000404040FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
    OnClick = spdbtnOpenPatternClick
  end
  object lblScale: TLabel
    Left = 98
    Top = 20
    Width = 30
    Height = 13
    Caption = 'Scale:'
  end
  object lblScalePercent: TLabel
    Left = 176
    Top = 20
    Width = 8
    Height = 13
    Caption = '%'
  end
  object pnlPattern: TPanel
    Left = 7
    Top = 7
    Width = 65
    Height = 65
    BevelInner = bvLowered
    TabOrder = 0
    object imgPattern: TImage32
      Left = 2
      Top = 2
      Width = 61
      Height = 61
      AutoSize = True
      Bitmap.ResamplerClassName = 'TNearestResampler'
      BitmapAlign = baTopLeft
      Scale = 1.000000000000000000
      ScaleMode = smNormal
      TabOrder = 0
    end
  end
  object edtScale: TEdit
    Left = 130
    Top = 16
    Width = 41
    Height = 21
    TabOrder = 1
    Text = '100'
    OnChange = edtScaleChange
  end
  object btbtnOK: TBitBtn
    Left = 254
    Top = 13
    Width = 60
    Height = 20
    Cursor = crHandPoint
    TabOrder = 2
    Kind = bkOK
  end
  object btbtnCancel: TBitBtn
    Left = 254
    Top = 46
    Width = 60
    Height = 20
    Cursor = crHandPoint
    TabOrder = 3
    Kind = bkCancel
  end
  object ggbrScale: TGaugeBar
    Left = 96
    Top = 48
    Width = 145
    Height = 16
    Cursor = crHandPoint
    Backgnd = bgPattern
    Max = 1000
    Min = 1
    ShowHandleGrip = True
    Style = rbsMac
    Position = 100
    OnChange = ggbrScaleChange
  end
end
