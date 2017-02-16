object frmColorSwatchName: TfrmColorSwatchName
  Left = 191
  Top = 129
  BorderStyle = bsDialog
  Caption = 'Color Swatch Name'
  ClientHeight = 79
  ClientWidth = 344
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
  object grpbxColorSwatchName: TGroupBox
    Left = 7
    Top = 10
    Width = 254
    Height = 59
    TabOrder = 0
    object shpColorSwatch: TShape
      Left = 12
      Top = 20
      Width = 21
      Height = 21
      Cursor = crHandPoint
    end
    object lblColorSwatchName: TLabel
      Left = 39
      Top = 24
      Width = 31
      Height = 13
      Caption = 'Name:'
    end
    object edtColorSwatchName: TEdit
      Left = 78
      Top = 20
      Width = 163
      Height = 21
      TabOrder = 0
    end
  end
  object btbtnOK: TBitBtn
    Left = 273
    Top = 10
    Width = 60
    Height = 20
    Cursor = crHandPoint
    TabOrder = 1
    Kind = bkOK
  end
  object btbtnCancel: TBitBtn
    Left = 273
    Top = 46
    Width = 60
    Height = 20
    Cursor = crHandPoint
    TabOrder = 2
    Kind = bkCancel
  end
end
