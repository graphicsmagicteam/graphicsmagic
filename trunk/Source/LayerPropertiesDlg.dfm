object frmLayerProperties: TfrmLayerProperties
  Left = 185
  Top = 128
  BorderStyle = bsDialog
  Caption = 'Layer Properties'
  ClientHeight = 95
  ClientWidth = 417
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lblLayerName: TLabel
    Left = 16
    Top = 40
    Width = 31
    Height = 13
    Caption = '&Name:'
  end
  object edtLayerName: TEdit
    Left = 56
    Top = 36
    Width = 256
    Height = 21
    TabOrder = 0
  end
  object btbtnOK: TBitBtn
    Left = 328
    Top = 16
    Width = 75
    Height = 25
    Cursor = crHandPoint
    TabOrder = 1
    OnClick = btbtnOKClick
    Kind = bkOK
  end
  object btbtnCancel: TBitBtn
    Left = 328
    Top = 56
    Width = 75
    Height = 25
    Cursor = crHandPoint
    TabOrder = 2
    Kind = bkCancel
  end
end
