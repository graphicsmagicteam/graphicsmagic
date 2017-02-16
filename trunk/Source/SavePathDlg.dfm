object frmSavePath: TfrmSavePath
  Left = 191
  Top = 131
  BorderStyle = bsDialog
  Caption = 'Save Path'
  ClientHeight = 78
  ClientWidth = 282
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lblPathName: TLabel
    Left = 8
    Top = 32
    Width = 31
    Height = 13
    Caption = 'Name:'
  end
  object edtPathName: TEdit
    Left = 48
    Top = 28
    Width = 150
    Height = 21
    TabOrder = 0
  end
  object btbtnOK: TBitBtn
    Left = 208
    Top = 13
    Width = 61
    Height = 20
    Cursor = crHandPoint
    TabOrder = 1
    OnClick = btbtnOKClick
    Kind = bkOK
  end
  object btbtnCancel: TBitBtn
    Left = 208
    Top = 46
    Width = 61
    Height = 20
    Cursor = crHandPoint
    TabOrder = 2
    Kind = bkCancel
  end
end
