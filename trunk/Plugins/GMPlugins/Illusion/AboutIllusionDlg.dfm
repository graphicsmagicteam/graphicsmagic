object frmAboutIllusion: TfrmAboutIllusion
  Left = 191
  Top = 128
  BorderStyle = bsDialog
  Caption = 'About Illusion'
  ClientHeight = 159
  ClientWidth = 301
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
  object Memo1: TMemo
    Left = 8
    Top = 8
    Width = 281
    Height = 97
    Color = clBtnFace
    Lines.Strings = (
      ''
      ' Originally, this is a plug-in for the GIMP 1.0'
      '  '
      ' Copyright (C) 1997  Hirotsuna Mizuno'
      ''
      ' Licence: GNU'
      ' '
      '  ')
    TabOrder = 0
  end
  object BitBtn1: TBitBtn
    Left = 112
    Top = 120
    Width = 75
    Height = 25
    Cursor = crHandPoint
    TabOrder = 1
    Kind = bkOK
  end
end
