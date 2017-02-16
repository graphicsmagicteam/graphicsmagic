object frmAboutEmboss: TfrmAboutEmboss
  Left = 240
  Top = 224
  BorderStyle = bsDialog
  Caption = 'About Emboss'
  ClientHeight = 204
  ClientWidth = 315
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
    Width = 297
    Height = 145
    Color = clBtnFace
    Lines.Strings = (
      ''
      ' The original auther of this Emboss algorithm is John Schlag.'
      ' Eric L. Hernes makes it as a GIMP filter.'
      ''
      ' Copyright (C) John Schlag < jfs@kerner.com >'
      ' Copyright (C) 1997 Eric L. Hernes < erich@rrnet.com > '
      ''
      ' Version: 1.50.2.1'
      ''
      ' Licence: GNU')
    ReadOnly = True
    TabOrder = 0
  end
  object btbtnOK: TBitBtn
    Left = 119
    Top = 168
    Width = 75
    Height = 25
    Cursor = crHandPoint
    TabOrder = 1
    Kind = bkOK
  end
end
