object frmAboutGlassTile: TfrmAboutGlassTile
  Left = 190
  Top = 130
  BorderStyle = bsDialog
  Caption = 'About GlassTile'
  ClientHeight = 190
  ClientWidth = 362
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
    Width = 345
    Height = 129
    Color = clBtnFace
    Lines.Strings = (
      ''
      ' Originally, it is the Glass Tile plug-in for the GIMP 1.2'
      ' '
      
        ' Copyright (C) 1997 Karl-Johan Andersson (t96kja@student.tdb.uu.' +
        'se) '
      ''
      ' Version: 1.02'
      ''
      ' Licence: GNU'
      ' '
      '  ')
    TabOrder = 0
  end
  object BitBtn1: TBitBtn
    Left = 144
    Top = 152
    Width = 75
    Height = 25
    Cursor = crHandPoint
    TabOrder = 1
    Kind = bkOK
  end
end
