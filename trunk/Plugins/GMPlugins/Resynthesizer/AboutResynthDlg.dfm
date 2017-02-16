object frmAboutResynthesizer: TfrmAboutResynthesizer
  Left = 193
  Top = 130
  ActiveControl = BitBtn1
  BorderStyle = bsDialog
  Caption = 'About Resynthesizer'
  ClientHeight = 239
  ClientWidth = 394
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
    Width = 377
    Height = 177
    Color = clBtnFace
    Lines.Strings = (
      ''
      
        '  The Resynthesizer - Originally, a GIMP plug-in for resynthesiz' +
        'ing textures'
      '  '
      '  Copyright (C) 2000  Paul Francis Harrison'
      '  Copyright (C) 2002  Laurent Despeyroux'
      '  Copyright (C) 2002  David Rodr'#38860'uez Garc'#38854
      ''
      '  Version: 0.14'
      '  License: GNU'
      ' '
      
        '  Note: We changed it for GraphicsMagic, but due to our poor dev' +
        'eloping '
      'skills, we just get part functionalities of the plug-in.')
    ReadOnly = True
    TabOrder = 0
  end
  object BitBtn1: TBitBtn
    Left = 160
    Top = 200
    Width = 75
    Height = 25
    Cursor = crHandPoint
    TabOrder = 1
    Kind = bkOK
  end
end
