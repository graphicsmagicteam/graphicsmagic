object frmAboutExpBlur: TfrmAboutExpBlur
  Left = 190
  Top = 141
  ActiveControl = BitBtn1
  BorderStyle = bsDialog
  Caption = 'About Exponential Blur'
  ClientHeight = 113
  ClientWidth = 403
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
    Width = 385
    Height = 49
    Color = clBtnFace
    Lines.Strings = (
      ''
      
        '  The author of original C/C++ code is Petri Damsten ( petri.dam' +
        'sten@iki.fi )')
    ReadOnly = True
    TabOrder = 0
  end
  object BitBtn1: TBitBtn
    Left = 163
    Top = 72
    Width = 75
    Height = 25
    Cursor = crHandPoint
    TabOrder = 1
    Kind = bkOK
  end
end
