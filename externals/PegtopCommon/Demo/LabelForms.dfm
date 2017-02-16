object LabelForm: TLabelForm
  Left = 557
  Top = 156
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'PegtopQualityLabel demo'
  ClientHeight = 300
  ClientWidth = 436
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefaultPosOnly
  PixelsPerInch = 96
  TextHeight = 13
  object PegtopQualityLabel8: TPegtopQualityLabel
    Left = 16
    Top = 176
    Width = 70
    Height = 108
    Caption = 'A'
    Font.Charset = ANSI_CHARSET
    Font.Color = clRed
    Font.Height = -96
    Font.Name = 'Arial'
    Font.Style = [fsBold, fsItalic]
    ParentFont = False
    Transparent = True
    Quality = plqResampling4x
  end
  object PegtopQualityLabel1: TPegtopQualityLabel
    Left = 8
    Top = 8
    Width = 95
    Height = 13
    Caption = 'Quality = plqNormal'
    Quality = plqNormal
  end
  object PegtopQualityLabel2: TPegtopQualityLabel
    Left = 8
    Top = 64
    Width = 283
    Height = 36
    Caption = 'Quality = plqNormal'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -32
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    Quality = plqNormal
  end
  object PegtopQualityLabel3: TPegtopQualityLabel
    Left = 8
    Top = 24
    Width = 126
    Height = 13
    Caption = 'Quality = plqResampling2x'
  end
  object PegtopQualityLabel4: TPegtopQualityLabel
    Left = 8
    Top = 40
    Width = 128
    Height = 13
    Caption = 'Quality = plqResampling4x'
    Quality = plqResampling4x
  end
  object PegtopQualityLabel5: TPegtopQualityLabel
    Left = 8
    Top = 104
    Width = 383
    Height = 36
    Caption = 'Quality = plqResampling2x'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -32
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object PegtopQualityLabel6: TPegtopQualityLabel
    Left = 8
    Top = 144
    Width = 381
    Height = 36
    Caption = 'Quality = plqResampling4x'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -32
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    Quality = plqResampling4x
  end
  object PegtopQualityLabel7: TPegtopQualityLabel
    Left = 8
    Top = 192
    Width = 393
    Height = 72
    Caption = 
      'TPegtopQualityLabel works like a normal TLabel, including transp' +
      'arence and word wrapping!'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -21
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    Transparent = True
    WordWrap = True
    Quality = plqResampling4x
  end
end
