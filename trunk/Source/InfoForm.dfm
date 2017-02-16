object frmInfo: TfrmInfo
  Left = 195
  Top = 136
  Width = 236
  Height = 160
  BorderStyle = bsSizeToolWin
  Caption = 'Info'
  Color = clBtnFace
  DragKind = dkDock
  DragMode = dmAutomatic
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 8
    Top = 74
    Width = 161
    Height = 3
    Shape = bsTopLine
  end
  object lblRed: TLabel
    Left = 21
    Top = 12
    Width = 11
    Height = 13
    Caption = 'R:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Bevel2: TBevel
    Left = 112
    Top = 12
    Width = 3
    Height = 55
    Shape = bsLeftLine
  end
  object Bevel3: TBevel
    Left = 112
    Top = 80
    Width = 3
    Height = 27
    Shape = bsLeftLine
  end
  object lblGreen: TLabel
    Left = 21
    Top = 26
    Width = 11
    Height = 13
    Caption = 'G:'
  end
  object lblBlue: TLabel
    Left = 22
    Top = 40
    Width = 10
    Height = 13
    Caption = 'B:'
  end
  object lblHex: TLabel
    Left = 10
    Top = 54
    Width = 22
    Height = 13
    Caption = 'Hex:'
  end
  object lblCyan: TLabel
    Left = 128
    Top = 12
    Width = 10
    Height = 13
    Caption = 'C:'
  end
  object lblMagenta: TLabel
    Left = 127
    Top = 26
    Width = 12
    Height = 13
    Caption = 'M:'
  end
  object lblYellow: TLabel
    Left = 128
    Top = 40
    Width = 10
    Height = 13
    Caption = 'Y:'
  end
  object lblBlack: TLabel
    Left = 128
    Top = 54
    Width = 10
    Height = 13
    Caption = 'K:'
  end
  object lblOriginalX: TLabel
    Left = 8
    Top = 80
    Width = 10
    Height = 13
    Caption = 'X:'
  end
  object lblCurrentX: TLabel
    Left = 120
    Top = 80
    Width = 10
    Height = 13
    Caption = 'X:'
  end
  object lblOriginalY: TLabel
    Left = 8
    Top = 96
    Width = 10
    Height = 13
    Caption = 'Y:'
  end
  object lblCurrentY: TLabel
    Left = 120
    Top = 96
    Width = 10
    Height = 13
    Caption = 'Y:'
  end
end
