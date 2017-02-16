object ProgressBarForm: TProgressBarForm
  Left = 378
  Top = 182
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'PegtopProgressBar Demo'
  ClientHeight = 297
  ClientWidth = 289
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefaultPosOnly
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 12
    Width = 198
    Height = 13
    Caption = 'Progress bar position (of all progress bars):'
  end
  object PegtopProgressBar1: TPegtopProgressBar
    Left = 8
    Top = 40
    Width = 249
    Height = 17
    Min = 0
    Max = 100
    Position = 100
    Extent = 3
    Spacing = 1
    Caption = '100 %'
    BackSpace = 36
  end
  object PegtopProgressBar2: TPegtopProgressBar
    Left = 8
    Top = 64
    Width = 249
    Height = 17
    Min = 0
    Max = 100
    Position = 100
    Extent = 6
    Spacing = 2
  end
  object PegtopProgressBar3: TPegtopProgressBar
    Left = 8
    Top = 88
    Width = 249
    Height = 17
    Min = 0
    Max = 100
    Position = 100
    Extent = 8
    Spacing = 2
    Invers = True
  end
  object PegtopProgressBar4: TPegtopProgressBar
    Left = 8
    Top = 112
    Width = 249
    Height = 17
    Min = 0
    Max = 100
    Position = 100
    Direction = ppdLeft
    Extent = 1
    Spacing = 1
    OnColor = PegtopProgressBar4Color
  end
  object PegtopProgressBar5: TPegtopProgressBar
    Left = 8
    Top = 136
    Width = 249
    Height = 17
    Min = 0
    Max = 100
    Position = 100
    Extent = 6
    Spacing = 2
    OnColor = PegtopProgressBar5Color
  end
  object PegtopProgressBar6: TPegtopProgressBar
    Left = 8
    Top = 160
    Width = 249
    Height = 17
    Min = 0
    Max = 100
    Position = 100
    Extent = 6
    Spacing = 2
    OnColor = PegtopProgressBar6Color
  end
  object PegtopProgressBar7: TPegtopProgressBar
    Left = 8
    Top = 184
    Width = 249
    Height = 17
    Min = 0
    Max = 100
    Position = 100
    Extent = 6
    Spacing = 2
    Caption = '100 %'
    CaptionAlignment = ppaCenter
    OnColor = PegtopProgressBar7Color
  end
  object PegtopProgressBar8: TPegtopProgressBar
    Left = 264
    Top = 40
    Width = 17
    Height = 185
    Min = 0
    Max = 100
    Position = 100
    Direction = ppdUp
    Extent = 6
    Spacing = 2
    Caption = '100 %'
    CaptionAlignment = ppaFront
    FrontSpace = 34
    OnColor = PegtopProgressBar8Color
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
  end
  object PegtopProgressBar9: TPegtopProgressBar
    Left = 8
    Top = 208
    Width = 249
    Height = 17
    Min = 0
    Max = 100
    Position = 100
    Extent = 6
    Spacing = 2
    Snake = 5
  end
  object PegtopIntEdit1: TPegtopIntEdit
    Left = 232
    Top = 8
    Width = 49
    Height = 21
    Beep = False
    Caption = '%'
    CaptionAlignment = pcaRight
    Options = []
    SpecialChar = '*'
    SelLength = 0
    SelStart = 0
    TabOrder = 0
    OnChange = PegtopIntEdit1Change
    MaxValue = 100
    MinValue = 0
    Base = 10
    Value = 100
  end
  object Button1: TButton
    Left = 8
    Top = 232
    Width = 273
    Height = 25
    Caption = 'Start / stop animation'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 8
    Top = 264
    Width = 273
    Height = 25
    Caption = 'Enable / disable XP theming'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 20
    OnTimer = Timer1Timer
    Left = 160
    Top = 8
  end
end
