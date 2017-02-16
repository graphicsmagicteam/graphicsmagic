object frmAbout: TfrmAbout
  Left = 188
  Top = 129
  BorderStyle = bsDialog
  Caption = 'About GraphicsMagic'
  ClientHeight = 313
  ClientWidth = 392
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object pnlPicture: TPanel
    Left = 0
    Top = 0
    Width = 392
    Height = 313
    Align = alClient
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 0
    object lblProgramName: TLabel
      Left = 13
      Top = 15
      Width = 189
      Height = 29
      Caption = 'GraphicsMagic'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -25
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold, fsItalic]
      ParentFont = False
    end
    object lblCopyright: TLabel
      Left = 137
      Top = 223
      Width = 114
      Height = 13
      Caption = 'Copyright (C) 2001-2017'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object lblVersion: TLabel
      Left = 23
      Top = 51
      Width = 66
      Height = 13
      Caption = 'Professional 2'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label1: TLabel
      Left = 24
      Top = 240
      Width = 345
      Height = 13
      Caption = 'Ma Xiaoguang, Ma Xiaoming and the GraphicsMagic Development Team'
    end
    object btbtnOk: TBitBtn
      Left = 295
      Top = 272
      Width = 75
      Height = 25
      Cursor = crHandPoint
      TabOrder = 0
      Kind = bkOK
    end
    object btnLicence: TButton
      Left = 93
      Top = 272
      Width = 75
      Height = 25
      Cursor = crHandPoint
      Caption = 'Licence'
      TabOrder = 1
      OnClick = ShowLicence
    end
    object memoSpeeches: TMemo
      Left = 8
      Top = 80
      Width = 377
      Height = 129
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      Lines.Strings = (
        'Dear ladies and gentlemen:'
        '    '
        '    Thank you for using GraphicsMagic!'
        ''
        
          '    We are the authors of GraphicsMagic. We are twins. The elder' +
          #39's '
        'name is Ma Xiaoguang and the younger'#39's name is Ma Xiaoming. We '
        
          'both love developing softweres. So we developed this program. In' +
          ' period '
        
          'of developing this program, we got a lot of helps from our frien' +
          'ds on '
        
          'World Wide Web. Without your helps, we couldn'#39't develop this pro' +
          'gram. '
        
          'Thanks again. By the way, you could click the Credits button to ' +
          'see '
        'the special thanks list.'
        ''
        '    Ma Xiaoming & Ma Xiaoguang')
      ParentFont = False
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 2
    end
    object btnCredits: TButton
      Left = 13
      Top = 272
      Width = 75
      Height = 25
      Caption = 'Credits'
      TabOrder = 3
      OnClick = ShowCredits
    end
  end
end
