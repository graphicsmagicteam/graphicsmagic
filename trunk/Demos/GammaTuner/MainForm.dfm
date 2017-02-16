object frmMain: TfrmMain
  Left = 192
  Top = 133
  Width = 577
  Height = 361
  Caption = 'Gamma Tuner'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = mnMain
  OldCreateOrder = False
  WindowState = wsMaximized
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object pnlGammaOptionsHolder: TPanel
    Left = 384
    Top = 0
    Width = 185
    Height = 296
    Align = alRight
    TabOrder = 0
    Visible = False
    object lblChannel: TLabel
      Left = 16
      Top = 16
      Width = 42
      Height = 13
      Caption = 'Channel:'
    end
    object lblGammaValue: TLabel
      Left = 16
      Top = 72
      Width = 15
      Height = 13
      Caption = '1.0'
    end
    object cmbbxChannel: TComboBox
      Left = 16
      Top = 32
      Width = 145
      Height = 21
      Cursor = crHandPoint
      ItemHeight = 13
      TabOrder = 0
      OnChange = cmbbxChannelChange
      Items.Strings = (
        'RGB'
        'Red'
        'Green'
        'Blue')
    end
    object ggbrGammaValue: TGaugeBar
      Left = 16
      Top = 88
      Width = 145
      Height = 16
      Cursor = crHandPoint
      Backgnd = bgPattern
      Max = 500
      Min = 1
      ShowHandleGrip = True
      Position = 1
      OnChange = ggbrGammaValueChange
      OnMouseUp = ggbrGammaValueMouseUp
    end
  end
  object imgWorkArea: TImgView32
    Left = 0
    Top = 0
    Width = 384
    Height = 296
    Align = alClient
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baCustom
    Scale = 1.000000000000000000
    ScaleMode = smScale
    ScrollBars.ShowHandleGrip = True
    ScrollBars.Style = rbsDefault
    ScrollBars.Size = 21
    OverSize = 0
    TabOrder = 1
  end
  object OpenPictureDialog: TOpenPictureDialog
    Filter = 'Bitmaps (*.bmp)|*.bmp|JPEG Image File (*.jpg)|*.jpg'
    Left = 64
    Top = 24
  end
  object mnMain: TMainMenu
    Left = 24
    Top = 24
    object mnhdFile: TMenuItem
      Caption = '&File'
      object mnitmOpenFile: TMenuItem
        Caption = 'Open...'
        ShortCut = 16463
        OnClick = mnitmOpenFileClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object mnitmExitProgram: TMenuItem
        Caption = 'Exit'
        ShortCut = 16465
        OnClick = mnitmExitProgramClick
      end
    end
  end
end
