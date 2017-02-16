object frmMain: TfrmMain
  Left = 191
  Top = 133
  Width = 574
  Height = 320
  Caption = 'Gimp Levels Tool for GR32'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  WindowState = wsMaximized
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 566
    Height = 41
    Align = alTop
    TabOrder = 0
    object lblZoom: TLabel
      Left = 272
      Top = 14
      Width = 30
      Height = 13
      Caption = 'Zoom:'
      Enabled = False
    end
    object btnOpenImage: TButton
      Left = 8
      Top = 8
      Width = 120
      Height = 25
      Cursor = crHandPoint
      Caption = 'Open Image...'
      TabOrder = 0
      OnClick = OpenImage
    end
    object btnOpenLevelsDlg: TButton
      Left = 136
      Top = 8
      Width = 120
      Height = 25
      Cursor = crHandPoint
      Caption = 'Open Levels Dialog...'
      Enabled = False
      TabOrder = 1
      OnClick = OpenLevelsDialog
    end
    object cmbbxZoom: TComboBox
      Left = 312
      Top = 10
      Width = 145
      Height = 21
      Cursor = crHandPoint
      Enabled = False
      ItemHeight = 13
      TabOrder = 2
      Text = '100%'
      OnChange = cmbbxZoomChange
      Items.Strings = (
        '25%'
        '50%'
        '75%'
        '100%'
        '125%'
        '150%'
        '175%'
        '200%')
    end
  end
  object imgViewer: TImgView32
    Left = 0
    Top = 41
    Width = 566
    Height = 235
    Align = alClient
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baCustom
    Centered = False
    Scale = 1.000000000000000000
    ScaleMode = smScale
    ScrollBars.ShowHandleGrip = True
    ScrollBars.Style = rbsMac
    ScrollBars.Size = 21
    OverSize = 0
    TabOrder = 1
    OnMouseDown = imgViewerMouseDown
  end
  object OpenPictureDialog: TOpenPictureDialog
    Filter = 'Bitmaps (*.bmp)|*.bmp'
    Left = 266
    Top = 171
  end
end
