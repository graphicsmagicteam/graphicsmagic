object frmMain: TfrmMain
  Left = 190
  Top = 131
  Width = 738
  Height = 495
  Caption = 'Gimp Curves Tool for GR32'
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
    Width = 730
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    Ctl3D = False
    ParentCtl3D = False
    TabOrder = 0
    object lblZoom: TLabel
      Left = 376
      Top = 14
      Width = 30
      Height = 13
      Caption = 'Zoom:'
      Enabled = False
    end
    object btnOpenBitmap: TButton
      Left = 16
      Top = 8
      Width = 105
      Height = 25
      Cursor = crHandPoint
      Caption = 'Open Bitmap...'
      TabOrder = 0
      OnClick = OpenBitmap
    end
    object btnGimpCurves: TButton
      Left = 128
      Top = 8
      Width = 105
      Height = 25
      Cursor = crHandPoint
      Caption = 'Curves...'
      Enabled = False
      TabOrder = 1
      OnClick = OpenGimpCurvesDialog
    end
    object btnRestore: TButton
      Left = 240
      Top = 8
      Width = 105
      Height = 25
      Cursor = crHandPoint
      Caption = 'Restore'
      Enabled = False
      TabOrder = 2
      OnClick = btnRestoreClick
    end
    object cmbbxZoom: TComboBox
      Left = 416
      Top = 10
      Width = 145
      Height = 21
      Cursor = crHandPoint
      Enabled = False
      ItemHeight = 13
      TabOrder = 3
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
  object ScrollBox1: TScrollBox
    Left = 0
    Top = 41
    Width = 730
    Height = 410
    Align = alClient
    TabOrder = 1
    object imgPreview: TImgView32
      Left = 0
      Top = 0
      Width = 726
      Height = 406
      Align = alClient
      Bitmap.ResamplerClassName = 'TNearestResampler'
      BitmapAlign = baCustom
      Centered = False
      Scale = 1.000000000000000000
      ScaleMode = smScale
      ScrollBars.ShowHandleGrip = True
      ScrollBars.Style = rbsDefault
      ScrollBars.Size = 21
      OverSize = 0
      TabOrder = 0
    end
  end
  object OpenPictureDialog: TOpenPictureDialog
    Filter = 'Bitmaps (*.bmp)|*.bmp'
    Left = 256
    Top = 80
  end
end
