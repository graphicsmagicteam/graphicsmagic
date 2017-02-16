object frmMain: TfrmMain
  Left = 192
  Top = 133
  Width = 870
  Height = 640
  Caption = 'PNG Alpha --> Bitmap Mask'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  WindowState = wsMaximized
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 625
    Top = 41
    Width = 8
    Height = 555
    Color = clActiveBorder
    ParentColor = False
  end
  object imgPNG: TImgView32
    Left = 0
    Top = 41
    Width = 625
    Height = 555
    Align = alLeft
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baCustom
    Color = clBtnFace
    ParentColor = False
    Scale = 1.000000000000000000
    ScaleMode = smScale
    ScrollBars.ShowHandleGrip = True
    ScrollBars.Style = rbsMac
    ScrollBars.Size = 21
    OverSize = 0
    TabOrder = 0
    OnPaintStage = imgPNGPaintStage
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 862
    Height = 41
    Align = alTop
    TabOrder = 1
    object btnLoad: TButton
      Left = 8
      Top = 8
      Width = 120
      Height = 25
      Cursor = crHandPoint
      Caption = 'Load PNG Image...'
      TabOrder = 0
      OnClick = btnLoadClick
    end
    object btnSave: TButton
      Left = 264
      Top = 8
      Width = 120
      Height = 25
      Cursor = crHandPoint
      Caption = 'Save Mask As...'
      Enabled = False
      TabOrder = 1
      OnClick = btnSaveClick
    end
    object btnGenerateMask: TButton
      Left = 136
      Top = 8
      Width = 120
      Height = 25
      Cursor = crHandPoint
      Caption = 'Generate Mask'
      Enabled = False
      TabOrder = 2
      OnClick = btnGenerateMaskClick
    end
    object cmbbxZoom: TComboBox
      Left = 392
      Top = 10
      Width = 80
      Height = 21
      Cursor = crHandPoint
      Enabled = False
      ItemHeight = 13
      ItemIndex = 3
      TabOrder = 3
      Text = '100%'
      OnChange = ZoomChange
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
  object imgMask: TImgView32
    Left = 633
    Top = 41
    Width = 229
    Height = 555
    Align = alClient
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baCustom
    Scale = 1.000000000000000000
    ScaleMode = smScale
    ScrollBars.ShowHandleGrip = True
    ScrollBars.Style = rbsMac
    ScrollBars.Size = 21
    OverSize = 0
    TabOrder = 2
  end
  object OpenPictureDialog: TOpenPictureDialog
    Filter = 'Portable network graphic images (*.png)|*.png'
    Left = 16
    Top = 56
  end
  object SavePictureDialog: TSavePictureDialog
    Filter = 'Bitmaps (*.bmp)|*.bmp'
    Left = 48
    Top = 56
  end
end
