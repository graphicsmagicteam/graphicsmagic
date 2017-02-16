object frmMain: TfrmMain
  Left = 192
  Top = 133
  Width = 861
  Height = 640
  Caption = 'Histogram Equalize'
  Color = clBtnFace
  Constraints.MinHeight = 480
  Constraints.MinWidth = 640
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 853
    Height = 49
    Align = alTop
    TabOrder = 0
    object lblZoom: TLabel
      Left = 408
      Top = 16
      Width = 30
      Height = 13
      Caption = 'Zoom:'
      Enabled = False
    end
    object btnLoadImage: TButton
      Left = 8
      Top = 10
      Width = 120
      Height = 25
      Cursor = crHandPoint
      Caption = 'Load Image...'
      TabOrder = 0
      OnClick = btnLoadImageClick
    end
    object btnExecute: TButton
      Left = 264
      Top = 10
      Width = 120
      Height = 25
      Cursor = crHandPoint
      Caption = 'Equalize'
      Enabled = False
      TabOrder = 1
      OnClick = btnExecuteClick
    end
    object btnRestore: TButton
      Left = 136
      Top = 10
      Width = 120
      Height = 25
      Cursor = crHandPoint
      Caption = 'Restore'
      Enabled = False
      TabOrder = 2
      OnClick = btnRestoreClick
    end
    object cmbbxZoom: TComboBox
      Left = 445
      Top = 12
      Width = 116
      Height = 21
      Cursor = crHandPoint
      Enabled = False
      ItemHeight = 13
      ItemIndex = 3
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
  object StatusBar: TStatusBar
    Left = 0
    Top = 577
    Width = 853
    Height = 19
    Panels = <
      item
        Width = 50
      end>
  end
  object imgView: TImgView32
    Left = 0
    Top = 49
    Width = 853
    Height = 528
    Align = alClient
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baCustom
    Scale = 1.000000000000000000
    ScaleMode = smScale
    ScrollBars.ShowHandleGrip = True
    ScrollBars.Style = rbsDefault
    ScrollBars.Size = 21
    OverSize = 0
    TabOrder = 2
  end
  object OpenPictureDialog: TOpenPictureDialog
    Left = 40
    Top = 49
  end
end
