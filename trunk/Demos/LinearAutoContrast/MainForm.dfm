object frmMain: TfrmMain
  Left = 192
  Top = 133
  Width = 861
  Height = 640
  Caption = 'Linear Auto Contrast'
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
    Left = 655
    Top = 0
    Width = 198
    Height = 577
    Align = alRight
    TabOrder = 0
    object lblZoom: TLabel
      Left = 24
      Top = 11
      Width = 30
      Height = 13
      Caption = 'Zoom:'
      Enabled = False
    end
    object lblShadowsClip: TLabel
      Left = 24
      Top = 200
      Width = 67
      Height = 13
      Caption = 'Shadows Clip:'
    end
    object lblHighlightsClip: TLabel
      Left = 24
      Top = 248
      Width = 66
      Height = 13
      Caption = 'Highlights Clip'
    end
    object btnLoadImage: TButton
      Left = 24
      Top = 50
      Width = 153
      Height = 25
      Cursor = crHandPoint
      Caption = 'Load Image...'
      TabOrder = 0
      OnClick = btnLoadImageClick
    end
    object btnExecute: TButton
      Left = 24
      Top = 128
      Width = 153
      Height = 25
      Cursor = crHandPoint
      Caption = 'Execute Auto Level'
      Enabled = False
      TabOrder = 1
      OnClick = btnExecuteClick
    end
    object cmbbxZoom: TComboBox
      Left = 61
      Top = 8
      Width = 116
      Height = 21
      Cursor = crHandPoint
      Enabled = False
      ItemHeight = 13
      ItemIndex = 3
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
    object btnRestore: TButton
      Left = 24
      Top = 88
      Width = 153
      Height = 25
      Cursor = crHandPoint
      Caption = 'Restore'
      Enabled = False
      TabOrder = 3
      OnClick = btnRestoreClick
    end
    object ggbrShadowsClip: TGaugeBar
      Left = 24
      Top = 216
      Width = 153
      Height = 16
      Cursor = crHandPoint
      Backgnd = bgPattern
      Max = 999
      ShowHandleGrip = True
      Style = rbsMac
      Position = 0
      OnChange = ggbrShadowsClipChange
    end
    object ggbrHighlightsClip: TGaugeBar
      Left = 24
      Top = 264
      Width = 153
      Height = 16
      Cursor = crHandPoint
      Backgnd = bgPattern
      Max = 999
      ShowHandleGrip = True
      Style = rbsMac
      Position = 0
      OnChange = ggbrHighlightsClipChange
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
    Top = 0
    Width = 655
    Height = 577
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
