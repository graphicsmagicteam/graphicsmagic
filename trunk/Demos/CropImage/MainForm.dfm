object frmMain: TfrmMain
  Left = 190
  Top = 130
  Width = 448
  Height = 329
  Caption = 'Crop Demo'
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
    Left = 255
    Top = 0
    Width = 185
    Height = 285
    Align = alRight
    TabOrder = 0
    object Button1: TButton
      Left = 8
      Top = 8
      Width = 169
      Height = 25
      Cursor = crHandPoint
      Caption = 'Load Image...'
      TabOrder = 0
      OnClick = LoadImageClick
    end
    object GroupBox: TGroupBox
      Left = 8
      Top = 48
      Width = 169
      Height = 153
      Caption = 'Crop Options:'
      TabOrder = 1
      Visible = False
      object lblShieldOpacity: TLabel
        Left = 8
        Top = 24
        Width = 71
        Height = 13
        Caption = 'Shield Opacity:'
      end
      object Label1: TLabel
        Left = 8
        Top = 64
        Width = 59
        Height = 13
        Caption = 'Shield Color:'
      end
      object shpShieldColor: TShape
        Left = 8
        Top = 80
        Width = 153
        Height = 17
        Cursor = crHandPoint
        OnMouseDown = shpShieldColorMouseDown
      end
      object ggbrShieldOpacity: TGaugeBar
        Left = 8
        Top = 40
        Width = 153
        Height = 16
        Cursor = crHandPoint
        Backgnd = bgPattern
        ShowHandleGrip = True
        Style = rbsMac
        Position = 0
        OnChange = ggbrShieldOpacityChange
      end
      object btnCommit: TButton
        Left = 8
        Top = 112
        Width = 153
        Height = 25
        Cursor = crHandPoint
        Caption = 'Commit Crop'
        TabOrder = 1
        OnClick = CommitCropClick
      end
    end
  end
  object imgvwWorkArea: TImgView32
    Left = 0
    Top = 0
    Width = 255
    Height = 285
    Cursor = crCross
    Align = alClient
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baCustom
    Scale = 1.000000000000000000
    ScaleMode = smScale
    ScrollBars.ShowHandleGrip = True
    ScrollBars.Style = rbsMac
    ScrollBars.Size = 17
    OverSize = 0
    TabOrder = 1
    OnMouseDown = imgvwWorkAreaMouseDown
    OnMouseMove = imgvwWorkAreaMouseMove
    OnMouseUp = imgvwWorkAreaMouseUp
    OnResize = imgvwWorkAreaResize
  end
  object OpenPictureDialog: TOpenPictureDialog
    Filter = 'JPEG Image File (*.jpg)|*.jpg'
    Left = 61
    Top = 56
  end
  object ColorDialog: TColorDialog
    Left = 96
    Top = 56
  end
end
