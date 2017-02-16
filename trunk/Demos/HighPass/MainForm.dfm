object frmMain: TfrmMain
  Left = 192
  Top = 128
  Width = 397
  Height = 548
  Caption = 'High-Pass Filter'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object imgvwPreview: TImgView32
    Left = 0
    Top = 0
    Width = 389
    Height = 424
    Align = alClient
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baCustom
    Scale = 1.000000000000000000
    ScaleMode = smScale
    ScrollBars.ShowHandleGrip = True
    ScrollBars.Style = rbsMac
    ScrollBars.Size = 21
    OverSize = 0
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 424
    Width = 389
    Height = 59
    Align = alBottom
    BevelOuter = bvNone
    Ctl3D = False
    ParentCtl3D = False
    TabOrder = 1
    object chckbxHighPass: TCheckBox
      Left = 8
      Top = 12
      Width = 97
      Height = 17
      Cursor = crHandPoint
      Caption = 'Radius'
      Enabled = False
      TabOrder = 0
      OnClick = chckbxHighPassClick
    end
    object ggbrRadius: TGaugeBar
      Left = 8
      Top = 33
      Width = 369
      Height = 16
      Backgnd = bgPattern
      Enabled = False
      Max = 2500
      Min = 1
      ShowHandleGrip = True
      Style = rbsMac
      Position = 1
      OnChange = ggbrRadiusChange
      OnMouseUp = ggbrRadiusMouseUp
    end
    object cmbbxZoom: TComboBox
      Left = 317
      Top = 10
      Width = 60
      Height = 21
      Cursor = crHandPoint
      Style = csDropDownList
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
  end
  object OpenPictureDialog: TOpenPictureDialog
    Filter = 'JPEG Image File (*.jpg)|*.jpg'
    Left = 8
    Top = 32
  end
  object MainMenu1: TMainMenu
    Left = 40
    Top = 32
    object File1: TMenuItem
      Caption = 'File'
      object mnitmLoadImage: TMenuItem
        Caption = 'Load Image ...'
        OnClick = mnitmLoadImageClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object mnitmExit: TMenuItem
        Caption = 'Exit'
        OnClick = mnitmExitClick
      end
    end
  end
end
