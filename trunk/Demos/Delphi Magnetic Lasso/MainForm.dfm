object frmMain: TfrmMain
  Left = 190
  Top = 131
  Width = 800
  Height = 333
  Caption = 'Magnetic Lasso Demo for GR32'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poDefault
  WindowState = wsMaximized
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 617
    Top = 0
    Width = 16
    Height = 227
    Beveled = True
  end
  object Panel1: TPanel
    Left = 0
    Top = 227
    Width = 792
    Height = 41
    Align = alBottom
    TabOrder = 0
    object Panel2: TPanel
      Left = 1
      Top = 1
      Width = 288
      Height = 39
      Align = alLeft
      BevelInner = bvLowered
      TabOrder = 0
      object chckbxInteractive: TCheckBox
        Left = 8
        Top = 10
        Width = 81
        Height = 17
        Cursor = crHandPoint
        Caption = 'Interactive'
        TabOrder = 0
        OnClick = chckbxInteractiveClick
      end
      object cmbbxZoomer: TComboBox
        Left = 96
        Top = 8
        Width = 89
        Height = 21
        Cursor = crHandPoint
        ItemHeight = 13
        ItemIndex = 4
        TabOrder = 1
        Text = '100%'
        OnChange = cmbbxZoomerChange
        Items.Strings = (
          '1600%'
          '800%'
          '400%'
          '200%'
          '100%'
          '50%'
          '25%'
          '12.5%'
          '6.25%')
      end
      object btnReset: TButton
        Left = 200
        Top = 8
        Width = 75
        Height = 21
        Cursor = crHandPoint
        Caption = 'Reset'
        TabOrder = 2
        OnClick = btnResetClick
      end
    end
    object pnlStatus: TPanel
      Left = 289
      Top = 1
      Width = 502
      Height = 39
      Align = alClient
      BevelInner = bvLowered
      TabOrder = 1
    end
  end
  object imgvwWorkArea: TImgView32
    Left = 0
    Top = 0
    Width = 617
    Height = 227
    Align = alLeft
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baCustom
    Scale = 1.000000000000000000
    ScaleMode = smScale
    ScrollBars.ShowHandleGrip = True
    ScrollBars.Style = rbsDefault
    ScrollBars.Size = 21
    OverSize = 0
    TabOrder = 1
    OnMouseDown = imgvwWorkAreaMouseDown
    OnMouseMove = imgvwWorkAreaMouseMove
    OnMouseUp = imgvwWorkAreaMouseUp
  end
  object imgvwMask: TImgView32
    Left = 633
    Top = 0
    Width = 159
    Height = 227
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
  object MainMenu1: TMainMenu
    Left = 120
    Top = 88
    object File1: TMenuItem
      Caption = 'File'
      object mnitmOpenImage: TMenuItem
        Caption = 'Open...'
        OnClick = OpenImageClick
      end
    end
  end
  object OpenPictureDialog: TOpenPictureDialog
    Filter = 'JPEG Image File (*.jpg)|*.jpg'
    Left = 152
    Top = 88
  end
end
