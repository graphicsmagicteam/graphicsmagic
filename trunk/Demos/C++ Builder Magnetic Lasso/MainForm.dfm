object frmMain: TfrmMain
  Left = 203
  Top = 215
  Width = 926
  Height = 640
  Caption = 'Gimp Magnetic Lasso Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  WindowState = wsMaximized
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 609
    Top = 0
    Width = 12
    Height = 543
    Cursor = crHSplit
  end
  object ScrollBox1: TScrollBox
    Left = 0
    Top = 0
    Width = 609
    Height = 543
    Align = alLeft
    TabOrder = 0
    object imgWorkArea: TImage
      Left = 10
      Top = 10
      Width = 519
      Height = 495
      AutoSize = True
      OnMouseDown = imgWorkAreaMouseDown
      OnMouseMove = imgWorkAreaMouseMove
      OnMouseUp = imgWorkAreaMouseUp
    end
  end
  object ScrollBox2: TScrollBox
    Left = 621
    Top = 0
    Width = 297
    Height = 543
    Align = alClient
    TabOrder = 1
    object imgMask: TImage
      Left = 10
      Top = 10
      Width = 105
      Height = 105
      AutoSize = True
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 543
    Width = 918
    Height = 32
    Align = alBottom
    TabOrder = 2
    object Panel2: TPanel
      Left = 1
      Top = 1
      Width = 104
      Height = 30
      Align = alLeft
      BevelInner = bvLowered
      TabOrder = 0
      object chckbxInteractive: TCheckBox
        Left = 8
        Top = 6
        Width = 80
        Height = 17
        Cursor = crHandPoint
        Caption = 'Interactive'
        TabOrder = 0
        OnClick = chckbxInteractiveClick
      end
    end
    object StatusBar1: TStatusBar
      Left = 105
      Top = 1
      Width = 812
      Height = 30
      Align = alClient
      Panels = <>
      SimplePanel = False
    end
  end
  object MainMenu1: TMainMenu
    Left = 120
    Top = 40
    object File1: TMenuItem
      Caption = '&File'
      object mnitmOpen: TMenuItem
        Caption = 'Open...'
        OnClick = OpenImageClick
      end
    end
  end
  object OpenPictureDialog1: TOpenPictureDialog
    Filter = 'Bitmaps (*.bmp)|*.bmp'
    Left = 154
    Top = 42
  end
end
