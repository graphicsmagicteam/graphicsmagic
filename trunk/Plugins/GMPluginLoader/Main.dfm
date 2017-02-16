object frmMain: TfrmMain
  Left = 190
  Top = 130
  Width = 678
  Height = 390
  Caption = 'Plugin Loader Demo'
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
    Left = 254
    Top = 0
    Width = 16
    Height = 325
    Align = alRight
  end
  object ScrollBox1: TScrollBox
    Left = 0
    Top = 0
    Width = 254
    Height = 325
    Align = alClient
    TabOrder = 0
    object imgView: TImage32
      Left = 32
      Top = 32
      Width = 192
      Height = 192
      AutoSize = True
      Bitmap.ResamplerClassName = 'TNearestResampler'
      BitmapAlign = baTopLeft
      Scale = 1.000000000000000000
      ScaleMode = smNormal
      TabOrder = 0
    end
  end
  object Panel1: TPanel
    Left = 270
    Top = 0
    Width = 400
    Height = 325
    Align = alRight
    TabOrder = 1
    object Label1: TLabel
      Left = 8
      Top = 96
      Width = 45
      Height = 13
      Caption = 'Category:'
    end
    object Label2: TLabel
      Left = 192
      Top = 96
      Width = 25
      Height = 13
      Caption = 'Filter:'
    end
    object CheckListBox1: TCheckListBox
      Left = 8
      Top = 8
      Width = 380
      Height = 73
      ItemHeight = 13
      TabOrder = 0
    end
    object ListBox1: TListBox
      Left = 8
      Top = 112
      Width = 177
      Height = 200
      ItemHeight = 13
      TabOrder = 1
    end
    object ListBox2: TListBox
      Left = 192
      Top = 112
      Width = 193
      Height = 200
      ItemHeight = 13
      TabOrder = 2
    end
  end
  object MainMenu1: TMainMenu
    Left = 56
    Top = 392
    object File1: TMenuItem
      Caption = '&File'
      object mnitmOpen: TMenuItem
        Caption = '&Open...'
        OnClick = mnitmOpenClick
      end
      object mnitmExit: TMenuItem
        Caption = 'E&xit'
        OnClick = mnitmExitClick
      end
    end
    object mnitmFilters: TMenuItem
      Caption = 'Filters'
    end
  end
  object OpenPictureDialog: TOpenPictureDialog
    Filter = 
      'All (*.jpg;*.bmp)|*.jpg;*.bmp|JPEG Image File (*.jpg)|*.jpg|Bitm' +
      'aps (*.bmp)|*.bmp'
    Left = 88
    Top = 392
  end
end
