object FileDialogForm: TFileDialogForm
  Left = 506
  Top = 281
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'PegtopFileDialog Demo'
  ClientHeight = 137
  ClientWidth = 266
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefaultPosOnly
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 249
    Height = 25
    Caption = 'Open extended file dialog (user defined)'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 8
    Top = 40
    Width = 249
    Height = 25
    Caption = 'Open graphic file dialog'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 8
    Top = 72
    Width = 249
    Height = 25
    Caption = 'Open text file dialog'
    TabOrder = 2
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 8
    Top = 104
    Width = 249
    Height = 25
    Caption = 'Open wave file dialog'
    TabOrder = 3
    OnClick = Button4Click
  end
  object PegtopExtendedSaveDialog1: TPegtopExtendedSaveDialog
    OnShow = PegtopExtendedSaveDialog1Show
    Filter = 'Text files|*.txt|Delphi units|*.pas'
    ExtendedSize = 150
    ExtendedAlignment = peaRight
    AutoAdjustExtension = True
    Left = 224
    Top = 8
  end
  object PegtopGraphicOpenDialog1: TPegtopGraphicOpenDialog
    Title = 'Select a bmp or jpg file'
    PreviewQuality = ptqResampling2x
    ExtendedSize = 160
    Left = 224
    Top = 40
  end
  object PegtopTextOpenDialog1: TPegtopTextOpenDialog
    DefaultExt = 'txt'
    Title = 'Select a txt or rtf file'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ExtendedSize = 160
    Left = 224
    Top = 72
  end
  object PegtopWaveOpenDialog1: TPegtopWaveOpenDialog
    DefaultExt = 'wav'
    Title = 'Select a wav file'
    Left = 224
    Top = 104
  end
end
