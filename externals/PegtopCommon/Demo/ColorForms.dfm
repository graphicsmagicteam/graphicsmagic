object ColorForm: TColorForm
  Left = 246
  Top = 243
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'PegtopColorControl Demo'
  ClientHeight = 148
  ClientWidth = 280
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefaultPosOnly
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Tag = 1
    Left = 8
    Top = 8
    Width = 265
    Height = 25
    Caption = 'Open color dialog with opactiy control'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 8
    Top = 40
    Width = 265
    Height = 25
    Caption = 'Change form color'
    TabOrder = 1
    OnClick = Button2Click
  end
  object PegtopColorBox1: TPegtopColorBox
    Left = 8
    Top = 72
    Width = 265
    Height = 25
    Color = clBlue
    Caption = 'Select color with this color box'
    TabOrder = 2
    TabStop = True
  end
  object Memo1: TMemo
    Left = 8
    Top = 104
    Width = 265
    Height = 33
    Lines.Strings = (
      'TPegtopColorClipboard supports binary and text '
      'format. Try copy & paste here...')
    ScrollBars = ssVertical
    TabOrder = 3
  end
  object PegtopColorDialog1: TPegtopColorDialog
    Color = clBlack
    Opacity = 50
    Options = [pcoOpacity, pcoUserDefinedColors]
    Left = 16
    Top = 16
  end
  object PegtopColorDialog2: TPegtopColorDialog
    Color = clBtnFace
    Options = [pcoUserDefinedColors]
    Left = 16
    Top = 48
  end
end
