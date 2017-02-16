object frmChannelOptions: TfrmChannelOptions
  Left = 189
  Top = 130
  BorderStyle = bsDialog
  Caption = 'Channel Options'
  ClientHeight = 214
  ClientWidth = 394
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lblChannelName: TLabel
    Left = 8
    Top = 16
    Width = 31
    Height = 13
    Caption = 'Name:'
  end
  object edtChannelName: TEdit
    Left = 48
    Top = 12
    Width = 241
    Height = 21
    TabOrder = 0
  end
  object rdgrpColorIndicator: TRadioGroup
    Left = 8
    Top = 48
    Width = 281
    Height = 73
    Caption = 'Color Indicates:'
    Items.Strings = (
      'Masked Areas'
      'Selected Areas')
    TabOrder = 1
  end
  object grpbxColor: TGroupBox
    Left = 8
    Top = 128
    Width = 281
    Height = 73
    Caption = 'Color'
    TabOrder = 2
    object shpMaskColor: TShape
      Left = 24
      Top = 22
      Width = 32
      Height = 32
      Cursor = crHandPoint
      OnMouseDown = shpMaskColorMouseDown
    end
    object lblMaskOpacity: TLabel
      Left = 80
      Top = 32
      Width = 39
      Height = 13
      Caption = 'Opacity:'
    end
    object Label1: TLabel
      Left = 184
      Top = 32
      Width = 8
      Height = 13
      Caption = '%'
    end
    object edtMaskOpacity: TEdit
      Left = 128
      Top = 28
      Width = 49
      Height = 21
      TabOrder = 0
      OnChange = edtMaskOpacityChange
    end
  end
  object btbtnOK: TBitBtn
    Left = 304
    Top = 10
    Width = 75
    Height = 25
    Cursor = crHandPoint
    TabOrder = 3
    OnClick = btbtnOKClick
    Kind = bkOK
  end
  object btbtnCancel: TBitBtn
    Left = 304
    Top = 48
    Width = 75
    Height = 25
    Cursor = crHandPoint
    TabOrder = 4
    Kind = bkCancel
  end
  object clrdlgMaskColorSelector: TColorDialog
    Options = [cdFullOpen]
    Left = 304
    Top = 88
  end
end
