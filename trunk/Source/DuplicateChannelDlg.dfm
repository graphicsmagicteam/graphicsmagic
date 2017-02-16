object frmDuplicateChannel: TfrmDuplicateChannel
  Left = 192
  Top = 133
  BorderStyle = bsDialog
  Caption = 'Duplicate Channel'
  ClientHeight = 109
  ClientWidth = 378
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
  object lblDuplicateChannelName: TLabel
    Left = 16
    Top = 16
    Width = 48
    Height = 13
    Caption = 'Duplicate:'
  end
  object lblDuplicateChannelAs: TLabel
    Left = 49
    Top = 40
    Width = 15
    Height = 13
    Caption = 'As:'
  end
  object edtDuplicateChannelAs: TEdit
    Left = 72
    Top = 36
    Width = 200
    Height = 21
    TabOrder = 0
  end
  object chckbxInvertChannel: TCheckBox
    Left = 16
    Top = 80
    Width = 97
    Height = 17
    Cursor = crHandPoint
    Caption = 'Invert'
    TabOrder = 1
  end
  object btbtnOK: TBitBtn
    Left = 288
    Top = 16
    Width = 75
    Height = 25
    Cursor = crHandPoint
    TabOrder = 2
    OnClick = btbtnOKClick
    Kind = bkOK
  end
  object btbtnCancel: TBitBtn
    Left = 288
    Top = 56
    Width = 75
    Height = 25
    Cursor = crHandPoint
    TabOrder = 3
    Kind = bkCancel
  end
end
