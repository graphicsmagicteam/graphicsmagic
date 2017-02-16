object frmDuplicateLayer: TfrmDuplicateLayer
  Left = 193
  Top = 132
  BorderStyle = bsDialog
  Caption = 'Duplicate Layer'
  ClientHeight = 82
  ClientWidth = 411
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lblDuplicateLayer: TLabel
    Left = 16
    Top = 16
    Width = 48
    Height = 13
    Caption = 'Duplicate:'
  end
  object lblDuplicateLayerAs: TLabel
    Left = 49
    Top = 40
    Width = 15
    Height = 13
    Caption = 'As:'
  end
  object edtDuplicateLayerName: TEdit
    Left = 72
    Top = 36
    Width = 250
    Height = 21
    TabOrder = 0
  end
  object btbtnOK: TBitBtn
    Left = 336
    Top = 16
    Width = 60
    Height = 20
    Cursor = crHandPoint
    TabOrder = 1
    OnClick = btbtnOKClick
    Kind = bkOK
  end
  object btbtnCancel: TBitBtn
    Left = 336
    Top = 48
    Width = 60
    Height = 20
    Cursor = crHandPoint
    TabOrder = 2
    Kind = bkCancel
  end
end
