object frmFeatherSelection: TfrmFeatherSelection
  Left = 191
  Top = 130
  BorderStyle = bsDialog
  Caption = 'Feather Selection'
  ClientHeight = 74
  ClientWidth = 290
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object grpbxFeatherRadius: TGroupBox
    Left = 7
    Top = 7
    Width = 202
    Height = 58
    TabOrder = 0
    object lblFeatherRadius: TLabel
      Left = 13
      Top = 22
      Width = 72
      Height = 13
      Caption = 'FeatherRadius:'
    end
    object lblPixels: TLabel
      Left = 163
      Top = 22
      Width = 27
      Height = 13
      Caption = 'Pixels'
    end
    object edtFeatherRadius: TEdit
      Left = 91
      Top = 18
      Width = 66
      Height = 21
      TabOrder = 0
      Text = '0'
      OnChange = edtFeatherRadiusChange
    end
  end
  object btbtnOK: TBitBtn
    Left = 221
    Top = 10
    Width = 60
    Height = 20
    Cursor = crHandPoint
    TabOrder = 1
    OnClick = btbtnOKClick
    Kind = bkOK
  end
  object btbtnCancel: TBitBtn
    Left = 221
    Top = 43
    Width = 60
    Height = 20
    Cursor = crHandPoint
    TabOrder = 2
    Kind = bkCancel
  end
end
