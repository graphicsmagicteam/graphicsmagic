object frmSolarize: TfrmSolarize
  Left = 192
  Top = 133
  BorderStyle = bsDialog
  Caption = 'Solarize'
  ClientHeight = 124
  ClientWidth = 327
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 217
    Height = 105
    TabOrder = 0
    object lblAmount: TLabel
      Left = 40
      Top = 32
      Width = 39
      Height = 13
      Caption = 'Amount:'
    end
    object edtAmount: TEdit
      Left = 88
      Top = 28
      Width = 50
      Height = 21
      TabOrder = 0
      Text = '0'
      OnChange = edtAmountChange
    end
    object ggbrAmount: TGaugeBar
      Left = 16
      Top = 62
      Width = 185
      Height = 16
      Cursor = crHandPoint
      Backgnd = bgPattern
      Max = 255
      ShowHandleGrip = True
      Style = rbsMac
      Position = 0
      OnChange = ggbrAmountChange
      OnMouseDown = ggbrAmountMouseDown
      OnMouseUp = ggbrAmountMouseUp
    end
  end
  object btbtnOK: TBitBtn
    Left = 240
    Top = 16
    Width = 75
    Height = 25
    Cursor = crHandPoint
    TabOrder = 1
    OnClick = btbtnOKClick
    Kind = bkOK
  end
  object btbtnCancel: TBitBtn
    Left = 240
    Top = 56
    Width = 75
    Height = 25
    Cursor = crHandPoint
    TabOrder = 2
    Kind = bkCancel
  end
  object chckbxPreview: TCheckBox
    Left = 240
    Top = 96
    Width = 73
    Height = 17
    Cursor = crHandPoint
    Caption = 'Preview'
    Checked = True
    State = cbChecked
    TabOrder = 3
    OnClick = chckbxPreviewClick
  end
end
