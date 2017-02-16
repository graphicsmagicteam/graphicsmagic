object frmSqueeze: TfrmSqueeze
  Left = 192
  Top = 133
  Width = 357
  Height = 209
  Caption = 'Squeeze'
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
    Width = 241
    Height = 145
    TabOrder = 0
    object lblAmount: TLabel
      Left = 40
      Top = 72
      Width = 39
      Height = 13
      Caption = 'Amount:'
    end
    object lblPixels: TLabel
      Left = 160
      Top = 72
      Width = 27
      Height = 13
      Caption = 'Pixels'
    end
    object lblSqueezeStyle: TLabel
      Left = 40
      Top = 32
      Width = 26
      Height = 13
      Caption = 'Style:'
    end
    object edtAmount: TEdit
      Left = 96
      Top = 68
      Width = 50
      Height = 21
      TabOrder = 0
      Text = '0'
      OnChange = edtAmountChange
    end
    object ggbrAmount: TGaugeBar
      Left = 16
      Top = 104
      Width = 209
      Height = 16
      Cursor = crHandPoint
      Backgnd = bgPattern
      ShowHandleGrip = True
      Style = rbsMac
      Position = 1
      OnChange = ggbrAmountChange
      OnMouseDown = ggbrAmountMouseDown
      OnMouseUp = ggbrAmountMouseUp
    end
    object cmbbxSqueezeStyle: TComboBox
      Left = 96
      Top = 28
      Width = 129
      Height = 21
      Cursor = crHandPoint
      ItemHeight = 13
      TabOrder = 2
      OnChange = ChangeSqueezeStyle
      Items.Strings = (
        'Red'
        'Green'
        'Blue'
        'Cyan'
        'Magenta'
        'Yellow')
    end
  end
  object btbtnOK: TBitBtn
    Left = 264
    Top = 16
    Width = 75
    Height = 25
    Cursor = crHandPoint
    TabOrder = 1
    OnClick = btbtnOKClick
    Kind = bkOK
  end
  object btbtnCancel: TBitBtn
    Left = 264
    Top = 56
    Width = 75
    Height = 25
    Cursor = crHandPoint
    TabOrder = 2
    Kind = bkCancel
  end
  object chckbxPreview: TCheckBox
    Left = 264
    Top = 128
    Width = 75
    Height = 17
    Cursor = crHandPoint
    Caption = 'Preview'
    Checked = True
    State = cbChecked
    TabOrder = 3
    OnClick = chckbxPreviewClick
  end
end
