object frmSplitImage: TfrmSplitImage
  Left = 192
  Top = 132
  BorderStyle = bsDialog
  Caption = 'Split Image'
  ClientHeight = 171
  ClientWidth = 352
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
    Height = 89
    TabOrder = 0
    object lblAmount: TLabel
      Left = 40
      Top = 24
      Width = 39
      Height = 13
      Caption = 'Amount:'
    end
    object Label1: TLabel
      Left = 168
      Top = 24
      Width = 27
      Height = 13
      Caption = 'Pixels'
    end
    object edtAmount: TEdit
      Left = 104
      Top = 20
      Width = 50
      Height = 21
      TabOrder = 0
      Text = '0'
      OnChange = edtAmountChange
    end
    object ggbrAmount: TGaugeBar
      Left = 16
      Top = 56
      Width = 209
      Height = 16
      Cursor = crHandPoint
      Backgnd = bgPattern
      Max = 200
      ShowHandleGrip = True
      Style = rbsMac
      Position = 1
      OnChange = ggbrAmountChange
      OnMouseDown = ggbrAmountMouseDown
      OnMouseUp = ggbrAmountMouseUp
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
    Top = 136
    Width = 75
    Height = 17
    Cursor = crHandPoint
    Caption = 'Preview'
    Checked = True
    State = cbChecked
    TabOrder = 3
    OnClick = chckbxPreviewClick
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 104
    Width = 241
    Height = 57
    Caption = 'Split Options:'
    TabOrder = 4
    object rdbtnRoundSplit: TRadioButton
      Left = 24
      Top = 24
      Width = 89
      Height = 17
      Cursor = crHandPoint
      Caption = 'Round Split'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = ChangeSplitType
    end
    object rdbtnWasteSplit: TRadioButton
      Left = 128
      Top = 24
      Width = 81
      Height = 17
      Cursor = crHandPoint
      Caption = 'Waste Split'
      TabOrder = 1
      OnClick = ChangeSplitType
    end
  end
end
