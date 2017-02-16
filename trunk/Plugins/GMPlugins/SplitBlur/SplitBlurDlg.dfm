object frmSplitBlur: TfrmSplitBlur
  Left = 188
  Top = 127
  Width = 358
  Height = 169
  Caption = 'Split Blur'
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
    Height = 105
    TabOrder = 0
    object lblRadius: TLabel
      Left = 56
      Top = 32
      Width = 36
      Height = 13
      Caption = 'Radius:'
    end
    object Label1: TLabel
      Left = 160
      Top = 32
      Width = 27
      Height = 13
      Caption = 'Pixels'
    end
    object edtRadius: TEdit
      Left = 104
      Top = 28
      Width = 50
      Height = 21
      TabOrder = 0
      Text = '0'
      OnChange = edtRadiusChange
    end
    object ggbrRadius: TGaugeBar
      Left = 16
      Top = 64
      Width = 209
      Height = 16
      Cursor = crHandPoint
      Backgnd = bgPattern
      ShowHandleGrip = True
      Style = rbsMac
      Position = 1
      OnChange = ggbrRadiusChange
      OnMouseDown = ggbrRadiusMouseDown
      OnMouseUp = ggbrRadiusMouseUp
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
