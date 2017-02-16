object frmExpBlur: TfrmExpBlur
  Left = 191
  Top = 140
  BorderStyle = bsDialog
  Caption = 'Exponential Blur'
  ClientHeight = 140
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
    Height = 121
    TabOrder = 0
    object lblAmout: TLabel
      Left = 64
      Top = 32
      Width = 36
      Height = 13
      Caption = 'Radius:'
    end
    object edtRadius: TEdit
      Left = 112
      Top = 28
      Width = 50
      Height = 21
      TabOrder = 0
      Text = '1'
      OnChange = edtRadiusChange
    end
    object ggbrRadius: TGaugeBar
      Left = 16
      Top = 64
      Width = 209
      Height = 16
      Cursor = crHandPoint
      Backgnd = bgPattern
      Min = 1
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
    Top = 8
    Width = 75
    Height = 25
    Cursor = crHandPoint
    TabOrder = 1
    OnClick = btbtnOKClick
    Kind = bkOK
  end
  object btbtnCancel: TBitBtn
    Left = 264
    Top = 40
    Width = 75
    Height = 25
    Cursor = crHandPoint
    TabOrder = 2
    Kind = bkCancel
  end
  object chckbxPreview: TCheckBox
    Left = 264
    Top = 112
    Width = 73
    Height = 17
    Cursor = crHandPoint
    Caption = 'Preview'
    Checked = True
    State = cbChecked
    TabOrder = 3
    OnClick = chckbxPreviewClick
  end
  object btnAbout: TButton
    Left = 264
    Top = 72
    Width = 75
    Height = 25
    Cursor = crHandPoint
    Caption = 'About...'
    TabOrder = 4
    OnClick = btnAboutClick
  end
end
