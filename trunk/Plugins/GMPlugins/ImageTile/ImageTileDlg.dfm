object frmImageTile: TfrmImageTile
  Left = 191
  Top = 131
  BorderStyle = bsDialog
  Caption = 'Image Tile'
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
    object lblAmount: TLabel
      Left = 56
      Top = 24
      Width = 39
      Height = 13
      Caption = 'Amount:'
    end
    object Label1: TLabel
      Left = 168
      Top = 24
      Width = 8
      Height = 13
      Caption = '%'
    end
    object edtAmount: TEdit
      Left = 112
      Top = 20
      Width = 50
      Height = 21
      TabOrder = 0
      Text = '1'
      OnChange = edtAmountChange
    end
    object ggbrAmount: TGaugeBar
      Left = 16
      Top = 56
      Width = 209
      Height = 16
      Cursor = crHandPoint
      Backgnd = bgPattern
      Min = 1
      ShowHandleGrip = True
      Style = rbsMac
      Position = 1
      OnChange = ggbrAmountChange
      OnMouseDown = ggbrAmountMouseDown
      OnMouseUp = ggbrAmountMouseUp
    end
    object chckbxOverlapped: TCheckBox
      Left = 16
      Top = 88
      Width = 97
      Height = 17
      Cursor = crHandPoint
      Caption = 'Overlapped'
      TabOrder = 2
      OnClick = chckbxOverlappedClick
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
    Top = 112
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
