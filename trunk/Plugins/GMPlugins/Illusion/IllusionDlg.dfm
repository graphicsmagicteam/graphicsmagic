object frmIllusion: TfrmIllusion
  Left = 190
  Top = 134
  BorderStyle = bsDialog
  Caption = 'Illusion'
  ClientHeight = 142
  ClientWidth = 231
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
  object grpbxIllusion: TGroupBox
    Left = 7
    Top = 7
    Width = 124
    Height = 124
    Caption = 'Parameter Settings:'
    TabOrder = 0
    object lblDivision: TLabel
      Left = 13
      Top = 33
      Width = 40
      Height = 13
      Caption = 'Division:'
    end
    object edtDivision: TEdit
      Left = 59
      Top = 29
      Width = 40
      Height = 21
      TabOrder = 0
      Text = '8'
      OnChange = edtDivisionChange
    end
    object updwnDivision: TUpDown
      Left = 99
      Top = 29
      Width = 14
      Height = 21
      Cursor = crHandPoint
      Associate = edtDivision
      Min = -32
      Max = 64
      Position = 8
      TabOrder = 1
      OnMouseDown = updwnDivisionMouseDown
      OnMouseUp = updwnDivisionMouseUp
    end
    object rdbtnIllusionType1: TRadioButton
      Left = 13
      Top = 65
      Width = 92
      Height = 14
      Cursor = crHandPoint
      Caption = 'Type1'
      Checked = True
      TabOrder = 2
      TabStop = True
      OnClick = ChangeIllusioinType
    end
    object rdbtnIllusionType2: TRadioButton
      Left = 13
      Top = 92
      Width = 92
      Height = 14
      Cursor = crHandPoint
      Caption = 'Type2'
      TabOrder = 3
      OnClick = ChangeIllusioinType
    end
  end
  object btbtnOK: TBitBtn
    Left = 143
    Top = 13
    Width = 75
    Height = 25
    Cursor = crHandPoint
    TabOrder = 1
    OnClick = btbtnOKClick
    Kind = bkOK
  end
  object btbtnCancel: TBitBtn
    Left = 143
    Top = 46
    Width = 75
    Height = 25
    Cursor = crHandPoint
    TabOrder = 2
    Kind = bkCancel
  end
  object chckbxPreview: TCheckBox
    Left = 143
    Top = 117
    Width = 59
    Height = 13
    Cursor = crHandPoint
    Caption = 'Preview'
    Checked = True
    State = cbChecked
    TabOrder = 3
    OnClick = chckbxPreviewClick
  end
  object btnAbout: TButton
    Left = 144
    Top = 80
    Width = 75
    Height = 25
    Cursor = crHandPoint
    Caption = 'About...'
    TabOrder = 4
    OnClick = btnAboutClick
  end
end
