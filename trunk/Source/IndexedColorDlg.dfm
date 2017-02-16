object frmIndexedColor: TfrmIndexedColor
  Left = 210
  Top = 147
  BorderStyle = bsDialog
  Caption = 'Indexed Color'
  ClientHeight = 161
  ClientWidth = 279
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object grpbxOptions: TGroupBox
    Left = 7
    Top = 91
    Width = 189
    Height = 59
    Caption = 'Option:'
    TabOrder = 3
    object lblDither: TLabel
      Left = 13
      Top = 26
      Width = 31
      Height = 13
      Caption = 'Dither:'
    end
    object cmbbxDitherMode: TComboBox
      Left = 52
      Top = 23
      Width = 118
      Height = 21
      Cursor = crHandPoint
      ItemHeight = 13
      TabOrder = 0
      OnChange = cmbbxDitherModeChange
    end
  end
  object grpbxPalette: TGroupBox
    Left = 7
    Top = 7
    Width = 189
    Height = 78
    Caption = 'Palette:'
    TabOrder = 2
    object lblColorReduction: TLabel
      Left = 13
      Top = 26
      Width = 36
      Height = 13
      Caption = 'Palette:'
    end
    object lblPaletteColors: TLabel
      Left = 13
      Top = 52
      Width = 32
      Height = 13
      Caption = 'Colors:'
    end
    object cmbbxColorReduction: TComboBox
      Left = 59
      Top = 23
      Width = 117
      Height = 21
      Cursor = crHandPoint
      ItemHeight = 13
      TabOrder = 0
      OnChange = cmbbxColorReductionChange
    end
    object edtPaletteColors: TEdit
      Left = 59
      Top = 49
      Width = 65
      Height = 21
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 1
    end
  end
  object btbtnOK: TBitBtn
    Left = 208
    Top = 13
    Width = 61
    Height = 20
    Cursor = crHandPoint
    TabOrder = 0
    OnClick = btbtnOKClick
    Kind = bkOK
  end
  object btbtnCancel: TBitBtn
    Left = 208
    Top = 46
    Width = 61
    Height = 20
    Cursor = crHandPoint
    TabOrder = 1
    OnClick = btbtnCancelClick
    Kind = bkCancel
  end
  object chckbxPreview: TCheckBox
    Left = 208
    Top = 85
    Width = 59
    Height = 13
    Cursor = crHandPoint
    Caption = 'Preview'
    Checked = True
    State = cbChecked
    TabOrder = 4
    OnClick = chckbxPreviewClick
  end
end
