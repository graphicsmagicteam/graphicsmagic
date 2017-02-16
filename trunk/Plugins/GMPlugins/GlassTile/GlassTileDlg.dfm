object frmGlassTile: TfrmGlassTile
  Left = 187
  Top = 126
  BorderStyle = bsDialog
  Caption = 'Glass Tile'
  ClientHeight = 172
  ClientWidth = 261
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
    Width = 161
    Height = 153
    Caption = 'Parameter Settings:'
    TabOrder = 0
    object lblTileWidth: TLabel
      Left = 16
      Top = 48
      Width = 51
      Height = 13
      Caption = 'Tile Width:'
    end
    object lblTileHeight: TLabel
      Left = 14
      Top = 96
      Width = 54
      Height = 13
      Caption = 'Tile Height:'
    end
    object edtTileWidth: TEdit
      Left = 72
      Top = 44
      Width = 50
      Height = 21
      TabOrder = 0
      Text = '20'
      OnChange = edtTileWidthChange
    end
    object edtTileHeight: TEdit
      Left = 72
      Top = 92
      Width = 50
      Height = 21
      TabOrder = 1
      Text = '20'
      OnChange = edtTileHeightChange
    end
    object updwnTileWidth: TUpDown
      Left = 122
      Top = 44
      Width = 20
      Height = 21
      Cursor = crHandPoint
      Associate = edtTileWidth
      Min = 10
      Max = 50
      Position = 20
      TabOrder = 2
      OnMouseDown = updwnTileWidthMouseDown
      OnMouseUp = updwnTileWidthMouseUp
    end
    object updwnTileHeight: TUpDown
      Left = 122
      Top = 92
      Width = 20
      Height = 21
      Cursor = crHandPoint
      Associate = edtTileHeight
      Min = 10
      Max = 50
      Position = 20
      TabOrder = 3
      OnMouseDown = updwnTileHeightMouseDown
      OnMouseUp = updwnTileHeightMouseUp
    end
  end
  object btbtnOK: TBitBtn
    Left = 176
    Top = 16
    Width = 75
    Height = 25
    Cursor = crHandPoint
    TabOrder = 1
    OnClick = btbtnOKClick
    Kind = bkOK
  end
  object btbtnCancel: TBitBtn
    Left = 176
    Top = 56
    Width = 75
    Height = 25
    Cursor = crHandPoint
    TabOrder = 2
    Kind = bkCancel
  end
  object chckbxPreview: TCheckBox
    Left = 176
    Top = 136
    Width = 70
    Height = 17
    Cursor = crHandPoint
    Caption = 'Preview'
    Checked = True
    State = cbChecked
    TabOrder = 3
    OnClick = chckbxPreviewClick
  end
  object btnAbout: TButton
    Left = 176
    Top = 96
    Width = 75
    Height = 25
    Cursor = crHandPoint
    Caption = 'About...'
    TabOrder = 4
    OnClick = btnAboutClick
  end
end
