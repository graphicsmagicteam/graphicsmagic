object frmSplitColor: TfrmSplitColor
  Left = 191
  Top = 130
  BorderStyle = bsDialog
  Caption = 'Split Color'
  ClientHeight = 164
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
    Height = 145
    TabOrder = 0
    object lblLightness: TLabel
      Left = 40
      Top = 72
      Width = 48
      Height = 13
      Caption = 'Lightness:'
    end
    object Label1: TLabel
      Left = 160
      Top = 72
      Width = 31
      Height = 13
      Caption = 'Levels'
    end
    object lblColorChannel: TLabel
      Left = 24
      Top = 32
      Width = 69
      Height = 13
      Caption = 'Color Channel:'
    end
    object edtLightness: TEdit
      Left = 104
      Top = 68
      Width = 50
      Height = 21
      TabOrder = 0
      Text = '0'
      OnChange = edtLightnessChange
      OnExit = edtLightnessExit
    end
    object ggbrLightness: TGaugeBar
      Left = 16
      Top = 104
      Width = 209
      Height = 16
      Cursor = crHandPoint
      Backgnd = bgPattern
      Max = 200
      ShowHandleGrip = True
      Style = rbsMac
      Position = 1
      OnChange = ggbrLightnessChange
      OnMouseDown = ggbrLightnessMouseDown
      OnMouseUp = ggbrLightnessMouseUp
    end
    object cmbbxColorChannel: TComboBox
      Left = 104
      Top = 28
      Width = 105
      Height = 21
      Cursor = crHandPoint
      ItemHeight = 13
      TabOrder = 2
      OnChange = cmbbxColorChannelChange
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
