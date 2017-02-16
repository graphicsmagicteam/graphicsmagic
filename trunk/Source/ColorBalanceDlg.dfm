object frmColorBalance: TfrmColorBalance
  Left = 193
  Top = 130
  BorderStyle = bsDialog
  Caption = 'Color Balance'
  ClientHeight = 204
  ClientWidth = 376
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object grpbxColorBalance: TGroupBox
    Left = 7
    Top = 7
    Width = 286
    Height = 122
    Caption = 'Color Balance:'
    TabOrder = 0
    object lblColorLevels: TLabel
      Left = 26
      Top = 26
      Width = 61
      Height = 13
      Caption = 'Color Levels:'
    end
    object lblCyan: TLabel
      Left = 33
      Top = 55
      Width = 24
      Height = 13
      Caption = 'Cyan'
    end
    object lblRed: TLabel
      Left = 239
      Top = 55
      Width = 20
      Height = 13
      Caption = 'Red'
    end
    object lblMagenta: TLabel
      Left = 15
      Top = 75
      Width = 42
      Height = 13
      Caption = 'Magenta'
    end
    object lblYellow: TLabel
      Left = 26
      Top = 94
      Width = 31
      Height = 13
      Caption = 'Yellow'
    end
    object lblGreen: TLabel
      Left = 239
      Top = 75
      Width = 29
      Height = 13
      Caption = 'Green'
    end
    object lblBlue: TLabel
      Left = 239
      Top = 94
      Width = 21
      Height = 13
      Caption = 'Blue'
    end
    object edtCyanToRed: TEdit
      Left = 104
      Top = 23
      Width = 41
      Height = 21
      TabOrder = 0
      OnChange = edtCyanToRedChange
    end
    object edtMagentaToGreen: TEdit
      Left = 156
      Top = 23
      Width = 41
      Height = 21
      TabOrder = 1
      OnChange = edtMagentaToGreenChange
    end
    object edtYellowToBlue: TEdit
      Left = 208
      Top = 23
      Width = 41
      Height = 21
      TabOrder = 2
      OnChange = edtYellowToBlueChange
    end
    object ggbrYellowToBlue: TGaugeBar
      Left = 64
      Top = 94
      Width = 168
      Height = 16
      Cursor = crHandPoint
      Backgnd = bgPattern
      Max = 200
      ShowHandleGrip = True
      Style = rbsMac
      Position = 0
      OnChange = AdjustColorBalance
      OnMouseUp = ColorBalanceBarMouseUp
    end
    object ggbrCyanToRed: TGaugeBar
      Left = 64
      Top = 54
      Width = 168
      Height = 16
      Cursor = crHandPoint
      Backgnd = bgPattern
      Max = 200
      ShowHandleGrip = True
      Style = rbsMac
      Position = 0
      OnChange = AdjustColorBalance
      OnMouseUp = ColorBalanceBarMouseUp
    end
    object ggbrMagentaToGreen: TGaugeBar
      Left = 64
      Top = 74
      Width = 168
      Height = 16
      Cursor = crHandPoint
      Backgnd = bgPattern
      Max = 200
      ShowHandleGrip = True
      Style = rbsMac
      Position = 0
      OnChange = AdjustColorBalance
      OnMouseUp = ColorBalanceBarMouseUp
    end
  end
  object grpbxToneBalance: TGroupBox
    Left = 7
    Top = 138
    Width = 286
    Height = 59
    Caption = 'Tone Balance:'
    TabOrder = 1
    object rdbtnShadows: TRadioButton
      Left = 13
      Top = 20
      Width = 73
      Height = 13
      Cursor = crHandPoint
      Caption = 'Shadows'
      TabOrder = 0
      OnClick = ChangeTransferMode
    end
    object rdbtnMidtones: TRadioButton
      Left = 98
      Top = 20
      Width = 73
      Height = 13
      Cursor = crHandPoint
      Caption = 'Midtones'
      Checked = True
      TabOrder = 1
      TabStop = True
      OnClick = ChangeTransferMode
    end
    object rdbtnHighlights: TRadioButton
      Left = 169
      Top = 20
      Width = 73
      Height = 13
      Cursor = crHandPoint
      Caption = 'Highlights'
      TabOrder = 2
      OnClick = ChangeTransferMode
    end
    object chckbxPreserveLuminosity: TCheckBox
      Left = 13
      Top = 39
      Width = 122
      Height = 14
      Cursor = crHandPoint
      Caption = 'Preserve Luminosity'
      Checked = True
      State = cbChecked
      TabOrder = 3
      OnClick = chckbxPreserveLuminosityClick
    end
  end
  object btbtnOK: TBitBtn
    Left = 306
    Top = 20
    Width = 60
    Height = 20
    Cursor = crHandPoint
    TabOrder = 2
    OnClick = btbtnOKClick
    Kind = bkOK
  end
  object btbtnCancel: TBitBtn
    Left = 306
    Top = 52
    Width = 60
    Height = 20
    Cursor = crHandPoint
    TabOrder = 3
    Kind = bkCancel
  end
  object btnReset: TButton
    Left = 306
    Top = 85
    Width = 60
    Height = 20
    Cursor = crHandPoint
    Caption = 'Reset'
    TabOrder = 4
    OnClick = btnResetClick
  end
  object chckbxPreview: TCheckBox
    Left = 306
    Top = 169
    Width = 59
    Height = 14
    Cursor = crHandPoint
    Caption = 'Preview'
    Checked = True
    State = cbChecked
    TabOrder = 5
    OnClick = chckbxPreviewClick
  end
end
