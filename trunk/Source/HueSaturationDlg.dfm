object frmHueSaturation: TfrmHueSaturation
  Left = 188
  Top = 132
  BorderStyle = bsDialog
  Caption = 'Hue/Saturation'
  ClientHeight = 142
  ClientWidth = 383
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object btbtnOK: TBitBtn
    Left = 312
    Top = 13
    Width = 61
    Height = 20
    Cursor = crHandPoint
    TabOrder = 0
    OnClick = btbtnOKClick
    Kind = bkOK
  end
  object btbtnCancel: TBitBtn
    Left = 312
    Top = 46
    Width = 61
    Height = 20
    Cursor = crHandPoint
    TabOrder = 1
    Kind = bkCancel
  end
  object chckbxPreview: TCheckBox
    Left = 312
    Top = 111
    Width = 59
    Height = 13
    Cursor = crHandPoint
    Caption = 'Preview'
    Checked = True
    State = cbChecked
    TabOrder = 2
    OnClick = chckbxPreviewClick
  end
  object grpbxHueSaturation: TGroupBox
    Left = 8
    Top = 8
    Width = 297
    Height = 121
    TabOrder = 3
    object lblHue: TLabel
      Left = 13
      Top = 17
      Width = 23
      Height = 13
      Caption = 'Hue:'
    end
    object lblSaturation: TLabel
      Left = 13
      Top = 43
      Width = 51
      Height = 13
      Caption = 'Saturation:'
    end
    object lblLightnessOrValue: TLabel
      Left = 13
      Top = 69
      Width = 30
      Height = 13
      Caption = 'Value:'
    end
    object rdbtnHLS: TRadioButton
      Left = 70
      Top = 96
      Width = 49
      Height = 14
      Cursor = crHandPoint
      Caption = 'HSL'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = rdbtnHLSClick
    end
    object rdbtnHSV: TRadioButton
      Left = 181
      Top = 96
      Width = 48
      Height = 14
      Cursor = crHandPoint
      Caption = 'HSV'
      TabOrder = 1
      OnClick = rdbtnHSVClick
    end
    object edtLightnessOrValue: TEdit
      Left = 246
      Top = 66
      Width = 32
      Height = 21
      TabOrder = 2
      OnChange = edtLightnessOrValueChange
    end
    object edtSaturation: TEdit
      Left = 246
      Top = 40
      Width = 32
      Height = 21
      TabOrder = 3
      OnChange = edtSaturationChange
    end
    object edtHue: TEdit
      Left = 246
      Top = 14
      Width = 32
      Height = 21
      TabOrder = 4
      OnChange = edtHueChange
    end
    object ggbrHue: TGaugeBar
      Left = 72
      Top = 16
      Width = 169
      Height = 16
      Cursor = crHandPoint
      Backgnd = bgPattern
      Max = 360
      ShowHandleGrip = True
      Style = rbsMac
      Position = 0
      OnChange = AdjustHue
      OnMouseUp = ExecuteChanges
    end
    object ggbrSaturation: TGaugeBar
      Left = 72
      Top = 42
      Width = 169
      Height = 16
      Cursor = crHandPoint
      Backgnd = bgPattern
      Max = 256
      ShowHandleGrip = True
      Style = rbsMac
      Position = 0
      OnChange = AdjustSaturation
      OnMouseUp = ExecuteChanges
    end
    object ggbrLightnessOrValue: TGaugeBar
      Left = 72
      Top = 68
      Width = 169
      Height = 16
      Cursor = crHandPoint
      Backgnd = bgPattern
      Max = 256
      ShowHandleGrip = True
      Style = rbsMac
      Position = 0
      OnChange = AdjustLightnessOrValue
      OnMouseUp = ExecuteChanges
    end
  end
end
