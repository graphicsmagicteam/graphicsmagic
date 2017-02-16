object frmBrightnessContrast: TfrmBrightnessContrast
  Left = 191
  Top = 130
  BorderStyle = bsDialog
  Caption = 'Brightness / Contrast'
  ClientHeight = 141
  ClientWidth = 279
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
  object btbtnOK: TBitBtn
    Left = 208
    Top = 13
    Width = 60
    Height = 20
    Cursor = crHandPoint
    TabOrder = 0
    OnClick = btbtnOKClick
    Kind = bkOK
  end
  object btbtnCancel: TBitBtn
    Left = 208
    Top = 46
    Width = 60
    Height = 20
    Cursor = crHandPoint
    TabOrder = 1
    Kind = bkCancel
  end
  object chckbxPreview: TCheckBox
    Left = 208
    Top = 107
    Width = 61
    Height = 14
    Cursor = crHandPoint
    Caption = 'Preview'
    Checked = True
    State = cbChecked
    TabOrder = 2
    OnClick = chckbxPreviewClick
  end
  object grpbxBrightnessContrast: TGroupBox
    Left = 8
    Top = 8
    Width = 193
    Height = 121
    TabOrder = 3
    object lblBrightness: TLabel
      Left = 12
      Top = 15
      Width = 52
      Height = 13
      Caption = 'Brightness:'
    end
    object lblContrast: TLabel
      Left = 12
      Top = 68
      Width = 42
      Height = 13
      Caption = 'Contrast:'
    end
    object edtBrightness: TEdit
      Left = 116
      Top = 12
      Width = 52
      Height = 21
      TabOrder = 0
      Text = '0'
      OnChange = edtBrightnessChange
    end
    object edtContrast: TEdit
      Left = 116
      Top = 65
      Width = 52
      Height = 21
      TabOrder = 1
      Text = '0'
      OnChange = edtContrastChange
    end
    object ggbrBrightness: TGaugeBar
      Left = 12
      Top = 40
      Width = 168
      Height = 16
      Cursor = crHandPoint
      Backgnd = bgPattern
      Max = 200
      ShowHandleGrip = True
      Style = rbsMac
      Position = 0
      OnChange = AdjustBrightness
      OnMouseUp = ggbrBrightnessMouseUp
    end
    object ggbrContrast: TGaugeBar
      Left = 12
      Top = 96
      Width = 168
      Height = 16
      Cursor = crHandPoint
      Backgnd = bgPattern
      Max = 200
      ShowHandleGrip = True
      Style = rbsMac
      Position = 0
      OnChange = AdjustContrast
      OnMouseUp = ggbrContrastMouseUp
    end
  end
end
