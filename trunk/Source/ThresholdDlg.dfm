object frmThreshold: TfrmThreshold
  Left = 190
  Top = 133
  BorderStyle = bsDialog
  Caption = 'Threshold'
  ClientHeight = 239
  ClientWidth = 370
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
  object grpbxThreshold: TGroupBox
    Left = 7
    Top = 7
    Width = 281
    Height = 221
    TabOrder = 0
    object lblThresholdLevels: TLabel
      Left = 65
      Top = 20
      Width = 79
      Height = 13
      Caption = 'Threshold Level:'
    end
    object imgThresholdHistogram: TImage
      Left = 13
      Top = 46
      Width = 256
      Height = 139
    end
    object edtLevel: TEdit
      Left = 156
      Top = 16
      Width = 41
      Height = 21
      TabOrder = 0
      Text = '127'
      OnChange = edtLevelChange
    end
    object ggbrLevel: TGaugeBar
      Left = 13
      Top = 192
      Width = 256
      Height = 16
      Cursor = crHandPoint
      Backgnd = bgPattern
      Max = 255
      Min = 1
      ShowHandleGrip = True
      Style = rbsMac
      Position = 127
      OnChange = ggbrLevelChange
      OnMouseUp = ggbrLevelMouseUp
    end
  end
  object btbtnOK: TBitBtn
    Left = 299
    Top = 13
    Width = 61
    Height = 20
    Cursor = crHandPoint
    TabOrder = 1
    OnClick = btbtnOKClick
    Kind = bkOK
  end
  object btbtnCancel: TBitBtn
    Left = 299
    Top = 46
    Width = 61
    Height = 20
    Cursor = crHandPoint
    TabOrder = 2
    Kind = bkCancel
  end
  object chckbxPreview: TCheckBox
    Left = 299
    Top = 91
    Width = 61
    Height = 14
    Cursor = crHandPoint
    Caption = 'Preview'
    Checked = True
    State = cbChecked
    TabOrder = 3
    OnClick = chckbxPreviewClick
  end
end
