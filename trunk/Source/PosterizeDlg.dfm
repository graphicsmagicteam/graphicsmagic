object frmPosterize: TfrmPosterize
  Left = 573
  Top = 126
  BorderStyle = bsDialog
  Caption = 'Posterize'
  ClientHeight = 115
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
  object lblLevel: TLabel
    Left = 16
    Top = 29
    Width = 29
    Height = 13
    Caption = 'Level:'
  end
  object edtLevel: TEdit
    Left = 54
    Top = 26
    Width = 41
    Height = 21
    TabOrder = 0
    Text = '255'
    OnChange = edtLevelChange
  end
  object btbtnOK: TBitBtn
    Left = 299
    Top = 10
    Width = 61
    Height = 25
    Cursor = crHandPoint
    TabOrder = 1
    OnClick = btbtnOKClick
    Kind = bkOK
  end
  object btbtnCancel: TBitBtn
    Left = 299
    Top = 47
    Width = 61
    Height = 25
    Cursor = crHandPoint
    TabOrder = 2
    Kind = bkCancel
  end
  object chckbxPreview: TCheckBox
    Left = 299
    Top = 88
    Width = 59
    Height = 13
    Cursor = crHandPoint
    Caption = 'Preview'
    Checked = True
    State = cbChecked
    TabOrder = 3
    OnClick = chckbxPreviewClick
  end
  object ggbrLevel: TGaugeBar
    Left = 16
    Top = 56
    Width = 256
    Height = 16
    Cursor = crHandPoint
    Backgnd = bgPattern
    Max = 255
    ShowHandleGrip = True
    Style = rbsMac
    Position = 0
    OnChange = ggbrLevelChange
    OnMouseUp = ggbrLevelMouseUp
  end
end
