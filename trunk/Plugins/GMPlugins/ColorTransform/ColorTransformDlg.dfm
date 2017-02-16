object frmColorTransform: TfrmColorTransform
  Left = 192
  Top = 133
  BorderStyle = bsDialog
  Caption = 'Color Transform'
  ClientHeight = 322
  ClientWidth = 263
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
  object btbtnOK: TBitBtn
    Left = 176
    Top = 13
    Width = 75
    Height = 25
    Cursor = crHandPoint
    TabOrder = 0
    OnClick = btbtnOKClick
    Kind = bkOK
  end
  object btbtnCancel: TBitBtn
    Left = 176
    Top = 46
    Width = 75
    Height = 25
    Cursor = crHandPoint
    TabOrder = 1
    Kind = bkCancel
  end
  object chckbxPreview: TCheckBox
    Left = 176
    Top = 157
    Width = 75
    Height = 14
    Cursor = crHandPoint
    Caption = 'Preview'
    Checked = True
    State = cbChecked
    TabOrder = 2
    OnClick = chckbxPreviewClick
  end
  object btnReset: TButton
    Left = 176
    Top = 78
    Width = 75
    Height = 25
    Cursor = crHandPoint
    Caption = 'Reset'
    TabOrder = 3
    OnClick = btnResetClick
  end
  object grpbxColorTransform: TGroupBox
    Left = 8
    Top = 8
    Width = 158
    Height = 305
    TabOrder = 4
    object lblRedToGreen: TLabel
      Left = 12
      Top = 20
      Width = 70
      Height = 13
      Caption = 'Red --> Green:'
    end
    object Label1: TLabel
      Left = 136
      Top = 20
      Width = 8
      Height = 13
      Caption = '%'
    end
    object lblRedToBlue: TLabel
      Left = 12
      Top = 65
      Width = 62
      Height = 13
      Caption = 'Red --> Blue:'
    end
    object Label2: TLabel
      Left = 136
      Top = 66
      Width = 8
      Height = 13
      Caption = '%'
    end
    object lblGreenToRed: TLabel
      Left = 12
      Top = 111
      Width = 70
      Height = 13
      Caption = 'Green --> Red:'
    end
    object Label3: TLabel
      Left = 136
      Top = 111
      Width = 8
      Height = 13
      Caption = '%'
    end
    object lblGreenToBlue: TLabel
      Left = 12
      Top = 156
      Width = 71
      Height = 13
      Caption = 'Green --> Blue:'
    end
    object Label4: TLabel
      Left = 136
      Top = 157
      Width = 8
      Height = 13
      Caption = '%'
    end
    object lblBlueToRed: TLabel
      Left = 12
      Top = 202
      Width = 62
      Height = 13
      Caption = 'Blue --> Red:'
    end
    object Label5: TLabel
      Left = 136
      Top = 202
      Width = 8
      Height = 13
      Caption = '%'
    end
    object lblBlueToGreen: TLabel
      Left = 12
      Top = 247
      Width = 71
      Height = 13
      Caption = 'Blue --> Green:'
    end
    object Label6: TLabel
      Left = 136
      Top = 248
      Width = 8
      Height = 13
      Caption = '%'
    end
    object edtRedToGreen: TEdit
      Left = 93
      Top = 16
      Width = 33
      Height = 21
      TabOrder = 0
      Text = '0'
      OnChange = EditChange
    end
    object edtRedToBlue: TEdit
      Left = 93
      Top = 62
      Width = 33
      Height = 21
      TabOrder = 1
      Text = '0'
      OnChange = EditChange
    end
    object edtGreenToRed: TEdit
      Left = 93
      Top = 107
      Width = 33
      Height = 21
      TabOrder = 2
      Text = '0'
      OnChange = EditChange
    end
    object edtGreenToBlue: TEdit
      Left = 93
      Top = 153
      Width = 33
      Height = 21
      TabOrder = 3
      Text = '0'
      OnChange = EditChange
    end
    object edtBlueToRed: TEdit
      Left = 93
      Top = 198
      Width = 33
      Height = 21
      TabOrder = 4
      Text = '0'
      OnChange = EditChange
    end
    object edtBlueToGreen: TEdit
      Left = 93
      Top = 244
      Width = 33
      Height = 21
      TabOrder = 5
      Text = '0'
      OnChange = EditChange
    end
    object ggbrRedToGreen: TGaugeBar
      Left = 12
      Top = 42
      Width = 134
      Height = 16
      Cursor = crHandPoint
      Backgnd = bgPattern
      ShowHandleGrip = True
      Style = rbsMac
      Position = 0
      OnChange = GaugeBarChange
      OnMouseDown = GaugeBarMouseDown
      OnMouseUp = GaugeBarMouseUp
    end
    object ggbrRedToBlue: TGaugeBar
      Left = 12
      Top = 87
      Width = 134
      Height = 16
      Cursor = crHandPoint
      Backgnd = bgPattern
      ShowHandleGrip = True
      Style = rbsMac
      Position = 0
      OnChange = GaugeBarChange
      OnMouseDown = GaugeBarMouseDown
      OnMouseUp = GaugeBarMouseUp
    end
    object ggbrGreenToRed: TGaugeBar
      Left = 12
      Top = 133
      Width = 134
      Height = 16
      Cursor = crHandPoint
      Backgnd = bgPattern
      ShowHandleGrip = True
      Style = rbsMac
      Position = 0
      OnChange = GaugeBarChange
      OnMouseDown = GaugeBarMouseDown
      OnMouseUp = GaugeBarMouseUp
    end
    object ggbrGreenToBlue: TGaugeBar
      Left = 12
      Top = 178
      Width = 134
      Height = 16
      Cursor = crHandPoint
      Backgnd = bgPattern
      ShowHandleGrip = True
      Style = rbsMac
      Position = 0
      OnChange = GaugeBarChange
      OnMouseDown = GaugeBarMouseDown
      OnMouseUp = GaugeBarMouseUp
    end
    object ggbrBlueToRed: TGaugeBar
      Left = 12
      Top = 224
      Width = 134
      Height = 16
      Cursor = crHandPoint
      Backgnd = bgPattern
      ShowHandleGrip = True
      Style = rbsMac
      Position = 0
      OnChange = GaugeBarChange
      OnMouseDown = GaugeBarMouseDown
      OnMouseUp = GaugeBarMouseUp
    end
    object ggbrBlueToGreen: TGaugeBar
      Left = 12
      Top = 270
      Width = 134
      Height = 16
      Cursor = crHandPoint
      Backgnd = bgPattern
      ShowHandleGrip = True
      Style = rbsMac
      Position = 0
      OnChange = GaugeBarChange
      OnMouseDown = GaugeBarMouseDown
      OnMouseUp = GaugeBarMouseUp
    end
  end
  object btnAbout: TButton
    Left = 176
    Top = 112
    Width = 75
    Height = 25
    Cursor = crHandPoint
    Caption = 'About...'
    TabOrder = 5
    OnClick = btnAboutClick
  end
end
