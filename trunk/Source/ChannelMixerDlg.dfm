object frmChannelMixer: TfrmChannelMixer
  Left = 190
  Top = 129
  BorderStyle = bsDialog
  Caption = 'Channel Mixer'
  ClientHeight = 314
  ClientWidth = 335
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lblOutputChannel: TLabel
    Left = 8
    Top = 16
    Width = 77
    Height = 13
    Caption = 'Output Channel:'
  end
  object lblConstant: TLabel
    Left = 16
    Top = 236
    Width = 45
    Height = 13
    Caption = 'Constant:'
  end
  object lblConstantPercent: TLabel
    Left = 208
    Top = 236
    Width = 8
    Height = 13
    Caption = '%'
  end
  object cmbbxOutputChannel: TComboBox
    Left = 88
    Top = 12
    Width = 145
    Height = 21
    Cursor = crHandPoint
    ItemHeight = 13
    TabOrder = 0
    OnChange = ChangeOutputChannel
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 40
    Width = 225
    Height = 177
    Caption = 'Source Channels:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clActiveCaption
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    object lblRed: TLabel
      Left = 8
      Top = 28
      Width = 23
      Height = 13
      Caption = 'Red:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object lblRedPercent: TLabel
      Left = 200
      Top = 28
      Width = 8
      Height = 13
      Caption = '%'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object lblGreen: TLabel
      Left = 8
      Top = 76
      Width = 32
      Height = 13
      Caption = 'Green:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object lblGreenPercent: TLabel
      Left = 200
      Top = 76
      Width = 8
      Height = 13
      Caption = '%'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object lblBlue: TLabel
      Left = 8
      Top = 124
      Width = 24
      Height = 13
      Caption = 'Blue:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object lblBluePercent: TLabel
      Left = 200
      Top = 124
      Width = 8
      Height = 13
      Caption = '%'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object edtRedScale: TEdit
      Left = 144
      Top = 24
      Width = 50
      Height = 21
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      OnChange = edtRedScaleChange
      OnExit = edtRedScaleExit
    end
    object edtGreenScale: TEdit
      Left = 144
      Top = 72
      Width = 50
      Height = 21
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      OnChange = edtGreenScaleChange
      OnExit = edtGreenScaleExit
    end
    object edtBlueScale: TEdit
      Left = 144
      Top = 120
      Width = 50
      Height = 21
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      OnChange = edtBlueScaleChange
      OnExit = edtBlueScaleExit
    end
    object ggbrRedScale: TGaugeBar
      Left = 8
      Top = 48
      Width = 200
      Height = 16
      Cursor = crHandPoint
      Backgnd = bgPattern
      Max = 400
      ShowHandleGrip = True
      Style = rbsMac
      Position = 0
      OnChange = ggbrRedScaleChange
      OnMouseUp = ggbrRedScaleMouseUp
    end
    object ggbrGreenScale: TGaugeBar
      Left = 8
      Top = 96
      Width = 200
      Height = 16
      Cursor = crHandPoint
      Backgnd = bgPattern
      Max = 400
      ShowHandleGrip = True
      Style = rbsMac
      Position = 0
      OnChange = ggbrGreenScaleChange
      OnMouseUp = ggbrGreenScaleMouseUp
    end
    object ggbrBlueScale: TGaugeBar
      Left = 8
      Top = 144
      Width = 200
      Height = 16
      Cursor = crHandPoint
      Backgnd = bgPattern
      Max = 400
      ShowHandleGrip = True
      Style = rbsMac
      Position = 0
      OnChange = ggbrBlueScaleChange
      OnMouseUp = ggbrBlueScaleMouseUp
    end
  end
  object edtConstantScale: TEdit
    Left = 152
    Top = 232
    Width = 50
    Height = 21
    TabOrder = 2
    OnChange = edtConstantScaleChange
    OnExit = edtConstantScaleExit
  end
  object chckbxMonochrome: TCheckBox
    Left = 16
    Top = 288
    Width = 97
    Height = 17
    Cursor = crHandPoint
    Caption = 'Monochrome'
    TabOrder = 3
    OnClick = chckbxMonochromeClick
  end
  object ggbrConstantScale: TGaugeBar
    Left = 16
    Top = 256
    Width = 200
    Height = 16
    Cursor = crHandPoint
    Backgnd = bgPattern
    Max = 400
    ShowHandleGrip = True
    Style = rbsMac
    Position = 0
    OnChange = ggbrConstantScaleChange
    OnMouseUp = ggbrConstantScaleMouseUp
  end
  object btbtnOK: TBitBtn
    Left = 248
    Top = 10
    Width = 75
    Height = 25
    Cursor = crHandPoint
    TabOrder = 5
    OnClick = btbtnOKClick
    Kind = bkOK
  end
  object btbtnCancel: TBitBtn
    Left = 248
    Top = 48
    Width = 75
    Height = 25
    Cursor = crHandPoint
    TabOrder = 6
    Kind = bkCancel
  end
  object chckbxPreview: TCheckBox
    Left = 248
    Top = 128
    Width = 75
    Height = 17
    Cursor = crHandPoint
    Caption = 'Preview'
    Checked = True
    State = cbChecked
    TabOrder = 7
    OnClick = chckbxPreviewClick
  end
  object btnRestore: TButton
    Left = 248
    Top = 88
    Width = 75
    Height = 25
    Cursor = crHandPoint
    Caption = 'Restore'
    TabOrder = 8
    OnClick = btnRestoreClick
  end
end
