object frmSepia: TfrmSepia
  Left = 191
  Top = 133
  BorderStyle = bsDialog
  Caption = 'Sepia'
  ClientHeight = 169
  ClientWidth = 318
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
    Width = 217
    Height = 153
    TabOrder = 0
    object lblDepth: TLabel
      Left = 40
      Top = 40
      Width = 32
      Height = 13
      Caption = 'Depth:'
    end
    object lblLevels: TLabel
      Left = 144
      Top = 40
      Width = 31
      Height = 13
      Caption = 'Levels'
    end
    object edtDepth: TEdit
      Left = 80
      Top = 36
      Width = 50
      Height = 21
      TabOrder = 0
      OnChange = edtDepthChange
    end
    object ggbrDepth: TGaugeBar
      Left = 16
      Top = 94
      Width = 185
      Height = 16
      Cursor = crHandPoint
      Backgnd = bgPattern
      ShowHandleGrip = True
      Style = rbsMac
      Position = 0
      OnChange = ggbrDepthChange
      OnMouseDown = ggbrDepthMouseDown
      OnMouseUp = ggbrDepthMouseUp
    end
  end
  object btbtnOK: TBitBtn
    Left = 232
    Top = 16
    Width = 75
    Height = 25
    Cursor = crHandPoint
    TabOrder = 1
    OnClick = btbtnOKClick
    Kind = bkOK
  end
  object btbtnCancel: TBitBtn
    Left = 232
    Top = 56
    Width = 75
    Height = 25
    Cursor = crHandPoint
    TabOrder = 2
    Kind = bkCancel
  end
  object chckbxPreview: TCheckBox
    Left = 232
    Top = 136
    Width = 75
    Height = 17
    Cursor = crHandPoint
    Caption = 'Preview'
    Checked = True
    State = cbChecked
    TabOrder = 3
    OnClick = chckbxPreviewClick
  end
  object btnAboutSepia: TButton
    Left = 232
    Top = 96
    Width = 75
    Height = 25
    Cursor = crHandPoint
    Caption = 'About...'
    TabOrder = 4
    OnClick = btnAboutSepiaClick
  end
end
