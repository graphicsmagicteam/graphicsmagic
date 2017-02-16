object frmPrintOptions: TfrmPrintOptions
  Left = 194
  Top = 130
  BorderStyle = bsDialog
  Caption = 'Print Options'
  ClientHeight = 286
  ClientWidth = 557
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
  object pnlPrintOptionsPreview: TPanel
    Left = 0
    Top = 0
    Width = 222
    Height = 286
    Align = alLeft
    BevelInner = bvLowered
    TabOrder = 0
    object imgPrintOptionsPreview: TImage32
      Left = 15
      Top = 15
      Width = 192
      Height = 256
      Bitmap.ResamplerClassName = 'TNearestResampler'
      BitmapAlign = baTopLeft
      Scale = 1.000000000000000000
      ScaleMode = smStretch
      TabOrder = 0
    end
  end
  object grpbxPrintPosition: TGroupBox
    Left = 232
    Top = 7
    Width = 228
    Height = 106
    Caption = 'Position:'
    TabOrder = 1
    object lblTopPos: TLabel
      Left = 11
      Top = 29
      Width = 22
      Height = 13
      Caption = 'Top:'
    end
    object lblLeftPos: TLabel
      Left = 13
      Top = 55
      Width = 21
      Height = 13
      Caption = 'Left:'
    end
    object edtTopPos: TEdit
      Left = 39
      Top = 26
      Width = 66
      Height = 21
      TabOrder = 0
      OnChange = edtTopPosChange
      OnExit = edtTopPosExit
    end
    object edtLeftPos: TEdit
      Left = 39
      Top = 52
      Width = 66
      Height = 21
      TabOrder = 1
      OnChange = edtLeftPosChange
      OnExit = edtLeftPosExit
    end
    object cmbbxTopPosUnit: TComboBox
      Left = 111
      Top = 26
      Width = 104
      Height = 21
      Cursor = crHandPoint
      ItemHeight = 13
      TabOrder = 2
      OnChange = cmbbxTopPosUnitChange
      Items.Strings = (
        'inches'
        'cm'
        'points'
        'pixels')
    end
    object cmbbxLeftPosUnit: TComboBox
      Left = 111
      Top = 52
      Width = 104
      Height = 21
      Cursor = crHandPoint
      ItemHeight = 13
      TabOrder = 3
      OnChange = cmbbxLeftPosUnitChange
      Items.Strings = (
        'inches'
        'cm'
        'points'
        'pixels')
    end
    object chckbxCenterImage: TCheckBox
      Left = 39
      Top = 78
      Width = 89
      Height = 14
      Cursor = crHandPoint
      Caption = 'Center Image'
      Checked = True
      State = cbChecked
      TabOrder = 4
      OnClick = chckbxCenterImageClick
    end
  end
  object grpbxPrintScaled: TGroupBox
    Left = 232
    Top = 124
    Width = 228
    Height = 105
    Caption = 'Scaled Print Size:'
    TabOrder = 2
    object lblPrintScale: TLabel
      Left = 13
      Top = 26
      Width = 30
      Height = 13
      Caption = 'Scale:'
    end
    object lblPrintHeight: TLabel
      Left = 10
      Top = 46
      Width = 34
      Height = 13
      Caption = 'Height:'
    end
    object lblPrintWidth: TLabel
      Left = 14
      Top = 72
      Width = 31
      Height = 13
      Caption = 'Width:'
    end
    object lblScalePercent: TLabel
      Left = 101
      Top = 26
      Width = 8
      Height = 13
      Caption = '%'
    end
    object edtPrintScale: TEdit
      Left = 46
      Top = 23
      Width = 51
      Height = 21
      TabOrder = 0
      OnChange = edtPrintScaleChange
    end
    object edtPrintHeight: TEdit
      Left = 46
      Top = 46
      Width = 63
      Height = 21
      TabOrder = 1
      OnChange = edtPrintHeightChange
      OnExit = edtPrintHeightExit
    end
    object edtPrintWidth: TEdit
      Left = 46
      Top = 72
      Width = 63
      Height = 21
      TabOrder = 2
      OnChange = edtPrintWidthChange
      OnExit = edtPrintWidthExit
    end
    object cmbbxPrintWidthUnit: TComboBox
      Left = 117
      Top = 72
      Width = 104
      Height = 21
      Cursor = crHandPoint
      ItemHeight = 13
      TabOrder = 3
      OnChange = cmbbxPrintWidthUnitChange
      Items.Strings = (
        'inches'
        'cm'
        'points'
        'pixels')
    end
    object cmbbxPrintHeightUnit: TComboBox
      Left = 117
      Top = 46
      Width = 104
      Height = 21
      Cursor = crHandPoint
      ItemHeight = 13
      TabOrder = 4
      OnChange = cmbbxPrintHeightUnitChange
      Items.Strings = (
        'inches'
        'cm'
        'points'
        'pixels')
    end
    object chckbxFitMedia: TCheckBox
      Left = 117
      Top = 24
      Width = 105
      Height = 14
      Cursor = crHandPoint
      Caption = 'Scale to fit media'
      TabOrder = 5
      OnClick = chckbxFitMediaClick
    end
  end
  object btbtnOK: TBitBtn
    Left = 472
    Top = 13
    Width = 75
    Height = 25
    Cursor = crHandPoint
    TabOrder = 3
    OnClick = btbtnOKClick
    Kind = bkOK
  end
  object btbtnCancel: TBitBtn
    Left = 472
    Top = 45
    Width = 75
    Height = 25
    Cursor = crHandPoint
    TabOrder = 4
    Kind = bkCancel
  end
  object btnPageSetup: TButton
    Left = 472
    Top = 109
    Width = 75
    Height = 25
    Cursor = crHandPoint
    Caption = 'Page Setup...'
    TabOrder = 5
    OnClick = btnPageSetupClick
  end
  object btnPrintImage: TButton
    Left = 472
    Top = 77
    Width = 75
    Height = 25
    Cursor = crHandPoint
    Caption = 'Print...'
    TabOrder = 6
    OnClick = btnPrintImageClick
  end
end
