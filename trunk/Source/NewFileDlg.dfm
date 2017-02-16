object frmCreateNewFile: TfrmCreateNewFile
  Left = 193
  Top = 129
  BorderStyle = bsDialog
  Caption = 'New'
  ClientHeight = 122
  ClientWidth = 331
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
  object grpbxBitmapDimension: TGroupBox
    Left = 7
    Top = 7
    Width = 241
    Height = 111
    Caption = 'Bitmap Dimension:'
    TabOrder = 0
    object lblBitmapWidth: TLabel
      Left = 33
      Top = 26
      Width = 31
      Height = 13
      Caption = 'Width:'
    end
    object lblBitmapHeight: TLabel
      Left = 26
      Top = 52
      Width = 34
      Height = 13
      Caption = 'Height:'
    end
    object lblResolution: TLabel
      Left = 7
      Top = 78
      Width = 53
      Height = 13
      Caption = 'Resolution:'
    end
    object edtBitmapWidth: TEdit
      Left = 65
      Top = 26
      Width = 79
      Height = 21
      TabOrder = 0
      OnChange = edtBitmapWidthChange
      OnExit = edtBitmapWidthExit
    end
    object edtBitmapHeight: TEdit
      Left = 65
      Top = 52
      Width = 79
      Height = 21
      TabOrder = 1
      OnChange = edtBitmapHeightChange
      OnExit = edtBitmapHeightExit
    end
    object edtResolution: TEdit
      Left = 65
      Top = 78
      Width = 79
      Height = 21
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 2
    end
    object cmbbxWidthUnit: TComboBox
      Left = 150
      Top = 26
      Width = 85
      Height = 21
      Cursor = crHandPoint
      ItemHeight = 13
      TabOrder = 3
      OnChange = cmbbxWidthUnitChange
      Items.Strings = (
        'inches'
        'cm'
        'points'
        'pixels')
    end
    object cmbbxHeightUnit: TComboBox
      Left = 150
      Top = 52
      Width = 85
      Height = 21
      Cursor = crHandPoint
      ItemHeight = 13
      TabOrder = 4
      OnChange = cmbbxHeightUnitChange
      Items.Strings = (
        'inches'
        'cm'
        'points'
        'pixels')
    end
    object cmbbxResolutionUnit: TComboBox
      Left = 150
      Top = 78
      Width = 85
      Height = 21
      Cursor = crHandPoint
      ItemHeight = 13
      TabOrder = 5
      OnChange = cmbbxResolutionUnitChange
      Items.Strings = (
        'Pixels/Inch'
        'Pixels/cm')
    end
  end
  object btbtnOK: TBitBtn
    Left = 260
    Top = 13
    Width = 60
    Height = 20
    Cursor = crHandPoint
    TabOrder = 1
    OnClick = btbtnOKClick
    Kind = bkOK
  end
  object btbtnCancel: TBitBtn
    Left = 260
    Top = 46
    Width = 60
    Height = 20
    Cursor = crHandPoint
    TabOrder = 2
    Kind = bkCancel
  end
end
