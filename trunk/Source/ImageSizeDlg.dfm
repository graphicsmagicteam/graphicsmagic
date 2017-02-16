object frmImageSize: TfrmImageSize
  Left = 191
  Top = 131
  BorderStyle = bsDialog
  Caption = 'Image Size'
  ClientHeight = 327
  ClientWidth = 384
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
  object grpbxImageSize: TGroupBox
    Left = 7
    Top = 7
    Width = 282
    Height = 82
    Caption = 'Pixel Dimensions:'
    TabOrder = 0
    object lblImageWidth: TLabel
      Left = 13
      Top = 26
      Width = 31
      Height = 13
      Caption = 'Width:'
    end
    object lblImageHeight: TLabel
      Left = 13
      Top = 55
      Width = 34
      Height = 13
      Caption = 'Height:'
    end
    object edtImageWidth: TEdit
      Left = 52
      Top = 26
      Width = 98
      Height = 21
      TabOrder = 0
      OnChange = edtImageWidthChange
      OnExit = edtImageWidthExit
    end
    object edtImageHeight: TEdit
      Left = 52
      Top = 51
      Width = 98
      Height = 21
      TabOrder = 1
      OnChange = edtImageHeightChange
      OnExit = edtImageHeightExit
    end
    object cmbbxWidthUnit: TComboBox
      Left = 156
      Top = 26
      Width = 85
      Height = 21
      Cursor = crHandPoint
      ItemHeight = 13
      TabOrder = 2
      OnChange = cmbbxWidthUnitChange
      Items.Strings = (
        'inches'
        'cm'
        'points'
        'pixels')
    end
    object cmbbxHeightUnit: TComboBox
      Left = 156
      Top = 51
      Width = 85
      Height = 21
      Cursor = crHandPoint
      ItemHeight = 13
      TabOrder = 3
      OnChange = cmbbxHeightUnitChange
      Items.Strings = (
        'inches'
        'cm'
        'points'
        'pixels')
    end
    object imgvwChain: TImgView32
      Left = 248
      Top = 24
      Width = 22
      Height = 49
      Bitmap.ResamplerClassName = 'TNearestResampler'
      Bitmap.Data = {
        1600000019000000000000FF000000FF000000FF000000FF000000FF000000FF
        000000FF000000FF000000FF000000FFFFFFFF00FFFFFF00FFFFFF00FFFFFF00
        FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
        000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF
        000000FF000000FFFFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
        FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
        FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000FF000000FF
        FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
        FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
        FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000FF000000FFFFFFFF00FFFFFF00
        FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000FF000000FF000000FF000000FF
        FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
        FFFFFF00FFFFFF00000000FF000000FFFFFFFF00FFFFFF00FFFFFF00FFFFFF00
        FFFFFF00000000FFFFFFFF00FFFFFF00FFFFFF00FFFFFF00000000FFFFFFFF00
        FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
        000000FF000000FFFFFFFF00FFFFFF00FFFFFF00FFFFFF00000000FFFFFFFF00
        FFFFFF00000000FF000000FFFFFFFF00FFFFFF00000000FFFFFFFF00FFFFFF00
        FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000FF000000FF
        FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000FFFFFFFF00000000FFFFFFFF00
        FFFFFF00000000FFFFFFFF00000000FFFFFFFF00FFFFFF00FFFFFF00FFFFFF00
        FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000FF000000FFFFFFFF00FFFFFF00
        FFFFFF00FFFFFF00000000FFFFFFFF00000000FF000000FF000000FF000000FF
        FFFFFF00000000FFFFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
        FFFFFF00FFFFFF00000000FF000000FFFFFFFF00FFFFFF00FFFFFF00FFFFFF00
        FFFFFF00000000FFFFFFFF00FFFFFF00000000FFFFFFFF00FFFFFF00000000FF
        FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
        000000FF000000FFFFFFFF00FFFFFF00FFFFFF00FFFFFF00000000FFFFFFFF00
        FFFFFF00000000FFFFFFFF00FFFFFF00000000FFFFFFFF00FFFFFF00FFFFFF00
        FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000FF000000FF
        FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000FFFFFFFF00000000FF000000FF
        000000FF000000FFFFFFFF00000000FFFFFFFF00FFFFFF00FFFFFF00FFFFFF00
        FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000FF000000FFFFFFFF00FFFFFF00
        FFFFFF00FFFFFF00000000FFFFFFFF00000000FFFFFFFF00FFFFFF00000000FF
        FFFFFF00000000FFFFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
        FFFFFF00FFFFFF00000000FF000000FFFFFFFF00FFFFFF00FFFFFF00FFFFFF00
        000000FFFFFFFF00000000FF000000FF000000FF000000FFFFFFFF00000000FF
        FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
        000000FF000000FFFFFFFF00FFFFFF00FFFFFF00FFFFFF00000000FFFFFFFF00
        FFFFFF00000000FFFFFFFF00FFFFFF00000000FFFFFFFF00FFFFFF00FFFFFF00
        FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000FF000000FF
        FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000FFFFFFFF00FFFFFF00
        000000FFFFFFFF00FFFFFF00000000FFFFFFFF00FFFFFF00FFFFFF00FFFFFF00
        FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000FF000000FFFFFFFF00FFFFFF00
        FFFFFF00FFFFFF00000000FFFFFFFF00000000FF000000FF000000FF000000FF
        FFFFFF00000000FFFFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
        FFFFFF00FFFFFF00000000FF000000FFFFFFFF00FFFFFF00FFFFFF00FFFFFF00
        000000FFFFFFFF00000000FFFFFFFF00FFFFFF00000000FFFFFFFF00000000FF
        FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
        000000FF000000FFFFFFFF00FFFFFF00FFFFFF00FFFFFF00000000FFFFFFFF00
        FFFFFF00000000FF000000FFFFFFFF00FFFFFF00000000FFFFFFFF00FFFFFF00
        FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000FF000000FF
        FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000FFFFFFFF00FFFFFF00
        FFFFFF00FFFFFF00000000FFFFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
        FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000FF000000FFFFFFFF00FFFFFF00
        FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000FF000000FF000000FF000000FF
        FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
        FFFFFF00FFFFFF00000000FF000000FFFFFFFF00FFFFFF00FFFFFF00FFFFFF00
        FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
        FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
        000000FF000000FFFFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
        FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
        FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000FF000000FF
        FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
        FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000FF000000FF000000FF000000FF
        000000FF000000FF000000FF000000FF000000FF000000FFFFFFFF00FFFFFF00
        FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00
        FFFFFF00FFFFFF00000000FF000000FF000000FF000000FF000000FF000000FF
        000000FF000000FF000000FF000000FFFFFFFF00FFFFFF00FFFFFF00FFFFFF00
        FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00}
      BitmapAlign = baCustom
      Scale = 1.000000000000000000
      ScaleMode = smResize
      ScrollBars.ShowHandleGrip = True
      ScrollBars.Style = rbsDefault
      ScrollBars.Size = 17
      ScrollBars.Visibility = svHidden
      OverSize = 0
      TabOrder = 4
    end
  end
  object btbtnOK: TBitBtn
    Left = 305
    Top = 12
    Width = 61
    Height = 20
    Cursor = crHandPoint
    TabOrder = 1
    OnClick = btbtnOKClick
    Kind = bkOK
  end
  object btbtnCancel: TBitBtn
    Left = 305
    Top = 44
    Width = 61
    Height = 20
    Cursor = crHandPoint
    TabOrder = 2
    Kind = bkCancel
  end
  object chckbxConstrainProperties: TCheckBox
    Left = 8
    Top = 96
    Width = 153
    Height = 17
    Cursor = crHandPoint
    Caption = 'Constrain Properties'
    Checked = True
    State = cbChecked
    TabOrder = 3
    OnClick = chckbxConstrainPropertiesClick
  end
  object grpbxResamplingOptions: TGroupBox
    Left = 8
    Top = 120
    Width = 281
    Height = 97
    Caption = 'Resampling Options:'
    TabOrder = 4
    object lblResampler: TLabel
      Left = 56
      Top = 24
      Width = 53
      Height = 13
      Caption = 'Resampler:'
    end
    object lblPAM: TLabel
      Left = 16
      Top = 48
      Width = 93
      Height = 13
      Caption = 'Pixel Access Mode:'
    end
    object lblWrapMode: TLabel
      Left = 50
      Top = 72
      Width = 59
      Height = 13
      Caption = 'Wrap Mode:'
    end
    object cmbbxResampler: TComboBox
      Left = 120
      Top = 20
      Width = 145
      Height = 21
      Cursor = crHandPoint
      ItemHeight = 13
      TabOrder = 0
      OnChange = cmbbxResamplerChange
    end
    object cmbbxPAM: TComboBox
      Left = 120
      Top = 44
      Width = 145
      Height = 21
      Cursor = crHandPoint
      ItemHeight = 13
      TabOrder = 1
      OnChange = cmbbxPAMChange
    end
    object cmbbxWrapMode: TComboBox
      Left = 120
      Top = 68
      Width = 145
      Height = 21
      Cursor = crHandPoint
      ItemHeight = 13
      TabOrder = 2
      OnChange = cmbbxWrapModeChange
    end
  end
  object grpbxKernelOptions: TGroupBox
    Left = 8
    Top = 224
    Width = 281
    Height = 97
    Caption = 'Kernel Options:'
    TabOrder = 5
    object lblKernel: TLabel
      Left = 76
      Top = 24
      Width = 33
      Height = 13
      Caption = 'Kernel:'
    end
    object lblKernelMode: TLabel
      Left = 46
      Top = 48
      Width = 63
      Height = 13
      Caption = 'Kernel Mode:'
    end
    object lblTableSize: TLabel
      Left = 12
      Top = 72
      Width = 97
      Height = 13
      Caption = 'Table Size (32/100):'
    end
    object cmbbxKernel: TComboBox
      Left = 120
      Top = 20
      Width = 145
      Height = 21
      Cursor = crHandPoint
      DropDownCount = 16
      ItemHeight = 13
      TabOrder = 0
      OnChange = cmbbxKernelChange
    end
    object cmbbxKernelMode: TComboBox
      Left = 120
      Top = 44
      Width = 145
      Height = 21
      Cursor = crHandPoint
      ItemHeight = 13
      TabOrder = 1
      OnChange = cmbbxKernelModeChange
    end
    object ggbrTableSize: TGaugeBar
      Left = 120
      Top = 72
      Width = 145
      Height = 16
      Cursor = crHandPoint
      Backgnd = bgPattern
      Min = 1
      ShowHandleGrip = True
      Style = rbsMac
      Position = 32
      OnChange = ggbrTableSizeChange
    end
  end
end
