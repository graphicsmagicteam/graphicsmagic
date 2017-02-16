object frmApplyImage: TfrmApplyImage
  Left = 191
  Top = 131
  BorderStyle = bsDialog
  Caption = 'Apply Image'
  ClientHeight = 399
  ClientWidth = 456
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
  object lblTarget: TLabel
    Left = 16
    Top = 136
    Width = 34
    Height = 13
    Caption = 'Target:'
  end
  object lblTargetString: TLabel
    Left = 64
    Top = 136
    Width = 58
    Height = 13
    Caption = 'TargetString'
  end
  object grpbxSource: TGroupBox
    Left = 8
    Top = 8
    Width = 345
    Height = 113
    Caption = 'Source:'
    TabOrder = 0
    object lblSourceImage: TLabel
      Left = 18
      Top = 24
      Width = 32
      Height = 13
      Caption = 'Image:'
    end
    object lblSourceLayer: TLabel
      Left = 21
      Top = 52
      Width = 29
      Height = 13
      Caption = 'Layer:'
    end
    object lblSourceChannel: TLabel
      Left = 8
      Top = 80
      Width = 42
      Height = 13
      Caption = 'Channel:'
    end
    object cmbbxSourceImage: TComboBox
      Left = 56
      Top = 20
      Width = 273
      Height = 21
      Cursor = crHandPoint
      DropDownCount = 30
      ItemHeight = 13
      TabOrder = 0
      OnChange = cmbbxSourceImageChange
    end
    object cmbbxSourceLayer: TComboBox
      Left = 56
      Top = 48
      Width = 273
      Height = 21
      Cursor = crHandPoint
      DropDownCount = 30
      ItemHeight = 13
      TabOrder = 1
      OnChange = cmbbxSourceLayerChange
    end
    object cmbbxSourceChannel: TComboBox
      Left = 56
      Top = 76
      Width = 201
      Height = 21
      Cursor = crHandPoint
      ItemHeight = 13
      TabOrder = 2
      OnChange = cmbbxSourceChannelChange
    end
    object chckbxSourceChannelInvert: TCheckBox
      Left = 269
      Top = 78
      Width = 60
      Height = 17
      Cursor = crHandPoint
      Caption = 'Invert'
      TabOrder = 3
      OnClick = chckbxSourceChannelInvertClick
    end
  end
  object btbtnOK: TBitBtn
    Left = 368
    Top = 16
    Width = 75
    Height = 25
    Cursor = crHandPoint
    TabOrder = 1
    Kind = bkOK
  end
  object btbtnCancel: TBitBtn
    Left = 368
    Top = 56
    Width = 75
    Height = 25
    Cursor = crHandPoint
    TabOrder = 2
    Kind = bkCancel
  end
  object chckbxPreview: TCheckBox
    Left = 368
    Top = 96
    Width = 73
    Height = 17
    Cursor = crHandPoint
    Caption = 'Preview'
    Checked = True
    State = cbChecked
    TabOrder = 3
    OnClick = chckbxPreviewClick
  end
  object grpbxBlending: TGroupBox
    Left = 8
    Top = 160
    Width = 345
    Height = 233
    Caption = 'Blend:'
    TabOrder = 4
    object lblBlendMode: TLabel
      Left = 16
      Top = 28
      Width = 30
      Height = 13
      Caption = 'Mode:'
    end
    object lblBlendOpacity: TLabel
      Left = 7
      Top = 56
      Width = 39
      Height = 13
      Caption = 'Opacity:'
    end
    object lblOpacityPercent: TLabel
      Left = 112
      Top = 56
      Width = 8
      Height = 13
      Caption = '%'
    end
    object cmbbxBlendMode: TComboBox
      Left = 56
      Top = 24
      Width = 273
      Height = 21
      Cursor = crHandPoint
      DropDownCount = 30
      ItemHeight = 13
      TabOrder = 0
      OnChange = cmbbxBlendModeChange
    end
    object edtBlendOpacity: TEdit
      Left = 56
      Top = 52
      Width = 50
      Height = 21
      TabOrder = 1
      Text = '100'
      OnChange = edtBlendOpacityChange
    end
    object chckbxPreserveTransparency: TCheckBox
      Left = 192
      Top = 54
      Width = 137
      Height = 17
      Cursor = crHandPoint
      Caption = 'Preserve Transparency'
      TabOrder = 2
      OnClick = chckbxPreserveTransparencyClick
    end
    object chckbxEnableMask: TCheckBox
      Left = 192
      Top = 80
      Width = 97
      Height = 17
      Cursor = crHandPoint
      Caption = 'Mask...'
      TabOrder = 3
      OnClick = chckbxEnableMaskClick
    end
    object grpbxMask: TGroupBox
      Left = 8
      Top = 104
      Width = 329
      Height = 121
      TabOrder = 4
      object lblMaskImage: TLabel
        Left = 21
        Top = 20
        Width = 29
        Height = 13
        Caption = 'Mask:'
        Enabled = False
      end
      object lblMaskLayer: TLabel
        Left = 21
        Top = 48
        Width = 29
        Height = 13
        Caption = 'Layer:'
        Enabled = False
      end
      object lblMaskChannel: TLabel
        Left = 8
        Top = 76
        Width = 42
        Height = 13
        Caption = 'Channel:'
        Enabled = False
      end
      object cmbbxMaskImage: TComboBox
        Left = 58
        Top = 16
        Width = 257
        Height = 21
        Cursor = crHandPoint
        DropDownCount = 30
        Enabled = False
        ItemHeight = 13
        TabOrder = 0
        OnChange = cmbbxMaskImageChange
      end
      object cmbbxMaskLayer: TComboBox
        Left = 58
        Top = 44
        Width = 255
        Height = 21
        Cursor = crHandPoint
        DropDownCount = 30
        Enabled = False
        ItemHeight = 13
        TabOrder = 1
        OnChange = cmbbxMaskLayerChange
      end
      object cmbbxMaskChannel: TComboBox
        Left = 58
        Top = 72
        Width = 183
        Height = 21
        Cursor = crHandPoint
        Enabled = False
        ItemHeight = 13
        TabOrder = 2
        OnChange = cmbbxMaskChannelChange
      end
      object chckbxMaskChannelInvert: TCheckBox
        Left = 253
        Top = 74
        Width = 60
        Height = 17
        Cursor = crHandPoint
        Caption = 'Invert'
        Enabled = False
        TabOrder = 3
        OnClick = chckbxMaskChannelInvertClick
      end
    end
  end
end
