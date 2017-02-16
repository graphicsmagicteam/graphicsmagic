object frmFill: TfrmFill
  Left = 189
  Top = 133
  BorderStyle = bsDialog
  Caption = 'Fill'
  ClientHeight = 211
  ClientWidth = 278
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
  object grpbxFillContent: TGroupBox
    Left = 7
    Top = 7
    Width = 189
    Height = 91
    Caption = 'Contents:'
    TabOrder = 0
    object lblUseFillMode: TLabel
      Left = 20
      Top = 26
      Width = 22
      Height = 13
      Caption = 'Use:'
    end
    object lblCustomPattern: TLabel
      Left = 20
      Top = 59
      Width = 75
      Height = 13
      Caption = 'Custom Pattern:'
    end
    object spdbtnSelectPattern: TSpeedButton
      Left = 134
      Top = 50
      Width = 18
      Height = 30
      Cursor = crHandPoint
      Glyph.Data = {
        F6000000424DF600000000000000360000002800000008000000080000000100
        180000000000C000000000000000000000000000000000000000FFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF404040404040FFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFF404040000000000000404040FFFFFFFFFFFFFFFFFF404040
        000000000000000000000000404040FFFFFF4040400000000000000000000000
        00000000000000404040FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
      OnClick = spdbtnSelectPatternClick
    end
    object cmbbxFillOptions: TComboBox
      Left = 46
      Top = 23
      Width = 117
      Height = 21
      Cursor = crHandPoint
      ItemHeight = 13
      TabOrder = 0
      OnChange = cmbbxFillOptionsChange
      Items.Strings = (
        'Foreground Color'
        'Background Color'
        'Pattern'
        'History'
        'Black'
        '50% Gray'
        'White')
    end
    object pnlCustomPattern: TPanel
      Left = 104
      Top = 50
      Width = 29
      Height = 30
      BevelInner = bvLowered
      TabOrder = 1
      object imgSelectedPattern: TImage32
        Left = 2
        Top = 2
        Width = 26
        Height = 26
        AutoSize = True
        Bitmap.ResamplerClassName = 'TNearestResampler'
        BitmapAlign = baTopLeft
        Scale = 1.000000000000000000
        ScaleMode = smNormal
        TabOrder = 0
      end
    end
  end
  object grpbxFillBlend: TGroupBox
    Left = 7
    Top = 104
    Width = 189
    Height = 98
    Caption = 'Blending:'
    TabOrder = 1
    object lblBlendMode: TLabel
      Left = 20
      Top = 20
      Width = 30
      Height = 13
      Caption = 'Mode:'
    end
    object lblBlendOpacity: TLabel
      Left = 10
      Top = 46
      Width = 39
      Height = 13
      Caption = 'Opacity:'
    end
    object Label1: TLabel
      Left = 112
      Top = 46
      Width = 8
      Height = 13
      Caption = '%'
    end
    object cmbbxBlendMode: TComboBox
      Left = 59
      Top = 16
      Width = 117
      Height = 21
      Cursor = crHandPoint
      DropDownCount = 30
      ItemHeight = 13
      TabOrder = 0
      OnChange = cmbbxBlendModeChange
    end
    object edtBlendOpacity: TEdit
      Left = 59
      Top = 42
      Width = 32
      Height = 21
      TabOrder = 1
      Text = '1'
      OnChange = edtBlendOpacityChange
    end
    object updwnBlendOpacity: TUpDown
      Left = 91
      Top = 42
      Width = 14
      Height = 21
      Cursor = crHandPoint
      Associate = edtBlendOpacity
      Min = 1
      Position = 1
      TabOrder = 2
    end
    object chckbxPreserveTransparency: TCheckBox
      Left = 10
      Top = 72
      Width = 134
      Height = 13
      Cursor = crHandPoint
      Caption = 'Preserve Transparency'
      TabOrder = 3
      OnClick = chckbxPreserveTransparencyClick
    end
  end
  object btbtnOK: TBitBtn
    Left = 208
    Top = 12
    Width = 60
    Height = 20
    Cursor = crHandPoint
    TabOrder = 2
    OnClick = btbtnOKClick
    Kind = bkOK
  end
  object btbtnCancel: TBitBtn
    Left = 208
    Top = 44
    Width = 60
    Height = 20
    Cursor = crHandPoint
    TabOrder = 3
    Kind = bkCancel
  end
end
