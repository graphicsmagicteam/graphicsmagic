object frmPaintBucketAdvancedOptions: TfrmPaintBucketAdvancedOptions
  Left = 192
  Top = 128
  BorderStyle = bsNone
  Caption = 'frmPaintBucketAdvancedOptions'
  ClientHeight = 124
  ClientWidth = 193
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDeactivate = FormDeactivate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlPaintBucketAdvancedOptions: TPanel
    Left = 0
    Top = 0
    Width = 193
    Height = 124
    Align = alClient
    BevelInner = bvRaised
    TabOrder = 0
    object grpbxPaintBucketAdvancedSettings: TGroupBox
      Left = 8
      Top = 8
      Width = 176
      Height = 106
      Caption = 'Advanced Settings:'
      TabOrder = 0
      object lblFillType: TLabel
        Left = 13
        Top = 33
        Width = 27
        Height = 13
        Caption = 'Type:'
      end
      object lblFillIntensity: TLabel
        Left = 16
        Top = 72
        Width = 42
        Height = 13
        Caption = 'Intensity:'
        Enabled = False
      end
      object lblFillIntensityPercentSign: TLabel
        Left = 128
        Top = 72
        Width = 8
        Height = 13
        Caption = '%'
        Enabled = False
      end
      object cmbbxFillType: TComboBox
        Left = 46
        Top = 29
        Width = 117
        Height = 21
        Cursor = crHandPoint
        ItemHeight = 13
        TabOrder = 0
        OnChange = cmbbxFillTypeChange
        Items.Strings = (
          'RGB'
          'Preserve Hue'
          'Preserve Saturation'
          'Preserve Luminosity'
          '+ Hue'
          '- Hue'
          '+ Saturation'
          '- Saturation'
          '+ Luminosity'
          '- Luminosity')
      end
      object edtFillIntensity: TEdit
        Left = 64
        Top = 68
        Width = 40
        Height = 21
        Color = clBtnFace
        Enabled = False
        TabOrder = 1
        Text = '10'
        OnChange = edtFillIntensityChange
      end
      object updwnFillIntensity: TUpDown
        Left = 104
        Top = 68
        Width = 17
        Height = 21
        Cursor = crHandPoint
        Associate = edtFillIntensity
        Enabled = False
        Min = 1
        Position = 10
        TabOrder = 2
      end
    end
  end
end
