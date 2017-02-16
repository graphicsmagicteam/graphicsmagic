object frmReplaceColor: TfrmReplaceColor
  Left = 192
  Top = 128
  BorderStyle = bsDialog
  Caption = 'Replace Color'
  ClientHeight = 334
  ClientWidth = 461
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
  object stsbrColorRange: TStatusBar
    Left = 0
    Top = 304
    Width = 461
    Height = 30
    Panels = <
      item
        Alignment = taCenter
        Width = 50
      end>
  end
  object stsbrCurrentColor: TStatusBar
    Left = 0
    Top = 274
    Width = 461
    Height = 30
    Panels = <
      item
        Alignment = taCenter
        Width = 50
      end>
  end
  object grpbxSelection: TGroupBox
    Left = 7
    Top = 7
    Width = 191
    Height = 273
    Caption = 'Selection:'
    TabOrder = 2
    object lblFuzziness: TLabel
      Left = 13
      Top = 20
      Width = 49
      Height = 13
      Caption = 'Fuzziness:'
    end
    object edtFuzziness: TEdit
      Left = 141
      Top = 20
      Width = 40
      Height = 21
      TabOrder = 0
      Text = '10'
      OnChange = edtFuzzinessChange
    end
    object pnlThumbnail: TPanel
      Left = 7
      Top = 65
      Width = 178
      Height = 179
      BevelOuter = bvLowered
      TabOrder = 1
      object img32Thumbnail: TImage32
        Left = 8
        Top = 8
        Width = 163
        Height = 163
        Bitmap.ResamplerClassName = 'TNearestResampler'
        BitmapAlign = baTopLeft
        Scale = 1.000000000000000000
        ScaleMode = smStretch
        TabOrder = 0
        OnMouseDown = img32ThumbnailMouseDown
        OnMouseMove = img32ThumbnailMouseMove
      end
    end
    object rdbtnSelection: TRadioButton
      Left = 20
      Top = 254
      Width = 72
      Height = 13
      Cursor = crHandPoint
      Caption = 'Selection'
      Checked = True
      TabOrder = 2
      TabStop = True
      OnClick = rdbtnSelectionClick
    end
    object rdbtnImage: TRadioButton
      Left = 104
      Top = 254
      Width = 59
      Height = 13
      Cursor = crHandPoint
      Caption = 'Image'
      TabOrder = 3
      OnClick = rdbtnImageClick
    end
    object ggbrFuzziness: TGaugeBar
      Left = 13
      Top = 44
      Width = 168
      Height = 16
      Cursor = crHandPoint
      Backgnd = bgPattern
      Max = 200
      ShowHandleGrip = True
      Style = rbsMac
      Position = 10
      OnChange = ggbrFuzzinessChange
    end
  end
  object grpbxTransform: TGroupBox
    Left = 202
    Top = 0
    Width = 176
    Height = 209
    Caption = 'Transform:'
    TabOrder = 3
    object lblHR: TLabel
      Left = 13
      Top = 20
      Width = 23
      Height = 13
      Caption = 'Hue:'
    end
    object lblSG: TLabel
      Left = 13
      Top = 65
      Width = 51
      Height = 13
      Caption = 'Saturation:'
    end
    object lblLVB: TLabel
      Left = 13
      Top = 111
      Width = 48
      Height = 13
      Caption = 'Lightness:'
    end
    object lblSample: TLabel
      Left = 13
      Top = 163
      Width = 38
      Height = 13
      Caption = 'Sample:'
    end
    object shpSample: TShape
      Left = 65
      Top = 159
      Width = 98
      Height = 21
    end
    object lblHRPercent: TLabel
      Left = 152
      Top = 20
      Width = 8
      Height = 13
      Caption = '%'
    end
    object lblSGPercent: TLabel
      Left = 152
      Top = 66
      Width = 8
      Height = 13
      Caption = '%'
    end
    object lblLVBPercent: TLabel
      Left = 152
      Top = 111
      Width = 8
      Height = 13
      Caption = '%'
    end
    object edtHR: TEdit
      Left = 108
      Top = 16
      Width = 40
      Height = 21
      TabOrder = 0
      Text = '0'
      OnChange = edtHRChange
    end
    object edtSG: TEdit
      Left = 108
      Top = 62
      Width = 40
      Height = 21
      TabOrder = 1
      Text = '0'
      OnChange = edtSGChange
    end
    object edtLVB: TEdit
      Left = 108
      Top = 107
      Width = 40
      Height = 21
      TabOrder = 2
      Text = '0'
      OnChange = edtLVBChange
    end
    object ggbrHR: TGaugeBar
      Left = 13
      Top = 41
      Width = 150
      Height = 16
      Cursor = crHandPoint
      Backgnd = bgPattern
      Max = 200
      ShowHandleGrip = True
      Style = rbsMac
      Position = 100
      OnChange = ChangeColor
    end
    object ggbrSG: TGaugeBar
      Left = 13
      Top = 87
      Width = 150
      Height = 16
      Cursor = crHandPoint
      Backgnd = bgPattern
      Max = 200
      ShowHandleGrip = True
      Style = rbsMac
      Position = 100
      OnChange = ChangeColor
    end
    object ggbrLVB: TGaugeBar
      Left = 13
      Top = 132
      Width = 150
      Height = 16
      Cursor = crHandPoint
      Backgnd = bgPattern
      Max = 200
      ShowHandleGrip = True
      Style = rbsMac
      Position = 100
      OnChange = ChangeColor
    end
  end
  object btbtnOK: TBitBtn
    Left = 390
    Top = 20
    Width = 61
    Height = 20
    Cursor = crHandPoint
    TabOrder = 4
    OnClick = btbtnOKClick
    Kind = bkOK
  end
  object btbtnCancel: TBitBtn
    Left = 390
    Top = 52
    Width = 61
    Height = 20
    Cursor = crHandPoint
    TabOrder = 5
    Kind = bkCancel
  end
  object chckbxPreview: TCheckBox
    Left = 390
    Top = 85
    Width = 59
    Height = 13
    Cursor = crHandPoint
    Caption = 'Preview'
    Checked = True
    State = cbChecked
    TabOrder = 6
    OnClick = chckbxPreviewClick
  end
  object grpbxColorMode: TGroupBox
    Left = 202
    Top = 208
    Width = 176
    Height = 72
    Caption = 'Color Mode:'
    TabOrder = 7
    object rdbtnHLS: TRadioButton
      Left = 13
      Top = 33
      Width = 41
      Height = 13
      Caption = 'HLS'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = ChangeColorModeClick
    end
    object rdbtnHSV: TRadioButton
      Left = 65
      Top = 33
      Width = 41
      Height = 13
      Caption = 'HSV'
      TabOrder = 1
      OnClick = ChangeColorModeClick
    end
    object rdbtnRGB: TRadioButton
      Left = 117
      Top = 33
      Width = 41
      Height = 13
      Caption = 'RGB'
      TabOrder = 2
      OnClick = ChangeColorModeClick
    end
  end
end
