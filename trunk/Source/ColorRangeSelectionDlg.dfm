object frmColorRangeSelection: TfrmColorRangeSelection
  Left = 192
  Top = 131
  BorderStyle = bsDialog
  Caption = 'Color Range'
  ClientHeight = 355
  ClientWidth = 360
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
  object lblSampledColor: TLabel
    Left = 273
    Top = 104
    Width = 71
    Height = 13
    Caption = 'Sampled Color:'
  end
  object shpSampledColor: TShape
    Left = 273
    Top = 124
    Width = 75
    Height = 24
  end
  object lblCurrentColor: TLabel
    Left = 273
    Top = 163
    Width = 64
    Height = 13
    Caption = 'Current Color:'
  end
  object shpCurrentColor: TShape
    Left = 273
    Top = 182
    Width = 75
    Height = 24
  end
  object grpbxColorRangeOptions: TGroupBox
    Left = 7
    Top = 7
    Width = 260
    Height = 338
    TabOrder = 0
    object lblFuzziness: TLabel
      Left = 13
      Top = 20
      Width = 49
      Height = 13
      Caption = 'Fuzziness:'
    end
    object edtFuzzinessValue: TEdit
      Left = 182
      Top = 16
      Width = 66
      Height = 21
      TabOrder = 0
      Text = '10'
      OnChange = edtFuzzinessValueChange
    end
    object pnlColorRangeThumbnail: TPanel
      Left = 13
      Top = 65
      Width = 234
      Height = 234
      BevelInner = bvLowered
      TabOrder = 1
      object imgColorRangeThumbnail: TImage32
        Left = 12
        Top = 12
        Width = 210
        Height = 210
        AutoSize = True
        Bitmap.ResamplerClassName = 'TNearestResampler'
        BitmapAlign = baTopLeft
        Scale = 1.000000000000000000
        ScaleMode = smNormal
        TabOrder = 0
        OnMouseMove = imgColorRangeThumbnailMouseMove
        OnMouseUp = imgColorRangeThumbnailMouseUp
      end
    end
    object rdbtnSelection: TRadioButton
      Left = 46
      Top = 312
      Width = 65
      Height = 14
      Cursor = crHandPoint
      Caption = 'Selection'
      Checked = True
      TabOrder = 2
      TabStop = True
      OnClick = rdbtnSelectionClick
    end
    object rdbtnImage: TRadioButton
      Left = 143
      Top = 312
      Width = 65
      Height = 14
      Cursor = crHandPoint
      Caption = 'Image'
      TabOrder = 3
      OnClick = rdbtnImageClick
    end
    object ggbrFuzziness: TGaugeBar
      Left = 13
      Top = 40
      Width = 236
      Height = 16
      Cursor = crHandPoint
      Backgnd = bgPattern
      Max = 200
      ShowHandleGrip = True
      Style = rbsMac
      Position = 10
      OnChange = ChangeFuzziness
    end
  end
  object btbtnOK: TBitBtn
    Left = 280
    Top = 20
    Width = 60
    Height = 20
    Cursor = crHandPoint
    TabOrder = 1
    OnClick = btbtnOKClick
    Kind = bkOK
  end
  object btbtnCancel: TBitBtn
    Left = 280
    Top = 52
    Width = 60
    Height = 20
    Cursor = crHandPoint
    TabOrder = 2
    Kind = bkCancel
  end
end
