object frmGradientFill: TfrmGradientFill
  Left = 191
  Top = 128
  BorderStyle = bsDialog
  Caption = 'Gradient Fill'
  ClientHeight = 391
  ClientWidth = 311
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
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lblGradient: TLabel
    Left = 16
    Top = 19
    Width = 43
    Height = 13
    Caption = 'Gradient:'
  end
  object spdbtnOpenGradientPicker: TSpeedButton
    Left = 198
    Top = 10
    Width = 18
    Height = 30
    Cursor = crHandPoint
    Hint = 'Open Gradient Picker'
    Glyph.Data = {
      F6000000424DF600000000000000360000002800000008000000080000000100
      180000000000C000000000000000000000000000000000000000FFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF404040404040FFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFF404040000000000000404040FFFFFFFFFFFFFFFFFF404040
      000000000000000000000000404040FFFFFF4040400000000000000000000000
      00000000000000404040FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
    ParentShowHint = False
    ShowHint = True
    OnClick = spdbtnOpenGradientPickerClick
    OnMouseDown = spdbtnOpenGradientPickerMouseDown
  end
  object lblGradientStyle: TLabel
    Left = 16
    Top = 56
    Width = 26
    Height = 13
    Caption = 'Style:'
  end
  object lblGradientAngle: TLabel
    Left = 16
    Top = 112
    Width = 30
    Height = 13
    Caption = 'Angle:'
  end
  object lblGradientScale: TLabel
    Left = 16
    Top = 168
    Width = 30
    Height = 13
    Caption = 'Scale:'
  end
  object lblScale: TLabel
    Left = 128
    Top = 168
    Width = 8
    Height = 13
    Caption = '%'
  end
  object pnlGradientHolder: TPanel
    Left = 72
    Top = 10
    Width = 125
    Height = 30
    BevelInner = bvLowered
    TabOrder = 0
    object imgSelectedGradient: TImage32
      Left = 2
      Top = 2
      Width = 121
      Height = 26
      Cursor = crHandPoint
      Hint = 'Click to edit gradient'
      Align = alClient
      Bitmap.ResamplerClassName = 'TNearestResampler'
      BitmapAlign = baTopLeft
      ParentShowHint = False
      Scale = 1.000000000000000000
      ScaleMode = smNormal
      ShowHint = True
      TabOrder = 0
      OnClick = imgSelectedGradientClick
      OnPaintStage = imgSelectedGradientPaintStage
    end
  end
  object cmbbxGradientStyle: TComboBox
    Left = 72
    Top = 52
    Width = 145
    Height = 21
    ItemHeight = 13
    TabOrder = 1
    OnChange = cmbbxGradientStyleChange
    Items.Strings = (
      'Linear'
      'Radial'
      'Angle'
      'Reflected'
      'Diamond')
  end
  object pnlGradientAngleHolder: TPanel
    Left = 72
    Top = 93
    Width = 50
    Height = 50
    BevelInner = bvLowered
    TabOrder = 2
    object imgGradientAngle: TImage32
      Left = 2
      Top = 2
      Width = 46
      Height = 46
      Align = alClient
      Bitmap.ResamplerClassName = 'TNearestResampler'
      BitmapAlign = baTopLeft
      Scale = 1.000000000000000000
      ScaleMode = smNormal
      TabOrder = 0
      OnMouseDown = imgGradientAngleMouseDown
      OnMouseMove = imgGradientAngleMouseMove
      OnMouseUp = imgGradientAngleMouseUp
    end
  end
  object edtGradientAngle: TEdit
    Left = 128
    Top = 108
    Width = 49
    Height = 21
    TabOrder = 3
    OnChange = edtGradientAngleChange
  end
  object pnlDegreesHolder: TPanel
    Left = 184
    Top = 108
    Width = 21
    Height = 21
    BevelOuter = bvNone
    TabOrder = 4
    object imgDegrees: TImage
      Left = 0
      Top = 0
      Width = 21
      Height = 21
      Align = alClient
      Picture.Data = {
        07544269746D617072010000424D720100000000000076000000280000001500
        0000150000000100040000000000FC0000000000000000000000100000000000
        0000000000000000800000800000008080008000000080008000808000008080
        8000C0C0C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFF
        FF00FFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFF
        FFFFFFFFF000FFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFF000FFFF
        FFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFF
        F000FFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFF
        FFFFFFFFF000FFFF00FFFFFFFFFFFFFFF000FFF0FF0FFFFFFFFFFFFFF000FFF0
        FF0FFFFFFFFFFFFFF000FFFF00FFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFF
        F000FFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFF
        FFFFFFFFF000FFFFFFFFFFFFFFFFFFFFF000FFFFFFFFFFFFFFFFFFFFF000}
      Stretch = True
    end
  end
  object edtGradientScale: TEdit
    Left = 72
    Top = 164
    Width = 49
    Height = 21
    TabOrder = 5
    OnChange = edtGradientScaleChange
  end
  object grpbxTranslate: TGroupBox
    Left = 16
    Top = 224
    Width = 281
    Height = 153
    Caption = 'Translate:'
    TabOrder = 6
    object lblTranslateX: TLabel
      Left = 16
      Top = 32
      Width = 10
      Height = 13
      Caption = 'X:'
    end
    object lblTranslateY: TLabel
      Left = 16
      Top = 88
      Width = 10
      Height = 13
      Caption = 'Y:'
    end
    object edtTranslateX: TEdit
      Left = 56
      Top = 28
      Width = 80
      Height = 21
      TabOrder = 0
      OnChange = edtTranslateXChange
    end
    object edtTranslateY: TEdit
      Left = 56
      Top = 84
      Width = 80
      Height = 21
      TabOrder = 1
      OnChange = edtTranslateYChange
    end
    object ggbrTranslateX: TGaugeBar
      Left = 16
      Top = 56
      Width = 249
      Height = 16
      Cursor = crHandPoint
      Backgnd = bgPattern
      Max = 10
      ShowHandleGrip = True
      Style = rbsMac
      Position = 0
      OnChange = ChangeTranslateX
    end
    object ggbrTranslateY: TGaugeBar
      Left = 16
      Top = 112
      Width = 249
      Height = 16
      Cursor = crHandPoint
      Backgnd = bgPattern
      Max = 10
      ShowHandleGrip = True
      Style = rbsMac
      Position = 0
      OnChange = ChangeTranslateY
    end
  end
  object btbtnOK: TBitBtn
    Left = 240
    Top = 13
    Width = 60
    Height = 20
    Cursor = crHandPoint
    TabOrder = 7
    Kind = bkOK
  end
  object btbtnCancel: TBitBtn
    Left = 240
    Top = 48
    Width = 60
    Height = 20
    Cursor = crHandPoint
    TabOrder = 8
    Kind = bkCancel
  end
  object chckbxReverse: TCheckBox
    Left = 240
    Top = 88
    Width = 60
    Height = 17
    Cursor = crHandPoint
    Caption = 'Reverse'
    TabOrder = 9
    OnClick = chckbxReverseClick
  end
  object ggbrGradientScale: TGaugeBar
    Left = 16
    Top = 192
    Width = 281
    Height = 16
    Cursor = crHandPoint
    Backgnd = bgPattern
    Max = 150
    Min = 10
    ShowHandleGrip = True
    Style = rbsMac
    Position = 10
    OnChange = ChangeGradientScale
  end
end
