object HueCirclingForm: THueCirclingForm
  Left = 192
  Top = 128
  BorderStyle = bsDialog
  Caption = 'Hue Circling'
  ClientHeight = 169
  ClientWidth = 313
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
  DesignSize = (
    313
    169)
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 201
    Height = 150
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    DesignSize = (
      201
      150)
    object lblDepth: TLabel
      Left = 40
      Top = 95
      Width = 23
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = 'Hue:'
    end
    object lblDegrees: TLabel
      Left = 136
      Top = 95
      Width = 40
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = 'Degrees'
    end
    object editDegrees: TEdit
      Left = 72
      Top = 91
      Width = 50
      Height = 21
      Anchors = [akLeft, akBottom]
      TabOrder = 0
      OnChange = editDegreesChange
    end
    object gbDegrees: TGaugeBar
      Left = 8
      Top = 120
      Width = 185
      Height = 16
      Cursor = crHandPoint
      Anchors = [akLeft, akBottom]
      Backgnd = bgPattern
      Max = 359
      ShowHandleGrip = True
      Style = rbsMac
      Position = 0
      OnChange = gbDegreesChange
      OnMouseDown = gbDegreesMouseDown
      OnMouseUp = gbDegreesMouseUp
    end
    object PreviewImage: TImage32
      Left = 8
      Top = 92
      Width = 17
      Height = 17
      Anchors = [akLeft, akTop, akRight, akBottom]
      Bitmap.ResamplerClassName = 'TNearestResampler'
      BitmapAlign = baTopLeft
      Color = clSkyBlue
      ParentColor = False
      Scale = 1.000000000000000000
      ScaleMode = smNormal
      TabOrder = 2
    end
    object Memo1: TMemo
      Left = 8
      Top = 16
      Width = 185
      Height = 65
      BevelInner = bvNone
      Color = clInfoBk
      Lines.Strings = (
        'Note: Saturation and Intensity of each '
        'pixel is fixed while the Hue value '
        'can be changed to any color '
        'of the chromatic circle.')
      TabOrder = 3
    end
  end
  object OKButton: TBitBtn
    Left = 216
    Top = 16
    Width = 88
    Height = 25
    Cursor = crHandPoint
    TabOrder = 1
    Kind = bkOK
  end
  object CancelButton: TBitBtn
    Left = 216
    Top = 56
    Width = 88
    Height = 25
    Cursor = crHandPoint
    TabOrder = 2
    Kind = bkCancel
  end
  object checkboxPreview: TCheckBox
    Left = 224
    Top = 136
    Width = 75
    Height = 17
    Cursor = crHandPoint
    Caption = 'Preview'
    Checked = True
    State = cbChecked
    TabOrder = 3
    OnClick = checkboxPreviewClick
  end
  object AboutButton: TButton
    Left = 216
    Top = 96
    Width = 88
    Height = 25
    Cursor = crHandPoint
    Caption = 'About...'
    TabOrder = 4
    OnClick = AboutButtonClick
  end
end
