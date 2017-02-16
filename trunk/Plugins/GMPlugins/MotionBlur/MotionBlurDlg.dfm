object frmMotionBlur: TfrmMotionBlur
  Left = 188
  Top = 133
  BorderStyle = bsDialog
  Caption = 'Motion Blur'
  ClientHeight = 133
  ClientWidth = 367
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
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 273
    Height = 113
    TabOrder = 0
    object lblAngle: TLabel
      Left = 116
      Top = 32
      Width = 30
      Height = 13
      Caption = 'Angle:'
    end
    object lblDegrees: TLabel
      Left = 213
      Top = 32
      Width = 40
      Height = 13
      Caption = 'Degrees'
    end
    object lblPixels: TLabel
      Left = 213
      Top = 68
      Width = 27
      Height = 13
      Caption = 'Pixels'
    end
    object lblRadius: TLabel
      Left = 109
      Top = 68
      Width = 36
      Height = 13
      Caption = 'Radius:'
    end
    object pnlImageHolder: TPanel
      Left = 8
      Top = 16
      Width = 86
      Height = 86
      BevelInner = bvLowered
      TabOrder = 0
      object imgCircle: TImage
        Left = 2
        Top = 2
        Width = 82
        Height = 82
        AutoSize = True
        OnMouseDown = imgCircleMouseDown
        OnMouseMove = imgCircleMouseMove
        OnMouseUp = imgCircleMouseUp
      end
    end
    object edtAngle: TEdit
      Left = 155
      Top = 28
      Width = 33
      Height = 21
      TabOrder = 1
      Text = '0'
      OnChange = edtAngleChange
    end
    object updwnAngle: TUpDown
      Left = 188
      Top = 28
      Width = 20
      Height = 21
      Cursor = crHandPoint
      Associate = edtAngle
      Min = -90
      Max = 90
      TabOrder = 2
      OnMouseDown = updwnAngleRadiusMouseDown
      OnMouseUp = updwnAngleRadiusMouseUp
    end
    object updwnRadius: TUpDown
      Left = 188
      Top = 65
      Width = 20
      Height = 21
      Cursor = crHandPoint
      Associate = edtRadius
      Min = 1
      Max = 200
      Position = 1
      TabOrder = 3
      OnMouseDown = updwnAngleRadiusMouseDown
      OnMouseUp = updwnAngleRadiusMouseUp
    end
    object edtRadius: TEdit
      Left = 155
      Top = 65
      Width = 33
      Height = 21
      TabOrder = 4
      Text = '1'
      OnChange = edtRadiusChange
    end
  end
  object btbtnOK: TBitBtn
    Left = 293
    Top = 21
    Width = 60
    Height = 20
    Cursor = crHandPoint
    TabOrder = 1
    OnClick = btbtnOKClick
    Kind = bkOK
  end
  object btbtnCancel: TBitBtn
    Left = 293
    Top = 54
    Width = 60
    Height = 20
    Cursor = crHandPoint
    TabOrder = 2
    Kind = bkCancel
  end
end
