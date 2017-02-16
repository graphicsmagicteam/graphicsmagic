object frmRotateCanvas: TfrmRotateCanvas
  Left = 193
  Top = 130
  ActiveControl = edtAngle
  BorderStyle = bsDialog
  Caption = 'Rotate Canvas'
  ClientHeight = 83
  ClientWidth = 319
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
  object bvlRotateCanvas: TBevel
    Left = 7
    Top = 7
    Width = 226
    Height = 65
    Shape = bsFrame
  end
  object lblAngle: TLabel
    Left = 44
    Top = 20
    Width = 30
    Height = 13
    Caption = 'Angle:'
  end
  object imgDegrees: TImage
    Left = 168
    Top = 16
    Width = 21
    Height = 21
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
  object edtAngle: TEdit
    Left = 83
    Top = 16
    Width = 81
    Height = 21
    TabOrder = 0
    OnChange = edtAngleChange
  end
  object rdbtnClockwiseRotate: TRadioButton
    Left = 20
    Top = 46
    Width = 78
    Height = 13
    Cursor = crHandPoint
    Caption = 'Clockwise'
    Checked = True
    TabOrder = 1
    TabStop = True
    OnClick = ChangeRotateDirection
  end
  object rdbtnCounterclockwiseRotate: TRadioButton
    Left = 111
    Top = 46
    Width = 114
    Height = 13
    Cursor = crHandPoint
    Caption = 'Counterclockwise'
    TabOrder = 2
    OnClick = ChangeRotateDirection
  end
  object btbtnOK: TBitBtn
    Left = 245
    Top = 13
    Width = 61
    Height = 20
    Cursor = crHandPoint
    TabOrder = 3
    OnClick = btbtnOKClick
    Kind = bkOK
  end
  object btbtnCancel: TBitBtn
    Left = 245
    Top = 46
    Width = 61
    Height = 20
    Cursor = crHandPoint
    TabOrder = 4
    Kind = bkCancel
  end
end
