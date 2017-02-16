object frmGradientMap: TfrmGradientMap
  Left = 189
  Top = 131
  BorderStyle = bsDialog
  Caption = 'Gradient Map'
  ClientHeight = 187
  ClientWidth = 460
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
  PixelsPerInch = 96
  TextHeight = 13
  object grpbxGradientMapHolder: TGroupBox
    Left = 16
    Top = 16
    Width = 353
    Height = 81
    Caption = 'Gradient Used for Grayscale Mapping:'
    TabOrder = 0
    object spdbtnOpenGradientPicker: TSpeedButton
      Left = 317
      Top = 24
      Width = 18
      Height = 41
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
    object pnlGradientMapHolder: TPanel
      Left = 16
      Top = 24
      Width = 300
      Height = 41
      BevelInner = bvLowered
      TabOrder = 0
      object imgSelectedGradient: TImage32
        Left = 2
        Top = 2
        Width = 296
        Height = 37
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
      end
    end
  end
  object grpbxGradientOptions: TGroupBox
    Left = 16
    Top = 112
    Width = 353
    Height = 57
    Caption = 'Gradient Options:'
    TabOrder = 1
    object chckbxReverse: TCheckBox
      Left = 16
      Top = 24
      Width = 97
      Height = 17
      Cursor = crHandPoint
      Caption = 'Reverse'
      TabOrder = 0
      OnClick = chckbxReverseClick
    end
  end
  object btbtnOK: TBitBtn
    Left = 384
    Top = 24
    Width = 60
    Height = 20
    Cursor = crHandPoint
    TabOrder = 2
    OnClick = btbtnOKClick
    Kind = bkOK
  end
  object btbtnCancel: TBitBtn
    Left = 384
    Top = 56
    Width = 60
    Height = 20
    Cursor = crHandPoint
    TabOrder = 3
    Kind = bkCancel
  end
  object chckbxPreview: TCheckBox
    Left = 384
    Top = 96
    Width = 60
    Height = 17
    Cursor = crHandPoint
    Caption = 'Preview'
    Checked = True
    State = cbChecked
    TabOrder = 4
    OnClick = chckbxPreviewClick
  end
end
