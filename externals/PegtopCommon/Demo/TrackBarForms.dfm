object TrackBarForm: TTrackBarForm
  Left = 268
  Top = 233
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'PegtopTrackBar Demo'
  ClientHeight = 217
  ClientWidth = 409
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefaultPosOnly
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 176
    Width = 353
    Height = 33
    AutoSize = False
    Caption = 
      'Try to edit the numbers by clicking at them. Also try the built ' +
      'in context menu (rightclick).'
    WordWrap = True
  end
  object PegtopTrackBar1: TPegtopTrackBar
    Left = 8
    Top = 16
    Width = 353
    Height = 32
    Cursor = crDefault
    TabOrder = 0
    LabelCaption = 'Position: <pos> (Options includes ploExplicitSign)'
    LabelMode = plmPos
    LabelParam = 1
    LabelOptions = [ploVisible, ploPlusMinusZero, ploExplicitSign]
    Min = -100
    SmallChange = 5
    Position = 0
  end
  object PegtopTrackBar2: TPegtopTrackBar
    Left = 8
    Top = 56
    Width = 353
    Height = 32
    Cursor = crDefault
    TabOrder = 1
    LabelCaption = 'Position: <pos> (LabelMode = plmBin)'
    LabelMode = plmBin
    LabelParam = 1
    LabelOptions = [ploVisible]
    Max = 10
    Position = 5
  end
  object PegtopRangeBar1: TPegtopRangeBar
    Left = 8
    Top = 96
    Width = 353
    Height = 32
    Cursor = crDefault
    Hint = 'Test hint'
    TabOrder = 2
    LabelCaption = 'Position: <min> % - <max> %'
    LabelMin = 'low'
    LabelMax = 'high'
    LabelMode = plmPos
    LabelParam = 1
    LabelOptions = [ploVisible]
    RangeOptions = []
    ParentShowHint = False
    ShowHint = True
    PositionMin = 25
    PositionMax = 75
  end
  object PegtopColorTrackBar1: TPegtopColorTrackBar
    Left = 8
    Top = 136
    Width = 353
    Height = 32
    Cursor = crDefault
    TabOrder = 3
    LabelCaption = 'Position: <pos>'
    LabelMin = 'black'
    LabelMax = 'white'
    LabelMode = plmShr
    LabelParam = 1
    LabelOptions = [ploVisible, ploHint, ploFlip]
    Max = 255
    OnChange = PegtopColorTrackBar1Change
    Color = clBtnFace
    ParentColor = False
    Position = 128
    ButtonColor = clGray
  end
  object PegtopTrackBar3: TPegtopTrackBar
    Left = 368
    Top = 8
    Width = 33
    Height = 201
    Cursor = crDefault
    TabOrder = 4
    LabelCaption = 'Position: <pos>'
    LabelMin = 'min'
    LabelMax = 'max'
    LabelMode = plmPos
    LabelParam = 1
    LabelOptions = [ploVisible, ploFlip, ploRotate]
    Orientation = psoVertical
    Position = 10
  end
end
