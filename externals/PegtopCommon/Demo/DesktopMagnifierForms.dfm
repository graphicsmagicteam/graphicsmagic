object DesktopMagnifierForm: TDesktopMagnifierForm
  Left = 303
  Top = 216
  Width = 407
  Height = 337
  Caption = 'PegtopDesktopMagnifier Demo'
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
  object PegtopDesktopMagnifier1: TPegtopDesktopMagnifier
    Left = 0
    Top = 0
    Width = 264
    Height = 303
    Options = [pmoFollowCursor]
    CrossHairs = pmcOpenCross
    IndicatorTopLeft = pmiCaption
    IndicatorTopRight = pmiZoom
    IndicatorBottomLeft = pmiCursorPos
    IndicatorBottomRight = pmiHexCode
    OnTargetColorChange = PegtopDesktopMagnifier1TargetColorChange
    Align = alClient
    Caption = 'Your magnifier'
    Interval = 50
    IntervalForce = 500
  end
  object Panel1: TPanel
    Left = 264
    Top = 0
    Width = 135
    Height = 303
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 0
    object PegtopTrackBar1: TPegtopTrackBar
      Left = 8
      Top = 8
      Width = 120
      Height = 32
      Cursor = crDefault
      TabOrder = 0
      LabelCaption = 'Zoom: <pos> %'
      LabelMode = plmMul
      LabelParam = 100
      LabelOptions = [ploVisible]
      Min = 1
      Max = 9
      OnChange = PegtopTrackBar1Change
      Position = 5
    end
    object PegtopCheckBox1: TPegtopCheckBox
      Left = 8
      Top = 48
      Width = 121
      Height = 17
      Caption = 'Draw cursor'
      TabOrder = 1
      OnClick = PegtopCheckBox1Click
    end
    object Panel2: TPanel
      Left = 8
      Top = 72
      Width = 121
      Height = 57
      BevelOuter = bvNone
      Caption = 'Target color'
      TabOrder = 2
      Visible = False
    end
  end
end
