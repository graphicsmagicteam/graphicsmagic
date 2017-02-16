object AlarmScheduleForm: TAlarmScheduleForm
  Left = 315
  Top = 193
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'PegtopAlarmSchedule Demo'
  ClientHeight = 179
  ClientWidth = 641
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefaultPosOnly
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 104
    Width = 121
    Height = 13
    AutoSize = False
  end
  object Button1: TButton
    Tag = 2
    Left = 8
    Top = 8
    Width = 121
    Height = 25
    Caption = 'Alarm in 2 seconds'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 136
    Top = 8
    Width = 161
    Height = 161
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object Button2: TButton
    Tag = 5
    Left = 8
    Top = 40
    Width = 121
    Height = 25
    Caption = 'Alarm in 5 seconds'
    TabOrder = 2
    OnClick = Button1Click
  end
  object Button3: TButton
    Tag = 10
    Left = 8
    Top = 72
    Width = 121
    Height = 25
    Caption = 'Alarm in 10 seconds'
    TabOrder = 3
    OnClick = Button1Click
  end
  object Memo2: TMemo
    Left = 304
    Top = 8
    Width = 161
    Height = 161
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 4
  end
  object Memo3: TMemo
    Left = 472
    Top = 8
    Width = 161
    Height = 161
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 5
  end
  object PegtopAlarmSchedule1: TPegtopAlarmSchedule
    OnAlarm = PegtopAlarmSchedule1Alarm
    OnRemove = PegtopAlarmSchedule1Remove
    Enabled = True
    Left = 88
    Top = 144
  end
end
