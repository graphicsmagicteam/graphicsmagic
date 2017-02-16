object MainForm: TMainForm
  Left = 240
  Top = 220
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'PegtopCommon Demo'
  ClientHeight = 362
  ClientWidth = 257
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
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 241
    Height = 25
    Caption = 'Color controls'
    TabOrder = 0
    OnClick = DemoButtonClick
  end
  object Button2: TButton
    Tag = 1
    Left = 8
    Top = 40
    Width = 241
    Height = 25
    Caption = 'File dialogs'
    TabOrder = 1
    OnClick = DemoButtonClick
  end
  object Button3: TButton
    Tag = 2
    Left = 8
    Top = 72
    Width = 241
    Height = 25
    Caption = 'Color gradients'
    TabOrder = 2
    OnClick = DemoButtonClick
  end
  object Button4: TButton
    Tag = 3
    Left = 8
    Top = 104
    Width = 241
    Height = 25
    Caption = 'Radio groups'
    TabOrder = 3
    OnClick = DemoButtonClick
  end
  object Button5: TButton
    Tag = 4
    Left = 8
    Top = 136
    Width = 241
    Height = 25
    Caption = 'Desktop magnifiers'
    TabOrder = 4
    OnClick = DemoButtonClick
  end
  object Button6: TButton
    Tag = 5
    Left = 8
    Top = 168
    Width = 241
    Height = 25
    Caption = 'Track bars'
    TabOrder = 5
    OnClick = DemoButtonClick
  end
  object Button7: TButton
    Tag = 6
    Left = 8
    Top = 200
    Width = 241
    Height = 25
    Caption = 'Progress bars'
    TabOrder = 6
    OnClick = DemoButtonClick
  end
  object Button8: TButton
    Tag = 7
    Left = 8
    Top = 232
    Width = 241
    Height = 25
    Caption = 'Links'
    TabOrder = 7
    OnClick = DemoButtonClick
  end
  object Button9: TButton
    Tag = 8
    Left = 8
    Top = 264
    Width = 241
    Height = 25
    Caption = 'Alarm schedules'
    TabOrder = 8
    OnClick = DemoButtonClick
  end
  object Button10: TButton
    Tag = 9
    Left = 8
    Top = 296
    Width = 241
    Height = 25
    Caption = 'System images'
    TabOrder = 9
    OnClick = DemoButtonClick
  end
  object Button11: TButton
    Tag = 10
    Left = 8
    Top = 328
    Width = 241
    Height = 25
    Caption = 'Labels'
    TabOrder = 10
    OnClick = DemoButtonClick
  end
end
