object SystemImageForm: TSystemImageForm
  Left = 265
  Top = 126
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'PegtopSystemImages Demo'
  ClientHeight = 282
  ClientWidth = 570
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefaultPosOnly
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ListView1: TListView
    Left = 8
    Top = 8
    Width = 233
    Height = 265
    Columns = <
      item
        Caption = 'Special folders'
        Width = 200
      end>
    ReadOnly = True
    SmallImages = PegtopSystemImages1
    TabOrder = 0
    ViewStyle = vsReport
  end
  object ListView2: TListView
    Left = 248
    Top = 8
    Width = 313
    Height = 265
    Columns = <
      item
        Caption = 'File types'
        Width = 200
      end
      item
        Caption = 'extension'
        Width = 80
      end>
    ReadOnly = True
    SmallImages = PegtopSystemImages1
    TabOrder = 1
    ViewStyle = vsReport
  end
  object PegtopSystemImages1: TPegtopSystemImages
    ImageSize = pisSmallIcons
    Left = 24
    Top = 40
  end
  object ApplicationEvents1: TApplicationEvents
    OnIdle = ApplicationEvents1Idle
    Left = 24
    Top = 72
  end
end
