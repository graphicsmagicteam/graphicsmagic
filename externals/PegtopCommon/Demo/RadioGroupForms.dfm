object RadioGroupForm: TRadioGroupForm
  Left = 431
  Top = 272
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'PegtopRadioGroup Demo'
  ClientHeight = 473
  ClientWidth = 521
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
  object PegtopRadioGroup1: TPegtopRadioGroup
    Left = 8
    Top = 8
    Width = 249
    Height = 121
    Caption = 'PegtopRadioGroup1'
    ItemIndex = 0
    Items.Strings = (
      'These items are top aligned'
      'ButtonAlignment = praTop'
      'Looks pretty cool')
    TabOrder = 0
    ButtonAlignment = praTop
    ButtonHeight = 20
  end
  object PegtopRadioGroup2: TPegtopRadioGroup
    Left = 264
    Top = 8
    Width = 249
    Height = 121
    Caption = 'PegtopRadioGroup2'
    ItemIndex = 0
    Items.Strings = (
      'These items are block aligned'
      'ButtonAlignment = praBlock'
      'Just like standard TRadioGroup')
    TabOrder = 1
    ButtonAlignment = praBlock
    ButtonHeight = 20
  end
  object PegtopRadioGroup3: TPegtopRadioGroup
    Left = 8
    Top = 136
    Width = 249
    Height = 121
    Caption = 'PegtopRadioGroup3'
    ItemIndex = 0
    Items.Strings = (
      'These items are bottom aligned'
      'ButtonAlignment = praBottom'
      'Impossible with standard TRadioGroup, too')
    TabOrder = 2
    ButtonAlignment = praBottom
    ButtonHeight = 20
  end
  object PegtopRadioGroup4: TPegtopRadioGroup
    Left = 264
    Top = 136
    Width = 249
    Height = 121
    Caption = 'PegtopRadioGroup4'
    ItemIndex = 0
    Items.Strings = (
      'These items are centered'
      'ButtonAlignment = praCenter'
      'Another cute alignment')
    TabOrder = 3
    ButtonAlignment = praCenter
    ButtonHeight = 20
  end
  object PegtopRadioGroup5: TPegtopRadioGroup
    Left = 8
    Top = 264
    Width = 249
    Height = 121
    Caption = 'PegtopRadioGroup5'
    ItemIndex = 0
    Items.Strings = (
      'These lines are very narrow'
      'ButtonAlignment = praTop again'
      'ButtonHeight = 14 (instead of 20)'
      'Everything is possible')
    TabOrder = 4
    ButtonAlignment = praTop
    ButtonHeight = 14
  end
  object PegtopRadioGroup6: TPegtopRadioGroup
    Left = 264
    Top = 264
    Width = 249
    Height = 121
    Caption = 'PegtopRadioGroup6'
    ItemIndex = 0
    Items.Strings = (
      'Any lines can be formatted'
      'or disabled'
      'if you don'#39't want the user'
      'to click on certain items'
      '(only available at runtime)')
    TabOrder = 5
    ButtonAlignment = praTop
    ButtonHeight = 20
  end
  object PegtopRadioGroup7: TPegtopRadioGroup
    Left = 8
    Top = 392
    Width = 505
    Height = 73
    Caption = 'PegtopRadioGroup7'
    Columns = 2
    ItemIndex = 0
    Items.Strings = (
      'Multiple columns'
      'are possible, too'
      'of course')
    TabOrder = 6
    ButtonAlignment = praTop
    ButtonHeight = 20
  end
end
