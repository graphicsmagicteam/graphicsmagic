object LinkForm: TLinkForm
  Left = 362
  Top = 259
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'PegtopLink Demo'
  ClientHeight = 66
  ClientWidth = 394
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
  object PegtopLink1: TPegtopLink
    Left = 8
    Top = 8
    Width = 377
    Height = 33
    Cursor = crDefault
    CaptionPrefix = 'This is a '
    CaptionSuffix = ' surrounded by some other text (one control only)'
    LinkCursor = crHandPoint
    Alignment = taCenter
    Caption = 'Link'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Shell Dlg 2'
    Font.Style = []
    LinkFont.Charset = ANSI_CHARSET
    LinkFont.Color = clBlue
    LinkFont.Height = -21
    LinkFont.Name = 'Impact'
    LinkFont.Style = [fsBold]
    ParentFont = False
    Visited = False
    OnStateChange = PegtopLink1StateChange
    OnVisit = PegtopLink1Visit
  end
  object PegtopWebLink1: TPegtopWebLink
    Left = 8
    Top = 48
    Width = 377
    Height = 13
    Cursor = crDefault
    CaptionPrefix = 'This is a weblink to '
    LinkCursor = crHandPoint
    Alignment = taCenter
    Caption = 'www.pegtop.net/delphi'
    LinkFont.Charset = DEFAULT_CHARSET
    LinkFont.Color = clBlue
    LinkFont.Height = -11
    LinkFont.Name = 'MS Shell Dlg 2'
    LinkFont.Style = []
    Visited = False
    OnStateChange = PegtopLink1StateChange
    Address = 'http://www.pegtop.net/delphi'
  end
end
