object frmGhost: TfrmGhost
  Left = 381
  Top = 200
  AlphaBlend = True
  AlphaBlendValue = 30
  BorderStyle = bsNone
  Caption = 'frmGhost'
  ClientHeight = 488
  ClientWidth = 188
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object timerFade: TTimer
    Enabled = False
    Interval = 50
    OnTimer = timerFadeTimer
    Left = 72
    Top = 32
  end
end
