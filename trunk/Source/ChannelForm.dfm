object frmChannels: TfrmChannels
  Left = 192
  Top = 134
  Width = 313
  Height = 192
  BorderStyle = bsSizeToolWin
  Caption = 'Channels'
  Color = clBtnFace
  Constraints.MinHeight = 160
  Constraints.MinWidth = 236
  DragKind = dkDock
  DragMode = dmAutomatic
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object tlbrChannelTools: TToolBar
    Left = 0
    Top = 124
    Width = 305
    Height = 24
    Align = alBottom
    AutoSize = True
    Caption = 'tlbrChannelTools'
    Flat = True
    Images = dmMain.imglstTools
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    object tlbtnSeparator1: TToolButton
      Left = 0
      Top = 0
      Width = 8
      Caption = 'tlbtnSeparator1'
      Style = tbsSeparator
    end
    object tlbtnLoadChannelAsSelection: TToolButton
      Left = 8
      Top = 0
      Action = dmMain.actnLoadChannelAsSelection
    end
    object tlbtnSaveSelectionAsChannel: TToolButton
      Left = 31
      Top = 0
      Action = dmMain.actnSaveSelectionAsChannel
    end
    object tlbtnNewAlphaChannel: TToolButton
      Left = 54
      Top = 0
      Action = dmMain.actnNewAlphaChannel
    end
    object tlbtnDeleteAlphaChannel: TToolButton
      Left = 77
      Top = 0
      Action = dmMain.actnDeleteChannel
    end
  end
end
