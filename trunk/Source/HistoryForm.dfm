object frmHistory: TfrmHistory
  Left = 189
  Top = 127
  Width = 244
  Height = 160
  BorderStyle = bsSizeToolWin
  Caption = 'History'
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
  object tlbrHistoryTools: TToolBar
    Left = 0
    Top = 90
    Width = 236
    Height = 26
    Align = alBottom
    Caption = 'tlbrHistoryTools'
    Flat = True
    Images = dmMain.imglstTools
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    object ToolButton1: TToolButton
      Left = 0
      Top = 0
      Width = 8
      Caption = 'ToolButton1'
      ImageIndex = 1
      Style = tbsSeparator
    end
    object tlbtnDeleteCurrentState: TToolButton
      Left = 8
      Top = 0
      Action = dmMain.actnDeleteCurrentState
    end
  end
end
