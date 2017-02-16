object frmPaths: TfrmPaths
  Left = 193
  Top = 132
  Width = 236
  Height = 260
  BorderStyle = bsSizeToolWin
  Caption = 'Paths'
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
  object tlbrPathTools: TToolBar
    Left = 0
    Top = 192
    Width = 228
    Height = 24
    Align = alBottom
    AutoSize = True
    Caption = 'tlbrPathTools'
    Flat = True
    Images = dmMain.imglstTools
    TabOrder = 0
    object ToolButton1: TToolButton
      Left = 0
      Top = 0
      Width = 8
      Caption = 'ToolButton1'
      Style = tbsSeparator
    end
    object ToolButton2: TToolButton
      Left = 8
      Top = 0
      Cursor = crHandPoint
      Action = dmMain.actnFillPath
      ParentShowHint = False
      ShowHint = True
    end
    object ToolButton3: TToolButton
      Left = 31
      Top = 0
      Cursor = crHandPoint
      Action = dmMain.actnStrokePath
      ParentShowHint = False
      ShowHint = True
    end
    object ToolButton4: TToolButton
      Left = 54
      Top = 0
      Cursor = crHandPoint
      Action = dmMain.actnLoadPathAsSelection
      ParentShowHint = False
      ShowHint = True
    end
    object tlbtnCreateNewPath: TToolButton
      Left = 77
      Top = 0
      Cursor = crHandPoint
      Action = dmMain.actnCreateNewPath
      ParentShowHint = False
      ShowHint = True
    end
    object tlbtnDeleteCurrentPath: TToolButton
      Left = 100
      Top = 0
      Cursor = crHandPoint
      Action = dmMain.actnDeleteCurrentPath
      ParentShowHint = False
      ShowHint = True
    end
  end
end
