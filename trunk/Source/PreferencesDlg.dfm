object frmPreferences: TfrmPreferences
  Left = 192
  Top = 133
  BorderStyle = bsDialog
  Caption = 'Preferences'
  ClientHeight = 216
  ClientWidth = 490
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object btbtnOK: TBitBtn
    Left = 400
    Top = 8
    Width = 75
    Height = 25
    Cursor = crHandPoint
    TabOrder = 0
    OnClick = btbtnOKClick
    Kind = bkOK
  end
  object btbtnCancel: TBitBtn
    Left = 400
    Top = 40
    Width = 75
    Height = 25
    Cursor = crHandPoint
    TabOrder = 1
    Kind = bkCancel
  end
  object lstbxCategories: TListBox
    Left = 0
    Top = 0
    Width = 121
    Height = 216
    Align = alLeft
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = bsNone
    ItemHeight = 13
    Items.Strings = (
      'Interface'
      'Performance')
    TabOrder = 2
    OnClick = lstbxCategoriesClick
  end
  object ntbkPreferences: TNotebook
    Left = 121
    Top = 0
    Width = 264
    Height = 216
    Align = alLeft
    TabOrder = 3
    object TPage
      Left = 0
      Top = 0
      Caption = 'Interface'
      object pnlInterface: TPanel
        Left = 0
        Top = 0
        Width = 264
        Height = 216
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object pnlInterfaceGeneral: TGroupBox
          Left = 16
          Top = 8
          Width = 233
          Height = 113
          Caption = 'General'
          TabOrder = 0
          object lblGhostFadeInterval: TLabel
            Left = 32
            Top = 48
            Width = 65
            Height = 13
            Caption = 'Fade Interval:'
            Enabled = False
          end
          object lblGhostFadeIntervalUnit: TLabel
            Left = 160
            Top = 48
            Width = 13
            Height = 13
            Caption = 'ms'
            Enabled = False
          end
          object lblGhostMaxOpaque: TLabel
            Left = 32
            Top = 72
            Width = 64
            Height = 13
            Caption = 'Max Opaque:'
            Enabled = False
          end
          object lblGhostMaxOpaquePercent: TLabel
            Left = 160
            Top = 72
            Width = 8
            Height = 13
            Caption = '%'
            Enabled = False
          end
          object chckbxGhostMode: TCheckBox
            Left = 8
            Top = 24
            Width = 145
            Height = 17
            Caption = 'Ghost Mode'
            TabOrder = 0
            OnClick = chckbxGhostModeClick
          end
          object edtGhostFadeInterval: TEdit
            Left = 104
            Top = 44
            Width = 50
            Height = 21
            Enabled = False
            TabOrder = 1
            OnChange = edtGhostFadeIntervalChange
          end
          object edtGhostMaxOpaque: TEdit
            Left = 104
            Top = 68
            Width = 49
            Height = 21
            Enabled = False
            TabOrder = 2
            OnChange = edtGhostMaxOpaqueChange
          end
        end
      end
    end
    object TPage
      Left = 0
      Top = 0
      Caption = 'Performance'
      object pnlPerformance: TPanel
        Left = 0
        Top = 0
        Width = 264
        Height = 216
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object grpbxHistory: TGroupBox
          Left = 16
          Top = 8
          Width = 185
          Height = 57
          Caption = 'History'
          TabOrder = 0
          object lblHistoryStates: TLabel
            Left = 16
            Top = 24
            Width = 68
            Height = 13
            Caption = 'History States:'
          end
          object edtHistoryStates: TEdit
            Left = 96
            Top = 20
            Width = 49
            Height = 21
            TabOrder = 0
            OnChange = edtHistoryStatesChange
          end
        end
      end
    end
  end
end
