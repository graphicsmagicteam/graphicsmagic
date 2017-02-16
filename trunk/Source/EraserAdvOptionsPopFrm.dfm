object frmEraserAdvancedOptions: TfrmEraserAdvancedOptions
  Left = 216
  Top = 160
  BorderStyle = bsNone
  Caption = 'frmEraserAdvancedOptions'
  ClientHeight = 147
  ClientWidth = 186
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnDeactivate = FormDeactivate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlEraserAdvancedOptions: TPanel
    Left = 0
    Top = 0
    Width = 186
    Height = 147
    Align = alClient
    BevelInner = bvRaised
    TabOrder = 0
    object grpbxEraserAdvancedOptions: TGroupBox
      Left = 8
      Top = 8
      Width = 170
      Height = 130
      Caption = 'Eraser Advanced Options:'
      TabOrder = 0
      object chckbxEraserHistory: TCheckBox
        Left = 16
        Top = 24
        Width = 97
        Height = 17
        Cursor = crHandPoint
        Hint = 'Erases area from designated history state'
        Caption = 'Erase to History'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
      end
      object chckbxProtectForeground: TCheckBox
        Left = 16
        Top = 48
        Width = 145
        Height = 17
        Cursor = crHandPoint
        Hint = 'Do not erase the foreground swatch color'
        Caption = 'Protect Foreground Color'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
      end
      object chckbxContiguous: TCheckBox
        Left = 16
        Top = 72
        Width = 97
        Height = 17
        Cursor = crHandPoint
        Hint = 'Erase only contiguous pixels'
        Caption = 'Contiguous'
        Checked = True
        ParentShowHint = False
        ShowHint = True
        State = cbChecked
        TabOrder = 2
      end
      object chckbxUseAllLayers: TCheckBox
        Left = 16
        Top = 96
        Width = 97
        Height = 17
        Cursor = crHandPoint
        Hint = 'Use merged data to determine areas to erase'
        Caption = 'Use All Layers'
        Checked = True
        ParentShowHint = False
        ShowHint = True
        State = cbChecked
        TabOrder = 3
      end
    end
  end
end
