object frmBrushDynamics: TfrmBrushDynamics
  Left = 192
  Top = 131
  BorderStyle = bsNone
  Caption = 'frmBrushDynamics'
  ClientHeight = 145
  ClientWidth = 274
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDeactivate = FormDeactivate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlBrushDynamics: TPanel
    Left = 0
    Top = 0
    Width = 274
    Height = 145
    Align = alClient
    BevelInner = bvRaised
    TabOrder = 0
    object grpbxBrushDynamics: TGroupBox
      Left = 8
      Top = 8
      Width = 257
      Height = 129
      Caption = 'Brush Dynamics:'
      TabOrder = 0
      object lblSizeDynamics: TLabel
        Left = 12
        Top = 32
        Width = 23
        Height = 13
        Caption = 'Size:'
      end
      object lblSizeSteps: TLabel
        Left = 216
        Top = 32
        Width = 27
        Height = 13
        Caption = 'Steps'
      end
      object lblOpacityDynamics: TLabel
        Left = 12
        Top = 64
        Width = 51
        Height = 13
        Caption = 'Tolerance:'
      end
      object lblOpacitySteps: TLabel
        Left = 216
        Top = 64
        Width = 27
        Height = 13
        Caption = 'Steps'
      end
      object lblColorDynamics: TLabel
        Left = 12
        Top = 96
        Width = 27
        Height = 13
        Caption = 'Color:'
      end
      object lblColorSteps: TLabel
        Left = 216
        Top = 96
        Width = 27
        Height = 13
        Caption = 'Steps'
      end
      object cmbbxSizeDynamics: TComboBox
        Left = 72
        Top = 28
        Width = 80
        Height = 21
        Cursor = crHandPoint
        ItemHeight = 13
        TabOrder = 0
        OnChange = cmbbxSizeDynamicsChange
        Items.Strings = (
          'Off'
          'Fade')
      end
      object edtSizeSteps: TEdit
        Left = 160
        Top = 28
        Width = 50
        Height = 21
        TabOrder = 1
        OnChange = edtSizeStepsChange
      end
      object cmbbxOpacityDynamics: TComboBox
        Left = 72
        Top = 60
        Width = 80
        Height = 21
        Cursor = crHandPoint
        ItemHeight = 13
        TabOrder = 2
        OnChange = cmbbxOpacityDynamicsChange
        Items.Strings = (
          'Off'
          'Fade')
      end
      object edtOpacitySteps: TEdit
        Left = 160
        Top = 60
        Width = 50
        Height = 21
        TabOrder = 3
        OnChange = edtOpacityStepsChange
      end
      object cmbbxColorDynamics: TComboBox
        Left = 72
        Top = 92
        Width = 80
        Height = 21
        Cursor = crHandPoint
        ItemHeight = 13
        TabOrder = 4
        OnChange = cmbbxColorDynamicsChange
        Items.Strings = (
          'Off'
          'Fade')
      end
      object edtColorSteps: TEdit
        Left = 160
        Top = 92
        Width = 50
        Height = 21
        TabOrder = 5
        OnChange = edtColorStepsChange
      end
    end
  end
end
