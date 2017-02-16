object frmColor: TfrmColor
  Left = 544
  Top = 458
  Width = 236
  Height = 160
  BorderStyle = bsSizeToolWin
  Caption = 'Color'
  Color = clBtnFace
  DragKind = dkDock
  DragMode = dmAutomatic
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object pnlSpectra: TPanel
    Left = 0
    Top = 96
    Width = 228
    Height = 20
    Align = alBottom
    BevelInner = bvLowered
    TabOrder = 0
    object imgSpectra: TImage
      Left = 2
      Top = 2
      Width = 224
      Height = 16
      Align = alClient
      OnMouseDown = imgSpectraMouseDown
      OnMouseMove = imgSpectraMouseMove
    end
  end
  object pnlColor: TPanel
    Left = 0
    Top = 0
    Width = 228
    Height = 96
    Align = alClient
    BevelInner = bvLowered
    DragMode = dmAutomatic
    TabOrder = 1
    object lblRValue: TLabel
      Left = 60
      Top = 7
      Width = 14
      Height = 13
      Caption = 'R:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblGValue: TLabel
      Left = 60
      Top = 27
      Width = 14
      Height = 13
      Caption = 'G:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGreen
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblBValue: TLabel
      Left = 60
      Top = 46
      Width = 13
      Height = 13
      Caption = 'B:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object shpBackgroundColor: TShape
      Left = 26
      Top = 25
      Width = 20
      Height = 20
      Cursor = crHandPoint
      OnMouseDown = shpBackgroundColorMouseDown
    end
    object shpForegroundColor: TShape
      Left = 13
      Top = 12
      Width = 20
      Height = 20
      Cursor = crHandPoint
      OnMouseDown = shpForegroundColorMouseDown
    end
    object lblGrayPercent: TLabel
      Left = 203
      Top = 7
      Width = 8
      Height = 13
      Caption = '%'
      Visible = False
    end
    object edtRValue: TEdit
      Left = 169
      Top = 5
      Width = 30
      Height = 16
      AutoSize = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      Text = '0'
      OnChange = edtRValueChange
      OnKeyPress = ColorValueKeyPress
    end
    object edtGValue: TEdit
      Left = 169
      Top = 25
      Width = 30
      Height = 16
      AutoSize = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGreen
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
      Text = '0'
      OnChange = edtGValueChange
      OnKeyPress = ColorValueKeyPress
    end
    object edtBValue: TEdit
      Left = 169
      Top = 44
      Width = 30
      Height = 16
      AutoSize = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
      Text = '0'
      OnChange = edtBValueChange
      OnKeyPress = ColorValueKeyPress
    end
    object ggbrRValue: TGaugeBar
      Left = 80
      Top = 6
      Width = 85
      Height = 14
      Cursor = crHandPoint
      Backgnd = bgPattern
      Max = 255
      ShowHandleGrip = True
      Style = rbsMac
      Position = 0
      OnChange = ColorValueChange
    end
    object ggbrGValue: TGaugeBar
      Left = 80
      Top = 26
      Width = 85
      Height = 14
      Cursor = crHandPoint
      Backgnd = bgPattern
      Max = 255
      ShowHandleGrip = True
      Style = rbsMac
      Position = 0
      OnChange = ColorValueChange
    end
    object ggbrBValue: TGaugeBar
      Left = 80
      Top = 45
      Width = 85
      Height = 14
      Cursor = crHandPoint
      Backgnd = bgPattern
      Max = 255
      ShowHandleGrip = True
      Style = rbsMac
      Position = 0
      OnChange = ColorValueChange
    end
  end
end
