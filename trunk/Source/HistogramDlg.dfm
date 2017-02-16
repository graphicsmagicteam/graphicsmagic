object frmHistogram: TfrmHistogram
  Left = 190
  Top = 131
  ActiveControl = btbtnOK
  BorderStyle = bsDialog
  Caption = 'Histogram'
  ClientHeight = 369
  ClientWidth = 381
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object btbtnOK: TBitBtn
    Left = 304
    Top = 20
    Width = 61
    Height = 20
    Cursor = crHandPoint
    TabOrder = 0
    OnClick = btbtnOKClick
    Kind = bkOK
  end
  object grpbxStatistics: TGroupBox
    Left = 8
    Top = 184
    Width = 278
    Height = 177
    TabOrder = 1
    object lblMinimum: TLabel
      Left = 60
      Top = 20
      Width = 44
      Height = 13
      Caption = 'Minimum:'
    end
    object lblMaximum: TLabel
      Left = 57
      Top = 36
      Width = 47
      Height = 13
      Caption = 'Maximum:'
    end
    object lblMode: TLabel
      Left = 74
      Top = 52
      Width = 30
      Height = 13
      Caption = 'Mode:'
    end
    object lblMedian: TLabel
      Left = 66
      Top = 68
      Width = 38
      Height = 13
      Caption = 'Median:'
    end
    object lblMean: TLabel
      Left = 74
      Top = 84
      Width = 30
      Height = 13
      Caption = 'Mean:'
    end
    object lblStandardDeviation: TLabel
      Left = 10
      Top = 100
      Width = 94
      Height = 13
      Caption = 'Standard Deviation:'
    end
    object lblExcessKurtosis: TLabel
      Left = 27
      Top = 116
      Width = 77
      Height = 13
      Caption = 'Excess Kurtosis:'
    end
    object lblColors: TLabel
      Left = 75
      Top = 132
      Width = 29
      Height = 13
      Caption = 'Colors'
    end
    object lblPixels: TLabel
      Left = 77
      Top = 148
      Width = 27
      Height = 13
      Caption = 'Pixels'
    end
    object lblMinimumValue: TLabel
      Left = 120
      Top = 20
      Width = 68
      Height = 13
      Caption = 'MinimumValue'
    end
    object lblMaximumValue: TLabel
      Left = 120
      Top = 36
      Width = 71
      Height = 13
      Caption = 'MaximumValue'
    end
    object lblModeValue: TLabel
      Left = 120
      Top = 52
      Width = 54
      Height = 13
      Caption = 'ModeValue'
    end
    object lblMedianValue: TLabel
      Left = 120
      Top = 68
      Width = 62
      Height = 13
      Caption = 'MedianValue'
    end
    object lblMeanValue: TLabel
      Left = 120
      Top = 84
      Width = 54
      Height = 13
      Caption = 'MeanValue'
    end
    object lblStandardDeviationValue: TLabel
      Left = 120
      Top = 100
      Width = 115
      Height = 13
      Caption = 'StandardDeviationValue'
    end
    object lblExcessKurtosisValue: TLabel
      Left = 120
      Top = 116
      Width = 98
      Height = 13
      Caption = 'ExcessKurtosisValue'
    end
    object lblColorsValue: TLabel
      Left = 120
      Top = 132
      Width = 66
      Height = 13
      Caption = 'lblColorsValue'
    end
    object lblPixelsValue: TLabel
      Left = 120
      Top = 148
      Width = 64
      Height = 13
      Caption = 'lblPixelsValue'
    end
  end
  object grpbxHistogram: TGroupBox
    Left = 8
    Top = 8
    Width = 278
    Height = 169
    TabOrder = 2
    object lblChannel: TLabel
      Left = 27
      Top = 20
      Width = 42
      Height = 13
      Caption = 'Channel:'
    end
    object imgHistogram: TImage
      Left = 10
      Top = 45
      Width = 256
      Height = 84
      AutoSize = True
    end
    object imgColorBar: TImage
      Left = 10
      Top = 136
      Width = 256
      Height = 16
      AutoSize = True
    end
    object cmbbxChannel: TComboBox
      Left = 74
      Top = 16
      Width = 118
      Height = 21
      Cursor = crHandPoint
      ItemHeight = 13
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      OnChange = cmbbxChannelChange
      Items.Strings = (
        'Luminosity'
        'Red'
        'Green'
        'Blue'
        'Intensity')
    end
  end
end
