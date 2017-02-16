object frmGradientPicker: TfrmGradientPicker
  Left = 191
  Top = 129
  BorderStyle = bsNone
  Caption = 'Gradient Picker'
  ClientHeight = 233
  ClientWidth = 331
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDeactivate = FormDeactivate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlFormBorder: TPanel
    Left = 0
    Top = 0
    Width = 331
    Height = 233
    Align = alClient
    BevelInner = bvRaised
    TabOrder = 0
    object spdbtnGradientOptions: TSpeedButton
      Left = 259
      Top = 21
      Width = 61
      Height = 18
      Caption = 'Options'
      Glyph.Data = {
        F6000000424DF600000000000000360000002800000008000000080000000100
        180000000000C000000000000000000000000000000000000000FFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF404040404040FFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFF404040000000000000404040FFFFFFFFFFFFFFFFFF404040
        000000000000000000000000404040FFFFFF4040400000000000000000000000
        00000000000000404040FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
      Layout = blGlyphRight
      OnClick = spdbtnGradientOptionsClick
    end
    object scrlbxGradientPicker: TScrollBox
      Left = 2
      Top = 2
      Width = 247
      Height = 205
      Align = alLeft
      TabOrder = 0
      object ggGradients: TgmGradientsGrid
        Left = 0
        Top = 0
        Width = 192
        Height = 192
        AutoSize = True
        OnMouseDown = ggGradientsMouseDown
        OnMouseMove = ggGradientsMouseMove
        OnMouseUp = ggGradientsMouseUp
        GradientIndex = -1
      end
    end
    object stsbrGradientInfo: TStatusBar
      Left = 2
      Top = 207
      Width = 327
      Height = 24
      Panels = <
        item
          Width = 50
        end>
    end
  end
  object pmnGradientOptions: TPopupMenu
    OnPopup = pmnGradientOptionsPopup
    Left = 96
    Top = 16
    object pmnitmNewGradient: TMenuItem
      Caption = 'New Gradient...'
      OnClick = pmnitmNewGradientClick
    end
    object pmnitmSeparator3: TMenuItem
      Caption = '-'
    end
    object pmnitmRenameGradient: TMenuItem
      Caption = 'Rename Gradient...'
      OnClick = pmnitmRenameGradientClick
    end
    object pmnitmDeleteGradient: TMenuItem
      Caption = 'Delete Gradient'
      OnClick = pmnitmDeleteGradientClick
    end
    object pmnitmSeparator2: TMenuItem
      Caption = '-'
    end
    object pmnitmSmallThumbnail: TMenuItem
      Caption = 'Small Thumbnail'
      OnClick = ChangeGradientThumbnailSize
    end
    object pmnitmLargeThumbnail: TMenuItem
      Caption = 'Large Thumbnail'
      OnClick = ChangeGradientThumbnailSize
    end
    object pmnitmSeparator1: TMenuItem
      Caption = '-'
    end
    object pmnitmResetGradients: TMenuItem
      Caption = 'Reset Gradients'
      OnClick = pmnitmResetGradientsClick
    end
    object pmnitmLoadGradients: TMenuItem
      Caption = 'Load Gradients...'
      OnClick = pmnitmLoadGradientsClick
    end
    object pmnitmSaveChanges: TMenuItem
      Caption = 'Save Changes'
      OnClick = pmnitmSaveChangesClick
    end
    object pmnitmSaveGradientsAs: TMenuItem
      Caption = 'Save Gradients As...'
      OnClick = pmnitmSaveGradientsAsClick
    end
    object pmnitmReplaceGradients: TMenuItem
      Caption = 'Replace Gradients...'
      OnClick = pmnitmReplaceGradientsClick
    end
  end
  object glDefaultGradients: TgmGradientList
    Gradients = <
      item
        DisplayName = 'Foreground to Background'
        AlphaGradient = <
          item
            Value = clWhite
          end
          item
            Value = clWhite
            LocationScale = 1.000000000000000000
          end>
        RGBGradient = <
          item
            Value = clDefault
          end
          item
            Value = clBackground
            LocationScale = 1.000000000000000000
          end>
      end
      item
        DisplayName = 'Foreground to Transparent'
        AlphaGradient = <
          item
            Value = clWhite
          end
          item
            Value = clBlack
            LocationScale = 1.000000000000000000
          end>
        RGBGradient = <
          item
            Value = clDefault
          end
          item
            Value = clDefault
            LocationScale = 1.000000000000000000
          end>
      end
      item
        DisplayName = 'Black, White'
        AlphaGradient = <
          item
            Value = clWhite
          end
          item
            Value = clWhite
            LocationScale = 1.000000000000000000
          end>
        RGBGradient = <
          item
            Value = clBlack
          end
          item
            Value = clWhite
            LocationScale = 1.000000000000000000
          end>
      end
      item
        DisplayName = 'Red, Green'
        AlphaGradient = <
          item
            Value = clWhite
          end
          item
            Value = clWhite
            LocationScale = 1.000000000000000000
          end>
        RGBGradient = <
          item
            Value = 1638625
          end
          item
            Value = 1794048
            LocationScale = 1.000000000000000000
          end>
      end
      item
        DisplayName = 'Violet, Orange'
        AlphaGradient = <
          item
            Value = clWhite
          end
          item
            Value = clWhite
            LocationScale = 1.000000000000000000
          end>
        RGBGradient = <
          item
            Value = 5835305
          end
          item
            Value = 31999
            LocationScale = 1.000000000000000000
          end>
      end
      item
        DisplayName = 'Blue, Red, Yellow'
        AlphaGradient = <
          item
            Value = clWhite
          end
          item
            Value = clWhite
            LocationScale = 1.000000000000000000
          end>
        RGBGradient = <
          item
            Value = 11665418
          end
          item
            Value = clRed
            LocationScale = 0.500000000000000000
          end
          item
            Value = 64767
            LocationScale = 1.000000000000000000
          end>
      end
      item
        DisplayName = 'Blue, Yellow, Blue'
        AlphaGradient = <
          item
            Value = clWhite
          end
          item
            Value = clWhite
            LocationScale = 1.000000000000000000
          end>
        RGBGradient = <
          item
            Value = 11993355
            LocationScale = 0.100097656250000000
          end
          item
            Value = 129532
            LocationScale = 0.500000000000000000
          end
          item
            Value = 11075850
            LocationScale = 0.899902343750000000
          end>
      end
      item
        DisplayName = 'Orange, Yellow, Orange'
        AlphaGradient = <
          item
            Value = clWhite
          end
          item
            Value = clWhite
            LocationScale = 1.000000000000000000
          end>
        RGBGradient = <
          item
            Value = 93694
          end
          item
            Value = clYellow
            LocationScale = 0.500000000000000000
          end
          item
            Value = 28159
            LocationScale = 1.000000000000000000
            MidPoint = 0.469999998807907100
          end>
      end
      item
        DisplayName = 'Violet, Green, Orange'
        AlphaGradient = <
          item
            Value = clWhite
          end
          item
            Value = clWhite
            LocationScale = 1.000000000000000000
          end>
        RGBGradient = <
          item
            Value = 7083375
          end
          item
            Value = 1794048
            LocationScale = 0.500000000000000000
          end
          item
            Value = 31997
            LocationScale = 1.000000000000000000
          end>
      end
      item
        DisplayName = 'Yellow, Violet, Orange, Blue'
        AlphaGradient = <
          item
            Value = clWhite
          end
          item
            Value = clWhite
            LocationScale = 1.000000000000000000
          end>
        RGBGradient = <
          item
            Value = 59129
            LocationScale = 0.050048828125000000
          end
          item
            Value = 7083375
            LocationScale = 0.350097656250000000
          end
          item
            Value = 31997
            LocationScale = 0.649902343750000000
          end
          item
            Value = 7612416
            LocationScale = 0.949951171875000100
          end>
      end
      item
        DisplayName = 'Copper'
        AlphaGradient = <
          item
            Value = clWhite
          end
          item
            Value = clWhite
            LocationScale = 1.000000000000000000
          end>
        RGBGradient = <
          item
            Value = 1722007
          end
          item
            Value = 6452633
            LocationScale = 0.300048828125000000
          end
          item
            Value = 1453676
            LocationScale = 0.830078125000000000
          end
          item
            Value = 13491183
            LocationScale = 1.000000000000000000
            MidPoint = 0.600000023841857900
          end>
      end
      item
        DisplayName = 'Chrome'
        AlphaGradient = <
          item
            Value = clWhite
          end
          item
            Value = clWhite
            LocationScale = 1.000000000000000000
          end>
        RGBGradient = <
          item
            Value = 12088340
          end
          item
            Value = clWhite
            LocationScale = 0.500000000000000000
          end
          item
            Value = 27280
            LocationScale = 0.520019531250000000
            MidPoint = 0.129999995231628400
          end
          item
            Value = 40921
            LocationScale = 0.639892578124999900
          end
          item
            Value = clWhite
            LocationScale = 1.000000000000000000
          end>
      end
      item
        DisplayName = 'Spectrum'
        AlphaGradient = <
          item
            Value = clWhite
          end
          item
            Value = clWhite
            LocationScale = 1.000000000000000000
          end>
        RGBGradient = <
          item
            Value = clRed
          end
          item
            Value = clFuchsia
            LocationScale = 0.149902343750000000
          end
          item
            Value = clBlue
            LocationScale = 0.330078125000000000
          end
          item
            Value = clAqua
            LocationScale = 0.489990234375000000
          end
          item
            Value = clLime
            LocationScale = 0.669921875000000000
          end
          item
            Value = clYellow
            LocationScale = 0.840087890624999900
          end
          item
            Value = clRed
            LocationScale = 1.000000000000000000
          end>
      end
      item
        DisplayName = 'Transparent Rainbow'
        AlphaGradient = <
          item
            Value = clBlack
          end
          item
            Value = 13421772
            LocationScale = 0.070068359375000000
          end
          item
            Value = clWhite
            LocationScale = 0.110107421875000000
            MidPoint = 0.699999988079071100
          end
          item
            Value = clWhite
            LocationScale = 0.879882812500000000
          end
          item
            Value = 13421772
            LocationScale = 0.929931640624999900
            MidPoint = 0.200000002980232200
          end
          item
            Value = clBlack
            LocationScale = 1.000000000000000000
          end>
        RGBGradient = <
          item
            Value = clRed
            LocationScale = 0.120117187500000000
          end
          item
            Value = 64767
            LocationScale = 0.280029296875000000
          end
          item
            Value = 3781632
            LocationScale = 0.449951171875000000
          end
          item
            Value = 16771584
            LocationScale = 0.600097656250000000
            MidPoint = 0.600000023841857900
          end
          item
            Value = 9437952
            LocationScale = 0.750000000000000000
            MidPoint = 0.349999994039535600
          end
          item
            Value = 12976383
            LocationScale = 0.879882812500000000
            MidPoint = 0.600000023841857900
          end>
      end
      item
        DisplayName = 'Transparent Stripes'
        AlphaGradient = <
          item
            Value = clWhite
          end
          item
            Value = clWhite
            LocationScale = 0.090087890625000000
          end
          item
            Value = clBlack
            LocationScale = 0.100097656250000000
          end
          item
            Value = clBlack
            LocationScale = 0.189941406250000000
          end
          item
            Value = clWhite
            LocationScale = 0.199951171875000000
          end
          item
            Value = clWhite
            LocationScale = 0.290039062500000000
          end
          item
            Value = clBlack
            LocationScale = 0.300048828125000000
          end
          item
            Value = clBlack
            LocationScale = 0.389892578125000000
          end
          item
            Value = clWhite
            LocationScale = 0.399902343750000000
          end
          item
            Value = clWhite
            LocationScale = 0.489990234375000000
          end
          item
            Value = clBlack
            LocationScale = 0.500000000000000000
          end
          item
            Value = clBlack
            LocationScale = 0.590087890624999900
          end
          item
            Value = clWhite
            LocationScale = 0.600097656250000000
          end
          item
            Value = clWhite
            LocationScale = 0.689941406250000000
          end
          item
            Value = clBlack
            LocationScale = 0.699951171875000100
          end
          item
            Value = clBlack
            LocationScale = 0.790039062500000000
          end
          item
            Value = clWhite
            LocationScale = 0.800048828124999900
          end
          item
            Value = clWhite
            LocationScale = 0.889892578124999900
          end
          item
            Value = clBlack
            LocationScale = 0.899902343750000000
          end
          item
            Value = clBlack
            LocationScale = 1.000000000000000000
          end>
        RGBGradient = <
          item
            Value = clDefault
          end
          item
            Value = clDefault
            LocationScale = 1.000000000000000000
          end>
      end>
    ForegroundColor = clBlack
    BackgroundColor = clWhite
    Left = 32
    Top = 16
  end
  object glDrawingToolGradients: TgmGradientList
    Gradients = <>
    ForegroundColor = clBlack
    BackgroundColor = clWhite
    Left = 32
    Top = 48
  end
  object ogdOpenGradientDialog: TOpenGradientDialog
    Left = 64
    Top = 16
  end
  object sgdSaveGradientDialog: TSaveGradientDialog
    Left = 64
    Top = 48
  end
  object glMapCommandGradients: TgmGradientList
    Gradients = <>
    ForegroundColor = clBlack
    BackgroundColor = clWhite
    Left = 32
    Top = 80
  end
  object glFillLayerGradients: TgmGradientList
    Gradients = <>
    ForegroundColor = clBlack
    BackgroundColor = clWhite
    Left = 32
    Top = 112
  end
  object glMapLayerGradients: TgmGradientList
    Gradients = <>
    ForegroundColor = clBlack
    BackgroundColor = clWhite
    Left = 32
    Top = 144
  end
end
