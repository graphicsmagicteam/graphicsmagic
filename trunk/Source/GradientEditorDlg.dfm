object frmGradientEditor: TfrmGradientEditor
  Left = 188
  Top = 129
  BorderStyle = bsDialog
  Caption = 'Gradient Editor'
  ClientHeight = 353
  ClientWidth = 422
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object btbtnOK: TBitBtn
    Left = 344
    Top = 16
    Width = 65
    Height = 25
    Cursor = crHandPoint
    TabOrder = 0
    OnClick = btbtnOKClick
    Kind = bkOK
  end
  object btbtnCancel: TBitBtn
    Left = 345
    Top = 48
    Width = 65
    Height = 25
    Cursor = crHandPoint
    TabOrder = 1
    Kind = bkCancel
  end
  object btnSaveGradients: TButton
    Left = 345
    Top = 120
    Width = 65
    Height = 25
    Cursor = crHandPoint
    Caption = '&Save...'
    TabOrder = 2
    OnClick = btnSaveGradientsClick
  end
  object btnLoadGradients: TButton
    Left = 344
    Top = 88
    Width = 65
    Height = 25
    Cursor = crHandPoint
    Caption = '&Load...'
    TabOrder = 3
    OnClick = btnLoadGradientsClick
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 329
    Height = 353
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 4
    object Bevel1: TBevel
      Left = 0
      Top = 234
      Width = 329
      Height = 5
      Align = alBottom
      Shape = bsSpacer
    end
    object grpbxStops: TGroupBox
      Left = 0
      Top = 239
      Width = 329
      Height = 114
      Align = alBottom
      Caption = 'Stops'
      TabOrder = 0
      object lbl2: TLabel
        Left = 48
        Top = 14
        Width = 233
        Height = 13
        Caption = 'Right click on any color stop to swtich stop types.'
      end
      object lblOpacity: TLabel
        Left = 16
        Top = 36
        Width = 56
        Height = 13
        Caption = 'Opacity: 0%'
      end
      object lblColor: TLabel
        Left = 16
        Top = 84
        Width = 27
        Height = 13
        Caption = 'Color:'
      end
      object lblAlphaLocation: TLabel
        Left = 120
        Top = 56
        Width = 21
        Height = 13
        Caption = 'Loc:'
      end
      object lblColorLocation: TLabel
        Left = 120
        Top = 84
        Width = 21
        Height = 13
        Caption = 'Loc:'
      end
      object lblAlphaLocationPercent: TLabel
        Left = 185
        Top = 56
        Width = 8
        Height = 13
        Caption = '%'
      end
      object lblColorLocationPercent: TLabel
        Left = 185
        Top = 84
        Width = 8
        Height = 13
        Caption = '%'
      end
      object ggbrOpacity: TGaugeBar
        Left = 16
        Top = 56
        Width = 89
        Height = 16
        Cursor = crHandPoint
        Backgnd = bgPattern
        ShowHandleGrip = True
        Style = rbsMac
        Position = 0
        OnChange = ggbrOpacityChange
      end
      object pnlPrimaryColor: TPanel
        Left = 48
        Top = 80
        Width = 57
        Height = 21
        BevelInner = bvLowered
        TabOrder = 1
        object shpStopColor: TShape
          Left = 2
          Top = 2
          Width = 53
          Height = 17
          Cursor = crHandPoint
          Align = alClient
          Visible = False
          OnMouseDown = shpStopColorMouseDown
        end
      end
      object edtAlphaLocation: TEdit
        Left = 144
        Top = 52
        Width = 40
        Height = 21
        Color = clBtnFace
        TabOrder = 2
        OnChange = edtAlphaLocationChange
      end
      object edtColorLocation: TEdit
        Left = 144
        Top = 80
        Width = 40
        Height = 21
        Color = clBtnFace
        TabOrder = 3
        OnChange = edtColorLocationChange
      end
      object btnDeleteSelectedAlphaValue: TButton
        Left = 208
        Top = 50
        Width = 50
        Height = 25
        Cursor = crHandPoint
        Caption = 'Delete'
        Enabled = False
        TabOrder = 4
        OnClick = btnDeleteSelectedAlphaValueClick
      end
      object btnDeleteSelectedColor: TButton
        Left = 208
        Top = 78
        Width = 50
        Height = 25
        Cursor = crHandPoint
        Caption = 'Delete'
        Enabled = False
        TabOrder = 5
        OnClick = btnDeleteSelectedColorClick
      end
      object btnAverageAlphaValues: TButton
        Left = 264
        Top = 50
        Width = 50
        Height = 25
        Cursor = crHandPoint
        Caption = 'Average'
        TabOrder = 6
        OnClick = btnAverageAlphaValuesClick
      end
      object btnAverageColors: TButton
        Left = 264
        Top = 78
        Width = 50
        Height = 25
        Cursor = crHandPoint
        Caption = 'Average'
        TabOrder = 7
        OnClick = btnAverageColorsClick
      end
    end
    object geGradientEditor: TgmGradientEditor
      Left = 0
      Top = 171
      Width = 329
      Height = 63
      Align = alBottom
      OnMouseDown = geGradientEditorMouseDown
      OnMouseMove = geGradientEditorMouseMove
      OnMouseUp = geGradientEditorMouseUp
      EditMode = eemCopyEdit
      Gradients = glGradients
      GradientIndex = -1
      DefaultNewColor = clBlack
      DefaultNewAlpha = 255
      OnStateChanged = geGradientEditorStateChanged
      OnCursorPosChanged = geGradientEditorCursorPosChanged
    end
    object Panel2: TPanel
      Left = 0
      Top = 135
      Width = 329
      Height = 36
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 2
      object lblGradientName: TLabel
        Left = 8
        Top = 10
        Width = 31
        Height = 13
        Caption = 'Name:'
      end
      object edtGradientName: TEdit
        Left = 48
        Top = 6
        Width = 225
        Height = 21
        TabOrder = 0
      end
      object btnNewGradient: TButton
        Left = 283
        Top = 6
        Width = 40
        Height = 20
        Cursor = crHandPoint
        Hint = 'Create a gradient'
        Caption = 'Ne&w'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        OnClick = btnNewGradientClick
      end
    end
    object grpbxPresets: TGroupBox
      Left = 0
      Top = 0
      Width = 329
      Height = 135
      Align = alClient
      Caption = 'Presets:'
      TabOrder = 3
      object scrlbxGradientEditor: TScrollBox
        Left = 2
        Top = 15
        Width = 325
        Height = 118
        VertScrollBar.Position = 8
        Align = alClient
        PopupMenu = pmnGradientEditorOptions
        TabOrder = 0
        object glvGradients: TgmGradientListView
          Left = 0
          Top = -8
          Width = 300
          Height = 129
          AutoSize = True
          GradientList = glGradients
          ParentShowHint = False
          Scale = 1.000000000000000000
          ScaleMode = smNormal
          ShowHint = True
          TabOrder = 0
          GridOptions = [goDragable, goSelection]
          CellBorderStyle = borContrastGrid
          OnMouseDown = glvGradientsMouseDown
          OnMouseMove = glvGradientsMouseMove
          OnMouseUp = glvGradientsMouseUp
        end
      end
    end
  end
  object clrdlgColorDialog: TColorDialog
    Options = [cdFullOpen]
    Left = 344
    Top = 160
  end
  object pmnGradientEditorOptions: TPopupMenu
    OnPopup = pmnGradientEditorOptionsPopup
    Left = 376
    Top = 160
    object pmnitmSmallThumbnail: TMenuItem
      Caption = 'Small Thumbnail'
      OnClick = ChangeEditorThumbnailSizeMode
    end
    object pmnitmLargeThumbnail: TMenuItem
      Caption = 'Large Thumbnail'
      OnClick = ChangeEditorThumbnailSizeMode
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object pmnitmDeleteGradient: TMenuItem
      Caption = 'Delete Gradient'
      OnClick = pmnitmDeleteGradientClick
    end
  end
  object glGradients: TgmGradientList
    Gradients = <>
    ForegroundColor = clBlack
    BackgroundColor = clWhite
    Left = 344
    Top = 192
  end
end
