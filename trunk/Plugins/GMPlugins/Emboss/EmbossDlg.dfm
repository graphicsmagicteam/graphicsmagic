object frmEmboss: TfrmEmboss
  Left = 192
  Top = 128
  BorderStyle = bsDialog
  Caption = 'Emboss'
  ClientHeight = 480
  ClientWidth = 339
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 225
    Height = 465
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 240
      Width = 42
      Height = 13
      Caption = 'Channel:'
    end
    object Label2: TLabel
      Left = 8
      Top = 272
      Width = 54
      Height = 13
      Caption = 'Function:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblAzimuth: TLabel
      Left = 16
      Top = 320
      Width = 40
      Height = 13
      Caption = 'Azimuth:'
    end
    object lblElevation: TLabel
      Left = 16
      Top = 368
      Width = 47
      Height = 13
      Caption = 'Elevation:'
    end
    object lblDepth: TLabel
      Left = 16
      Top = 416
      Width = 32
      Height = 13
      Caption = 'Depth:'
    end
    object pnlThumbnail: TPanel
      Left = 8
      Top = 16
      Width = 192
      Height = 192
      BevelInner = bvLowered
      TabOrder = 0
      object imgThumbnail: TImage32
        Left = 2
        Top = 2
        Width = 188
        Height = 188
        Align = alClient
        Bitmap.ResamplerClassName = 'TNearestResampler'
        BitmapAlign = baTopLeft
        Scale = 1.000000000000000000
        ScaleMode = smNormal
        TabOrder = 0
        OnMouseDown = imgThumbnailMouseDown
        OnMouseMove = imgThumbnailMouseMove
        OnMouseUp = imgThumbnailMouseUp
        OnPaintStage = imgThumbnailPaintStage
      end
    end
    object ggbrVert: TGaugeBar
      Left = 200
      Top = 16
      Width = 16
      Height = 193
      Cursor = crHandPoint
      Backgnd = bgPattern
      Kind = sbVertical
      ShowHandleGrip = True
      Style = rbsMac
      Visible = False
      Position = 0
      OnChange = ggbrVertChange
      OnMouseDown = GaugeBarMouseDown
      OnMouseUp = GaugeBarMouseUp
    end
    object ggbrHorz: TGaugeBar
      Left = 8
      Top = 208
      Width = 193
      Height = 16
      Cursor = crHandPoint
      Backgnd = bgPattern
      ShowHandleGrip = True
      Style = rbsMac
      Visible = False
      Position = 0
      OnChange = ggbrHorzChange
      OnMouseDown = GaugeBarMouseDown
      OnMouseUp = GaugeBarMouseUp
    end
    object cmbbxChannel: TComboBox
      Left = 64
      Top = 236
      Width = 145
      Height = 21
      Cursor = crHandPoint
      ItemHeight = 13
      TabOrder = 3
      OnChange = cmbbxChannelChange
      Items.Strings = (
        'Red'
        'Green'
        'Blue'
        'Grayscale')
    end
    object rdbtnBumpmap: TRadioButton
      Left = 24
      Top = 288
      Width = 80
      Height = 17
      Cursor = crHandPoint
      Caption = 'Bumpmap'
      TabOrder = 4
      OnClick = ChangeFumctionMode
    end
    object rdbtnEmboss: TRadioButton
      Left = 128
      Top = 288
      Width = 80
      Height = 17
      Cursor = crHandPoint
      Caption = 'Emboss'
      TabOrder = 5
      OnClick = ChangeFumctionMode
    end
    object ggbrAzimuth: TGaugeBar
      Left = 8
      Top = 344
      Width = 201
      Height = 16
      Cursor = crHandPoint
      Backgnd = bgPattern
      Max = 360
      ShowHandleGrip = True
      Style = rbsMac
      Position = 0
      OnChange = ggbrAzimuthChange
      OnMouseUp = GaugeBarMouseUp
    end
    object ggbrElevation: TGaugeBar
      Left = 8
      Top = 392
      Width = 201
      Height = 16
      Cursor = crHandPoint
      Backgnd = bgPattern
      Max = 180
      ShowHandleGrip = True
      Style = rbsMac
      Position = 0
      OnChange = ggbrElevationChange
      OnMouseUp = GaugeBarMouseUp
    end
    object ggbrDepth: TGaugeBar
      Left = 8
      Top = 440
      Width = 201
      Height = 16
      Cursor = crHandPoint
      Backgnd = bgPattern
      Min = 1
      ShowHandleGrip = True
      Style = rbsMac
      Position = 1
      OnChange = ggbrDepthChange
      OnMouseUp = GaugeBarMouseUp
    end
    object edtAzimuth: TEdit
      Left = 160
      Top = 316
      Width = 50
      Height = 21
      TabOrder = 9
      OnChange = edtAzimuthChange
    end
    object edtElevation: TEdit
      Left = 160
      Top = 364
      Width = 50
      Height = 21
      TabOrder = 10
      OnChange = edtElevationChange
    end
    object edtDepth: TEdit
      Left = 160
      Top = 412
      Width = 50
      Height = 21
      TabOrder = 11
      OnChange = edtDepthChange
    end
  end
  object btbtnOK: TBitBtn
    Left = 248
    Top = 32
    Width = 75
    Height = 25
    Cursor = crHandPoint
    TabOrder = 1
    OnClick = btbtnOKClick
    Kind = bkOK
  end
  object btbtnCancel: TBitBtn
    Left = 248
    Top = 72
    Width = 75
    Height = 25
    Cursor = crHandPoint
    TabOrder = 2
    Kind = bkCancel
  end
  object chckbxPreview: TCheckBox
    Left = 248
    Top = 152
    Width = 75
    Height = 17
    Cursor = crHandPoint
    Caption = 'Preview'
    TabOrder = 3
    OnClick = chckbxPreviewClick
  end
  object btnAbout: TButton
    Left = 248
    Top = 112
    Width = 75
    Height = 25
    Cursor = crHandPoint
    Caption = 'About...'
    TabOrder = 4
    OnClick = btnAboutClick
  end
end
