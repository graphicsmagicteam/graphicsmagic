object frmFlareFX: TfrmFlareFX
  Left = 193
  Top = 134
  BorderStyle = bsDialog
  Caption = 'FlareFX'
  ClientHeight = 325
  ClientWidth = 303
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
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
  object grpbxPreview: TGroupBox
    Left = 7
    Top = 7
    Width = 195
    Height = 203
    Caption = 'Preview:'
    TabOrder = 0
    object pnlThumbnailHolder: TPanel
      Left = 8
      Top = 16
      Width = 179
      Height = 179
      BevelInner = bvLowered
      TabOrder = 0
      object img32Thumbnail: TImage32
        Left = 8
        Top = 8
        Width = 163
        Height = 163
        AutoSize = True
        Bitmap.ResamplerClassName = 'TNearestResampler'
        BitmapAlign = baTopLeft
        Scale = 1.000000000000000000
        ScaleMode = smNormal
        TabOrder = 0
        OnMouseDown = img32ThumbnailMouseDown
        OnMouseMove = img32ThumbnailMouseMove
        OnPaintStage = img32ThumbnailPaintStage
      end
    end
  end
  object grpbxParamSettings: TGroupBox
    Left = 7
    Top = 213
    Width = 195
    Height = 85
    Caption = 'Center of FlareFX:'
    TabOrder = 1
    object lblCenterX: TLabel
      Left = 33
      Top = 26
      Width = 10
      Height = 13
      Caption = 'X:'
    end
    object lblCenterY: TLabel
      Left = 33
      Top = 52
      Width = 10
      Height = 13
      Caption = 'Y:'
    end
    object edtFlareCX: TEdit
      Left = 52
      Top = 23
      Width = 98
      Height = 21
      TabOrder = 0
      Text = '0'
      OnChange = edtFlareCXChange
    end
    object edtFlareCY: TEdit
      Left = 52
      Top = 49
      Width = 98
      Height = 21
      TabOrder = 1
      Text = '0'
      OnChange = edtFlareCYChange
    end
    object updwnFlareCX: TUpDown
      Left = 150
      Top = 23
      Width = 11
      Height = 21
      Cursor = crHandPoint
      Associate = edtFlareCX
      TabOrder = 2
      OnMouseDown = updwnFlareCXMouseDown
      OnMouseUp = updwnFlareCXMouseUp
    end
    object updwnFlareCY: TUpDown
      Left = 150
      Top = 49
      Width = 11
      Height = 21
      Cursor = crHandPoint
      Associate = edtFlareCY
      TabOrder = 3
      OnMouseDown = updwnFlareCYMouseDown
      OnMouseUp = updwnFlareCYMouseUp
    end
  end
  object btbtnOK: TBitBtn
    Left = 215
    Top = 12
    Width = 75
    Height = 25
    Cursor = crHandPoint
    TabOrder = 2
    Kind = bkOK
  end
  object btbtnCancel: TBitBtn
    Left = 215
    Top = 46
    Width = 75
    Height = 25
    Cursor = crHandPoint
    TabOrder = 3
    Kind = bkCancel
  end
  object chckbxPreview: TCheckBox
    Left = 215
    Top = 123
    Width = 74
    Height = 14
    Cursor = crHandPoint
    Caption = 'Preview:'
    Checked = True
    State = cbChecked
    TabOrder = 4
    OnClick = chckbxPreviewClick
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 306
    Width = 303
    Height = 19
    Panels = <>
  end
  object btnAbout: TButton
    Left = 216
    Top = 80
    Width = 75
    Height = 25
    Cursor = crHandPoint
    Caption = 'About...'
    TabOrder = 6
    OnClick = btnAboutClick
  end
end
