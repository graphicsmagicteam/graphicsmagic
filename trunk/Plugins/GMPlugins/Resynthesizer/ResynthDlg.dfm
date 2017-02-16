object frmResynthesizer: TfrmResynthesizer
  Left = 197
  Top = 137
  BorderStyle = bsDialog
  Caption = 'Resynthesizer '
  ClientHeight = 289
  ClientWidth = 328
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 217
    Height = 65
    Caption = 'Input:'
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 26
      Width = 76
      Height = 13
      Caption = 'Texture Source:'
    end
    object btnLoadSrcTexture: TButton
      Left = 136
      Top = 20
      Width = 65
      Height = 25
      Cursor = crHandPoint
      Caption = 'Load...'
      TabOrder = 0
      OnClick = LoadSourceTexture
    end
    object imgvwSrcTexture: TImgView32
      Left = 96
      Top = 16
      Width = 32
      Height = 32
      Bitmap.ResamplerClassName = 'TNearestResampler'
      BitmapAlign = baCustom
      Scale = 1.000000000000000000
      ScaleMode = smResize
      ScrollBars.ShowHandleGrip = True
      ScrollBars.Style = rbsDefault
      ScrollBars.Size = 21
      ScrollBars.Visibility = svHidden
      OverSize = 0
      TabOrder = 1
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 80
    Width = 217
    Height = 105
    Caption = 'Output:'
    TabOrder = 1
    object chckbxHTile: TCheckBox
      Left = 8
      Top = 24
      Width = 201
      Height = 17
      Cursor = crHandPoint
      Caption = 'Make horizontally tileable'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = chckbxHTileClick
    end
    object chckbxVTile: TCheckBox
      Left = 8
      Top = 48
      Width = 201
      Height = 17
      Cursor = crHandPoint
      Caption = 'Make vertically tileable'
      Checked = True
      State = cbChecked
      TabOrder = 1
      OnClick = chckbxVTileClick
    end
    object chckbxUseBorder: TCheckBox
      Left = 8
      Top = 72
      Width = 201
      Height = 17
      Cursor = crHandPoint
      Caption = 'Fit output to bordering pixels'
      Checked = True
      State = cbChecked
      TabOrder = 2
      OnClick = chckbxUseBorderClick
    end
  end
  object GroupBox3: TGroupBox
    Left = 8
    Top = 192
    Width = 217
    Height = 89
    Caption = 'Personality:'
    TabOrder = 2
    object Label2: TLabel
      Left = 8
      Top = 24
      Width = 98
      Height = 13
      Caption = 'Sensitivity to outliers:'
    end
    object edtAutism: TEdit
      Left = 112
      Top = 20
      Width = 81
      Height = 21
      TabOrder = 0
      Text = '0.12'
      OnChange = edtAutismChange
    end
    object ggbrAutism: TGaugeBar
      Left = 8
      Top = 56
      Width = 185
      Height = 16
      Cursor = crHandPoint
      Backgnd = bgPattern
      Min = 1
      ShowHandleGrip = True
      Style = rbsMac
      Position = 12
      OnChange = ggbrAutismChange
    end
  end
  object btbtnOK: TBitBtn
    Left = 240
    Top = 16
    Width = 75
    Height = 25
    Cursor = crHandPoint
    TabOrder = 3
    OnClick = btbtnOKClick
    Kind = bkOK
  end
  object btbtnCancel: TBitBtn
    Left = 240
    Top = 56
    Width = 75
    Height = 25
    Cursor = crHandPoint
    TabOrder = 4
    Kind = bkCancel
  end
  object btnPreview: TButton
    Left = 240
    Top = 96
    Width = 75
    Height = 25
    Cursor = crHandPoint
    Caption = 'Preview'
    TabOrder = 5
    OnClick = btnPreviewClick
  end
  object btnAbout: TButton
    Left = 240
    Top = 136
    Width = 75
    Height = 25
    Cursor = crHandPoint
    Caption = 'About...'
    TabOrder = 6
    OnClick = btnAboutClick
  end
  object OpenPictureDialog: TOpenPictureDialog
    Filter = 
      'All (*.jpg;*.bmp)|*.jpg;*.bmp|JPEG Image File (*.jpg)|*.jpg|Bitm' +
      'aps (*.bmp)|*.bmp'
    Left = 248
    Top = 200
  end
end
