object frmPatterns: TfrmPatterns
  Left = 189
  Top = 133
  BorderStyle = bsNone
  Caption = 'Select Pattern'
  ClientHeight = 222
  ClientWidth = 343
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnDeactivate = FormDeactivate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlPatternFrmBorder: TPanel
    Left = 0
    Top = 0
    Width = 343
    Height = 222
    Align = alClient
    BevelInner = bvRaised
    TabOrder = 0
    object spdbtnPatternOptions: TSpeedButton
      Left = 265
      Top = 21
      Width = 61
      Height = 18
      Cursor = crHandPoint
      Caption = 'Option'
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
      OnClick = spdbtnPatternOptionsClick
    end
    object stsbrPatternInfo: TStatusBar
      Left = 2
      Top = 196
      Width = 339
      Height = 24
      Panels = <
        item
          Width = 50
        end>
    end
    object Panel1: TPanel
      Left = 2
      Top = 2
      Width = 250
      Height = 194
      Align = alLeft
      BevelInner = bvLowered
      Caption = 'Panel1'
      TabOrder = 1
      object imgvwPatterns: TImgView32
        Left = 2
        Top = 2
        Width = 246
        Height = 190
        Align = alClient
        Bitmap.ResamplerClassName = 'TNearestResampler'
        BitmapAlign = baCustom
        Scale = 1.000000000000000000
        ScaleMode = smScale
        ScrollBars.ShowHandleGrip = True
        ScrollBars.Style = rbsMac
        ScrollBars.Size = 21
        ScrollBars.Visibility = svAuto
        OverSize = 0
        TabOrder = 0
        OnMouseDown = imgvwPatternsMouseDown
        OnMouseMove = imgvwPatternsMouseMove
      end
    end
  end
  object pmnPatternOptions: TPopupMenu
    OnPopup = pmnPatternOptionsPopup
    Left = 35
    Top = 59
    object pmnitmResetPattern: TMenuItem
      Caption = 'Reset Pattern'
      OnClick = pmnitmResetPatternClick
    end
    object pmnitmReplacePattern: TMenuItem
      Caption = 'Replace Pattern...'
      OnClick = pmnitmReplacePatternClick
    end
    object pmnitmSaveChanges: TMenuItem
      Caption = 'Save Changes'
      OnClick = pmnitmSaveChangesClick
    end
    object pmnitmSavePatternAs: TMenuItem
      Caption = 'Save Pattern As...'
      OnClick = pmnitmSavePatternAsClick
    end
    object pmnitmSeparator1: TMenuItem
      Caption = '-'
    end
    object pmnitmSmallThumbnail: TMenuItem
      Caption = 'Small Thumbnail'
      OnClick = ChangePatternThumbnailSize
    end
    object pmnitmLargeThumbnail: TMenuItem
      Caption = 'Large Thumbnail'
      OnClick = ChangePatternThumbnailSize
    end
    object pmnitmSeparator2: TMenuItem
      Caption = '-'
    end
    object pmnitmLoadImageToPattern: TMenuItem
      Caption = 'Load Image To Pattern...'
      OnClick = pmnitmLoadImageToPatternClick
    end
    object pmnitmRenamePattern: TMenuItem
      Caption = 'Rename Pattern...'
      OnClick = pmnitmRenamePatternClick
    end
    object pmnitmDeletePattern: TMenuItem
      Caption = 'Delete Pattern'
      OnClick = pmnitmDeletePatternClick
    end
  end
  object opndlgOpenPattern: TOpenDialog
    Filter = 'Pattern Files (*.pat)|*.pat'
    Left = 67
    Top = 59
  end
  object svdlgSavePattern: TSaveDialog
    Filter = 'Pattern Files (*.pat)|*.pat'
    Left = 99
    Top = 59
  end
  object opnpctrdlgOpenImage: TOpenPictureDialog
    Filter = 
      'All Files (*.bmp;*.jpg;*.png)|*.bmp;*.jpg;*.png|Bitmaps (*.bmp)|' +
      '*.bmp|JPEG Image File (*.jpg)|*.jpg|Portable Network Graphics (*' +
      '.png)|*.png'
    Left = 131
    Top = 59
  end
end
