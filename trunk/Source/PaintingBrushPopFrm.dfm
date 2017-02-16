object frmPaintingBrush: TfrmPaintingBrush
  Left = 186
  Top = 128
  BorderStyle = bsNone
  Caption = 'Painting Brush'
  ClientHeight = 187
  ClientWidth = 341
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
  object pnlPaintingBrushViewer: TPanel
    Left = 0
    Top = 0
    Width = 341
    Height = 187
    Align = alClient
    BevelInner = bvRaised
    TabOrder = 0
    object spdbtnPaintingBrushOptions: TSpeedButton
      Left = 267
      Top = 20
      Width = 59
      Height = 17
      Cursor = crHandPoint
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
      OnClick = spdbtnPaintingBrushOptionsClick
    end
    object stsbrPaintingBrushInfo: TStatusBar
      Left = 2
      Top = 166
      Width = 337
      Height = 19
      Panels = <
        item
          Width = 50
        end>
    end
    object Panel1: TPanel
      Left = 2
      Top = 2
      Width = 250
      Height = 164
      Align = alLeft
      BevelInner = bvLowered
      TabOrder = 1
      object imgPaintingBrushes: TImgView32
        Left = 2
        Top = 2
        Width = 246
        Height = 160
        Align = alClient
        Bitmap.ResamplerClassName = 'TNearestResampler'
        BitmapAlign = baCustom
        Scale = 1.000000000000000000
        ScaleMode = smScale
        ScrollBars.ShowHandleGrip = True
        ScrollBars.Style = rbsMac
        ScrollBars.Visibility = svAuto
        OverSize = 0
        TabOrder = 0
        OnMouseDown = imgPaintingBrushesMouseDown
        OnMouseMove = imgPaintingBrushesMouseMove
      end
    end
  end
  object pmnPaintingBrushOptions: TPopupMenu
    OnPopup = pmnPaintingBrushOptionsPopup
    Left = 42
    Top = 90
    object pmnitmResetBrushes: TMenuItem
      Caption = 'Reset Brushes'
      OnClick = pmnitmResetBrushesClick
    end
    object pmnitmReplaceBrushes: TMenuItem
      Caption = 'Replace Brushes...'
      OnClick = pmnitmReplaceBrushesClick
    end
    object pmnitmSaveChanges: TMenuItem
      Caption = 'Save Changes'
      OnClick = pmnitmSaveChangesClick
    end
    object pmnitmSaveBrushesAs: TMenuItem
      Caption = 'Save Brushes As...'
      OnClick = pmnitmSaveBrushesAsClick
    end
    object pmnitmSeparator1: TMenuItem
      Caption = '-'
    end
    object pmnitmSmallThumbnail: TMenuItem
      Caption = 'Small Thumbnail'
      OnClick = ChangeBrushesThumbnailSize
    end
    object pmnitmLargeThumbnail: TMenuItem
      Caption = 'Large Thumbnail'
      OnClick = ChangeBrushesThumbnailSize
    end
    object pmnitmSeparator2: TMenuItem
      Caption = '-'
    end
    object pmnitmLoadMask: TMenuItem
      Caption = 'Load Mask...'
      OnClick = pmnitmLoadMaskClick
    end
    object pmnitmRenameBrush: TMenuItem
      Caption = 'Rename Brush...'
      OnClick = pmnitmRenameBrushClick
    end
    object pmnitmDeleteBrush: TMenuItem
      Caption = 'Delete Brush'
      OnClick = pmnitmDeleteBrushClick
    end
  end
  object opndlgOpenBrush: TOpenDialog
    Filter = 'Painting Brush Files (*.gmb)|*.gmb'
    Left = 74
    Top = 90
  end
  object svdlgSaveBrushes: TSaveDialog
    Filter = 'Painting Brush Files (*.gmb)|*.gmb'
    Left = 106
    Top = 90
  end
  object opnpctrdlgOpenBitmap: TOpenPictureDialog
    Filter = 'Bitmaps (*.bmp)|*.bmp'
    Left = 138
    Top = 90
  end
end
