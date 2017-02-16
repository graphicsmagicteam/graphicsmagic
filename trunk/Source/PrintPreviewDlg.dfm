object frmPrintPreview: TfrmPrintPreview
  Left = 234
  Top = 221
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Print Preview'
  ClientHeight = 150
  ClientWidth = 202
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 202
    Height = 38
    Align = alTop
    BevelInner = bvLowered
    TabOrder = 0
    object Label1: TLabel
      Left = 40
      Top = 13
      Width = 30
      Height = 13
      Alignment = taRightJustify
      Caption = 'Zoom:'
      FocusControl = cmbbxZoom
    end
    object spdbtnPrintImage: TSpeedButton
      Left = 8
      Top = 8
      Width = 23
      Height = 22
      Hint = 'Print'
      Flat = True
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        0400000000008000000000000000000000001000000000000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00888888888888
        888F8800000000000888808888888880808800000000000008080888888BBB88
        0008088888877788080800000000000008800888888888808080800000000008
        0800880FFFFFFFF080808880F00000F000088880FFFFFFFF088888880F00000F
        088888880FFFFFFFF08888888000000000888888888888888888}
      ParentShowHint = False
      ShowHint = True
      OnClick = spdbtnPrintImageClick
    end
    object cmbbxZoom: TComboBox
      Left = 81
      Top = 9
      Width = 100
      Height = 21
      Hint = 'Preview Scale'
      Style = csDropDownList
      ItemHeight = 13
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      OnChange = cmbbxZoomChange
      Items.Strings = (
        '500%'
        '200%'
        '150%'
        '100%'
        '75%'
        '50%'
        '25%'
        '10%'
        'Page Width'
        'Page Height'
        'Whole Page')
    end
  end
  object prntprvwPreview: TPrintPreview
    Left = 0
    Top = 38
    Width = 202
    Height = 112
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    TabOrder = 1
    Units = mmPixel
    UsePrinterOptions = True
  end
end
