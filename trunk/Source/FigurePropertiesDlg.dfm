object frmFigureProperties: TfrmFigureProperties
  Left = 189
  Top = 131
  BorderStyle = bsDialog
  Caption = 'Figure Properties'
  ClientHeight = 396
  ClientWidth = 333
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object grpbxFigureProperties: TGroupBox
    Left = 7
    Top = 7
    Width = 241
    Height = 378
    Caption = 'Properties:'
    TabOrder = 0
    object lblLayerName: TLabel
      Left = 13
      Top = 26
      Width = 92
      Height = 13
      Caption = 'Figure Layer Name:'
    end
    object lblFigureIndex: TLabel
      Left = 13
      Top = 78
      Width = 61
      Height = 13
      Caption = 'Figure Index:'
    end
    object lblLayerIndex: TLabel
      Left = 13
      Top = 52
      Width = 90
      Height = 13
      Caption = 'Figure Layer Index:'
    end
    object lblFigureName: TLabel
      Left = 13
      Top = 104
      Width = 63
      Height = 13
      Caption = 'Figure Name:'
    end
    object lblPenColor: TLabel
      Left = 13
      Top = 234
      Width = 49
      Height = 13
      Caption = 'Pen Color:'
    end
    object shpPenColor: TShape
      Left = 117
      Top = 231
      Width = 80
      Height = 19
    end
    object spdbtnChangePenColor: TSpeedButton
      Left = 197
      Top = 232
      Width = 19
      Height = 17
      Cursor = crHandPoint
      Caption = '...'
      OnClick = spdbtnChangePenColorClick
    end
    object lblBrushColor: TLabel
      Left = 13
      Top = 260
      Width = 57
      Height = 13
      Caption = 'Brush Color:'
    end
    object shpBrushColor: TShape
      Left = 117
      Top = 257
      Width = 80
      Height = 19
    end
    object spdbtnBrushColor: TSpeedButton
      Left = 197
      Top = 258
      Width = 19
      Height = 17
      Cursor = crHandPoint
      Caption = '...'
      OnClick = spdbtnBrushColorClick
    end
    object lblPenWidth: TLabel
      Left = 13
      Top = 286
      Width = 53
      Height = 13
      Caption = 'Pen Width:'
    end
    object lblPenStyle: TLabel
      Left = 13
      Top = 312
      Width = 48
      Height = 13
      Caption = 'Pen Style:'
    end
    object lblBrushStyle: TLabel
      Left = 13
      Top = 345
      Width = 56
      Height = 13
      Caption = 'Brush Style:'
    end
    object spdbtnBrushStyle: TSpeedButton
      Left = 141
      Top = 339
      Width = 13
      Height = 24
      Cursor = crHandPoint
      Glyph.Data = {
        F6000000424DF600000000000000360000002800000008000000080000000100
        180000000000C000000000000000000000000000000000000000FFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF404040404040FFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFF404040000000000000404040FFFFFFFFFFFFFFFFFF404040
        000000000000000000000000404040FFFFFF4040400000000000000000000000
        00000000000000404040FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
      OnClick = spdbtnBrushStyleClick
    end
    object lblOriginX: TLabel
      Left = 13
      Top = 156
      Width = 40
      Height = 13
      Caption = 'Origin X:'
    end
    object lblOriginY: TLabel
      Left = 13
      Top = 182
      Width = 40
      Height = 13
      Caption = 'Origin Y:'
    end
    object lblRadius: TLabel
      Left = 13
      Top = 208
      Width = 36
      Height = 13
      Caption = 'Radius:'
    end
    object lblUnit: TLabel
      Left = 13
      Top = 130
      Width = 22
      Height = 13
      Caption = 'Unit:'
    end
    object edtLayerName: TEdit
      Left = 117
      Top = 23
      Width = 98
      Height = 21
      Color = clBtnFace
      ParentShowHint = False
      ReadOnly = True
      ShowHint = True
      TabOrder = 0
    end
    object edtFigureIndex: TEdit
      Left = 117
      Top = 75
      Width = 98
      Height = 21
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 1
    end
    object edtLayerIndex: TEdit
      Left = 117
      Top = 49
      Width = 98
      Height = 21
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 2
    end
    object edtFigureName: TEdit
      Left = 117
      Top = 101
      Width = 98
      Height = 21
      TabOrder = 3
    end
    object edtPenWidth: TEdit
      Left = 117
      Top = 286
      Width = 86
      Height = 21
      TabOrder = 4
      Text = '1'
      OnChange = edtPenWidthChange
    end
    object updwnPenWidth: TUpDown
      Left = 203
      Top = 286
      Width = 12
      Height = 21
      Cursor = crHandPoint
      Associate = edtPenWidth
      Min = 1
      Max = 10
      Position = 1
      TabOrder = 5
    end
    object cmbbxPenStyle: TComboBox
      Left = 117
      Top = 312
      Width = 98
      Height = 22
      Cursor = crHandPoint
      Style = csOwnerDrawFixed
      ItemHeight = 16
      TabOrder = 6
      OnChange = cmbbxPenStyleChange
      OnDrawItem = cmbbxPenStyleDrawItem
      Items.Strings = (
        '0'
        '1'
        '2'
        '3'
        '4')
    end
    object pnlBrushStyle: TPanel
      Left = 117
      Top = 339
      Width = 24
      Height = 24
      BevelInner = bvRaised
      BevelOuter = bvLowered
      TabOrder = 7
      object imgBrushStyle: TImage
        Left = 2
        Top = 2
        Width = 21
        Height = 21
        ParentShowHint = False
        ShowHint = True
      end
    end
    object edtOriginX: TEdit
      Left = 117
      Top = 152
      Width = 98
      Height = 21
      TabOrder = 8
      OnChange = edtOriginXChange
    end
    object edtOriginY: TEdit
      Left = 117
      Top = 178
      Width = 98
      Height = 21
      TabOrder = 9
      OnChange = edtOriginYChange
    end
    object edtRadius: TEdit
      Left = 117
      Top = 204
      Width = 98
      Height = 21
      TabOrder = 10
      OnChange = edtRadiusChange
    end
    object cmbbxUnit: TComboBox
      Left = 117
      Top = 126
      Width = 98
      Height = 21
      ItemHeight = 13
      TabOrder = 11
      OnChange = cmbbxUnitChange
      Items.Strings = (
        'inches'
        'cm'
        'points'
        'pixels')
    end
  end
  object btbtnOK: TBitBtn
    Left = 260
    Top = 13
    Width = 61
    Height = 20
    Cursor = crHandPoint
    TabOrder = 1
    OnClick = btbtnOKClick
    Kind = bkOK
  end
  object btbtnCancel: TBitBtn
    Left = 260
    Top = 46
    Width = 61
    Height = 20
    Cursor = crHandPoint
    TabOrder = 2
    Kind = bkCancel
  end
  object pmnFigureBrushStyle: TPopupMenu
    Images = dmMain.imglstCommon
    OnPopup = pmnFigureBrushStylePopup
    Left = 80
    Top = 216
    object pmnitmClearBrush: TMenuItem
      Caption = 'Clear'
      ImageIndex = 0
      OnClick = ChangeBrushStyle
    end
    object pmnitmSolidBrush: TMenuItem
      Caption = 'Solid'
      ImageIndex = 1
      OnClick = ChangeBrushStyle
    end
    object pmnitmHorizontalBrush: TMenuItem
      Caption = 'Horizontal'
      ImageIndex = 2
      OnClick = ChangeBrushStyle
    end
    object pmnitmVerticalBrush: TMenuItem
      Caption = 'Vertical'
      ImageIndex = 3
      OnClick = ChangeBrushStyle
    end
    object pmnitmFDiagonalBrush: TMenuItem
      Caption = 'F-Diagonal'
      ImageIndex = 4
      OnClick = ChangeBrushStyle
    end
    object pmnitmBDiagonalBrush: TMenuItem
      Caption = 'B-Diagonal'
      ImageIndex = 5
      OnClick = ChangeBrushStyle
    end
    object pmnitmCrossBrush: TMenuItem
      Caption = 'Cross'
      ImageIndex = 6
      OnClick = ChangeBrushStyle
    end
    object pmnitmDiagonalCrossBrush: TMenuItem
      Caption = 'Diagonal Cross'
      ImageIndex = 7
      OnClick = ChangeBrushStyle
    end
  end
end
