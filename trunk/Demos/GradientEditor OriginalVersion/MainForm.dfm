object frmMain: TfrmMain
  Left = 188
  Top = 130
  Width = 870
  Height = 640
  Caption = 'Gradient Editor'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label3: TLabel
    Left = 16
    Top = 308
    Width = 31
    Height = 13
    Caption = 'Name:'
  end
  object Label4: TLabel
    Left = 8
    Top = 440
    Width = 233
    Height = 13
    Caption = 'Right click on any color stop to swtich stop types.'
  end
  object imgGradientEditor: TImage32
    Left = 8
    Top = 344
    Width = 401
    Height = 81
    AutoSize = True
    Bitmap.ResamplerClassName = 'TNearestResampler'
    BitmapAlign = baTopLeft
    Scale = 1.000000000000000000
    ScaleMode = smNormal
    TabOrder = 0
  end
  object grpbxStops: TGroupBox
    Left = 8
    Top = 464
    Width = 398
    Height = 121
    Caption = 'Stops'
    TabOrder = 1
    object lblColor: TLabel
      Left = 16
      Top = 84
      Width = 27
      Height = 13
      Caption = 'Color:'
      Enabled = False
    end
    object lblOpacity: TLabel
      Left = 16
      Top = 28
      Width = 56
      Height = 13
      Caption = 'Opacity: 0%'
      Enabled = False
    end
    object lblColorLocation: TLabel
      Left = 152
      Top = 84
      Width = 44
      Height = 13
      Caption = 'Location:'
      Enabled = False
    end
    object lblColorLocationPercent: TLabel
      Left = 246
      Top = 84
      Width = 8
      Height = 13
      Caption = '%'
      Enabled = False
    end
    object lblAlphaLocation: TLabel
      Left = 152
      Top = 48
      Width = 44
      Height = 13
      Caption = 'Location:'
      Enabled = False
    end
    object lblAlphaLocationPercent: TLabel
      Left = 246
      Top = 48
      Width = 8
      Height = 13
      Caption = '%'
      Enabled = False
    end
    object ggbrOpacity: TGaugeBar
      Left = 16
      Top = 48
      Width = 121
      Height = 16
      Cursor = crHandPoint
      Backgnd = bgPattern
      Enabled = False
      ShowHandleGrip = True
      Style = rbsMac
      Position = 0
      OnChange = ggbrOpacityChange
    end
    object pnlPrimaryColor: TPanel
      Left = 48
      Top = 80
      Width = 89
      Height = 21
      BevelInner = bvLowered
      TabOrder = 1
      object shpPrimaryColor: TShape
        Left = 2
        Top = 2
        Width = 85
        Height = 17
        Cursor = crHandPoint
        Align = alClient
        Visible = False
        OnMouseDown = shpPrimaryColorMouseDown
      end
    end
    object edtColorLocation: TEdit
      Left = 200
      Top = 80
      Width = 40
      Height = 21
      Color = clBtnFace
      Enabled = False
      TabOrder = 2
      OnChange = edtColorLocationChange
    end
    object edtAlphaLocation: TEdit
      Left = 200
      Top = 44
      Width = 40
      Height = 21
      Color = clBtnFace
      Enabled = False
      TabOrder = 3
      OnChange = edtAlphaLocationChange
    end
    object btnDeleteSelectedColor: TButton
      Left = 272
      Top = 78
      Width = 50
      Height = 25
      Cursor = crHandPoint
      Caption = 'Delete'
      Enabled = False
      TabOrder = 4
      OnClick = btnDeleteSelectedColorClick
    end
    object btnDeleteSelectedAlphaValue: TButton
      Left = 272
      Top = 42
      Width = 50
      Height = 25
      Cursor = crHandPoint
      Caption = 'Delete'
      Enabled = False
      TabOrder = 5
      OnClick = btnDeleteSelectedAlphaValueClick
    end
    object btnAverageColors: TButton
      Left = 328
      Top = 78
      Width = 50
      Height = 25
      Cursor = crHandPoint
      Caption = 'Average'
      TabOrder = 6
      OnClick = btnAverageColorsClick
    end
    object btnAverageAlphaValues: TButton
      Left = 328
      Top = 42
      Width = 50
      Height = 25
      Cursor = crHandPoint
      Caption = 'Average'
      TabOrder = 7
      OnClick = btnAverageAlphaValuesClick
    end
  end
  object GroupBox1: TGroupBox
    Left = 424
    Top = 8
    Width = 425
    Height = 401
    Caption = 'Draw Gradient Test:'
    TabOrder = 2
    object imgvwDrawingArea: TImgView32
      Left = 16
      Top = 32
      Width = 385
      Height = 345
      Cursor = crCross
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
      OnMouseDown = imgvwDrawingAreaMouseDown
      OnMouseMove = imgvwDrawingAreaMouseMove
      OnMouseUp = imgvwDrawingAreaMouseUp
      OnPaintStage = imgvwDrawingAreaPaintStage
    end
  end
  object GroupBox2: TGroupBox
    Left = 424
    Top = 424
    Width = 425
    Height = 161
    Caption = 'Drawing Settings:'
    TabOrder = 3
    object spdbtnLinearGradient: TSpeedButton
      Left = 32
      Top = 27
      Width = 23
      Height = 22
      Cursor = crHandPoint
      Hint = 'Linear Gradient'
      GroupIndex = 1
      Down = True
      Flat = True
      Glyph.Data = {
        36050000424D3605000000000000360400002800000010000000100000000100
        0800000000000001000000000000000000000001000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000C0DCC000F0CA
        A6000020400000206000002080000020A0000020C0000020E000004000000040
        20000040400000406000004080000040A0000040C0000040E000006000000060
        20000060400000606000006080000060A0000060C0000060E000008000000080
        20000080400000806000008080000080A0000080C0000080E00000A0000000A0
        200000A0400000A0600000A0800000A0A00000A0C00000A0E00000C0000000C0
        200000C0400000C0600000C0800000C0A00000C0C00000C0E00000E0000000E0
        200000E0400000E0600000E0800000E0A00000E0C00000E0E000400000004000
        20004000400040006000400080004000A0004000C0004000E000402000004020
        20004020400040206000402080004020A0004020C0004020E000404000004040
        20004040400040406000404080004040A0004040C0004040E000406000004060
        20004060400040606000406080004060A0004060C0004060E000408000004080
        20004080400040806000408080004080A0004080C0004080E00040A0000040A0
        200040A0400040A0600040A0800040A0A00040A0C00040A0E00040C0000040C0
        200040C0400040C0600040C0800040C0A00040C0C00040C0E00040E0000040E0
        200040E0400040E0600040E0800040E0A00040E0C00040E0E000800000008000
        20008000400080006000800080008000A0008000C0008000E000802000008020
        20008020400080206000802080008020A0008020C0008020E000804000008040
        20008040400080406000804080008040A0008040C0008040E000806000008060
        20008060400080606000806080008060A0008060C0008060E000808000008080
        20008080400080806000808080008080A0008080C0008080E00080A0000080A0
        200080A0400080A0600080A0800080A0A00080A0C00080A0E00080C0000080C0
        200080C0400080C0600080C0800080C0A00080C0C00080C0E00080E0000080E0
        200080E0400080E0600080E0800080E0A00080E0C00080E0E000C0000000C000
        2000C0004000C0006000C0008000C000A000C000C000C000E000C0200000C020
        2000C0204000C0206000C0208000C020A000C020C000C020E000C0400000C040
        2000C0404000C0406000C0408000C040A000C040C000C040E000C0600000C060
        2000C0604000C0606000C0608000C060A000C060C000C060E000C0800000C080
        2000C0804000C0806000C0808000C080A000C080C000C080E000C0A00000C0A0
        2000C0A04000C0A06000C0A08000C0A0A000C0A0C000C0A0E000C0C00000C0C0
        2000C0C04000C0C06000C0C08000C0C0A000F0FBFF00A4A0A000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000
        0000000000000000000000FFFFFFFFA4A4A4525252000000000000FFFFFFA4FF
        A452A45200520000000000FFFFFFFFA4A4A4525252000000000000FFFFFFA4FF
        A452A45200520000000000FFFFFFFFA4A4A4525252000000000000FFFFFFA4FF
        A452A45200520000000000FFFFFFFFA4A4A4525252000000000000FFFFFFA4FF
        A452A45200520000000000FFFFFFFFA4A4A4525252000000000000FFFFFFA4FF
        A452A45200520000000000000000000000000000000000000000FFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
      ParentShowHint = False
      ShowHint = True
    end
    object spdbtnRadialGradient: TSpeedButton
      Left = 56
      Top = 27
      Width = 23
      Height = 22
      Cursor = crHandPoint
      Hint = 'Radial Gradient'
      GroupIndex = 1
      Flat = True
      Glyph.Data = {
        36050000424D3605000000000000360400002800000010000000100000000100
        0800000000000001000000000000000000000001000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000C0DCC000F0CA
        A6000020400000206000002080000020A0000020C0000020E000004000000040
        20000040400000406000004080000040A0000040C0000040E000006000000060
        20000060400000606000006080000060A0000060C0000060E000008000000080
        20000080400000806000008080000080A0000080C0000080E00000A0000000A0
        200000A0400000A0600000A0800000A0A00000A0C00000A0E00000C0000000C0
        200000C0400000C0600000C0800000C0A00000C0C00000C0E00000E0000000E0
        200000E0400000E0600000E0800000E0A00000E0C00000E0E000400000004000
        20004000400040006000400080004000A0004000C0004000E000402000004020
        20004020400040206000402080004020A0004020C0004020E000404000004040
        20004040400040406000404080004040A0004040C0004040E000406000004060
        20004060400040606000406080004060A0004060C0004060E000408000004080
        20004080400040806000408080004080A0004080C0004080E00040A0000040A0
        200040A0400040A0600040A0800040A0A00040A0C00040A0E00040C0000040C0
        200040C0400040C0600040C0800040C0A00040C0C00040C0E00040E0000040E0
        200040E0400040E0600040E0800040E0A00040E0C00040E0E000800000008000
        20008000400080006000800080008000A0008000C0008000E000802000008020
        20008020400080206000802080008020A0008020C0008020E000804000008040
        20008040400080406000804080008040A0008040C0008040E000806000008060
        20008060400080606000806080008060A0008060C0008060E000808000008080
        20008080400080806000808080008080A0008080C0008080E00080A0000080A0
        200080A0400080A0600080A0800080A0A00080A0C00080A0E00080C0000080C0
        200080C0400080C0600080C0800080C0A00080C0C00080C0E00080E0000080E0
        200080E0400080E0600080E0800080E0A00080E0C00080E0E000C0000000C000
        2000C0004000C0006000C0008000C000A000C000C000C000E000C0200000C020
        2000C0204000C0206000C0208000C020A000C020C000C020E000C0400000C040
        2000C0404000C0406000C0408000C040A000C040C000C040E000C0600000C060
        2000C0604000C0606000C0608000C060A000C060C000C060E000C0800000C080
        2000C0804000C0806000C0808000C080A000C080C000C080E000C0A00000C0A0
        2000C0A04000C0A06000C0A08000C0A0A000C0A0C000C0A0E000C0C00000C0C0
        2000C0C04000C0C06000C0C08000C0C0A000F0FBFF00A4A0A000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000
        00000000000000000000000000525252525252525252000000000000525252A4
        A4A4A4A452525200000000525252A4A4A4A4A4A4A45252520000005252A4A4A4
        FFFFFFA4A4A452520000005252A4A4FFFFFFFFFFA4A452520000005252A4A4FF
        FFFFFFFFA4A452520000005252A4A4FFFFFFFFFFA4A452520000005252A4A4A4
        FFFFFFA4A4A45252000000525252A4A4A4A4A4A4A452525200000000525252A4
        A4A4A4A452525200000000000052525252525252525200000000FFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
      ParentShowHint = False
      ShowHint = True
    end
    object spdbtnAngleGradient: TSpeedButton
      Left = 80
      Top = 27
      Width = 23
      Height = 22
      Cursor = crHandPoint
      Hint = 'Angle Gradient'
      GroupIndex = 1
      Glyph.Data = {
        36050000424D3605000000000000360400002800000010000000100000000100
        0800000000000001000000000000000000000001000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000C0DCC000F0CA
        A6000020400000206000002080000020A0000020C0000020E000004000000040
        20000040400000406000004080000040A0000040C0000040E000006000000060
        20000060400000606000006080000060A0000060C0000060E000008000000080
        20000080400000806000008080000080A0000080C0000080E00000A0000000A0
        200000A0400000A0600000A0800000A0A00000A0C00000A0E00000C0000000C0
        200000C0400000C0600000C0800000C0A00000C0C00000C0E00000E0000000E0
        200000E0400000E0600000E0800000E0A00000E0C00000E0E000400000004000
        20004000400040006000400080004000A0004000C0004000E000402000004020
        20004020400040206000402080004020A0004020C0004020E000404000004040
        20004040400040406000404080004040A0004040C0004040E000406000004060
        20004060400040606000406080004060A0004060C0004060E000408000004080
        20004080400040806000408080004080A0004080C0004080E00040A0000040A0
        200040A0400040A0600040A0800040A0A00040A0C00040A0E00040C0000040C0
        200040C0400040C0600040C0800040C0A00040C0C00040C0E00040E0000040E0
        200040E0400040E0600040E0800040E0A00040E0C00040E0E000800000008000
        20008000400080006000800080008000A0008000C0008000E000802000008020
        20008020400080206000802080008020A0008020C0008020E000804000008040
        20008040400080406000804080008040A0008040C0008040E000806000008060
        20008060400080606000806080008060A0008060C0008060E000808000008080
        20008080400080806000808080008080A0008080C0008080E00080A0000080A0
        200080A0400080A0600080A0800080A0A00080A0C00080A0E00080C0000080C0
        200080C0400080C0600080C0800080C0A00080C0C00080C0E00080E0000080E0
        200080E0400080E0600080E0800080E0A00080E0C00080E0E000C0000000C000
        2000C0004000C0006000C0008000C000A000C000C000C000E000C0200000C020
        2000C0204000C0206000C0208000C020A000C020C000C020E000C0400000C040
        2000C0404000C0406000C0408000C040A000C040C000C040E000C0600000C060
        2000C0604000C0606000C0608000C060A000C060C000C060E000C0800000C080
        2000C0804000C0806000C0808000C080A000C080C000C080E000C0A00000C0A0
        2000C0A04000C0A06000C0A08000C0A0A000C0A0C000C0A0E000C0C00000C0C0
        2000C0C04000C0C06000C0C08000C0C0A000F0FBFF00A4A0A000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000
        000000000000000000000052A452A4525252525200520000000000A4A4A452A4
        5252520052000000000000A4A4A4A4525252525200000000000000A4A4A4A4A4
        5252520000000000000000A4FFA4FFA4A452000052005200000000FFA4FFA4FF
        FFFF525200520052000000FFFFFFFFFFFFA4A45252525252520000FFFFFFA4FF
        A4A4A4A452525252520000FFFFA4FFA4A4A4A452A4525252520000FFA4FFA4A4
        A4A4A4A452A452A4520000000000000000000000000000000000FFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
      ParentShowHint = False
      ShowHint = True
    end
    object spdbtnReflectedGradient: TSpeedButton
      Left = 104
      Top = 27
      Width = 23
      Height = 22
      Cursor = crHandPoint
      Hint = 'Reflected Gradient'
      GroupIndex = 1
      Flat = True
      Glyph.Data = {
        36050000424D3605000000000000360400002800000010000000100000000100
        0800000000000001000000000000000000000001000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000C0DCC000F0CA
        A6000020400000206000002080000020A0000020C0000020E000004000000040
        20000040400000406000004080000040A0000040C0000040E000006000000060
        20000060400000606000006080000060A0000060C0000060E000008000000080
        20000080400000806000008080000080A0000080C0000080E00000A0000000A0
        200000A0400000A0600000A0800000A0A00000A0C00000A0E00000C0000000C0
        200000C0400000C0600000C0800000C0A00000C0C00000C0E00000E0000000E0
        200000E0400000E0600000E0800000E0A00000E0C00000E0E000400000004000
        20004000400040006000400080004000A0004000C0004000E000402000004020
        20004020400040206000402080004020A0004020C0004020E000404000004040
        20004040400040406000404080004040A0004040C0004040E000406000004060
        20004060400040606000406080004060A0004060C0004060E000408000004080
        20004080400040806000408080004080A0004080C0004080E00040A0000040A0
        200040A0400040A0600040A0800040A0A00040A0C00040A0E00040C0000040C0
        200040C0400040C0600040C0800040C0A00040C0C00040C0E00040E0000040E0
        200040E0400040E0600040E0800040E0A00040E0C00040E0E000800000008000
        20008000400080006000800080008000A0008000C0008000E000802000008020
        20008020400080206000802080008020A0008020C0008020E000804000008040
        20008040400080406000804080008040A0008040C0008040E000806000008060
        20008060400080606000806080008060A0008060C0008060E000808000008080
        20008080400080806000808080008080A0008080C0008080E00080A0000080A0
        200080A0400080A0600080A0800080A0A00080A0C00080A0E00080C0000080C0
        200080C0400080C0600080C0800080C0A00080C0C00080C0E00080E0000080E0
        200080E0400080E0600080E0800080E0A00080E0C00080E0E000C0000000C000
        2000C0004000C0006000C0008000C000A000C000C000C000E000C0200000C020
        2000C0204000C0206000C0208000C020A000C020C000C020E000C0400000C040
        2000C0404000C0406000C0408000C040A000C040C000C040E000C0600000C060
        2000C0604000C0606000C0608000C060A000C060C000C060E000C0800000C080
        2000C0804000C0806000C0808000C080A000C080C000C080E000C0A00000C0A0
        2000C0A04000C0A06000C0A08000C0A0A000C0A0C000C0A0E000C0C00000C0C0
        2000C0C04000C0C06000C0C08000C0C0A000F0FBFF00A4A0A000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000
        00000000000000000000005252525252525252525252525252000052A452A452
        A452A452A452A452A40000A4A4A4A4A4A4A4A4A4A4A4A4A4A40000FFA4FFA4FF
        A4FFA4FFA4FFA4FFA40000FFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFF
        FFFFFFFFFFFFFFFFFF0000A4FFA4FFA4FFA4FFA4FFA4FFA4FF0000A4A4A4A4A4
        A4A4A4A4A4A4A4A4A40000A452A452A452A452A452A452A45200005252525252
        5252525252525252520000000000000000000000000000000000FFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
      ParentShowHint = False
      ShowHint = True
    end
    object spdbtnDiamondGradient: TSpeedButton
      Left = 128
      Top = 27
      Width = 23
      Height = 22
      Cursor = crHandPoint
      Hint = 'Diamond Gradient'
      GroupIndex = 1
      Flat = True
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        0400000000008000000000000000000000001000000000000000000000000000
        8000008000000080800080000000800080008080000080808000C0C0C0000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFF000000000000000000000070F07000000000070F7F07
        0000000070F7F7F0700000070F7FFF7F07000070F7FFFFF7F07000070F7FFF7F
        0700000070F7F7F070000000070F7F07000000000070F0700000000000070700
        00000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
      ParentShowHint = False
      ShowHint = True
    end
    object lblBlendMode: TLabel
      Left = 184
      Top = 96
      Width = 30
      Height = 13
      Caption = 'Mode:'
      Enabled = False
    end
    object lblBlendOpacity: TLabel
      Left = 176
      Top = 120
      Width = 39
      Height = 13
      Caption = 'Opacity:'
      Enabled = False
    end
    object lblBlendOpacityPercent: TLabel
      Left = 368
      Top = 120
      Width = 26
      Height = 13
      Caption = '100%'
      Enabled = False
    end
    object lblChannel: TLabel
      Left = 172
      Top = 68
      Width = 42
      Height = 13
      Caption = 'Channel:'
      Enabled = False
    end
    object chckbxReverse: TCheckBox
      Left = 232
      Top = 30
      Width = 97
      Height = 17
      Cursor = crHandPoint
      Caption = 'Reverse'
      TabOrder = 0
    end
    object cmbbxBlendModes: TComboBox
      Left = 232
      Top = 92
      Width = 169
      Height = 21
      Cursor = crHandPoint
      DropDownCount = 40
      Enabled = False
      ItemHeight = 13
      TabOrder = 1
    end
    object rdbtnDrawNormal: TRadioButton
      Left = 32
      Top = 62
      Width = 89
      Height = 17
      Cursor = crHandPoint
      Caption = 'Draw Normal'
      Checked = True
      TabOrder = 2
      TabStop = True
      OnClick = ChangeDrawingModeClick
    end
    object rdbtnDrawBlend: TRadioButton
      Left = 32
      Top = 88
      Width = 81
      Height = 17
      Cursor = crHandPoint
      Caption = 'Draw Blend'
      TabOrder = 3
      OnClick = ChangeDrawingModeClick
    end
    object ggbrBlendOpacity: TGaugeBar
      Left = 232
      Top = 120
      Width = 129
      Height = 16
      Cursor = crHandPoint
      Backgnd = bgPattern
      Enabled = False
      ShowHandleGrip = True
      Style = rbsMac
      Position = 100
      OnChange = ggbrBlendOpacityChange
    end
    object cmbbxChannel: TComboBox
      Left = 232
      Top = 64
      Width = 169
      Height = 21
      Cursor = crHandPoint
      Enabled = False
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 5
      Text = 'RGB'
      Items.Strings = (
        'RGB'
        'Red'
        'Green'
        'Blue')
    end
  end
  object grpbxPresets: TGroupBox
    Left = 8
    Top = 8
    Width = 401
    Height = 273
    Caption = 'Presets:'
    TabOrder = 4
    object imgvwGradientGallery: TImgView32
      Left = 16
      Top = 24
      Width = 369
      Height = 153
      Bitmap.ResamplerClassName = 'TNearestResampler'
      BitmapAlign = baCustom
      Scale = 1.000000000000000000
      ScaleMode = smScale
      ScrollBars.ShowHandleGrip = True
      ScrollBars.Style = rbsDefault
      ScrollBars.Size = 21
      ScrollBars.Visibility = svAuto
      OverSize = 0
      TabOrder = 0
      OnMouseDown = imgvwGradientGalleryMouseDown
      OnMouseMove = imgvwGradientGalleryMouseMove
    end
    object GroupBox3: TGroupBox
      Left = 16
      Top = 192
      Width = 369
      Height = 65
      TabOrder = 1
      object Label1: TLabel
        Left = 20
        Top = 16
        Width = 84
        Height = 13
        Caption = 'Foreground Color:'
      end
      object shpForegroundColor: TShape
        Left = 112
        Top = 14
        Width = 20
        Height = 17
        Cursor = crHandPoint
        Brush.Color = clBlack
        OnMouseDown = shpForegroundColorMouseDown
      end
      object Label2: TLabel
        Left = 16
        Top = 40
        Width = 88
        Height = 13
        Caption = 'Background Color:'
      end
      object shpBackgroundColor: TShape
        Left = 112
        Top = 38
        Width = 20
        Height = 17
        Cursor = crHandPoint
        OnMouseDown = shpBackgroundColorMouseDown
      end
      object btnGradientOptions: TButton
        Left = 152
        Top = 22
        Width = 65
        Height = 25
        Cursor = crHandPoint
        Caption = 'Options'
        TabOrder = 0
        OnClick = btnGradientOptionsClick
      end
    end
  end
  object edtGradientName: TEdit
    Left = 56
    Top = 304
    Width = 265
    Height = 21
    TabOrder = 5
    OnChange = edtGradientNameChange
  end
  object btnAddNewGradient: TButton
    Left = 336
    Top = 302
    Width = 65
    Height = 25
    Cursor = crHandPoint
    Caption = 'New'
    TabOrder = 6
    OnClick = btnAddNewGradientClick
  end
  object clrdlgPickColor: TColorDialog
    Options = [cdFullOpen]
    Left = 120
    Top = 472
  end
  object opndlgLoadGradients: TOpenDialog
    Filter = 'GraphicsMagic Gradient files (*.grd)|*.grd'
    Left = 152
    Top = 472
  end
  object svdlgSaveGradients: TSaveDialog
    Filter = 'GraphicsMagic Gradient files (*.grd)|*.grd'
    Left = 184
    Top = 472
  end
  object pmnGradientOptions: TPopupMenu
    OnPopup = pmnGradientOptionsPopup
    Left = 216
    Top = 472
    object mnitmSmallThumbnail: TMenuItem
      Caption = 'Small Thumbnail'
      OnClick = mnitmSmallThumbnailClick
    end
    object mnitmLargeThumbnail: TMenuItem
      Caption = 'Large Thumbnail'
      OnClick = mnitmLargeThumbnailClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object mnitmResetGradients: TMenuItem
      Caption = 'Reset Gradients'
      OnClick = mnitmResetGradientsClick
    end
    object mnitmLoadGradients: TMenuItem
      Caption = 'Load Gradients...'
      OnClick = mnitmLoadGradientsClick
    end
    object mnitmSaveGradients: TMenuItem
      Caption = 'Save Gradients...'
      OnClick = mnitmSaveGradientsClick
    end
    object mnitmReplaceGradients: TMenuItem
      Caption = 'Replace Gradients...'
      OnClick = mnitmReplaceGradientsClick
    end
  end
end
