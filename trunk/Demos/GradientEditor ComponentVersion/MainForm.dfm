object frmMain: TfrmMain
  Left = 192
  Top = 133
  Width = 804
  Height = 640
  Caption = 'Gradient Editor Example'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 393
    Top = 0
    Width = 4
    Height = 575
  end
  object pnlEdit: TPanel
    Left = 0
    Top = 0
    Width = 393
    Height = 575
    Align = alLeft
    TabOrder = 0
    object Bevel1: TBevel
      Left = 1
      Top = 375
      Width = 391
      Height = 9
      Align = alBottom
      Shape = bsSpacer
    end
    object Bevel2: TBevel
      Left = 1
      Top = 295
      Width = 391
      Height = 9
      Align = alBottom
      Shape = bsSpacer
    end
    object grpbxStops: TGroupBox
      Left = 1
      Top = 384
      Width = 391
      Height = 190
      Align = alBottom
      Caption = 'Stops'
      TabOrder = 0
      object lbl2: TLabel
        Left = 72
        Top = 22
        Width = 233
        Height = 13
        Caption = 'Right click on any color stop to swtich stop types.'
      end
      object lblOpacity: TLabel
        Left = 16
        Top = 52
        Width = 56
        Height = 13
        Caption = 'Opacity: 0%'
      end
      object lblColor: TLabel
        Left = 16
        Top = 108
        Width = 27
        Height = 13
        Caption = 'Color:'
      end
      object lblAlphaLocation: TLabel
        Left = 152
        Top = 72
        Width = 44
        Height = 13
        Caption = 'Location:'
      end
      object lblColorLocation: TLabel
        Left = 152
        Top = 108
        Width = 44
        Height = 13
        Caption = 'Location:'
      end
      object lblAlphaLocationPercent: TLabel
        Left = 246
        Top = 72
        Width = 8
        Height = 13
        Caption = '%'
      end
      object lblColorLocationPercent: TLabel
        Left = 246
        Top = 108
        Width = 8
        Height = 13
        Caption = '%'
      end
      object rdbtnDirectMode: TRadioButton
        Left = 192
        Top = 152
        Width = 113
        Height = 17
        Cursor = crHandPoint
        Caption = 'Direct Edit Mode'
        TabOrder = 0
        OnClick = ChangeEditMode
      end
      object rdbtnCopyMode: TRadioButton
        Left = 48
        Top = 152
        Width = 113
        Height = 17
        Cursor = crHandPoint
        Caption = 'Copy Edit Mode'
        TabOrder = 1
        OnClick = ChangeEditMode
      end
      object ggbrOpacity: TGaugeBar
        Left = 16
        Top = 72
        Width = 121
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
        Top = 104
        Width = 89
        Height = 21
        BevelInner = bvLowered
        TabOrder = 3
        object shpStopColor: TShape
          Left = 2
          Top = 2
          Width = 85
          Height = 17
          Cursor = crHandPoint
          Align = alClient
          Visible = False
          OnMouseDown = shpStopColorMouseDown
        end
      end
      object edtAlphaLocation: TEdit
        Left = 200
        Top = 68
        Width = 40
        Height = 21
        Color = clBtnFace
        TabOrder = 4
        OnChange = edtAlphaLocationChange
      end
      object edtColorLocation: TEdit
        Left = 200
        Top = 104
        Width = 40
        Height = 21
        Color = clBtnFace
        TabOrder = 5
        OnChange = edtColorLocationChange
      end
      object btnDeleteSelectedAlphaValue: TButton
        Left = 272
        Top = 66
        Width = 50
        Height = 25
        Cursor = crHandPoint
        Caption = 'Delete'
        Enabled = False
        TabOrder = 6
        OnClick = btnDeleteSelectedAlphaValueClick
      end
      object btnDeleteSelectedColor: TButton
        Left = 272
        Top = 102
        Width = 50
        Height = 25
        Cursor = crHandPoint
        Caption = 'Delete'
        Enabled = False
        TabOrder = 7
        OnClick = btnDeleteSelectedColorClick
      end
      object btnAverageAlphaValues: TButton
        Left = 328
        Top = 66
        Width = 50
        Height = 25
        Cursor = crHandPoint
        Caption = 'Average'
        TabOrder = 8
        OnClick = btnAverageAlphaValuesClick
      end
      object btnAverageColors: TButton
        Left = 328
        Top = 102
        Width = 50
        Height = 25
        Cursor = crHandPoint
        Caption = 'Average'
        TabOrder = 9
        OnClick = btnAverageColorsClick
      end
    end
    object geGradientEditor: TgmGradientEditor
      Left = 1
      Top = 304
      Width = 391
      Height = 71
      Align = alBottom
      OnMouseDown = geGradientEditorMouseDown
      OnMouseMove = geGradientEditorMouseMove
      OnMouseUp = geGradientEditorMouseUp
      EditMode = eemCopyEdit
      Gradients = glGradientList
      GradientIndex = -1
      DefaultNewColor = clBlack
      DefaultNewAlpha = 255
      OnStateChanged = geGradientEditorStateChanged
      OnCursorPosChanged = geGradientEditorCursorPosChanged
    end
    object pnlNewGradient: TPanel
      Left = 1
      Top = 252
      Width = 391
      Height = 43
      Align = alBottom
      TabOrder = 2
      object lblGradientName: TLabel
        Left = 16
        Top = 16
        Width = 31
        Height = 13
        Caption = 'Name:'
      end
      object edtGradientName: TEdit
        Left = 56
        Top = 12
        Width = 265
        Height = 21
        TabOrder = 0
      end
      object btnAdd: TButton
        Left = 328
        Top = 10
        Width = 43
        Height = 25
        Cursor = crHandPoint
        Caption = 'Add'
        TabOrder = 1
        OnClick = btnAddClick
      end
    end
    object Panel1: TPanel
      Left = 1
      Top = 1
      Width = 391
      Height = 251
      Align = alClient
      TabOrder = 3
      object Label1: TLabel
        Left = 24
        Top = 224
        Width = 84
        Height = 13
        Caption = 'Foreground Color:'
      end
      object shpForeColor: TShape
        Left = 112
        Top = 224
        Width = 65
        Height = 16
        Cursor = crHandPoint
        Brush.Color = clBlack
        OnMouseDown = shpForeColorMouseDown
      end
      object Label2: TLabel
        Left = 192
        Top = 224
        Width = 88
        Height = 13
        Caption = 'Background Color:'
      end
      object shpBackColor: TShape
        Left = 288
        Top = 224
        Width = 65
        Height = 16
        Cursor = crHandPoint
        OnMouseDown = shpBackColorMouseDown
      end
      object grpbxPreset: TGroupBox
        Left = 1
        Top = 1
        Width = 389
        Height = 208
        Align = alTop
        Caption = 'Presets'
        TabOrder = 0
        object ScrollBox1: TScrollBox
          Left = 8
          Top = 23
          Width = 369
          Height = 170
          TabOrder = 0
          object glvGradientListView: TgmGradientListView
            Left = 0
            Top = 0
            Width = 321
            Height = 65
            AutoSize = True
            GradientList = glGradientList
            ParentShowHint = False
            Scale = 1.000000000000000000
            ScaleMode = smNormal
            ShowHint = True
            TabOrder = 0
            GridOptions = [goDragable, goSelection]
            CellBorderStyle = borContrastGrid
            OnChange = glvGradientListViewChange
            OnMouseDown = glvGradientListViewMouseDown
          end
        end
      end
    end
  end
  object Panel2: TPanel
    Left = 397
    Top = 0
    Width = 399
    Height = 575
    Align = alClient
    TabOrder = 1
    object grp1: TGroupBox
      Left = 1
      Top = 1
      Width = 397
      Height = 415
      Align = alClient
      Caption = 'Draw Gradient Test:'
      TabOrder = 0
      DesignSize = (
        397
        415)
      object imgvwDrawingArea: TImgView32
        Left = 16
        Top = 24
        Width = 361
        Height = 369
        Cursor = crCross
        Anchors = [akLeft, akTop, akBottom]
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
    object grp2: TGroupBox
      Left = 1
      Top = 416
      Width = 397
      Height = 158
      Align = alBottom
      Caption = 'Drawing Settings:'
      TabOrder = 1
      object btnLinearGradient: TSpeedButton
        Left = 16
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
      object btnRadialGradient: TSpeedButton
        Left = 40
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
      object btnAngleGradient: TSpeedButton
        Left = 64
        Top = 27
        Width = 23
        Height = 22
        Cursor = crHandPoint
        Hint = 'Angle Gradient'
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
      object btnReflectedGradient: TSpeedButton
        Left = 88
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
      object btnDiamondGradient: TSpeedButton
        Left = 112
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
        Left = 168
        Top = 96
        Width = 30
        Height = 13
        Caption = 'Mode:'
        Enabled = False
      end
      object lblBlendOpacity: TLabel
        Left = 160
        Top = 120
        Width = 39
        Height = 13
        Caption = 'Opacity:'
        Enabled = False
      end
      object lblBlendOpacityPercent: TLabel
        Left = 352
        Top = 120
        Width = 26
        Height = 13
        Caption = '100%'
        Enabled = False
      end
      object lblChannel: TLabel
        Left = 156
        Top = 68
        Width = 42
        Height = 13
        Caption = 'Channel:'
        Enabled = False
      end
      object chkReverse: TCheckBox
        Left = 216
        Top = 30
        Width = 97
        Height = 17
        Cursor = crHandPoint
        Caption = 'Reverse'
        TabOrder = 0
      end
      object cbbBlendModes: TComboBox
        Left = 216
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
        Left = 16
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
        Left = 16
        Top = 88
        Width = 81
        Height = 17
        Cursor = crHandPoint
        Caption = 'Draw Blend'
        TabOrder = 3
        OnClick = ChangeDrawingModeClick
      end
      object scrollBlendOpacity: TGaugeBar
        Left = 216
        Top = 120
        Width = 129
        Height = 16
        Cursor = crHandPoint
        Backgnd = bgPattern
        Enabled = False
        ShowHandleGrip = True
        Style = rbsMac
        Position = 100
        OnChange = scrollBlendOpacityChange
      end
      object cbbChannel: TComboBox
        Left = 216
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
  end
  object glGradientList: TgmGradientList
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
            Value = 12058891
            LocationScale = 0.100000001490116100
          end
          item
            Value = 260861
            LocationScale = 0.500000000000000000
          end
          item
            Value = 11141643
            LocationScale = 0.899999976158142100
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
            Value = 159487
          end
          item
            Value = clYellow
            LocationScale = 0.500000000000000000
            MidPoint = 0.469999998807907100
          end
          item
            Value = 28159
            LocationScale = 1.000000000000000000
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
            LocationScale = 0.050000000745058060
          end
          item
            Value = 7083375
            LocationScale = 0.349999994039535600
          end
          item
            Value = 31997
            LocationScale = 0.649999976158142100
          end
          item
            Value = 7612416
            LocationScale = 0.949999988079071100
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
            Value = 12966139
            LocationScale = 0.300000011920929000
          end
          item
            Value = 1453676
            LocationScale = 0.829999983310699500
            MidPoint = 0.600000023841857900
          end
          item
            Value = 13491183
            LocationScale = 1.000000000000000000
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
            Value = 13404457
          end
          item
            Value = clWhite
            LocationScale = 0.500000000000000000
            MidPoint = 0.129999995231628400
          end
          item
            Value = 27280
            LocationScale = 0.519999980926513700
          end
          item
            Value = 40921
            LocationScale = 0.639999985694885200
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
            LocationScale = 0.150000005960464500
          end
          item
            Value = clBlue
            LocationScale = 0.330000013113021900
          end
          item
            Value = clAqua
            LocationScale = 0.490000009536743200
          end
          item
            Value = clLime
            LocationScale = 0.670000016689300500
          end
          item
            Value = clYellow
            LocationScale = 0.839999973773956400
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
            LocationScale = 0.070000000298023220
            MidPoint = 0.699999988079071100
          end
          item
            Value = clWhite
            LocationScale = 0.109999999403953600
          end
          item
            Value = clWhite
            LocationScale = 0.879999995231628400
            MidPoint = 0.200000002980232200
          end
          item
            Value = 13421772
            LocationScale = 0.930000007152557300
          end
          item
            Value = clBlack
            LocationScale = 1.000000000000000000
          end>
        RGBGradient = <
          item
            Value = clRed
            LocationScale = 0.119999997317791000
          end
          item
            Value = 64767
            LocationScale = 0.280000001192092900
          end
          item
            Value = 3781633
            LocationScale = 0.449999988079071000
            MidPoint = 0.600000023841857900
          end
          item
            Value = 16771584
            LocationScale = 0.600000023841857900
            MidPoint = 0.349999994039535600
          end
          item
            Value = 9437952
            LocationScale = 0.750000000000000000
            MidPoint = 0.600000023841857900
          end
          item
            Value = 12976383
            LocationScale = 0.879999995231628400
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
            LocationScale = 0.090000003576278680
          end
          item
            Value = clBlack
            LocationScale = 0.100000001490116100
          end
          item
            Value = clBlack
            LocationScale = 0.189999997615814200
          end
          item
            Value = clWhite
            LocationScale = 0.200000002980232200
          end
          item
            Value = clWhite
            LocationScale = 0.289999991655349700
          end
          item
            Value = clBlack
            LocationScale = 0.300000011920929000
          end
          item
            Value = clBlack
            LocationScale = 0.389999985694885300
          end
          item
            Value = clWhite
            LocationScale = 0.400000005960464400
          end
          item
            Value = clWhite
            LocationScale = 0.490000009536743200
          end
          item
            Value = clBlack
            LocationScale = 0.500000000000000000
          end
          item
            Value = clBlack
            LocationScale = 0.589999973773956400
          end
          item
            Value = clWhite
            LocationScale = 0.600000023841857900
          end
          item
            Value = clWhite
            LocationScale = 0.689999997615814300
          end
          item
            Value = clBlack
            LocationScale = 0.699999988079071100
          end
          item
            Value = clBlack
            LocationScale = 0.790000021457672100
          end
          item
            Value = clWhite
            LocationScale = 0.800000011920928900
          end
          item
            Value = clWhite
            LocationScale = 0.889999985694885200
          end
          item
            Value = clBlack
            LocationScale = 0.899999976158142100
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
    Left = 472
    Top = 224
  end
  object ActionList: TActionList
    Left = 472
    Top = 192
    object actDelete: TAction
      Category = 'Options'
      Caption = 'actDelete'
      OnExecute = actDeleteExecute
      OnUpdate = actDeleteUpdate
    end
    object actClear: TAction
      Category = 'Options'
      Caption = 'actClear'
      OnExecute = actClearExecute
      OnUpdate = actClearUpdate
    end
    object actLoadGradients: TAction
      Category = 'Options'
      Caption = 'actLoadGradients'
      OnExecute = actLoadGradientsExecute
    end
    object actReplaceGradients: TAction
      Category = 'Options'
      Caption = 'actReplaceGradients'
      OnExecute = actReplaceGradientsExecute
    end
    object actSaveGradients: TAction
      Category = 'Options'
      Caption = 'actSaveGradients'
      OnExecute = actSaveGradientsExecute
    end
    object actThumbSize16: TAction
      Tag = 16
      Category = 'Options'
      Caption = 'actThumbSize16'
      OnExecute = ChangeThumbSize
      OnUpdate = ThumbSizeUpdate
    end
    object actThumbSize32: TAction
      Tag = 32
      Category = 'Options'
      Caption = 'actThumbSize32'
      OnExecute = ChangeThumbSize
      OnUpdate = ThumbSizeUpdate
    end
    object actThumbSize48: TAction
      Tag = 48
      Category = 'Options'
      Caption = 'actThumbSize48'
      OnExecute = ChangeThumbSize
      OnUpdate = ThumbSizeUpdate
    end
    object actThumbSize64: TAction
      Tag = 64
      Category = 'Options'
      Caption = 'actThumbSize64'
      OnExecute = ChangeThumbSize
      OnUpdate = ThumbSizeUpdate
    end
    object actExit: TAction
      Category = 'Options'
      Caption = 'Exit'
      OnExecute = actExitExecute
    end
  end
  object MainMenu: TMainMenu
    Left = 512
    Top = 192
    object mnitmOptions: TMenuItem
      Caption = 'Options'
      object mnitmClear: TMenuItem
        Action = actClear
        Caption = 'Clear'
      end
      object mnitmDelete: TMenuItem
        Action = actDelete
        Caption = 'Delete'
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object mnitmLoadGradients: TMenuItem
        Action = actLoadGradients
        Caption = 'Load Gradients...'
      end
      object mnitmReplaceGradients1: TMenuItem
        Action = actReplaceGradients
        Caption = 'Replace Gradients...'
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object mnitmSaveGradients: TMenuItem
        Action = actSaveGradients
        Caption = 'Save Gradients...'
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object mnitmThumbSize16: TMenuItem
        Action = actThumbSize16
        Caption = 'Thumbnail Size 16 x 16'
      end
      object mnitmThumbSize32: TMenuItem
        Action = actThumbSize32
        Caption = 'Thumbnail Size 32 x 32'
      end
      object mnitmThumbSize48: TMenuItem
        Action = actThumbSize48
        Caption = 'Thumbnail Size 48 x 48'
      end
      object mnitmThumbSize64: TMenuItem
        Action = actThumbSize64
        Caption = 'Thumbnail Size 64 x 64'
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object mnitmExit: TMenuItem
        Action = actExit
      end
    end
  end
  object OpenGradientDialog: TOpenGradientDialog
    Left = 512
    Top = 224
  end
  object SaveGradientDialog: TSaveGradientDialog
    Left = 552
    Top = 224
  end
  object ColorDialog: TColorDialog
    Options = [cdFullOpen]
    Left = 552
    Top = 192
  end
end
