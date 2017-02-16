// efg, November 1998

UNIT ColorNameLibrary;

INTERFACE

  USES
    Graphics;   // TColor

//  ===== GretagMacbeth ColorChecker (R) Colors  ==============================

  // The GretagMacbeth ColorChecker (R) displays 24 controlled colors.
  // This routine creates an equivalent Color Table on the screen or the
  // printer for comparison with the GretagMacbeth controlled reference.

  // The back of the enclosure for the ColorChecker provides the following
  // information about these 24 colors:
  //
  //      +-----+-----+-----+-----+-----+-----+
  //      |  1  |  2  |  3  |  4  |  5  |  6  |
  //      +-----+-----+-----+-----+-----+-----+
  //      |  7  |  8  |  9  | 10  | 11  | 12  |
  //      +-----+-----+-----+-----+-----+-----+
  //      | 13  | 14  | 15  | 16  | 17  | 18  |
  //      +-----+-----+-----+-----+-----+-----+
  //      | 19  | 20  | 21  | 22  | 23  | 24  |
  //      +-----+-----+-----+-----+-----+-----+
  //
  //                         CIE (1931)       Munsell Notation  ISCC/NBS Name
  //  No. Name             x      y    Y
  //  1.  dark skin     0.400  0.350  10.1    3   YR 3.7 / 3.2  moderate brown
  //  2.  light skin    0.377  0.345  35.8    2.2 YR 6.47/ 4.1  light redish brown
  //  3.  blue sky      0.247  0.251  19.3    4.3 PB 4.95/ 5.5  moderate blue
  //  4.  foliage       0.337  0.422  13.3    6.7 GY 4.2 / 4.1  moderate olive green
  //  5.  blue flower   0.265  0.240  24.3    9.7 PB 5.47/ 6.7  light violet
  //  6.  bluish green  0.261  0.343  43.1    2.5 BG 7   / 6    light bluish green
  //  7.  orange        0.506  0.407  30.1    5   YR 6   /11    strong orange
  //  8.  purplish blue 0.211  0.175  12.0    7.5 PB 4   /10.7  strong purplish blue
  //  9.  moderate red  0.453  0.306  19.8    2.5 R  5   /10    moderate red
  // 10.  purple        0.285  0.202   6.6    5   P  3   / 7    deep purple
  // 11.  yellow green  0.380  0.489  44.3    5   GY 7.1 / 9.1  strong yellow green
  // 12.  orange yellow 0.473  0.438  43.1   10   YR 7   /10.5  strong orange yellow
  // 13.  blue          0.187  0.129   6.1    7.5 PB 2.9 /12.7  vivid purplish blue
  // 14.  green         0.305  0.478  23.4    0.25 G 5.4 / 8.65 strong yellowish green
  // 15.  red           0.539  0.313  12.0    5   R  4   /12    strong red
  // 16.  yellow        0.448  0.470  59.1    5   Y  8   /11.1  vivid yellow
  // 17.  magenta       0.364  0.233  19.8    2.5 RP 5   /12    strong reddish purple
  // 18.  cyan          0.196  0.252  19.8    5   B  5   / 8    strong greenish blue
  // 19.  white         0.310  0.316  90.0        N  9.5 /      white
  // 20.  neutral 8     0.310  0.316  59.1        N  8   /      light gray
  // 21.  neutral 6.5   0.310  0.316  36.2        N  6.5 /      light-medium gray
  // 22.  neutral 5     0.310  0.316  19.8        N  5   /      medium gray
  // 23.  neutral 3.35  0.310  0.316   9.0        N  3.5 /      dark gray
  // 24.  black         0.310  0.316   3.1        N  2   /      black

  // The color values in Munsell Notation were converted to RGB values using
  // the Munsell Conversion Program (Version 1.5 Beta), 2/3/1998.  These RGB
  // values are stored in the following array of TRGBTriples.

  CONST
    // Name               $bbggrr         R    G   B       H      S      V
    //-----------------   -------        ---  --- ---    -----  -----  -----
    GretagDarkSkin      = $526081;   //  129   96  82     17.9  0.364  0.506
    GretagLightSkin     = $90A3CD;   //  205  163 144     18.7  0.298  0.804
    GretagBlueSky       = $A98770;   //  112  135 169    215.8  0.337  0.663
    GretagFoliage       = $517967;   //  103  121  81     87.0  0.331  0.475
    GretagBlueFlower    = $BC8E92;   //  146  142 188    245.2  0.245  0.737
    GretagBluishGreen   = $B6C678;   //  120  198 182    167.7  0.394  0.776
    GretagOrange        = $398DDF;   //  223  141  57     30.4  0.744  0.875
    GretagPurplishBlue  = $B26A5B;   //   91  106 178    229.7  0.489  0.698
    GretagModerateRed   = $726ACB;   //  203  106 114    355.1  0.478  0.796
    GretagPurple        = $7A4A6B;   //  107   74 122    281.3  0.393  0.478
    GretagYellowGreen   = $4EC5AD;   //  173  197  78     72.1  0.604  0.773
    GretagOrangeYellow  = $3DAFE9;   //  233  175  61     39.8  0.738  0.914
    GretagBlue          = $A34B41;   //   65   75 163    233.9  0.601  0.639
    GretagGreen         = $57A058;   //   88  160  87    119.2  0.456  0.627
    GretagRed           = $4946BA;   //  186   70  73    358.4  0.624  0.729
    GretagYellow        = $2FD0F0;   //  240  208  47     50.1  0.804  0.941
    GretagMagenta       = $A267C5;   //  197  103 162    322.3  0.477  0.773
    GretagCyan          = $B29200;   //    0  146 178    190.8  1.000  0.698
    GretagWhite         = $F8F8F9;   //  249  248 248      0.0  0.004  0.976
    GretagNeutral8      = $D2D1D3;   //  211  209 210    330.0  0.009  0.827
    GretagNeutral65     = $ADACAD;   //  173  172 173    300.0  0.006  0.678
    GretagNeutral5      = $878788;   //  136  135 135      0.0  0.007  0.533
    GretagNeutral35     = $636363;   //   99   99  99       -   0.000  0.388
    GretagBlack         = $414141;   //   65   65  65       -   0.000  0.255

    GretagMacbethColors:  ARRAY[1..24] OF TColor =
      (GretagDarkSkin,
       GretagLightSkin,
       GretagBlueSky,
       GretagFoliage,
       GretagBlueFlower,
       GretagBluishGreen,
       GretagOrange,
       GretagPurplishBlue,
       GretagModerateRed,
       GretagPurple,
       GretagYellowGreen,
       GretagOrangeYellow,
       GretagBlue,
       GretagGreen,
       GretagRed,
       GretagYellow,
       GretagMagenta,
       GretagCyan,
       GretagWhite,
       GretagNeutral8,
       GretagNeutral65,
       GretagNeutral5,
       GretagNeutral35,
       GretagBlack);

    GretagMacbethNames:  ARRAY[1..24] OF STRING =
      ('dark skin',
       'light skin',
       'blue sky',
       'foliage',
       'blue flower',
       'bluish green',
       'orange',
       'purplish blue',
       'moderate red',
       'purple',
       'yellow green',
       'orange yellow',
       'blue',
       'green',
       'red',
       'yellow',
       'magenta',
       'cyan',
       'white',
       'neutral 8',
       'neutral 6.5',
       'neutral 5',
       'neutral 3.5',
       'black');


//  ===== Netscape Colors =====================================================

  // Adapted from "HYPE's Color Specifier for Netscape v.3" at
  // www.users.interport.net/~giant/default.html, and convert to HSV and
  // sorted by HSV (Hue, Saturation, Value)
  CONST
    // Name                   $bbggrr       R   G   B      H    S     V
    //-------------------     -------      --- --- ---  ----- ----- -----
    ColorBlack              = $000000;  //   0   0   0     -  0.000 0.000
    ColorGrey11             = $1C1C1C;  //  28  28  28     -  0.000 0.110
    ColorGrey21             = $363636;  //  54  54  54     -  0.000 0.212
    ColorGrey31             = $4F4F4F;  //  79  79  79     -  0.000 0.310
    ColorDimGrey            = $696969;  // 105 105 105     -  0.000 0.412 * Duplicate
    ColorGrey41             = $696969;  // 105 105 105     -  0.000 0.412
    ColorGrey51             = $828282;  // 130 130 130     -  0.000 0.510
    ColorGrey61             = $9C9C9C;  // 156 156 156     -  0.000 0.612
    ColorDarkGrey           = $A9A9A9;  // 169 169 169     -  0.000 0.663
    ColorGrey71             = $B5B5B5;  // 181 181 181     -  0.000 0.710
    ColorGrey               = $BEBEBE;  // 190 190 190     -  0.000 0.745
    ColorGray81             = $CFCFCF;  // 207 207 207     -  0.000 0.812
    ColorLightGray          = $D3D3D3;  // 211 211 211     -  0.000 0.827
    ColorGainsboro          = $DCDCDC;  // 220 220 220     -  0.000 0.863
    ColorGray91             = $E8E8E8;  // 232 232 232     -  0.000 0.910
    ColorWhiteSmoke         = $F5F5F5;  // 245 245 245     -  0.000 0.961
    ColorWhite              = $FFFFFF;  // 255 255 255     -  0.000 1.000
    ColorSnow4              = $89898B;  // 139 137 137    0.0 0.014 0.545
    ColorSnow3              = $C9C9CD;  // 205 201 201    0.0 0.020 0.804
    ColorSnow               = $FAFAFF;  // 255 250 250    0.0 0.020 1.000
    ColorSnow1              = $FAFAFF;  // 255 250 250    0.0 0.020 1.000 *
    ColorSnow2              = $E9E9EE;  // 238 233 233    0.0 0.021 0.933
    ColorRosyBrown          = $8F8FBC;  // 188 143 143    0.0 0.239 0.737
    ColorRosyBrown1         = $C1C1FF;  // 255 193 193    0.0 0.243 1.000
    ColorRosyBrown3         = $9B9BCD;  // 205 155 155    0.0 0.244 0.804
    ColorRosyBrown2         = $B4B4EE;  // 238 180 180    0.0 0.244 0.933
    ColorRosyBrown4         = $69698B;  // 139 105 105    0.0 0.245 0.545
    ColorLightCoral         = $8080F0;  // 240 128 128    0.0 0.467 0.941
    ColorIndianRed          = $5C5CCD;  // 205  92  92    0.0 0.551 0.804
    ColorIndianRed4         = $3A3A8B;  // 139  58  58    0.0 0.583 0.545
    ColorIndianRed2         = $6363EE;  // 238  99  99    0.0 0.584 0.933
    ColorIndianRed1         = $6A6AFF;  // 255 106 106    0.0 0.584 1.000
    ColorIndianRed3         = $5555CD;  // 205  85  85    0.0 0.585 0.804
    ColorBrown              = $2A2AA5;  // 165  42  42    0.0 0.745 0.647
    ColorBrown4             = $23238B;  // 139  35  35    0.0 0.748 0.545
    ColorBrown1             = $4040FF;  // 255  64  64    0.0 0.749 1.000
    ColorBrown3             = $3333CD;  // 205  51  51    0.0 0.751 0.804
    ColorBrown2             = $3B3BEE;  // 238  59  59    0.0 0.752 0.933
    ColorFirebrick          = $2222B2;  // 178  34  34    0.0 0.809 0.698
    ColorFirebrick1         = $3030FF;  // 255  48  48    0.0 0.812 1.000
    ColorFirebrick4         = $1A1A8B;  // 139  26  26    0.0 0.813 0.545
    ColorFirebrick3         = $2626CD;  // 205  38  38    0.0 0.815 0.804
    ColorFirebrick2         = $2C2CEE;  // 238  44  44    0.0 0.815 0.933
    ColorRed4               = $00008B;  // 139   0   0    0.0 1.000 0.545
    ColorDarkRed            = $00008B;  // 139   0   0    0.0 1.000 0.545 *
    ColorRed3               = $0000CD;  // 205   0   0    0.0 1.000 0.804
    ColorRed2               = $0000EE;  // 238   0   0    0.0 1.000 0.933
    ColorRed                = $0000FF;  // 255   0   0    0.0 1.000 1.000
    ColorRed1               = $0000FF;  // 255   0   0    0.0 1.000 1.000 *
    ColorMistyRose3         = $B5B7CD;  // 205 183 181    5.0 0.117 0.804
    ColorMistyRose          = $E1E4FF;  // 255 228 225    6.0 0.118 1.000
    ColorMistyRose1         = $E1E4FF;  // 255 228 225    6.0 0.118 1.000 *
    ColorSalmon             = $7280FA;  // 250 128 114    6.2 0.544 0.980
    ColorMistyRose2         = $D2D5EE;  // 238 213 210    6.4 0.118 0.933
    ColorMistyRose4         = $7B7D8B;  // 139 125 123    7.5 0.115 0.545
    ColorTomato3            = $394FCD;  // 205  79  57    8.9 0.722 0.804
    ColorTomato             = $4763FF;  // 255  99  71    9.1 0.722 1.000
    ColorTomato1            = $4763FF;  // 255  99  71    9.1 0.722 1.000
    ColorTomato2            = $425CEE;  // 238  92  66    9.1 0.723 0.933
    ColorTomato4            = $26368B;  // 139  54  38    9.5 0.727 0.545
    ColorCoral3             = $455BCD;  // 205  91  69    9.7 0.663 0.804
    ColorCoral4             = $2F3E8B;  // 139  62  47    9.8 0.662 0.545
    ColorCoral1             = $5672FF;  // 255 114  86    9.9 0.663 1.000
    ColorCoral2             = $506AEE;  // 238 106  80    9.9 0.664 0.933
    ColorSalmon2            = $6282EE;  // 238 130  98   13.7 0.588 0.933
    ColorSalmon4            = $394C8B;  // 139  76  57   13.9 0.590 0.545
    ColorSalmon3            = $5470CD;  // 205 112  84   13.9 0.590 0.804
    ColorSalmon1            = $698CFF;  // 255 140 105   14.0 0.588 1.000
    ColorDarkSalmon         = $7A96E9;  // 233 150 122   15.1 0.476 0.914
    ColorOrangeRed4         = $00258B;  // 139  37   0   16.0 1.000 0.545
    ColorCoral              = $507FFF;  // 255 127  80   16.1 0.686 1.000
    ColorOrangeRed3         = $0037CD;  // 205  55   0   16.1 1.000 0.804
    ColorOrangeRed2         = $0040EE;  // 238  64   0   16.1 1.000 0.933
    ColorOrangeRed          = $0045FF;  // 255  69   0   16.2 1.000 1.000
    ColorOrangeRed1         = $0045FF;  // 255  69   0   16.2 1.000 1.000 *
    ColorLightSalmon2       = $7295EE;  // 238 149 114   16.9 0.521 0.933
    ColorLightSalmon        = $7AA0FF;  // 255 160 122   17.1 0.522 1.000
    ColorLightSalmon1       = $7AA0FF;  // 255 160 122   17.1 0.522 1.000 *
    ColorLightSalmon4       = $42578B;  // 139  87  66   17.3 0.525 0.545
    ColorLightSalmon3       = $6281CD;  // 205 129  98   17.4 0.522 0.804
    ColorSienna3            = $3968CD;  // 205 104  57   19.1 0.722 0.804
    ColorSienna1            = $4782FF;  // 255 130  71   19.2 0.722 1.000
    ColorSienna2            = $4279EE;  // 238 121  66   19.2 0.723 0.933
    ColorSienna             = $2D52A0;  // 160  82  45   19.3 0.719 0.627
    ColorSienna4            = $26478B;  // 139  71  38   19.6 0.727 0.545
    ColorSeashell           = $EEF5FF;  // 255 245 238   24.7 0.067 1.000
    ColorSeashell1          = $EEF5FF;  // 255 245 238   24.7 0.067 1.000 *
    ColorChocolate3         = $1D66CD;  // 205 102  29   24.9 0.859 0.804
    ColorChocolate1         = $247FFF;  // 255 127  36   24.9 0.859 1.000
    ColorChocolate2         = $2176EE;  // 238 118  33   24.9 0.861 0.933
    ColorChocolate          = $1E69D2;  // 210 105  30   25.0 0.857 0.824
    ColorSaddleBrown        = $13458B;  // 139  69  19   25.0 0.863 0.545
    ColorChocolate4         = $13458B;  // 139  69  19   25.0 0.863 0.545 *
    ColorSeashell3          = $BFC5CD;  // 205 197 191   25.7 0.068 0.804
    ColorSeashell2          = $DEE5EE;  // 238 229 222   26.3 0.067 0.933
    ColorSeashell4          = $82868B;  // 139 134 130   26.7 0.065 0.545
    ColorSandyBrown         = $60A4F4;  // 244 164  96   27.6 0.607 0.957
    ColorPeachPuff2         = $ADCBEE;  // 238 203 173   27.7 0.273 0.933
    ColorPeachPuff3         = $95AFCD;  // 205 175 149   27.9 0.273 0.804
    ColorPeachPuff          = $B9DAFF;  // 255 218 185   28.3 0.275 1.000
    ColorPeachPuff1         = $B9DAFF;  // 255 218 185   28.3 0.275 1.000 *
    ColorPeachPuff4         = $65778B;  // 139 119 101   28.4 0.273 0.545
    ColorTan1               = $4FA5FF;  // 255 165  79   29.3 0.690 1.000
    ColorTan4               = $2B5A8B;  // 139  90  43   29.4 0.691 0.545
    ColorTan2               = $499AEE;  // 238 154  73   29.5 0.693 0.933
    ColorPeru               = $3F85CD;  // 205 133  63   29.6 0.693 0.804
    ColorTan3               = $3F85CD;  // 205 133  63   29.6 0.693 0.804 *
    ColorDarkOrange2        = $0076EE;  // 238 118   0   29.7 1.000 0.933
    ColorDarkOrange4        = $00458B;  // 139  69   0   29.8 1.000 0.545
    ColorDarkOrange3        = $0066CD;  // 205 102   0   29.9 1.000 0.804
    ColorDarkOrange1        = $007FFF;  // 255 127   0   29.9 1.000 1.000
    ColorLinen              = $E6F0FA;  // 250 240 230   30.0 0.080 0.980
    ColorBisque3            = $9EB7CD;  // 205 183 158   31.9 0.229 0.804
    ColorBisque             = $C4E4FF;  // 255 228 196   32.5 0.231 1.000
    ColorBisque1            = $C4E4FF;  // 255 228 196   32.5 0.231 1.000 *
    ColorBisque2            = $B7D5EE;  // 238 213 183   32.7 0.231 0.933
    ColorDarkOrange         = $008CFF;  // 255 140   0   32.9 1.000 1.000
    ColorAntiqueWhite3      = $B0C0CD;  // 205 192 176   33.1 0.141 0.804
    ColorAntiqueWhite1      = $DBEFFF;  // 255 239 219   33.3 0.141 1.000
    ColorBurlywood4         = $55738B;  // 139 115  85   33.3 0.388 0.545
    ColorAntiqueWhite2      = $CCDFEE;  // 238 223 204   33.5 0.143 0.933
    ColorBurlywood2         = $91C5EE;  // 238 197 145   33.5 0.391 0.933
    ColorBurlywood1         = $9BD3FF;  // 255 211 155   33.6 0.392 1.000
    ColorBisque4            = $6B7D8B;  // 139 125 107   33.7 0.230 0.545
    ColorBurlywood3         = $7DAACD;  // 205 170 125   33.8 0.390 0.804
    ColorBurlywood          = $87B8DE;  // 222 184 135   33.8 0.392 0.871
    ColorAntiqueWhite       = $D7EBFA;  // 250 235 215   34.3 0.140 0.980
    ColorTan                = $8CB4D2;  // 210 180 140   34.3 0.333 0.824
    ColorAntiqueWhite4      = $78838B;  // 139 131 120   34.7 0.137 0.545
    ColorNavajoWhite2       = $A1CFEE;  // 238 207 161   35.8 0.324 0.933
    ColorNavajoWhite        = $ADDEFF;  // 255 222 173   35.9 0.322 1.000
    ColorNavajoWhite1       = $ADDEFF;  // 255 222 173   35.9 0.322 1.000
    ColorBlanchedAlmond     = $CDEBFF;  // 255 235 205   36.0 0.196 1.000
    ColorNavajoWhite4       = $5E798B;  // 139 121  94   36.0 0.324 0.545
    ColorNavajoWhite3       = $8BB3CD;  // 205 179 139   36.4 0.322 0.804
    ColorPapayaWhip         = $D5EFFF;  // 255 239 213   37.1 0.165 1.000
    ColorMoccasin           = $B5E4FF;  // 255 228 181   38.1 0.290 1.000
    ColorOrange4            = $005A8B;  // 139  90   0   38.8 1.000 0.545
    ColorOrange2            = $009AEE;  // 238 154   0   38.8 1.000 0.933
    ColorOrange             = $00A5FF;  // 255 165   0   38.8 1.000 1.000 *
    ColorOrange1            = $00A5FF;  // 255 165   0   38.8 1.000 1.000
    ColorWheat4             = $667E8B;  // 139 126 102   38.9 0.266 0.545
    ColorOrange3            = $0085CD;  // 205 133   0   38.9 1.000 0.804
    ColorOldLace            = $E6F5FD;  // 253 245 230   39.1 0.091 0.992
    ColorWheat              = $B3DEF5;  // 245 222 179   39.1 0.269 0.961
    ColorWheat1             = $BAE7FF;  // 255 231 186   39.1 0.271 1.000
    ColorWheat3             = $96BACD;  // 205 186 150   39.3 0.268 0.804
    ColorWheat2             = $AED8EE;  // 238 216 174   39.4 0.269 0.933
    ColorFloralWhite        = $F0FAFF;  // 255 250 240   40.0 0.059 1.000
    ColorDarkGoldenrod1     = $0FB9FF;  // 255 185  15   42.5 0.941 1.000
    ColorDarkGoldenrod3     = $0C95CD;  // 205 149  12   42.6 0.941 0.804
    ColorDarkGoldenrod2     = $0EADEE;  // 238 173  14   42.6 0.941 0.933
    ColorDarkGoldenrod      = $0B86B8;  // 184 134  11   42.7 0.940 0.722
    ColorGoldenrod          = $20A5DA;  // 218 165  32   42.9 0.853 0.855
    ColorGoldenrod1         = $25C1FF;  // 255 193  37   42.9 0.855 1.000
    ColorGoldenrod4         = $14698B;  // 139 105  20   42.9 0.856 0.545
    ColorGoldenrod2         = $22B4EE;  // 238 180  34   42.9 0.857 0.933
    ColorGoldenrod3         = $1D9BCD;  // 205 155  29   43.0 0.859 0.804
    ColorCornsilk           = $DCF8FF;  // 255 248 220   48.0 0.137 1.000
    ColorCornsilk1          = $DCF8FF;  // 255 248 220   48.0 0.137 1.000 *
    ColorCornsilk2          = $CDE8EE;  // 238 232 205   49.1 0.139 0.933
    ColorCornsilk3          = $B1C8CD;  // 205 200 177   49.3 0.137 0.804
    ColorLightGoldenrod2    = $82DCEE;  // 238 220 130   50.0 0.454 0.933
    ColorLightGoldenrod1    = $8BECFF;  // 255 236 139   50.2 0.455 1.000
    ColorLightGoldenrod3    = $70BECD;  // 205 190 112   50.3 0.454 0.804
    ColorCornsilk4          = $78888B;  // 139 136 120   50.5 0.137 0.545
    ColorLightGoldenrod4    = $4C818B;  // 139 129  76   50.5 0.453 0.545
    ColorGold4              = $00758B;  // 139 117   0   50.5 1.000 0.545
    ColorLightGoldenrod     = $82DDEE;  // 238 221 130   50.6 0.454 0.933
    ColorGold3              = $00ADCD;  // 205 173   0   50.6 1.000 0.804
    ColorGold               = $00D7FF;  // 255 215   0   50.6 1.000 1.000
    ColorGold1              = $00D7FF;  // 255 215   0   50.6 1.000 1.000 *
    ColorGold2              = $00C9EE;  // 238 201   0   50.7 1.000 0.933
    ColorLemonChiffon2      = $BFE9EE;  // 238 233 191   53.6 0.197 0.933
    ColorLemonChiffon3      = $A5C9CD;  // 205 201 165   54.0 0.195 0.804
    ColorLemonChiffon       = $CDFAFF;  // 255 250 205   54.0 0.196 1.000
    ColorLemonChiffon1      = $CDFAFF;  // 255 250 205   54.0 0.196 1.000 *
    ColorPaleGoldenrod      = $AAE8EE;  // 238 232 170   54.7 0.286 0.933
    ColorKhaki4             = $4E868B;  // 139 134  78   55.1 0.439 0.545
    ColorKhaki1             = $8FF6FF;  // 255 246 143   55.2 0.439 1.000
    ColorKhaki3             = $73C6CD;  // 205 198 115   55.3 0.439 0.804
    ColorKhaki2             = $85E6EE;  // 238 230 133   55.4 0.441 0.933
    ColorLemonChiffon4      = $70898B;  // 139 137 112   55.6 0.194 0.545
    ColorDarkKhaki          = $6BB7BD;  // 189 183 107   55.6 0.434 0.741
    ColorIvory4             = $838B8B;  // 139 139 131   60.0 0.058 0.545
    ColorIvory3             = $C1CDCD;  // 205 205 193   60.0 0.059 0.804
    ColorIvory2             = $E0EEEE;  // 238 238 224   60.0 0.059 0.933
    ColorIvory              = $F0FFFF;  // 255 255 240   60.0 0.059 1.000
    ColorIvory1             = $F0FFFF;  // 255 255 240   60.0 0.059 1.000 *
    ColorBeige              = $DCF5F5;  // 245 245 220   60.0 0.102 0.961
    ColorLightYellow4       = $7A8B8B;  // 139 139 122   60.0 0.122 0.545
    ColorLightYellow3       = $B4CDCD;  // 205 205 180   60.0 0.122 0.804
    ColorLightYellow2       = $D1EEEE;  // 238 238 209   60.0 0.122 0.933
    ColorLightYellow        = $E0FFFF;  // 255 255 224   60.0 0.122 1.000
    ColorLightYellow1       = $E0FFFF;  // 255 255 224   60.0 0.122 1.000 *
    ColorLtGoldenrodYello   = $D2FAFA;  // 250 250 210   60.0 0.160 0.980
    ColorYellow4            = $008B8B;  // 139 139   0   60.0 1.000 0.545
    ColorYellow3            = $00CDCD;  // 205 205   0   60.0 1.000 0.804
    ColorYellow2            = $00EEEE;  // 238 238   0   60.0 1.000 0.933
    ColorYellow             = $00FFFF;  // 255 255   0   60.0 1.000 1.000
    ColorYellow1            = $00FFFF;  // 255 255   0   60.0 1.000 1.000 *
    ColorOliveDrab4         = $228B69;  // 105 139  34   79.4 0.755 0.545
    ColorOliveDrab          = $238E6B;  // 107 142  35   79.6 0.754 0.557
    ColorOliveDrab1         = $3EFFC0;  // 192 255  62   79.6 0.757 1.000
    ColorYellowGreen        = $32CD9A;  // 154 205  50   79.7 0.756 0.804
    ColorOliveDrab3         = $32CD9A;  // 154 205  50   79.7 0.756 0.804 *
    ColorOliveDrab2         = $3AEEB3;  // 179 238  58   79.7 0.756 0.933
    ColorDarkOliveGreen     = $2F6B55;  //  85 107  47   82.0 0.561 0.420
    ColorDarkOliveGreen1    = $70FFCA;  // 202 255 112   82.2 0.561 1.000
    ColorDarkOliveGreen4    = $3D8B6E;  // 110 139  61   82.3 0.561 0.545
    ColorDarkOliveGreen3    = $5ACDA2;  // 162 205  90   82.4 0.561 0.804
    ColorDarkOliveGreen2    = $68EEBC;  // 188 238 104   82.4 0.563 0.933
    ColorGreenYellow        = $2FFFAD;  // 173 255  47   83.7 0.816 1.000
    ColorChartreuse3        = $00CD66;  // 102 205   0   90.1 1.000 0.804
    ColorChartreuse         = $00FF7F;  // 127 255   0   90.1 1.000 1.000
    ColorChartreuse1        = $00FF7F;  // 127 255   0   90.1 1.000 1.000 *
    ColorChartreuse4        = $008B45;  //  69 139   0   90.2 1.000 0.545
    ColorChartreuse2        = $00EE76;  // 118 238   0   90.3 1.000 0.933
    ColorLawnGreen          = $00FC7C;  // 124 252   0   90.5 1.000 0.988
    ColorHoneydew4          = $838B83;  // 131 139 131  120.0 0.058 0.545
    ColorHoneydew3          = $C1CDC1;  // 193 205 193  120.0 0.059 0.804
    ColorHoneydew2          = $E0EEE0;  // 224 238 224  120.0 0.059 0.933
    ColorHoneydew           = $F0FFF0;  // 240 255 240  120.0 0.059 1.000
    ColorHoneydew1          = $F0FFF0;  // 240 255 240  120.0 0.059 1.000 *
    ColorDarkSeaGreen       = $8FBC8F;  // 143 188 143  120.0 0.239 0.737
    ColorDarkSeaGreen1      = $C1FFC1;  // 193 255 193  120.0 0.243 1.000
    ColorDarkSeaGreen3      = $9BCD9B;  // 155 205 155  120.0 0.244 0.804
    ColorDarkSeaGreen2      = $B4EEB4;  // 180 238 180  120.0 0.244 0.933
    ColorDarkSeaGreen4      = $698B69;  // 105 139 105  120.0 0.245 0.545
    ColorPaleGreen          = $98FB98;  // 152 251 152  120.0 0.394 0.984
    ColorPaleGreen3         = $7CCD7C;  // 124 205 124  120.0 0.395 0.804
    ColorPaleGreen2         = $90EE90;  // 144 238 144  120.0 0.395 0.933
    ColorLightGreen         = $90EE90;  // 144 238 144  120.0 0.395 0.933 *
    ColorPaleGreen4         = $548B54;  //  84 139  84  120.0 0.396 0.545
    ColorPaleGreen1         = $9AFF9A;  // 154 255 154  120.0 0.396 1.000
    ColorForestGreen        = $228B22;  //  34 139  34  120.0 0.755 0.545
    ColorLimeGreen          = $32CD32;  //  50 205  50  120.0 0.756 0.804
    ColorDarkGreen          = $006400;  //   0 100   0  120.0 1.000 0.392
    ColorGreen4             = $008B00;  //   0 139   0  120.0 1.000 0.545
    ColorGreen3             = $00CD00;  //   0 205   0  120.0 1.000 0.804
    ColorGreen2             = $00EE00;  //   0 238   0  120.0 1.000 0.933
    ColorGreen              = $00FF00;  //   0 255   0  120.0 1.000 1.000
    ColorGreen1             = $00FF00;  //   0 255   0  120.0 1.000 1.000 *
    ColorSeaGreen1          = $9FFF54;  //  84 255 159  146.3 0.671 1.000
    ColorSeaGreen2          = $94EE4E;  //  78 238 148  146.3 0.672 0.933
    ColorSeaGreen           = $578B2E;  //  46 139  87  146.5 0.669 0.545
    ColorSeaGreen4          = $578B2E;  //  46 139  87  146.5 0.669 0.545 *
    ColorSeaGreen3          = $80CD43;  //  67 205 128  146.5 0.673 0.804
    ColorMediumSeaGreen     = $71B33C;  //  60 179 113  146.7 0.665 0.702
    ColorSpringGreen2       = $76EE00;  //   0 238 118  149.7 1.000 0.933
    ColorSpringGreen4       = $458B00;  //   0 139  69  149.8 1.000 0.545
    ColorSpringGreen3       = $66CD00;  //   0 205 102  149.9 1.000 0.804
    ColorSpringGreen        = $7FFF00;  //   0 255 127  149.9 1.000 1.000
    ColorSpringGreen1       = $7FFF00;  //   0 255 127  149.9 1.000 1.000 *
    ColorMintCream          = $FAFFF5;  // 245 255 250  150.0 0.039 1.000
    ColorMedSpringGreen     = $9AFA00;  //   0 250 154  157.0 1.000 0.980
    ColorMediumAquamarine   = $AACD66;  // 102 205 170  159.6 0.502 0.804
    ColorAquamarine3        = $AACD66;  // 102 205 170  159.6 0.502 0.804
    ColorAquamarine         = $D4FF7F;  // 127 255 212  159.8 0.502 1.000
    ColorAquamarine1        = $D4FF7F;  // 127 255 212  159.8 0.502 1.000 *
    ColorAquamarine2        = $C6EE76;  // 118 238 198  160.0 0.504 0.933
    ColorAquamarine4        = $748B45;  //  69 139 116  160.3 0.504 0.545
    ColorTurquoise          = $D0E040;  //  64 224 208  174.0 0.714 0.878
    ColorLightSeaGreen      = $AAB220;  //  32 178 170  176.7 0.820 0.698
    ColorMediumTurquoise    = $CCD148;  //  72 209 204  177.8 0.656 0.820
    ColorAzure4             = $8B8B83;  // 131 139 139  180.0 0.058 0.545
    ColorAzure3             = $CDCDC1;  // 193 205 205  180.0 0.059 0.804
    ColorAzure2             = $EEEEE0;  // 224 238 238  180.0 0.059 0.933
    ColorAzure              = $FFFFF0;  // 240 255 255  180.0 0.059 1.000
    ColorAzure1             = $FFFFF0;  // 240 255 255  180.0 0.059 1.000 *
    ColorLightCyan4         = $8B8B7A;  // 122 139 139  180.0 0.122 0.545
    ColorLightCyan3         = $CDCDB4;  // 180 205 205  180.0 0.122 0.804
    ColorLightCyan2         = $EEEED1;  // 209 238 238  180.0 0.122 0.933
    ColorLightCyan          = $FFFFE0;  // 224 255 255  180.0 0.122 1.000
    ColorLightCyan1         = $FFFFE0;  // 224 255 255  180.0 0.122 1.000 *
    ColorPaleTurquoise      = $EEEEAF;  // 175 238 238  180.0 0.265 0.933
    ColorPaleTurquoise4     = $8B8B66;  // 102 139 139  180.0 0.266 0.545
    ColorPaleTurquoise1     = $FFFFBB;  // 187 255 255  180.0 0.267 1.000
    ColorPaleTurquoise3     = $CDCD96;  // 150 205 205  180.0 0.268 0.804
    ColorPaleTurquoise2     = $EEEEAE;  // 174 238 238  180.0 0.269 0.933
    ColorDarkSlateGray      = $4F4F2F;  //  47  79  79  180.0 0.405 0.310
    ColorDarkSlateGray2     = $EEEE8D;  // 141 238 238  180.0 0.408 0.933
    ColorDarkSlateGray1     = $FFFF97;  // 151 255 255  180.0 0.408 1.000
    ColorDarkSlateGray4     = $8B8B52;  //  82 139 139  180.0 0.410 0.545
    ColorDarkSlateGray3     = $CDCD79;  // 121 205 205  180.0 0.410 0.804
    ColorCyan4              = $8B8B00;  //   0 139 139  180.0 1.000 0.545
    ColorDarkCyan           = $8B8B00;  //   0 139 139  180.0 1.000 0.545 *
    ColorCyan3              = $CDCD00;  //   0 205 205  180.0 1.000 0.804
    ColorCyan2              = $EEEE00;  //   0 238 238  180.0 1.000 0.933
    ColorCyan               = $FFFF00;  //   0 255 255  180.0 1.000 1.000
    ColorCyan1              = $FFFF00;  //   0 255 255  180.0 1.000 1.000 *
    ColorDarkTurquoise      = $D1CE00;  //   0 206 209  180.9 1.000 0.820
    ColorCadetBlue          = $A09E5F;  //  95 158 160  181.8 0.406 0.627
    ColorTurquoise4         = $8B8600;  //   0 134 139  182.2 1.000 0.545
    ColorTurquoise3         = $CDC500;  //   0 197 205  182.3 1.000 0.804
    ColorTurquoise2         = $EEE500;  //   0 229 238  182.3 1.000 0.933
    ColorTurquoise1         = $FFF500;  //   0 245 255  182.4 1.000 1.000
    ColorCadetBlue4         = $8B8653;  //  83 134 139  185.4 0.403 0.545
    ColorCadetBlue2         = $EEE58E;  // 142 229 238  185.6 0.403 0.933
    ColorCadetBlue1         = $FFF598;  // 152 245 255  185.8 0.404 1.000
    ColorCadetBlue3         = $CDC57A;  // 122 197 205  185.8 0.405 0.804
    ColorPowderBlue         = $E6E0B0;  // 176 224 230  186.7 0.235 0.902
    ColorLightBlue4         = $8B8368;  // 104 131 139  193.7 0.252 0.545
    ColorLightBlue          = $E6D8AD;  // 173 216 230  194.7 0.248 0.902
    ColorDeepSkyBlue3       = $CD9A00;  //   0 154 205  194.9 1.000 0.804
    ColorLightBlue1         = $FFEFBF;  // 191 239 255  195.0 0.251 1.000
    ColorLightBlue2         = $EEDFB2;  // 178 223 238  195.0 0.252 0.933
    ColorDeepSkyBlue4       = $8B6800;  //   0 104 139  195.1 1.000 0.545
    ColorDeepSkyBlue2       = $EEB200;  //   0 178 238  195.1 1.000 0.933
    ColorDeepSkyBlue        = $FFBF00;  //   0 191 255  195.1 1.000 1.000
    ColorDeepSkyBlue1       = $FFBF00;  //   0 191 255  195.1 1.000 1.000 *
    ColorLightBlue3         = $CDC09A;  // 154 192 205  195.3 0.249 0.804
    ColorSkyBlue            = $EBCE87;  // 135 206 235  197.4 0.426 0.922
    ColorLightSkyBlue3      = $CDB68D;  // 141 182 205  201.6 0.312 0.804
    ColorLightSkyBlue2      = $EED3A4;  // 164 211 238  201.9 0.311 0.933
    ColorLightSkyBlue1      = $FFE2B0;  // 176 226 255  202.0 0.310 1.000
    ColorLightSkyBlue4      = $8B7B60;  //  96 123 139  202.3 0.309 0.545
    ColorLightSkyBlue       = $FACE87;  // 135 206 250  203.0 0.460 0.980
    ColorSkyBlue3           = $CDA66C;  // 108 166 205  204.1 0.473 0.804
    ColorSkyBlue1           = $FFCE87;  // 135 206 255  204.5 0.471 1.000
    ColorSkyBlue2           = $EEC07E;  // 126 192 238  204.6 0.471 0.933
    ColorSkyBlue4           = $8B704A;  //  74 112 139  204.9 0.468 0.545
    ColorSteelBlue2         = $EEAC5C;  //  92 172 238  207.1 0.613 0.933
    ColorSteelBlue3         = $CD944F;  //  79 148 205  207.1 0.615 0.804
    ColorSteelBlue          = $B48246;  //  70 130 180  207.3 0.611 0.706
    ColorSteelBlue1         = $FFB863;  //  99 184 255  207.3 0.612 1.000
    ColorSteelBlue4         = $8B6436;  //  54 100 139  207.5 0.612 0.545
    ColorAliceBlue          = $FFF8F0;  // 240 248 255  208.0 0.059 1.000
    ColorDodgerBlue3        = $CD7418;  //  24 116 205  209.5 0.883 0.804
    ColorDodgerBlue         = $FF901E;  //  30 144 255  209.6 0.882 1.000
    ColorDodgerBlue1        = $FF901E;  //  30 144 255  209.6 0.882 1.000 *
    ColorDodgerBlue2        = $EE861C;  //  28 134 238  209.7 0.882 0.933
    ColorDodgerBlue4        = $8B4E10;  //  16  78 139  209.8 0.885 0.545
    ColorSlateGrey          = $908070;  // 112 128 144  210.0 0.222 0.565
    ColorLightSlateGray     = $998877;  // 119 136 153  210.0 0.222 0.600
    ColorSlateGray3         = $CDB69F;  // 159 182 205  210.0 0.224 0.804
    ColorSlateGray1         = $FFE2C6;  // 198 226 255  210.5 0.224 1.000
    ColorSlateGray2         = $EED3B9;  // 185 211 238  210.6 0.223 0.933
    ColorSlateGray4         = $8B7B6C;  // 108 123 139  211.0 0.223 0.545
    ColorLightSteelBlue4    = $8B7B6E;  // 110 123 139  213.1 0.209 0.545
    ColorLightSteelBlue3    = $CDB5A2;  // 162 181 205  213.5 0.210 0.804
    ColorLightSteelBlue2    = $EED2BC;  // 188 210 238  213.6 0.210 0.933
    ColorLightSteelBlue     = $DEC4B0;  // 176 196 222  213.9 0.207 0.871
    ColorLightSteelBlue1    = $FFE1CA;  // 202 225 255  214.0 0.208 1.000
    ColorCornflowerBlue     = $ED9564;  // 100 149 237  218.5 0.578 0.929
    ColorRoyalBlue3         = $CD5F3A;  //  58  95 205  224.9 0.717 0.804
    ColorRoyalBlue2         = $EE6E43;  //  67 110 238  224.9 0.718 0.933
    ColorRoyalBlue1         = $FF7648;  //  72 118 255  224.9 0.718 1.000
    ColorRoyalBlue          = $E16941;  //  65 105 225  225.0 0.711 0.882
    ColorRoyalBlue4         = $8B4027;  //  39  64 139  225.0 0.719 0.545
    ColorGhostWhite         = $FFF8F8;  // 248 248 255  240.0 0.027 1.000
    ColorLavender           = $FAE6E6;  // 230 230 250  240.0 0.080 0.980
    ColorMidnightBlue       = $701919;  //  25  25 112  240.0 0.777 0.439
    ColorNavyBlue           = $800000;  //   0   0 128  240.0 1.000 0.502
    ColorBlue4              = $8B0000;  //   0   0 139  240.0 1.000 0.545
    ColorDarkBlue           = $8B0000;  //   0   0 139  240.0 1.000 0.545 *
    ColorMediumBlue         = $CD0000;  //   0   0 205  240.0 1.000 0.804
    ColorBlue3              = $CD0000;  //   0   0 205  240.0 1.000 0.804 *
    ColorBlue2              = $EE0000;  //   0   0 238  240.0 1.000 0.933
    ColorBlue               = $FF0000;  //   0   0 255  240.0 1.000 1.000
    ColorBlue1              = $FF0000;  //   0   0 255  240.0 1.000 1.000 *
    ColorSlateBlue          = $CD5A6A;  // 106  90 205  248.3 0.561 0.804
    ColorSlateBlue1         = $FF6F83;  // 131 111 255  248.3 0.565 1.000
    ColorSlateBlue3         = $CD5969;  // 105  89 205  248.3 0.566 0.804
    ColorLightSlateBlue     = $FF7084;  // 132 112 255  248.4 0.561 1.000
    ColorSlateBlue2         = $EE677A;  // 122 103 238  248.4 0.567 0.933
    ColorSlateBlue4         = $8B3C47;  //  71  60 139  248.4 0.568 0.545
    ColorDarkSlateBlue      = $8B3D48;  //  72  61 139  248.5 0.561 0.545
    ColorMediumSlateBlue    = $EE687B;  // 123 104 238  248.5 0.563 0.933
    ColorMediumPurple4      = $8B475D;  //  93  71 139  259.4 0.489 0.545
    ColorMediumPurple2      = $EE799F;  // 159 121 238  259.5 0.492 0.933
    ColorMediumPurple       = $DB7093;  // 147 112 219  259.6 0.489 0.859
    ColorMediumPurple3      = $CD6889;  // 137 104 205  259.6 0.493 0.804
    ColorMediumPurple1      = $FF82AB;  // 171 130 255  259.7 0.490 1.000
    ColorPurple1            = $FF309B;  // 155  48 255  271.0 0.812 1.000
    ColorBlueViolet         = $E22B8A;  // 138  43 226  271.1 0.810 0.886
    ColorPurple2            = $EE2C91;  // 145  44 238  271.2 0.815 0.933
    ColorPurple4            = $8B1A55;  //  85  26 139  271.3 0.813 0.545
    ColorPurple3            = $CD267D;  // 125  38 205  271.3 0.815 0.804
    ColorPurple             = $F020A0;  // 160  32 240  276.9 0.867 0.941
    ColorDarkOrchid4        = $8B2268;  // 104  34 139  280.0 0.755 0.545
    ColorDarkOrchid2        = $EE3AB2;  // 178  58 238  280.0 0.756 0.933
    ColorDarkOrchid         = $CC3299;  // 153  50 204  280.1 0.755 0.800
    ColorDarkOrchid1        = $FF3EBF;  // 191  62 255  280.1 0.757 1.000
    ColorDarkOrchid3        = $CD329A;  // 154  50 205  280.3 0.756 0.804
    ColorDarkViolet         = $D30094;  // 148   0 211  282.1 1.000 0.827
    ColorMediumOrchid3      = $CD52B4;  // 180  82 205  287.8 0.600 0.804
    ColorMediumOrchid1      = $FF66E0;  // 224 102 255  287.8 0.600 1.000
    ColorMediumOrchid2      = $EE5FD1;  // 209  95 238  287.8 0.601 0.933
    ColorMediumOrchid4      = $8B377A;  // 122  55 139  287.9 0.604 0.545
    ColorMediumOrchid       = $D355BA;  // 186  85 211  288.1 0.597 0.827
    ColorThistle4           = $8B7B8B;  // 139 123 139  300.0 0.115 0.545
    ColorThistle            = $D8BFD8;  // 216 191 216  300.0 0.116 0.847
    ColorThistle3           = $CDB5CD;  // 205 181 205  300.0 0.117 0.804
    ColorThistle2           = $EED2EE;  // 238 210 238  300.0 0.118 0.933
    ColorThistle1           = $FFE1FF;  // 255 225 255  300.0 0.118 1.000
    ColorPlum4              = $8B668B;  // 139 102 139  300.0 0.266 0.545
    ColorPlum1              = $FFBBFF;  // 255 187 255  300.0 0.267 1.000
    ColorPlum3              = $CD96CD;  // 205 150 205  300.0 0.268 0.804
    ColorPlum2              = $EEAEEE;  // 238 174 238  300.0 0.269 0.933
    ColorDarkGoldenrod4     = $8B658B;  // 139 101 139  300.0 0.273 0.545
    ColorPlum               = $DDA0DD;  // 221 160 221  300.0 0.276 0.867
    ColorViolet             = $EE82EE;  // 238 130 238  300.0 0.454 0.933
    ColorMagenta4           = $8B008B;  // 139   0 139  300.0 1.000 0.545
    ColorDarkMagenta        = $8B008B;  // 139   0 139  300.0 1.000 0.545 *
    ColorMagenta3           = $CD00CD;  // 205   0 205  300.0 1.000 0.804
    ColorMagenta2           = $EE00EE;  // 238   0 238  300.0 1.000 0.933
    ColorMagenta            = $FF00FF;  // 255   0 255  300.0 1.000 1.000
    ColorMagenta1           = $FF00FF;  // 255   0 255  300.0 1.000 1.000 *
    ColorOrchid4            = $89478B;  // 139  71 137  301.8 0.489 0.545
    ColorOrchid             = $D670DA;  // 218 112 214  302.3 0.486 0.855
    ColorOrchid1            = $FA83FF;  // 255 131 250  302.4 0.486 1.000
    ColorOrchid3            = $C969CD;  // 205 105 201  302.4 0.488 0.804
    ColorOrchid2            = $E97AEE;  // 238 122 233  302.6 0.487 0.933
    ColorVioletRed          = $9020D0;  // 208  32 144  321.8 0.846 0.816
    ColorMaroon4            = $621C8B;  // 139  28  98  322.2 0.799 0.545
    ColorMediumVioletRed    = $8515C7;  // 199  21 133  322.2 0.894 0.780
    ColorMaroon3            = $9029CD;  // 205  41 144  322.3 0.800 0.804
    ColorMaroon2            = $A730EE;  // 238  48 167  322.4 0.798 0.933
    ColorMaroon1            = $B334FF;  // 255  52 179  322.5 0.796 1.000
    ColorDeepPink4          = $500A8B;  // 139  10  80  327.4 0.928 0.545
    ColorDeepPink2          = $8912EE;  // 238  18 137  327.5 0.924 0.933
    ColorDeepPink3          = $7610CD;  // 205  16 118  327.6 0.922 0.804
    ColorDeepPink           = $9314FF;  // 255  20 147  327.6 0.922 1.000
    ColorDeepPink1          = $9314FF;  // 255  20 147  327.6 0.922 1.000 *
    ColorHotPink            = $B469FF;  // 255 105 180  330.0 0.588 1.000
    ColorHotPink4           = $623A8B;  // 139  58  98  330.4 0.583 0.545
    ColorHotPink1           = $B46EFF;  // 255 110 180  331.0 0.569 1.000
    ColorHotPink2           = $A76AEE;  // 238 106 167  332.3 0.555 0.933
    ColorVioletRed4         = $52228B;  // 139  34  82  332.6 0.755 0.545
    ColorVioletRed1         = $963EFF;  // 255  62 150  332.6 0.757 1.000
    ColorVioletRed2         = $8C3AEE;  // 238  58 140  332.7 0.756 0.933
    ColorVioletRed3         = $7832CD;  // 205  50 120  332.9 0.756 0.804
    ColorHotPink3           = $9060CD;  // 205  96 144  333.6 0.532 0.804
    ColorLavenderBlush4     = $86838B;  // 139 131 134  337.5 0.058 0.545
    ColorMaroon             = $6030B0;  // 176  48  96  337.5 0.727 0.690
    ColorLavenderBlush2     = $E5E0EE;  // 238 224 229  338.6 0.059 0.933
    ColorLavenderBlush3     = $C5C1CD;  // 205 193 197  340.0 0.059 0.804
    ColorLavenderBlush      = $F5F0FF;  // 255 240 245  340.0 0.059 1.000
    ColorLavenderBlush1     = $F5F0FF;  // 255 240 245  340.0 0.059 1.000 *
    ColorPaleVioletRed1     = $AB82FF;  // 255 130 171  340.3 0.490 1.000
    ColorPaleVioletRed      = $9370DB;  // 219 112 147  340.4 0.489 0.859
    ColorPaleVioletRed3     = $8968CD;  // 205 104 137  340.4 0.493 0.804
    ColorPaleVioletRed2     = $9F79EE;  // 238 121 159  340.5 0.492 0.933
    ColorPaleVioletRed4     = $5D478B;  // 139  71  93  340.6 0.489 0.545
    ColorPink4              = $6C638B;  // 139  99 108  346.5 0.288 0.545
    ColorPink2              = $B8A9EE;  // 238 169 184  347.0 0.290 0.933
    ColorPink1              = $C5B5FF;  // 255 181 197  347.0 0.290 1.000
    ColorPink3              = $9E91CD;  // 205 145 158  347.0 0.293 0.804
    ColorPink               = $CBC0FF;  // 255 192 203  349.5 0.247 1.000
    ColorLightPink          = $C1B6FF;  // 255 182 193  351.0 0.286 1.000
    ColorLightPink2         = $ADA2EE;  // 238 162 173  351.3 0.319 0.933
    ColorLightPink3         = $958CCD;  // 205 140 149  351.7 0.317 0.804
    ColorLightPink4         = $655F8B;  // 139  95 101  351.8 0.317 0.545
    ColorLightPink1         = $B9AEFF;  // 255 174 185  351.9 0.318 1.000


  // Use this to index through colors as an array
  CONST HYPEColors:  ARRAY[1..455] OF TColor =
    ( ColorBlack,
      ColorGrey11,
      ColorGrey21,
      ColorGrey31,
      ColorDimGrey,
      ColorGrey41,
      ColorGrey51,
      ColorGrey61,
      ColorDarkGrey,
      ColorGrey71,
      ColorGrey,
      ColorGray81,
      ColorLightGray,
      ColorGainsboro,
      ColorGray91,
      ColorWhiteSmoke,
      ColorWhite,
      ColorSnow4,
      ColorSnow3,
      ColorSnow,
      ColorSnow1,
      ColorSnow2,
      ColorRosyBrown,
      ColorRosyBrown1,
      ColorRosyBrown3,
      ColorRosyBrown2,
      ColorRosyBrown4,
      ColorLightCoral,
      ColorIndianRed,
      ColorIndianRed4,
      ColorIndianRed2,
      ColorIndianRed1,
      ColorIndianRed3,
      ColorBrown,
      ColorBrown4,
      ColorBrown1,
      ColorBrown3,
      ColorBrown2,
      ColorFirebrick,
      ColorFirebrick1,
      ColorFirebrick4,
      ColorFirebrick3,
      ColorFirebrick2,
      ColorRed4,
      ColorDarkRed,
      ColorRed3,
      ColorRed2,
      ColorRed,
      ColorRed1,
      ColorMistyRose3,
      ColorMistyRose,
      ColorMistyRose1,
      ColorSalmon,
      ColorMistyRose2,
      ColorMistyRose4,
      ColorTomato3,
      ColorTomato,
      ColorTomato1,
      ColorTomato2,
      ColorTomato4,
      ColorCoral3,
      ColorCoral4,
      ColorCoral1,
      ColorCoral2,
      ColorSalmon2,
      ColorSalmon4,
      ColorSalmon3,
      ColorSalmon1,
      ColorDarkSalmon,
      ColorOrangeRed4,
      ColorCoral,
      ColorOrangeRed3,
      ColorOrangeRed2,
      ColorOrangeRed,
      ColorOrangeRed1,
      ColorLightSalmon2,
      ColorLightSalmon,
      ColorLightSalmon1,
      ColorLightSalmon4,
      ColorLightSalmon3,
      ColorSienna3,
      ColorSienna1,
      ColorSienna2,
      ColorSienna,
      ColorSienna4,
      ColorSeashell,
      ColorSeashell1,
      ColorChocolate3,
      ColorChocolate1,
      ColorChocolate2,
      ColorChocolate,
      ColorSaddleBrown,
      ColorChocolate4,
      ColorSeashell3,
      ColorSeashell2,
      ColorSeashell4,
      ColorSandyBrown,
      ColorPeachPuff2,
      ColorPeachPuff3,
      ColorPeachPuff,
      ColorPeachPuff1,
      ColorPeachPuff4,
      ColorTan1,
      ColorTan4,
      ColorTan2,
      ColorPeru,
      ColorTan3,
      ColorDarkOrange2,
      ColorDarkOrange4,
      ColorDarkOrange3,
      ColorDarkOrange1,
      ColorLinen,
      ColorBisque3,
      ColorBisque,
      ColorBisque1,
      ColorBisque2,
      ColorDarkOrange,
      ColorAntiqueWhite3,
      ColorAntiqueWhite1,
      ColorBurlywood4,
      ColorAntiqueWhite2,
      ColorBurlywood2,
      ColorBurlywood1,
      ColorBisque4,
      ColorBurlywood3,
      ColorBurlywood,
      ColorAntiqueWhite,
      ColorTan,
      ColorAntiqueWhite4,
      ColorNavajoWhite2,
      ColorNavajoWhite,
      ColorNavajoWhite1,
      ColorBlanchedAlmond,
      ColorNavajoWhite4,
      ColorNavajoWhite3,
      ColorPapayaWhip,
      ColorMoccasin,
      ColorOrange4,
      ColorOrange2,
      ColorOrange,
      ColorOrange1,
      ColorWheat4,
      ColorOrange3,
      ColorOldLace,
      ColorWheat,
      ColorWheat1,
      ColorWheat3,
      ColorWheat2,
      ColorFloralWhite,
      ColorDarkGoldenrod1,
      ColorDarkGoldenrod3,
      ColorDarkGoldenrod2,
      ColorDarkGoldenrod,
      ColorGoldenrod,
      ColorGoldenrod1,
      ColorGoldenrod4,
      ColorGoldenrod2,
      ColorGoldenrod3,
      ColorCornsilk,
      ColorCornsilk1,
      ColorCornsilk2,
      ColorCornsilk3,
      ColorLightGoldenrod2,
      ColorLightGoldenrod1,
      ColorLightGoldenrod3,
      ColorCornsilk4,
      ColorLightGoldenrod4,
      ColorGold4,
      ColorLightGoldenrod,
      ColorGold3,
      ColorGold,
      ColorGold1,
      ColorGold2,
      ColorLemonChiffon2,
      ColorLemonChiffon3,
      ColorLemonChiffon,
      ColorLemonChiffon1,
      ColorPaleGoldenrod,
      ColorKhaki4,
      ColorKhaki1,
      ColorKhaki3,
      ColorKhaki2,
      ColorLemonChiffon4,
      ColorDarkKhaki,
      ColorIvory4,
      ColorIvory3,
      ColorIvory2,
      ColorIvory,
      ColorIvory1,
      ColorBeige,
      ColorLightYellow4,
      ColorLightYellow3,
      ColorLightYellow2,
      ColorLightYellow,
      ColorLightYellow1,
      ColorLtGoldenrodYello,
      ColorYellow4,
      ColorYellow3,
      ColorYellow2,
      ColorYellow,
      ColorYellow1,
      ColorOliveDrab4,
      ColorOliveDrab,
      ColorOliveDrab1,
      ColorYellowGreen,
      ColorOliveDrab3,
      ColorOliveDrab2,
      ColorDarkOliveGreen,
      ColorDarkOliveGreen1,
      ColorDarkOliveGreen4,
      ColorDarkOliveGreen3,
      ColorDarkOliveGreen2,
      ColorGreenYellow,
      ColorChartreuse3,
      ColorChartreuse,
      ColorChartreuse1,
      ColorChartreuse4,
      ColorChartreuse2,
      ColorLawnGreen,
      ColorHoneydew4,
      ColorHoneydew3,
      ColorHoneydew2,
      ColorHoneydew,
      ColorHoneydew1,
      ColorDarkSeaGreen,
      ColorDarkSeaGreen1,
      ColorDarkSeaGreen3,
      ColorDarkSeaGreen2,
      ColorDarkSeaGreen4,
      ColorPaleGreen,
      ColorPaleGreen3,
      ColorPaleGreen2,
      ColorLightGreen,
      ColorPaleGreen4,
      ColorPaleGreen1,
      ColorForestGreen,
      ColorLimeGreen,
      ColorDarkGreen,
      ColorGreen4,
      ColorGreen3,
      ColorGreen2,
      ColorGreen,
      ColorGreen1,
      ColorSeaGreen1,
      ColorSeaGreen2,
      ColorSeaGreen,
      ColorSeaGreen4,
      ColorSeaGreen3,
      ColorMediumSeaGreen,
      ColorSpringGreen2,
      ColorSpringGreen4,
      ColorSpringGreen3,
      ColorSpringGreen,
      ColorSpringGreen1,
      ColorMintCream,
      ColorMedSpringGreen,
      ColorMediumAquamarine,
      ColorAquamarine3,
      ColorAquamarine,
      ColorAquamarine1,
      ColorAquamarine2,
      ColorAquamarine4,
      ColorTurquoise,
      ColorLightSeaGreen,
      ColorMediumTurquoise,
      ColorAzure4,
      ColorAzure3,
      ColorAzure2,
      ColorAzure,
      ColorAzure1,
      ColorLightCyan4,
      ColorLightCyan3,
      ColorLightCyan2,
      ColorLightCyan,
      ColorLightCyan1,
      ColorPaleTurquoise,
      ColorPaleTurquoise4,
      ColorPaleTurquoise1,
      ColorPaleTurquoise3,
      ColorPaleTurquoise2,
      ColorDarkSlateGray,
      ColorDarkSlateGray2,
      ColorDarkSlateGray1,
      ColorDarkSlateGray4,
      ColorDarkSlateGray3,
      ColorCyan4,
      ColorDarkCyan,
      ColorCyan3,
      ColorCyan2,
      ColorCyan,
      ColorCyan1,
      ColorDarkTurquoise,
      ColorCadetBlue,
      ColorTurquoise4,
      ColorTurquoise3,
      ColorTurquoise2,
      ColorTurquoise1,
      ColorCadetBlue4,
      ColorCadetBlue2,
      ColorCadetBlue1,
      ColorCadetBlue3,
      ColorPowderBlue,
      ColorLightBlue4,
      ColorLightBlue,
      ColorDeepSkyBlue3,
      ColorLightBlue1,
      ColorLightBlue2,
      ColorDeepSkyBlue4,
      ColorDeepSkyBlue2,
      ColorDeepSkyBlue,
      ColorDeepSkyBlue1,
      ColorLightBlue3,
      ColorSkyBlue,
      ColorLightSkyBlue3,
      ColorLightSkyBlue2,
      ColorLightSkyBlue1,
      ColorLightSkyBlue4,
      ColorLightSkyBlue,
      ColorSkyBlue3,
      ColorSkyBlue1,
      ColorSkyBlue2,
      ColorSkyBlue4,
      ColorSteelBlue2,
      ColorSteelBlue3,
      ColorSteelBlue,
      ColorSteelBlue1,
      ColorSteelBlue4,
      ColorAliceBlue,
      ColorDodgerBlue3,
      ColorDodgerBlue,
      ColorDodgerBlue1,
      ColorDodgerBlue2,
      ColorDodgerBlue4,
      ColorSlateGrey,
      ColorLightSlateGray,
      ColorSlateGray3,
      ColorSlateGray1,
      ColorSlateGray2,
      ColorSlateGray4,
      ColorLightSteelBlue4,
      ColorLightSteelBlue3,
      ColorLightSteelBlue2,
      ColorLightSteelBlue,
      ColorLightSteelBlue1,
      ColorCornflowerBlue,
      ColorRoyalBlue3,
      ColorRoyalBlue2,
      ColorRoyalBlue1,
      ColorRoyalBlue,
      ColorRoyalBlue4,
      ColorGhostWhite,
      ColorLavender,
      ColorMidnightBlue,
      ColorNavyBlue,
      ColorBlue4,
      ColorDarkBlue,
      ColorMediumBlue,
      ColorBlue3,
      ColorBlue2,
      ColorBlue,
      ColorBlue1,
      ColorSlateBlue,
      ColorSlateBlue1,
      ColorSlateBlue3,
      ColorLightSlateBlue,
      ColorSlateBlue2,
      ColorSlateBlue4,
      ColorDarkSlateBlue,
      ColorMediumSlateBlue,
      ColorMediumPurple4,
      ColorMediumPurple2,
      ColorMediumPurple,
      ColorMediumPurple3,
      ColorMediumPurple1,
      ColorPurple1,
      ColorBlueViolet,
      ColorPurple2,
      ColorPurple4,
      ColorPurple3,
      ColorPurple,
      ColorDarkOrchid4,
      ColorDarkOrchid2,
      ColorDarkOrchid,
      ColorDarkOrchid1,
      ColorDarkOrchid3,
      ColorDarkViolet,
      ColorMediumOrchid3,
      ColorMediumOrchid1,
      ColorMediumOrchid2,
      ColorMediumOrchid4,
      ColorMediumOrchid,
      ColorThistle4,
      ColorThistle,
      ColorThistle3,
      ColorThistle2,
      ColorThistle1,
      ColorPlum4,
      ColorPlum1,
      ColorPlum3,
      ColorPlum2,
      ColorDarkGoldenrod4,
      ColorPlum,
      ColorViolet,
      ColorMagenta4,
      ColorDarkMagenta,
      ColorMagenta3,
      ColorMagenta2,
      ColorMagenta,
      ColorMagenta1,
      ColorOrchid4,
      ColorOrchid,
      ColorOrchid1,
      ColorOrchid3,
      ColorOrchid2,
      ColorVioletRed,
      ColorMaroon4,
      ColorMediumVioletRed,
      ColorMaroon3,
      ColorMaroon2,
      ColorMaroon1,
      ColorDeepPink4,
      ColorDeepPink2,
      ColorDeepPink3,
      ColorDeepPink,
      ColorDeepPink1,
      ColorHotPink,
      ColorHotPink4,
      ColorHotPink1,
      ColorHotPink2,
      ColorVioletRed4,
      ColorVioletRed1,
      ColorVioletRed2,
      ColorVioletRed3,
      ColorHotPink3,
      ColorLavenderBlush4,
      ColorMaroon,
      ColorLavenderBlush2,
      ColorLavenderBlush3,
      ColorLavenderBlush,
      ColorLavenderBlush1,
      ColorPaleVioletRed1,
      ColorPaleVioletRed,
      ColorPaleVioletRed3,
      ColorPaleVioletRed2,
      ColorPaleVioletRed4,
      ColorPink4,
      ColorPink2,
      ColorPink1,
      ColorPink3,
      ColorPink,
      ColorLightPink,
      ColorLightPink2,
      ColorLightPink3,
      ColorLightPink4,
      ColorLightPink1);

  // Use this to index through colors as an array
  CONST HYPEColorNames:  ARRAY[1..455] OF STRING =
    ( 'Black',
      'Grey11',
      'Grey21',
      'Grey31',
      'DimGrey',
      'Grey41',
      'Grey51',
      'Grey61',
      'DarkGrey',
      'Grey71',
      'Grey',
      'Gray81',
      'LightGray',
      'Gainsboro',
      'Gray91',
      'WhiteSmoke',
      'White',
      'Snow4',
      'Snow3',
      'Snow',
      'Snow1',
      'Snow2',
      'RosyBrown',
      'RosyBrown1',
      'RosyBrown3',
      'RosyBrown2',
      'RosyBrown4',
      'LightCoral',
      'IndianRed',
      'IndianRed4',
      'IndianRed2',
      'IndianRed1',
      'IndianRed3',
      'Brown',
      'Brown4',
      'Brown1',
      'Brown3',
      'Brown2',
      'Firebrick',
      'Firebrick1',
      'Firebrick4',
      'Firebrick3',
      'Firebrick2',
      'Red4',
      'DarkRed',
      'Red3',
      'Red2',
      'Red',
      'Red1',
      'MistyRose3',
      'MistyRose',
      'MistyRose1',
      'Salmon',
      'MistyRose2',
      'MistyRose4',
      'Tomato3',
      'Tomato',
      'Tomato1',
      'Tomato2',
      'Tomato4',
      'Coral3',
      'Coral4',
      'Coral1',
      'Coral2',
      'Salmon2',
      'Salmon4',
      'Salmon3',
      'Salmon1',
      'DarkSalmon',
      'OrangeRed4',
      'Coral',
      'OrangeRed3',
      'OrangeRed2',
      'OrangeRed',
      'OrangeRed1',
      'LightSalmon2',
      'LightSalmon',
      'LightSalmon1',
      'LightSalmon4',
      'LightSalmon3',
      'Sienna3',
      'Sienna1',
      'Sienna2',
      'Sienna',
      'Sienna4',
      'Seashell',
      'Seashell1',
      'Chocolate3',
      'Chocolate1',
      'Chocolate2',
      'Chocolate',
      'SaddleBrown',
      'Chocolate4',
      'Seashell3',
      'Seashell2',
      'Seashell4',
      'SandyBrown',
      'PeachPuff2',
      'PeachPuff3',
      'PeachPuff',
      'PeachPuff1',
      'PeachPuff4',
      'Tan1',
      'Tan4',
      'Tan2',
      'Peru',
      'Tan3',
      'DarkOrange2',
      'DarkOrange4',
      'DarkOrange3',
      'DarkOrange1',
      'Linen',
      'Bisque3',
      'Bisque',
      'Bisque1',
      'Bisque2',
      'DarkOrange',
      'AntiqueWhite3',
      'AntiqueWhite1',
      'Burlywood4',
      'AntiqueWhite2',
      'Burlywood2',
      'Burlywood1',
      'Bisque4',
      'Burlywood3',
      'Burlywood',
      'AntiqueWhite',
      'Tan',
      'AntiqueWhite4',
      'NavajoWhite2',
      'NavajoWhite',
      'NavajoWhite1',
      'BlanchedAlmond',
      'NavajoWhite4',
      'NavajoWhite3',
      'PapayaWhip',
      'Moccasin',
      'Orange4',
      'Orange2',
      'Orange',
      'Orange1',
      'Wheat4',
      'Orange3',
      'OldLace',
      'Wheat',
      'Wheat1',
      'Wheat3',
      'Wheat2',
      'FloralWhite',
      'DarkGoldenrod1',
      'DarkGoldenrod3',
      'DarkGoldenrod2',
      'DarkGoldenrod',
      'Goldenrod',
      'Goldenrod1',
      'Goldenrod4',
      'Goldenrod2',
      'Goldenrod3',
      'Cornsilk',
      'Cornsilk1',
      'Cornsilk2',
      'Cornsilk3',
      'LightGoldenrod2',
      'LightGoldenrod1',
      'LightGoldenrod3',
      'Cornsilk4',
      'LightGoldenrod4',
      'Gold4',
      'LightGoldenrod',
      'Gold3',
      'Gold',
      'Gold1',
      'Gold2',
      'LemonChiffon2',
      'LemonChiffon3',
      'LemonChiffon',
      'LemonChiffon1',
      'PaleGoldenrod',
      'Khaki4',
      'Khaki1',
      'Khaki3',
      'Khaki2',
      'LemonChiffon4',
      'DarkKhaki',
      'Ivory4',
      'Ivory3',
      'Ivory2',
      'Ivory',
      'Ivory1',
      'Beige',
      'LightYellow4',
      'LightYellow3',
      'LightYellow2',
      'LightYellow',
      'LightYellow1',
      'LtGoldenrodYellow',  // fix this one
      'Yellow4',
      'Yellow3',
      'Yellow2',
      'Yellow',
      'Yellow1',
      'OliveDrab4',
      'OliveDrab',
      'OliveDrab1',
      'YellowGreen',
      'OliveDrab3',
      'OliveDrab2',
      'DarkOliveGreen',
      'DarkOliveGreen1',
      'DarkOliveGreen4',
      'DarkOliveGreen3',
      'DarkOliveGreen2',
      'GreenYellow',
      'Chartreuse3',
      'Chartreuse',
      'Chartreuse1',
      'Chartreuse4',
      'Chartreuse2',
      'LawnGreen',
      'Honeydew4',
      'Honeydew3',
      'Honeydew2',
      'Honeydew',
      'Honeydew1',
      'DarkSeaGreen',
      'DarkSeaGreen1',
      'DarkSeaGreen3',
      'DarkSeaGreen2',
      'DarkSeaGreen4',
      'PaleGreen',
      'PaleGreen3',
      'PaleGreen2',
      'LightGreen',
      'PaleGreen4',
      'PaleGreen1',
      'ForestGreen',
      'LimeGreen',
      'DarkGreen',
      'Green4',
      'Green3',
      'Green2',
      'Green',
      'Green1',
      'SeaGreen1',
      'SeaGreen2',
      'SeaGreen',
      'SeaGreen4',
      'SeaGreen3',
      'MediumSeaGreen',
      'SpringGreen2',
      'SpringGreen4',
      'SpringGreen3',
      'SpringGreen',
      'SpringGreen1',
      'MintCream',
      'MedSpringGreen',
      'MediumAquamarine',
      'Aquamarine3',
      'Aquamarine',
      'Aquamarine1',
      'Aquamarine2',
      'Aquamarine4',
      'Turquoise',
      'LightSeaGreen',
      'MediumTurquoise',
      'Azure4',
      'Azure3',
      'Azure2',
      'Azure',
      'Azure1',
      'LightCyan4',
      'LightCyan3',
      'LightCyan2',
      'LightCyan',
      'LightCyan1',
      'PaleTurquoise',
      'PaleTurquoise4',
      'PaleTurquoise1',
      'PaleTurquoise3',
      'PaleTurquoise2',
      'DarkSlateGray',
      'DarkSlateGray2',
      'DarkSlateGray1',
      'DarkSlateGray4',
      'DarkSlateGray3',
      'Cyan4',
      'DarkCyan',
      'Cyan3',
      'Cyan2',
      'Cyan',
      'Cyan1',
      'DarkTurquoise',
      'CadetBlue',
      'Turquoise4',
      'Turquoise3',
      'Turquoise2',
      'Turquoise1',
      'CadetBlue4',
      'CadetBlue2',
      'CadetBlue1',
      'CadetBlue3',
      'PowderBlue',
      'LightBlue4',
      'LightBlue',
      'DeepSkyBlue3',
      'LightBlue1',
      'LightBlue2',
      'DeepSkyBlue4',
      'DeepSkyBlue2',
      'DeepSkyBlue',
      'DeepSkyBlue1',
      'LightBlue3',
      'SkyBlue',
      'LightSkyBlue3',
      'LightSkyBlue2',
      'LightSkyBlue1',
      'LightSkyBlue4',
      'LightSkyBlue',
      'SkyBlue3',
      'SkyBlue1',
      'SkyBlue2',
      'SkyBlue4',
      'SteelBlue2',
      'SteelBlue3',
      'SteelBlue',
      'SteelBlue1',
      'SteelBlue4',
      'AliceBlue',
      'DodgerBlue3',
      'DodgerBlue',
      'DodgerBlue1',
      'DodgerBlue2',
      'DodgerBlue4',
      'SlateGrey',
      'LightSlateGray',
      'SlateGray3',
      'SlateGray1',
      'SlateGray2',
      'SlateGray4',
      'LightSteelBlue4',
      'LightSteelBlue3',
      'LightSteelBlue2',
      'LightSteelBlue',
      'LightSteelBlue1',
      'CornflowerBlue',
      'RoyalBlue3',
      'RoyalBlue2',
      'RoyalBlue1',
      'RoyalBlue',
      'RoyalBlue4',
      'GhostWhite',
      'Lavender',
      'MidnightBlue',
      'NavyBlue',
      'Blue4',
      'DarkBlue',
      'MediumBlue',
      'Blue3',
      'Blue2',
      'Blue',
      'Blue1',
      'SlateBlue',
      'SlateBlue1',
      'SlateBlue3',
      'LightSlateBlue',
      'SlateBlue2',
      'SlateBlue4',
      'DarkSlateBlue',
      'MediumSlateBlue',
      'MediumPurple4',
      'MediumPurple2',
      'MediumPurple',
      'MediumPurple3',
      'MediumPurple1',
      'Purple1',
      'BlueViolet',
      'Purple2',
      'Purple4',
      'Purple3',
      'Purple',
      'DarkOrchid4',
      'DarkOrchid2',
      'DarkOrchid',
      'DarkOrchid1',
      'DarkOrchid3',
      'DarkViolet',
      'MediumOrchid3',
      'MediumOrchid1',
      'MediumOrchid2',
      'MediumOrchid4',
      'MediumOrchid',
      'Thistle4',
      'Thistle',
      'Thistle3',
      'Thistle2',
      'Thistle1',
      'Plum4',
      'Plum1',
      'Plum3',
      'Plum2',
      'DarkGoldenrod4',
      'Plum',
      'Violet',
      'Magenta4',
      'DarkMagenta',
      'Magenta3',
      'Magenta2',
      'Magenta',
      'Magenta1',
      'Orchid4',
      'Orchid',
      'Orchid1',
      'Orchid3',
      'Orchid2',
      'VioletRed',
      'Maroon4',
      'MediumVioletRed',
      'Maroon3',
      'Maroon2',
      'Maroon1',
      'DeepPink4',
      'DeepPink2',
      'DeepPink3',
      'DeepPink',
      'DeepPink1',
      'HotPink',
      'HotPink4',
      'HotPink1',
      'HotPink2',
      'VioletRed4',
      'VioletRed1',
      'VioletRed2',
      'VioletRed3',
      'HotPink3',
      'LavenderBlush4',
      'Maroon',
      'LavenderBlush2',
      'LavenderBlush3',
      'LavenderBlush',
      'LavenderBlush1',
      'PaleVioletRed1',
      'PaleVioletRed',
      'PaleVioletRed3',
      'PaleVioletRed2',
      'PaleVioletRed4',
      'Pink4',
      'Pink2',
      'Pink1',
      'Pink3',
      'Pink',
      'LightPink',
      'LightPink2',
      'LightPink3',
      'LightPink4',
      'LightPink1');

IMPLEMENTATION

END.
