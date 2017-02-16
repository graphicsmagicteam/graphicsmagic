// GraphicsMagic Library Include file

{ ?? Use Andres Melanders GIFImage lib }
{$DEFINE USE_GIFImage}

{ Use Codegear/Embarcadero GIFImg.pas }
{$IFNDEF USE_GIFImage}
  {$DEFINE USE_CG_GIFImg}
{$ENDIF}

{ Use Christian-W. Budde PNG lib }
{$DEFINE USE_GR32_PNG}

{ Use JPEG Unit}
{$DEFINE USE_JPEG}

{ Use Wolfgang Krug TIFF Unit }
{$DEFINE USE_KRUG_TIFF}


