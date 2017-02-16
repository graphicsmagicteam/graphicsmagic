unit ReadTiff;
(*
	10.10.1998: started with code of Jean Marc CARAYON and Daniel DROUIN, France
  14.10.1998: Uncompressed Palette Image added by Wolfgang Krug
  15.10.1998: Uncompressed Bilevel Image added by Wolfgang Krug
  15.10.1998: Uncompressed Gray Scale Image added by Wolfgang Krug
  15.10.1998: Uncompressed RGB Full Color Image added by Wolfgang Krug
*)


interface

uses Windows, SysUtils, Graphics, Dialogs, ReadTiffTags;


Type
  ColorTableRed   = array [0..256] of Word;
  ColorTableGreen = array [0..256] of Word;
  ColorTableBlue  = array [0..256] of Word;

	// File format functions
procedure LoadTiffFromFile     (FileName : string; Bitmap : TBitmap);
procedure TIFFBilevelImage     (FileName : string; GraphTiff : TGraphicTiff; var Bitmap : TBitmap);
procedure TIFFGrayScaleImage   (FileName : string; GraphTiff : TGraphicTiff; var Bitmap : TBitmap);
procedure TIFFColorPaletImage  (FileName : string; GraphTiff : TGraphicTiff; var Bitmap : TBitmap);
procedure TIFFRGBFullColorImage(FileName : string; GraphTiff : TGraphicTiff; var Bitmap : TBitmap);

	// Internal Functions
procedure InitColTab;
procedure CreateGreyColTab( BitsPerSample : Integer);
procedure CreateBilevelColTab( White : Integer);
procedure ReadColTabFromFile( AFileName   : string;
                              ACounter    : LongInt;
                              AFileOffset : LongInt);
function ReadImageDataFromFile(AFileName    : string;
                               var AImageBuffer : PChar;
                               AGraphTiff   : TGraphicTiff):Integer;
procedure AlignBmpRows(var AImageBuffer : PChar; AGraphTiff : TGraphicTiff);
procedure ReadBitsPerSampleColor(AFileName    : string;
                                 AFileOffset  : LongInt;
	                    					 var BPSRed   : Integer;
                                 var BPSGreen : Integer;
                                 var BPSBlue  : Integer);
procedure DisplayImage(width    : Word; height : Word;
                       BitCount : Word; buff   : PChar;
                       Bitmap   : TBitmap);

var
  FColorTableRed   : ColorTableRed;
  FColorTableGreen : ColorTableGreen;
  FColorTableBlue  : ColorTableBlue;

implementation

uses Unit1;

//
// Internal Functions
//

(*---------------------------------------------------------------*)
procedure InitColTab;
var
	I : Integer;
begin
		// create Color Table of 256 shades of grey
	for I:=0 to 255 do begin
	  FColorTableRed[I]   := Word(I);
  	FColorTableGreen[I] := Word(I);
	  FColorTableBlue[I]  := Word(I);
  end;
end;

(*---------------------------------------------------------------*)
procedure CreateGreyColTab( BitsPerSample : Integer);
var
	I : Integer;
begin
	InitColTab;

	if BitsPerSample = 4 then
  begin
		for I:=0 to 15 do begin
		  FColorTableRed[I]   := Word(I * 255 div 15);
  		FColorTableGreen[I] := Word(I * 255 div 15);
	  	FColorTableBlue[I]  := Word(I * 255 div 15);
	  end;
  end;
end;

(*---------------------------------------------------------------*)
procedure CreateBilevelColTab( White : Integer);
begin
	InitColTab;

	if White = 1 then
  begin
		  FColorTableRed[0]   := 0;
  		FColorTableGreen[0] := 0;
	  	FColorTableBlue[0]  := 0;

		  FColorTableRed[1]   := 255;
  		FColorTableGreen[1] := 255;
	  	FColorTableBlue[1]  := 255;
  end
  else
  begin
		  FColorTableRed[0]   := 255;
  		FColorTableGreen[0] := 255;
	  	FColorTableBlue[0]  := 255;

		  FColorTableRed[1]   := 0;
  		FColorTableGreen[1] := 0;
	  	FColorTableBlue[1]  := 0;
  end;
end;

(*---------------------------------------------------------------*)
procedure ReadColTabFromFile( AFileName   : string;
                              ACounter    : LongInt;
                              AFileOffset : LongInt);
var
 	TiffHandle : Integer;
	I          : Integer;
begin
	InitColTab;

	ACounter := ACounter div 3;
  if ACounter < 1   then exit;
  if ACounter > 256 then exit;

	TiffHandle := FileOpen(AFileName, fmShareDenyNone);

  	// read Color Table from file
  FileSeek(TiffHandle, AFileOffset, 0);
  FileRead(TiffHandle, FColorTableRed,   ACounter*SizeOf(Word));
  FileRead(TiffHandle, FColorTableGreen, ACounter*SizeOf(Word));
  FileRead(TiffHandle, FColorTableBlue,  ACounter*SizeOf(Word));

	for I:=0 to ACounter do begin
  	FColorTableRed[I]   := FColorTableRed[I]   div 256;
  	FColorTableGreen[I] := FColorTableGreen[I] div 256;
	  FColorTableBlue[I]  := FColorTableBlue[I]  div 256;
  end;

	FileClose(TiffHandle);
end;

(*---------------------------------------------------------------*)
function ReadImageDataFromFile( AFileName    : string;
                                var AImageBuffer : PChar;
                                AGraphTiff   : TGraphicTiff) : Integer;
var
	ImageSize  : LongInt;
 	TiffHandle : Integer;
  PStrpInfo  : PStripInfo;
  I          : Integer;
  BuffPtr    : PChar;
  Count      : Integer;
Begin
	Result := 0;

			// Prepare Data buffer
	ImageSize := 0;
	for I:=0 to AGraphTiff.GetNoOfOffsets-1 do
  begin
		PStrpInfo := AGraphTiff.QueryGraphicByOffsets(I);
    if PStrpInfo <> nil then
	    ImageSize := ImageSize + PStrpInfo^.StripByteCounts;
  end;
  GetMem(AImageBuffer, ImageSize);
	BuffPtr := AImageBuffer;

	TiffHandle := FileOpen(AFileName, fmShareDenyNone);
    	// Read Image Data
	for I:=0 to AGraphTiff.GetNoOfOffsets-1 do
  begin
		PStrpInfo := AGraphTiff.QueryGraphicByOffsets(I);
    if PStrpInfo <> nil then
    begin
		  FileSeek(TiffHandle, PStrpInfo^.StripOffsets, 0);
  		Count := FileRead(TiffHandle, BuffPtr^, PStrpInfo^.StripByteCounts);
    	BuffPtr := BuffPtr + PStrpInfo^.StripByteCounts;
	    if Count <> PStrpInfo^.StripByteCounts then
  	  begin
    		Result := 1;
	  		FreeMem(AImageBuffer, ImageSize);
  	    exit;
    	end;
		end;
	end;
	FileClose(TiffHandle);
end;

(*---------------------------------------------------------------*)
procedure AlignBmpRows(var AImageBuffer : PChar; AGraphTiff : TGraphicTiff);
var
  PTag             : PTagTiff;
  PStrpInfo        : PStripInfo;
  ImageHeight	     : LongInt;
  RowsPerStrip     : LongInt;
  RowLength        : LongInt;
  AlignedRowLength : LongInt;
  TargetBuffer     : PChar;
  TargetBufPtr     : PChar;
  SourceBufPtr     : PChar;
  I                : Integer;
begin
		// Keep in mind: The rows of a Bitmap are DWORD aligned !!!

	PStrpInfo := AGraphTiff.QueryGraphicByOffsets(0);

			// Read Image Height
  PTag := AGraphTiff.QueryGraphicByTag( tiffTag_ImageLength );
  ImageHeight := PTag^.Value;

			// Read Rows Per Strip
  PTag := AGraphTiff.QueryGraphicByTag( tiffTag_RowsPerStrip );
  RowsPerStrip := PTag^.Value;

	RowLength := PStrpInfo^.StripByteCounts div RowsPerStrip;
  AlignedRowLength := ((RowLength+3) div 4) * 4;

  if RowLength <> AlignedRowLength then
  begin
	  GetMem(TargetBuffer, AlignedRowLength*ImageHeight);
    SourceBufPtr := AImageBuffer;
    TargetBufPtr := TargetBuffer;
  	for I:=0 to ImageHeight-1 do
	  begin
      CopyMemory(TargetBufPtr, SourceBufPtr, RowLength*SizeOf(Byte));
	    SourceBufPtr := SourceBufPtr + RowLength;
	    TargetBufPtr := TargetBufPtr + AlignedRowLength;
  	end;
	  FreeMem(AImageBuffer);
  	AImageBuffer := TargetBuffer;
	end;
end;

(*---------------------------------------------------------------*)
procedure ReadBitsPerSampleColor(AFileName    : string;
                                 AFileOffset  : LongInt;
	                    					 var BPSRed   : Integer;
                                 var BPSGreen : Integer;
                                 var BPSBlue  : Integer);
var
 	TiffHandle : Integer;
begin
    	// Read Bits per sample for Red green and Blue Color

	TiffHandle := FileOpen(AFileName, fmShareDenyNone);
	FileSeek(TiffHandle, AFileOffset, 0);
  FileRead(TiffHandle, BPSRed,   2);
  FileRead(TiffHandle, BPSGreen, 2);
  FileRead(TiffHandle, BPSBlue,  2);
	FileClose(TiffHandle);
end;

(*---------------------------------------------------------------*)
procedure ExchangeRedAndBlue(var AImageBuffer : PChar; Size : LongInt);
var
  Bits      : PChar;
  BitsPtr   : PChar;
  I         : LongInt;
  Red, Blue : Char;
begin
		// Keep in mind: The RGB-Pixels of a Bitmap are ordered BGR !!!
	BitsPtr := AImageBuffer;
  for I:=0 to Size-1 do
  begin
  	Blue := (BitsPtr)^ ;
    Red  := (BitsPtr+2)^;
    (BitsPtr)^   := Red;
    (BitsPtr+2)^ := Blue;
    BitsPtr := BitsPtr + 3;
  end;
end;

(*---------------------------------------------------------------*)
procedure DisplayImage(width    : Word; height : Word;
                       BitCount : Word; buff: PChar;
                       Bitmap : TBitmap);
var
  I       : Integer;
	hBmp    : HBITMAP;
  BI      : PBitmapInfo;
  BIH     : TBitmapInfoHeader;
  Bmp     : TBitmap;
  ImagoDC : hDC;
begin
		// Fill BitmapInfoHeader structure
	BIH.biSize   		 	 	:= Sizeof(BIH);
	BIH.biWidth  		 	 	:= width;
  BIH.biHeight 		 	 	:= -height;
	BIH.biPlanes 		 	 	:= 1;
  BIH.biBitCount 	 	 	:= BitCount;
	BIH.biCompression 	:= BI_RGB;
  BIH.biSizeImage	 	 	:= 0;
	BIH.biXPelsPerMeter := 0;
  BIH.biYPelsPerMeter := 0;
	BIH.biClrUsed       := 0;
  BIH.biClrImportant  := 0;

{$P+,S-,W-,R-}

 		// Create DIB Bitmap Info with actual color table
	BI := AllocMem(SizeOf(TBitmapInfoHeader) + 256*Sizeof(TRGBQuad));
	try
	  BI^.bmiHeader := BIH;
	  for I:=0 to 255 do begin
 			BI^.bmiColors[I].rgbBlue     := Byte(FColorTableBlue[I]);
	   	BI^.bmiColors[I].rgbGreen    := Byte(FColorTableGreen[I]);
  	  BI^.bmiColors[I].rgbRed      := Byte(FColorTableRed[I]);
	  	BI^.bmiColors[I].rgbReserved := 0;
	  end;

 		Bitmap.Assign( nil );	// Clear actual Image

	  Bmp        := TBitmap.Create;
  	Bmp.Height := width;
	  Bmp.Width  := height;

	  ImagoDC := GetDC(Bitmap.Handle);
	  hBmp :=  CreateDIBitmap(
    				ImagoDC,					// handle of device context
    				BIH,							// address of bitmap size and format data
    				CBM_INIT,					// initialization flag
	    			buff,							// address of initialization data
  	  			BI^,							// address of bitmap color-format data
    				DIB_RGB_COLORS ); // color-data usage
	  Bmp.Handle := hBmp;

		Bitmap.Assign( Bmp );

	  Bmp.Free;
	except
		showmessage('Out Of Memory ');
	  exit;
  end;
  FreeMem( BI, SizeOf(TBitmapInfoHeader) + 256*Sizeof(TRGBQuad));
{$P-,S+,W+,R+}
end;


(*---------------------------------------------------------------*)
(*---------------------------------------------------------------*)
(*-- Main Function: Load TIFF File and determine type of image --*)
(*---------------------------------------------------------------*)
(*---------------------------------------------------------------*)
procedure LoadTiffFromFile(FileName : string; Bitmap : TBitmap);
var
	TiffFile                  : TFileTiff;
  GraphTiff                 : TGraphicTiff;
  I                         : Integer;
  PTag                      : PTagTiff;
  PhotometricInterpretation : LongInt;
  BitsPerSample             : LongInt;
begin
		TiffFile := TFileTiff.Create;
	  TiffFile.FileName := FileName;

		if NOT TiffFile.Valid then
    begin
    	showmessage('<'+FileName+'> is not a valid TIFF file');
	  	TiffFile.Free;
      exit;
    end;

			// Go over all images within the TIFF file
    for I := 0 to TiffFile.GraphicCount-1 do
    begin
	    GraphTiff := TiffFile.GetGraphicTiff(I);

	      //
  	   	// Check the type of image
    	  //

				// Read Photometric interpretation
			PTag := GraphTiff.QueryGraphicByTag( tiffTag_PhotometricInterpretation );
    	if PTag = nil then
    	begin
    		showmessage('Error reading tag Photometric interpretation');
		  	TiffFile.Free;
  	    exit;
			end;
    	PhotometricInterpretation := PTag^.Value;

					//  Photometric interpretation should be:
        	// 0, 1 = Bilevel Image
	        // 2 = Palette color image
  	      // 3 = RGB Full Color Image
			if (PhotometricInterpretation < 0) or
      	 (PhotometricInterpretation > 3) then
	    begin
  	  	showmessage('Error evaluating Photometric interpretation: '+IntToStr(PhotometricInterpretation));
		  	TiffFile.Free;
  	    exit;
			end;

  			// Bilevel Image or Grayscale Image
			if (PhotometricInterpretation = 0) or (PhotometricInterpretation = 1) then
	  	begin
						// Read Bits Per Sample
  		  PTag := GraphTiff.QueryGraphicByTag( tiffTag_BitsPerSample );
    		if PTag = nil then
	      begin
        	TIFFBilevelImage(TiffFile.FileName, GraphTiff, Bitmap);
				end
        else
        begin
	        BitsPerSample := PTag^.Value;
        	if BitsPerSample = 1 then
        	begin
          	TIFFBilevelImage(TiffFile.FileName, GraphTiff, Bitmap);
          end
          else
          begin
	        	if (BitsPerSample = 4) or (BitsPerSample = 8) then
  	      	begin
      	    	TIFFGrayScaleImage(TiffFile.FileName, GraphTiff, Bitmap);
        	  end;
          end;
        end;
      end;

        // Color Palette Image
			if (PhotometricInterpretation = 3) then
      begin
      	TIFFColorPaletImage(TiffFile.FileName, GraphTiff, Bitmap);
      end;

        // RGB Full Color Image
			if (PhotometricInterpretation = 2) then
      begin
      	TIFFRGBFullColorImage(TiffFile.FileName, GraphTiff, Bitmap);
      end;
		end;

	  TiffFile.Free;
end;


(*---------------------------------------------------------------*)
(*---------------------------------------------------------------*)
(*--- Bilevel Image ---*)
(*---------------------------------------------------------------*)
(*---------------------------------------------------------------*)
procedure TIFFBilevelImage(FileName : string; GraphTiff : TGraphicTiff; var Bitmap : TBitmap);
var
  PTag        : PTagTiff;
  ImageWidth	: LongInt;
  ImageHeight	: LongInt;
  Compression : LongInt;
  PhotometricInterpretation : LongInt;
  ImageBuffer : PChar;
begin
		// Read Image Width
	PTag := GraphTiff.QueryGraphicByTag( tiffTag_ImageWidth );
  if PTag = nil then
  begin
  	showmessage('Error reading tag Image Width');
    exit;
	end;
  ImageWidth := PTag^.Value;

			// Read Image Height
  PTag := GraphTiff.QueryGraphicByTag( tiffTag_ImageLength );
  if PTag = nil then
  begin
    showmessage('Error reading tag Image Height');
    exit;
	end;
  ImageHeight := PTag^.Value;

				// Read Photometric interpretation
	PTag := GraphTiff.QueryGraphicByTag( tiffTag_PhotometricInterpretation );
  if PTag = nil then
  begin
  	showmessage('Error reading tag Photometric interpretation');
    exit;
	end;
	PhotometricInterpretation := PTag^.Value;

			// Read Compression
	PTag := GraphTiff.QueryGraphicByTag( tiffTag_Compression );
  if PTag = nil then
  begin
  	showmessage('Error reading tag Compression');
    exit;
	end;
  Compression := PTag^.Value;
  	// 1 = No Compression
    // 2 = CCITT Group 3 (1-Dimensional Modified Huffman run length encoding)
    // 32773 = PxckBits compression, a simple byte-oriented run length scheme.

	if Compression = 1 then			// NO Compression
  begin
    CreateBilevelColTab( PhotometricInterpretation );

    	// Read Image Data
		if ReadImageDataFromFile( FileName, ImageBuffer, GraphTiff) = 0 then
    begin
			AlignBmpRows(ImageBuffer, GraphTiff);
    	DisplayImage(ImageWidth, ImageHeight, 1, ImageBuffer, Bitmap);
      FreeMem(ImageBuffer);
    end
    else
    begin
  		showmessage('Error reading image data');
    end;
  end
  else
  begin
		showmessage('Cant read compressed bilevel image'+#13+#13+
  	            'feel free to implement this type of image'+#13+
    	          'and send it back to krug@sdm.de');
    exit;
  end;
end;

(*---------------------------------------------------------------*)
(*---------------------------------------------------------------*)
(*--- Gray Scale Image ---*)
(*---------------------------------------------------------------*)
(*---------------------------------------------------------------*)
procedure TIFFGrayScaleImage (FileName : string; GraphTiff : TGraphicTiff; var Bitmap : TBitmap);
var
  PTag          : PTagTiff;
  ImageWidth	  : LongInt;
  ImageHeight	  : LongInt;
  Compression   : LongInt;
  BitsPerSample : LongInt;
  ImageBuffer   : PChar;
begin
		// Read Image Width
	PTag := GraphTiff.QueryGraphicByTag( tiffTag_ImageWidth );
  if PTag = nil then
  begin
  	showmessage('Error reading tag Image Width');
    exit;
	end;
  ImageWidth := PTag^.Value;

			// Read Image Height
  PTag := GraphTiff.QueryGraphicByTag( tiffTag_ImageLength );
  if PTag = nil then
  begin
    showmessage('Error reading tag Image Height');
    exit;
	end;
  ImageHeight := PTag^.Value;

			// Read Compression
	PTag := GraphTiff.QueryGraphicByTag( tiffTag_Compression );
  if PTag = nil then
  begin
  	showmessage('Error reading tag Compression');
    exit;
	end;
  Compression := PTag^.Value;
  	// 1 = No Compression
    // 2 = CCITT Group 3 (1-Dimensional Modified Huffman run length encoding)
    // 32773 = PxckBits compression, a simple byte-oriented run length scheme.

	if Compression = 1 then	// NO Compression
  begin
  		// Read Bits Per Sample
  	PTag := GraphTiff.QueryGraphicByTag( tiffTag_BitsPerSample );
  	if PTag = nil then
	  begin
  		showmessage('Error reading tag Bits Per Sample');
    	exit;
		end;
    BitsPerSample := PTag^.Value;
    	// Check Value of BitsPerSample
    if (BitsPerSample <> 4) and (BitsPerSample <> 8) then
    begin
  		showmessage('Error: Bits Per Sample not 4 or 8');
    	exit;
    end;

		CreateGreyColTab( BitsPerSample );

    	// Read Image Data
		if ReadImageDataFromFile( FileName, ImageBuffer, GraphTiff) = 0 then
    begin
			AlignBmpRows(ImageBuffer, GraphTiff);
    	DisplayImage(ImageWidth, ImageHeight, BitsPerSample, ImageBuffer, Bitmap);
      FreeMem(ImageBuffer);
    end
    else
    begin
  		showmessage('Error reading image data');
    end;
  end
  else
  begin
		showmessage('Cant read compressed Gray Scale Image'+#13+#13+
  	            'feel free to implement this type of image'+#13+
    	          'and send it back to krug@sdm.de');
  end;
end;

(*---------------------------------------------------------------*)
(*---------------------------------------------------------------*)
(*--- Color Palet Image ---*)
(*---------------------------------------------------------------*)
(*---------------------------------------------------------------*)
procedure TIFFColorPaletImage(FileName : string; GraphTiff : TGraphicTiff; var Bitmap : TBitmap);
var
  PTag          : PTagTiff;
  ImageWidth	  : LongInt;
  ImageHeight	  : LongInt;
  Compression   : LongInt;
  BitsPerSample : LongInt;
  ImageBuffer   : PChar;
begin
		// Read Image Width
	PTag := GraphTiff.QueryGraphicByTag( tiffTag_ImageWidth );
  if PTag = nil then
  begin
  	showmessage('Error reading tag Image Width');
    exit;
	end;
  ImageWidth := PTag^.Value;

			// Read Image Height
  PTag := GraphTiff.QueryGraphicByTag( tiffTag_ImageLength );
  if PTag = nil then
  begin
    showmessage('Error reading tag Image Height');
    exit;
	end;
  ImageHeight := PTag^.Value;

			// Read Compression
	PTag := GraphTiff.QueryGraphicByTag( tiffTag_Compression );
  if PTag = nil then
  begin
  	showmessage('Error reading tag Compression');
    exit;
	end;
  Compression := PTag^.Value;
  	// 1 = No Compression
    // 2 = CCITT Group 3 (1-Dimensional Modified Huffman run length encoding)
    // 32773 = PxckBits compression, a simple byte-oriented run length scheme.

	if Compression = 1 then	// NO Compression
  begin
  		// Read Bits Per Sample
  	PTag := GraphTiff.QueryGraphicByTag( tiffTag_BitsPerSample );
  	if PTag = nil then
	  begin
  		showmessage('Error reading tag Bits Per Sample');
    	exit;
		end;
    BitsPerSample := PTag^.Value;

			// read Color Table Offset
	  PTag := GraphTiff.QueryGraphicByTag( tiffTag_ColorMapOffset );
  	if PTag = nil then
	  begin
  		showmessage('Error reading tag Color Table Offset');
    	exit;
		end;
		ReadColTabFromFile( FileName, PTag^.Count, PTag^.Value);

    	// Read Image Data
		if ReadImageDataFromFile( FileName, ImageBuffer, GraphTiff) = 0 then
    begin
			AlignBmpRows(ImageBuffer, GraphTiff);
    	DisplayImage(ImageWidth, ImageHeight, BitsPerSample, ImageBuffer, Bitmap);
      FreeMem(ImageBuffer);
    end
    else
    begin
  		showmessage('Error reading image data');
    end;
  end
  else
  begin
		showmessage('Cant read compressed ColorPaletImage'+#13+#13+
  	            'feel free to implement this type of image'+#13+
    	          'and send it back to krug@sdm.de');
  end;
end;

(*---------------------------------------------------------------*)
(*---------------------------------------------------------------*)
(*--- RGB Full Color Image ---*)
(*---------------------------------------------------------------*)
(*---------------------------------------------------------------*)
procedure TIFFRGBFullColorImage(FileName : string; GraphTiff : TGraphicTiff; var Bitmap : TBitmap);
var
  PTag               : PTagTiff;
  ImageWidth	       : LongInt;
  ImageHeight	       : LongInt;
  Compression        : LongInt;
  BitsPerSampleRed   : Integer;
  BitsPerSampleGreen : Integer;
  BitsPerSampleBlue  : Integer;
  ImageBuffer        : PChar;
begin
		// Read Image Width
	PTag := GraphTiff.QueryGraphicByTag( tiffTag_ImageWidth );
  if PTag = nil then
  begin
  	showmessage('Error reading tag Image Width');
    exit;
	end;
  ImageWidth := PTag^.Value;

			// Read Image Height
  PTag := GraphTiff.QueryGraphicByTag( tiffTag_ImageLength );
  if PTag = nil then
  begin
    showmessage('Error reading tag Image Height');
    exit;
	end;
  ImageHeight := PTag^.Value;

			// Read Compression
	PTag := GraphTiff.QueryGraphicByTag( tiffTag_Compression );
  if PTag = nil then
  begin
  	showmessage('Error reading tag Compression');
    exit;
	end;
  Compression := PTag^.Value;
  	// 1 = No Compression
    // 2 = CCITT Group 3 (1-Dimensional Modified Huffman run length encoding)
    // 32773 = PxckBits compression, a simple byte-oriented run length scheme.

	if Compression = 1 then	// NO Compression
  begin
  		// Read Bits Per Sample
  	PTag := GraphTiff.QueryGraphicByTag( tiffTag_BitsPerSample );
  	if PTag = nil then
	  begin
  		showmessage('Error reading tag Bits Per Sample');
    	exit;
		end;
		if PTag^.Count = 1 then
    begin
	    BitsPerSampleRed   := PTag^.Value;
	    BitsPerSampleGreen := PTag^.Value;
	    BitsPerSampleBlue  := PTag^.Value;
		end
    else
    begin
    	ReadBitsPerSampleColor(FileName, PTag^.Value,
	                    BitsPerSampleRed, BitsPerSampleGreen, BitsPerSampleBlue);
    end;

    if (BitsPerSampleRed   <> 8) and
       (BitsPerSampleGreen <> 8) and
       (BitsPerSampleBlue  <> 8) then
    begin
  		showmessage('Error: Bits Per Sample (RGB) not 8');
    	exit;
    end;

    	// Read Image Data
		if ReadImageDataFromFile( FileName, ImageBuffer, GraphTiff) = 0 then
    begin
			ExchangeRedAndBlue(ImageBuffer, ImageWidth*ImageHeight);
			AlignBmpRows(ImageBuffer, GraphTiff);
    	DisplayImage(ImageWidth, ImageHeight, 24, ImageBuffer, Bitmap);
      FreeMem(ImageBuffer);
    end
    else
    begin
  		showmessage('Error reading image data');
    end;
  end
  else
  begin
		showmessage('Cant read compressed RGB Full Color Image'+#13+#13+
  	            'feel free to implement this type of image'+#13+
    	          'and send it back to krug@sdm.de');
  end;
end;


end.
