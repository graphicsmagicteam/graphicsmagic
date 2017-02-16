unit ReadTiffTags;

interface

uses WinProcs, SysUtils,Controls,Classes,Graphics, Dialogs;

const
  	// Byte Order
  tiffLittleEndian=$4949; // II: least significant byte to the most significant byte
  												// e.g. Intel
  tiffBigEndian=$4D4D;    // MM: most significant byte to least significant byte
  												// e.g. Motorola

  // identifies TIFF file
  tiffConfirm=$002A;

	// Tiff Tags
	tiffTag_ImageWidth							  = $0100;
	tiffTag_ImageLength               = $0101;
	tiffTag_BitsPerSample             = $0102;
	tiffTag_Compression               = $0103;
	tiffTag_PhotometricInterpretation = $0106;
	tiffTag_StripOffsets              = $0111;
	tiffTag_SamplesPerPixels          = $0115;
	tiffTag_RowsPerStrip              = $0116;
	tiffTag_StripByteCounts           = $0117;
	tiffTag_ColorMapOffset            = $0140;


type
		// Header structure
  THeaderTiff=record
    Endian  : Word;
    Confirm : Word;
    FirstIFD: LongInt;
  end;

  	// Image file Directory element (IFD)
	TTagTiff=record
    Tag     : Word;
    VarType : Word;
    Count   : LongInt;
    Value   : LongInt;
  end;
  PTagTiff=^TTagTiff;
  TListTag=array[0..0] of TTagTiff;
  PListTag=^TListTag;

  	// StripOffsets and StripByteCounts Information List
	TStripInfo=record
    StripOffsets    : LongInt;
    StripByteCounts : LongInt;
  end;
  PStripInfo=^TStripInfo;
  TListStripInfo=array[0..0] of TStripInfo;
  PListStripInfo=^TListStripInfo;


		// ------------------------------------------------------------------
		//  Class containing the graphic information
		// ------------------------------------------------------------------
  TFileTiff=class;

  TGraphicTiff=class
  private
    FCountTag			: Word;
    FError				: Boolean;
    FFileTiff			: TFileTiff;
    FFilePosition	: LongInt;
    FListTag			: PListTag;
    FNextIFD			: LongInt;
    FListStripInfo: PListStripInfo;
    FCountStripInfo: Word;
  protected
    procedure LoadIFD(AFileHandle:Integer);
  public
    constructor Create(AFileTiff:TFileTiff); virtual;
    procedure  QueryGraphic(AStringList:TStringList);
    destructor Destroy; override;
    property Error :Boolean read FError;
    property NextIFD :LongInt read FNextIFD;
  	function QueryGraphicByTag( TagId : Word ) : PTagTiff;
  	function QueryGraphicByOffsets( OffsetId : Word ) : PStripInfo;
  	function GetNoOfOffsets : Word;
  end;

		// ------------------------------------------------------------------
		//  Class containing image independent information
		// ------------------------------------------------------------------
  TFileTiff=class(TPersistent)
  private
    FFileName     : TFileName;
    FFileSize     : LongInt;
    FGraphicsList : TList;
    FLittleEndian : Boolean;
    FValid        : Boolean;
  protected
    function AddGraphicFromFile(AFileHandle:Integer;AFileOffset:LongInt):TGraphicTiff;
    procedure DeleteAll;
    function GetGraphicCount:Integer; virtual;
    procedure SetFileName(AFileName:TFileName); virtual;
  public
    constructor Create; virtual;
    function  GetGraphicTiff(AGraphicID:Byte):TGraphicTiff; virtual;
    procedure DeleteGraphic(AGraphicID:Byte);
    procedure ExchangeGraphic(AIndex1,AIndex2:Byte);
    destructor Destroy; override;
    property FileSize:LongInt read FFileSize;
    property GraphicTiff[AGraphicID:Byte]:TGraphicTiff read GetGraphicTiff;
    property GraphicCount:Integer read GetGraphicCount;
    property LittleEndian:Boolean read FLittleEndian;
    property Valid:Boolean read FValid;
  published
    property FileName:TFileName read FFileName write SetFileName;
  end;

function SwapLongInt(ALongInt:LongInt):LongInt;

implementation

function SwapLongInt(ALongInt:LongInt):LongInt;
var
  WordA :Word;
  WordB :Word;
begin
  WordA := Swap(HiWord(ALongInt));
  WordB := Swap(LoWord(ALongInt));
  SwapLongInt := MakeLong(WordA,WordB);
end;

{$R-}

	// ------------------------------------------------------------------
	//  Class containing the graphic information
	// ------------------------------------------------------------------

(*---------------------------------------------------------------*)
procedure TGraphicTiff.LoadIFD(AFileHandle:Integer);
var
  TagLoop             : Word;
  NoOfStripByteCounts : LongInt;
	NoOfStripOffsets    : LongInt;
  I                   : Integer;
begin
  if Assigned(FFileTiff) and (AFileHandle>-1) then
  begin
  	try
    	FFilePosition := FileSeek(AFileHandle,0,1); // actual position
    	FileRead(AFileHandle, FCountTag, 2); 				// read no of tags in Image file Directory
    	if not FFileTiff.LittleEndian then FCountTag := Swap(FCountTag);

    	if FCountTag>0 then
      begin
					// get memory for all IFD entries
      	GetMem(FListTag,SizeOf(TTagTiff)*FCountTag);
        	// read all ifd entries
      	FileRead(AFileHandle, FListTag^, SizeOf(TTagTiff)*FCountTag );
    		FFilePosition := FileSeek(AFileHandle,0,1); // actual position
      	if not FFileTiff.LittleEndian then		// Swap Bytes
        begin
        	for TagLoop := 0 to FCountTag-1 do
          begin
          	with FListTag^[TagLoop] do
            begin
			        Tag     := Swap(Tag);
        			VarType := Swap(VarType);
        			Count   := SwapLongInt(Count);
        			Value   := SwapLongInt(Value);
						end;
					end;
				end;

					// Check No of StripByteCounts and No of StripOffsets
				NoOfStripByteCounts := 0;
				NoOfStripOffsets    := 0;
        FCountStripInfo     := 0;
        for TagLoop := 0 to FCountTag-1 do
        begin
						// Strip Byte Counts
         	if FListTag^[TagLoop].Tag = tiffTag_StripByteCounts then
          begin
          	NoOfStripByteCounts := FListTag^[TagLoop].Count;
					end;
						// StripOffsets
         	if FListTag^[TagLoop].Tag = tiffTag_StripOffsets then
          begin
          	NoOfStripOffsets := FListTag^[TagLoop].Count;
					end;
				end;

				if (NoOfStripByteCounts = NoOfStripOffsets) then
        begin
        	FCountStripInfo := NoOfStripByteCounts;
        	GetMem(FListStripInfo, SizeOf(TListStripInfo)*FCountStripInfo);

				  	// Read StripOffsets and StripByteCounts into Information List
	        for TagLoop := 0 to FCountTag-1 do
        	begin
							// Strip Byte Counts
         		if FListTag^[TagLoop].Tag = tiffTag_StripByteCounts then
          	begin
	          	if FListTag^[TagLoop].Count = 1 then
  	          begin
	  	          FListStripInfo^[0].StripByteCounts := FListTag^[TagLoop].Value;
      	      end
	            else
  	          begin
              		// Scan the file for the list of Strip Bytes Counts
    						FileSeek(AFileHandle,FListTag^[TagLoop].Value,0);
  	  	  	    for I := 0 to FCountStripInfo-1 do
    	  	  		begin
		  	          FileRead(AFileHandle, FListStripInfo^[I].StripByteCounts, 4);
                end;
	 	        	end;
						end;

							// StripOffsets
         		if FListTag^[TagLoop].Tag = tiffTag_StripOffsets then
          	begin
	          	if FListTag^[TagLoop].Count = 1 then
  	          begin
	  	          FListStripInfo^[0].StripOffsets := FListTag^[TagLoop].Value;
      	      end
	            else
  	          begin
              		// Scan the file for the list of StripOffsets
    						FileSeek(AFileHandle,FListTag^[TagLoop].Value,0);
  	  	  	    for I := 0 to FCountStripInfo-1 do
    	  	  		begin
		  	          FileRead(AFileHandle, FListStripInfo^[I].StripOffsets, 4);
                end;
	 	        	end;
						end;
          end;
        end;
      end;

	    	// Read offset to the following IFD (or $00 00 00 00)
  	  try
    		FileSeek(AFileHandle,FFilePosition,0);
    	  FileRead(AFileHandle, FNextIFD, 4);
      	if not FFileTiff.LittleEndian then FNextIFD := SwapLongInt(FNextIFD);
	    except
  	    FNextIFD := 0;
    	end;
	  except
  	  FError:=True;
	  end;
	end;
end;

(*---------------------------------------------------------------*)
constructor TGraphicTiff.Create(AFileTiff:TFileTiff);
begin
  inherited Create;
  FCountTag      := 0;
  FError         := False;
  FFileTiff      := AFileTiff;
  FListTag       := nil;
  FListStripInfo := nil;
  FNextIFD       := 0;
	FCountStripInfo:= 0;
end;

(*---------------------------------------------------------------*)
(* return all IFD Tags as StringList
(*---------------------------------------------------------------*)
procedure TGraphicTiff.QueryGraphic(AStringList:TStringList);
var
  TagLoop : Word;
begin
  if Assigned(AStringList) then
  begin
    AStringList.Clear;
    if FCountTag>0 then
    begin
      AStringList.Add(' Tag  VarType  Count        Value');
      AStringList.Add('=====================================');
    	for TagLoop:=0 to FCountTag-1 do
      begin
      	with FListTag^[TagLoop] do
        begin
	       AStringList.Add(' $'+IntToHex(Tag,4)+
  	                     ' $'+IntToHex(VarType,4)+
    	                   ' $'+IntToHex(Count,8)+
      	                 ' $'+IntToHex(Value,8) )
				end;
			end;
    end;
  end;
end;

(*---------------------------------------------------------------*)
function TGraphicTiff.QueryGraphicByTag( TagId : Word ) : PTagTiff;
var
  TagLoop : Word;
begin
	Result := nil;
	if FCountTag>0 then
  begin
  	for TagLoop:=0 to FCountTag-1 do
    begin
    	if FListTag^[TagLoop].Tag = TagId then
      begin
      	Result := @FListTag^[TagLoop];
			end;
		end;
	end;
end;

(*---------------------------------------------------------------*)
function TGraphicTiff.QueryGraphicByOffsets( OffsetId : Word ) : PStripInfo;
begin
	Result := nil;
  if FListStripInfo = nil then exit;
 	if OffsetId >= FCountStripInfo then exit;

  if FCountStripInfo > 0 then
  begin
  	Result := @FListStripInfo^[OffsetId];
	end;
end;

(*---------------------------------------------------------------*)
function TGraphicTiff.GetNoOfOffsets : Word;
begin
	Result := FCountStripInfo;
end;

(*---------------------------------------------------------------*)
destructor TGraphicTiff.Destroy;
begin
  if FListTag<>nil then FreeMem(FListTag,SizeOf(TTagTiff)*FCountTag);
  if FListStripInfo<>nil then FreeMem(FListStripInfo);
  inherited Destroy;
end;


	// ------------------------------------------------------------------
	//  Class containing image independent information
	// ------------------------------------------------------------------

(*---------------------------------------------------------------*)
function TFileTiff.AddGraphicFromFile( AFileHandle : Integer;
                                       AFileOffset : LongInt):TGraphicTiff;
var
  NewGraphic : TGraphicTiff;
begin
  NewGraphic := TGraphicTiff.Create(Self);
  FileSeek(AFileHandle,AFileOffset,0);	// Reset FilePointer
  NewGraphic.LoadIFD(AFileHandle);			// Read Image File Directory
  FGraphicsList.Add(NewGraphic);				// Add List of IFD Tags
  AddGraphicFromFile := NewGraphic;			// Return list of IFD Tags
end;

(*---------------------------------------------------------------*)
procedure TFileTiff.DeleteAll;
var
  GraphicLoop : Integer;
begin
  if FGraphicsList.Count>0 then
  begin
    for GraphicLoop:=0 to FGraphicsList.Count-1 do
      TGraphicTiff(FGraphicsList.Items[GraphicLoop]).Free;
	end;
  FGraphicsList.Clear;
end;

(*---------------------------------------------------------------*)
function TFileTiff.GetGraphicCount:Integer;
begin
  GetGraphicCount := FGraphicsList.Count;
end;

(*---------------------------------------------------------------*)
function TFileTiff.GetGraphicTiff(AGraphicID:Byte):TGraphicTiff;
begin
  Result:=nil;
  if (FGraphicsList.Count>0) and (AGraphicID<FGraphicsList.Count)
    then Result:=TGraphicTiff(FGraphicsList.Items[AGraphicID]);
end;

(*---------------------------------------------------------------*)
procedure TFileTiff.SetFileName(AFileName:TFileName);
var
   TiffHandle:Integer;
   SearchFile:TSearchRec;
   HeaderTiff:THeaderTiff;
   GraphicTiff:TGraphicTiff;
begin
		// if it does not exists create it
  if not FileExists(AFileName) and (AFileName<>'') then
  begin
    TiffHandle := FileCreate(AFileName);
    if TiffHandle>-1 then
    begin
      if LittleEndian
        then HeaderTiff.Endian := tiffLittleEndian
        else HeaderTiff.Endian := tiffBigEndian;
      HeaderTiff.Confirm := tiffConfirm;
      if not LittleEndian then HeaderTiff.Confirm := Swap(HeaderTiff.Confirm);
      HeaderTiff.FirstIFD:=0;
      FileWrite(TiffHandle,HeaderTiff,SizeOf(THeaderTiff));
      FileClose(TiffHandle);
    end;
  end;

		// Try to Read Tiff Image
  if (AFileName<>'') and (AFileName<>FFileName) then
  begin
    DeleteAll;
    FValid := False;
    SysUtils.FindFirst(AFileName,faAnyFile,SearchFile);
    FFileSize:=SearchFile.Size;
    SysUtils.FindClose(SearchFile);
    if FFileSize>=SizeOf(THeaderTiff) then
    begin
      TiffHandle := FileOpen(AFileName,fmShareDenyNone);
				// Read Header
      FileRead(TiffHandle,HeaderTiff,SizeOf(THeaderTiff));
      FValid := True;
      if HeaderTiff.Endian=tiffLittleEndian then
      begin
      	FLittleEndian:=True;
			end
      else
      begin
      	if HeaderTiff.Endian=tiffBigEndian then
	      begin
  	    	FLittleEndian:=False;
				end
      	else FValid:=False;
			end;

      	// Header is a valid Tiff Header
      if FValid then
      begin
        if not FLittleEndian then
        begin
          HeaderTiff.Confirm  := Swap(HeaderTiff.Confirm);
          HeaderTiff.FirstIFD := SwapLongInt(HeaderTiff.FirstIFD);
        end;
        if HeaderTiff.Confirm<>tiffConfirm then FValid:=False;
      end;
	      	// Header is a valid Tiff Header. Try to read IFD
      FFileName:=AFileName;
      if HeaderTiff.FirstIFD >= SizeOf(THeaderTiff) then
      begin
      		// read Graphic Information of first image
        GraphicTiff := AddGraphicFromFile(TiffHandle,HeaderTiff.FirstIFD);
      		// if present: read Graphic Information of all following images
        while (GraphicTiff.NextIFD>=SizeOf(THeaderTiff)) do
          GraphicTiff:=AddGraphicFromFile(TiffHandle,GraphicTiff.NextIFD);
      end;
      FileClose(TiffHandle);
    end;
  end
  else
  begin
  	if AFileName='' then
    begin
    		// Reinitialization
    	DeleteAll;
	    FFileName := '';
  	  FFileSize := 0;
    	FValid    := False;
		end;
  end;
end;

(*---------------------------------------------------------------*)
constructor TFileTiff.Create;
begin
  inherited Create;
  FFileName     := '';
  FFileSize     := 0;
  FGraphicsList := TList.Create;
  FLittleEndian := True;
  FValid        := False;
end;

(*---------------------------------------------------------------*)
procedure TFileTiff.DeleteGraphic(AGraphicID:Byte);
begin
  if (FGraphicsList.Count>0) and (AGraphicID<FGraphicsList.Count) then
  begin
    TGraphicTiff(FGraphicsList).Free;
    FGraphicsList.Delete(AGraphicID);
    FGraphicsList.Pack;
  end;
end;

(*---------------------------------------------------------------*)
procedure TFileTiff.ExchangeGraphic(AIndex1, AIndex2 : Byte);
begin
  if (FGraphicsList.Count > 0)       and
     (AIndex1 < FGraphicsList.Count) and
     (AIndex2 < FGraphicsList.Count)
    then FGraphicsList.Exchange(AIndex1, AIndex2);
end;

(*---------------------------------------------------------------*)
destructor TFileTiff.Destroy;
begin
  DeleteAll;
  FGraphicsList.Free;
  inherited Destroy;
end;


end.
