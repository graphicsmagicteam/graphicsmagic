unit PegtopTimeUtils;

interface

function GetLocalDateTime: TDateTime;
function GetUTCDateTime: TDateTime;
function SetLocalDateTime(Value: TDateTime): Boolean;
function SetUTCDateTime(Value: TDateTime): Boolean;

function GetNextDayMonthly(MinDate: TDateTime; Day: Smallint; OptTime: Double = 0.0): TDateTime;
function GetNextDayYearly(MinDate: TDateTime; Month, Day: Word; OptTime: Double = 0.0): TDateTime;
function GetNextDayOfWeekWeekly(MinDate: TDateTime; Day: Word; OptTime: Double = 0.0): TDateTime;
function GetNextDayOfWeekMonthly(MinDate: TDateTime; Week: Smallint; Day: Word; OptTime: Double = 0.0): TDateTime;
function GetNextDayOfWeekYearly(MinDate: TDateTime; Month: Word; Week: SmallInt; Day: Word; OptTime: Double = 0.0): TDateTime;

function ConvertUTCToInternetTime(UTCTime: TDateTime): Double;
function ConvertInternetTimeToUTC(InternetTime: Double): TDateTime;

function GetFileTimes(const FileName: String; out Created, Accessed, Modified: TDateTime): Boolean;

implementation

uses
  Windows, SysUtils;

function GetLocalDateTime: TDateTime;
var
  T: TSystemTime;
begin
  GetLocalTime(T);
  Result := EncodeDate(T.wYear, T.wMonth, T.wDay) +
    EncodeTime(T.wHour, T.wMinute, T.wSecond, T.wMilliseconds);
end;

function GetUTCDateTime: TDateTime;
var
  T: TSystemTime;
begin
  GetSystemTime(T);
  Result := EncodeDate(T.wYear, T.wMonth, T.wDay) +
    EncodeTime(T.wHour, T.wMinute, T.wSecond, T.wMilliseconds);
end;

function SetLocalDateTime(Value: TDateTime): Boolean;
var
  T: TSystemTime;
begin
  DecodeDate(Value, T.wYear, T.wMonth, T.wDay);
  DecodeTime(Value, T.wHour, T.wMinute, T.wSecond, T.wMilliseconds);
  // The wDayOfWeek member of the SYSTEMTIME structure is ignored.
  Result := SetLocalTime(T);
end;

function SetUTCDateTime(Value: TDateTime): Boolean;
var
  T: TSystemTime;
begin
  DecodeDate(Value, T.wYear, T.wMonth, T.wDay);
  DecodeTime(Value, T.wHour, T.wMinute, T.wSecond, T.wMilliseconds);
  // The wDayOfWeek member of the SYSTEMTIME structure is ignored.
  Result := SetSystemTime(T);
end;

function GetNextDayMonthly(MinDate: TDateTime; Day: Smallint; OptTime: Double = 0.0): TDateTime;
var
  Y, M, D: Word;
begin
  // find day of month after MinDate
  // ("next 5th" if Day > 0, or "next 5th last day of month" if Day < 0)
  if Day < 0 then begin
    // count backwards from end of month
    DecodeDate(MinDate, Y, M, D);
    Inc(M);
    if M > 12 then begin
      M := 1;
      Inc(Y);
    end;
    Result := EncodeDate(Y, M, 1) + Day + OptTime;
    if Result < MinDate then begin
      if M = 12 then // next January
        Result := EncodeDate(Y + 1, 1, 1) + Day + OptTime
      else // one month later
        Result := EncodeDate(Y, M + 1, 1) + Day + OptTime;
    end;
  end
  else begin
    // normal (from beginning of month)
    DecodeDate(MinDate, Y, M, D);
    Result := EncodeDate(Y, M, Word(Day)) + OptTime;
    if Result < MinDate then begin
      if M = 12 then // next January
        Result := EncodeDate(Y + 1, 1, Word(Day)) + OptTime
      else // one month later
        Result := EncodeDate(Y, M + 1, Word(Day)) + OptTime;
    end;
  end;
end;

function GetNextDayYearly(MinDate: TDateTime; Month, Day: Word; OptTime: Double = 0.0): TDateTime;
var
  Y, M, D: Word;
begin
  // find day of year after MinDate
  // (anniversary or birthday like "next November the 24th")
  DecodeDate(MinDate, Y, M, D);
  Result := EncodeDate(Y, Month, Day) + OptTime;
  if Result < MinDate then // one year later
    Result := EncodeDate(Y + 1, Month, Day) + OptTime;
end;

function GetNextDayOfWeekWeekly(MinDate: TDateTime; Day: Word; OptTime: Double = 0.0): TDateTime;
var
  W: Word;
begin
  // find day of week after MinDate
  // ("next Wednesday" with Day in 1 for Sunday to 7 for Saturday)
  W := DayOfWeek(MinDate);
  Result := Trunc(MinDate) + Day - W + OptTime;
  if Result < MinDate then // one week later
    Result := Result + 7.0;
end;

function GetNextDayOfWeekMonthly(MinDate: TDateTime; Week: Smallint; Day: Word; OptTime: Double = 0.0): TDateTime;
var
  Y, M, D: Word;
  T: TDateTime;
begin
  // find n-th day of week after MinDate
  // ("next 2nd Friday of month" with Day in 1 for Sunday to 7 for Saturday,
  // or "last Tuesday of month" if Week < 0)
  if Week < 0 then begin
    // count backwards from end of month
    DecodeDate(MinDate, Y, M, D);
    Inc(M);
    if M > 12 then begin
      M := 1;
      Inc(Y);
    end;
    T := EncodeDate(Y, M, 1);
    Result := T - ((Week - 1) * 7) - ((DayOfWeek(T) - Day + 6) mod 7) - 1.0 + OptTime;
    if Result < MinDate then begin
      if M = 12 then begin
        // next January
        Result := EncodeDate(Y + 1, M, 1) - ((Week - 1) * 7) - ((DayOfWeek(T) - Day + 6) mod 7) - 1.0 + OptTime;
      end
      else begin
        // one month later
        Result := EncodeDate(Y, M + 1, 1) - ((Week - 1) * 7) - ((DayOfWeek(T) - Day + 6) mod 7) - 1.0 + OptTime;
      end;
    end;
  end
  else begin
    // normal (from beginning of month)
    DecodeDate(MinDate, Y, M, D);
    T := EncodeDate(Y, M, 1);
    Result := T + ((Week - 1) * 7) + ((Day - DayOfWeek(T) + 7) mod 7) + OptTime;
    if Result < MinDate then begin
      if M = 12 then begin
        // next January
        T := EncodeDate(Y + 1, 1, 1);
        Result := T + ((Week - 1) * 7) + ((Day - DayOfWeek(T) + 7) mod 7) + OptTime;
      end
      else begin
        // one month later
        T := EncodeDate(Y, M + 1, 1);
        Result := T + ((Week - 1) * 7) + ((Day - DayOfWeek(T) + 7) mod 7) + OptTime;
      end;
    end;
  end;
end;

function GetNextDayOfWeekYearly(MinDate: TDateTime; Month: Word; Week: SmallInt; Day: Word; OptTime: Double = 0.0): TDateTime;
var
  Y, M, D: Word;
  T: TDateTime;
begin
  // find n-th day of week for a given month after MinDate
  // ("next 1st Sunday in April" with Day in 1 for Sunday to 7 for Saturday,
  // or "last Sunday in October" if Week < 0)
  if Week < 0 then begin
    // count backwards from end of month
    DecodeDate(MinDate, Y, M, D);
    M := Month + 1;
    if M > 12 then begin
      M := 1;
      Inc(Y);
    end;
    T := EncodeDate(Y, M, 1);
    Result := T + ((Week + 1) * 7) - ((DayOfWeek(T) - Day + 6) mod 7) - 1.0 + OptTime;
    if Result < MinDate then begin
      // one year later
      T := EncodeDate(Y + 1, M, 1);
      Result := T + ((Week + 1) * 7) - ((DayOfWeek(T) - Day + 6) mod 7) - 1.0 + OptTime;
    end;
  end
  else begin
    // normal (from beginning of month)
    DecodeDate(MinDate, Y, M, D);
    T := EncodeDate(Y, Month, 1);
    Result := T + ((Week - 1) * 7) + ((Day - DayOfWeek(T) + 7) mod 7) + OptTime;
    if Result < MinDate then begin
      // one year later
      T := EncodeDate(Y + 1, Month, 1);
      Result := T + ((Week - 1) * 7) + ((Day - DayOfWeek(T) + 7) mod 7) + OptTime;
    end;
  end;
end;

function ConvertUTCToInternetTime(UTCTime: TDateTime): Double;
begin
  Result := Frac(UTCTime + (1/24)) * 1000.0;
end;

function ConvertInternetTimeToUTC(InternetTime: Double): TDateTime;
begin
  Result := Frac(InternetTime * 0.001 + (23/24));
end;

function GetFileTimes(const FileName: String; out Created, Accessed, Modified: TDateTime): Boolean;
var
  Handle: THandle;
  Time1, Time2, Time3: TFileTime;
  SysTime: TSystemTime;
  TimeZoneInfo: TTimeZoneInformation;
  Bias: Double;
begin
  Result := False;
  Handle := FileOpen(FileName, fmOpenRead or fmShareDenyNone);
  if Handle > 0 then try
    if GetTimeZoneInformation(TimeZoneInfo) <> $FFFFFFFF then
      Bias := TimeZoneInfo.Bias * (1/1440)
    else
      Bias := 0.0;
    GetFileTime(Handle, @Time1, @Time2, @Time3);
    if FileTimeToSystemTime(Time1, SysTime) then
      Created := SystemTimeToDateTime(SysTime) - Bias;
    if FileTimeToSystemTime(Time2, SysTime) then
      Accessed := SystemTimeToDateTime(SysTime) - Bias;
    if FileTimeToSystemTime(Time3, SysTime) then
      Modified := SystemTimeToDateTime(SysTime) - Bias;
    Result := True;
  finally
    FileClose(Handle);
  end;
end;

end.
