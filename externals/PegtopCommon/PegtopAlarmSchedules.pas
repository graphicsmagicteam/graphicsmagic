////////////////////////////////////////////////////////////////////////////////
// File:       PegtopAlarmSchedules.pas
// Components: TPegtopAlarmSchedule
// Version:    1.02
// History:    1.00 05 Oct 2003 created
//             1.01 16 Feb 2004 uses PegtopTimeUtils now
//             1.02 25 Sep 2005 simple item removal added, hourly mode added,
//                              AddAlarmAfter added, OnTime event added,
//                              OnFire event extended
// Author:     Jens Gruschel (GRU)
// Copyright:  (c) 2003 Jens Gruschel
// Website:    http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Description:
// TPegtopAlarmSchedule holds a list of alarm times (daily, weekly, monthly or
// yearly repeating or only once) and fires OnAlarm whenever such a time is
// reached. Afterwards the alarm is removed from the schedule list if its Mode
// is amOnce (otherwise it fires again after one day, week, month or year
// depending on its mode). You can change this behaviour by changing the Remove
// variable in the OnAlarm event handler (make sure a non-repeating alarm
// (amOnce) is removed or its Time is changed to some time in the future,
// otherwise it fires over and over again).
////////////////////////////////////////////////////////////////////////////////
// Note:
// For each alarm you can specify some additional data. If you do so, add an
// OnRemove event handler, which is called every time an alarm is removed (after
// OnAlarm or Clear), and which can be used to free data objects.
////////////////////////////////////////////////////////////////////////////////
// Description:
// TPegtopAlarm holds the alarm properties.
// * Time is the alarm date / time (for pamYear, pamMonth, pamWeek and pamDay
//   this is any one typical date / time), which can only be changed by
//   ChangeTime (you have to specify after which time you want the alarm to be
//   fired for repeating alarms, typically this parameter is Now).
// * Fire is the next date / time the alarm is fired (for repeating alarms).
// * Mode is the alarm mode (pamYearly, pamMonthly, pamWeekly, pamDaily for
//   repeating alarms or pamOnce, which is the default)
// * Data is an additional pointer (that typically references some object or
//   record).
////////////////////////////////////////////////////////////////////////////////
// License:
// Please read the license.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Documentation / Installation:
// Please read the readme.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////

unit PegtopAlarmSchedules;

interface

uses
  Windows, Messages, Classes,
  PegtopMessageReceivers, PegtopPriorityQueues;

type
  TPegtopAlarmMode = (pamOnce, pamYearly, pamMonthly, pamWeekly, pamDaily, pamHourly);

  TPegtopAlarm = class
  private
    FTime: TDateTime;
    FFire: TDateTime;
    FMode: TPegtopAlarmMode;
    FData: Pointer;
  public
    constructor Create(ATime: TDateTime; AMode: TPegtopAlarmMode; After: TDateTime; AData: Pointer);
    function CalcFire(After: TDateTime): TDateTime;
    procedure ChangeTime(Value: TDateTime; After: TDateTime);
    property Time: TDateTime read FTime;
    property Fire: TDateTime read FFire;
    property Mode: TPegtopAlarmMode read FMode;
    property Data: Pointer read FData write FData;
  end;

  TPegtopScheduleTimeEvent = procedure(Sender: TObject; ATime: TDateTime) of object;
  TPegtopScheduleAlarmEvent = procedure(Sender: TObject; Alarm: TPegtopAlarm; FireTime: TDateTime; var Remove: Boolean) of object;
  TPegtopScheduleRemoveEvent = procedure(Sender: TObject; Alarm: TPegtopAlarm) of object;

  TPegtopAlarmSchedule = class(TPegtopCustomMessageReceiver)
  private
    FAlarmQueue: TPegtopFloatPriorityQueue;
    FOnTime: TPegtopScheduleTimeEvent;
    FOnAlarm: TPegtopScheduleAlarmEvent;
    FOnRemove: TPegtopScheduleRemoveEvent;
    FEnabled: Boolean;
    procedure AlarmQueueRemove(Sender: TObject; Data: Pointer);
    procedure SetEnabled(Value: Boolean);
    function GetAlarmCount: Integer;
  protected
    procedure HandleMessage(var Msg: TMessage; var Handled: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddAlarm(ATime: TDateTime; AMode: TPegtopAlarmMode; AData: Pointer = NIL); overload;
    procedure AddAlarm(ATime: TDateTime; AData: Pointer = NIL); overload;
    procedure AddAlarmAfter(After, ATime: TDateTime; AMode: TPegtopAlarmMode; AData: Pointer = NIL); overload;
    procedure AddAlarmAfter(After, ATime: TDateTime; AData: Pointer = NIL); overload;
    function RemoveAlarm(AData: Pointer): Boolean;
    procedure Clear;
    procedure Check;
    procedure CheckWithoutDelay;
    function GetNextAlarm: TPegtopAlarm;
  published
    property OnTime: TPegtopScheduleTimeEvent read FOnTime write FOnTime;
    property OnAlarm: TPegtopScheduleAlarmEvent read FOnAlarm write FOnAlarm;
    property OnRemove: TPegtopScheduleRemoveEvent read FOnRemove write FOnRemove;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property AlarmCount: Integer read GetAlarmCount;
  end;

implementation

uses
  SysUtils, PegtopTimeUtils;

const
  WM_CHECKALARM = WM_USER + $CA;

////////////////////////////////////////////////////////////////////////////////
// TPegtopAlarm
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopAlarm.Create(ATime: TDateTime; AMode: TPegtopAlarmMode; After: TDateTime; AData: Pointer);
begin
  FTime := ATime;
  FMode := AMode;
  FData := AData;
  FFire := CalcFire(After);
end;

function TPegtopAlarm.CalcFire(After: TDateTime): TDateTime;
var
  Year, Month, Day: Word;
begin
  case FMode of
    pamOnce:
      Result := FTime;
    pamYearly:
      begin
        DecodeDate(FTime, Year, Month, Day);
        Result := GetNextDayYearly(After, Month, Day, Frac(FTime));
      end;
    pamMonthly:
      begin
        DecodeDate(FTime, Year, Month, Day);
        Result := GetNextDayMonthly(After, Day, Frac(FTime));
      end;
    pamWeekly:
      begin
        Day := DayOfWeek(FTime);
        Result := GetNextDayOfWeekWeekly(After, Day, Frac(FTime));
      end;
    pamDaily:
      begin
        Result := Trunc(After) + Frac(FTime);
        if Result <= After then Result := Result + 1.0; // one day later
      end;
    pamHourly:
      begin
        Result := Trunc(After * 24.0) / 24.0 + Frac(FTime * 24.0) / 24.0;
        if Result <= After then Result := Result + 1.0 / 24.0; // one hour later
      end;
    else // avoid compiler warnings
      Result := FTime;
  end;
end;

procedure TPegtopAlarm.ChangeTime(Value: TDateTime; After: TDateTime);
begin
  FTime := Value;
  FFire := CalcFire(After);
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopAlarmSchedule
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopAlarmSchedule.Create(AOwner: TComponent);
begin
  FAlarmQueue := TPegtopFloatPriorityQueue.Create;
  FAlarmQueue.Order := poLowestFirst;
  FOnAlarm := NIL;
  FOnRemove := NIL;
  FEnabled := True;
  inherited;
end;

destructor TPegtopAlarmSchedule.Destroy;
begin
  KillTimer(Handle, 0);
  inherited;
  FAlarmQueue.Clear(AlarmQueueRemove);
  FAlarmQueue.Free;
end;

procedure TPegtopAlarmSchedule.AlarmQueueRemove(Sender: TObject; Data: Pointer); // callback for FAlarmQueue.Clear;
var
  Alarm: TPegtopAlarm;
begin
  Alarm := Data;
  if Assigned(FOnRemove) then FOnRemove(Self, Alarm);
  Alarm.Free;
end;

procedure TPegtopAlarmSchedule.HandleMessage(var Msg: TMessage; var Handled: Boolean);
begin
  if Msg.Msg = WM_TIMER then begin
    if FEnabled then CheckWithoutDelay;
    Handled := True;
  end
  else if Msg.Msg = WM_CHECKALARM then begin
    CheckWithoutDelay;
    Handled := True;
  end;
end;

procedure TPegtopAlarmSchedule.CheckWithoutDelay;
var
  Alarm: TPegtopAlarm;
  NowTime: TDateTime;
  FireTime: TDateTime;
  TimeSpan: Double;
  Remove: Boolean;
const
  SecPerDay = 86400.0;
  Sec = 1.0 / SecPerDay;
begin
  KillTimer(Handle, 0);
  Alarm := FAlarmQueue.Peek;
  NowTime := Now;
  if Assigned(FOnTime) then FOnTime(Self, NowTime);
  if Alarm <> NIL then begin
    if Alarm.Fire <= NowTime then begin
      Remove := Alarm.Mode = pamOnce;
      FireTime := Alarm.Fire;
      if not Remove then Alarm.ChangeTime(Alarm.Time, Alarm.Fire);
      if Assigned(FOnAlarm) then FOnAlarm(Self, Alarm, FireTime, Remove);
      if Remove then begin
        FAlarmQueue.Pop;
        if Assigned(FOnRemove) then FOnRemove(Self, Alarm);
        Alarm.Free;
      end
      else begin
        FAlarmQueue.Repulse(Alarm.Fire);
      end;
      Check;
    end
    else begin
      TimeSpan := Alarm.Fire - NowTime;
      if TimeSpan >= 360*Sec then      // more than 6 minutes?
        SetTimer(Handle, 0, 300000, NIL) //   => signal after 5 minutes
      else if TimeSpan >= 60*Sec then  // more than 1 minute?
        SetTimer(Handle, 0, 60000, NIL)  //   => signal again after 1 minute
      else if TimeSpan < 0.1*Sec then  // less than 1/10 second?
        SetTimer(Handle, 0, 100, NIL)    //   => signal after 1/10 second
      else
        SetTimer(Handle, 0, Round(TimeSpan * SecPerDay * 1000.0), NIL)
    end;
  end;
end;

procedure TPegtopAlarmSchedule.AddAlarm(ATime: TDateTime; AMode: TPegtopAlarmMode; AData: Pointer = NIL);
var
  Alarm: TPegtopAlarm;
begin
  Alarm := TPegtopAlarm.Create(ATime, AMode, Now, AData);
  FAlarmQueue.Push(Alarm.Fire, Alarm);
  if FEnabled then Check;
end;

procedure TPegtopAlarmSchedule.AddAlarm(ATime: TDateTime; AData: Pointer = NIL);
var
  Alarm: TPegtopAlarm;
begin
  Alarm := TPegtopAlarm.Create(ATime, pamOnce, Now, AData);
  FAlarmQueue.Push(Alarm.Fire, Alarm);
  if FEnabled then Check;
end;

procedure TPegtopAlarmSchedule.AddAlarmAfter(After, ATime: TDateTime; AMode: TPegtopAlarmMode; AData: Pointer = NIL);
var
  Alarm: TPegtopAlarm;
begin
  if (AMode <> pamOnce) or (ATime > After) then begin
    Alarm := TPegtopAlarm.Create(ATime, AMode, After, AData);
    FAlarmQueue.Push(Alarm.Fire, Alarm);
    if FEnabled then Check;
  end;
end;

procedure TPegtopAlarmSchedule.AddAlarmAfter(After, ATime: TDateTime; AData: Pointer = NIL);
var
  Alarm: TPegtopAlarm;
begin
  if ATime > After then begin
    Alarm := TPegtopAlarm.Create(ATime, pamOnce, After, AData);
    FAlarmQueue.Push(Alarm.Fire, Alarm);
    if FEnabled then Check;
  end;
end;

function TPegtopAlarmSchedule.RemoveAlarm(AData: Pointer): Boolean;
var
  Alarm: TPegtopAlarm;
  I: Integer;
begin
  Result := False;
  for I := FAlarmQueue.Count - 1 downto 0 do begin
    Alarm := FAlarmQueue.Data[I];
    if Alarm.Data = AData then FAlarmQueue.Delete(I);
  end;
end;

procedure TPegtopAlarmSchedule.Clear;
begin
  KillTimer(Handle, 0);
  FAlarmQueue.Clear(AlarmQueueRemove);
end;

procedure TPegtopAlarmSchedule.Check;
begin
  if not (csDesigning in ComponentState) then PostMessage(Handle, WM_CHECKALARM, 0, 0);
end;

function TPegtopAlarmSchedule.GetNextAlarm: TPegtopAlarm;
begin
  Result := FAlarmQueue.Peek;
end;

procedure TPegtopAlarmSchedule.SetEnabled(Value: Boolean);
begin
  if FEnabled <> Value then begin
    FEnabled := Value;
    if FEnabled then Check else KillTimer(Handle, 0);
  end;
end;

function TPegtopAlarmSchedule.GetAlarmCount: Integer;
begin
  Result := FAlarmQueue.Count;
end;

end.
