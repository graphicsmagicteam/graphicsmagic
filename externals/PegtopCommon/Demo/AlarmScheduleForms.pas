unit AlarmScheduleForms;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, PegtopAlarmSchedules, PegtopMessageReceivers;

type
  TAlarmScheduleForm = class(TForm)
    PegtopAlarmSchedule1: TPegtopAlarmSchedule;
    Button1: TButton;
    Memo1: TMemo;
    Button2: TButton;
    Button3: TButton;
    Memo2: TMemo;
    Memo3: TMemo;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure PegtopAlarmSchedule1Alarm(Sender: TObject; Alarm: TPegtopAlarm;
      var Remove: Boolean);
    procedure PegtopAlarmSchedule1Remove(Sender: TObject; Alarm: TPegtopAlarm);
  private
    Counter: Integer;
  public
    { Public declarations }
  end;

var
  AlarmScheduleForm: TAlarmScheduleForm;

implementation

{$R *.DFM}

type
  TMyAlarm = class
  public
    Counter: Integer;
    Echo: Boolean;
  end;

procedure TAlarmScheduleForm.FormCreate(Sender: TObject);
begin
  Counter := 0;
end;

procedure TAlarmScheduleForm.Button1Click(Sender: TObject);
var
  Seconds: Integer;
  MyAlarm: TMyAlarm;
begin
  Seconds := TComponent(Sender).Tag;
  Inc(Counter);
  // add new alarm:
  MyAlarm := TMyAlarm.Create;
  MyAlarm.Counter := Counter;
  MyAlarm.Echo := False;
  PegtopAlarmSchedule1.AddAlarm(Now + Seconds / 86400, pamOnce, MyAlarm);
  // show message:
  Memo1.Lines.Add(Format('Click %d at %s', [MyAlarm.Counter, FormatDateTime('hh:nn:ss.zzz', Now)]));
  // update display:
  Label1.Caption := IntToStr(PegtopAlarmSchedule1.AlarmCount) + ' alarms queued';
end;

procedure TAlarmScheduleForm.PegtopAlarmSchedule1Alarm(Sender: TObject; Alarm: TPegtopAlarm;
  var Remove: Boolean);
var
  MyAlarm: TMyAlarm;
begin
  MyAlarm := Alarm.Data;
  if not MyAlarm.Echo then begin
    // show message:
    Memo2.Lines.Add(Format('Alarm %d at %s', [MyAlarm.Counter, FormatDateTime('hh:nn:ss.zzz', Alarm.Time)]));
    // to generate an echo, change the alarm time:
    Alarm.ChangeTime(Alarm.Time + 1 / 86400, Alarm.Fire); // echo after 1 second (86400 seconds per day)
    MyAlarm.Echo := True;
    Remove := False; // do not remove alarm (Remove is True by default for Alarm.Mode = amOnce)
  end
  else begin
    // show message:
    Memo3.Lines.Add(Format('Echo %d at %s', [MyAlarm.Counter, FormatDateTime('hh:nn:ss.zzz', Alarm.Time)]));
    // update display (alarm is removed AFTER OnAlarm, so subtract 1):
    Label1.Caption := IntToStr(PegtopAlarmSchedule1.AlarmCount - 1) + ' alarms queued';
  end;
end;

procedure TAlarmScheduleForm.PegtopAlarmSchedule1Remove(Sender: TObject;
  Alarm: TPegtopAlarm);
var
  MyAlarm: TMyAlarm;
begin
  MyAlarm := Alarm.Data;
  MyAlarm.Free;
end;

end.
