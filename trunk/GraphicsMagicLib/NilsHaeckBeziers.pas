{ This library is used to call the routines in the NilsHaeckBeziers.dll which
  includes the routines of sdBeziers.pas written by Nils Haeck.

  Although Nils permitted us to distribute the sdBezier.pas with GraphicsMagic,
  we don't want to damage his interests. So we think that open the source code
  of sdBeziers.pas by himself is better.

  The NilsHaeckBeziers.dll is for demonstrational purposes only. You may NOT
  use it in any commercial or freeware project, unless with SPECIFIC permission
  from Nils Haeck.

  Detailed information about the sdBeziers.pas, please contact the author of
  the library or visit the following website:
  http://www.simdesign.nl/tips/tip003.html

  Many thanks to Nils Haeck < n.haeck@simdesign.nl >.
  SimDesign B.V.
  http://www.simdesign.nl/ }

unit NilsHaeckBeziers;

{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CAST OFF}
{$WARN UNSAFE_CODE OFF}

interface

uses
  Windows;  // TPoint

type
  TsdPoint = record
    X, Y: Double;
  end;

  TsdBezier = record
    P: array [0..3] of TsdPoint;
  end;

  TsdRectangle = record
    Left, Top, Right, Bottom: Double;
  end;

  TClosestPointOnBezierFunc = procedure (const ABezier: TsdBezier;
                                         const APoint: TsdPoint; Tol: Double;
                                         var A: Double; var Steps: Integer;
                                         var ADist: Double; var Closest: TsdPoint); stdcall;

  TIsPointOnBezierFunc = function (const ABezier: TsdBezier; const APoint: TsdPoint;
                                   var Steps: Integer; Tol: Double): Boolean; stdcall;

  TSplitBezierWithFactorFunc = procedure (const ABezier: TsdBezier; A: Double;
                                          var B1, B2: TsdBezier); stdcall;

  TBezierToWindowsFormatFunc = procedure (const ABezier: TsdBezier;
                                          var P: array of TPoint); stdcall;

var
  NilsHaeckClosestPointOnBezier : TClosestPointOnBezierFunc;
  NilsHaeckIsPointOnBezier      : TIsPointOnBezierFunc;
  NilsHaeckSplitBezierWithFactor: TSplitBezierWithFactorFunc;
  NilsHaeckBezierToWindowsFormat: TBezierToWindowsFormatFunc;

implementation

uses
  SysUtils, Dialogs, Forms;

var
  BezierLibHandle: THandle;

procedure LoadNilsHaeckBeziersFuntions;
begin
  NilsHaeckClosestPointOnBezier  := nil;
  NilsHaeckIsPointOnBezier       := nil;
  NilsHaeckSplitBezierWithFactor := nil;
  NilsHaeckBezierToWindowsFormat := nil;

  if FileExists('NilsHaeckBeziers.dll') then
  begin
    BezierLibHandle := LoadLibrary( PChar('NilsHaeckBeziers.dll') );

    if BezierLibHandle >= 32 then
    begin
      NilsHaeckClosestPointOnBezier  := GetProcAddress(BezierLibHandle, 'NilsClosestPointOnBezier');
      NilsHaeckIsPointOnBezier       := GetProcAddress(BezierLibHandle, 'NilsIsPointOnBezier');
      NilsHaeckSplitBezierWithFactor := GetProcAddress(BezierLibHandle, 'NilsSplitBezierWithFactor');
      NilsHaeckBezierToWindowsFormat := GetProcAddress(BezierLibHandle, 'NilsBezierToWindowsFormat');
    end
    else
    begin
      // raise some exception (DLL not loaded)
      MessageDlg('Cannot load the NilsHaeckBeziers.dll.', mtError, [mbOK], 0);
    end;
  end
  else
  begin
    MessageDlg('The NilsHaeckBeziers.dll is not found.', mtError, [mbOK], 0);
    Application.Terminate;
  end;
end; 

initialization
  LoadNilsHaeckBeziersFuntions;

finalization
  NilsHaeckClosestPointOnBezier  := nil;
  NilsHaeckIsPointOnBezier       := nil;
  NilsHaeckSplitBezierWithFactor := nil;
  NilsHaeckBezierToWindowsFormat := nil;

  BezierLibHandle := 0;


end.
