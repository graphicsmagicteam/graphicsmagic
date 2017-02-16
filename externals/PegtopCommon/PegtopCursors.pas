unit PegtopCursors;

interface

const
  crCircle = 5;
  crMagnify = 6;
  crInvisible = 7;
  crScrollH = 8;

implementation

{$R PegtopCursors.res}

uses
  Windows, Forms;

initialization
  Screen.Cursors[crCircle] := LoadCursor(HInstance,'PEGTOPCIRCLECURSOR');
  Screen.Cursors[crMagnify] := LoadCursor(HInstance,'PEGTOPMAGNIFYCURSOR');
  Screen.Cursors[crInvisible] := LoadCursor(HInstance,'PEGTOPINVISIBLECURSOR');
  Screen.Cursors[crScrollH] := LoadCursor(HInstance,'PEGTOPSCROLLHCURSOR');
end.
