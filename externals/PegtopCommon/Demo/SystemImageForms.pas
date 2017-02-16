unit SystemImageForms;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ImgList, PegtopSystemImages, ComCtrls, CommCtrl, AppEvnts;

type
  TSystemImageForm = class(TForm)
    ListView1: TListView;
    PegtopSystemImages1: TPegtopSystemImages;
    ListView2: TListView;
    ApplicationEvents1: TApplicationEvents;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
  private
    Extensions: TStringList;
    ExtIndex: Integer;
  public
    { Public declarations }
  end;

var
  SystemImageForm: TSystemImageForm;

implementation

{$R *.DFM}

procedure TSystemImageForm.FormCreate(Sender: TObject);
begin
  // first of all we need a list of all registered file extensions:
  Extensions := TStringList.Create;
  PegtopSystemImages1.ReadFileExtensions(Extensions);
  ExtIndex := 0;
end;

procedure TSystemImageForm.FormShow(Sender: TObject);
var
  Item: TListItem;
  Folder: TPegtopSpecialFolder;
begin
  // secondly we populate the list with the special folders:
  ListView1.Items.BeginUpdate;
  try
    for Folder := Low(TPegtopSpecialFolder) to High(TPegtopSpecialFolder) do begin
      Item := ListView1.Items.Add;
      Item.Caption := PegtopSystemImages1.GetSpecialDisplayName(Folder);
      Item.ImageIndex := PegtopSystemImages1.GetSpecialIconIndex(Folder);
    end;
  finally
    ListView1.Items.EndUpdate;
  end;
end;

procedure TSystemImageForm.FormDestroy(Sender: TObject);
begin
  Extensions.Free;
end;

procedure TSystemImageForm.ApplicationEvents1Idle(Sender: TObject;
  var Done: Boolean);
var
  Item: TListItem;
  I: Integer;
begin
  if ExtIndex < Extensions.Count then begin
    // if app is idle and we still have extensions left...
    I := 0;
    ListView2.Items.BeginUpdate;
    try
      // add (up to) 40 items:
      while (ExtIndex < Extensions.Count) and (I < 40) do begin
        Item := ListView2.Items.Add;
        Item.Caption := PegtopSystemImages1.GetExtTypeName(Extensions[ExtIndex]);
        Item.SubItems.Add(Extensions[ExtIndex]);
        Item.ImageIndex := PegtopSystemImages1.GetExtIconIndex(Extensions[ExtIndex] );
        Inc(ExtIndex);
        Inc(I);
      end;
    finally
      ListView2.Items.EndUpdate;
    end;
  end;
  Done := ExtIndex >= Extensions.Count;
end;

end.
