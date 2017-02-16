////////////////////////////////////////////////////////////////////////////////
// File:       PegtopGraphicFileDialogs.pas
// Classes:    TPegtopGraphicOpenDialog, TPegtopGraphicSaveDialog
// Version:    1.04
// Date:       31 Oct 2004 1.00
//             13 Mar 2005 1.01 (default filter added)
//             17 Mar 2005 1.02 (TPegtopThumbnail instead of TPaintBox)
//             15 Aug 2005 1.03 (number of megapixels displayed,
//                              bugfix: correct size displayed for JPeg images)
//             30 Sep 2005 1.04 (graphic progress support added)
//             01 Nov 2005 1.05 (special button / result added)
// Author:     Jens Gruschel (GRU)
// Copyright:  (c) 2004, 2005 Jens Gruschel
// Website:    http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Description:
// TPegtopGraphicOpenDialog and TPegtopGraphicSaveDialog are file dialogs for
// selecting graphic files. Much like Delphi's TPictureOpen/SaveDialog (which I
// don't like for some reasons, especially because square bitmaps are not
// displayed properly). Supports 2x and 4x antialiasing and fast jpeg decoding.
////////////////////////////////////////////////////////////////////////////////
// License:
// Please read the license.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Documentation / Installation:
// Please read the readme.txt file that comes with this software
// and visit http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////

unit PegtopGraphicFileDialogs;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Dialogs, Forms,
  StdCtrls, ExtCtrls, PegtopFileDialogs, PegtopThumbnails;

type
  TPegtopModalResultEvent = procedure (Sender: TObject; var ModalResult: TModalResult) of object;

  TPegtopGraphicOpenDialog = class(TPegtopCustomExtendedFileDialog)
  private
    FThumbnail: TPegtopThumbnail;
    FInfoLabel: TLabel;
    FSpecialButton: TButton;
    FSpecialResult: TModalResult;
    FOnSpecialClick: TPegtopModalResultEvent;
    FProgressPercent: Byte;
    procedure GraphicProgress(Sender: TObject; Stage: TProgressStage; PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string);
    function GetGraphicText(Graphic: TGraphic): String;
    procedure SpecialButtonClick(Sender: TObject);
    function GetPreviewQuality: TPegtopThumbnailQuality;
    procedure SetPreviewQuality(Value: TPegtopThumbnailQuality);
    function IsFilterStored: Boolean;
  protected
    procedure DoShow; override;
    procedure DoClose; override;
    procedure DoAlign; override;
    procedure DoSelectionChange; override;
  public
    constructor Create(AOwner: TComponent); override;
    function Execute: Boolean; override;
    function ExecuteSpecial(ButtonCaption: String): TModalResult;
  published
    property PreviewQuality: TPegtopThumbnailQuality read GetPreviewQuality write SetPreviewQuality;
    property ExtendedSize;
    property ExtendedAlignment;
    property Filter stored IsFilterStored;
    property OnSpecialClick: TPegtopModalResultEvent read FOnSpecialClick write FOnSpecialClick;
  end;

  TPegtopGraphicSaveDialog = class(TPegtopGraphicOpenDialog)
  protected
    function IsSaveDialog: Boolean; override;
  published
    property AutoAdjustExtension;
  end;

implementation

////////////////////////////////////////////////////////////////////////////////
// TPegtopGraphicOpenDialog
////////////////////////////////////////////////////////////////////////////////

constructor TPegtopGraphicOpenDialog.Create(AOwner: TComponent);
begin
  inherited;
  FInfoLabel := TLabel.Create(Self);
  FInfoLabel.AutoSize := False;
  FInfoLabel.Alignment := taCenter;
  FInfoLabel.Layout := tlCenter;
  FInfoLabel.Transparent := True;
  FInfoLabel.Parent := ExtendedContainer;
  FThumbnail := TPegtopThumbnail.Create(Self);
  FThumbnail.Parent := ExtendedContainer;
  FSpecialButton := TButton.Create(Self);
  FSpecialButton.Parent := ExtendedContainer;
  FSpecialButton.OnClick := SpecialButtonClick;
  ExtendedSize := 160;
  Filter := GraphicFilter(TGraphic);
end;

function TPegtopGraphicOpenDialog.Execute: Boolean;
begin
  FSpecialButton.Visible := False;
  Result := inherited Execute;
end;

function TPegtopGraphicOpenDialog.ExecuteSpecial(ButtonCaption: String): TModalResult;
begin
  FSpecialButton.Visible := True;
  FSpecialButton.Caption := ButtonCaption;
  FSpecialResult := mrCancel;
  if inherited Execute then Result := mrOk
  else Result := FSpecialResult;
end;

procedure TPegtopGraphicOpenDialog.DoShow;
begin
  FThumbnail.Graphic := NIL;
  inherited;
end;

procedure TPegtopGraphicOpenDialog.DoClose;
begin
  FThumbnail.Graphic := NIL;
  inherited;
end;

procedure TPegtopGraphicOpenDialog.DoAlign;
var
  R: TRect;
begin
  FInfoLabel.SetBounds(0, 0, ExtendedContainer.Width, 24);
  FThumbnail.SetBounds(0, 24, ExtendedContainer.Width, ExtendedContainer.Height - 24);
  if FSpecialButton.Visible then begin
    R := ItemRects[pfiCancelButton];
    FSpecialButton.SetBounds(R.Left - ExtendedContainer.Left, 0, R.Right - R.Left, R.Bottom - R.Top);
  end;
  inherited;
end;

procedure TPegtopGraphicOpenDialog.GraphicProgress(Sender: TObject; Stage: TProgressStage; PercentDone: Byte; RedrawNow: Boolean; const R: TRect; const Msg: string);
begin
  if (Sender is TGraphic) and (Stage = psRunning) and RedrawNow and (PercentDone >= FProgressPercent + 10) then begin
    FProgressPercent := PercentDone;
    FInfoLabel.Caption := GetGraphicText(TGraphic(Sender));
    FThumbnail.Graphic := TGraphic(Sender);
    FInfoLabel.Update;
    FThumbnail.Update;
  end;
end;

function TPegtopGraphicOpenDialog.GetGraphicText(Graphic: TGraphic): String;
begin
  Result := IntToStr(Graphic.Width) + ' x ' + IntToStr(Graphic.Height)
        + ' = ' + FloatToStrF(Graphic.Width * Graphic.Height * 0.000001, ffFixed, 15, 2) + ' MP';
end;

procedure TPegtopGraphicOpenDialog.DoSelectionChange;
var
  Valid: Boolean;
  Picture: TPicture;
begin
  Valid := FileExists(FileName) and (GetFileAttributes(PChar(FileName)) <> $FFFFFFFF);
  if Valid then begin
    try
      Picture := TPicture.Create;
      try
        FProgressPercent := 0;
        Picture.OnProgress := GraphicProgress;
        Picture.LoadFromFile(FileName);
        FInfoLabel.Caption := GetGraphicText(Picture.Graphic);
        FThumbnail.Graphic := Picture.Graphic;
      finally
        Picture.Free;
      end;
    except
      Valid := False;
    end;
  end;
  if not Valid then
  begin
    FInfoLabel.Caption := '';
    FThumbnail.Graphic := NIL;
  end;
  inherited;
end;

procedure TPegtopGraphicOpenDialog.SpecialButtonClick(Sender: TObject);
begin
  FSpecialResult := mrNone;
  if Assigned(FOnSpecialClick) then FOnSpecialClick(Self, FSpecialResult);
  if FSpecialResult <> mrNone then Close;
end;

function TPegtopGraphicOpenDialog.GetPreviewQuality: TPegtopThumbnailQuality;
begin
  Result := FThumbnail.Quality;
end;

procedure TPegtopGraphicOpenDialog.SetPreviewQuality(Value: TPegtopThumbnailQuality);
begin
  FThumbnail.Quality := Value;
end;

function TPegtopGraphicOpenDialog.IsFilterStored: Boolean;
begin
  Result := Filter <> GraphicFilter(TGraphic);
end;

////////////////////////////////////////////////////////////////////////////////
// TPegtopGraphicSaveDialog
////////////////////////////////////////////////////////////////////////////////

function TPegtopGraphicSaveDialog.IsSaveDialog: Boolean;
begin
  Result := True;
end;

end.
