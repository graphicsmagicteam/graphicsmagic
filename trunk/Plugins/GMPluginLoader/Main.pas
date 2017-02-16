{ This program is for testing how to load/run the .gmp plugins which
  specifically designed for GraphicsMagic.

  CopyRight(C) 2001-2008, Ma Xiaoguang & Ma Xiaoming < gmbros@hotmail.com >.
  All rights reserved. }

unit Main;

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1/LGPL 2.1/GPL 2.0
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Initial Developer of this unit are
 *
 * Ma Xiaoguang and Ma Xiaoming < gmbros@hotmail.com >
 *
 * Contributor(s):
 *
 * Alternatively, the contents of this file may be used under the terms of
 * either the GNU General Public License Version 2 or later (the "GPL"), or
 * the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
 * in which case the provisions of the GPL or the LGPL are applicable instead
 * of those above. If you wish to allow use of your version of this file only
 * under the terms of either the GPL or the LGPL, and not to allow others to
 * use your version of this file under the terms of the MPL, indicate your
 * decision by deleting the provisions above and replace them with the notice
 * and other provisions required by the GPL or the LGPL. If you do not delete
 * the provisions above, a recipient may use your version of this file under
 * the terms of any one of the MPL, the GPL or the LGPL.
 * ***** END LICENSE BLOCK ***** *)

{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ExtDlgs, StdCtrls, CheckLst, ExtCtrls, GR32, GR32_Image,
  GR32_Layers, gmPluginManager;

type
  TfrmMain = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    mnitmOpen: TMenuItem;
    mnitmExit: TMenuItem;
    ScrollBox1: TScrollBox;
    imgView: TImage32;
    mnitmFilters: TMenuItem;
    OpenPictureDialog: TOpenPictureDialog;
    Panel1: TPanel;
    Splitter1: TSplitter;
    CheckListBox1: TCheckListBox;
    ListBox1: TListBox;
    ListBox2: TListBox;
    Label1: TLabel;
    Label2: TLabel;
    procedure mnitmExitClick(Sender: TObject);
    procedure mnitmOpenClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FSourceBmp, FDestBmp : TBitmap32;
    FGMPluginInfoList    : TgmPluginInfoList;
    FCategoryMenuItemList: TgmCategoryMenuItemList;

    procedure ExecuteFilters(Sender: TObject);
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses
{ externals }
  GR32_Add_BlendModes,
{ GraphicsMagic Lib }
  gmIO;               // LoadGraphicsFile(), SaveGraphicsFile()

{$R *.dfm}

procedure UpdateView;
var
  BmpLayer: TBitmapLayer;
begin
  BmpLayer := TBitmapLayer(frmMain.imgView.Layers[0]);

  // Do everything we want at here...

  BmpLayer.Bitmap.Changed;
end; 

procedure TfrmMain.ExecuteFilters(Sender: TObject);
var
  PluginInfo: TgmPluginInfo;
  pm        : TgmPlugin;
  P         : PColor32;
  BmpLayer  : TBitmapLayer;
begin
  PluginInfo := FGMPluginInfoList.GetPluginInfoByMenuItem(Sender);

  if PluginInfo <> nil then
  begin
    pm := TgmPlugin.Create(PluginInfo.LibFileName);
    try
      BmpLayer := TBitmapLayer(imgView.Layers[0]);
      P        := @BmpLayer.Bitmap.Bits[0];
      
      pm.RunPlugin(P, BmpLayer.Bitmap.Width, BmpLayer.Bitmap.Height, @UpdateView);
    finally
      pm.Free;
    end;
  end;
end; 

procedure TfrmMain.mnitmExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.mnitmOpenClick(Sender: TObject);
var
  ALayer: TBitmapLayer;
  P     : TPoint;
  Bmp   : TBitmap32;
  W, H  : Double;
begin
  if OpenPictureDialog.Execute then
  begin
    Bmp := LoadGraphicsFile(OpenPictureDialog.FileName);

    if Bmp = nil then
    begin
      MessageDlg('Cannot load picture ''' + ExtractFileName(OpenPictureDialog.FileName) + '''', mtError, [mbOK], 0);
    end
    else
    begin
      try
        FSourceBmp.Assign(Bmp);

        imgView.Bitmap.SetSize(FSourceBmp.Width, FSourceBmp.Height);
        imgView.Bitmap.Clear(clRed32);

        if imgView.Layers.Count > 0 then
        begin
          ALayer := TBitmapLayer(imgView.Layers[0]);
        end
        else
        begin
          ALayer := TBitmapLayer.Create(imgView.Layers);
        end;

        ALayer.Bitmap.Assign(FSourceBmp);

        ALayer.Bitmap.DrawMode       := dmCustom;
        ALayer.Bitmap.OnPixelCombine := BlendMode.NormalBlend;
        ALayer.Scaled                := True;

        with imgView.GetViewportRect do
        begin
          P := imgView.ControlToBitmap( Point((Right + Left) div 2, (Bottom + Top) div 2) );
        end;

        W := FSourceBmp.Width  / 2;
        H := FSourceBmp.Height / 2;

        ALayer.Location := FloatRect(P.X - W, P.Y - H, P.X + W, P.Y + H);
      finally
        Bmp.Free;
      end;
    end;
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  APath            : string;
  AStringList      : TStringList;
  ACategoryList    : TgmUniqueStringList;
  APluginNameList  : TgmUniqueStringList;
  i, j             : Integer;
  GMPlugin         : TgmPlugin;
  CategoryName     : string;
  PluginName       : string;
  PluginInfo       : TgmPluginInfo;
  ACategoryMenuItem: TMenuItem;
begin
  FSourceBmp          := TBitmap32.Create;
  FSourceBmp.DrawMode := dmBlend;

  FDestBmp          := TBitmap32.Create;
  FDestBmp.DrawMode := dmBlend;

  FGMPluginInfoList     := TgmPluginInfoList.Create;
  FCategoryMenuItemList := TgmCategoryMenuItemList.Create;

  AStringList := TStringList.Create;
  APath       := ExtractFilePath( ParamStr(0) ) + 'Plug-Ins';

  CheckListBox1.Clear;
  ZarkoFileSearch(APath, '*.gmp', False, AStringList);
  AStringList.Sort;
  
  CheckListBox1.Items := AStringList;
  CheckListBox1.Update;

  if AStringList.Count > 0 then
  begin
    for i := 0 to AStringList.Count - 1 do
    begin
      GMPlugin := TgmPlugin.Create(AStringList.Strings[i]);
      try
        CategoryName := GMPlugin.GetPluginCategory;
        PluginName   := GMPlugin.GetPluginName;

        PluginInfo              := TgmPluginInfo.Create;
        PluginInfo.LibFileName  := AStringList.Strings[i];
        PluginInfo.CategoryName := CategoryName;
        PluginInfo.PluginName   := PluginName;
        PluginInfo.IsAdjustable := GMPlugin.IsAdjustablePlugin;

        FGMPluginInfoList.Add(PluginInfo);
      finally
        GMPlugin.Free;
      end;
    end;

    for i := 0 to FGMPluginInfoList.Count - 1 do
    begin
      PluginInfo := TgmPluginInfo(FGMPluginInfoList.Items[i]);

      ListBox2.Items.Add(PluginInfo.PluginName);
      ListBox2.Items.Add(PluginInfo.CategoryName);
    end;

    ACategoryList := FGMPluginInfoList.GetSortedCategoryList;
    
    if ACategoryList <> nil then
    begin
      for i := 0 to ACategoryList.Count - 1 do
      begin
        ACategoryMenuItem         := TMenuItem.Create(Self);
        CategoryName              := ACategoryList.Strings[i];
        ACategoryMenuItem.Caption := CategoryName;
        ACategoryMenuItem.Visible := True;

        mnitmFilters.Add(ACategoryMenuItem);
        FCategoryMenuItemList.Add(ACategoryMenuItem);

        APluginNameList := FGMPluginInfoList.GetSortedPluginNameList(CategoryName);

        ListBox2.Items.Add(APluginNameList.Strings[0]);

        if APluginNameList <> nil then
        begin
          for j := 0 to APluginNameList.Count - 1 do
          begin
            PluginName := APluginNameList.Strings[j];
            PluginInfo := FGMPluginInfoList.GetPluginInfoByName(PluginName);

            if PluginInfo <> nil then
            begin
              PluginInfo.PluginMenu := TMenuItem.Create(Self);

              if PluginInfo.IsAdjustable then
              begin
                PluginInfo.PluginMenu.Caption := PluginName + '...';
              end
              else
              begin
                PluginInfo.PluginMenu.Caption := PluginName;
              end;

              PluginInfo.PluginMenu.OnClick := ExecuteFilters;
              ACategoryMenuItem.Add(PluginInfo.PluginMenu);
            end;
          end;
          
          APluginNameList.Clear;
          APluginNameList.Free;
        end;
      end;

      ListBox1.Items := ACategoryList;
      ListBox1.Update;
    end;
  end;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FSourceBmp.Free;
  FDestBmp.Free;
  FGMPluginInfoList.Free;
  FCategoryMenuItemList.Free;
end; 

end.
