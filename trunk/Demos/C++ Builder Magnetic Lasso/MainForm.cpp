//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "MainForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TfrmMain *frmMain;
//---------------------------------------------------------------------------
__fastcall TfrmMain::TfrmMain(TComponent* Owner)
        : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfrmMain::FormCreate(TObject *Sender)
{
  FIscissorsTool = NULL;
  FDrawing       = False;
}
//---------------------------------------------------------------------------


void __fastcall TfrmMain::FormDestroy(TObject *Sender)
{
  if (FIscissorsTool != NULL)
    FIscissorsTool->Free();        
}
//---------------------------------------------------------------------------


void __fastcall TfrmMain::OpenImageClick(TObject *Sender)
{
  if (this->OpenPictureDialog1->Execute())
  {
    this->imgWorkArea->Picture->Bitmap->LoadFromFile(this->OpenPictureDialog1->FileName);
    this->imgWorkArea->Picture->Bitmap->PixelFormat = pf24bit;

    if (FIscissorsTool != NULL)
      delete FIscissorsTool;

    FIscissorsTool = new TGimpIscissorsTool(this->imgWorkArea->Picture->Bitmap,
                                            this->imgWorkArea->Canvas);

    FIscissorsTool->IsInteractive = this->chckbxInteractive->Checked;
  }
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::imgWorkAreaMouseDown(TObject *Sender,
      TMouseButton Button, TShiftState Shift, int X, int Y)
{
  if (Button == mbLeft)
  {
    if (this->FIscissorsTool != NULL)
    {
      this->FIscissorsTool->button_press(Sender, Button, Shift, X, Y);
    }

    FDrawing = True;
  }
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::imgWorkAreaMouseUp(TObject *Sender,
      TMouseButton Button, TShiftState Shift, int X, int Y)
{
  if (FDrawing)
  {
    FDrawing = False;
    
    if (this->FIscissorsTool != NULL)
    {
      this->FIscissorsTool->button_release(Sender, Button, Shift, X, Y);

      if (this->FIscissorsTool->IsConnected)
        this->imgMask->Picture->Bitmap->Assign(this->FIscissorsTool->Mask);
    }
  }
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::imgWorkAreaMouseMove(TObject *Sender,
      TShiftState Shift, int X, int Y)
{
  if (this->FIscissorsTool != NULL)
  {
    if (FDrawing)
    {
      this->FIscissorsTool->motion(Sender, Shift, X, Y);
    }

    this->StatusBar1->SimpleText = Format( "X = %d Y = %d", ARRAYOFCONST((X, Y)) );
  }
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::chckbxInteractiveClick(TObject *Sender)
{
  if (this->FIscissorsTool != NULL)
  {
    this->FIscissorsTool->IsInteractive = this->chckbxInteractive->Checked;
  }
}
//---------------------------------------------------------------------------

