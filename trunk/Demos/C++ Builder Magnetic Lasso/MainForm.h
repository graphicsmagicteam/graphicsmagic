//---------------------------------------------------------------------------

#ifndef MainFormH
#define MainFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>

#include "cbiscissorstool.h"
#include <ExtCtrls.hpp>
#include <Menus.hpp>
#include <Dialogs.hpp>
#include <ExtDlgs.hpp>
#include <ComCtrls.hpp>
//---------------------------------------------------------------------------
class TfrmMain : public TForm
{
__published:	// IDE-managed Components
        TMainMenu *MainMenu1;
        TMenuItem *File1;
        TMenuItem *mnitmOpen;
        TScrollBox *ScrollBox1;
        TImage *imgWorkArea;
        TOpenPictureDialog *OpenPictureDialog1;
        TSplitter *Splitter1;
        TScrollBox *ScrollBox2;
        TImage *imgMask;
        TPanel *Panel1;
        TPanel *Panel2;
        TStatusBar *StatusBar1;
        TCheckBox *chckbxInteractive;
        void __fastcall FormCreate(TObject *Sender);
        void __fastcall FormDestroy(TObject *Sender);
        void __fastcall OpenImageClick(TObject *Sender);
        void __fastcall imgWorkAreaMouseDown(TObject *Sender,
          TMouseButton Button, TShiftState Shift, int X, int Y);
        void __fastcall imgWorkAreaMouseUp(TObject *Sender,
          TMouseButton Button, TShiftState Shift, int X, int Y);
        void __fastcall imgWorkAreaMouseMove(TObject *Sender,
          TShiftState Shift, int X, int Y);
        void __fastcall chckbxInteractiveClick(TObject *Sender);
private:	// User declarations
        TGimpIscissorsTool *FIscissorsTool;
        bool                FDrawing;
public:		// User declarations
        __fastcall TfrmMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmMain *frmMain;
//---------------------------------------------------------------------------
#endif
