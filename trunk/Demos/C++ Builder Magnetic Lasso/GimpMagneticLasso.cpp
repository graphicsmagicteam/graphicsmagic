//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
USERES("GimpMagneticLasso.res");
USEFORM("MainForm.cpp", frmMain);
USEUNIT("Libraries\cbtempbuf.cpp");
USEUNIT("Libraries\cbgarray.cpp");
USEUNIT("Libraries\cbgimpdrawtool.cpp");
USEUNIT("Libraries\cbglist.cpp");
USEUNIT("Libraries\cbgqueue.cpp");
USEUNIT("Libraries\cbiscissorstool.cpp");
USEUNIT("Libraries\cbpaintfuncs.cpp");
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
        try
        {
                 Application->Initialize();
                 Application->CreateForm(__classid(TfrmMain), &frmMain);
                 Application->Run();
        }
        catch (Exception &exception)
        {
                 Application->ShowException(&exception);
        }
        return 0;
}
//---------------------------------------------------------------------------
