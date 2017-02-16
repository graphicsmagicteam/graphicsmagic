unit gmGradient_Reg;

interface

procedure Register;

implementation

uses
{ Standard }
  SysUtils, Classes,
{ GraphicsMagic }
  gmGradient_List,
  gmGradientsGrid,
  gmGradient_FileDlgs,
  gmGradientEditor,
  gmGradient_Preview,
  gmGradient_ListView;

{.$R *.dcr}

procedure Register;
begin
  RegisterComponents('Graphics Magic', [TgmGradientList]);
  RegisterComponents('Graphics Magic', [TgmGradientsGrid]);
  RegisterComponents('Graphics Magic', [TOpenGradientDialog,TSaveGradientDialog]);
  RegisterComponents('Graphics Magic', [TgmGradientEditor]);
  RegisterComponents('Graphics Magic', [TgmGradientPreview]);
  RegisterComponents('Graphics Magic', [TgmGradientListView]);
end;

end.
