program dicomviewer;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  interfaces, Classes, main, fpg_main, fpg_img_utils, commonmessages,
  viewerpanel, remoteloader, layoutdefinition, viewercanvas, paperprint,
  printer4lazarus, bitmapprint, positions;

procedure MainProc;
var
  frm: TMainForm;
begin
  fpgApplication.Initialize;
  frm := TMainForm.Create(nil);
  try
    frm.Show;
    fpgApplication.Run;
  finally
    frm.Free;
  end;
end;

begin
  MainProc;
end.

