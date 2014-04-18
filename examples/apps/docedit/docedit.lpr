program docedit;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  lq_main,
  frm_main, frm_options, model, FPDEUtil, doceditmsg, doceditopts;


procedure MainProc;
var
  frm: TMainForm;
begin
  lqApplication.Initialize;
  try
    frm := TMainForm.Create(nil);
    frm.Show;
    lqApplication.Run;
  finally
    frm.Free;
  end;
end;

begin
  MainProc;
end.


