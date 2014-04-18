program imgcnv;

{$mode objfpc}{$H+}
{$ifdef mswindows}{$apptype gui}{$endif}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, lq_base, lq_main, lqui_toolkit, frm_main;


procedure MainProc;
var
  frm: TMainForm;
begin
  lqApplication.Initialize;
  frm := TMainForm.Create(nil);
  try
    frm.Show;
    lqApplication.Run;
  finally
    frm.Free;
  end;
end;

begin
  MainProc;
end.


