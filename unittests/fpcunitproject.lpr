program fpcunitproject;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  lq_base, lq_main, lq_guitestrunner, tcTreeview;

procedure MainProc;

var
  frm: TGUITestRunnerForm;

begin
  lqApplication.Initialize;
  frm := TGUITestRunnerForm.Create(nil);
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
