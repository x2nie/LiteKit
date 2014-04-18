program panel_test;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  lq_main,
  MainForm;

procedure MainProc;
var
  frmMain: TfrmMain;
begin
  lqApplication.Initialize;
  frmMain:= TfrmMain.Create(nil);
  try
    frmMain.Show;
    lqApplication.Run;
  finally
    frmMain.Free;
  end;
end;

begin
  MainProc;
end.


