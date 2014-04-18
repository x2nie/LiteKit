program jpeg;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, lq_main, main;

procedure MainProc;
begin
  lqApplication.Initialize;
  frmMain := TfrmMain.Create(nil);
  frmMain.Show;
  lqApplication.Run;
  frmMain.Free;
end;

begin
  MainProc;
end.


