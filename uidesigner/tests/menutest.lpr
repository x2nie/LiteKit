program menutest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, frm_menutest, lqfx;



procedure MainProc;
var
  frm: TfrmMain;
begin
  lqApplication.Initialize;
  frm := TfrmMain.Create(nil);
  frm.Show;
  lqApplication.Run;
end;

begin
  MainProc;
end.


