program tabsheet_design;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, lqfx, tabtest;


procedure MainProc;
var
  frm: TfrmTabTest;
begin
  lqApplication.Initialize;
  frm := TfrmTabTest.Create(nil);
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


