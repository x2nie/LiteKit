program nanoedit;

{$mode objfpc}{$H+}
{$ifdef mswindows} {$apptype gui} {$endif}

uses
  Classes, lq_main, mainfrm;


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

