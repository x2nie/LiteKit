{
    LiteKit IDE - Maximus

    Copyright (C) 2012 - 2013 Graeme Geldenhuys

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing LiteKit.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      ---
}

program maximus;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, lq_base, lq_main, frm_main, frm_configureide, ideconst, idemacros,
  frm_debug, project, unitlist, frm_projectoptions, ideutils, builderthread,
  ideimages, stringhelpers, frm_procedurelist, mPasLex, filemonitor, SynRegExpr,
  lq_textedit, frm_find, Sha1;


procedure MainProc;
var
  frm: TMainForm;
begin
//  LQ_DEFAULT_FONT_DESC := 'DejaVu Sans-9';
  lqApplication.Initialize;
  RegisterIDEImages;
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

