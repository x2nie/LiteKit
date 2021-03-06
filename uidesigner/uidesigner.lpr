{
    LiteKit  -  Free Pascal GUI Library

    Copyright (C) 2006 - 2010 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing LiteKit.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      The starting unit for the UI Designer project.
}

program uidesigner;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, lq_base, lq_main, vfdmain, vfdresizer, vfdforms,
  vfdfile, newformdesigner, vfdwidgets, vfdformparser, vfdeditors,
  vfdwidgetclass, vfdutils, vfdprops, vfddesigner, vfdpropeditgrid;


procedure MainProc;
begin
  lqApplication.Initialize;
  try
    RegisterWidgets;
    PropList := TPropertyList.Create;
    maindsgn := TMainDesigner.Create;
    maindsgn.CreateWindows;

    // Note:  This needs improving!!
    lqApplication.MainForm := frmMain;

    { If file passed in as param, load it! }
    maindsgn.EditedFileName := ParamStr(1);
    if FileExists(maindsgn.EditedFileName) then
      maindsgn.OnLoadFile(maindsgn);

    lqApplication.Run;
    
    PropList.Free;
    
  finally
    maindsgn.Free;
  end;
end;

begin
  MainProc;
end.


