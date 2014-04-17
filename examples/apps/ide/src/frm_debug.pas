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

unit frm_debug;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, lq_base, lq_main, lq_form, lq_tab, lq_grid;

type

  TDebugForm = class(TlqForm)
  private
    {@VFD_HEAD_BEGIN: DebugForm}
    PageControl1: TlqPageControl;
    TabSheet1: TlqTabSheet;
    TabSheet2: TlqTabSheet;
    TabSheet3: TlqTabSheet;
    TabSheet4: TlqTabSheet;
    TabSheet5: TlqTabSheet;
    TabSheet6: TlqTabSheet;
    TabSheet7: TlqTabSheet;
    Grid1: TlqStringGrid;
    Grid2: TlqStringGrid;
    Grid3: TlqStringGrid;
    {@VFD_HEAD_END: DebugForm}
  public
    procedure AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

var
  DebugForm: TDebugForm;

implementation

{@VFD_NEWFORM_IMPL}

procedure TDebugForm.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: DebugForm}
  Name := 'DebugForm';
  SetPosition(690, 193, 512, 247);
  WindowTitle := 'Debug Window';
  Hint := '';

  PageControl1 := TlqPageControl.Create(self);
  with PageControl1 do
  begin
    Name := 'PageControl1';
    SetPosition(4, 4, 506, 240);
    ActivePageIndex := 0;
    Hint := '';
    TabOrder := 0;
  end;

  TabSheet1 := TlqTabSheet.Create(PageControl1);
  with TabSheet1 do
  begin
    Name := 'TabSheet1';
    SetPosition(3, 24, 500, 213);
    Text := 'Watches';
  end;

  TabSheet2 := TlqTabSheet.Create(PageControl1);
  with TabSheet2 do
  begin
    Name := 'TabSheet2';
    SetPosition(3, 24, 500, 213);
    Text := 'BreakPoints';
  end;

  TabSheet3 := TlqTabSheet.Create(PageControl1);
  with TabSheet3 do
  begin
    Name := 'TabSheet3';
    SetPosition(3, 24, 500, 213);
    Text := 'Local Vars';
  end;

  TabSheet4 := TlqTabSheet.Create(PageControl1);
  with TabSheet4 do
  begin
    Name := 'TabSheet4';
    SetPosition(3, 24, 500, 213);
    Text := 'Call Stack';
  end;

  TabSheet5 := TlqTabSheet.Create(PageControl1);
  with TabSheet5 do
  begin
    Name := 'TabSheet5';
    SetPosition(3, 24, 500, 213);
    Text := 'Registers';
  end;

  TabSheet6 := TlqTabSheet.Create(PageControl1);
  with TabSheet6 do
  begin
    Name := 'TabSheet6';
    SetPosition(3, 24, 500, 213);
    Text := 'Asm';
  end;

  TabSheet7 := TlqTabSheet.Create(PageControl1);
  with TabSheet7 do
  begin
    Name := 'TabSheet7';
    SetPosition(3, 24, 500, 213);
    Text := 'GDB output';
  end;

  Grid1 := TlqStringGrid.Create(TabSheet1);
  with Grid1 do
  begin
    Name := 'Grid1';
    SetPosition(0, 4, 496, 204);
    AddColumn('Expression', 100, taLeftJustify);
    AddColumn('Value', 350, taLeftJustify);
    FontDesc := '#Grid';
    HeaderFontDesc := '#GridHeader';
    Hint := '';
    RowCount := 5;
    RowSelect := False;
    TabOrder := 0;
  end;

  Grid2 := TlqStringGrid.Create(TabSheet2);
  with Grid2 do
  begin
    Name := 'Grid2';
    SetPosition(0, 4, 496, 204);
    AddColumn('State', 50, taLeftJustify);
    AddColumn('Filename/Addres', 120, taLeftJustify);
    AddColumn('Line/Length', 85, taLeftJustify);
    AddColumn('Condition', 70, taLeftJustify);
    AddColumn('Action', 50, taLeftJustify);
    AddColumn('Count', 50, taLeftJustify);
    AddColumn('Group', 80, taLeftJustify);
    FontDesc := '#Grid';
    HeaderFontDesc := '#GridHeader';
    Hint := '';
    RowCount := 5;
    RowSelect := False;
    TabOrder := 0;
  end;

  Grid3 := TlqStringGrid.Create(TabSheet3);
  with Grid3 do
  begin
    Name := 'Grid3';
    SetPosition(0, 4, 496, 204);
    AddColumn('Name', 150, taLeftJustify);
    AddColumn('Value', 250, taLeftJustify);
    FontDesc := '#Grid';
    HeaderFontDesc := '#GridHeader';
    Hint := '';
    RowCount := 5;
    RowSelect := False;
    TabOrder := 0;
  end;

  {@VFD_BODY_END: DebugForm}
  {%endregion}
end;


end.
