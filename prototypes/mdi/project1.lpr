program project1;

{$mode objfpc}{$H+}
{$ifdef mswindows}{$apptype gui}{$endif}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, lq_base, lq_main, lq_form, lq_mdi, frm_child,
  lq_menu, lq_panel;

type

	TMainForm = class(TlqForm)
	private
		{@VFD_HEAD_BEGIN: MainForm}
		MainBar: TlqMenuBar;
		MDIWorkArea: TlqMDIWorkArea;
		Bevel1: TlqBevel;
		pmChildren: TlqPopupMenu;
		{@VFD_HEAD_END: MainForm}
		procedure NewFormClicked(Sender: TObject);
		procedure miQuitClicked(Sender: TObject);
	public
		procedure AfterCreate; override;
	end;

{@VFD_NEWFORM_DECL}



{@VFD_NEWFORM_IMPL}

procedure TMainForm.NewFormClicked(Sender: TObject);
begin
	ChildForm := MDIWorkArea.AddWindow(TChildForm) as TChildForm;
	ChildForm.WindowTitle := Format('Child %d', [MDIWorkArea.ChildWindowCount]);
end;

procedure TMainForm.miQuitClicked(Sender: TObject);
begin
	Close;
end;

procedure TMainForm.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: MainForm}
	Name := 'MainForm';
	SetPosition(351, 159, 555, 321);
	WindowTitle := 'LiteKit''s MDI Demo';
	Hint := '';

	MainBar := TlqMenuBar.Create(self);
	with MainBar do
	begin
		Name := 'MainBar';
		SetPosition(0, 0, 555, 24);
		Anchors := [anLeft,anRight,anTop];
	end;

	MDIWorkArea := TlqMDIWorkArea.Create(self);
	with MDIWorkArea do
	begin
		Name := 'MDIWorkArea';
		SetPosition(3, 32, 548, 264);
		Anchors := [anLeft,anRight,anTop,anBottom];
	end;

	Bevel1 := TlqBevel.Create(self);
	with Bevel1 do
	begin
		Name := 'Bevel1';
		SetPosition(0, 300, 555, 20);
		Anchors := [anLeft,anRight,anBottom];
		Hint := '';
		Style := bsLowered;
	end;

	pmChildren := TlqPopupMenu.Create(self);
	with pmChildren do
	begin
		Name := 'pmChildren';
		SetPosition(336, 88, 120, 20);
		AddMenuItem('Add child', '', @NewFormClicked);
		AddMenuItem('-', '', nil);
		AddMenuItem('Quit', '', @miQuitClicked);
	end;

	{@VFD_BODY_END: MainForm}
  {%endregion}
  MainBar.AddMenuItem('Children', nil).SubMenu := pmChildren;
end;


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

