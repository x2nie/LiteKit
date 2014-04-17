program cursordemo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, lq_base, lq_main, lq_form, lq_panel, lq_label;

type

  TMainForm = class(TlqForm)
  public
    {@VFD_HEAD_BEGIN: MainForm}
    Panel1: TlqPanel;
    Panel2: TlqPanel;
    Panel3: TlqPanel;
    Panel4: TlqPanel;
    Panel5: TlqPanel;
    Panel6: TlqPanel;
    Panel7: TlqPanel;
    Panel8: TlqPanel;
    Panel9: TlqPanel;
    Panel10: TlqPanel;
    Panel11: TlqPanel;
    Panel12: TlqPanel;
    Panel13: TlqPanel;
    Label1: TlqLabel;
    {@VFD_HEAD_END: MainForm}
    procedure AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}



{@VFD_NEWFORM_IMPL}

procedure TMainForm.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(388, 200, 311, 204);
  WindowTitle := 'Mouse Cursor Demo';
  Hint := '';
  WindowPosition := wpOneThirdDown;

  Label1 := TlqLabel.Create(self);
  with Label1 do
  begin
    Name := 'Label1';
    SetPosition(20, 4, 256, 16);
    Alignment := taCenter;
    FontDesc := '#Label2';
    Hint := '';
    Text := 'Available mouse cursors in fpGUI';
  end;

  Panel1 := TlqPanel.Create(self);
  with Panel1 do
  begin
    Name := 'Panel1';
    SetPosition(8, 32, 92, 24);
    FontDesc := '#Grid';
    Hint := '';
    Text := 'mcDefault';
    MouseCursor := mcDefault;
  end;

  Panel2 := TlqPanel.Create(self);
  with Panel2 do
  begin
    Name := 'Panel2';
    SetPosition(108, 32, 92, 24);
    FontDesc := '#Grid';
    Hint := '';
    Text := 'mcArrow';
    MouseCursor := mcArrow;
  end;

  Panel3 := TlqPanel.Create(self);
  with Panel3 do
  begin
    Name := 'Panel3';
    SetPosition(208, 32, 92, 24);
    FontDesc := '#Grid';
    Hint := '';
    Text := 'mcCross';
    MouseCursor := mcCross;
  end;

  Panel4 := TlqPanel.Create(self);
  with Panel4 do
  begin
    Name := 'Panel4';
    SetPosition(8, 64, 92, 24);
    FontDesc := '#Grid';
    Hint := '';
    Text := 'mcIBeam';
    MouseCursor := mcIBeam;
  end;

  Panel5 := TlqPanel.Create(self);
  with Panel5 do
  begin
    Name := 'Panel5';
    SetPosition(108, 64, 92, 24);
    FontDesc := '#Grid';
    Hint := '';
    Text := 'mcSizeEW';
    MouseCursor := mcSizeEW;
  end;

  Panel6 := TlqPanel.Create(self);
  with Panel6 do
  begin
    Name := 'Panel6';
    SetPosition(208, 64, 92, 24);
    FontDesc := '#Grid';
    Hint := '';
    Text := 'mcSizeNS';
    MouseCursor := mcSizeNS;
  end;

  Panel7 := TlqPanel.Create(self);
  with Panel7 do
  begin
    Name := 'Panel7';
    SetPosition(8, 96, 92, 24);
    FontDesc := '#Grid';
    Hint := '';
    Text := 'mcSizeNWSE';
    MouseCursor := mcSizeNWSE;
  end;

  Panel8 := TlqPanel.Create(self);
  with Panel8 do
  begin
    Name := 'Panel8';
    SetPosition(108, 96, 92, 24);
    FontDesc := '#Grid';
    Hint := '';
    Text := 'mcSizeNESW';
    MouseCursor := mcSizeNESW;
  end;

  Panel9 := TlqPanel.Create(self);
  with Panel9 do
  begin
    Name := 'Panel9';
    SetPosition(208, 96, 92, 24);
    FontDesc := '#Grid';
    Hint := '';
    Text := 'mcSizeSWNE';
    MouseCursor := mcSizeSWNE;
  end;

  Panel10 := TlqPanel.Create(self);
  with Panel10 do
  begin
    Name := 'Panel10';
    SetPosition(8, 128, 92, 24);
    FontDesc := '#Grid';
    Hint := '';
    Text := 'mcSizeSENW';
    MouseCursor := mcSizeSENW;
  end;

  Panel11 := TlqPanel.Create(self);
  with Panel11 do
  begin
    Name := 'Panel11';
    SetPosition(108, 128, 92, 24);
    FontDesc := '#Grid';
    Hint := '';
    Text := 'mcMove';
    MouseCursor := mcMove;
  end;

  Panel12 := TlqPanel.Create(self);
  with Panel12 do
  begin
    Name := 'Panel12';
    SetPosition(208, 128, 92, 24);
    FontDesc := '#Grid';
    Hint := '';
    Text := 'mcHourGlass';
    MouseCursor := mcHourGlass;
  end;

  Panel13 := TlqPanel.Create(self);
  with Panel13 do
  begin
    Name := 'Panel13';
    SetPosition(8, 160, 92, 24);
    FontDesc := '#Grid';
    Hint := '';
    Text := 'mcHand';
    MouseCursor := mcHand;
  end;

  {@VFD_BODY_END: MainForm}
  {%endregion}
end;


procedure MainProc;
var
  frm: TMainForm;
begin
  fpgApplication.Initialize;
  frm := TMainForm.Create(nil);
  try
    frm.Show;
    fpgApplication.Run;
  finally
    frm.Free;
  end;
end;


begin
  MainProc;
end.

