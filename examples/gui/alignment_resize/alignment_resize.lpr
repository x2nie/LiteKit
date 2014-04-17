program alignment_resize;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils,
  lq_base, lq_main, lq_form, lq_button, lq_panel, lq_label,
  lq_radiobutton, lq_memo;

type

  TMainForm = class(TlqForm)
  private
    procedure btnGoClicked(Sender: TObject);
    procedure btnResetClicked(Sender: TObject);
    procedure PanelResized(Sender: TObject);
  public
    {@VFD_HEAD_BEGIN: MainForm}
    btnName1: TlqButton;
    btnName2: TlqButton;
    pnlName1: TlqPanel;
    btnName3: TlqButton;
    btnName4: TlqButton;
    btnName5: TlqButton;
    btnName6: TlqButton;
    lblName1: TlqLabel;
    pnlName2: TlqPanel;
    rbName1: TlqRadioButton;
    rbName2: TlqRadioButton;
    rbName3: TlqRadioButton;
    memName1: TlqMemo;
    {@VFD_HEAD_END: MainForm}
    procedure AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}



{@VFD_NEWFORM_IMPL}

procedure TMainForm.btnGoClicked(Sender: TObject);
begin
  if rbName1.Checked then
  begin
    pnlName1.SetPosition(4, 68, 240, 192);
  end
  else if rbName2.Checked then
  begin
    pnlName1.Width := 240;
    pnlName1.Height := 192;
    pnlName1.UpdateWindowPosition;
  end
  else if rbName3.Checked then
  begin
    pnlName1.MoveAndResizeBy(0, 0, 100, 100);
  end;
end;

procedure TMainForm.btnResetClicked(Sender: TObject);
begin
  pnlName1.SetPosition(4, 68, 140, 92);
end;

procedure TMainForm.PanelResized(Sender: TObject);
begin
  memName1.Lines.Insert(0, FormatDateTime('HH:mm:ss', Time) + ' Panel.OnResize');
end;

procedure TMainForm.AfterCreate;
begin
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(316, 186, 467, 284);
  WindowTitle := 'Runtime resize/alignment test';
  WindowPosition := wpScreenCenter;
//  Sizeable := False;

  btnName1 := TlqButton.Create(self);
  with btnName1 do
  begin
    Name := 'btnName1';
    SetPosition(4, 4, 80, 24);
    Text := 'Go';
    FontDesc := '#Label1';
    ImageName := '';
    OnClick := @btnGoClicked;
  end;

  btnName2 := TlqButton.Create(self);
  with btnName2 do
  begin
    Name := 'btnName2';
    SetPosition(4, 32, 80, 24);
    Text := 'Reset';
    FontDesc := '#Label1';
    ImageName := '';
    TabOrder := 1;
    OnClick := @btnResetClicked;
  end;

  pnlName1 := TlqPanel.Create(self);
  with pnlName1 do
  begin
    Name := 'pnlName1';
    SetPosition(4, 68, 140, 92);
    Text := '';
    BackgroundColor := clDarkKhaki;
    OnResize := @PanelResized;
  end;

  btnName3 := TlqButton.Create(pnlName1);
  with btnName3 do
  begin
    Name := 'btnName3';
    SetPosition(8, 8, 24, 24);
    Text := '1';
    FontDesc := '#Label1';
    ImageName := '';
  end;

  btnName4 := TlqButton.Create(pnlName1);
  with btnName4 do
  begin
    Name := 'btnName4';
    SetPosition(108, 8, 24, 24);
    Anchors := [anRight,anTop];
    Text := '2';
    FontDesc := '#Label1';
    ImageName := '';
    TabOrder := 1;
  end;

  btnName5 := TlqButton.Create(pnlName1);
  with btnName5 do
  begin
    Name := 'btnName5';
    SetPosition(8, 60, 24, 24);
    Anchors := [anLeft,anBottom];
    Text := '3';
    FontDesc := '#Label1';
    ImageName := '';
    TabOrder := 2;
  end;

  btnName6 := TlqButton.Create(pnlName1);
  with btnName6 do
  begin
    Name := 'btnName6';
    SetPosition(108, 60, 24, 24);
    Anchors := [anRight,anBottom];
    Text := '4';
    FontDesc := '#Label1';
    ImageName := '';
    TabOrder := 3;
  end;

  lblName1 := TlqLabel.Create(self);
  with lblName1 do
  begin
    Name := 'lblName1';
    SetPosition(272, 4, 116, 16);
    FontDesc := '#Label2';
    Text := 'Update method';
  end;

  pnlName2 := TlqPanel.Create(self);
  with pnlName2 do
  begin
    Name := 'pnlName2';
    SetPosition(256, 12, 4, 264);
    Style := bsLowered;
    Text := '';
  end;

  rbName1 := TlqRadioButton.Create(self);
  with rbName1 do
  begin
    Name := 'rbName1';
    SetPosition(288, 28, 172, 20);
    Checked := True;
    FontDesc := '#Label1';
    TabOrder := 5;
    Text := 'SetPosition()';
  end;

  rbName2 := TlqRadioButton.Create(self);
  with rbName2 do
  begin
    Name := 'rbName2';
    SetPosition(288, 48, 172, 20);
    FontDesc := '#Label1';
    TabOrder := 6;
    Text := 'UpdateWindowPosition()';
  end;

  rbName3 := TlqRadioButton.Create(self);
  with rbName3 do
  begin
    Name := 'rbName3';
    SetPosition(288, 68, 172, 20);
    FontDesc := '#Label1';
    TabOrder := 7;
    Text := 'MoveAndResizeBy()';
  end;

  memName1 := TlqMemo.Create(self);
  with memName1 do
  begin
    Name := 'memName1';
    SetPosition(272, 104, 184, 164);
    FontDesc := '#Edit1';
    TabOrder := 8;
    Anchors := AllAnchors;
  end;

  {@VFD_BODY_END: MainForm}
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


