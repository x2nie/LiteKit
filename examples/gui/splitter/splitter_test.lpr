program splitter_test;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  SysUtils, Classes, lq_base, lq_main,
  lq_form, lq_memo, lq_listbox,
  lq_panel, lq_progressbar, lq_splitter, lq_checkbox;

type
  { TfrmSplitterTest }

  TfrmSplitterTest = class(TlqForm)
  private
    procedure CheckBoxChanged(Sender: TObject);
  public
    {@VFD_HEAD_BEGIN: frmSplitterTest}
    lstChoice: TlqListBox;
    spl1: TlqSplitter;
    mmSource: TlqMemo;
    spl2: TlqSplitter;
    mmDest: TlqMemo;
    pnlName1: TlqPanel;
    spl3: TlqSplitter;
    pbName1: TlqProgressBar;
    spl4: TlqSplitter;
    cbShowGrabBar: TlqCheckBox;
    {@VFD_HEAD_END: frmSplitterTest}
    procedure AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

{@VFD_NEWFORM_IMPL}

procedure TfrmSplitterTest.CheckBoxChanged(Sender: TObject);
begin
  //
end;

procedure TfrmSplitterTest.AfterCreate;
begin
  {@VFD_BODY_BEGIN: frmSplitterTest}
  Name := 'frmSplitterTest';
  SetPosition(292, 184, 553, 290);
  WindowTitle := 'Splitter Demo';

  lstChoice := TlqListBox.Create(self);
  with lstChoice do
  begin
    Name := 'lstChoice';
    SetPosition(-1, 0, 160, 211);
    FontDesc := '#List';
    Items.Add('List item #1');
    Items.Add('List item #2');
    TabOrder := 3;
    Align := alLeft;
  end;

  spl1 := TlqSplitter.Create(self);
  with spl1 do
  begin
    Name := 'spl1';
    SetPosition(159, 0, 8, 212);
    Align := alLeft;
  end;

  mmSource := TlqMemo.Create(self);
  with mmSource do
  begin
    Name := 'mmSource';
    SetPosition(164, 0, 257, 90);
    Lines.Add('Memo has a MinHeight=30 so the splitter');
    Lines.Add('snap effect will not take affect - as expected.');
    FontDesc := '#Edit1';
    TabOrder := 2;
    Align := alTop;
  end;

  spl2 := TlqSplitter.Create(self);
  with spl2 do
  begin
    Name := 'spl2';
    SetPosition(164, 90, 257, 8);
    Align := alTop;
  end;

  pnlName1 := TlqPanel.Create(self);
  with pnlName1 do
  begin
    Name := 'pnlName1';
    SetPosition(425, 0, 128, 208);
    Text := 'Panel';
    Align := alRight;
  end;
  
  cbShowGrabBar := TlqCheckBox.Create(pnlName1);
  with cbShowGrabBar do
  begin
    Name := 'cbShowGrabBar';
    SetPosition(4, 4, 120, 23);
    Text := 'Show GrabBar';
    Checked := True;
    OnChange :=@CheckBoxChanged;
  end;

  spl3 := TlqSplitter.Create(self);
  with spl3 do
  begin
    Name := 'spl3';
    SetPosition(422, 0, 8, 208);
    Align := alRight;
  end;

  pbName1 := TlqProgressBar.Create(self);
  with pbName1 do
  begin
    Name := 'pbName1';
    SetPosition(0, 213, 554, 78);
    Position := 100;
    Align := alBottom;
  end;

  spl4 := TlqSplitter.Create(self);
  with spl4 do
  begin
    Name := 'spl4';
    SetPosition(0, 211, 554, 8);
    Align := alBottom;
  end;

  mmDest := TlqMemo.Create(self);
  with mmDest do
  begin
    Name := 'mmDest';
    SetPosition(165, 94, 100, 100);
    Lines.Add('Memo2 Line #1');
    Lines.Add('Memo2 Line #2');
    FontDesc := '#Edit1';
    TabOrder := 1;
    Align := alClient;
  end;

  {@VFD_BODY_END: frmSplitterTest}
end;

procedure MainProc;
var
  frmSplitterTest: TfrmSplitterTest;
begin
  lqApplication.Initialize;
  frmSplitterTest := TfrmSplitterTest.Create(nil);
  try
    frmSplitterTest.Show;
    lqApplication.Run;
  finally
    frmSplitterTest.Free;
  end;
end;

begin
  MainProc;
end.
