unit frm_main;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  lq_base, lq_main, lq_edit,
  lq_widget, lq_form, lq_label, lq_button,
  lq_listbox, lq_memo, lq_combobox, lq_basegrid, lq_grid,
  lq_dialogs, lq_checkbox, lq_tree, lq_trackbar,
  lq_progressbar, lq_radiobutton, lq_tab, lq_menu,
  lq_panel, lq_popupcalendar, lq_gauge;

type

  TMainForm = class(TlqForm)
  private
    procedure cbHotTrackChanged(Sender: TObject);
    procedure ckhReorderChanged(Sender: TObject);
    procedure btnAdd1Clicked(Sender: TObject);
    procedure btnFocusClicked(Sender: TObject);
    procedure btnClearClicked(Sender: TObject);
    procedure btnAdd10Clicked(Sender: TObject);
  public
    {@VFD_HEAD_BEGIN: MainForm}
    lstName1: TlqListBox;
    btnAdd10: TlqButton;
    btnClear: TlqButton;
    btnFocus: TlqButton;
    btnAdd1: TlqButton;
    memName1: TlqMemo;
    cbHotTrack: TlqCheckBox;
    chkReorder: TlqCheckBox;
    {@VFD_HEAD_END: MainForm}
    procedure AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

implementation

{@VFD_NEWFORM_IMPL}

procedure TMainForm.cbHotTrackChanged(Sender: TObject);
begin
  lstName1.HotTrack := cbHotTrack.Checked;
end;

procedure TMainForm.ckhReorderChanged(Sender: TObject);
begin
  lstName1.DragToReorder := not lstName1.DragToReorder;
end;

procedure TMainForm.btnAdd1Clicked(Sender: TObject);
begin
  lstName1.Items.Add(Format('Item %2d', [lstName1.ItemCount]));
end;

procedure TMainForm.btnFocusClicked(Sender: TObject);
begin
  if lstName1.ItemCount > 1 then
    lstName1.FocusItem := 2;
end;

procedure TMainForm.btnClearClicked(Sender: TObject);
begin
  lstName1.Items.Clear;
end;

procedure TMainForm.btnAdd10Clicked(Sender: TObject);
var
  i: integer;
begin
  for i := 1 to 10 do
    lstName1.Items.Add(Format('Item %2d', [lstName1.ItemCount]));
end;

procedure TMainForm.AfterCreate;
begin
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(345, 220, 300, 270);
  WindowTitle := 'ListBox test';
  WindowPosition := wpScreenCenter;

  lstName1 := TlqListBox.Create(self);
  with lstName1 do
  begin
    Name := 'lstName1';
    SetPosition(12, 12, 140, 168);
    FontDesc := '#List';
    HotTrack := False;
    PopupFrame := False;
    TabOrder := 0;
  end;

  btnAdd10 := TlqButton.Create(self);
  with btnAdd10 do
  begin
    Name := 'btnAdd10';
    SetPosition(172, 28, 92, 23);
    Text := 'Add 10 items';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 1;
    OnClick := @btnAdd10Clicked;
  end;

  btnClear := TlqButton.Create(self);
  with btnClear do
  begin
    Name := 'btnClear';
    SetPosition(172, 56, 92, 23);
    Text := 'Clear Items';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 2;
    OnClick := @btnClearClicked;
  end;

  btnFocus := TlqButton.Create(self);
  with btnFocus do
  begin
    Name := 'btnFocus';
    SetPosition(172, 84, 92, 23);
    Text := 'FocusItem = 2';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 3;
    OnClick := @btnFocusClicked;
  end;

  btnAdd1 := TlqButton.Create(self);
  with btnAdd1 do
  begin
    Name := 'btnAdd1';
    SetPosition(172, 112, 92, 23);
    Text := 'Add 1 item';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 4;
    OnClick := @btnAdd1Clicked;
  end;

  memName1 := TlqMemo.Create(self);
  with memName1 do
  begin
    Name := 'memName1';
    SetPosition(12, 188, 280, 77);
    FontDesc := '#Edit1';
    TabOrder := 5;
  end;

  cbHotTrack := TlqCheckBox.Create(self);
  with cbHotTrack do
  begin
    Name := 'cbHotTrack';
    SetPosition(172, 140, 120, 19);
    FontDesc := '#Label1';
    TabOrder := 6;
    Text := 'Track Focus';
    OnChange := @cbHotTrackChanged;
  end;

  chkReorder := TlqCheckBox.Create(self);
  with chkReorder do
  begin
    Name := 'chkReorder';
    SetPosition(172, 160, 120, 20);
    FontDesc := '#Label1';
    TabOrder := 7;
    Text := 'Drag to reorder';
    OnChange := @ckhReorderChanged;
  end;

  {@VFD_BODY_END: MainForm}
end;


end.
