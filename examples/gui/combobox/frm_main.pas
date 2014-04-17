unit frm_main;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, lq_base, lq_main, lq_edit,
  lq_widget, lq_form, lq_label, lq_button,
  lq_listbox, lq_memo, lq_combobox, lq_basegrid, lq_grid,
  lq_dialogs, lq_checkbox, lq_tree, lq_trackbar,
  lq_progressbar, lq_radiobutton, lq_tab, lq_menu,
  lq_panel, lq_popupcalendar, lq_gauge, lq_editcombo;

type

  TMainForm = class(TlqForm)
  private
    procedure rbChanged(Sender: TObject);
    procedure cbAutoCompleteChanged(Sender: TObject);
    procedure cbAllowNewChanged(Sender: TObject);
    procedure btnAdd1Clicked(Sender: TObject);
    procedure btnFocusClicked(Sender: TObject);
    procedure btnClearClicked(Sender: TObject);
    procedure btnAdd10Clicked(Sender: TObject);
  public
    {@VFD_HEAD_BEGIN: MainForm}
    btnAdd10: TlqButton;
    btnClear: TlqButton;
    btnFocus: TlqButton;
    btnAdd1: TlqButton;
    cbAutoComplete: TlqCheckBox;
    Combo1: TlqComboBox;
    lblName1: TlqLabel;
    lblName2: TlqLabel;
    EditCombo1: TlqEditCombo;
    lblName3: TlqLabel;
    lblName4: TlqLabel;
    cbAllowNew: TlqComboBox;
    grpBox: TlqGroupBox;
    rbName1: TlqRadioButton;
    rbName2: TlqRadioButton;
    rbName3: TlqRadioButton;
    {@VFD_HEAD_END: MainForm}
    constructor Create(AOwner: TComponent); override;
    procedure   AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

implementation

uses
  RandomData;

{@VFD_NEWFORM_IMPL}

procedure TMainForm.rbChanged(Sender: TObject);
begin
  if Sender is TlqRadioButton then
  begin
    Combo1.DropDownCount := TlqRadioButton(Sender).Tag;
    EditCombo1.DropDownCount := TlqRadioButton(Sender).Tag;
  end;
end;

procedure TMainForm.cbAutoCompleteChanged(Sender: TObject);
begin
  EditCombo1.AutoCompletion := cbAutoComplete.Checked;
end;

procedure TMainForm.cbAllowNewChanged(Sender: TObject);
begin
  if cbAllowNew.Text = 'anNo' then
    EditCombo1.AllowNew := anNo
  else if cbAllowNew.Text = 'anYes' then
    EditCombo1.AllowNew := anYes
  else if cbAllowNew.Text = 'anAsk' then
    EditCombo1.AllowNew := anAsk
end;

procedure TMainForm.btnAdd1Clicked(Sender: TObject);
var
  Gender: TGender;
  n: string;
begin
  Gender := TGender(Random(2));
  n := RandomFullName(Gender);
  Combo1.Items.Add(n);
  EditCombo1.Items.Add(n);
  Combo1.Items.Sort;
  EditCombo1.Items.Sort;
end;

procedure TMainForm.btnFocusClicked(Sender: TObject);
begin
  if Combo1.Items.Count > 1 then
    Combo1.FocusItem := 2;
  if EditCombo1.Items.Count > 1 then
    EditCombo1.FocusItem := 2;
end;

procedure TMainForm.btnClearClicked(Sender: TObject);
begin
  Combo1.Items.Clear;
  EditCombo1.Items.Clear;
end;

procedure TMainForm.btnAdd10Clicked(Sender: TObject);
var
  i: integer;
  Gender: TGender;
  n: string;
begin
  for i := 1 to 10 do
  begin
    Gender := TGender(Random(2));
    n := RandomFullName(Gender);
    Combo1.Items.Add(n);
    EditCombo1.Items.Add(n);
  end;
  Combo1.Items.Sort;
  EditCombo1.Items.Sort;
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Randomize;
end;

procedure TMainForm.AfterCreate;
begin
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(345, 220, 484, 260);
  WindowTitle := 'ComboBox test';
  WindowPosition := wpScreenCenter;

  btnAdd10 := TlqButton.Create(self);
  with btnAdd10 do
  begin
    Name := 'btnAdd10';
    SetPosition(220, 28, 92, 23);
    Text := 'Add 10 items';
    FontDesc := '#Label1';
    ImageName := '';
    TabOrder := 1;
    OnClick := @btnAdd10Clicked;
  end;

  btnClear := TlqButton.Create(self);
  with btnClear do
  begin
    Name := 'btnClear';
    SetPosition(220, 56, 92, 23);
    Text := 'Clear Items';
    FontDesc := '#Label1';
    ImageName := '';
    TabOrder := 2;
    OnClick := @btnClearClicked;
  end;

  btnFocus := TlqButton.Create(self);
  with btnFocus do
  begin
    Name := 'btnFocus';
    SetPosition(220, 84, 92, 23);
    Text := 'FocusItem = 2';
    FontDesc := '#Label1';
    ImageName := '';
    TabOrder := 3;
    OnClick := @btnFocusClicked;
  end;

  btnAdd1 := TlqButton.Create(self);
  with btnAdd1 do
  begin
    Name := 'btnAdd1';
    SetPosition(220, 112, 92, 23);
    Text := 'Add 1 item';
    FontDesc := '#Label1';
    ImageName := '';
    TabOrder := 4;
    OnClick := @btnAdd1Clicked;
  end;

  cbAutoComplete := TlqCheckBox.Create(self);
  with cbAutoComplete do
  begin
    Name := 'cbAutoComplete';
    SetPosition(216, 168, 120, 19);
    FontDesc := '#Label1';
    TabOrder := 6;
    Text := 'Auto Complete';
    OnChange := @cbAutoCompleteChanged;
  end;

  Combo1 := TlqComboBox.Create(self);
  with Combo1 do
  begin
    Name := 'Combo1';
    SetPosition(8, 24, 168, 22);
    FontDesc := '#List';
    TabOrder := 6;
  end;

  lblName1 := TlqLabel.Create(self);
  with lblName1 do
  begin
    Name := 'lblName1';
    SetPosition(8, 8, 176, 15);
    FontDesc := '#Label1';
    Text := 'Static ComboBox';
  end;

  lblName2 := TlqLabel.Create(self);
  with lblName2 do
  begin
    Name := 'lblName2';
    SetPosition(8, 68, 176, 15);
    FontDesc := '#Label1';
    Text := 'Edit ComboBox';
  end;

  EditCombo1 := TlqEditCombo.Create(self);
  with EditCombo1 do
  begin
    Name := 'EditCombo1';
    SetPosition(8, 88, 168, 21);
  end;

  lblName3 := TlqLabel.Create(self);
  with lblName3 do
  begin
    Name := 'lblName3';
    SetPosition(204, 148, 128, 15);
    FontDesc := '#Label2';
    Text := 'EditCombo only';
  end;

  lblName4 := TlqLabel.Create(self);
  with lblName4 do
  begin
    Name := 'lblName4';
    SetPosition(204, 8, 136, 15);
    FontDesc := '#Label2';
    Text := 'Both components';
  end;

  cbAllowNew := TlqComboBox.Create(self);
  with cbAllowNew do
  begin
    Name := 'cbAllowNew';
    SetPosition(220, 196, 100, 22);
    FontDesc := '#List';
    Items.Add('anNo');
    Items.Add('anYes');
    Items.Add('anAsk');
    TabOrder := 13;
    OnChange := @cbAllowNewChanged;
    FocusItem := 0;
  end;

  grpBox := TlqGroupBox.Create(self);
  with grpBox do
  begin
    Name := 'grpBox';
    SetPosition(328, 28, 144, 108);
    Text := 'Dropdown Count:';
  end;

  rbName1 := TlqRadioButton.Create(grpBox);
  with rbName1 do
  begin
    Name := 'rbName1';
    SetPosition(16, 24, 80, 20);
    Checked := True;
    FontDesc := '#Label1';
    Text := '8 items';
    Tag := 8;
    OnChange := @rbChanged;
  end;

  rbName2 := TlqRadioButton.Create(grpBox);
  with rbName2 do
  begin
    Name := 'rbName2';
    SetPosition(16, 44, 80, 20);
    FontDesc := '#Label1';
    TabOrder := 1;
    Text := '12 items';
    Tag := 12;
    OnChange := @rbChanged;
  end;

  rbName3 := TlqRadioButton.Create(grpBox);
  with rbName3 do
  begin
    Name := 'rbName3';
    SetPosition(16, 64, 80, 20);
    FontDesc := '#Label1';
    TabOrder := 2;
    Text := '25 items';
    Tag := 25;
    OnChange := @rbChanged;
  end;

  {@VFD_BODY_END: MainForm}

end;


end.
