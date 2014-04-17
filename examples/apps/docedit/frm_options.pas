unit frm_options;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, lq_base, lq_main, lq_edit,
  lq_widget, lq_form, lq_label, lq_button,
  lq_listbox, lq_memo, lq_combobox, lq_grid,
  lq_dialogs, lq_checkbox, lq_tree, lq_trackbar,
  lq_progressbar, lq_radiobutton, lq_tab, lq_menu,
  lq_panel;

type

  TfrmOptions = class(TlqForm)
  public
    {@VFD_HEAD_BEGIN: frmOptions}
    lblName1: TlqLabel;
    cbConfirmDeletes: TlqCheckBox;
    cbBackups: TlqCheckBox;
    cbSkipNodes: TlqCheckBox;
    edtDftExt: TlqEdit;
    edtBakExt: TlqEdit;
    edtMRU: TlqEdit;
    lblName2: TlqLabel;
    lblName3: TlqLabel;
    lblName4: TlqLabel;
    edtMakeSkel: TlqEdit;
    btnMakeSkel: TlqButton;
    edtFPDoc: TlqEdit;
    btnFPDoc: TlqButton;
    cbShowHints: TlqCheckBox;
    pnlName1: TlqBevel;
    btnOK: TlqButton;
    btnCancel: TlqButton;
    lblName5: TlqLabel;
    lblName6: TlqLabel;
    lblName7: TlqLabel;
    {@VFD_HEAD_END: frmOptions}

    procedure AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

implementation

{@VFD_NEWFORM_IMPL}

procedure TfrmOptions.AfterCreate;
begin
  {@VFD_BODY_BEGIN: frmOptions}
  SetPosition(394, 264, 397, 357);
  WindowTitle := 'Options';
  WindowPosition := wpScreenCenter;

  lblName1 := TlqLabel.Create(self);
  with lblName1 do
  begin
    SetPosition(8, 8, 51, 16);
    Text := 'General';
    FontDesc := '#Label2';
    AutoSize := True;
  end;

  cbConfirmDeletes := TlqCheckBox.Create(self);
  with cbConfirmDeletes do
  begin
    SetPosition(24, 28, 120, 20);
    Text := 'Confirm deletes';
    FontDesc := '#Label1';
  end;

  cbBackups := TlqCheckBox.Create(self);
  with cbBackups do
  begin
    SetPosition(24, 52, 120, 20);
    Text := 'Create backups';
    FontDesc := '#Label1';
  end;

  cbSkipNodes := TlqCheckBox.Create(self);
  with cbSkipNodes do
  begin
    SetPosition(24, 76, 216, 20);
    Text := 'Skip empty nodes when saving';
    FontDesc := '#Label1';
  end;

  edtDftExt := TlqEdit.Create(self);
  with edtDftExt do
  begin
    SetPosition(164, 104, 120, 22);
    Text := '.xml';
    FontDesc := '#Edit1';
  end;

  edtBakExt := TlqEdit.Create(self);
  with edtBakExt do
  begin
    SetPosition(164, 128, 120, 22);
    Text := '.~xml';
    FontDesc := '#Edit1';
  end;

  edtMRU := TlqEdit.Create(self);
  with edtMRU do
  begin
    SetPosition(164, 152, 120, 22);
    Text := '10';
    FontDesc := '#Edit1';
  end;

  lblName2 := TlqLabel.Create(self);
  with lblName2 do
  begin
    SetPosition(8, 252, 50, 16);
    Text := 'Desktop';
    FontDesc := '#Label2';
    AutoSize := True;
  end;

  lblName3 := TlqLabel.Create(self);
  with lblName3 do
  begin
    SetPosition(24, 108, 104, 16);
    Text := 'Default extension:';
    FontDesc := '#Label1';
  end;

  lblName4 := TlqLabel.Create(self);
  with lblName4 do
  begin
    SetPosition(24, 132, 108, 16);
    Text := 'Backup extension:';
    FontDesc := '#Label1';
  end;

  edtMakeSkel := TlqEdit.Create(self);
  with edtMakeSkel do
  begin
    SetPosition(164, 184, 196, 22);
    Anchors := [anLeft,anRight,anTop];
    Text := '';
    FontDesc := '#Edit1';
  end;

  btnMakeSkel := TlqButton.Create(self);
  with btnMakeSkel do
  begin
    SetPosition(364, 184, 23, 20);
    Anchors := [anRight,anTop];
    Text := '...';
    FontDesc := '#Label1';
    ImageName := '';
  end;

  edtFPDoc := TlqEdit.Create(self);
  with edtFPDoc do
  begin
    SetPosition(164, 208, 196, 22);
    Anchors := [anLeft,anRight,anTop];
    Text := '';
    FontDesc := '#Edit1';
  end;

  btnFPDoc := TlqButton.Create(self);
  with btnFPDoc do
  begin
    SetPosition(364, 208, 23, 20);
    Anchors := [anRight,anTop];
    Text := '...';
    FontDesc := '#Label1';
    ImageName := '';
  end;

  cbShowHints := TlqCheckBox.Create(self);
  with cbShowHints do
  begin
    SetPosition(24, 272, 120, 20);
    Text := 'Show hints';
    FontDesc := '#Label1';
  end;

  pnlName1 := TlqBevel.Create(self);
  with pnlName1 do
  begin
    SetPosition(4, 313, 389, 11);
    Anchors := [anLeft,anRight,anBottom];
    Shape := bsTopLine;
    Style := bsLowered;
    Focusable := False;
  end;

  btnOK := TlqButton.Create(self);
  with btnOK do
  begin
    SetPosition(234, 325, 75, 24);
    Anchors := [anRight,anBottom];
    Text := 'OK';
    FontDesc := '#Label1';
    ImageName := 'stdimg.ok';
    ModalResult := mrOK;
  end;

  btnCancel := TlqButton.Create(self);
  with btnCancel do
  begin
    SetPosition(314, 325, 75, 24);
    Anchors := [anRight,anBottom];
    Text := 'Cancel';
    FontDesc := '#Label1';
    ImageName := 'stdimg.cancel';
    ModalResult := mrCancel;
  end;

  lblName5 := TlqLabel.Create(self);
  with lblName5 do
  begin
    SetPosition(24, 156, 132, 16);
    Text := 'Max. recent used files:';
    FontDesc := '#Label1';
  end;

  lblName6 := TlqLabel.Create(self);
  with lblName6 do
  begin
    SetPosition(24, 188, 127, 16);
    Text := 'makeskel executable:';
    FontDesc := '#Label1';
  end;

  lblName7 := TlqLabel.Create(self);
  with lblName7 do
  begin
    SetPosition(24, 212, 102, 16);
    Text := 'fpdoc executable:';
    FontDesc := '#Label1';
  end;

  {@VFD_BODY_END: frmOptions}
end;


end.
