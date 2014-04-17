unit frm_find;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, lq_base, lq_main, lq_form, lq_label,
  lq_edit, lq_button, lq_checkbox, lq_panel, lq_radiobutton,
  lq_textedit;

type

  TFindForm = class(TlqForm)
  private
    {@VFD_HEAD_BEGIN: FindForm}
    Label1: TlqLabel;
    FindEdit: TlqEdit;
    btnFind: TlqButton;
    btnCancel: TlqButton;
    GroupBox1: TlqGroupBox;
    chkWholeWord: TlqCheckBox;
    chkCaseSensitive: TlqCheckBox;
    rbForward: TlqRadioButton;
    rbBackward: TlqRadioButton;
    {@VFD_HEAD_END: FindForm}
    procedure btnFindClicked(Sender: TObject);
    function GetTextToFind: TlqString;
    function GetIsForward: boolean;
    function GetFindOptions: TlqFindOptions;
  public
    procedure AfterCreate; override;
    function Execute: boolean;
    property TextToFind: TlqString read GetTextToFind;
    property IsForward: boolean read GetIsForward;
    property FindOptions: TlqFindOptions read GetFindOptions;
  end;

{@VFD_NEWFORM_DECL}

implementation

{@VFD_NEWFORM_IMPL}

procedure TFindForm.btnFindClicked(Sender: TObject);
begin
  ModalResult := mrOK;
end;

function TFindForm.GetTextToFind: TlqString;
begin
  Result := FindEdit.Text;
end;

function TFindForm.GetIsForward: boolean;
begin
  Result := rbForward.Checked;
end;

function TFindForm.GetFindOptions: TlqFindOptions;
begin
  Result := [foEntireScope];
  if chkWholeWord.Checked then
    Result := Result + [foWholeWords];
  if chkCaseSensitive.Checked then
    Result := Result + [foMatchCase];
end;

procedure TFindForm.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: FindForm}
  Name := 'FindForm';
  SetPosition(292, 173, 429, 110);
  WindowTitle := 'Find';
  Hint := '';

  Label1 := TlqLabel.Create(self);
  with Label1 do
  begin
    Name := 'Label1';
    SetPosition(4, 4, 280, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Text to find:';
  end;

  FindEdit := TlqEdit.Create(self);
  with FindEdit do
  begin
    Name := 'FindEdit';
    SetPosition(4, 20, 332, 24);
    Anchors := [anLeft,anRight,anTop];
    ExtraHint := '';
    FontDesc := '#Edit1';
    Hint := '';
    TabOrder := 2;
    Text := '';
  end;

  btnFind := TlqButton.Create(self);
  with btnFind do
  begin
    Name := 'btnFind';
    SetPosition(345, 8, 80, 24);
    Anchors := [anRight,anTop];
    Text := 'Find';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 3;
    OnClick := @btnFindClicked;
  end;

  btnCancel := TlqButton.Create(self);
  with btnCancel do
  begin
    Name := 'btnCancel';
    SetPosition(345, 36, 80, 24);
    Anchors := [anRight,anTop];
    Text := 'Cancel';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    ModalResult := mrCancel;
    TabOrder := 4;
  end;

  GroupBox1 := TlqGroupBox.Create(self);
  with GroupBox1 do
  begin
    Name := 'GroupBox1';
    SetPosition(160, 56, 176, 44);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Direction';
  end;

  chkWholeWord := TlqCheckBox.Create(self);
  with chkWholeWord do
  begin
    Name := 'chkWholeWord';
    SetPosition(4, 52, 148, 20);
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 6;
    Text := 'Whole words only';
  end;

  chkCaseSensitive := TlqCheckBox.Create(self);
  with chkCaseSensitive do
  begin
    Name := 'chkCaseSensitive';
    SetPosition(4, 72, 148, 20);
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 7;
    Text := 'Case sensitive';
  end;

  rbForward := TlqRadioButton.Create(GroupBox1);
  with rbForward do
  begin
    Name := 'rbForward';
    SetPosition(8, 20, 76, 20);
    Checked := True;
    FontDesc := '#Label1';
    GroupIndex := 0;
    Hint := '';
    TabOrder := 1;
    Text := 'Forword';
  end;

  rbBackward := TlqRadioButton.Create(GroupBox1);
  with rbBackward do
  begin
    Name := 'rbBackward';
    SetPosition(88, 20, 84, 20);
    FontDesc := '#Label1';
    GroupIndex := 0;
    Hint := '';
    TabOrder := 2;
    Text := 'Backward';
  end;

  {@VFD_BODY_END: FindForm}
  {%endregion}
end;

function TFindForm.Execute: boolean;
begin
  Result := ShowModal <> mrCancel;
end;


end.
