unit frm_child;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, lq_base, lq_main, lq_form, lq_button, lq_edit,
  lq_checkbox, lq_radiobutton, lq_gauge, lq_mdi, lq_panel, lq_trackbar;

type

	TChildForm = class(TlqFrame)
	private
		{@VFD_HEAD_BEGIN: ChildForm}
		btnClose: TlqButton;
		CheckBox1: TlqCheckBox;
		CheckBox2: TlqCheckBox;
		RadioButton1: TlqRadioButton;
		RadioButton2: TlqRadioButton;
		Edit1: TlqEdit;
		Gauge1: TlqGauge;
		TrackBar1: TlqTrackBar;
		{@VFD_HEAD_END: ChildForm}
		FWindowTitle: TlqString;
		procedure btnCloseClicked(Sender: TObject);
		procedure TrackBarChanged(Sender: TObject; APosition: integer);
		procedure SetWindowTitle(AValue: TlqString);
	public
		procedure AfterCreate; override;
		property WindowTitle: TlqString read FWindowTitle write SetWindowTitle;
	end;

{@VFD_NEWFORM_DECL}

var
  ChildForm: TChildForm;

implementation


{@VFD_NEWFORM_IMPL}

procedure TChildForm.TrackBarChanged(Sender: TObject; APosition: integer);
begin
	Gauge1.Progress := APosition;
end;

procedure TChildForm.SetWindowTitle(AValue: TlqString);
begin
	if FWindowTitle = AValue then
		Exit;
	FWindowTitle := AValue;
	TlqMDIChildForm(Owner.Owner).WindowTitle := FWindowTitle;
end;

procedure TChildForm.btnCloseClicked(Sender: TObject);
begin
	TlqMDIChildForm(Owner).Close;
end;

procedure TChildForm.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: ChildForm}
	Name := 'ChildForm';
	SetPosition(391, 210, 271, 150);
//	WindowTitle := 'ChildForm';
	Hint := '';

	btnClose := TlqButton.Create(self);
	with btnClose do
	begin
		Name := 'btnClose';
		SetPosition(180, 116, 80, 24);
		Text := 'Close';
		FontDesc := '#Label1';
		Hint := '';
		ImageName := '';
		TabOrder := 1;
		OnClick := @btnCloseClicked;
	end;

	CheckBox1 := TlqCheckBox.Create(self);
	with CheckBox1 do
	begin
		Name := 'CheckBox1';
		SetPosition(164, 16, 120, 20);
		FontDesc := '#Label1';
		Hint := '';
		TabOrder := 2;
		Text := 'CheckBox';
	end;

	CheckBox2 := TlqCheckBox.Create(self);
	with CheckBox2 do
	begin
		Name := 'CheckBox2';
		SetPosition(164, 36, 120, 20);
		FontDesc := '#Label1';
		Hint := '';
		TabOrder := 3;
		Text := 'CheckBox';
	end;

	RadioButton1 := TlqRadioButton.Create(self);
	with RadioButton1 do
	begin
		Name := 'RadioButton1';
		SetPosition(164, 60, 120, 20);
		FontDesc := '#Label1';
		GroupIndex := 0;
		Hint := '';
		TabOrder := 4;
		Text := 'RadioButton';
	end;

	RadioButton2 := TlqRadioButton.Create(self);
	with RadioButton2 do
	begin
		Name := 'RadioButton2';
		SetPosition(164, 80, 120, 20);
		FontDesc := '#Label1';
		GroupIndex := 0;
		Hint := '';
		TabOrder := 5;
		Text := 'RadioButton';
	end;

	Edit1 := TlqEdit.Create(self);
	with Edit1 do
	begin
		Name := 'Edit1';
		SetPosition(8, 8, 120, 24);
		ExtraHint := '';
		FontDesc := '#Edit1';
		Hint := '';
		TabOrder := 6;
		Text := '';
	end;

	Gauge1 := TlqGauge.Create(self);
	with Gauge1 do
	begin
		Name := 'Gauge1';
		SetPosition(12, 44, 116, 25);
		Color := TlqColor($C4C4C4);
		Hint := '';
		Progress := 65;
	end;

	TrackBar1 := TlqTrackBar.Create(self);
	with TrackBar1 do
	begin
		Name := 'TrackBar1';
		SetPosition(12, 84, 116, 30);
		Hint := '';
		TabOrder := 8;
		Position := 65;
		OnChange  := @TrackBarChanged;
	end;

	{@VFD_BODY_END: ChildForm}
  {%endregion}
	Name := 'ChildForm' + IntToStr(Random(MaxInt));

end;


end.
