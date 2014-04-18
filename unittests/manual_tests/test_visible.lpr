{
  Project to test the .Visible property of components.
}
program test_visible;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, lq_main, lq_form, lq_edit, lq_button, lq_label,
  lq_memo, lq_checkbox, lq_radiobutton, lq_widget, lq_panel, lq_menu;

type

  TMainForm = class(TlqForm)
  private
    {@VFD_HEAD_BEGIN: MainForm}
    Edit1: TlqEdit;
    Label1: TlqLabel;
    Button1: TlqButton;
    Memo1: TlqMemo;
    CheckBox1: TlqCheckBox;
    RadioButton1: TlqRadioButton;
    Panel1: TlqPanel;
    Edit2: TlqEdit;
    Label2: TlqLabel;
    CheckBox2: TlqCheckBox;
    Memo2: TlqMemo;
    MainMenu: TlqMenuBar;
    pmFile: TlqPopupMenu;
    {@VFD_HEAD_END: MainForm}
    procedure FormShow(Sender: TObject);
    procedure CheckboxChanged(Sender: TObject);
    procedure miFileQuitClicked(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}



{@VFD_NEWFORM_IMPL}

var
  { manage indentation level }
  indent: integer = 0;

{ Output component hierarchy and status of each component's Visible property }
procedure PrintVisibleState(AComponent: TlqWidget);
var
  i: integer;
  { Create string equal to indentation level }
  function Spaces: string;
  var
    j: integer;
  begin
    if indent = 0 then
      exit;
    for j := 1 to indent do
      Result := Result + ' ';
  end;

begin
  writeln(Spaces + AComponent.ClassName + ' - [parent]: ' + BoolToStr(AComponent.Visible, True));
  Inc(indent, 2);
  for i := 0 to AComponent.ComponentCount-1 do
  begin
    if AComponent.Components[i].ComponentCount > 0 then
      PrintVisibleState(TlqWidget(AComponent.Components[i]))
    else
      writeln(Spaces + AComponent.Components[i].ClassName + ': ' + BoolToStr(TlqWidget(AComponent.Components[i]).Visible, True));
  end;
  dec(indent, 2);
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  writeln('DEBUG:  TMainForm.FormShow >>');
  PrintVisibleState(self);
  writeln('DEBUG:  TMainForm.FormShow <<');
end;

procedure TMainForm.CheckboxChanged(Sender: TObject);
begin
  writeln('Checkbox clicked...');
  Edit1.Visible := CheckBox1.Checked;
  Panel1.Visible := CheckBox1.Checked;
  PrintVisibleState(self);
end;

procedure TMainForm.miFileQuitClicked(Sender: TObject);
begin
  Close;
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  writeln('DEBUG:  TMainForm.Create >>');
  inherited Create(AOwner);
  OnShow  := @FormShow;
  writeln('DEBUG:  TMainForm.Create <<');
end;

procedure TMainForm.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(316, 186, 591, 250);
  WindowTitle := 'Test Visible property';
  Hint := '';

  Edit1 := TlqEdit.Create(self);
  with Edit1 do
  begin
    Name := 'Edit1';
    SetPosition(8, 28, 120, 24);
    Hint := '';
    TabOrder := 0;
    Text := '';
    FontDesc := '#Edit1';
    Visible := False;
  end;

  Label1 := TlqLabel.Create(self);
  with Label1 do
  begin
    Name := 'Label1';
    SetPosition(140, 152, 80, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Label';
  end;

  Button1 := TlqButton.Create(self);
  with Button1 do
  begin
    Name := 'Button1';
    SetPosition(140, 28, 80, 24);
    Text := 'Button';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 2;
  end;

  Memo1 := TlqMemo.Create(self);
  with Memo1 do
  begin
    Name := 'Memo1';
    SetPosition(8, 60, 120, 52);
    Hint := '';
    Lines.Add('');
    FontDesc := '#Edit1';
    TabOrder := 3;
  end;

  CheckBox1 := TlqCheckBox.Create(self);
  with CheckBox1 do
  begin
    Name := 'CheckBox1';
    SetPosition(144, 64, 84, 20);
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 4;
    Text := 'Is Visible';
    OnChange  := @CheckboxChanged;
  end;

  RadioButton1 := TlqRadioButton.Create(self);
  with RadioButton1 do
  begin
    Name := 'RadioButton1';
    SetPosition(8, 120, 120, 20);
    FontDesc := '#Label1';
    GroupIndex := 0;
    Hint := '';
    TabOrder := 5;
    Text := 'RadioButton';
  end;

  Panel1 := TlqPanel.Create(self);
  with Panel1 do
  begin
    Name := 'Panel1';
    SetPosition(8, 148, 116, 72);
    FontDesc := '#Label1';
    Hint := '';
    Text := '';
  end;

  Edit2 := TlqEdit.Create(Panel1);
  with Edit2 do
  begin
    Name := 'Edit2';
    SetPosition(8, 8, 96, 24);
    Hint := '';
    TabOrder := 0;
    Text := '';
    FontDesc := '#Edit1';
    Visible := False;
  end;

  Label2 := TlqLabel.Create(Panel1);
  with Label2 do
  begin
    Name := 'Label2';
    SetPosition(8, 36, 64, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Label';
  end;

  CheckBox2 := TlqCheckBox.Create(Panel1);
  with CheckBox2 do
  begin
    Name := 'CheckBox2';
    SetPosition(8, 50, 101, 20);
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 2;
    Text := 'CheckBox';
  end;

  Memo2 := TlqMemo.Create(self);
  with Memo2 do
  begin
    Name := 'Memo2';
    SetPosition(248, 4, 320, 232);
    Hint := '';
    Lines.Add('This apps tests the .Visible property of components.');
    Lines.Add('At startup, all TEdit componts should be invisible.');
    Lines.Add('When you click the "Is Visible" checkbox for the first');
    Lines.Add('time, then the Edit1 (top left) should become visible.');
    Lines.Add('');
    Lines.Add('All sub-sequent clicks of the "Is Visble" checkbox will');
    Lines.Add('toggle the Edit1 and Panel1 visibility. Not other');
    Lines.Add('components like Lable or Checbox inside Panel1');
    Lines.Add('should always stay Visible = True.');
    Lines.Add('');
    Lines.Add('Console output at runtime will show .Visible status of');
    Lines.Add('each component.');
    FontDesc := '#Edit1';
    TabOrder := 7;
  end;

  MainMenu := TlqMenuBar.Create(self);
  with MainMenu do
  begin
    Name := 'MainMenu';
    SetPosition(0, 0, 144, 24);
  end;

  pmFile := TlqPopupMenu.Create(self);
  with pmFile do
  begin
    Name := 'pmFile';
    SetPosition(108, 88, 120, 20);
    AddMenuItem('Quit', '', @miFileQuitClicked);
  end;

  {@VFD_BODY_END: MainForm}

  // hook up any sub-menus
  MainMenu.AddMenuItem('File', nil).SubMenu := pmFile;
  {%endregion}
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

