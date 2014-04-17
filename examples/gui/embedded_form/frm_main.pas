unit frm_main;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  lq_base, lq_main, lq_form, lq_panel, lq_button,
  lq_checkbox, fra_test, lq_menu;

type

  TMainForm = class(TlqForm)
  private
    {@VFD_HEAD_BEGIN: MainForm}
    BorderBevel: TlqBevel;
    Container: TlqBevel;
    btnExternal: TlqButton;
    btnEmbed1: TlqButton;
    CheckBox1: TlqCheckBox;
    MainMenu: TlqMenubar;
    mnuFile: TlqPopupMenu;
    mnuHelp: TlqPopupMenu;
    btnEmbed2: TlqButton;
    {@VFD_HEAD_END: MainForm}
    frame: TMyFrame;
    procedure btnExternalClicked(Sender: TObject);
    procedure btnEmbed1Clicked(Sender: TObject);
    procedure btnEmbed2Clicked(Sender: TObject);
    procedure CheckBoxChanged(Sender: TObject);
    procedure miQuitClicked(Sender: TObject);
    procedure miHelpAboutClicked(Sender: TObject);
  public
    procedure AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

implementation

uses
  lq_widget, lq_dialogs;


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


{@VFD_NEWFORM_IMPL}

procedure TMainForm.CheckBoxChanged(Sender: TObject);
begin
  if Assigned(frame) then
  begin
    frame.Visible := Checkbox1.Checked;
    writeln('Checkbox clicked...');
    PrintVisibleState(self);
  end;
end;

procedure TMainForm.miQuitClicked(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.miHelpAboutClicked(Sender: TObject);
begin
  TlqMessageDialog.Information('Embedded Frame Demo', 'A simple demo showing how to embed frames')
end;

procedure TMainForm.btnExternalClicked(Sender: TObject);
var
  frm: TlqForm;
  fra: TMyFrame;
begin
  { create a form at runtime that will hold our frame. }
  frm := TlqForm.Create(nil);
  try
    frm.WindowPosition := wpOneThirdDown;
    frm.Width := 284;
    frm.Height := 257;

    // embed the frame in the form
    fra := TMyFrame.Create(frm);
    fra.Align := alClient;

    frm.ShowModal;
  finally
    frm.Free;
  end;
end;

procedure TMainForm.btnEmbed1Clicked(Sender: TObject);
begin
  if Assigned(frame) then
    exit;

  frame := TMyFrame.Create(Container);
  with frame do
  begin
    Name := 'frame';
    SetPosition(0, 0, 280, 196);
    Align := alClient;
  end;
  PrintVisibleState(self);

  CheckBox1.Enabled := True;
  CheckBox1.Checked := True;
  btnEmbed2.Enabled := False;
end;

procedure TMainForm.btnEmbed2Clicked(Sender: TObject);
begin
  if Assigned(frame) then
    exit;

  frame := TMyFrame.Create(Container);
  with frame do
  begin
    Name := 'frame';
    SetPosition(0, 0, 280, 196);
    Align := alClient;
    Visible := False;
  end;
  PrintVisibleState(self);

  CheckBox1.Enabled := True;
  CheckBox1.Checked := False;
  btnEmbed1.Enabled := False;
end;

procedure TMainForm.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(387, 207, 393, 340);
  WindowTitle := 'MainForm';
  Hint := '';
  ShowHint := True;

  BorderBevel := TlqBevel.Create(self);
  with BorderBevel do
  begin
    Name := 'BorderBevel';
    SetPosition(0, 68, 393, 272);
    Anchors := [anLeft,anRight,anTop,anBottom];
    Hint := '';
    Shape := bsSpacer;
  end;

  Container := TlqBevel.Create(BorderBevel);
  with Container do
  begin
    Name := 'Container';
    SetPosition(8, 8, 377, 257);
    Anchors := [anLeft,anRight,anTop,anBottom];
    Hint := '';
    Style := bsLowered;
    Shape := bsSpacer;
  end;

  btnExternal := TlqButton.Create(self);
  with btnExternal do
  begin
    Name := 'btnExternal';
    SetPosition(12, 40, 80, 24);
    Text := 'External';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 1;
    OnClick := @btnExternalClicked;
  end;

  btnEmbed1 := TlqButton.Create(self);
  with btnEmbed1 do
  begin
    Name := 'btnEmbed1';
    SetPosition(96, 40, 80, 24);
    Text := 'Embed 1';
    FontDesc := '#Label1';
    Hint := 'Create embedded Form with Visible = True (default behaviour)';
    ImageName := '';
    TabOrder := 2;
    OnClick := @btnEmbed1Clicked;
  end;

  CheckBox1 := TlqCheckBox.Create(self);
  with CheckBox1 do
  begin
    Name := 'CheckBox1';
    SetPosition(296, 40, 93, 20);
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 4;
    Text := 'Visible';
    Enabled := false;
    OnChange  := @CheckBoxChanged;
  end;

  MainMenu := TlqMenubar.Create(self);
  with MainMenu do
  begin
    Name := 'MainMenu';
    SetPosition(0, 0, 393, 28);
    Anchors := [anLeft,anRight,anTop];
  end;

  mnuFile := TlqPopupMenu.Create(self);
  with mnuFile do
  begin
    Name := 'mnuFile';
    SetPosition(156, 88, 120, 20);
    AddMenuItem('Quit', '', @miQuitClicked);
  end;

  mnuHelp := TlqPopupMenu.Create(self);
  with mnuHelp do
  begin
    Name := 'mnuHelp';
    SetPosition(156, 112, 120, 20);
    AddMenuItem('About...', '', @miHelpAboutClicked);
  end;

  btnEmbed2 := TlqButton.Create(self);
  with btnEmbed2 do
  begin
    Name := 'btnEmbed2';
    SetPosition(180, 40, 80, 24);
    Text := 'Embed 2';
    FontDesc := '#Label1';
    Hint := 'Create embedded Form with Visible = False';
    ImageName := '';
    TabOrder := 7;
    OnClick := @btnEmbed2Clicked;
  end;

  {@VFD_BODY_END: MainForm}
  {%endregion}
  MainMenu.AddMenuItem('File', nil).SubMenu := mnuFile;
  MainMenu.AddMenuItem('Help', nil).SubMenu := mnuHelp;
end;


end.
