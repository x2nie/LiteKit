program menutest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  lq_base,
  lq_main,
  lq_form,
  lq_menu,
  lq_edit,
  lq_panel,
  lq_button,
  lq_dialogs,
  lq_memo;

type
  TMainForm = class(TlqForm)
  private
    {@VFD_HEAD_BEGIN: MainForm}
    FMenuBar: TlqMenuBar;
    StatusBar: TlqPanel;
    Toolbar: TlqBevel;
    btnQuit: TlqButton;
    btnSave: TlqButton;
    btnOpen: TlqButton;
    btnAbout: TlqButton;
    pnlClient: TlqBevel;
    edit1: TlqEdit;
    Memo1: TlqMemo;
    FFileSubMenu: TlqPopupMenu;
    FEditSubMenu: TlqPopupMenu;
    FEditSelectSubMenu: TlqPopupMenu;
    FViewSubMenu: TlqPopupMenu;
    FDisabledSubMenu: TlqPopupMenu;
    FHelpSubMenu: TlqPopupMenu;
    Bevel1: TlqBevel;
    Bevel2: TlqBevel;
    {@VFD_HEAD_END: MainForm}
    procedure   miExitClicked(Sender: TObject);
    procedure   miMenuItemSelected(Sender: TObject);
    procedure   miMenuItemChecked(Sender: TObject);
    procedure   miToolBarChecked(Sender: TObject);
    procedure   miStatusBarChecked(Sender: TObject);
    procedure   btnAboutClicked(Sender: TObject);
    procedure   Log(const AText: TlqString);
  public
    procedure   AfterCreate; override;
  end;


{ TMainForm }

procedure TMainForm.miExitClicked(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.miMenuItemSelected(Sender: TObject);
begin
  if Sender is TlqMenuItem then
    Log('Menu clicked: ' + TlqMenuItem(Sender).Text);
end;

procedure TMainForm.miMenuItemChecked(Sender: TObject);
begin
  TlqMenuItem(Sender).Checked := not TlqMenuItem(Sender).Checked;
  Log('Check Menu item toggled');
end;

procedure TMainForm.miToolBarChecked(Sender: TObject);
begin
  TlqMenuItem(Sender).Checked := not TlqMenuItem(Sender).Checked;
  ToolBar.Visible := not ToolBar.Visible;
  Realign;
  Log('Check Menu for Toolbar toggled');
end;

procedure TMainForm.miStatusBarChecked(Sender: TObject);
begin
  TlqMenuItem(Sender).Checked := not TlqMenuItem(Sender).Checked;
  StatusBar.Visible := not StatusBar.Visible;
  Realign;
  Log('Check Menu for Statusbar toggled');
end;

procedure TMainForm.btnAboutClicked(Sender: TObject);
begin
  TlqMessageDialog.AbouTlqui;
end;

procedure TMainForm.Log(const AText: TlqString);
begin
  Memo1.Lines.Add(AText);
end;

procedure TMainForm.AfterCreate;
var
  mi: TlqMenuItem;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(402, 189, 400, 200);
  WindowTitle := 'Menu Test';
  Hint := '';
  WindowPosition := wpOneThirdDown;
  MinWidth := 300;
  MinHeight := 100;

  FMenuBar := TlqMenuBar.Create(self);
  with FMenuBar do
  begin
    Name := 'FMenuBar';
    SetPosition(0, 0, 400, 24);
    Align := alTop;
  end;

  StatusBar := TlqPanel.Create(self);
  with StatusBar do
  begin
    Name := 'StatusBar';
    SetPosition(0, 176, 400, 24);
    Alignment := taLeftJustify;
    FontDesc := '#Label1';
    Hint := '';
    Style := bsLowered;
    Text := 'This is the status bar...';
    Align := alBottom;
  end;

  Toolbar := TlqBevel.Create(self);
  with Toolbar do
  begin
    Name := 'Toolbar';
    SetPosition(0, 24, 400, 29);
    Hint := '';
    Style := bsLowered;
    Shape := bsBottomLine;
    Align := alTop;
  end;

  btnQuit := TlqButton.Create(Toolbar);
  with btnQuit do
  begin
    Name := 'btnQuit';
    SetPosition(4, 2, 24, 24);
    Text := '';
    Flat := True;
    FontDesc := '#Label1';
    Hint := '';
    ImageMargin := -1;
    ImageName := 'stdimg.quit';
    TabOrder := 1;
    OnClick := @miExitClicked;
    Focusable := False;
  end;

  btnSave := TlqButton.Create(Toolbar);
  with btnSave do
  begin
    Name := 'btnSave';
    SetPosition(64, 2, 24, 24);
    Text := '';
    Flat := True;
    FontDesc := '#Label1';
    Hint := '';
    ImageMargin := -1;
    ImageName := 'stdimg.save';
    TabOrder := 2;
    Focusable := False;
  end;

  btnOpen := TlqButton.Create(Toolbar);
  with btnOpen do
  begin
    Name := 'btnOpen';
    SetPosition(40, 2, 24, 24);
    Text := '';
    Flat := True;
    FontDesc := '#Label1';
    Hint := '';
    ImageMargin := -1;
    ImageName := 'stdimg.open';
    TabOrder := 3;
    Focusable := False;
  end;

  btnAbout := TlqButton.Create(Toolbar);
  with btnAbout do
  begin
    Name := 'btnAbout';
    SetPosition(100, 2, 24, 24);
    Text := '';
    Flat := True;
    FontDesc := '#Label1';
    Hint := '';
    ImageMargin := -1;
    ImageName := 'stdimg.help';
    TabOrder := 4;
    OnClick := @btnAboutClicked;
    Focusable := False;
  end;

  pnlClient := TlqBevel.Create(self);
  with pnlClient do
  begin
    Name := 'pnlClient';
    SetPosition(56, 56, 244, 116);
    Hint := '';
    Shape := bsSpacer;
    Align := alClient;
  end;

  edit1 := TlqEdit.Create(pnlClient);
  with edit1 do
  begin
    Name := 'edit1';
    SetPosition(8, 4, 100, 24);
    ExtraHint := '';
    Hint := '';
    TabOrder := 6;
    Text := '';
    FontDesc := '#Edit1';
  end;

  Memo1 := TlqMemo.Create(pnlClient);
  with Memo1 do
  begin
    Name := 'Memo1';
    SetPosition(120, 4, 120, 108);
    Anchors := [anLeft,anRight,anTop,anBottom];
    Hint := '';
    Lines.Add('<= Text Edit has a popup menu too.');
    FontDesc := '#Edit1';
    TabOrder := 10;
  end;

  FFileSubMenu := TlqPopupMenu.Create(self);
  with FFileSubMenu do
  begin
    Name := 'FFileSubMenu';
    SetPosition(264, 60, 120, 20);
    AddMenuItem('&Open', 'Ctrl-O', @miMenuItemSelected);
    AddMenuItem('&Save', 'Ctrl-S', @miMenuItemSelected);
    AddMenuItem('S&ave As', 'Ctrl+Shift+S', @miMenuItemSelected);
    AddMenuItem('-', '', nil);
    AddMenuItem('Save && Reload', '', @miMenuItemSelected);
    AddMenuItem('-', '', nil);
    AddMenuItem('&Quit', 'Ctrl-Q', @miExitClicked);
  end;

  FEditSubMenu := TlqPopupMenu.Create(self);
  with FEditSubMenu do
  begin
    Name := 'FEditSubMenu';
    SetPosition(264, 80, 120, 20);
    AddMenuItem('&Cut', 'Ctrl-X', @miMenuItemSelected);
    AddMenuItem('C&opy', 'Ctrl-C', @miMenuItemSelected);
    AddMenuItem('&Paste', 'Ctrl-V', @miMenuItemSelected);
    AddMenuItem('-', '', nil);
    AddMenuItem('&Spell check', 'F4', @miMenuItemSelected).Enabled := False;
  end;

  FEditSelectSubMenu := TlqPopupMenu.Create(self);
  with FEditSelectSubMenu do
  begin
    Name := 'FEditSelectSubMenu';
    SetPosition(264, 100, 120, 20);
    AddMenuItem('Select All', '', @miMenuItemSelected);
    AddMenuItem('Select Word', '', @miMenuItemSelected);
    AddMenuItem('Select Line', '', @miMenuItemSelected);
    FEditSubMenu.AddMenuItem('Selec&t', '', nil).SubMenu := FEditSelectSubMenu;
  end;

  FViewSubMenu := TlqPopupMenu.Create(self);
  with FViewSubMenu do
  begin
    Name := 'FViewSubMenu';
    SetPosition(264, 120, 120, 20);
    AddMenuItem('Full Screen', '', @miMenuItemChecked);
    AddMenuItem('Tool Bar', '', @miToolBarChecked).Checked := True;
    AddMenuItem('Status Bar', '', @miStatusBarChecked).Checked := True;
    AddMenuItem('Line Numbers', '', @miMenuItemChecked);
  end;

  FHelpSubMenu := TlqPopupMenu.Create(self);
  with FHelpSubMenu do
  begin
    Name := 'FHelpSubMenu';
    SetPosition(264, 140, 120, 20);
    AddMenuItem('&About', 'F12', @btnAboutClicked);
    AddMenuItem('Test Russian text -> Òåñò', '', @miMenuItemSelected);
  end;

  FDisabledSubMenu := TlqPopupMenu.Create(self);
  with FDisabledSubMenu do
  begin
    Name := 'FDisabledSubMenu';
    SetPosition(264, 160, 120, 20);
    AddMenuItem('I''m not enabled', '', nil);
  end;

  Bevel1 := TlqBevel.Create(Toolbar);
  with Bevel1 do
  begin
    Name := 'Bevel1';
    SetPosition(32, 2, 5, 24);
    Hint := '';
    Style := bsLowered;
    Shape := bsLeftLine;
  end;

  Bevel2 := TlqBevel.Create(Toolbar);
  with Bevel2 do
  begin
    Name := 'Bevel2';
    SetPosition(92, 2, 5, 24);
    Hint := '';
    Style := bsLowered;
    Shape := bsLeftLine;
  end;

  {@VFD_BODY_END: MainForm}
  {%endregion}

  // Attach sub menus to main menu bar
  FMenuBar.AddMenuItem('&File', nil).SubMenu := FFileSubMenu;
  FMenuBar.AddMenuItem('&Edit', nil).SubMenu := FEditSubMenu;
  FMenuBar.AddMenuItem('&View', nil).SubMenu := FViewSubMenu;
  mi := FMenuBar.AddMenuItem('&Disabled', nil);
  mi.Enabled := False;
  mi.SubMenu := FDisabledSubMenu;
  FMenuBar.AddMenuItem('&Help', nil).SubMenu := FHelpSubMenu;
end;


procedure MainProc;
var
  frm: TMainForm;
begin
  lqApplication.Initialize;
  frm := TMainForm.Create(nil);
  frm.Show;
  lqApplication.Run;
  frm.Free;
end;

begin
  MainProc;
end.


