unit fra_test;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, lq_base, lq_main, lq_checkbox, lq_button,
  lq_menu, lq_memo, lq_panel;

type

  { Note the tags for the UI Designer. This allows use to visually design
    our frame. }
  TMyFrame = class(TlqFrame)
  private
    {@VFD_HEAD_BEGIN: MyFrame}
    fraCheckBox1: TlqCheckBox;
    fraMenu1: TlqMenuBar;
    Button1: TlqButton;
    Memo1: TlqMemo;
    {@VFD_HEAD_END: MyFrame}
    framnuFile: TlqPopupMenu;
    framnuHelp: TlqPopupMenu;
    procedure miHelpAboutClicked(Sender: TObject);
  public
    destructor Destroy; override;
    procedure AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

implementation

uses
  lq_dialogs;

{@VFD_NEWFORM_IMPL}

procedure TMyFrame.miHelpAboutClicked(Sender: TObject);
begin
  TlqMessageDialog.AbouTlqui('');
end;

destructor TMyFrame.Destroy;
begin
  framnuFile.Free;
  framnuHelp.Free;
  inherited Destroy;
end;

procedure TMyFrame.AfterCreate;
var
  miFile: TlqMenuItem;
  miHelp: TlqMenuItem;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: MyFrame}
  Name := 'MyFrame';
  SetPosition(380, 237, 200, 203);
  WindowTitle := 'MyFrame';
  Hint := '';

  fraCheckBox1 := TlqCheckBox.Create(self);
  with fraCheckBox1 do
  begin
    Name := 'fraCheckBox1';
    SetPosition(8, 40, 120, 20);
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 0;
    Text := 'CheckBox';
  end;

  fraMenu1 := TlqMenuBar.Create(self);
  with fraMenu1 do
  begin
    Name := 'fraMenu1';
    SetPosition(0, 0, 200, 24);
    Anchors := [anLeft,anRight,anTop];
    miFile := AddMenuItem('File', nil);
    AddMenuItem('Edit', nil).Enabled := False;
    miHelp := AddMenuItem('Help', nil);
  end;

  Button1 := TlqButton.Create(self);
  with Button1 do
  begin
    Name := 'Button1';
    SetPosition(104, 164, 80, 24);
    Anchors := [anRight,anBottom];
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
    SetPosition(12, 60, 172, 88);
    Anchors := [anLeft,anRight,anTop,anBottom];
    Hint := '';
    Lines.Add('');
    FontDesc := '#Edit1';
    TabOrder := 3;
  end;

  {@VFD_BODY_END: MyFrame}
  {%endregion}

  { There still seems to be a minor issue with Popup Menus used in a frame. So
    for now the work around is to manually maintain the life of the Popup
    Menus - so Owner is set to nil. }
  framnuFile := TlqPopupMenu.Create(nil);
  with framnuFile do
  begin
    Name := 'framnuFile';
    SetPosition(44, 64, 120, 20);
    AddMenuItem('Open...', '', nil);
    AddMenuItem('-', '', nil);
    AddMenuItem('Save', '', nil);
  end;
  miFile.SubMenu := framnuFile;

  framnuHelp := TlqPopupMenu.Create(nil);
  with framnuHelp do
  begin
    Name := 'framnuHelp';
    SetPosition(44, 64, 120, 20);
    AddMenuItem('About fpGUI...', '', @miHelpAboutClicked);
  end;
  miHelp.SubMenu := framnuHelp;
end;


end.
