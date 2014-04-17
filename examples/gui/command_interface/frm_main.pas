{
  This demonstrates the usage of ICommand and ICommandHolder. They work
  similar to Delphi's TAction classes
}
unit frm_main;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  Classes,
  lq_base,
  lq_main,
  lq_form,
  lq_button,
  lq_memo,
  lq_menu;

type

  TMainForm = class(TlqForm)
  private
    {@VFD_HEAD_BEGIN: MainForm}
    btnAdd: TlqButton;
    memName1: TlqMemo;
    btnQuit: TlqButton;
    MainMenu: TlqMenuBar;
    mnuFile: TlqPopupMenu;
    {@VFD_HEAD_END: MainForm}
    miAdd: TlqMenuItem;
    procedure CommandHandler(Sender: TObject);
  public
    procedure AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

implementation

uses
  lq_command_intf,
  commands;

{@VFD_NEWFORM_IMPL}

{ A single event handler that handles all Command based events. }
procedure TMainForm.CommandHandler(Sender: TObject);
var
  cmd: ICommand;
  holder: ICommandHolder;
begin
  if Supports(Sender, ICommandHolder, holder) then
  begin
    cmd := holder.GetCommand;
    cmd.Execute;
  end;
end;

procedure TMainForm.AfterCreate;
begin
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(293, 236, 284, 254);
  WindowTitle := 'Command Interface Test';
  WindowPosition := wpOneThirdDown;

  btnAdd := TlqButton.Create(self);
  with btnAdd do
  begin
    Name := 'btnAdd';
    SetPosition(204, 36, 75, 24);
    Text := 'Add';
    FontDesc := '#Label1';
    ImageName := '';
    OnClick := @CommandHandler;
  end;

  memName1 := TlqMemo.Create(self);
  with memName1 do
  begin
    Name := 'memName1';
    SetPosition(8, 36, 188, 208);
    FontDesc := '#Edit1';
  end;

  btnQuit := TlqButton.Create(self);
  with btnQuit do
  begin
    Name := 'btnQuit';
    SetPosition(204, 220, 75, 24);
    Text := 'Quit';
    FontDesc := '#Label1';
    ImageName := '';
    OnClick := @CommandHandler;
  end;

  MainMenu := TlqMenuBar.Create(self);
  with MainMenu do
  begin
    Name := 'MainMenu';
    SetPosition(0, 0, 284, 24);
    Anchors := [anLeft,anRight,anTop];
  end;

  mnuFile := TlqPopupMenu.Create(self);
  with mnuFile do
  begin
    Name := 'mnuFile';
    SetPosition(44, 72, 120, 20);
    miAdd := AddMenuItem('Add Item', '', @CommandHandler);
    AddMenuItem('Quit', '', @CommandHandler);
  end;

  {@VFD_BODY_END: MainForm}
  
  MainMenu.AddMenuItem('File', nil).SubMenu := mnuFile;

  // instantiate the Command classes
  btnAdd.SetCommand(TAddCommand.Create(memName1));
  btnQuit.SetCommand(TExitCommand.Create);
  miAdd.SetCommand(btnAdd.GetCommand);  // reuse exist command from btnAdd instance
  // The menu item File|Quit shares the command of btnQuit
  mnuFile.MenuItemByName('Quit').SetCommand(btnQuit.GetCommand);
end;


end.
