{
***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
}
unit lq_designer_menu;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, lq_menu, ComponentEditors, //LazarusIDEStrConsts,
  PropEdits;

type

  TlqMenuBarComponentEditor = class;

  { TlqMainMenuEditorForm }

  TlqMainMenuEditorForm = class(TForm)
    Edit1: TEdit;
    Panel1: TPanel;
    Tree_menus: TTreeView;
    procedure FormCreate(Sender: TObject);
    procedure OnPersistentDeleting(APersistent: TPersistent);
    procedure OnPersistentAdded(APersistent: TPersistent; Select: boolean);

  private
    { private declarations }
    FMenu : TlqMenuBar;
    FCE : TlqMenuBarComponentEditor;
    FDesigner: TComponentEditorDesigner;
    procedure UpdateListOfMenus;
  public
    { public declarations }
    procedure SetMenu(NewMenu: TlqMenuBar);
  end;


  { TMenuComponentEditor -- The default component editor for TMenu. }

  TlqMenuBarComponentEditor = class(TComponentEditor)
  private
    FDesigner: TComponentEditorDesigner;
  protected
  public
    constructor Create(AComponent: TComponent;
                       ADesigner: TComponentEditorDesigner); override;
    procedure Edit; override;
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

var
  lqMainMenuEditorForm: TlqMainMenuEditorForm;

procedure Register;

implementation

//uses CustomNonFormDesigner, NonControlDesigner;
{$R *.lfm}

procedure ShowMenuEditor(AMenu: TlqMenuBar; ACE: TlqMenuBarComponentEditor);
begin
  if AMenu=nil then exit;//RaiseGDBException('ShowMenuEditor AMenu=nil');
  if lqMainMenuEditorForm=nil then
    lqMainMenuEditorForm:=TlqMainMenuEditorForm.Create(Application);
  lqMainMenuEditorForm.FCE := ACE; //debug
  lqMainMenuEditorForm.SetMenu(AMenu);
  lqMainMenuEditorForm.ShowOnTop;
end;

{ TlqMainMenuEditorForm }

procedure TlqMainMenuEditorForm.FormCreate(Sender: TObject);
begin
  //Caption:=lisMenuEditorMenuEditor;
  //Panel.Height:=Panel.Parent.Height;
  //Label_menus.Caption:=lisMenuEditorSelectMenu;

  GlobalDesignHook.AddHandlerPersistentDeleting(@OnPersistentDeleting);
  GlobalDesignHook.AddHandlerPersistentAdded(@OnPersistentAdded);
end;

procedure TlqMainMenuEditorForm.OnPersistentDeleting(APersistent: TPersistent);
var
  AComponent: TComponent;
  n : TTreeNode;
begin
  if APersistent is TComponent then
  begin
    AComponent := TComponent(APersistent);
    if FindRootDesigner(AComponent) <> FDesigner then Exit;
    n := Tree_menus.Items.FindNodeWithText(AComponent.Name);
    if n <> nil then Tree_menus.Items.Delete(n);

    if AComponent = FMenu then
      SetMenu(nil);
  end;

end;

procedure TlqMainMenuEditorForm.OnPersistentAdded(APersistent: TPersistent;
  Select: boolean);
begin
  if APersistent is TlqMenuBar then
    UpdateListOfMenus;
end;

procedure TlqMainMenuEditorForm.UpdateListOfMenus;
var
  i: Integer;
  CurComponent: TComponent;
  R : TTreeNode;
begin
  Tree_menus.Items.BeginUpdate;
  Tree_menus.Items.Clear;

  try
    if FDesigner <> nil then
    begin
      if (FMenu <> nil) and (FMenu.Owner <> nil) then
      begin
        //R := Tree_menus.Items.Add(nil, 'Root FMenu.Root ');
        R := nil;
        with FMenu.Owner do //should be TlqForm
        begin
          Tree_menus.Items.AddChild(R, Name + ' |: '+ ClassName);
          for i := 0 to ComponentCount - 1 do
          begin
            CurComponent:= Components[i];
            //debugln('TMainMenuEditorForm.UpdateListOfMenus A ',dbgsName(CurComponent));
            if (CurComponent is TlqMenuBar) {or (CurComponent is TPopupMenu) }then
              Tree_menus.Items.AddChild(R, CurComponent.Name);
          end;
        end;
      end;


    end
    else
        ShowMessage('Gak ada FDesigner coy!');

  finally
    Tree_menus.Items.EndUpdate;
  end;

  if FMenu <> nil then
  begin
    R :=  Tree_menus.Items.FindNodeWithText(FMenu.Name);
    //for i := 0 to Tree_menus.Items.Count - 1 do
    //if (FMenu.Name = Tree_menus.Items[i]) then
    if R <> nil then
        Tree_menus.Selected := R;// .Selected[i] := True;
  end;

end;

procedure TlqMainMenuEditorForm.SetMenu(NewMenu: TlqMenuBar);
begin
  if NewMenu <> FMenu then
  begin
    //FreeAndNil(FDesignerMainMenu);
    FMenu := NewMenu;
    FDesigner := FindRootDesigner(FMenu) as TComponentEditorDesigner;
    UpdateListOfMenus;
    if FMenu <> nil then
    begin
	    {FDesignerMainMenu := TDesignerMainMenu.CreateWithMenu(Self, FMenu);
	    with FDesignerMainMenu do
	    begin
		    Parent := Self;
		    ParentCanvas := Canvas;
		    LoadMainMenu;
		    SetCoordinates(10, 10, 0, FDesignerMainMenu.Root);
	    end;
	    FDesignerMainMenu.Panel := Panel;
	    FDesignerMainMenu.RealignDesigner;}
    end
    else
     Close;
  end;
end;

{ TlqMenuBarComponentEditor}

constructor TlqMenuBarComponentEditor.Create(AComponent: TComponent;
  ADesigner: TComponentEditorDesigner);
begin
  inherited Create(AComponent, ADesigner);
  FDesigner := ADesigner;
end;

procedure TlqMenuBarComponentEditor.Edit;
begin
  ShowMenuEditor(Component as TlqMenuBar, self);
end;

function TlqMenuBarComponentEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

function TlqMenuBarComponentEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Menu Editor...';// lisMenuEditor;
  end;
end;

procedure TlqMenuBarComponentEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: Edit;
  end;
end;

procedure Register;
begin
  RegisterComponentEditor(TlqMenuBar,TlqMenuBarComponentEditor);

  //RegisterPropertyEditor(GetPropInfo(TMenu,'Items')^.PropType, TMenu,'',TMenuItemsPropertyEditor);
end;


end.

