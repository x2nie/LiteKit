{
    LiteKit  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2010 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing LiteKit.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Defines a MainMenu Bar, Popup Menu and Menu Item controls.
}

unit lq_menu;

{$mode objfpc}{$H+}

{.$Define DEBUG}

{
  TODO:
    * Refactor the HotKey painting code into Canvas.DrawString so that other
      widgets like TlqButton could also use it.
    * Global keyboard activation of menu items are still missing.
}

interface

uses
  Classes,
  SysUtils,
  lq_base,
  lq_main,
  lq_widget,
  lq_popupwindow,
  lq_stringutils,
  lq_command_intf;
  
type
  TlqHotKeyDef = string;
  
  TlqMenuOption = (mnuo_autoopen,          // auto open menus when mouse over menubar
                    mnuo_nofollowingmouse   // don't auto open new menus as mouse moves over menubar
                    );
  
  TlqMenuOptions = set of TlqMenuOption;
  
  // forward declarations
  TlqPopupMenu = class;
  TlqMenuBar = class;
  
  
  TlqMenuItem = class(TlqComponent, ICommandHolder)
  private
    FCommand: ICommand;
    FEnabled: boolean;
    FHint: TlqString;
    FHotKeyDef: TlqHotKeyDef;
    FOnClick: TNotifyEvent;
    FSeparator: boolean;
    FSubMenu: TlqPopupMenu;
    FText: TlqString;
    FVisible: boolean;
    FChecked: boolean;
    procedure   SetEnabled(const AValue: boolean);
    procedure   SetHotKeyDef(const AValue: TlqHotKeyDef);
    procedure   SetSeparator(const AValue: boolean);
    procedure   SetText(const AValue: TlqString);
    procedure   SetVisible(const AValue: boolean);
    procedure   SetChecked(const AValue: boolean);
  public
    constructor Create(AOwner: TComponent); override;
    procedure   Click;
    function    Selectable: boolean;
    function    GetAccelChar: string;
    procedure   DrawText(ACanvas: TlqCanvas; x, y: TlqCoord; const AImgWidth: integer);
    function    GetCommand: ICommand;
    procedure   SetCommand(ACommand: ICommand);
    property    Checked: boolean read FChecked write SetChecked;
    property    Text: TlqString read FText write SetText;
    property    Hint: TlqString read FHint write FHint;
    property    HotKeyDef: TlqHotKeyDef read FHotKeyDef write SetHotKeyDef;
    property    Separator: boolean read FSeparator write SetSeparator;
    property    Visible: boolean read FVisible write SetVisible;
    property    Enabled: boolean read FEnabled write SetEnabled;
    property    SubMenu: TlqPopupMenu read FSubMenu write FSubMenu;
    property    OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;
  
  
  // Actual Menu Items are stored in TComponent's Components property
  // Visible only items are stored in FItems just before a paint
  TlqPopupMenu = class(TlqPopupWindow)
  private
    FBeforeShow: TNotifyEvent;
    FMargin: TlqCoord;
    FTextMargin: TlqCoord;
    procedure   DoSelect;
    procedure   CloseSubmenus;
    function    GetItemPosY(index: integer): integer;
    function    CalcMouseRow(y: integer): integer;
    function    VisibleCount: integer;
    function    VisibleItem(ind: integer): TlqMenuItem;
    function    MenuFocused: boolean;
    function    SearchItemByAccel(s: string): integer;
  protected
    FMenuFont: TlqFont;
    FMenuAccelFont: TlqFont;
    FMenuDisabledFont: TlqFont;
    FSymbolWidth: integer;
    FItems: TList;
    FFocusItem: integer;
    procedure   HandleMouseEnter; override;
    procedure   HandleMouseExit; override;
    procedure   HandleMouseMove(x, y: integer; btnstate: word; shiftstate: TShiftState); override;
    procedure   HandleLMouseDown(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleLMouseUp(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); override;
    procedure   HandlePaint; override;
    procedure   HandleShow; override;
    procedure   HandleClose; override;
    procedure   DrawItem(mi: TlqMenuItem; rect: TlqRect; AFlags: TlqMenuItemFlags); virtual;
    procedure   DrawRow(line: integer; const AItemFocused: boolean); virtual;
    function    ItemHeight(mi: TlqMenuItem): integer; virtual;
    procedure   PrepareToShow;
    procedure   DoKeyShortcut(const AOrigin: TlqWidget; const keycode: word; const shiftstate: TShiftState; var consumed: boolean; const IsChildOfOrigin: boolean = False); override;
  public
    OpenerPopup: TlqPopupMenu;
    OpenerMenuBar: TlqMenuBar;
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   Close; override;
    function    AddMenuItem(const AMenuName: TlqString; const hotkeydef: string; OnClickProc: TNotifyEvent): TlqMenuItem;
    procedure   AddSeparator;
    function    MenuItemByName(const AMenuName: TlqString): TlqMenuItem;
    function    MenuItem(const AMenuPos: integer): TlqMenuItem;  // added to allow for localization
    property    BeforeShow: TNotifyEvent read FBeforeShow write FBeforeShow;
  end;
  
  
  // Actual Menu Items are stored in TComponents Components property
  // Visible only items are stored in FItems just before a paint
  TlqMenuBar = class(TlqWidget)
  private
    FBeforeShow: TNotifyEvent;
    FLightColor: TlqColor;
    FDarkColor: TlqColor;
    FMenuOptions: TlqMenuOptions;
    FPrevFocusItem: integer;
    FFocusItem: integer;
    FClicked: Boolean;
    FMouseIsOver: boolean;  { So we know when PopupMenu's close, if we should redraw bar }
    FLastItemClicked: integer;
    procedure   SetFocusItem(const AValue: integer);
    procedure   DoSelect;
    procedure   CloseSubmenus;
    function    ItemWidth(mi: TlqMenuItem): integer;
    procedure   InternalReset;
  protected
    FItems: TList;  // stores visible items only
    property    FocusItem: integer read FFocusItem write SetFocusItem;
    procedure   PrepareToShow;
    function    VisibleCount: integer;
    function    VisibleItem(ind: integer): TlqMenuItem;
    procedure   HandleShow; override;
    procedure   HandleMouseMove(x, y: integer; btnstate: word; shiftstate: TShiftState); override;
    procedure   HandleLMouseDown(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); override;
    procedure   HandlePaint; override;
    procedure   HandleMouseEnter; override;
    procedure   HandleMouseExit; override;
    function    CalcMouseCol(x: integer): integer;
    function    GetItemPosX(index: integer): integer;
    function    MenuFocused: boolean;
    function    SearchItemByAccel(s: string): integer;
    procedure   ActivateMenu;
    procedure   DeActivateMenu;
    procedure   DrawColumn(col: integer; focus: boolean); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    function    AddMenuItem(const AMenuTitle: string; OnClickProc: TNotifyEvent): TlqMenuItem;
    function    MenuItem(const AMenuPos: integer): TlqMenuItem;  // added to allow for localization
    property    MenuOptions: TlqMenuOptions read FMenuOptions write FMenuOptions;
    property    BeforeShow: TNotifyEvent read FBeforeShow write FBeforeShow;
  end;


function CreateMenuBar(AOwner: TlqWidget; x, y, w, h: TlqCoord): TlqMenuBar; overload;
function CreateMenuBar(AOwner: TlqWidget): TlqMenuBar; overload;


implementation
  
var
  uFocusedPopupMenu: TlqPopupMenu;

const
  cImgWidth: integer = 16;

  
function CreateMenuBar(AOwner: TlqWidget; x, y, w, h: TlqCoord): TlqMenuBar;
begin
  if AOwner = nil then
    raise Exception.Create('MenuBar component must have an Owner assigned');
  Result       := TlqMenuBar.Create(AOwner);
  Result.Left  := x;
  Result.Top   := y;
  if w = 0 then
    Result.Width := AOwner.Width
  else
    Result.Width := w;
  if h > 0 then
    Result.Height := h;
end;

function CreateMenuBar(AOwner: TlqWidget): TlqMenuBar;
begin
  Result := CreateMenuBar(AOwner, 0, 0, 0, 0);
end;


{ TlqMenuItem }

procedure TlqMenuItem.SetText(const AValue: TlqString);
begin
  if FText = AValue then
    Exit; //==>
  FText := AValue;
end;

procedure TlqMenuItem.SetVisible(const AValue: boolean);
begin
  if FVisible=AValue then exit;
  FVisible:=AValue;
end;

procedure TlqMenuItem.SetChecked(const AValue: boolean);
begin
  if FChecked = AValue then exit;
  FChecked := AValue;
end;

procedure TlqMenuItem.SetHotKeyDef(const AValue: TlqHotKeyDef);
begin
  if FHotKeyDef=AValue then exit;
  FHotKeyDef:=AValue;
end;

procedure TlqMenuItem.SetEnabled(const AValue: boolean);
begin
  if FEnabled=AValue then exit;
  FEnabled:=AValue;
end;

procedure TlqMenuItem.SetSeparator(const AValue: boolean);
begin
  if FSeparator=AValue then exit;
  FSeparator:=AValue;
end;

constructor TlqMenuItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Text := '';
  HotKeyDef := '';
  FSeparator := False;
  FVisible := True;
  FEnabled := True;
  FChecked := False;
  FSubMenu := nil;
  FOnClick := nil;
end;

procedure TlqMenuItem.Click;
begin
  if Assigned(FCommand) then    // ICommand takes preference over OnClick
    FCommand.Execute
  else if Assigned(FOnClick) then
    FOnClick(self);
end;

function TlqMenuItem.Selectable: boolean;
begin
  Result := Enabled and Visible and (not Separator);
end;

function TlqMenuItem.GetAccelChar: string;
var
  p: integer;
begin
  p := UTF8Pos('&', Text);
  if p > 0 then
  begin
    Result := UTF8Copy(Text, p+1, 1);
  end
  else
    Result := '';
end;

procedure TlqMenuItem.DrawText(ACanvas: TlqCanvas; x, y: TlqCoord; const AImgWidth: integer);
var
  s: string;
  p: integer;
  achar: string;
begin
  if not Enabled then
    ACanvas.SetFont(lqStyle.MenuDisabledFont)
  else
    ACanvas.SetFont(lqStyle.MenuFont);

  achar := '&';
  s := Text;

  repeat
    p := UTF8Pos(achar, s);
    if p > 0 then
    begin
      // first part of text before the & sign
      lqStyle.DrawString(ACanvas, x, y, UTF8Copy(s, 1, p-1), Enabled);

      inc(x, lqStyle.MenuFont.TextWidth(UTF8Copy(s, 1, p-1)));
      if UTF8Copy(s, p+1, 1) = achar then
      begin
        // Do we need to paint a actual & sign (create via && in item text)
        lqStyle.DrawString(ACanvas, x, y, achar, Enabled);
        inc(x, lqStyle.MenuFont.TextWidth(achar));
      end
      else
      begin
        // Draw the HotKey text
        if Enabled then
          ACanvas.SetFont(lqStyle.MenuAccelFont);
        lqStyle.DrawString(ACanvas, x, y, UTF8Copy(s, p+1, 1), Enabled);
        inc(x, ACanvas.Font.TextWidth(UTF8Copy(s, p+1, 1)));
        if Enabled then
          ACanvas.SetFont(lqStyle.MenuFont);
      end;
      s := UTF8Copy(s, p+2, UTF8Length(s));
    end;  { if }
  until p < 1;

  // Draw the remaining text after the & sign
  if UTF8Length(s) > 0 then
    lqStyle.DrawString(ACanvas, x, y, s, Enabled);
end;

function TlqMenuItem.GetCommand: ICommand;
begin
  Result := FCommand;
end;

procedure TlqMenuItem.SetCommand(ACommand: ICommand);
begin
  FCommand := ACommand;
end;

{ TlqMenuBar }

procedure TlqMenuBar.SetFocusItem(const AValue: integer);
begin
  if FFocusItem = AValue then
    Exit;
  FPrevFocusItem := FFocusItem;
  FFocusItem := AValue;
  Repaint;
end;

procedure TlqMenuBar.PrepareToShow;
var
  n: integer;
  mi: TlqMenuItem;
begin
  if Assigned(FBeforeShow) then
    FBeforeShow(self);

  FItems.Count := 0;
  // Collecting visible items
  for n := 0 to ComponentCount-1 do
  begin
    if Components[n] is TlqMenuItem then
    begin
      mi := TlqMenuItem(Components[n]);
      if mi.Visible then
        FItems.Add(mi);
    end;
  end;
end;

function TlqMenuBar.VisibleCount: integer;
begin
  Result := FItems.Count;
end;

function TlqMenuBar.VisibleItem(ind: integer): TlqMenuItem;
begin
  if (ind < 0) or (ind > FItems.Count-1) then
    Result := nil
  else
    Result := TlqMenuItem(FItems.Items[ind]);
end;

procedure TlqMenuBar.HandleShow;
begin
  PrepareToShow;
  inherited HandleShow;
end;

procedure TlqMenuBar.HandleMouseMove(x, y: integer; btnstate: word; shiftstate: TShiftState);
var
  newf: integer;
begin
  inherited HandleMouseMove(x, y, btnstate, shiftstate);

  newf := CalcMouseCol(x);

  // process menu options
  if mnuo_nofollowingmouse in FMenuOptions then
  begin
    if not MenuFocused then
      Exit; //==>
  end
  else if mnuo_autoopen in FMenuOptions then
  begin
//    if not Focused then
    FLastItemClicked := newf;
    FClicked := True;
    ActivateMenu;
  end
  else
  begin
    if not FClicked then
      exit
    else
      FLastItemClicked := newf;
  end;

  if not VisibleItem(newf).Selectable then
    Exit; //==>

  if newf = FFocusItem then
    Exit; //==>

  FocusItem := newf;
  // continue processing menu options
  if mnuo_autoopen in FMenuOptions then
    DoSelect
  else
  begin
    Repaint;
    if not MenuFocused then
      DoSelect;
  end
end;

procedure TlqMenuBar.HandleLMouseDown(x, y: integer; shiftstate: TShiftState);
var
  newf: integer;
begin
  inherited HandleLMouseDown(x, y, shiftstate);
  
  if ComponentCount = 0 then
    Exit; // We have no menu items in MainMenu.
    
  newf := CalcMouseCol(x);
  if (FLastItemClicked <> -1) and (FLastItemClicked <> newf) then
  begin
    // do nothing
    //FClicked := not FClicked
  end
  else
  begin
    if VisibleItem(newf).Selectable then
      FClicked := not FClicked;
  end;

  if FClicked then
  begin
    ActivateMenu;
    FLastItemClicked := newf;
  end
  else
  begin
    CloseSubmenus;
    DeActivateMenu;
    FLastItemClicked := -1;
    FocusItem := -1;
    exit; //==>
  end;

  if newf <> FFocusItem then
    FocusItem := newf;

  if not VisibleItem(newf).Selectable then
    Exit; //==>

  DoSelect;
end;

procedure TlqMenuBar.HandleKeyPress(var keycode: word;
  var shiftstate: TShiftState; var consumed: boolean);
var
  s: string;
  i: integer;
  mi: TlqMenuItem;
begin
  s := KeycodeToText(keycode, shiftstate);

  // handle MenuBar (Alt+?) shortcuts only
  if (length(s) = 5) and (copy(s, 1, 4) = 'Alt+') then
  begin
    s := KeycodeToText(keycode, []);
    i := SearchItemByAccel(s);
    if i <> -1 then
    begin
      consumed := True;
      FFocusItem := i;
      DoSelect;
    end;
  end;

  { now process sub-menus off the MenuBar }
  for i := 0 to ComponentCount-1 do
  begin
    if Components[i] is TlqMenuItem then     { these are main menu items }
    begin
      mi := TlqMenuItem(Components[i]);
      if mi.Visible and mi.Enabled then
      begin
        { process the sub menus }
        if mi.SubMenu <> nil then
          mi.SubMenu.DoKeyShortcut(nil, keycode, shiftstate, consumed);
        if consumed then
          Exit;
      end;
    end;
  end;

end;

procedure TlqMenuBar.HandlePaint;
var
  n: integer;
  r: TlqRect;
begin
  Canvas.BeginDraw;

  r.SetRect(0, 0, Width, Height);
  lqStyle.DrawMenuBar(Canvas, r, FBackgroundColor);

  for n := 0 to VisibleCount-1 do
    DrawColumn(n, n = FocusItem);
  Canvas.EndDraw;
end;

procedure TlqMenuBar.HandleMouseEnter;
begin
  inherited HandleMouseEnter;
  FMouseIsOver := True;
end;

procedure TlqMenuBar.HandleMouseExit;
begin
  inherited HandleMouseExit;
  FMouseIsOver := False;
end;

constructor TlqMenuBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems := TList.Create;
  FBeforeShow       := nil;
  FFocusItem        := -1;
  FPrevFocusItem    := -1;
  FLastItemClicked  := -1;
  FFocusable        := True;
  FClicked          := False;
  FBackgroundColor  := Parent.BackgroundColor;
  FTextColor        := Parent.TextColor;
  // calculate the best height based on font
  FHeight := lqStyle.MenuFont.Height + 6; // 3px margin top and bottom
  FMenuOptions := [];
  FMouseIsOver := False;
end;

destructor TlqMenuBar.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

function TlqMenuBar.ItemWidth(mi: TlqMenuItem): integer;
begin
  Result := lqStyle.MenuFont.TextWidth(mi.Text) + (2*6);
end;

procedure TlqMenuBar.InternalReset;
begin
  FClicked := False;
  FLastItemClicked := -1;
  FFocusItem := -1;
  Repaint;
end;

procedure TlqMenuBar.DrawColumn(col: integer; focus: boolean);
var
  n: integer;
  r: TlqRect;
  mi: TlqMenuItem;
begin
  r.SetRect(2, 1, 1, Height-4);

  for n := 0 to VisibleCount-1 do  { so we can calculate menu item position }
  begin
    mi := VisibleItem(n);
    r.width := ItemWidth(mi);
    if col = n then
    begin
      if focus then
      begin
        lqStyle.DrawBevel(Canvas, r.left, r.top, r.width, r.height, False);
        Canvas.SetColor(BackgroundColor);
        Canvas.SetTextColor(clMenuText);
      end
      else
      begin
        if mi.Enabled then
        begin
          Canvas.SetColor(BackgroundColor);
          Canvas.SetTextColor(clMenuText);
        end
        else
        begin
          Canvas.SetColor(BackgroundColor);
          Canvas.SetTextColor(clMenuDisabled);
        end;
      end;  { if/else }
      mi.DrawText(Canvas, r.left+4, r.top+1, cImgWidth);
      Exit; //==>
    end;  { if col=n }
    inc(r.Left, r.width);
  end;  { for }
end;

function TlqMenuBar.CalcMouseCol(x: integer): integer;
var
  w: integer;
  n: integer;
begin
  Result := 0;
  w := 0;
  n := 0;
  while (w <= x) and (n < VisibleCount) do
  begin
    Result := n;
    inc(w, ItemWidth(VisibleItem(n)));
    inc(n);
  end;
end;

function TlqMenuBar.GetItemPosX(index: integer): integer;
var
  n: integer;
begin
  Result := 0;
  if index < 0 then
    Exit; //==>
  n := 0;
  while (n < VisibleCount) and (n < index) do
  begin
    Inc(result, ItemWidth(VisibleItem(n)));
    inc(n);
  end;
end;

procedure TlqMenuBar.DoSelect;
var
  mi: TlqMenuItem;
begin
  mi := VisibleItem(FocusItem);
  CloseSubMenus;  // deactivates menubar!

  if mi.SubMenu <> nil then
  begin
    ActivateMenu;
    // showing the submenu
    mi.SubMenu.ShowAt(self, GetItemPosX(FocusItem)+2, lqStyle.MenuFont.Height+4);
    mi.SubMenu.OpenerPopup      := nil;
    mi.SubMenu.OpenerMenuBar    := self;
    mi.SubMenu.DontCloseWidget  := self;
    uFocusedPopupMenu           := mi.SubMenu;
    RePaint;
  end
  else
  begin
    VisibleItem(FocusItem).Click;
    DeActivateMenu;
  end;
end;

procedure TlqMenuBar.CloseSubmenus;
var
  n: integer;
begin
  // Close all previous popups
  for n := 0 to VisibleCount-1 do
    with VisibleItem(n) do
    begin
      if (SubMenu <> nil) and (SubMenu.HasHandle) then
        SubMenu.Close;
    end;
end;

function TlqMenuBar.MenuFocused: boolean;
var
  n: integer;
  mi: TlqMenuItem;
begin
  Result := True;
  for n := 0 to VisibleCount-1 do
  begin
    mi := VisibleItem(n);
    if (mi.SubMenu <> nil) and (mi.SubMenu.HasHandle) then
    begin
      Result := False;
      Break;
    end;
  end;
end;

function TlqMenuBar.SearchItemByAccel(s: string): integer;
var
  n: integer;
begin
  Result := -1;
  for n := 0 to VisibleCount-1 do
  begin
    with VisibleItem(n) do
    begin
      {$Note Should UpperCase take note of UTF-8? }
      if Enabled and (UpperCase(s) = UpperCase(GetAccelChar)) then
      begin
        Result := n;
        Exit; //==>
      end;
    end;
  end;
end;

procedure TlqMenuBar.DeActivateMenu;
begin
  Parent.ActiveWidget := nil;
  if not FMouseIsOver then
    InternalReset;
end;

procedure TlqMenuBar.ActivateMenu;
begin
  Parent.ActiveWidget := self;
end;

function TlqMenuBar.AddMenuItem(const AMenuTitle: string; OnClickProc: TNotifyEvent): TlqMenuItem;
begin
  Result := TlqMenuItem.Create(self);
  Result.Text       := AMenuTitle;
  Result.HotKeyDef  := '';
  Result.OnClick    := OnClickProc;
  Result.Separator  := False;
end;

function TlqMenuBar.MenuItem(const AMenuPos: integer): TlqMenuItem;
begin
  Result:= TlqMenuItem(Components[AMenuPos]);
end;


{ TlqPopupMenu }

procedure TlqPopupMenu.DoSelect;
var
  mi: TlqMenuItem;
  op: TlqPopupMenu;
begin
  mi := VisibleItem(FFocusItem);
  if mi.SubMenu <> nil then
  begin
    CloseSubMenus;
    // showing the submenu
    mi.SubMenu.ShowAt(self, Width-5, GetItemPosY(FFocusItem)); // 5 is the menu overlap in pixels
    mi.SubMenu.OpenerPopup := self;
    mi.SubMenu.OpenerMenuBar := OpenerMenuBar;
    uFocusedPopupMenu := mi.SubMenu;
    RePaint;
  end
  else
  begin
    // Close this popup
    Close;
    op := OpenerPopup;
    while op <> nil do
    begin
      if op.HasHandle then
        op.Close;
      op := op.OpenerPopup;
    end;
    // notify menubar that we clicked a menu item
    if Assigned(OpenerMenuBar) then
      OpenerMenuBar.InternalReset;
    VisibleItem(FFocusItem).Click;
    FFocusItem := -1;
  end;  { if/else }

//  if OpenerMenuBar <> nil then
//    OpenerMenuBar.DeActivateMenu;
end;

procedure TlqPopupMenu.CloseSubmenus;
var
  n: integer;
begin
  // Close all previous popups
  for n := 0 to VisibleCount-1 do
  with VisibleItem(n) do
  begin
    if (SubMenu <> nil) and (SubMenu.HasHandle) then
      SubMenu.Close;
  end;
end;

function TlqPopupMenu.GetItemPosY(index: integer): integer;
var
  n: integer;
begin
  Result := 2;
  if index < 0 then
    Exit; //==>
  n := 0;
  while (n < VisibleCount) and (n < index) do
  begin
    Inc(Result, ItemHeight(VisibleItem(n)));
    inc(n);
  end;
end;

procedure TlqPopupMenu.HandleMouseMove(x, y: integer; btnstate: word; shiftstate: TShiftState);
var
  newf: integer;
begin
  inherited HandleMouseMove(x, y, btnstate, shiftstate);

  if not MenuFocused then
    Exit; //==>

  newf := CalcMouseRow(y);
  if newf < 0 then
    Exit; //==>

  if newf = FFocusItem then
    Exit; //==>

  FFocusItem := newf;
  Repaint;
end;

procedure TlqPopupMenu.HandleLMouseDown(x, y: integer; shiftstate: TShiftState);
var
  r: TlqRect;
begin
  inherited HandleLMouseDown(x, y, shiftstate);

  r.SetRect(0, 0, Width, Height);
  if not PtInRect(r, Point(x, y)) then
  begin
    ClosePopups;
    Exit; //==>
  end;
end;

procedure TlqPopupMenu.HandleLMouseUp(x, y: integer; shiftstate: TShiftState);
var
  newf: integer;
  mi: TlqMenuItem;
begin
  inherited HandleLMouseUp(x, y, shiftstate);

  newf := CalcMouseRow(y);
  if newf < 0 then
    Exit;

  if not VisibleItem(newf).Selectable then
    Exit; //==>

  if newf <> FFocusItem then
    FFocusItem := newf;

  mi := VisibleItem(FFocusItem);
  if (mi <> nil) and (not MenuFocused) and (mi.SubMenu <> nil)
      and mi.SubMenu.HasHandle then
    mi.SubMenu.Close
  else
    DoSelect;
end;

procedure TlqPopupMenu.HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean);
var
  oldf: integer;
  i: integer;
  s: string;
  op: TlqPopupMenu;
  trycnt: integer;

  procedure FollowFocus;
  begin
    if oldf <> FFocusItem then
    begin
      Repaint;
    end;
  end;

begin
  inherited HandleKeyPress(keycode, shiftstate, consumed);

  oldf := FFocusItem;

  consumed := true;
  case keycode of
    keyUp:
        begin // up
          trycnt := 2;
          i := FFocusItem-1;
          repeat
            while (i >= 0) and not VisibleItem(i).Selectable do
              dec(i);

            if i >= 0 then
              break;  //==>

            i := VisibleCount-1;
            dec(trycnt);
          until trycnt > 0;

          if i >= 0 then
            FFocusItem := i;
        end;
        
    keyDown:
        begin // down
          trycnt := 2;
          i := FFocusItem+1;
          repeat
            while (i < VisibleCount) and not VisibleItem(i).Selectable do
              inc(i);
            if i < VisibleCount then
              Break;  //==>
            i := 0;
            dec(trycnt);
          until trycnt > 0;

          if i < VisibleCount then
            FFocusItem := i;
        end;
        
    keyReturn:
        begin
          DoSelect;
        end;

    keyLeft:
        begin
          if OpenerMenubar <> nil then
            OpenerMenubar.HandleKeyPress(keycode, shiftstate, consumed);
        end;

    keyRight:
        begin
          if OpenerMenubar <> nil then
            OpenerMenubar.HandleKeyPress(keycode, shiftstate, consumed);
          // VisibleItem(FFocusItem).SubMenu <> nil then DoSelect;
        end;

    keyBackSpace:
        begin
          //if self.OpenerPopup <> nil then
          Close;
        end;

    keyEscape:
        begin
          Close;
          op := OpenerPopup;
          while op <> nil do
          begin
            op.Close;
            op := op.OpenerPopup;
          end;
          if OpenerMenubar <> nil then
            OpenerMenubar.InternalReset;
        end;
  else
    consumed := false;
  end;

  FollowFocus;

  if (not consumed) and ((keycode and $8000) <> $8000) then
  begin
    // normal char
    s := chr(keycode and $00FF) + chr((keycode and $FF00) shr 8);
    i := SearchItemByAccel(s);
    if i >= 0 then
    begin
      FFocusItem := i;
      FollowFocus;
      Consumed := true;
      DoSelect;
    end;
  end;
end;

procedure TlqPopupMenu.HandlePaint;
var
  n: integer;
begin
  Canvas.Clear(BackgroundColor);
//  Canvas.SetColor(clBlack);
//  Canvas.DrawRectangle(0, 0, Width, Height);  // black rectangle border
  Canvas.DrawButtonFace(0, 0, Width, Height, []);  // 3d rectangle inside black border

  for n := 0 to VisibleCount-1 do
    DrawRow(n, n = FFocusItem);
end;

procedure TlqPopupMenu.HandleShow;
begin
  PrepareToShow;
  inherited HandleShow;
end;

procedure TlqPopupMenu.HandleClose;
begin
  {$IFDEF DEBUG}
  writeln(Classname, '.HandleClose');
  {$ENDIF}
  inherited HandleClose;
end;

function TlqPopupMenu.VisibleCount: integer;
begin
  Result := FItems.Count;
end;

function TlqPopupMenu.VisibleItem(ind: integer): TlqMenuItem;
begin
  if (ind < 0) or (ind > FItems.Count-1) then
    Result := nil
  else
    Result := TlqMenuItem(FItems.Items[ind]);
end;

procedure TlqPopupMenu.DrawItem(mi: TlqMenuItem; rect: TlqRect; AFlags: TlqMenuItemFlags);
var
  s: string;
  x: integer;
  img: TlqImage;
  lFlags: TlqMenuItemFlags;
begin
  lFlags := AFlags;
  if mi.Separator then
  begin
    lqStyle.DrawMenuItemSeparator(Canvas, rect);
  end
  else
  begin
    // process Check mark if needed
    if mi.Checked then
    begin
      lFlags := lFlags + [mifChecked];
      lqStyle.DrawMenuItemImage(Canvas, rect.Left, rect.Top, rect, lFlags);
      lFlags := lFlags - [mifChecked];
    end;

    // process menu item Text
    x := rect.Left + FSymbolWidth + FTextMargin;
    mi.DrawText(Canvas, x+cImgWidth, rect.top, cImgWidth);
    Canvas.SetColor(Canvas.TextColor);  // reset text default color

    // process menu item Hot Key text
    if mi.HotKeyDef <> '' then
    begin
      s := mi.HotKeyDef;
      lqStyle.DrawString(Canvas, rect.Right-FMenuFont.TextWidth(s)-FTextMargin, rect.Top, s, mi.Enabled);
    end;

    // process menu item submenu arrow image
    if mi.SubMenu <> nil then
    begin
      lFlags := lFlags + [mifSubMenu];
      lqStyle.DrawMenuItemImage(Canvas, rect.Left, rect.Top, rect, lFlags);
      lFlags := lFlags - [mifSubMenu];
    end;
  end;
end;

procedure TlqPopupMenu.DrawRow(line: integer; const AItemFocused: boolean);
var
  n: integer;
  r: TlqRect;
  mi: TlqMenuItem;
  lFlags: TlqMenuItemFlags;
begin
  r.SetRect(FMargin, FMargin, FWidth-(2*FMargin), FHeight-(2*FMargin));

  for n := 0 to VisibleCount-1 do
  begin
    mi := VisibleItem(n);
    lFlags := [];
    r.height := ItemHeight(mi);

    if line = n then
    begin
      if AItemFocused then
        lFlags := [mifSelected];     // refering to menu item in active popup menu
      if mi.Separator then
        lFlags := lFlags + [mifSeparator];
      if AItemFocused and (not mi.Separator) then
      begin
        if MenuFocused then          // refering to popup menu window
        begin
          lFlags := lFlags + [mifHasFocus];
          Canvas.SetColor(clSelection);
          if mi.Selectable then
          begin
            lFlags := lFlags + [mifEnabled];
            Canvas.SetTextColor(clSelectionText);
          end
          else
            Canvas.SetTextColor(clMenuDisabled);
        end
        else
        begin
          Canvas.SetColor(clShadow1);
          Canvas.SetTextColor(clInactiveSelText);
        end;
      end
      else
      begin
        if mi.Enabled then
        begin
          lFlags := lFlags + [mifEnabled];
          Canvas.SetColor(BackgroundColor);
          Canvas.SetTextColor(clMenuText);
        end
        else
        begin
          Canvas.SetColor(BackgroundColor);
          Canvas.SetTextColor(clMenuDisabled);
        end;
      end;
      lqStyle.DrawMenuRow(Canvas, r, lFlags);
      DrawItem(mi, r, lFlags);

      Exit; //==>
    end;
    inc(r.Top, ItemHeight(mi) );
  end;  { for }
end;

function TlqPopupMenu.ItemHeight(mi: TlqMenuItem): integer;
begin
  if mi.Separator then
    Result := 5
  else
    Result := FMenuFont.Height + 2;
end;

function TlqPopupMenu.MenuFocused: boolean;
begin
  Result := (uFocusedPopupMenu = self);
end;

function TlqPopupMenu.SearchItemByAccel(s: string): integer;
var
  n: integer;
begin
  result := -1;
  for n := 0 to VisibleCount-1 do
  begin
    with VisibleItem(n) do
    begin
      {$Note Do we need to use UTF-8 upper case? }
      if Enabled and (UpperCase(s) = UpperCase(GetAccelChar)) then
      begin
        result := n;
        Exit; //==>
      end;
    end;
  end;
end;

procedure TlqPopupMenu.HandleMouseEnter;
begin
  {$IFDEF DEBUG}
  writeln(Classname, '.HandleMouseEnter');
  {$ENDIF}
  inherited HandleMouseEnter;
end;

procedure TlqPopupMenu.HandleMouseExit;
begin
  {$IFDEF DEBUG}
  writeln(Classname, '.HandleMouseExit');
  {$ENDIF}
  inherited HandleMouseExit;
  FFocusItem := -1;
  Repaint;
end;

// Collecting visible items and measuring sizes
procedure TlqPopupMenu.PrepareToShow;
var
  n: integer;
  h: integer;
  tw: integer;
  hkw: integer;
  x: integer;
  mi: TlqMenuItem;
begin
  if Assigned(FBeforeShow) then
    BeforeShow(self);

  // Collecting visible items
  FItems.Count := 0;

  for n := 0 to ComponentCount-1 do
  begin
    if Components[n] is TlqMenuItem then
    begin
      mi := TlqMenuItem(Components[n]);
      if mi.Visible then
        FItems.Add(mi);
    end;
  end;

  // Measuring sizes
  h             := 0;   // height
  tw            := 0;   // text width
  hkw           := 0;   // hotkey width
  FSymbolWidth  := 0;
  for n := 0 to VisibleCount-1 do
  begin
    mi  := VisibleItem(n);
    x   := ItemHeight(mi);
    inc(h, x);
    x := FMenuFont.TextWidth(mi.Text);
    if tw < x then
      tw := x;

    if mi.SubMenu <> nil then
      x := FMenuFont.Height
    else
      x := FMenuFont.TextWidth(mi.HotKeyDef);
    if hkw < x then
      hkw := x;
  end;

  if hkw > 0 then
    hkw := hkw + 10; // spacing between text and hotkey text

  FHeight := FMargin*2 + h;
  FWidth  := ((FMargin+FTextMargin)*2) + FSymbolWidth + tw + hkw + (cImgWidth*2);
  
  uFocusedPopupMenu := self;
end;

procedure TlqPopupMenu.DoKeyShortcut(const AOrigin: TlqWidget;
  const keycode: word; const shiftstate: TShiftState; var consumed: boolean; const IsChildOfOrigin: boolean = False);
var
  s: TlqString;
  i: integer;
  mi: TlqMenuItem;
begin
  s := KeycodeToText(keycode, shiftstate);
  for i := 0 to ComponentCount-1 do
  begin
    if Components[i] is TlqMenuItem then
    begin
      mi := Components[i] as TlqMenuItem;
      if mi.Separator then
        Continue;
      if mi.Visible and mi.Enabled then
      begin
        { process the sub menus }
        if mi.SubMenu <> nil then
          mi.SubMenu.DoKeyShortcut(nil, keycode, shiftstate, consumed)
        else if mi.HotKeyDef = s then
        begin
          consumed := True;
          mi.Click;
        end;
        if consumed then
          Exit;
      end;
    end;
  end;

end;

function TlqPopupMenu.CalcMouseRow(y: integer): integer;
var
  h: integer;
  n: integer;
begin
  h := 2;
  n := 0;
  Result := n;

  // sanity check
  if y < 0 then
    Exit
  else
    n := 0;
    
  while (h <= y) and (n < VisibleCount) do
  begin
    Result := n;
    inc(h, ItemHeight(VisibleItem(n)));
    inc(n);
  end;
end;

constructor TlqPopupMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMargin     := 3;
  FTextMargin := 3;
  FItems      := TList.Create;

  // fonts
  FMenuFont         := lqStyle.MenuFont;
  FMenuAccelFont    := lqStyle.MenuAccelFont;
  FMenuDisabledFont := lqStyle.MenuDisabledFont;
  FSymbolWidth      := FMenuFont.Height+2;

  FBeforeShow   := nil;
  FFocusItem    := -1;
  OpenerPopup   := nil;
  OpenerMenubar := nil;
end;

destructor TlqPopupMenu.Destroy;
begin
  {$IFDEF DEBUG}
  writeln(Classname, '.Destroy');
  {$ENDIF}
  FItems.Free;
  inherited Destroy;
end;

{$Note See if we can move this to HandleHide + not make Close virtual! }
procedure TlqPopupMenu.Close;
var
  n: integer;
  mi: TlqMenuItem;
begin
  for n := 0 to FItems.Count-1 do
  begin
    mi := TlqMenuItem(FItems[n]);
    if mi.SubMenu <> nil then
    begin
      if mi.SubMenu.HasHandle then
        mi.SubMenu.Close;
    end;
  end;
  inherited Close;
  uFocusedPopupMenu := OpenerPopup;
  if (uFocusedPopupMenu <> nil) and uFocusedPopupMenu.HasHandle then
    uFocusedPopupMenu.RePaint;

  if (OpenerMenuBar <> nil) and OpenerMenuBar.HasHandle then
  begin
    if (OpenerPopup = nil) or not OpenerPopup.HasHandle then
    begin
      OpenerMenuBar.DeActivateMenu;
    end;
    //else
    //OpenerMenuBar.RePaint;
  end;
end;

function TlqPopupMenu.AddMenuItem(const AMenuName: TlqString;
    const hotkeydef: string; OnClickProc: TNotifyEvent): TlqMenuItem;
begin
  result := TlqMenuItem.Create(self);
  if AMenuName <> '-' then
  begin
    result.Text := AMenuName;
    result.hotkeydef := hotkeydef;
    result.OnClick := OnClickProc;
  end
  else
  begin
    result.Separator := true;
  end;
end;

procedure TlqPopupMenu.AddSeparator;
begin
  AddMenuitem('-', '', nil);
end;

function TlqPopupMenu.MenuItemByName(const AMenuName: TlqString): TlqMenuItem;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to ComponentCount-1 do
  begin
    if Components[i] is TlqMenuItem then
      if SameText(TlqMenuItem(Components[i]).Text, AMenuName) then
      begin
        Result := TlqMenuItem(Components[i]);
        Exit; //==>
      end;
  end;
end;

function TlqPopupMenu.MenuItem(const AMenuPos: integer): TlqMenuItem;
begin
  Result:= TlqMenuItem(Components[AMenuPos]);
end;

initialization
  uFocusedPopupMenu := nil;
  
end.

