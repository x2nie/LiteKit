{
    LiteKit  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2012 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing LiteKit.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Defines a edit ComboBox control with auto-complete feature.
}

unit lq_editcombo;

{$mode objfpc}{$H+}

{.$Define DEBUG}

{
    ***********************************************************
    **********   This is still under development!   ***********
    ***********************************************************

    It needs lots of testing and debugging.
}


{ TODO: Needs a lot of refactoring to get rid of code duplication. }

{
This is an example of what we can aim for:
You need a mono font to see the correct layout.


               TlqBaseComboBox
              _________|______________
             |                        |
   TlqBaseStaticCombo        TlqBaseEditCombo
       ______|_________               |
      |                |         TlqEditCombo
      |                |
 TlqComboBox   TlqBaseColorCombo
                       |
                 TlqColorComboBox
}

interface

uses
  Classes,
  SysUtils,
  lq_base,
  lq_main,
  lq_widget,
  lq_popupwindow,
  lq_menu,
  lq_combobox;

type
  TAllowNew = (anNo, anYes, anAsk);


  TlqBaseEditCombo = class(TlqBaseComboBox)
  private
    FAutoCompletion: Boolean;
    FAllowNew: TAllowNew;
    //FText: string;
    FSelectedItem: integer;
    FMaxLength: integer;
    FNewItem: boolean;
    FDefaultPopupMenu: TlqPopupMenu;
    procedure   SetAllowNew(const AValue: TAllowNew);
    procedure   InternalBtnClick(Sender: TObject);
    procedure   InternalListBoxSelect(Sender: TObject);
    procedure   InternalListBoxKeyPress(Sender: TObject; var keycode: word; var shiftstate: TShiftState; var consumed: Boolean);
    procedure   DefaultPopupInsertFromCharmap(Sender: TObject);
    procedure   DoPaste(const AText: TlqString);
    procedure   SetDefaultPopupMenuItemsState;
  protected
    FDropDown: TlqPopupWindow;
    FDrawOffset: integer;
    FSelStart: integer;
    FSelOffset: integer;
    FCursorPos: integer;
    procedure   DoDropDown; override;
    function    GetText: string; virtual;
    function    HasText: boolean; virtual;
    procedure   SetText(const AValue: string); override;
    procedure   ShowDefaultPopupMenu(const x, y: integer; const shiftstate: TShiftState); virtual;
    procedure   HandleResize(AWidth, AHeight: TlqCoord); override;
    procedure   HandleKeyChar(var AText: TlqChar; var shiftstate: TShiftState; var consumed: Boolean); override;
    procedure   HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: Boolean); override;
    procedure   HandleLMouseDown(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleLMouseUp(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleRMouseUp(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandlePaint; override;
    property    AutoCompletion: Boolean read FAutocompletion write FAutoCompletion default False;
    property    AllowNew: TAllowNew read FAllowNew write SetAllowNew default anNo;
    property    BackgroundColor default clBoxColor;
    property    TextColor default clText1;
    property    Text: string read GetText write SetText;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   Clear;
    procedure   SetFocus;
    procedure   Update;
    property    NewText: boolean read FNewItem;
  end;


  TlqEditCombo = class(TlqBaseEditCombo)
  published
    property    Align;
    property    AllowNew;
    property    AutoCompletion;
    property    BackgroundColor;
    property    DropDownCount;
    property    ExtraHint;
    property    FocusItem;
    property    FontDesc;
    property    Height;
    property    Hint;
    property    Items;
    property    Margin;
    property    Text;
    property    TextColor;
    property    Width;
    property    OnChange;
    property    OnCloseUp;
    property    OnDropDown;
    property    OnEnter;
    property    OnExit;
    property    OnKeyPress;
    property    OnShowHint;
  end;


function CreateEditCombo(AOwner: TComponent; x, y, w: TlqCoord; AList:TStringList; ACompletion: boolean = False;
      ANew: TAllowNew = anNo; h: TlqCoord = 0): TlqEditCombo;


implementation

uses
  lq_stringutils,
  lq_constants,
  lq_listbox,
  lq_dialogs;

const
  // internal popupmenu item names
  //ipmCut        = 'miDefaultCut';
  //ipmCopy       = 'miDefaultCopy';
  //ipmPaste      = 'miDefaultPaste';
  //ipmClearAll   = 'miDefaultClearAll';
  ipmCharmap    = 'miDefaultCharmap';

var
  OriginalFocusRoot: TlqWidget;

type
  { This is the class representing the dropdown window of the combo box. }
  TDropDownWindow = class(TlqPopupWindow)
  private
    FCallerWidget: TlqWidget;
    ListBox:    TlqListBox;
  protected
    procedure   HandlePaint; override;
    procedure   HandleKeyChar(var AText: TlqChar; var shiftstate: TShiftState; var consumed: Boolean); override;
    procedure   HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); override;
    procedure   HandleShow; override;
    procedure   HandleHide; override;
  public
    constructor Create(AOwner: TComponent); override;
    property    CallerWidget: TlqWidget read FCallerWidget write FCallerWidget;
  end;


{ TDropDownWindow }

procedure TDropDownWindow.HandlePaint;
begin
  Canvas.BeginDraw;
//  inherited HandlePaint;
  Canvas.Clear(clWhite);
  Canvas.EndDraw;
end;

procedure TDropDownWindow.HandleKeyChar(var AText: TlqChar;
  var shiftstate: TShiftState; var consumed: Boolean);
begin
  if TlqEditCombo(FCallerWidget).FAutoCompletion then
    TlqEditCombo(FCallerWidget).HandleKeyChar(AText,shiftstate,consumed);
end;

procedure TDropDownWindow.HandleKeyPress(var keycode: word;
  var shiftstate: TShiftState; var consumed: boolean);
begin
  if TlqEditCombo(FCallerWidget).FAutoCompletion then
  begin
    TlqEditCombo(FCallerWidget).HandleKeyPress(keycode,shiftstate,consumed);
//    consumed:= True;
  end
  else
  begin
    inherited HandleKeyPress(keycode, shiftstate, consumed);
    if keycode = keyEscape then
    begin
      consumed := True;
      Close;
    end;
  end;
end;

procedure TDropDownWindow.HandleShow;
begin
  ListBox.SetPosition(0, 0, Width, Height);
  inherited HandleShow;
  ActiveWidget := ListBox;
end;

procedure TDropDownWindow.HandleHide;
begin
  // HandleHide also gets called in TlqWidget.Destroy so we need a few
  // if Assigned() tests here. This should be improved on.
//  if Assigned(FocusRootWidget) then
//    FocusRootWidget.ReleaseMouse;  // for internal ListBox

  if Assigned(CallerWidget) then
    CallerWidget.SetFocus;
  inherited HandleHide;
end;

constructor TDropDownWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ListBox := TlqListBox.Create(self);
  ListBox.PopupFrame := True;
end;

function CreateEditCombo(AOwner: TComponent; x, y, w: TlqCoord; AList:TStringList; ACompletion: boolean = False;
      ANew: TAllowNew = anNo; h: TlqCoord = 0): TlqEditCombo;
begin
  Result           := TlqEditCombo.Create(AOwner);
  Result.Left      := x;
  Result.Top       := y;
  Result.Width     := w;
  Result.Focusable := True;
  Result.AutoCompletion := ACompletion;
  Result.AllowNew       := ANew;
  if h < TlqEditCombo(Result).Font.Height + (Result.FMargin * 2) then
    Result.Height := TlqEditCombo(Result).Font.Height + (Result.FMargin * 2)
  else
    Result.Height:= h;

  if Assigned(AList) then
    Result.Items.Assign(AList);
end;

{ TlqBaseEditCombo }

procedure TlqBaseEditCombo.SetAllowNew(const AValue: TAllowNew);
begin
  if FAllowNew <> AValue then
    FAllowNew := AValue;
end;

function TlqBaseEditCombo.GetText: string;
begin
  if FAutoCompletion then
    Result := FText
  else
    if (FocusItem >= 0) and (FocusItem <= FItems.Count-1) then
      Result := FItems.Strings[FocusItem]
    else
      Result := '';
end;

function TlqBaseEditCombo.HasText: boolean;
begin
  Result := FFocusItem >= 0;
end;

procedure TlqBaseEditCombo.DoDropDown;
var
  ddw: TDropDownWindow;
  rowcount, i: integer;
  r: TlqRect;
begin
  if (not Assigned(FDropDown)) or (not FDropDown.HasHandle) then
  begin
    FreeAndNil(FDropDown);
    OriginalFocusRoot := FocusRootWidget;
    FDropDown := TDropDownWindow.Create(nil);
    ddw := TDropDownWindow(FDropDown);
    ddw.Width := Width;
    ddw.CallerWidget := self;
    ddw.ListBox.OnSelect := @InternalListBoxSelect;
    ddw.ListBox.OnKeyPress := @InternalListBoxKeyPress;

    // Assign combobox text items to internal listbox
    if FAutoCompletion then
    begin
      for i := 0 to FItems.Count-1 do
        if SameText(UTF8Copy(FItems.Strings[i], 1, UTF8Length(FText)), FText) then
          ddw.ListBox.Items.Add(FItems.Strings[i]);
    end
    else
      ddw.ListBox.Items.Assign(FItems);

    // adjust the height of the dropdown
    rowcount := ddw.ListBox.Items.Count;
    if rowcount > DropDownCount then
      rowcount := DropDownCount;
    if rowcount < 1 then
      rowcount := 1;
    ddw.Height := (ddw.ListBox.RowHeight * rowcount) + 4;
    ddw.ListBox.Height := ddw.Height;   // needed in follow focus, otherwise, the default value (80) is used

    // set default focusitem
    ddw.ListBox.FocusItem := FFocusItem;

    ddw.DontCloseWidget := self;  // now we can control when the popup window closes
    r := GetDropDownPos(Parent, self, ddw);
    ddw.Height := r.Height;

    if (FItems.Count > 0) then
      DoOnDropDown;
    ddw.OnClose := @InternalOnClose;

    ddw.ShowAt(Parent, r.Left, r.Top);
  end
  else
  begin
    FBtnPressed := False;
    ddw := TDropDownWindow(FDropDown);
    ddw.Close;
    FreeAndNil(FDropDown);
  end;
end;

procedure TlqBaseEditCombo.InternalBtnClick(Sender: TObject);
begin
  DoDropDown;
end;

procedure TlqBaseEditCombo.InternalListBoxSelect(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to Items.Count-1 do
  begin
    if Items[i]= TDropDownWindow(FDropDown).ListBox.Items[TDropDownWindow(FDropDown).ListBox.FocusItem] then
    begin
      FocusItem := i;
      FSelectedItem:= i;
      FText:= Items[i];
      Break;
    end;
  end;
  FDropDown.Close;
  //Repaint will check if Handle is created
  Repaint;
end;

procedure TlqBaseEditCombo.InternalListBoxKeyPress(Sender: TObject; var keycode: word;
          var shiftstate: TShiftState; var consumed: Boolean);
var
  i: Integer;
begin
  if ((keycode = keyUp) or (keycode = keyDown)) and (TDropDownWindow(FDropDown).ListBox.FocusItem > -1) then
    for i := 0 to Items.Count-1 do
    begin
      if Items[i]= TDropDownWindow(FDropDown).ListBox.Items[TDropDownWindow(FDropDown).ListBox.FocusItem] then
      begin
        FSelectedItem:= i;
        Break;
      end;
    end;

  //Repaint will check if Handle is created
  Repaint;
end;

procedure TlqBaseEditCombo.DefaultPopupInsertFromCharmap(Sender: TObject);
var
  s: TlqString;
begin
  if FAllowNew= anNo then
    Exit;
  s := lqShowCharMap;
  if s <> '' then
    //SetText(s);
    DoPaste(s);
end;

procedure TlqBaseEditCombo.DoPaste(const AText: TlqString);
var
  s: string;
  prevval: TlqString;
  i: integer;
begin
  prevval := FText;
  s := AText;
    if (FMaxLength <= 0) or (UTF8Length(FText) < FMaxLength) then
    begin
      UTF8Insert(s, FText, FCursorPos + UTF8Length(s));
      Inc(FCursorPos);
      FSelStart := FCursorPos;
      if Assigned(FDropDown) then
        FDropDown.Close;
      FSelectedItem := -1;
      for i := 0 to FItems.Count-1 do
        if SameText(UTF8Copy(FItems.Strings[i], 1, UTF8Length(FText)), FText) then
        begin
          FSelectedItem:= i;
          DoDropDown;
          Break;
        end;
      if FSelectedItem = -1 then
        FNewItem:= True;
    end;
  Repaint;
  if prevval <> Text then
    DoOnChange;
end;

procedure TlqBaseEditCombo.SetDefaultPopupMenuItemsState;
var
  i: integer;
  itm: TlqMenuItem;
begin
  //for i := 0 to FDefaultPopupMenu.ComponentCount-1 do
  //begin
  //  if FDefaultPopupMenu.Components[i] is TlqMenuItem then
  //  begin
  //    itm := TlqMenuItem(FDefaultPopupMenu.Components[i]);
  //    // enabled/disable menu items
  //    if itm.Name = ipmCut then
  //      itm.Enabled := (not ReadOnly) and (FSelOffset <> 0)
  //    else if itm.Name = ipmCopy then
  //      itm.Enabled := FSelOffset <> 0
  //    else if itm.Name = ipmPaste then
  //      itm.Enabled := (not ReadOnly) and (lqClipboard.Text <> '')
  //    else if itm.Name = ipmClearAll then
  //      itm.Enabled := (not ReadOnly) and (Text <> '')
  //    else if itm.Name = ipmCharmap then
  //      itm.Enabled := (not ReadOnly);
  //  end;
  //end;
end;

procedure TlqBaseEditCombo.SetText(const AValue: string);
var
  i: integer;
begin
  if AValue = '' then
  begin
    FText:= '';
    FocusItem := -1;  // nothing selected
  end
  else
  begin
    for i := 0 to Items.Count-1 do
    begin
      if SameText(UTF8Copy(Items.Strings[i], 1, UTF8Length(AVAlue)), AValue) then
      begin
        FocusItem := i;
        FText:= AValue;
        Repaint;
        Exit; //==>
      end;
    end;
    // if we get here, we didn't find a match
    FocusItem := -1;
  end;
end;

procedure TlqBaseEditCombo.ShowDefaultPopupMenu(const x, y: integer;
  const shiftstate: TShiftState);
var
  itm: TlqMenuItem;
begin
  if not Assigned(FDefaultPopupMenu) then
  begin
    FDefaultPopupMenu := TlqPopupMenu.Create(nil);
    //itm := FDefaultPopupMenu.AddMenuItem(rsCut, '', @DefaultPopupCut);
    //itm.Name := ipmCut;
    //itm := FDefaultPopupMenu.AddMenuItem(rsCopy, '', @DefaultPopupCopy);
    //itm.Name := ipmCopy;
    //itm := FDefaultPopupMenu.AddMenuItem(rsPaste, '', @DefaultPopupPaste);
    //itm.Name := ipmPaste;
    //itm := FDefaultPopupMenu.AddMenuItem(rsDelete, '', @DefaultPopupClearAll);
    //itm.Name := ipmClearAll;
    //itm := FDefaultPopupMenu.AddMenuItem('-', '', nil);
    //itm.Name := 'N1';
    itm := FDefaultPopupMenu.AddMenuItem(rsInsertFromCharacterMap, '', @DefaultPopupInsertFromCharmap);
    itm.Name := ipmCharmap;
  end;

  SetDefaultPopupMenuItemsState;
  FDefaultPopupMenu.ShowAt(self, x, y);
end;

procedure TlqBaseEditCombo.HandleResize(AWidth, AHeight: TlqCoord);
begin
  inherited HandleResize(AWidth, AHeight);
  if FSizeIsDirty then
    CalculateInternalButtonRect;
end;

procedure TlqBaseEditCombo.HandleKeyChar(var AText: TlqChar;
    var shiftstate: TShiftState; var consumed: Boolean);
var
  s: TlqChar;
  prevval: string;
  i: integer;
begin
  prevval   := FText;
  s         := AText;
  consumed  := False;
  if FText = '' then
    FNewItem := False;

  // Handle only printable characters
  // Note: This is now UTF-8 compliant!
  if Enabled and (Ord(AText[1]) > 31) and (Ord(AText[1]) < 127) or (Length(AText) > 1) then
  begin
    if (FMaxLength <= 0) or (UTF8Length(FText) < FMaxLength) then
    begin
      UTF8Insert(s, FText, FCursorPos + UTF8Length(s));
      Inc(FCursorPos);
      FSelStart := FCursorPos;
      if Assigned(FDropDown) then
        FDropDown.Close;
      FSelectedItem := -1;
      for i := 0 to FItems.Count-1 do
        if SameText(UTF8Copy(FItems.Strings[i], 1, UTF8Length(FText)), FText) then
        begin
          FSelectedItem:= i;
          DoDropDown;
          Break;
        end;
      if FSelectedItem = -1 then
        if FAllowNew = anNo then
        begin
          UTF8Delete(FText, FCursorPos, 1);
          Dec(FCursorPos);
          FSelStart := FCursorPos;
          for i := 0 to FItems.Count-1 do
            if SameText(UTF8Copy(FItems.Strings[i], 1, UTF8Length(FText)), FText) then
            begin
              FSelectedItem:= i;
              DoDropDown;
              Break;
            end;
        end
        else
          FNewItem:= True;
    end;
    consumed := True;
  end;

  if prevval <> FText then
    DoOnChange;

  if consumed then
    RePaint;
//  else
    inherited HandleKeyChar(AText, shiftstate, consumed);
end;

procedure TlqBaseEditCombo.HandleKeyPress(var keycode: word;
    var shiftstate: TShiftState; var consumed: boolean);
var
  hasChanged: boolean;
  i: integer;
begin
  hasChanged := False;

  if not Enabled then
    consumed := False
  else
  begin
    consumed := True;

    case keycode of
      keyBackSpace:
          begin
            if FCursorPos > 0 then
            begin
              UTF8Delete(FText, FCursorPos, 1);
              Dec(FCursorPos);
              FSelStart := FCursorPos;
            if Assigned(FDropDown) then
              FDropDown.Close;
            FSelectedItem := -1;
            for i := 0 to FItems.Count-1 do
              if SameText(UTF8Copy(FItems.Strings[i], 1, UTF8Length(FText)), FText) then
              begin
                FSelectedItem:= i;
                if FNewItem then
                  FNewItem:= False;
                DoDropDown;
                Break;
              end;
            hasChanged := True;
            end;
          end;
      keyDelete:
          begin
            if FAllowNew <> anNo then
            begin
              FocusItem := -1;
              FSelectedItem := -1;
              FNewItem:= True;
              hasChanged := True;
            end;
          end;

      keyReturn,
      keyPEnter:
          begin
            if FSelectedItem > -1 then
              SetText(Items[FSelectedItem])
            else
              FocusItem:= -1;
            if FNewItem then
              case FAllowNew of
                anYes:
                  begin
                    FItems.Add(FText);
                    FocusItem := Pred(FItems.Count);
                  end;
                anAsk:
                  begin
                    if TlqMessageDialog.Question(rsNewItemDetected, Format(rsAddNewItem, [FText])) = mbYes then
                    begin
                      FItems.Add(FText);
                      FFocusItem := Pred(FItems.Count);
                    end
                    else
                    begin
                      FNewItem:= False;
                      FFocusItem := -1;
                      FText:= '';
                    end;  { if/else }
                    Parent.ActivateWindow;
                  end;
                end;
            hasChanged := True;
            if Assigned(FDropDown) then
              FDropDown.Close;
          end;
      else
      begin
        Consumed := False;
      end;
    end;
  end;

  if Consumed then
  begin
    FSelStart  := FCursorPos;
    FSelOffset := 0;
  end;

  if consumed and hasChanged then
    RePaint;

  if hasChanged then
    DoOnChange;
  inherited HandleKeyPress(keycode, shiftstate, consumed);
end;

procedure TlqBaseEditCombo.HandleLMouseDown(x, y: integer;
    shiftstate: TShiftState);
begin
  inherited HandleLMouseDown(x, y, shiftstate);
  // button state is down only if user clicked in the button rectangle.
  FBtnPressed := PtInRect(FInternalBtnRect, Point(x, y));
  if not FAutoCompletion then
  begin
    PaintInternalButton;
    DoDropDown;
  end
  else if FBtnPressed then
    begin
      PaintInternalButton;
      DoDropDown;
    end;
end;

procedure TlqBaseEditCombo.HandleLMouseUp(x, y: integer;
    shiftstate: TShiftState);
begin
  inherited HandleLMouseUp(x, y, shiftstate);
  FBtnPressed := False;
  PaintInternalButton;
end;

procedure TlqBaseEditCombo.HandleRMouseUp(x, y: integer; shiftstate: TShiftState);
begin
  inherited HandleRMouseUp(x, y, shiftstate);
  //if Assigned(PopupMenu) then
  //  PopupMenu.ShowAt(self, x, y)
  //else
    ShowDefaultPopupMenu(x, y, ShiftState);
end;

procedure TlqBaseEditCombo.HandlePaint;
var
  r: TlqRect;
  tw, tw2, st, len: integer;
  Texte: string;

  // paint selection rectangle
  procedure DrawSelection;
  var
    lcolor: TlqColor;
  begin
    if Focused then
    begin
      lcolor := clSelection;
      Canvas.SetTextColor(clSelectionText);
    end
    else
    begin
      lcolor := clInactiveSel;
      Canvas.SetTextColor(clText1);
    end;

    len := FSelOffset;
    st  := FSelStart;
    if len < 0 then
    begin
      st  := st + len;
      len := -len;
    end;
    tw  := Font.TextWidth(UTF8Copy(Items[FSelectedItem], 1, st));
    tw2 := Font.TextWidth(UTF8Copy(Items[FSelectedItem], 1, st + len));

    // XOR on Anti-aliased text doesn't look to good. Lets try standard
    // Blue & White like what was doen in TlqEdit.
{   Canvas.SetColor(lcolor);
    Canvas.FillRectangle(-FDrawOffset + FMargin + tw, 3, tw2 - tw, Font.Height);
    r.SetRect(-FDrawOffset + FMargin + tw, 3, tw2 - tw, Font.Height);
    Canvas.AddClipRect(r);
    Canvas.SetTextColor(clWhite);
    lqStyle.DrawString(Canvas, -FDrawOffset + FMargin, 3, Text, Enabled);
    Canvas.ClearClipRect;
}
    Canvas.XORFillRectangle(lqColorToRGB(lcolor) xor $FFFFFF,
      -FDrawOffset + FMargin + tw, 3, tw2 - tw, Font.Height);
  end;

begin
  Canvas.BeginDraw;
//  inherited HandlePaint;
  Canvas.ClearClipRect;
  r.SetRect(0, 0, Width, Height);
  Canvas.DrawControlFrame(r);

  // internal background rectangle (without frame)
  InflateRect(r, -2, -2);
  Canvas.SetClipRect(r);

  if Enabled then
    Canvas.SetColor(FBackgroundColor)
  else
    Canvas.SetColor(clWindowBackground);

  Canvas.FillRectangle(r);

  // paint the fake dropdown button
  PaintInternalButton;

  Dec(r.Width, FInternalBtnRect.Width);
  Canvas.SetClipRect(r);
  Canvas.SetFont(Font);

  if not AutoCompletion then
    if Focused then
    begin
      Canvas.SetColor(clSelection);
      Canvas.SetTextColor(clSelectionText);
      InflateRect(r, -1, -1);
    end
    else
    begin
      if Enabled then
        Canvas.SetColor(FBackgroundColor)
      else
        Canvas.SetColor(clWindowBackground);
      Canvas.SetTextColor(FTextColor);
    end
  else
  begin
    if Enabled then
      Canvas.SetColor(FBackgroundColor)
    else
      Canvas.SetColor(clWindowBackground);
    Canvas.SetTextColor(FTextColor);
  end;
  Canvas.FillRectangle(r);

  // Draw select item's text
  if not AutoCompletion then
  begin
    if HasText then
      lqStyle.DrawString(Canvas, FMargin+1, FMargin, Text, Enabled)
    else
      begin
      Canvas.SetTextColor(clShadow1);
      lqStyle.DrawString(Canvas, FMargin+1, FMargin, ExtraHint, Enabled);
      end;
  end
  else
  begin
    if HasText then
    begin
      FSelOffset := 0;
      lqStyle.DrawString(Canvas, FMargin+1, FMargin, Text, Enabled);
    end
    else
    begin
      Texte := Text;
      if Texte <> '' then
        if FSelectedItem > -1 then
        begin
          FSelOffset := Font.TextWidth(UTF8Copy(Items[FSelectedItem], UTF8Length(FText) + 1,
            UTF8Length(Items[FSelectedItem]) - UTF8Length(FText)));
          lqStyle.DrawString(Canvas, FMargin+1, FMargin, FText + UTF8Copy(Items[FSelectedItem],
            UTF8Length(FText) + 1, UTF8Length(Items[FSelectedItem]) - UTF8Length(FText)), Enabled);
        end
        else
        begin
          FSelOffset := 0;
          lqStyle.DrawString(Canvas, FMargin+1, FMargin, FText, Enabled);
        end;
    end;

    if Focused then
    begin
      // drawing selection
      if FSelOffset <> 0 then
        DrawSelection;

      // drawing cursor
      FCursorPos:= UTF8Length(FText);
      tw := Font.TextWidth(UTF8Copy(FText, 1, FCursorPos));
      lqCaret.SetCaret(Canvas, -FDrawOffset + FMargin + tw, FMargin, lqCaret.Width, Font.Height);
    end
    else
      lqCaret.UnSetCaret(Canvas);
  end;

  Canvas.EndDraw;
end;

constructor TlqBaseEditCombo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBackgroundColor  := clBoxColor;
  FTextColor        := Parent.TextColor;
  FWidth            := 120;
  FHeight           := Font.Height + 6;
  FMargin           := 3;
  FFocusable        := True;
  FAutocompletion   := False;
  FAllowNew         := anNo;

  FText             := '';
  FCursorPos        := UTF8Length(FText);
  FSelStart         := FCursorPos;
  FSelOffset        := 0;
  FDrawOffset       := 0;
  FSelectedItem     := -1;       // to allow typing if list is empty
  FNewItem          := False;

  CalculateInternalButtonRect;
end;

destructor TlqBaseEditCombo.Destroy;
begin
  if Assigned(FDropDown) then
    FDropDown.Free;
  inherited Destroy;
end;

procedure TlqBaseEditCombo.Clear;
begin
  Text := '';
  Items.Clear;
end;

procedure TlqBaseEditCombo.SetFocus;
var
  i: integer;
begin
  HandleSetFocus;
  if FText > '' then
    for i := 0 to Items.Count-1 do
      if SameText(UTF8Copy(Items.Strings[i], 1, UTF8Length(FText)), FText) then
      begin
        FSelectedItem := i;
        FNewItem := False;
        Exit; //==>
      end;
end;

procedure TlqBaseEditCombo.Update;
begin
  FFocusItem := -1;
  Repaint;
end;

end.
