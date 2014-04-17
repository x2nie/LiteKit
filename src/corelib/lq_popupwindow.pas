{
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2010 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Defines a PopupWindow control. It gets used for things like PopupMenu,
      ComboBox and Calendar controls.
}

unit lq_popupwindow;

{$mode objfpc}{$H+}

{.$Define DEBUG}

interface

uses
  Classes,
  SysUtils,
  lq_base,
  lq_main,
  lq_widget,
  lq_impl;
  
type

  TlqPopupWindow = class(TlqWidget)
  private
    FDontCloseWidget: TlqWidget;
    FOnClose: TNotifyEvent;
    FOnShow: TNotifyEvent;
    FPopupFrame: boolean;
    procedure   SetPopupFrame(const AValue: boolean);
    function    GetDisplayPos(AReferenceWindow: TlqWidget; const x, y: integer): TPoint;
  protected
    procedure   MsgClose(var msg: TlqMessageRec); message FPGM_CLOSE;
    procedure   HandleClose; virtual;
    procedure   HandleShow; override;
    procedure   HandlePaint; override;
    procedure   ProcessPopupFrame; virtual;
    procedure   DoPaintPopupFrame; virtual;
    procedure   DoOnClose; virtual;
    procedure   DoOnShow; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure   AdjustWindowStyle; override;
    procedure   ShowAt(AWidget: TlqWidget; x, y: TlqCoord; const ACanAdjustPos: boolean = false); overload;
    procedure   ShowAt(x, y: TlqCoord); overload;
    procedure   Close; virtual;
    property    DontCloseWidget: TlqWidget read FDontCloseWidget write FDontCloseWidget;
    property    PopupFrame: boolean read FPopupFrame write SetPopupFrame;
    property    OnClose: TNotifyEvent read FOnClose write FOnClose;
    property    OnShow: TNotifyEvent read FOnShow write FOnShow;
  end;


procedure ClosePopups;
function  PopupListFirst: TlqPopupWindow;
function  PopupListFind(AWinHandle: TlqWinHandle): TlqPopupWindow;
function  PopupDontCloseWidget(AWidget: TlqWidget): boolean;


implementation


type
  // Popup window linked list. Maybe we can implement it via a TList as well.
  PPopupListRec = ^PopupListRec;
  PopupListRec = record
    Widget: TlqPopupWindow;
    Next: PPopupListRec;
  end;

var
  uOriginalFocusRoot: TlqWidget;
  uFirstPopup: PPopupListRec;
  uLastPopup: PPopupListRec;
  
  
// local helper functions

procedure ClosePopups;
begin
  while uFirstPopup <> nil do
  begin
    {$IFDEF DEBUG}
    writeln('...closing ', uFirstPopup^.Widget.Name);
    {$ENDIF}
    TlqPopupWindow(uFirstPopup^.Widget).Close;
  end;
end;

procedure PopupListAdd(pw: TlqPopupWindow);
var
  p: PPopupListRec;
begin
  if pw = nil then
    Exit; //==>

  if uFirstPopup = nil then
    uOriginalFocusRoot := FocusRootWidget;

  FocusRootWidget := pw;

  New(p);
  p^.Widget := pw;
  p^.Next := nil;
  if uFirstPopup = nil then
    uFirstPopup := p
  else
    uLastPopup^.Next := p;
  uLastPopup := p;
end;

procedure PopupListRemove(pw: TlqPopupWindow);
var
  prevp: PPopupListRec;
  p: PPopupListRec;
  px: PPopupListRec;
begin
  p := uFirstPopup;
  prevp := nil;

  while p <> nil do
  begin
    if p^.Widget = pw then
    begin
      if prevp = nil then
        uFirstPopup := p^.Next
      else
        prevp^.Next := p^.Next;
      if uLastPopup = p then
        uLastPopup := prevp;
      px := p;
      p := p^.Next;
      Dispose(px);
    end
    else
    begin
      prevp := p;
      p := p^.Next;
    end;
  end;

  if uLastPopup <> nil then
    FocusRootWidget := uLastPopup^.Widget
  else
    FocusRootWidget := uOriginalFocusRoot;
end;

function PopupListFirst: TlqPopupWindow;
begin
  if uFirstPopup <> nil then
    Result := uFirstPopup^.Widget
  else
    Result := nil;
end;


function PopupListFind(AWinHandle: TlqWinHandle): TlqPopupWindow;
var
  p: PPopupListRec;
begin
  p := uFirstPopup;
  while p <> nil do
  begin
    if p^.Widget.WinHandle = AWinHandle then
    begin
      Result := p^.Widget;
      Exit; //==>
    end;
    p := p^.Next;
  end;
  Result := nil;
end;

function PopupDontCloseWidget(AWidget: TlqWidget): boolean;
var
  p: PPopupListRec;
begin
  Result := False;
  if AWidget = nil then
    Exit; //==>

  p := uFirstPopup;
  while p <> nil do
  begin
    if p^.Widget.DontCloseWidget = AWidget then
    begin
      Result := True;
      Exit; //==>
    end;
    p := p^.Next;
  end;
end;


{ TlqPopupWindow }

procedure TlqPopupWindow.SetPopupFrame(const AValue: boolean);
begin
  if FPopupFrame <> AValue then
  begin
    FPopupFrame := AValue;
    ProcessPopupFrame;
  end;
end;

function TlqPopupWindow.GetDisplayPos(AReferenceWindow: TlqWidget; const x, y: integer): TPoint;
begin
  // translate coordinates
  Result := WindowToScreen(AReferenceWindow, Point(x, y));

  // popup window will not fit below (x,y) so we place it above (x,y)
  if (Result.y + self.Height) > fpgApplication.ScreenHeight then
    Result.y := Result.y - self.Height;

  // popup window will not fit to right of (x,y) so we place it to left of (x,y)
  if (Result.x + self.Width) > fpgApplication.ScreenWidth then
    Result.x := Result.x - self.Width;
end;

procedure TlqPopupWindow.MsgClose(var msg: TlqMessageRec);
begin
  {$IFDEF DEBUG}
  writeln('TlqPopupWindow.MsgClose [', Classname, ']');
  {$ENDIF}
  HandleClose;
end;

procedure TlqPopupWindow.AdjustWindowStyle;
begin
  inherited AdjustWindowStyle;
  // We could possibly change this later
  Exclude(FWindowAttributes, waSizeable);
end;

procedure TlqPopupWindow.HandleClose;
begin
  DoOnClose;
  HandleHide;
end;

procedure TlqPopupWindow.HandleShow;
begin
  inherited HandleShow;
  DoOnShow;
end;

procedure TlqPopupWindow.HandlePaint;
begin
  inherited HandlePaint;
  if PopupFrame then
    DoPaintPopupFrame;
end;

procedure TlqPopupWindow.ProcessPopupFrame;
var
  i: integer;
begin
  if PopupFrame then
  begin
    for i := 0 to ComponentCount-1 do
    begin
      if Components[i] is TlqWidget then
        TlqWidget(Components[i]).Anchors := [anRight, anBottom];
    end;
    // make space for the frame
    HandleResize(Width+2, Height+2);
    UpdateWindowPosition;

    for i := 0 to ComponentCount-1 do
    begin
      if Components[i] is TlqWidget then
        TlqWidget(Components[i]).Anchors := [anLeft, anTop];
    end;
    HandleResize(Width+2, Height+2);
    UpdateWindowPosition;
    Repaint;
  end;
end;

procedure TlqPopupWindow.DoPaintPopupFrame;
var
  lColor: TlqColor;
begin
  lColor := fpgColorToRGB(BackgroundColor);
  Canvas.SetLineStyle(1, lsSolid);
  Canvas.SetColor(clWidgetFrame);
  Canvas.DrawRectangle(0, 0, Width, Height);
  Canvas.SetColor(lColor);
end;

procedure TlqPopupWindow.DoOnClose;
begin
  if Assigned(OnClose) then
    OnClose(self);
end;

procedure TlqPopupWindow.DoOnShow;
begin
  if Assigned(FOnShow) then
    FOnShow(self);
end;

constructor TlqPopupWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  WindowType := wtPopup;
  FDontCloseWidget := nil;
  Parent := nil;
  FPopupFrame := False;
  FIsContainer := True;
end;

procedure TlqPopupWindow.ShowAt(AWidget: TlqWidget; x, y: TlqCoord; const ACanAdjustPos: boolean);
var
  pt: TPoint;
begin
  PopupListAdd(self);
  
  if AWidget <> nil then
  begin
    // translate coordinates - window to screen
    if ACanAdjustPos then
      pt := GetDisplayPos(AWidget, x, y)
    else
      pt := WindowToScreen(AWidget, Point(x, y));
    // reposition
    Left  := pt.X;
    Top   := pt.Y;
  end
  else
  begin
    // no translation needed, they are already in screen coordinates
    Left  := x;
    Top   := y;
  end;
  
  // and show
  HandleShow;
end;

procedure TlqPopupWindow.ShowAt(x, y: TlqCoord);
begin
  ShowAt(nil, x, y);
end;

procedure TlqPopupWindow.Close;
begin
  HandleClose;
  PopupListRemove(self);
end;


initialization
  uFirstPopup := nil;
  uLastPopup  := nil;

end.

