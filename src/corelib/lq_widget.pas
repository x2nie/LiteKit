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
      The base widget, which all GUI widgets inherit from.
}

unit lq_widget;

{$mode objfpc}{$H+}

{.$Define DEBUG}
{.$Define CStackDebug}

interface

uses
  Classes,
  SysUtils,
  lq_main,
  lq_base;

type
  TFocusSearchDirection = (fsdFirst, fsdLast, fsdNext, fsdPrev);

  THintEvent = procedure(Sender: TObject; var AHint: TlqString) of object;

  TlqDragEnterEvent = procedure(Sender, Source: TObject; AMimeList: TStringList; var AMimeChoice: TlqString; var ADropAction: TlqDropAction; var Accept: Boolean) of object;
  TlqDragDropEvent = procedure(Sender, Source: TObject; X, Y: integer; AData: variant) of object;


  { TlqWidget }

  TlqWidget = class(TlqWindow)
  private
    FAcceptDrops: boolean;
    FAlignRect: TlqRect;
    FOnClick: TNotifyEvent;
    FOnDoubleClick: TMouseButtonEvent;
    FOnDragDrop: TlqDragDropEvent;
    FOnDragEnter: TlqDragEnterEvent;
    FOnDragLeave: TNotifyEvent;
    FOnEnter: TNotifyEvent;
    FOnExit: TNotifyEvent;
    FOnMouseDown: TMouseButtonEvent;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseExit: TNotifyEvent;
    FOnMouseMove: TMouseMoveEvent;
    FOnMouseUp: TMouseButtonEvent;
    FOnMouseScroll: TMouseWheelEvent;
    FOnPaint: TPaintEvent;
    FOnKeyPress: TKeyPressEvent;
    FOnResize: TNotifyEvent;
    FOnScreen: boolean;
    FOnShowHint: THintEvent;
    FDragStartPos: TlqPoint;
    alist: TList;
    function    GetChildrenWidget(Index: integer): TlqWidget;
    procedure   SetActiveWidget(const AValue: TlqWidget);
    function    IsShowHintStored: boolean;
    procedure   SetFormDesigner(const AValue: TObject);
    procedure   SetAlign(const AValue: TAlign);
  protected
    procedure   MsgPaint(var msg: TlqMessageRec); message LQM_PAINT;
    procedure   MsgResize(var msg: TlqMessageRec); message LQM_RESIZE;
    procedure   MsgMove(var msg: TlqMessageRec); message LQM_MOVE;
    procedure   MsgKeyChar(var msg: TlqMessageRec); message LQM_KEYCHAR;
    procedure   MsgKeyPress(var msg: TlqMessageRec); message LQM_KEYPRESS;
    procedure   MsgKeyRelease(var msg: TlqMessageRec); message LQM_KEYRELEASE;
    procedure   MsgMouseDown(var msg: TlqMessageRec); message LQM_MOUSEDOWN;
    procedure   MsgMouseUp(var msg: TlqMessageRec); message LQM_MOUSEUP;
    procedure   MsgMouseMove(var msg: TlqMessageRec); message LQM_MOUSEMOVE;
    procedure   MsgDoubleClick(var msg: TlqMessageRec); message LQM_DOUBLECLICK;
    procedure   MsgMouseEnter(var msg: TlqMessageRec); message LQM_MOUSEENTER;
    procedure   MsgMouseExit(var msg: TlqMessageRec); message LQM_MOUSEEXIT;
    procedure   MsgMouseScroll(var msg: TlqMessageRec); message LQM_SCROLL;
    procedure   MsgDropEnter(var msg: TlqMessageRec); message LQM_DROPENTER;
    procedure   MsgDropExit(var msg: TlqMessageRec); message LQM_DROPEXIT;
  protected
    FFormDesigner: TObject;
    FVisible: boolean;
    FEnabled: boolean;
    FFocusable: boolean;
    FFocused: boolean;
    FTabOrder: integer;
    FAnchors: TAnchors;
    FActiveWidget: TlqWidget;
    FAlign: TAlign;
    FHint: TlqString;
    FShowHint: boolean;
    FParentShowHint: boolean;
    FBackgroundColor: TlqColor;
    FTextColor: TlqColor;
    FIsContainer: Boolean;
    FOnClickPending: Boolean;
    FIgnoreDblClicks: Boolean;
    FParent: TlqWidget;

    procedure   AllocateWindowHandle; override;    
    procedure   SetAcceptDrops(const AValue: boolean); virtual;
    function    GetOnShowHint: THintEvent; virtual;
    procedure   SetOnShowHint(const AValue: THintEvent); virtual;
    procedure   SetBackgroundColor(const AValue: TlqColor); virtual;
    procedure   SetTextColor(const AValue: TlqColor); virtual;
    //function    GetParent: TlqWidget; reintroduce;
    procedure   SetParent(const AValue: TlqWidget); virtual;
    procedure   SetEnabled(const AValue: boolean); virtual;
    procedure   SetVisible(const AValue: boolean); virtual;
    procedure   SetShowHint(const AValue: boolean); virtual;
    procedure   SetParentShowHint(const AValue: boolean); virtual;
    function    GetHint: TlqString; virtual;
    procedure   SetHint(const AValue: TlqString); virtual;
    procedure   DoUpdateWindowPosition; override;
    procedure   DoAlignment;
    procedure   DoResize;
    procedure   DoShowHint(var AHint: TlqString);
    procedure   DoKeyShortcut(const AOrigin: TlqWidget; const keycode: word; const shiftstate: TShiftState; var consumed: boolean; const IsChildOfOrigin: boolean = False); virtual;
    procedure   HandlePaint; virtual;
    procedure   HandleKeyChar(var AText: TlqChar; var shiftstate: TShiftState; var consumed: boolean); virtual;
    procedure   HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); virtual;
    procedure   HandleKeyRelease(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); virtual;
    procedure   HandleSetFocus; virtual;
    procedure   HandleKillFocus; virtual;
    procedure   HandleLMouseDown(x, y: integer; shiftstate: TShiftState); virtual;
    procedure   HandleRMouseDown(x, y: integer; shiftstate: TShiftState); virtual;
    procedure   HandleLMouseUp(x, y: integer; shiftstate: TShiftState); virtual;
    procedure   HandleRMouseUp(x, y: integer; shiftstate: TShiftState); virtual;
    procedure   HandleMouseMove(x, y: integer; btnstate: word; shiftstate: TShiftState); virtual;
    procedure   HandleDoubleClick(x, y: integer; button: word; shiftstate: TShiftState); virtual;
    procedure   HandleMouseEnter; virtual;
    procedure   HandleMouseExit; virtual;
    procedure   HandleMouseScroll(x, y: integer; shiftstate: TShiftState; delta: smallint); virtual;
    function    FindFocusWidget(startwg: TlqWidget; direction: TFocusSearchDirection): TlqWidget;
    procedure   HandleAlignments(const dwidth, dheight: TlqCoord); virtual;
    procedure   HandleShow; virtual;
    procedure   InternalHandleShow; virtual;
    procedure   HandleHide; virtual;
    procedure   MoveAndResize(ALeft, ATop, AWidth, AHeight: TlqCoord);
    { property events }
    property    OnClick: TNotifyEvent read FOnClick write FOnClick;
    property    OnDoubleClick: TMouseButtonEvent read FOnDoubleClick write FOnDoubleClick;
    property    OnEnter: TNotifyEvent read FOnEnter write FOnEnter;
    property    OnExit: TNotifyEvent read FOnExit write FOnExit;
    property    OnKeyPress: TKeyPressEvent read FOnKeyPress write FOnKeyPress;
    property    OnMouseDown: TMouseButtonEvent read FOnMouseDown write FOnMouseDown;
    property    OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property    OnMouseExit: TNotifyEvent read FOnMouseExit write FOnMouseExit;
    property    OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property    OnMouseUp: TMouseButtonEvent read FOnMouseUp write FOnMouseUp;
    property    OnMouseScroll: TMouseWheelEvent read FOnMouseScroll write FOnMouseScroll;
    property    OnPaint: TPaintEvent read FOnPaint write FOnPaint;
    property    OnResize: TNotifyEvent read FOnResize write FOnResize;
    property    OnShowHint: THintEvent read GetOnShowHint write SetOnShowHint;
  protected
    {requires by IDE}
    FChilds: TList; // list of Widget
    procedure   SetParentComponent(Value: TComponent); override;
    procedure   GetChildren(Proc: TGetChildProc; Root: TComponent); override;
  public
    {requires by IDE}
    function    ChildrenCount: integer;

    function    HasParent: Boolean; override;
    function    GetParentComponent: TComponent; override;
    property    Children[Index: integer]: TlqWidget read GetChildrenWidget; //GetChildren has been reserved
    property    Parent: TlqWidget read FParent write SetParent;           
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   AfterConstruction; override;
    function    InDesigner: boolean;
    procedure   RePaint; virtual;
    procedure   InvokeHelp; virtual;
    procedure   Realign;
    procedure   SetFocus;
    procedure   KillFocus;
    procedure   MoveAndResizeBy(const dx, dy, dw, dh: TlqCoord);
    procedure   SetPosition(aleft, atop, awidth, aheight: TlqCoord); virtual;
    procedure   Invalidate; // double check this works as developers expect????
    property    FormDesigner: TObject read FFormDesigner write SetFormDesigner;

    property    AcceptDrops: boolean read FAcceptDrops write SetAcceptDrops default False;
    property    ActiveWidget: TlqWidget read FActiveWidget write SetActiveWidget;
    property    IsContainer: Boolean read FIsContainer;
    property    Enabled: boolean read FEnabled write SetEnabled default True;
    property    TabOrder: integer read FTabOrder write FTabOrder;
    { Is the widget allowed to receive keyboard focus. }
    property    Focusable: boolean read FFocusable write FFocusable default False;
    property    Focused: boolean read FFocused write FFocused default False;
    property    Anchors: TAnchors read FAnchors write FAnchors default [anLeft, anTop];
    property    Align: TAlign read FAlign write SetAlign default alNone;

    property    IgnoreDblClicks: Boolean read FIgnoreDblClicks write FIgnoreDblClicks default False;
    property    ShowHint: boolean read FShowHint write SetShowHint stored IsShowHintStored;
    property    ParentShowHint: boolean read FParentShowHint write SetParentShowHint default True;
    property    BackgroundColor: TlqColor read FBackgroundColor write SetBackgroundColor default clWindowBackground;
    property    TextColor: TlqColor read FTextColor write SetTextColor default clText1;
    property    OnDragEnter: TlqDragEnterEvent read FOnDragEnter write FOnDragEnter;
    property    OnDragLeave: TNotifyEvent read FOnDragLeave write FOnDragLeave;
    property    OnDragDrop: TlqDragDropEvent read FOnDragDrop write FOnDragDrop;
  published
    property    Left;
    property    Top;
    property    Width;
    property    Height;
    property    Visible: boolean read FVisible write SetVisible default True;
    property    Hint: TlqString read GetHint write SetHint;
  end;


var
  FocusRootWidget: TlqWidget;


function FindKeyboardFocus: TlqWidget;

implementation

uses
  lq_constants,
  lq_menu,
  lq_form;  { for OnKeyPress handling }


var
  uLastClickWidget: TlqWidget;
  uLastClickPoint: TPoint;
  uLastClickTime: DWord;
  uMouseDownSourceWidget: TlqWidget; { widget Left MButton was pressed on }


type
  TlqFormFriend = class(TlqBaseForm)
  end;

function FindKeyboardFocus: TlqWidget;
begin
  Result := nil;

  if FocusRootWidget <> nil then
  begin
    Result := FocusRootWidget;
    while (Result <> nil) and (Result.ActiveWidget <> nil) do
      Result := Result.ActiveWidget;
  end;
end;

function CompareInts(i1, i2: integer): integer;
begin
  if i1 < i2 then
    Result := -1
  else if i1 > i2 then
    Result := 1
  else
    Result := 0;
end;

function AlignCompare(p1, p2: Pointer): integer;
var
  w1: TlqWidget;
  w2: TlqWidget;
begin
  w1 := TlqWidget(p1);
  w2 := TlqWidget(p2);
  case w1.Align of
    alTop:    Result := CompareInts(w1.Top, w2.Top);
    alBottom: Result := CompareInts(w2.Top, w1.Top);
    alLeft:   Result := CompareInts(w1.Left, w2.Left);
    alRight:  Result := CompareInts(w2.Left, w1.Left);
    else
      Result         := 0;
  end;
end;


{ TlqWidget }

procedure TlqWidget.SetEnabled(const AValue: boolean);
var
  i: integer;
begin
  if FEnabled = AValue then
    Exit; //==>
  FEnabled := AValue;
  for i := 0 to ComponentCount - 1 do
  begin
    if Components[i] is TlqWidget then
      TlqWidget(Components[i]).Enabled := FEnabled;
  end;
  RePaint;
end;

procedure TlqWidget.SetActiveWidget(const AValue: TlqWidget);
begin
  if FActiveWidget = AValue then
    Exit; //==>
  if InDesigner then
    Exit; //==>
  
  try
    if FActiveWidget <> nil then
      FActiveWidget.HandleKillFocus;
  except
    { This is just a failsafe, in case FActiveWidget was not correctly set
      in the destructor of TlqWidget }
    FActiveWidget := nil;
  end;
  FActiveWidget := AValue;
  if FActiveWidget <> nil then
    FActiveWidget.HandleSetFocus;
end;

function TlqWidget.GetChildrenWidget(Index: integer): TlqWidget;
begin
  Result:=TlqWidget(FChilds[Index]);
end;

procedure TlqWidget.SetAcceptDrops(const AValue: boolean);
begin
  if FAcceptDrops = AValue then
    exit;
  FAcceptDrops := AValue;
  DoAcceptDrops(AValue);
end;

function TlqWidget.GetHint: TlqString;
begin
  Result := FHint;
end;

function TlqWidget.IsShowHintStored: boolean;
begin
  Result := not ParentShowHint;
end;

procedure TlqWidget.SetFormDesigner(const AValue: TObject);
var
  i: integer;
begin
  FFormDesigner := AValue;
  for i := 0 to ComponentCount-1 do
  begin
    if (Components[i] is TlqWidget) and (TlqWidget(Components[i]).Parent = self) then
      TlqWidget(Components[i]).FormDesigner := AValue;
  end;
end;

procedure TlqWidget.SetAlign(const AValue: TAlign);
begin
  if FAlign = AValue then
    Exit;
  FAlign := AValue;
  if Parent <> nil then
    Parent.Realign;
end;

procedure TlqWidget.SetVisible(const AValue: boolean);
begin
  if FVisible = AValue then
    Exit; //==>
  FVisible := AValue;
  if FOnScreen then
    if FVisible then
    begin
//      writeln('DEBUG:  TlqWidget.SetVisible - handleshow');
      HandleShow;
    end
    else
    begin
//      writeln('DEBUG:  TlqWidget.SetVisible - handlehide');
      HandleHide;
      FOnScreen := True;
    end;
end;

procedure TlqWidget.SetShowHint(const AValue: boolean);
begin
  if FShowHint <> AValue then
    FShowHint := AValue;
  if FShowHint then
    FParentShowHint := False;
end;

procedure TlqWidget.SetParentShowHint(const AValue: boolean);
begin
  if FParentShowHint <> AValue then
    FParentShowHint := AValue;
  if FParentShowHint then
    FShowHint := False;
end;

procedure TlqWidget.SetHint(const AValue: TlqString);
begin
  FHint := AValue;
end;

procedure TlqWidget.DoUpdateWindowPosition;
var
  dw: integer;
  dh: integer;
{$IFDEF CStackDebug}
  itf: IInterface;
{$ENDIF}
begin
  {$IFDEF CStackDebug}
  itf := DebugMethodEnter('TlqWidget.DoUpdateWindowPosition - ' + ClassName + ' ('+Name+')');
  {$ENDIF}
//  writeln('TlqWidget.DoUpdateWindowPosition - ' + Classname + ' ('+Name+')');
  dw      := FWidth - FPrevWidth;
  dh      := FHeight - FPrevHeight;

  if IsContainer and FSizeIsDirty then
  begin
    {$IFDEF CStackDebug}
    DebugLn(Format('  Alignment deltas  w: %d  h: %d', [dw, dh]));
    {$ENDIF}
    HandleAlignments(dw, dh);
  end;

  inherited DoUpdateWindowPosition;
  if (dw <> 0) or (dh <> 0) then
    DoResize;

  // We have now handled the difference between old and new values, so reset
  // them here not to affect the next iteration.
  FPrevWidth  := FWidth;
  FPrevHeight := FHeight;
  FSizeIsDirty:= False;
  FPosIsDirty := False;
end;

procedure TlqWidget.SetBackgroundColor(const AValue: TlqColor);
begin
  if FBackgroundColor <> AValue then
  begin
    FBackgroundColor := AValue;
    RePaint;
  end;
end;

procedure TlqWidget.SetTextColor(const AValue: TlqColor);
begin
  if FTextColor <> AValue then
  begin
    FTextColor := AValue;
    Repaint;
  end;
end;

function TlqWidget.InDesigner: boolean;
begin
  Result := (FFormDesigner <> nil)
end;

procedure TlqWidget.InvokeHelp;
begin
  case HelpType of
    htKeyword:
      if HelpKeyword <> '' then
      begin
        lqApplication.KeywordHelp(HelpKeyword);
        Exit; //==>
      end;
    htContext:
      if HelpContext <> 0 then
      begin
        lqApplication.ContextHelp(HelpContext);
        Exit; //==>
      end;
  end;
  if Parent <> nil then
    Parent.InvokeHelp
  else
    lqApplication.InvokeHelp;
end;

procedure TlqWidget.Realign;
begin
  HandleAlignments(0, 0);
  RePaint;
end;

{function TlqWidget.GetParent: TlqWidget;
begin
  Result := TlqWidget(inherited GetParent);
end;}


procedure TlqWidget.SetParent(const AValue: TlqWidget);   
begin
if FParent=AValue then exit;
  if FParent<>nil then begin
    Invalidate;
    FParent.FChilds.Remove(Self);
  end;
  FParent:=AValue;
  if FParent<>nil then begin
    FParent.FChilds.Add(Self);
  end;
  Invalidate;
end;

constructor TlqWidget.Create(AOwner: TComponent);
begin
  Loading;

  FIsContainer    := False;
  FOnScreen       := False;
  FVisible        := True;
  FActiveWidget   := nil;
  FEnabled        := True;
  FFocusable      := False;
  FFocused        := False;
  FTabOrder       := 0;
  FAnchors        := [anLeft, anTop];
  FAlign          := alNone;
  FHint           := '';
  FShowHint       := False;
  FParentShowHint := True;
  FBackgroundColor := clWindowBackground;
  FTextColor      := clText1;
  FAcceptDrops    := False;
  FOnClickPending := False;
  FIgnoreDblClicks := False;
  FParent         := nil;

  inherited Create(AOwner);

  if (AOwner <> nil) and (AOwner is TlqWidget) then
  begin
    Parent := TlqWidget(AOwner);
    FTabOrder := AOwner.ComponentCount;
  end
  else
    Parent := nil;

  if Parent <> nil then
  begin
    FWindowType := wtChild;
    FShowHint   := Parent.ShowHint;
  end;
  FChilds:=TList.Create;
end;

destructor TlqWidget.Destroy;
begin
  {$IFDEF DEBUG}
  writeln('TlqWidget.Destroy [', Classname, '.', Name, ']');
  {$ENDIF}
  HandleHide;
  if Owner <> nil then
    if (Owner is TlqWidget) and (TlqWidget(Owner).ActiveWidget = self) then
      TlqWidget(Owner).ActiveWidget := nil;
  inherited Destroy;
end;

procedure TlqWidget.AfterConstruction;
begin
  inherited AfterConstruction;
  // This is for components that are created at runtime, after it's
  // parent has already been shown.
  if (Parent <> nil) and (Parent.HasHandle) then
  begin
    HandleShow;
  end;

  Loaded;  // remove csLoading from ComponentState
end;

procedure TlqWidget.MsgKeyChar(var msg: TlqMessageRec);
var
  lChar: TlqChar;
  ss: TShiftState;
  consumed: boolean;
  wg: TlqWidget;
begin
  lChar := msg.params.keyboard.keychar;
  ss  := msg.params.keyboard.shiftstate;

  consumed := False;
  HandleKeyChar(lChar, ss, consumed);

  if not consumed then
  begin
    wg := Parent;
    while (not consumed) and (wg <> nil) do
    begin
      wg.HandleKeyChar(lChar, ss, consumed);
      wg := wg.Parent;
    end;
  end;
end;

procedure TlqWidget.MsgKeyPress(var msg: TlqMessageRec);
var
  key: word;
  ss: TShiftState;
  consumed: boolean;
  wg: TlqWidget;
  wlast: TlqWidget;
begin
  if InDesigner then
  begin
    FFormDesigner.Dispatch(msg);
    if msg.Stop then
      Exit;
  end;

  key := msg.params.keyboard.keycode;
  ss  := msg.params.keyboard.shiftstate;
  consumed := False;

  { can we handle it ourselves? }
  HandleKeyPress(key, ss, consumed);

  { process our children }
  if not consumed then
    DoKeyShortcut(self, key, ss, consumed, True);

  if not consumed then
  begin
    { Work its way to the top level form. The recursive calling of
      HandleKeyPress() also gives tab-to-change-focus a chance to work. }
    wg := Parent;
    wlast := wg;
    while (not consumed) and (wg <> nil) do
    begin
      wg.HandleKeyPress(key, ss, consumed);
      wlast := wg;
      wg := wg.Parent;
    end;
    wg := wlast;
  end;


  if not consumed then
  begin
    { Now let the top level form do keyboard shortcut processing. }
    if Assigned(wg) then
      wg.DoKeyShortcut(self, key, ss, consumed);

    { Forms aren't focusable, so Form.HandleKeyPress() will not suffice. Give
      the Form a chance to fire its OnKeyPress event. }
    if not consumed then
    begin
      if (wg is TlqForm) and Assigned(TlqForm(wg).OnKeyPress) then
        wg.OnKeyPress(self, key, ss, consumed);
    end;

    { now try the Application MainForm - if not the same as top-level form }
    if not consumed then
    begin
      { only do this if the top-level form is not Modal }
      if (wg is TlqForm) and (TlqForm(wg).WindowType <> wtModalForm) then
        if wg <> lqApplication.MainForm then
          TlqFormFriend(lqApplication.MainForm).DoKeyShortcut(self, key, ss, consumed);
    end;
  end;

  { now finaly, lets give lqApplication a chance }
  if (not consumed) and Assigned(lqApplication.OnKeyPress) then
    lqApplication.OnKeyPress(self, key, ss, consumed);
end;

procedure TlqWidget.MsgKeyRelease(var msg: TlqMessageRec);
var
  key: word;
  ss: TShiftState;
  consumed: boolean;
  wg: TlqWidget;
begin
  if InDesigner then
  begin
    FFormDesigner.Dispatch(msg);
    if msg.Stop then
      Exit;
  end;

  key := msg.params.keyboard.keycode;
  ss  := msg.params.keyboard.shiftstate;
  consumed := False;

  HandleKeyRelease(key, ss, consumed);
  if not consumed then
  begin
    wg := Parent;
    while (not consumed) and (wg <> nil) do
    begin
      wg.HandleKeyRelease(key, ss, consumed);
      wg := wg.Parent;
    end;
  end;
end;

procedure TlqWidget.MsgMouseDown(var msg: TlqMessageRec);
var
  mb: TMouseButton;
begin
  if InDesigner then
  begin
    // dispatching message to designer
    FFormDesigner.Dispatch(msg);
    if msg.Stop then
      Exit;
  end;

  if not FEnabled then
    exit;   // Do we want this here?

  case msg.Params.mouse.Buttons of
    MOUSE_LEFT:
      begin
        uMouseDownSourceWidget := self;
        FDragStartPos.SetPoint(msg.Params.mouse.x, msg.Params.mouse.y);
        mb := mbLeft;
        HandleLMouseDown(msg.Params.mouse.x, msg.Params.mouse.y, msg.Params.mouse.shiftstate);
      end;

    MOUSE_RIGHT:
      begin
        mb := mbRight;
        HandleRMouseDown(msg.Params.mouse.x, msg.Params.mouse.y, msg.Params.mouse.shiftstate);
      end;

    MOUSE_MIDDLE:
      begin
        mb := mbMiddle;
      end;
  end;
  if Assigned(FOnMouseDown) then
    FOnMouseDown(self, mb, msg.Params.mouse.shiftstate,
        Point(msg.Params.mouse.x, msg.Params.mouse.y));
end;

procedure TlqWidget.MsgMouseUp(var msg: TlqMessageRec);
var
  mb: TMouseButton;
  IsDblClick: boolean;
begin
  //writeln('>> TlqWidget.MsgMouseUp - ', Classname, '.', Name);
  FDragActive := False;
  if InDesigner then
  begin
    FFormDesigner.Dispatch(msg);
    if msg.Stop then
      Exit;
  end;

  if not FEnabled then
    exit;   // Do we want this here?
    
  IsDblClick := False;

  case msg.Params.mouse.Buttons of
    MOUSE_LEFT:
      begin
        FOnClickPending := True;
        mb := mbLeft;
        if (uLastClickWidget = self) and (not FIgnoreDblClicks) then
          IsDblClick := ((lqGetTickCount - uLastClickTime) <= DOUBLECLICK_MS)
            and (Abs(uLastClickPoint.x - msg.Params.mouse.x) <= DOUBLECLICK_DISTANCE)
            and (Abs(uLastClickPoint.y - msg.Params.mouse.y) <= DOUBLECLICK_DISTANCE)
          // we detected a double click
        else
          uLastClickWidget := self;

        uLastClickPoint := Point(msg.Params.mouse.x, msg.Params.mouse.y);
        uLastClickTime := lqGetTickCount;
        if IsDblClick then
        begin
          FOnClickPending := False; { When Double Click occurs we don't want single click }
          HandleDoubleClick(msg.Params.mouse.x, msg.Params.mouse.y, msg.Params.mouse.Buttons, msg.Params.mouse.shiftstate);
          if Assigned(FOnDoubleClick) then
            FOnDoubleClick(self, mb, msg.Params.mouse.shiftstate,
                Point(msg.Params.mouse.x, msg.Params.mouse.y));
        end;

        // The mouse up must still be handled even if we had a double click event.
        HandleLMouseUp(msg.Params.mouse.x, msg.Params.mouse.y, msg.Params.mouse.shiftstate);

        uMouseDownSourceWidget := nil;
      end;

    MOUSE_RIGHT:
      begin
        mb := mbRight;
        HandleRMouseUp(msg.Params.mouse.x, msg.Params.mouse.y, msg.Params.mouse.shiftstate);
      end;

    MOUSE_MIDDLE:
      begin
        mb := mbMiddle;
      end;
  end;
  if Assigned(FOnMouseUp) then // and not IsDblClick then
    FOnMouseUp(self, mb, msg.Params.mouse.shiftstate,
        Point(msg.Params.mouse.x, msg.Params.mouse.y));
  //writeln('<< TlqWidget.MsgMouseUp - ', Classname, '.', Name);
end;

procedure TlqWidget.MsgMouseMove(var msg: TlqMessageRec);
begin
  if InDesigner then
  begin
    FFormDesigner.Dispatch(msg);
    if msg.Stop then
      Exit;
  end;

  if ((msg.Params.mouse.Buttons and MOUSE_LEFT) = MOUSE_LEFT) and (self = uMouseDownSourceWidget) then
  begin
    if not FDragActive and (FDragStartPos.ManhattanLength(lqPoint(msg.Params.mouse.x, msg.Params.mouse.y)) > lqApplication.StartDragDistance) then
    begin
      FDragActive := True;
      // In Windows dragging is a blocking function, so FDragActive is false after this call
      DoDragStartDetected;
    end;
  end;

  HandleMouseMove(msg.Params.mouse.x, msg.Params.mouse.y, msg.Params.mouse.Buttons, msg.Params.mouse.shiftstate);
  if Assigned(OnMouseMove) then
    OnMouseMove(self, msg.Params.mouse.shiftstate,
        Point(msg.Params.mouse.x, msg.Params.mouse.y));
end;

procedure TlqWidget.MsgDoubleClick(var msg: TlqMessageRec);
begin
(*
  // If we don't generate a mouse down, we get a rapid click
  // delay under Windows.
  HandleLMouseDown(msg.Params.mouse.x, msg.Params.mouse.y, msg.Params.mouse.shiftstate);

  HandleDoubleClick(msg.Params.mouse.x, msg.Params.mouse.y,
      msg.Params.mouse.Buttons, msg.Params.mouse.shiftstate);
  if Assigned(FOnDoubleClick) then
    FOnDoubleClick(self, mbLeft, msg.Params.mouse.shiftstate,
        Point(msg.Params.mouse.x, msg.Params.mouse.y));
*)
end;

procedure TlqWidget.MsgMouseEnter(var msg: TlqMessageRec);
{$IFDEF Debug}
var
  itf: IInterface;
{$ENDIF}
begin
  {$IFDEF Debug}
  itf := DebugMethodEnter('TlqWidget.MsgMouseEnter - ' + ClassName + ' ('+Name+')');
  {$ENDIF}
  if InDesigner then
  begin
    FFormDesigner.Dispatch(msg);
    if msg.Stop then
      Exit;
  end;

  HandleMouseEnter;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(self);
end;

procedure TlqWidget.MsgMouseExit(var msg: TlqMessageRec);
{$IFDEF Debug}
var
  itf: IInterface;
{$ENDIF}
begin
  {$IFDEF Debug}
  itf := DebugMethodEnter('TlqWidget.MsgMouseExit - ' + ClassName + ' ('+Name+')');
  {$ENDIF}
  if InDesigner then
  begin
    FFormDesigner.Dispatch(msg);
    if msg.Stop then
      Exit;
  end;

  HandleMouseExit;
  if Assigned(FOnMouseExit) then
    FOnMouseExit(Self);
end;

procedure TlqWidget.MsgMouseScroll(var msg: TlqMessageRec);
begin
  HandleMouseScroll(msg.Params.mouse.x, msg.Params.mouse.y,
      msg.Params.mouse.shiftstate, msg.Params.mouse.delta);
end;

procedure TlqWidget.MsgDropEnter(var msg: TlqMessageRec);
begin
  // do nothing
end;

procedure TlqWidget.MsgDropExit(var msg: TlqMessageRec);
begin
  // do nothing
end;

function TlqWidget.GetOnShowHint: THintEvent;
begin
  Result := FOnShowHint;
end;

procedure TlqWidget.SetOnShowHint(const AValue: THintEvent);
begin
  FOnShowHint := AValue;
end;

procedure TlqWidget.HandleShow;
var
  n: integer;
  c: TComponent;
begin
  FOnScreen := True;
  AllocateWindowHandle;
  DoSetWindowVisible(FVisible);

  for n := 0 to ComponentCount - 1 do
  begin
    c := Components[n];
    if (c is TlqWidget) and (TlqWidget(c).Parent = self) then
    begin
      if not (c is TlqPopupMenu) then  // these should not be created yet
      begin
        TlqWidget(c).HandleShow;
      end;
    end;
  end;
end;

procedure TlqWidget.InternalHandleShow;
begin
  FOnScreen := True;
  AllocateWindowHandle;
  DoSetWindowVisible(FVisible);
end;

procedure TlqWidget.HandleHide;
var
  n: integer;
  c: TComponent;
begin
  for n := 0 to ComponentCount - 1 do
  begin
    c := Components[n];
    if (c is TlqWidget) and (TlqWidget(c).Parent = self) then
      TlqWidget(c).HandleHide;
  end;
  FOnScreen := False;

  if HasHandle then
    ReleaseWindowHandle;
end;

procedure TlqWidget.RePaint;
begin
  if HasHandle or LQU_PREVENTWND then
    lqSendMessage(self, self, LQM_PAINT);
end;

procedure TlqWidget.SetFocus;
begin
  HandleSetFocus;
end;

procedure TlqWidget.KillFocus;
begin
  HandleKillFocus;
end;

procedure TlqWidget.HandlePaint;
begin
  // descendants will implement this.
end;

procedure TlqWidget.HandleKeyChar(var AText: TlqChar; var shiftstate: TShiftState; var consumed: boolean);
begin
  // descendants will implement this.
end;

procedure TlqWidget.HandleKeyPress(var keycode: word; var shiftstate: TShiftState;
    var consumed: boolean);
var
  wg: TlqWidget;
  dir: integer;
begin
  if Assigned(OnKeyPress) and FFocusable then
    OnKeyPress(self, keycode, shiftstate, consumed);

  if consumed then
    Exit; //==>

  dir := 0;

  if not consumed and (keycode = lqApplication.HelpKey) and (shiftstate=[]) then
  begin
    InvokeHelp;
    consumed := True;
  end;

  case keycode of
    keyTab:
        if (ssShift in shiftstate) then
          dir := -1
        else
          dir := 1;
{
    keyReturn,
    keyDown,
    keyRight:
        dir := 1;

    keyUp,
    keyLeft:
        dir := -1;
}
      keyMenu:
        begin
          // ssExtra1 is a signal that keyMenu was used.
          HandleRMouseDown(Width div 2, Height div 2, [ssExtra1]);
          consumed := True;
        end;
  end;

  {$Note Optimize this code. Constantly setting ActiveWidget causes RePaint to be called!}
  if dir = 1 then
  begin
    // forward
    wg           := FindFocusWidget(ActiveWidget, fsdNext);
    ActiveWidget := wg;
    if wg <> nil then
      consumed := True
    else
    begin
      if Parent = nil then
      begin
        wg           := FindFocusWidget(ActiveWidget, fsdFirst);
        ActiveWidget := wg;
        consumed     := True;
      end;
    end;
  end
  else if dir = -1 then
  begin
    // backward
    wg           := FindFocusWidget(ActiveWidget, fsdPrev);
    ActiveWidget := wg;
    if wg <> nil then
    begin
      consumed := True;
      // we must find the last one!
      while wg <> nil do
      begin
        wg.ActiveWidget := wg.FindFocusWidget(ActiveWidget, fsdLast);
        wg := wg.ActiveWidget;
      end;
    end
    else if Parent = nil then
    begin
      wg           := FindFocusWidget(ActiveWidget, fsdLast);
      ActiveWidget := wg;
      consumed     := True;
    end;
  end;
end;

procedure TlqWidget.HandleKeyRelease(var keycode: word; var shiftstate: TShiftState; var consumed: boolean);
begin
  // descendants will implement this.
end;

procedure TlqWidget.HandleSetFocus;
var
  awg: TlqWidget;
begin
  if not FFocused and FFocusable then
  begin
    FFocused := True;
    RePaint;
    // focusing a child
    if ActiveWidget <> nil then
      ActiveWidget.SetFocus
    else
    begin
      // try to find it for the first time.
      awg := FindFocusWidget(nil, fsdFirst);
      if awg <> nil then
        ActiveWidget := awg;
    end;
  end;

  if Parent <> nil then
  begin
    Parent.ActiveWidget := self;
    Parent.SetFocus;
  end;
  
  if Assigned(OnEnter) then
    OnEnter(self);
end;

procedure TlqWidget.HandleKillFocus;
begin
  FFocused := False;
  RePaint;

  if ActiveWidget <> nil then
    ActiveWidget.KillFocus;
    
  if Assigned(OnExit) then
    OnExit(self);
end;

procedure TlqWidget.HandleLMouseDown(x, y: integer; shiftstate: TShiftState);
var
  pw: TlqWidget;
  w: TlqWidget;
begin
  if FShowHint then
    lqApplication.HideHint;

  // setting the focus through all parents
  pw := Parent;
  w  := self;
  while pw <> nil do
  begin
    if w.Visible and w.Enabled and w.Focusable then
      pw.ActiveWidget := w;
    w := pw;
    pw := pw.Parent;
  end;
end;

procedure TlqWidget.HandleRMouseDown(x, y: integer; shiftstate: TShiftState);
begin
  if FShowHint then
    lqApplication.HideHint;
  // keyMenu was pressed
  if shiftstate = [ssExtra1] then
    HandleRMouseUp(x, y, []);
end;

procedure TlqWidget.HandleLMouseUp(x, y: integer; shiftstate: TShiftState);
var
  r: TlqRect;
begin
  r.SetRect(0, 0, Width, Height);
  if PtInRect(r, Point(x, y)) and FOnClickPending and (self = uMouseDownSourceWidget) then
  begin
    if Assigned(FOnClick) then
    FOnClick(self);
  end;
end;

procedure TlqWidget.HandleRMouseUp(x, y: integer; shiftstate: TShiftState);
begin
  // do nothing yet
end;

procedure TlqWidget.HandleMouseMove(x, y: integer; btnstate: word; shiftstate: TShiftState);
var
  msgp: TlqMessageParams;
begin
  fillchar(msgp, sizeof(msgp), 0);
  msgp.user.Param1 := 2;
  msgp.user.Param2 := x+10;
  msgp.user.Param3 := y+2;

  { Only send message if really needed. }
  if Assigned(Parent) then
  begin
    if lqApplication.ShowHint and (FShowHint or (FParentShowHint and Parent.ShowHint)) and (FHint <> '') then
      lqPostMessage(Self, lqApplication, LQM_HINTTIMER, msgp);
  end
  else
  begin
    if lqApplication.ShowHint and FShowHint and (FHint <> '') then
      lqPostMessage(Self, lqApplication, LQM_HINTTIMER, msgp);
  end;
end;

procedure TlqWidget.HandleDoubleClick(x, y: integer; button: word; shiftstate: TShiftState);
begin
  // do nothing yet
end;

procedure TlqWidget.HandleMouseEnter;
var
  msgp: TlqMessageParams;
  b: boolean;
{$IFDEF Debug}
  itf: IInterface;
{$ENDIF}
begin
  {$IFDEF Debug}
  itf := DebugMethodEnter('TlqWidget.HandleMouseEnter - ' + ClassName + ' ('+Name+')');
  {$ENDIF}
  fillchar(msgp, sizeof(msgp), 0);

  if Assigned(Parent) then
    b := Enabled and lqApplication.ShowHint and (FShowHint or (FParentShowHint and Parent.ShowHint)) and (FHint <> '')
  else
    b := Enabled and lqApplication.ShowHint and FShowHint and (FHint <> '');

  msgp.user.Param1 := Ord(b);
  lqPostMessage(Self, lqApplication, LQM_HINTTIMER, msgp);
end;

procedure TlqWidget.HandleMouseExit;
begin
  {$IFDEF DEBUG}
  writeln('TlqWidget.HandleMouseExit: ' + ClassName);
  {$ENDIF}
  if FShowHint then
    lqApplication.HideHint;
end;

procedure TlqWidget.HandleMouseScroll(x, y: integer; shiftstate: TShiftState; delta: smallint);
begin
  if Assigned(FOnMouseScroll) then
    FOnMouseScroll(self, shiftstate, delta, Point(x, y));
end;

function TlqWidget.FindFocusWidget(startwg: TlqWidget; direction: TFocusSearchDirection): TlqWidget;
var
  w: TlqWidget;
  n: integer;
  FoundIt: boolean;
  lasttaborder: integer;
begin
  Result  := nil;
  FoundIt := False;
  if direction in [fsdLast, fsdPrev] then
    lasttaborder := Low(integer)
  else
    lasttaborder := High(integer);

  for n := 0 to ComponentCount - 1 do
  begin
    if Components[n] is TlqWidget then
    begin
      w := TlqWidget(Components[n]);
      if w.Enabled and w.Visible and w.Focusable then
      begin
        case direction of
          fsdFirst:
            if w.TabOrder < lasttaborder then
            begin
              Result       := w;
              lasttaborder := w.TabOrder;
            end;

          fsdLast:
            if lasttaborder <= w.TabOrder then
            begin
              Result       := w;
              lasttaborder := w.TabOrder;
            end;

          fsdNext:
            if startwg = w then
              FoundIt := True
            else if w.TabOrder < lasttaborder then
            begin
              if (startwg = nil) or
                (w.TabOrder > startwg.TabOrder) or
                (FoundIt and (w.TabOrder = startwg.TabOrder)) then
              begin
                Result       := w;
                lasttaborder := w.TabOrder;
              end;
            end;
          fsdPrev:
            if startwg = w then
              FoundIt := True
            else if w.TabOrder >= lasttaborder then
              if (startwg = nil) or
                (w.TabOrder < startwg.TabOrder) or
                (not FoundIt and (w.TabOrder = startwg.TabOrder)) then
              begin
                Result       := w;
                lasttaborder := w.TabOrder;
              end;

        end; { case }
      end; { if w.Enabled... }
    end;
  end; { if }
end;

procedure TlqWidget.MsgPaint(var msg: TlqMessageRec);
begin
//  writeln('TlqWidget.MsgPaint - ', Classname);
  Canvas.BeginDraw;
  HandlePaint;
  if Assigned(FOnPaint) then
    FOnPaint(Self);
  Canvas.EndDraw;
end;

procedure TlqWidget.MsgResize(var msg: TlqMessageRec);
var
  dw: integer;
  dh: integer;
  _w, _h: integer;
{$IFDEF CStackDebug}
  itf: IInterface;
{$ENDIF}
begin
  {$IFDEF CStackDebug}
  itf := DebugMethodEnter('TlqWidget.MsgResize - ' + ClassName + ' ('+Name+')');
  {$ENDIF}
  _w := FWidth;
  _h := FHeight;
  { Width and Height might not be what came through in the msg because of
    size constraints, so we calculate the delta diffs after HandleResize }
  HandleResize(msg.Params.rect.Width, msg.Params.rect.Height);
  //dw      := msg.Params.rect.Width - FWidth;
  //dh      := msg.Params.rect.Height - FHeight;
  dw := FWidth - _w;
  dh := FHeight - _h;
  HandleAlignments(dw, dh);
  if InDesigner then
  begin
    FFormDesigner.Dispatch(msg);
  end;
  DoResize;
end;

procedure TlqWidget.MsgMove(var msg: TlqMessageRec);
{$IFDEF CStackDebug}
var
  itf: IInterface;
{$ENDIF}
begin
  {$IFDEF CStackDebug}
  itf := DebugMethodEnter('TlqWidget.MsgMove - ' + ClassName + ' ('+Name+')');
  {$ENDIF}
  HandleMove(msg.Params.rect.Left, msg.Params.rect.Top);
  if InDesigner then
  begin
    FFormDesigner.Dispatch(msg);
  end;
end;

procedure TlqWidget.HandleAlignments(const dwidth, dheight: TlqCoord);
var
  n: integer;
  wg: TlqWidget;
  dx: integer;
  dy: integer;
  dw: integer;
  dh: integer;
  w: TlqWidget;
{$IFDEF CStackDebug}
  itf: IInterface;
{$ENDIF}
begin
  {$IFDEF CStackDebug}
  itf := DebugMethodEnter('TlqWidget.HandleAlignments - ' + ClassName + ' ('+Name+')');
  {$ENDIF}
  if (csLoading in ComponentState) then
  begin
    {$IFDEF CStackDebug}
    DebugLn('HandleAlignments ('+Name+'): csLoading detected, so we exit early');
    {$ENDIF}
    Exit;  //==>
  end;

  {$IFDEF CStackDebug}
  DebugLn(Format('dwidth=%d  dheight=%d  Classname=''%s''', [dwidth, dheight, ClassName]));
  {$ENDIF}

  FAlignRect := GetClientRect;
  alist := TList.Create;
  try
    for n := 0 to ComponentCount - 1 do
    begin
      if Components[n] is TlqWidget then
      begin
        w := TlqWidget(Components[n]);
        if (w.Align <> alNone) and (w.Visible) then
          alist.Add(w);
      end;
    end;

    DoAlignment;
    //DoAlign(alTop);
    //DoAlign(alBottom);
    //DoAlign(alLeft);
    //DoAlign(alRight);
    //DoAlign(alClient);
  finally
    alist.Free;
  end;

  // Finally handle anchors (where Align = alNone)
  for n := 0 to ComponentCount - 1 do
    if (Components[n] is TlqWidget) then
    begin
      wg := TlqWidget(Components[n]);
      if (wg.FAlign = alNone) and ([anLeft, anTop] <> wg.Anchors) then
      begin
        // we must alter the window
        dx := 0;
        dy := 0;
        dw := 0;
        dh := 0;

        if (anRight in wg.Anchors) then
          if (anLeft in wg.Anchors) then
            dw := dwidth
          else
            dx := dwidth
        else if not (anLeft in wg.Anchors) then
          dx := (dwidth div 2);

        if (anBottom in wg.Anchors) then
          if (anTop in wg.Anchors) then
            dh := dheight
          else
            dy := dheight
        else if not (anTop in wg.Anchors) then
          dy := (dheight div 2);

        wg.MoveAndResizeBy(dx, dy, dw, dh);
      end;
    end;  { if }
end;

procedure TlqWidget.MoveAndResize(ALeft, ATop, AWidth, AHeight: TlqCoord);
{$IFDEF CStackDebug}
var
  itf: IInterface;
{$ENDIF}
begin
  {$IFDEF CStackDebug}
  itf := DebugMethodEnter('TlqWidget.MoveAndResize');
  DebugLn(Format('Class:%s  t:%d  l:%d  w:%d  h:%d', [Classname, ATop, ALeft, AWidth, aHeight]));
  {$ENDIF}
  if not (csLoading in ComponentState) then
  begin
    if (ALeft <> FLeft) or (ATop <> FTop) then
      HandleMove(ALeft, ATop);
    if (AWidth <> FWidth) or (AHeight <> FHeight) then
      HandleResize(AWidth, AHeight);
  end
  else
  begin
    // When the widget is created, it's position will be applied
    Left   := ALeft;
    Top    := ATop;
    Width  := AWidth;
    Height := AHeight;
  end;
  UpdateWindowPosition;
end;

function TlqWidget.ChildrenCount: integer;
begin
  Result:=FChilds.Count;
end;

function TlqWidget.GetParentComponent: TComponent;
begin
  //Result:=inherited GetParentComponent;
  Result := Parent;      
end;

procedure TlqWidget.MoveAndResizeBy(const dx, dy, dw, dh: TlqCoord);
begin
  if (dx <> 0) or (dy <> 0) or
    (dw <> 0) or (dh <> 0) then
    MoveAndResize(FLeft + dx, FTop + dy, FWidth + dw, FHeight + dh);
end;

procedure TlqWidget.DoAlignment;
var
  w: TlqWidget;
  n: integer;
begin
  // and process this list in order
  for n := 0 to alist.Count - 1 do
  begin
    w := TlqWidget(alist[n]);
    case w.Align of
      alTop:
        begin
          w.MoveAndResize(FAlignRect.Left, FAlignRect.Top, FAlignRect.Width, w.Height);
          Inc(FAlignRect.top, w.Height);
          Dec(FAlignRect.Height, w.Height);
        end;

      alBottom:
        begin
          w.MoveAndResize(FAlignRect.Left, FAlignRect.Top + FAlignRect.Height - w.Height, FAlignRect.Width, w.Height);
          Dec(FAlignRect.Height, w.Height);
        end;

      alLeft:
        begin
          w.MoveAndResize(FAlignRect.Left, FAlignRect.Top, w.Width, FAlignRect.Height);
          Inc(FAlignRect.Left, w.Width);
          Dec(FAlignRect.Width, w.Width);
        end;

      alRight:
        begin
          w.MoveAndResize(FAlignRect.Left + FAlignRect.Width - w.Width, FAlignRect.Top, w.Width, FAlignRect.Height);
          Dec(FAlignRect.Width, w.Width);
        end;

      alClient:
        w.MoveAndResize(FAlignRect.Left, FAlignRect.Top, FAlignRect.Width, FAlignRect.Height);
    end; { case }
  end;
end;

procedure TlqWidget.DoResize;
begin
  if Assigned(FOnResize) then
    FOnResize(Self);
end;

procedure TlqWidget.DoShowHint(var AHint: TlqString);
begin
  AHint := Hint;
  if Assigned(FOnShowHint) then
  begin
    FOnShowHint(self, AHint);
  end;
end;

procedure TlqWidget.DoKeyShortcut(const AOrigin: TlqWidget;
  const keycode: word; const shiftstate: TShiftState; var consumed: boolean; const IsChildOfOrigin: boolean = False);
var
  c: TlqComponent;
  wg: TlqWidget;
  i: integer;
begin
  //writeln(Classname, ' - ', Name, '.DoKeyShortcut() - ' + KeycodeToText(keycode, shiftstate));
  { process children of self }
  for i := 0 to ComponentCount-1 do
  begin
    c := TlqComponent(Components[i]);
    if not (c is TlqWidget) then
    begin
      //writeln('** skipped ', Classname, ' - ', Name);
      continue;
    end
    else
      wg := TlqWidget(c);
    if (wg <> nil) and (wg <> self) and (wg <> AOrigin) then
    begin
      { ignore the MenuBar now, because it will be processed later by the top-level Form }
      if IsChildOfOrigin and (wg is TlqMenuBar) then
      begin
        continue;
      end
      else
        wg.DoKeyShortcut(AOrigin, keycode, shiftstate, consumed);
      if consumed then
        Exit;
    end;
  end;
end;

procedure TlqWidget.SetPosition(aleft, atop, awidth, aheight: TlqCoord);
{$IFDEF CStackDebug}
var
  itf: IInterface;
{$ENDIF}
begin
  {$IFDEF CStackDebug}
  itf := DebugMethodEnter('TlqWidget.SetPosition - ' + ClassName + ' ('+Name+')');
  {$ENDIF}
  if (FLeft <> ALeft) or (FTop <> ATop) or (FWidth <> AWidth) or (FHeight <> AHeight) then
    MoveAndResize(aleft, atop, awidth, aheight);
end;

procedure TlqWidget.Invalidate;
begin
  if [csLoading, csDestroying] * self.ComponentState <> [] then
     exit;
  RePaint;
end;


function TlqWidget.HasParent: Boolean;
begin
  Result:=Parent<>nil;
end;

procedure TlqWidget.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  i: Integer;
  OwnedComponent: TComponent;
begin
  {for i := 0 to ComponentCount-1 do
  begin
    if Components[i].Owner=Root then
      Proc(Components[i]);
  end;}
  //inherited GetChildren(Proc, Root);
  {if Root = Self then begin
    for I := 0 to ComponentCount - 1 do
    begin
      OwnedComponent := Components[I];
      if not OwnedComponent.HasParent then Proc(OwnedComponent);
    end;
  end;}
  {for I := 0 to ControlCount - 1 do
  begin
    Control := Controls[I];
    if Control.Owner = Root then Proc(Control);
  end;]}
  for i:=0 to ChildrenCount-1 do
      if Children[i].Owner=Root then
        Proc(Children[i]);

  if Root = Self then
    for I := 0 to ComponentCount - 1 do
    begin
      OwnedComponent := Components[I];
      if not OwnedComponent.HasParent then Proc(OwnedComponent);
    end;
  {
  if self is TMyForm then
  begin
    for i := 0 to ComponentCount-1 do
        if not (Components[i] is TpgfWidget) then
           Proc(Components[i])
  end}

end;


procedure TlqWidget.SetParentComponent(Value: TComponent);
begin
  if Value is TlqWidget then
    Parent:=TlqWidget(Value);
end;

procedure TlqWidget.AllocateWindowHandle;
//moved here because FParent is introduced here
begin
  DoAllocateWindowHandle(FParent);
  if FMouseCursorIsDirty then
    DoSetMouseCursor;
end;

initialization
  FocusRootWidget := nil;
  uLastClickWidget := nil;
  uLastClickTime := 0;

end.

