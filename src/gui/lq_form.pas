{
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2011 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Defines a Form control. Also known as a Window which holds other
      controls.
}

unit lq_form;

{$mode objfpc}{$H+}

{.$Define CStackDebug}

interface

uses
  Classes,
  SysUtils,
  lq_base,
  lq_widget;

type
  TWindowPosition = (wpUser, wpAuto, wpScreenCenter, wpOneThirdDown);
  TCloseAction = (caNone, caHide, caFree{, caMinimize});
  
  TFormCloseEvent = procedure(Sender: TObject; var CloseAction: TCloseAction) of object;
  TFormCloseQueryEvent = procedure(Sender: TObject; var CanClose: boolean) of object;
  TlqHelpEvent = function(AHelpType: THelpType; AHelpContext: THelpContext;
       const AHelpKeyword: String; const AHelpFile: String;
       var AHandled: Boolean): Boolean of object;


  TlqBaseForm = class(TlqWidget)
  private
    FFullScreen: boolean;
    FOnActivate: TNotifyEvent;
    FOnClose: TFormCloseEvent;
    FOnCloseQuery: TFormCloseQueryEvent;
    FOnCreate: TNotifyEvent;
    FOnDeactivate: TNotifyEvent;
    FOnDestroy: TNotifyEvent;
    FOnHide: TNotifyEvent;
    FOnShow: TNotifyEvent;
    FOnHelp: TlqHelpEvent;
    FDNDEnabled: boolean;
    procedure   SetDNDEnabled(const AValue: boolean);
  protected
    FModalResult: TlqModalResult;
    FParentForm: TlqBaseForm;
    FWindowPosition: TWindowPosition;
    FWindowTitle: string;
    FSizeable: boolean;
    procedure   SetWindowTitle(const ATitle: string); override;
    procedure   MsgActivate(var msg: TlqMessageRec); message FPGM_ACTIVATE;
    procedure   MsgDeActivate(var msg: TlqMessageRec); message FPGM_DEACTIVATE;
    procedure   MsgClose(var msg: TlqMessageRec); message FPGM_CLOSE;
    procedure   HandlePaint; override;
    procedure   HandleClose; virtual;
    procedure   HandleHide; override;
    procedure   HandleShow; override;
    procedure   HandleMove(x, y: TlqCoord); override;
    procedure   HandleResize(awidth, aheight: TlqCoord); override;
    procedure   DoOnClose(var CloseAction: TCloseAction); virtual;
    function    DoOnHelp(AHelpType: THelpType; AHelpContext: THelpContext; const AHelpKeyword: String; const AHelpFile: String; var AHandled: Boolean): Boolean; virtual;
    procedure   DoKeyShortcut(const AOrigin: TlqWidget; const keycode: word; const shiftstate: TShiftState; var consumed: boolean; const IsChildOfOrigin: boolean = False); override;
    { -- properties -- }
    property    DNDEnabled: boolean read FDNDEnabled write SetDNDEnabled default False;
    property    Sizeable: boolean read FSizeable write FSizeable;
    property    ModalResult: TlqModalResult read FModalResult write FModalResult;
    property    FullScreen: boolean read FFullScreen write FFullScreen default False;
    property    WindowPosition: TWindowPosition read FWindowPosition write FWindowPosition default wpAuto;
    property    WindowTitle: string read FWindowTitle write SetWindowTitle;
    { -- events -- }
    property    OnActivate: TNotifyEvent read FOnActivate write FOnActivate;
    property    OnClose: TFormCloseEvent read FOnClose write FOnClose;
    property    OnCloseQuery: TFormCloseQueryEvent read FOnCloseQuery write FOnCloseQuery;
    property    OnCreate: TNotifyEvent read FOnCreate write FOnCreate;
    property    OnDeactivate: TNotifyEvent read FOnDeactivate write FOnDeactivate;
    property    OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
    property    OnHelp: TlqHelpEvent read FOnHelp write FOnHelp;
    property    OnHide: TNotifyEvent read FOnHide write FOnHide;
    property    OnShow: TNotifyEvent read FOnShow write FOnShow;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   AfterConstruction; override;
    procedure   BeforeDestruction; override;
    procedure   AfterCreate; virtual;
    procedure   AdjustWindowStyle; override;
    procedure   SetWindowParameters; override;
    procedure   InvokeHelp; override;
    procedure   Show;
    procedure   Hide;
    function    ShowModal: TlqModalResult;
    procedure   Close;
    function    CloseQuery: boolean; virtual;
  end;
  
  
  TlqForm = class(TlqBaseForm)
  published
    property    BackgroundColor;
    property    DNDEnabled;
    property    FullScreen;
    property    Height;
    property    Hint;
    property    Left;
    property    MaxHeight;
    property    MaxWidth;
    property    MinHeight;
    property    MinWidth;
    property    ModalResult;
    property    ShowHint;
    property    Sizeable;
    property    TextColor;
    property    Top;
    property    Width;
    property    WindowPosition;
    property    WindowState;
    property    WindowTitle;
    property    OnActivate;
    property    OnClick;
    property    OnClose;
    property    OnCloseQuery;
    property    OnCreate;
    property    OnDeactivate;
    property    OnDestroy;
    property    OnDoubleClick;
    property    OnEnter;
    property    OnExit;
    property    OnHide;
    property    OnKeyPress;
    property    OnMouseDown;
    property    OnMouseEnter;
    property    OnMouseExit;
    property    OnMouseMove;
    property    OnMouseScroll;
    property    OnMouseUp;
    property    OnPaint;
    property    OnResize;
    property    OnShow;
    property    OnShowHint;
  end;


function WidgetParentForm(wg: TlqWidget): TlqForm;


implementation

uses
  lq_main,
  lq_popupwindow,
  lq_menu;
  
type
  // to access protected methods
  TlqMenuBarFriend = class(TlqMenuBar)
  end;

  TlqWidgetFriend = class(TlqWidget)
  end;


function WidgetParentForm(wg: TlqWidget): TlqForm;
var
  w: TlqWidget;
begin
  w := wg;
  while w <> nil do
  begin
    if w is TlqForm then
    begin
      Result := TlqForm(w);
      Exit; //==>
    end;
    w := w.Parent;
  end;
  Result := nil;
end;

{ TlqBaseForm }

procedure TlqBaseForm.SetDNDEnabled(const AValue: boolean);
begin
  if FDNDEnabled = AValue then exit;
  FDNDEnabled := AValue;
  DoDNDEnabled(AValue);
end;

procedure TlqBaseForm.SetWindowTitle(const ATitle: string);
begin
  FWindowTitle := ATitle;
  inherited SetWindowTitle(ATitle);
end;

procedure TlqBaseForm.MsgActivate(var msg: TlqMessageRec);
begin
  {$IFDEF DEBUG}
  DebugLn(Classname + ' ' + Name + '.BaseForm - MsgActivate');
  {$ENDIF}
  if (fpgApplication.TopModalForm = nil) or (fpgApplication.TopModalForm = self) then
  begin
    {$IFDEF DEBUG}
    DebugLn('Inside if block');
    {$ENDIF}
    FocusRootWidget := self;
    
    if FFormDesigner <> nil then
    begin
      FFormDesigner.Dispatch(msg);
      Exit;
    end;

    if ActiveWidget = nil then
      ActiveWidget := FindFocusWidget(nil, fsdFirst)
    else
      ActiveWidget.SetFocus;
  end;

  if Assigned(FOnActivate) then
    FOnActivate(self);
end;

procedure TlqBaseForm.MsgDeActivate(var msg: TlqMessageRec);
begin
  ClosePopups;
  if ActiveWidget <> nil then
    ActiveWidget.KillFocus;
  if Assigned(FOnDeactivate) then
    FOnDeactivate(self);
end;

procedure TlqBaseForm.HandlePaint;
begin
  inherited HandlePaint;
  Canvas.Clear(FBackgroundColor);
end;

procedure TlqBaseForm.AdjustWindowStyle;
begin
  if fpgApplication.MainForm = nil then
    fpgApplication.MainForm := self;

  if FWindowPosition = wpAuto then
    Include(FWindowAttributes, waAutoPos)
  else
    Exclude(FWindowAttributes, waAutoPos);

  if FWindowPosition = wpScreenCenter then
    Include(FWindowAttributes, waScreenCenterPos)
  else
    Exclude(FWindowAttributes, waScreenCenterPos);

  if FWindowPosition = wpOneThirdDown then
    Include(FWindowAttributes, waOneThirdDownPos)
  else
    Exclude(FWindowAttributes, waOneThirdDownPos);

  if FSizeable then
    Include(FWindowAttributes, waSizeable)
  else
    Exclude(FWindowAttributes, waSizeable);
    
  if FFullScreen then
    Include(FWindowAttributes, waFullScreen)
  else
    Exclude(FWindowAttributes, waFullScreen);
end;

procedure TlqBaseForm.SetWindowParameters;
begin
  inherited;
  DoSetWindowTitle(FWindowTitle);
end;

constructor TlqBaseForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWindowPosition  := wpAuto;
  FWindowTitle     := '';
  FSizeable        := True;
  FParentForm      := nil;
  FBackgroundColor := clWindowBackground;
  FTextColor       := clText1;
  FMinWidth        := 32;
  FMinHeight       := 32;
  FModalResult     := mrNone;
  FFullScreen      := False;
  FIsContainer     := True;
  FDNDEnabled      := False;
end;

destructor TlqBaseForm.Destroy;
begin
  fpgApplication.RemoveWindowFromModalStack(Self);
  inherited Destroy;
end;

procedure TlqBaseForm.AfterCreate;
begin
  // for the user
end;

procedure TlqBaseForm.InvokeHelp;
var
  lEventHandled: Boolean;
  lSucceeded: Boolean;
begin
  lEventHandled := False;
  lSucceeded := False;
  lSucceeded := DoOnHelp(HelpType, HelpContext, HelpKeyword, fpgApplication.HelpFile, lEventHandled);
  if (not lSucceeded) or (not lEventHandled) then
    inherited InvokeHelp;
end;

procedure TlqBaseForm.Show;
{$IFDEF CStackDebug}
var
  itf: IInterface;
{$ENDIF}
begin
  {$IFDEF CStackDebug}
  itf := DebugMethodEnter('TlqBaseForm.Show - ' + ClassName + ' ('+Name+')');
  {$ENDIF}
  FVisible := True;
  HandleShow;
end;

function TlqBaseForm.ShowModal: TlqModalResult;
var
  lCloseAction: TCloseAction;
begin
  FWindowType := wtModalForm;
  fpgApplication.PushModalForm(self);
  ModalResult := mrNone;

  try
    Show;
    // processing messages until this form ends.
    // delivering the remaining messages
    fpgApplication.ProcessMessages;
    try
      repeat
        fpgWaitWindowMessage;
      until (ModalResult <> mrNone) or (not Visible);
    except
      on E: Exception do
      begin
        Visible := False;
        fpgApplication.HandleException(self);
      end;
    end;  { try..except }
  finally
    { we have to pop the form even in an error occurs }
    fpgApplication.PopModalForm;
    Result := ModalResult;
  end;  { try..finally }
  
  if ModalResult <> mrNone then
  begin
    lCloseAction := caHide; // Dummy variable - we do nothing with it
    DoOnClose(lCloseAction); // Simply so the OnClose event fires.
  end;
end;

procedure TlqBaseForm.MsgClose(var msg: TlqMessageRec);
begin
  HandleClose;
end;

procedure TlqBaseForm.HandleClose;
begin
  Close;
end;

procedure TlqBaseForm.HandleHide;
begin
  if Assigned(FOnHide) then
    FOnHide(self);
  inherited HandleHide;
end;

procedure TlqBaseForm.HandleShow;
begin
  inherited HandleShow;
  HandleAlignments(0, 0);
  if Assigned(FOnShow) then
    FOnShow(self);
end;

procedure TlqBaseForm.HandleMove(x, y: TlqCoord);
{$IFDEF CStackDebug}
var
  itf: IInterface;
{$ENDIF}
begin
  {$IFDEF CStackDebug}
  itf := DebugMethodEnter('TlqBaseForm.HandleMove - ' + ClassName + ' ('+Name+')');
  DebugLn(Format('x:%d  y:%d', [x, y]));
  {$ENDIF}
  ClosePopups;
  inherited HandleMove(x, y);
end;

procedure TlqBaseForm.HandleResize(awidth, aheight: TlqCoord);
{$IFDEF CStackDebug}
var
  i: iinterface;
{$ENDIF}
begin
  {$IFDEF CStackDebug}
  DebugMethodEnter('TlqBaseForm.HandleResize - ' + ClassName + ' ('+Name+')');
  DebugLn(Format('w:%d  h:%d', [awidth, aheight]));
  {$ENDIF}
  ClosePopups;
  inherited HandleResize(awidth, aheight);
end;

procedure TlqBaseForm.AfterConstruction;
begin
  AfterCreate;
  inherited AfterConstruction;
  if Assigned(FOnCreate) then
    FOnCreate(self);
end;

procedure TlqBaseForm.BeforeDestruction;
begin
  inherited BeforeDestruction;
  if Assigned(FOnDestroy) then
    FOnDestroy(self);
end;

procedure TlqBaseForm.DoOnClose(var CloseAction: TCloseAction);
begin
  if Assigned(FOnClose) then
    OnClose(self, CloseAction);
end;

function TlqBaseForm.DoOnHelp(AHelpType: THelpType; AHelpContext: THelpContext;
  const AHelpKeyword: String; const AHelpFile: String; var AHandled: Boolean): Boolean;
begin
  if Assigned(FOnHelp) then
    Result := FOnHelp(AHelpType, AHelpContext, AHelpKeyword, AHelpFile, AHandled);
end;

procedure TlqBaseForm.DoKeyShortcut(const AOrigin: TlqWidget;
  const keycode: word; const shiftstate: TShiftState; var consumed: boolean; const IsChildOfOrigin: boolean = False);
var
  wg: TlqWidget;
  menu: TlqMenuBar;
  i: integer;
  ss: TShiftState;
  key: word;
  c: TlqComponent;

  function FindMenuBar(AWidget: TlqWidget): TlqWidget;
  var
    n: integer;
    w: TlqWidget;
  begin
    Result := nil;
    for n := 0 to AWidget.ComponentCount-1 do
    begin
      w := TlqWidget(AWidget.Components[n]);
      if (w <> nil) and (w <> self) and (w <> AOrigin) and (w is TlqMenuBar) then
      begin
        Result := w;
        exit;
      end;
      if w.ComponentCount > 0 then
        Result := FindMenuBar(w);
      if Result <> nil then
        exit;
    end;
  end;

begin
  // find the first TlqMenuBar - if it exits
  wg := FindMenuBar(self);
  if (wg <> nil) then
  begin
    menu := wg as TlqMenuBar;
    key := keycode;
    ss := shiftstate;
    TlqMenuBarFriend(wg).HandleKeyPress(key, ss, consumed);
  end;

  if consumed then
    Exit;
  // now send to each widget on the form - excluding AOrigin and MenuBar widgets
  for i := 0 to ComponentCount-1 do
  begin
    c := TlqComponent(Components[i]);
    if c is TlqWidget then
      wg := TlqWidget(c)
    else
      wg := nil;
    if (wg <> nil) and (wg <> self) and (wg <> AOrigin) and (wg <> menu) and (not (wg is TlqPopupMenu)) then
    begin
      if (not wg.Visible) or (not wg.Enabled) then
        continue
      else
      begin
        TlqWidgetFriend(wg).DoKeyShortcut(AOrigin, keycode, shiftstate, consumed);
        if consumed then
          Exit;
      end;
    end;
  end;
end;

procedure TlqBaseForm.Hide;
begin
  Visible := False;
end;

procedure TlqBaseForm.Close;
var
  CloseAction: TCloseAction;
  IsMainForm: Boolean;
begin
  if CloseQuery then  // May we close the form? User could override decision
  begin
    IsMainForm := fpgApplication.MainForm = self;
    if IsMainForm then
      CloseAction := caFree
    else
      CloseAction := caHide;

    // execute event handler - maybe user wants to modify it.
    DoOnClose(CloseAction);
    // execute action according to close action
    case CloseAction of
      caHide:
        begin
          Hide;
        end;
        // fpGUI Forms don't have a WindowState property yet!
//      caMinimize: WindowState := wsMinimized;
      caFree:
        begin
          HandleHide;
          if IsMainForm then
            fpgApplication.Terminate
          else
            // We can't free ourselves, somebody else needs to do it
            fpgPostMessage(Self, fpgApplication, FPGM_FREEME);
        end;
    end;  { case CloseAction }
  end;  { if CloseQuery }
end;

function TlqBaseForm.CloseQuery: boolean;
begin
  Result := True;
  if Assigned(FOnCloseQuery) then
    FOnCloseQuery(self, Result);
end;


end.

