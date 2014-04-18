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
      Defines a spinedit control.
}

unit lq_spinedit;

{$mode objfpc}{$H+}

{
    *****************************************************************
    **********   This is still under heavy development!   ***********
    *****************************************************************
}

{ TODO : Base classes need to be abstracted from final classes. }
{ TODO : Up/Down keyboard input needs to be corrected. }
{ TODO : Step size needs to be implemented (small and large) }
{ TODO : PgUp/PgDn keyboard needs to be supported. }
{ TODO : Improve Timer and Step support. If the mouse is kept down on
         a button, it should increment by small steps. After a certain
         period, it should start incrementing by large steps. }
{ TODO : Text cursor positioning should be fixed. }

interface

uses
  Classes,
  SysUtils,
  lq_base,
  lq_main,
  lq_widget,
  lq_panel,
  lq_edit,
  lq_button;

type


  TlqAbstractSpinEdit = class(TlqBevel)
  private
    FButtonUp: TlqButton;
    FButtonDown: TlqButton;
    FArrowUpColor: TlqColor;
    FArrowDownColor: TlqColor;
    FOnChange: TNotifyEvent;
    FTimer: TlqTimer;
    FUp: Boolean;
    FDown: Boolean;
    FSteps: integer;
    FSpeedUpSteps: integer;
    procedure   SetButtonWidth(const AValue: integer);
  protected
    FButtonWidth: integer;
    procedure   DoOnChange; virtual;
    procedure   ResizeChildren; virtual;
    procedure   HandlePaint; override;
    procedure   HandleResize(AWidth, AHeight: TlqCoord); override;
    function    IsMinLimitReached: Boolean; virtual; abstract;
    function    IsMaxLimitReached: Boolean; virtual; abstract;
    function    GetButtonsBackgroundColor: TlqColor;
    procedure   SetButtonsBackgroundColor(const AValue: Tlqcolor);
    procedure   SetArrowUpColor(const AValue: Tlqcolor);
    procedure   SetArrowDownColor(const AValue: Tlqcolor);
    procedure   SetSpeedUp(const AValue: integer);
    procedure   DisableTimer;
    procedure   ButtonUpPaint(Sender: TObject);
    procedure   ButtonDownPaint(Sender: TObject);
    property    ButtonsBackgroundColor: Tlqcolor read GetButtonsBackgroundColor write SetButtonsBackgroundColor default clButtonFace;
    property    ArrowUpColor: TlqColor read FArrowUpColor write SetArrowUpColor;
    property    ArrowDownColor: TlqColor read FArrowDownColor write SetArrowDownColor;
    property    ButtonWidth: integer read FButtonWidth write SetButtonWidth default 13;
    property    StepsSpeedUp: integer read FSpeedUpSteps write SetSpeedUp;
    property    OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create(AOwner: TComponent); override;
  end;


  TlqSpinEditFloat = class(TlqAbstractSpinEdit)
  private
    FEdit: TlqEditFloat;
    FLargeIncrement: extended;
    FMaxValue: extended;
    FMinValue: extended;
    FIncrement: extended;
    FTempIncrement: extended;   // value varies depending on increment speed
    FValue: extended;
    procedure EnableButtons;
  protected
    function IsMinLimitReached: Boolean; override;
    function IsMaxLimitReached: Boolean; override;
    function GetEditBackgroundColor: TlqColor;
    function GetTextColor: TlqColor;
    function GetNegativeColor: TlqColor;
    function GetFontDesc: string;
    function GetDecimals: integer;
    function GetFixedDecimals: integer;
    procedure ResizeChildren; override;
    procedure SetEditBackgroundColor(const AValue: Tlqcolor);
    procedure SetTextColor(const AValue: Tlqcolor); override;
    procedure SetNegativeColor(const AValue: Tlqcolor);
    procedure SetFontDesc(const AValue: string);
    procedure SetMaxValue(const AValue: extended);
    procedure SetMinValue(const AValue: extended);
    procedure SetIncrement(const AValue: extended);
    procedure SetLargeIncrement(const AValue: extended);
    procedure SetValue(const AValue: extended);
    procedure SetDecimals(const AValue: integer);
    procedure SetFixedDecimals(const AValue: integer);
    procedure SetHint(const AValue: TlqString); override;
    procedure ButtonUpClick(Sender: TObject);
    procedure ButtonDownClick(Sender: TObject);
    procedure ButtonUpMouseDown(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
    procedure ButtonUpMouseUp(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
    procedure ButtonDownMouseDown(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
    procedure ButtonDownMouseUp(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
    procedure EditKeyPress(Sender: TObject; var keycode: word; var shiftstate: TShiftState; var consumed: Boolean);
    procedure EditExit(Sender: TObject);
    procedure MouseEnter(Sender: TObject);
    procedure MouseMove(Sender: TObject; AShift: TShiftState; const AMousePos: TPoint);
    procedure MouseExit(Sender: TObject);
    procedure TimerStep(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property EditBackgroundColor: Tlqcolor read GetEditBackgroundColor write SetEditBackgroundColor default clBoxColor;
    property ButtonsBackgroundColor;
    property ButtonWidth;
    property TextColor: Tlqcolor read GetTextColor write SetTextColor;
    property NegativeColor: TlqColor read GetNegativeColor write SetNegativeColor;
    property ArrowUpColor;
    property ArrowDownColor;
    property FontDesc: string read GetFontDesc write SetFontDesc;
    property MaxValue: extended read FMaxValue write SetMaxValue;
    property MinValue: extended read FMinValue write SetMinValue;
    property Increment: extended read FIncrement write SetIncrement;
    property LargeIncrement: extended read FLargeIncrement write SetLargeIncrement;
    property Value: extended read FValue write SetValue;
    property Decimals: integer read GetDecimals write SetDecimals;
    property FixedDecimals: integer read GetFixedDecimals write SetFixedDecimals;
    property Hint;
    property TabOrder;
    property    OnChange;
    property    OnEnter;
    property    OnExit;
    property    OnKeyPress;
    property    OnMouseEnter;
    property    OnMouseExit;
    property    OnPaint;
    property    OnShowHint;
  end;


  TlqSpinEdit = class(TlqAbstractSpinEdit)
  private
    FEdit: TlqEditInteger;
    FLargeIncrement: integer;
    FMaxValue: integer;
    FMinValue: integer;
    FIncrement: integer;
    FTempIncrement: integer;  // value varies depending on increment speed
    FValue: integer;
    procedure EnableButtons;
  protected
    function IsMinLimitReached: Boolean; override;
    function IsMaxLimitReached: Boolean; override;
    function GetEditBackgroundColor: TlqColor;
    function GetTextColor: TlqColor;
    function GetNegativeColor: TlqColor;
    function GetFontDesc: string;
    procedure ResizeChildren; override;
    procedure SetEditBackgroundColor(const AValue: Tlqcolor);
    procedure SetTextColor(const AValue: Tlqcolor); override;
    procedure SetNegativeColor(const AValue: Tlqcolor);
    procedure SetFontDesc(const AValue: string);
    procedure SetMaxValue(const AValue: integer);
    procedure SetMinValue(const AValue: integer);
    procedure SetIncrement(const AValue: integer);
    procedure SetLargeIncrement(const AValue: integer);
    procedure SetValue(const AValue: integer);
    procedure SetHint(const AValue: TlqString); override;
    procedure ButtonUpClick(Sender: TObject);
    procedure ButtonDownClick(Sender: TObject);
    procedure ButtonUpMouseDown(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
    procedure ButtonUpMouseUp(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
    procedure ButtonDownMouseDown(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
    procedure ButtonDownMouseUp(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
    procedure EditKeyPress(Sender: TObject; var keycode: word; var shiftstate: TShiftState; var consumed: Boolean);
    procedure EditExit(Sender: TObject);
    procedure MouseEnter(Sender: TObject);
    procedure MouseMove(Sender: TObject; AShift: TShiftState; const AMousePos: TPoint);
    procedure MouseExit(Sender: TObject);
    procedure TimerStep(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property EditBackgroundColor: Tlqcolor read GetEditBackgroundColor write SetEditBackgroundColor default clBoxColor;
    property ButtonsBackgroundColor;
    property ButtonWidth;
    property TextColor: Tlqcolor read GetTextColor write SetTextColor;
    property NegativeColor: TlqColor read GetNegativeColor write SetNegativeColor;
    property ArrowUpColor;
    property ArrowDownColor;
    property FontDesc: string read GetFontDesc write SetFontDesc;
    property MaxValue: integer read FMaxValue write SetMaxValue default 100;
    property MinValue: integer read FMinValue write SetMinValue default 0;
    property Increment: integer read FIncrement write SetIncrement default 1;
    property LargeIncrement: integer read FLargeIncrement write SetLargeIncrement default 10;
    property Value: integer read FValue write SetValue default 0;
    property Hint;
    property TabOrder;
    property    OnChange;
    property    OnEnter;
    property    OnExit;
    property    OnKeyPress;
    property    OnMouseEnter;
    property    OnMouseExit;
    property    OnPaint;
    property    OnShowHint;
  end;


function CreateSpinEditFloat(AOwner: TComponent; x, y, w, h: TlqCoord;
         AMinValue: extended = 0; AMaxValue: extended = 100; AIncrement: extended = 1; ALargeIncrement: extended = 10.0;
         AFixedDecimals: integer = 1; AValue: extended = 0; ADecimals: integer = -1): TlqSpinEditFloat;
function CreateSpinEdit(AOwner: TComponent; x, y, w, h: TlqCoord; AMinValue: integer = 0;
         AMaxValue: integer = 100; AIncrement: integer = 1; ALargeIncrement: integer = 10;
         AValue: integer = 0): TlqSpinEdit;


implementation

uses
  lq_extgraphics,
  lq_stringutils,
  math;


function CreateSpinEditFloat(AOwner: TComponent; x, y, w, h: TlqCoord;
         AMinValue: extended = 0; AMaxValue: extended = 100; AIncrement: extended = 1; ALargeIncrement: extended = 10.0;
         AFixedDecimals: integer = 1; AValue: extended = 0; ADecimals: integer = -1): TlqSpinEditFloat;
var
  newh: TlqCoord;
begin
  Result       := TlqSpinEditFloat.Create(AOwner);
  if h < Result.FEdit.Font.Height + 6 then
    newh := Result.FEdit.Font.Height + 6
  else
    newh := h;
  Result.SetPosition(x, y, w, newh);

  if AMaxValue > AMinValue then
  begin
    Result.MinValue := AMinValue;
    Result.MaxValue := AMaxValue;
  end;
  Result.Increment := AIncrement;
  Result.LargeIncrement := ALargeIncrement;
  Result.FEdit.FixedDecimals := AFixedDecimals;
  Result.FEdit.Decimals := ADecimals;
  if (AValue <= Result.MaxValue) and (AValue >= Result.MinValue) then
    Result.Value := AValue;
end;

function CreateSpinEdit(AOwner: TComponent; x, y, w, h: TlqCoord; AMinValue: integer = 0;
         AMaxValue: integer = 100; AIncrement: integer = 1; ALargeIncrement: integer = 10;
         AValue: integer = 0): TlqSpinEdit;
var
  newh: TlqCoord;
begin
  Result       := TlqSpinEdit.Create(AOwner);
  if h < Result.FEdit.Font.Height + 6 then
    newh := Result.FEdit.Font.Height + 6
  else
    newh := h;
  Result.SetPosition(x, y, w, newh);

  if AMaxValue > AMinValue then
  begin
    Result.MinValue := AMinValue;
    Result.MaxValue := AMaxValue;
  end;
  Result.Increment := AIncrement;
  Result.LargeIncrement := ALargeIncrement;
  if (AValue <= Result.MaxValue) and (AValue >= Result.MinValue) then
    Result.Value := AValue;
end;


{ TlqAbstractSpinEdit }

procedure TlqAbstractSpinEdit.SetButtonWidth(const AValue: integer);
begin
  if FButtonWidth = AValue then
    Exit;
  FButtonWidth := AValue;
  { Apply some limits for sanity sake }
  if FButtonWidth < 5 then
    FButtonWidth := 5;

  ResizeChildren;
  RePaint;
end;

procedure TlqAbstractSpinEdit.DoOnChange;
begin
  if Assigned(FOnChange) then
    FOnChange(self);
end;

procedure TlqAbstractSpinEdit.ResizeChildren;
begin
  FButtonUp.SetPosition(Width - FButtonWidth, 0, FButtonWidth, Height div 2);
  FButtonDown.SetPosition(Width - FButtonWidth, Height div 2, FButtonWidth, Height div 2);
end;

procedure TlqAbstractSpinEdit.HandlePaint;
begin
  Canvas.Clear(BackgroundColor);
end;

procedure TlqAbstractSpinEdit.HandleResize(AWidth, AHeight: TlqCoord);
begin
  inherited HandleResize(AWidth, AHeight);
  if not (csLoading in ComponentState) then
    ResizeChildren;
end;

function TlqAbstractSpinEdit.GetButtonsBackgroundColor: TlqColor;
begin
  Result := FButtonUp.BackgroundColor;
end;

procedure TlqAbstractSpinEdit.SetButtonsBackgroundColor(const AValue: Tlqcolor);
begin
  if FButtonUp.BackgroundColor <> AValue then
  begin
    FButtonUp.BackgroundColor   := AValue;
    FButtonDown.BackgroundColor := AValue;
  end;
end;

procedure TlqAbstractSpinEdit.SetArrowUpColor(const AValue: Tlqcolor);
begin
  if FArrowUpColor <> AValue then
    FArrowUpColor := AValue;
end;

procedure TlqAbstractSpinEdit.SetArrowDownColor(const AValue: Tlqcolor);
begin
  if FArrowDownColor <> AValue then
    FArrowDownColor := AValue;
end;

procedure TlqAbstractSpinEdit.SetSpeedUp(const AValue: integer);
begin
  if FSpeedUpSteps <> AValue then
    FSpeedUpSteps := AValue;
end;

procedure TlqAbstractSpinEdit.DisableTimer;
begin
  FUp:= False;
  FDown:= False;
  if Assigned(FTimer) then
    FTimer.Enabled:= False;
end;

function GetButtonRect(AButton: TlqButton): TRect;
var
  r: TlqRect;
begin
  r := AButton.GetClientRect;

  InflateRect(r, -2, -2); // button borders
  if AButton.Down then
    OffsetRect(r, 1, 1);

  Result := lqRectToRect(r);
end;

procedure TlqAbstractSpinEdit.ButtonUpPaint(Sender: TObject);
var
  btn: TlqButton;
  r: TRect;
begin
  btn := TlqButton(Sender);
  if btn.Enabled then
    btn.Canvas.SetColor(FArrowUpColor)
  else
    btn.Canvas.SetColor(clShadow1);

  r := GetButtonRect(btn);
  PaintTriangle(btn.Canvas, r, degtorad(90.0));
//  lqStyle.DrawDirectionArrow(btn.Canvas, r.Top, r.Left, r.Width, r.Height, adUp);
end;

procedure TlqAbstractSpinEdit.ButtonDownPaint(Sender: TObject);
var
  btn: TlqButton;
  r: TRect;
begin
  btn := TlqButton(Sender);
  if btn.Enabled {and (not IsMinLimitReached)} then
    btn.Canvas.SetColor(FArrowDownColor)
  else
    btn.Canvas.SetColor(clShadow1);

  r := GetButtonRect(btn);
  PaintTriangle(btn.Canvas, r, degtorad(270.0));
//  lqStyle.DrawDirectionArrow(TlqButton(Sender).Canvas, 0, 0, TlqButton(Sender).Width - 3, TlqButton(Sender).Height, adDown);
end;

constructor TlqAbstractSpinEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FButtonWidth := 13; // width of spin buttons
  Shape := bsSpacer;

  FButtonUp := TlqButton.Create(Self);
  with FButtonUp do
  begin
    SetPosition(Width - FButtonWidth, 0, FButtonWidth, Height div 2);
    Text      := '';
    BackgroundColor := clButtonFace;
    Focusable := False;
    OnPaint   := @ButtonUpPaint;
  end;
  FArrowUpColor := clText1;
  FButtonDown := TlqButton.Create(Self);
  with FButtonDown do
  begin
    SetPosition(Width - FButtonWidth, Height div 2, FButtonWidth, Height div 2);
    Text      := '';
    BackgroundColor := clButtonFace;
    Focusable := False;
    OnPaint   := @ButtonDownPaint;
  end;
  FArrowDownColor := clText1;
  FTimer         := TlqTimer.Create(200);
  FTimer.Enabled := False;
  FSpeedUpSteps  := 10;
end;


{ TlqSpinEditFloat }

procedure TlqSpinEditFloat.EnableButtons;
begin
  FButtonUp.Enabled := True;
  FButtonDown.Enabled := True;
  if IsMaxLimitReached then
    FButtonUp.Enabled := False
  else
    if IsMinLimitReached then
      FButtonDown.Enabled := False;
end;

function TlqSpinEditFloat.IsMinLimitReached: Boolean;
begin
  Result := FValue = FMinValue;
end;

function TlqSpinEditFloat.IsMaxLimitReached: Boolean;
begin
  Result := FValue = FMaxValue;
end;

function TlqSpinEditFloat.GetEditBackgroundColor: TlqColor;
begin
  Result := FEdit.BackgroundColor;
end;

function TlqSpinEditFloat.GetTextColor: TlqColor;
begin
  Result := FEdit.TextColor;
end;

function TlqSpinEditFloat.GetNegativeColor: TlqColor;
begin
  Result := FEdit.NegativeColor;
end;

function TlqSpinEditFloat.GetFontDesc: string;
begin
  Result := FEdit.FontDesc;
end;

function TlqSpinEditFloat.GetDecimals: integer;
begin
  Result := FEdit.Decimals;
end;

function TlqSpinEditFloat.GetFixedDecimals: integer;
begin
  Result := FEdit.FixedDecimals;
end;

procedure TlqSpinEditFloat.ResizeChildren;
begin
  FEdit.SetPosition(0, 0, Width - FButtonWidth, Height);
  inherited ResizeChildren;
end;

procedure TlqSpinEditFloat.SetEditBackgroundColor(const AValue: Tlqcolor);
begin
  if FEdit.BackgroundColor <> AValue then
    FEdit.BackgroundColor := AValue;
end;

procedure TlqSpinEditFloat.SetTextColor(const AValue: Tlqcolor);
begin
  if FEdit.OldColor <> AValue then
    FEdit.OldColor := AValue;
end;

procedure TlqSpinEditFloat.SetNegativeColor(const AValue: Tlqcolor);
begin
  if FEdit.NegativeColor <> AValue then
    FEdit.NegativeColor := AValue;
end;

procedure TlqSpinEditFloat.SetFontDesc(const AValue: string);
begin
  if FEdit.FontDesc <> AValue then
  begin
    FEdit.FontDesc := AValue;
    if Height < FEdit.Height then
    begin
      Height           := FEdit.Height;
      FButtonUp.Height := Height div 2;
      FButtonDown.Height := Height div 2;
      FButtonDown.Top  := FButtonUp.Height + 1;
    end;
  end;
end;

procedure TlqSpinEditFloat.SetMaxValue(const AValue: extended);
begin
  if (FMaxValue <> AValue) and (AValue > FMinValue) then
  begin
    FMaxValue := AValue;
    if FValue > FMaxValue then
    begin
      FValue      := FMaxValue;
      FEdit.Value := FValue;
    end;
  end;
end;

procedure TlqSpinEditFloat.SetMinValue(const AValue: extended);
begin
  if (FMinValue <> AValue) and (AValue < FMaxValue) then
  begin
    FMinValue := AValue;
    if FValue < FMinValue then
    begin
      FValue      := FMinValue;
      FEdit.Value := FValue;
    end;
  end;
end;

procedure TlqSpinEditFloat.SetIncrement(const AValue: extended);
begin
  if FIncrement <> AValue then
  begin
    FIncrement := AValue;
    if FLargeIncrement < AValue then
      FLargeIncrement := AValue;
  end;
end;

procedure TlqSpinEditFloat.SetLargeIncrement(const AValue: extended);
begin
  if FLargeIncrement <> AValue then
    if FIncrement <= AValue then
      FLargeIncrement := AValue;
end;

procedure TlqSpinEditFloat.SetValue(const AValue: extended);
begin
  if (FValue <> AValue) and (AValue <= FMaxValue) and (AValue >= FMinValue) then
  begin
    FValue      := AValue;
    FEdit.Value := FValue;
    EnableButtons;
  end;
end;

procedure TlqSpinEditFloat.SetDecimals(const AValue: integer);
begin
  if AValue < 0 then
    Exit; // =>
  if FEdit.Decimals <> AValue then
    FEdit.Decimals := AValue;
end;

procedure TlqSpinEditFloat.SetFixedDecimals(const AValue: integer);
begin
  if AValue < 0 then
    Exit; // =>
  if FEdit.FixedDecimals <> AValue then
    FEdit.FixedDecimals := AValue;
end;

procedure TlqSpinEditFloat.SetHint(const AValue: TlqString);
begin
  inherited SetHint(AValue);
  // let child component use the same hint
  FEdit.Hint := AValue;
  FButtonUp.Hint := AValue;
  FButtonDown.Hint := AValue;
end;

procedure TlqSpinEditFloat.ButtonUpClick(Sender: TObject);
begin
  if FValue + FIncrement <= FMaxValue then
  begin
    FValue := FValue + FIncrement;
    FEdit.Value := FValue;
  end
  else if not IsMaxLimitReached then
  begin
    FValue := FMaxValue;
    FEdit.Value := FValue;
  end;
  DoOnChange;
  EnableButtons;
end;

procedure TlqSpinEditFloat.ButtonDownClick(Sender: TObject);
begin
  if FValue - FIncrement >= FMinValue then
  begin
    FValue := FValue - FIncrement;
    FEdit.Value := FValue;
  end
  else if not IsMinLimitReached then
  begin
    FValue := FMinValue;
    FEdit.Value := FValue;
  end;
  DoOnChange;
  EnableButtons;
end;

procedure TlqSpinEditFloat.ButtonUpMouseDown(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
begin
  FUp := True;
  FTimer.Enabled := True;
  FSteps := 0;
  FTempIncrement := Increment;
end;

procedure TlqSpinEditFloat.ButtonUpMouseUp(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
begin
  DisableTimer;
end;

procedure TlqSpinEditFloat.ButtonDownMouseDown(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
begin
  FDown          := True;
  FTimer.Enabled := True;
  FSteps := 0;
  FTempIncrement := Increment;
end;

procedure TlqSpinEditFloat.ButtonDownMouseUp(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
begin
  DisableTimer;
end;

procedure TlqSpinEditFloat.EditKeyPress(Sender: TObject; var keycode: word; var shiftstate: TShiftState; var consumed: Boolean);
begin
  if (KeyCode = KeyReturn) or (KeyCode = KeyPEnter) then
    if FEdit.Text = '' then
    begin
      FValue      := 0.0;
      FEdit.Value := FValue;
    end
    else if (StrToFloat(FEdit.Text) <= FMaxValue) and (StrToFloat(FEdit.Text) >= FMinValue) then
      FValue      := StrToFloat(FEdit.Text)
    else
      FEdit.Value := FValue;

  if KeyCode = KeyUp then
    if FEdit.Value + Increment <= FMaxValue then
    begin
      FValue      := FValue + FIncrement;
      FEdit.Value := FValue;
    end
    else if not IsMaxLimitReached then
    begin
      FValue      := FMaxValue;
      FEdit.Value := FValue;
    end;

  if KeyCode = KeyDown then
    if FEdit.Value - Increment >= FMinValue then
    begin
      FValue      := FValue - FIncrement;
      FEdit.Value := FValue;
    end
    else if not IsMinLimitReached then
    begin
      FValue      := FMinValue;
      FEdit.Value := FValue;
    end;

  if KeyCode = KeyPageUp then
  begin
    FValue      := FMaxValue;
    FEdit.Value := FValue;
  end;

  if KeyCode = KeyPageDown then
  begin
    FValue      := FMinValue;
    FEdit.Value := FValue;
  end;

  EnableButtons;
end;

procedure TlqSpinEditFloat.EditExit(Sender: TObject);
begin
  if FEdit.Text = '' then
  begin
    FValue      := 0.0;
    FEdit.Value := FValue;
  end
  else if (StrToFloat(FEdit.Text) <= FMaxValue) and (StrToFloat(FEdit.Text) >= FMinValue) then
    FValue      := StrToFloat(FEdit.Text)
  else
    FEdit.Value := FValue;
  EnableButtons;
end;

procedure TlqSpinEditFloat.MouseEnter(Sender: TObject);
var
  msgp: TlqMessageParams;
  b: boolean;
begin
  fillchar(msgp, sizeof(msgp), 0);
  if Sender is TlqEditFloat then
    with Sender as TlqEditFloat do
      if Assigned(Parent) then
        b := Enabled and lqApplication.ShowHint and (FShowHint or (FParentShowHint and Parent.Parent.ShowHint)) and (FHint <> '')
      else
        b := Enabled and lqApplication.ShowHint and FShowHint and (FHint <> '');
  if Sender is TlqButton then
    with Sender as TlqButton do
      if Assigned(Parent) then
        b := Enabled and lqApplication.ShowHint and (FShowHint or (FParentShowHint and Parent.Parent.ShowHint)) and (FHint <> '')
      else
        b := Enabled and lqApplication.ShowHint and FShowHint and (FHint <> '');

  msgp.user.Param1 := Ord(b);
  lqPostMessage(Sender, lqApplication, LQM_HINTTIMER, msgp);
end;

procedure TlqSpinEditFloat.MouseMove(Sender: TObject; AShift: TShiftState; const AMousePos: TPoint);
var
  msgp: TlqMessageParams;
begin
  fillchar(msgp, sizeof(msgp), 0);
  msgp.user.Param1 := 2;
  msgp.user.Param2 := AMousePos.x+10;
  msgp.user.Param3 := AMousePos.y+2;

  { Only send message if really needed. }
  if Sender is TlqEditFloat then
    with Sender as TlqEditFloat do
      if Assigned(Parent) then
      begin
        if lqApplication.ShowHint and (FShowHint or (FParentShowHint and Parent.Parent.ShowHint)) and (FHint <> '') then
          lqPostMessage(Sender, lqApplication, LQM_HINTTIMER, msgp);
      end
      else
        if lqApplication.ShowHint and FShowHint and (FHint <> '') then
          lqPostMessage(Sender, lqApplication, LQM_HINTTIMER, msgp);
  if Sender is TlqButton then
    with Sender as TlqButton do
      if Assigned(Parent) then
      begin
        if lqApplication.ShowHint and (FShowHint or (FParentShowHint and Parent.Parent.ShowHint)) and (FHint <> '') then
          lqPostMessage(Sender, lqApplication, LQM_HINTTIMER, msgp);
      end
      else
        if lqApplication.ShowHint and FShowHint and (FHint <> '') then
          lqPostMessage(Sender, lqApplication, LQM_HINTTIMER, msgp);
end;

procedure TlqSpinEditFloat.MouseExit(Sender: TObject);
begin
  if Sender is TlqEditFloat then
    with Sender as TlqEditFloat do
      if FShowHint then
        lqApplication.HideHint;
  if Sender is TlqButton then
    with Sender as TlqButton do
      if FShowHint then
        lqApplication.HideHint;
end;

procedure TlqSpinEditFloat.TimerStep(Sender: TObject);
begin
  if FUp then
  begin
    if FValue + FTempIncrement <= FMaxValue then
    begin
      FValue       := FValue + FTempIncrement;
      FEdit.Value := FValue;
      if FSteps <= FSpeedUpSteps then
        Inc(FSteps);
      if FSteps > FSpeedUpSteps then
        FTempIncrement := LargeIncrement;
      DoOnChange;
      if FValue= FMaxValue then
        DisableTimer;
    end
    else if not IsMaxLimitReached then
    begin
      FValue := FMaxValue;
      FEdit.Value := FValue;
      DoOnChange;
      DisableTimer;
    end;
  end
  else if FDown then
  begin
    if FValue - FTempIncrement >= FMinValue then
    begin
      FValue      := FValue - FTempIncrement;
      FEdit.Value := FValue;
      if FSteps <= FSpeedUpSteps then
        Inc(FSteps);
      if FSteps > FSpeedUpSteps then
        FTempIncrement := LargeIncrement;
      DoOnChange;
      if FValue= FMinValue then
        DisableTimer;
    end
    else if not IsMinLimitReached then
    begin
      FValue := FMinValue;
      FEdit.Value := FValue;
      DoOnChange;
      DisableTimer;
    end;
  end;
end;

constructor TlqSpinEditFloat.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FEdit := CreateEditFloat(Self, 0, 0, Width - FButtonWidth, Height, False, 2);

  FMaxValue       := 100.0;
  FMinValue       := 0.0;
  FIncrement      := 1.0;
  FLargeIncrement := 10.0;
  FValue          := 0.0;
  FUp             := False;
  FDown           := False;

  FEdit.Decimals := -1;
  FEdit.FixedDecimals := 1;
  FEdit.Value    := FValue;

  FButtonUp.OnClick := @ButtonUpClick;
  FButtonDown.OnClick := @ButtonDownClick;
  FButtonUp.OnMouseDown := @ButtonUpMouseDown;
  FButtonUp.OnMouseUp := @ButtonUpMouseUp;
  FButtonDown.OnMouseDown := @ButtonDownMouseDown;
  FButtonDown.OnMouseUp := @ButtonDownMouseUp;
  FEdit.OnKeyPress  := @EditKeyPress;
  FEdit.OnExit      := @EditExit;
  FEdit.OnMouseEnter:= @MouseEnter;
  FButtonUp.OnMouseEnter:= @MouseEnter;
  FButtonDown.OnMouseEnter:= @MouseEnter;
  FEdit.OnMouseMove:= @MouseMove;
  FButtonUp.OnMouseMove:= @MouseMove;
  FButtonDown.OnMouseMove:= @MouseMove;
  FEdit.OnMouseExit:= @MouseExit;
  FButtonUp.OnMouseExit:= @MouseExit;
  FButtonDown.OnMouseExit:= @MouseExit;

  FTimer.OnTimer := @TimerStep;
  EnableButtons;
end;


{ TlqSpinEdit }

procedure TlqSpinEdit.EnableButtons;
begin
  FButtonUp.Enabled := True;
  FButtonDown.Enabled := True;
  if IsMaxLimitReached then
    FButtonUp.Enabled := False
  else
    if IsMinLimitReached then
      FButtonDown.Enabled := False;
end;

function TlqSpinEdit.IsMinLimitReached: Boolean;
begin
  Result := FValue = FMinValue;
end;

function TlqSpinEdit.IsMaxLimitReached: Boolean;
begin
  Result:= FValue = FMaxValue;
end;

function TlqSpinEdit.GetEditBackgroundColor: TlqColor;
begin
  Result := FEdit.BackgroundColor;
end;

function TlqSpinEdit.GetTextColor: TlqColor;
begin
  Result := FEdit.TextColor;
end;

function TlqSpinEdit.GetNegativeColor: TlqColor;
begin
  Result := FEdit.NegativeColor;
end;

function TlqSpinEdit.GetFontDesc: string;
begin
  Result := FEdit.FontDesc;
end;

procedure TlqSpinEdit.ResizeChildren;
begin
  FEdit.SetPosition(0, 0, Width - FButtonWidth, Height);
  inherited ResizeChildren;
end;

procedure TlqSpinEdit.SetEditBackgroundColor(const AValue: Tlqcolor);
begin
  if FEdit.BackgroundColor <> AValue then
    FEdit.BackgroundColor := AValue;
end;

procedure TlqSpinEdit.SetTextColor(const AValue: Tlqcolor);
begin
  if FEdit.OldColor <> AValue then
    FEdit.OldColor := AValue;
end;

procedure TlqSpinEdit.SetNegativeColor(const AValue: Tlqcolor);
begin
  if FEdit.NegativeColor <> AValue then
    FEdit.NegativeColor := AValue;
end;

procedure TlqSpinEdit.SetFontDesc(const AValue: string);
begin
  if FEdit.FontDesc <> AValue then
  begin
    FEdit.FontDesc := AValue;
    if Height < FEdit.Height then
    begin
      Height           := FEdit.Height;
      FButtonUp.Height := Height div 2;
      FButtonDown.Height := Height div 2;
      FButtonDown.Top  := FButtonUp.Height + 1;
    end;
  end;
end;

procedure TlqSpinEdit.SetMaxValue(const AValue: integer);
begin
  if (FMaxValue <> AValue) and (AValue > FMinValue) then
  begin
    FMaxValue := AValue;
    if FValue > FMaxValue then
    begin
      FValue      := FMaxValue;
      FEdit.Value := FValue;
    end;
  end;
end;

procedure TlqSpinEdit.SetMinValue(const AValue: integer);
begin
  if (FMinValue <> AValue) and (AValue < FMaxValue) then
  begin
    FMinValue := AValue;
    if FValue < FMinValue then
    begin
      FValue      := FMinValue;
      FEdit.Value := FValue;
    end;
  end;
end;

procedure TlqSpinEdit.SetIncrement(const AValue: integer);
begin
  if FIncrement <> AValue then
  begin
    FIncrement := AValue;
    if FLargeIncrement < AValue then
      FLargeIncrement := AValue;
  end;
end;

procedure TlqSpinEdit.SetLargeIncrement(const AValue: integer);
begin
  if FLargeIncrement <> AValue then
    if FIncrement <= AValue then
      FLargeIncrement := AValue;
end;

procedure TlqSpinEdit.SetValue(const AValue: integer);
begin
  if (FValue <> AValue) and (AValue <= FMaxValue) and (AValue >= FMinValue) then
  begin
    FValue      := AValue;
    FEdit.Value := FValue;
    EnableButtons;
  end;
end;

procedure TlqSpinEdit.SetHint(const AValue: TlqString);
begin
  inherited SetHint(AValue);
  // let child component use the same hint
  FEdit.Hint := AValue;
  FButtonUp.Hint := AValue;
  FButtonDown.Hint := AValue;
end;

procedure TlqSpinEdit.ButtonUpClick(Sender: TObject);
begin
  if FValue + FIncrement <= FMaxValue then
  begin
    Value := FValue + FIncrement;
    FEdit.Value:= FValue;
  end
  else if not IsMaxLimitReached then
  begin
    Value := FMaxValue;
    FEdit.Value:= FValue;
  end;
  DoOnChange;
  EnableButtons;
end;

procedure TlqSpinEdit.ButtonDownClick(Sender: TObject);
begin
  if FValue - FIncrement >= FMinValue then
  begin
    Value := FValue - FIncrement;
    FEdit.Value:= FValue;
  end
  else if not IsMinLimitReached then
  begin
    Value := FMinValue;
    FEdit.Value:= FValue;
  end;
  DoOnChange;
  EnableButtons;
end;

procedure TlqSpinEdit.ButtonUpMouseDown(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
begin
  FUp := True;
  FTimer.Enabled := True;
  FSteps := 0;
  FTempIncrement := Increment;
end;

procedure TlqSpinEdit.ButtonUpMouseUp(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
begin
  DisableTimer;
end;

procedure TlqSpinEdit.ButtonDownMouseDown(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
begin
  FDown          := True;
  FTimer.Enabled := True;
  FSteps := 0;
  FTempIncrement := Increment;
end;

procedure TlqSpinEdit.ButtonDownMouseUp(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
begin
  DisableTimer;
end;

procedure TlqSpinEdit.EditKeyPress(Sender: TObject; var keycode: word; var shiftstate: TShiftState; var consumed: Boolean);
begin
  if (KeyCode = KeyReturn) or (KeyCode = KeyPEnter) then
    if FEdit.Text = '' then
    begin
      FValue      := 0;
      FEdit.Value := FValue;
      DoOnChange;
    end
    else if (StrToInt(FEdit.Text) <= FMaxValue) and (StrToInt(FEdit.Text) >= FMinValue) then
    begin
      FValue      := FEdit.Value;
      DoOnChange;
    end
    else
      FEdit.Value := FValue;

  if KeyCode = KeyUp then
    if FEdit.Value + Increment <= FMaxValue then
    begin
      Inc(FValue, FIncrement);
      FEdit.Value := FValue;
      DoOnChange;
    end
    else if not IsMaxLimitReached then
    begin
      FValue      := FMaxValue;
      FEdit.Value := FValue;
      DoOnChange;
    end;

  if KeyCode = KeyDown then
    if FEdit.Value - Increment >= FMinValue then
    begin
      Dec(FValue, FIncrement);
      FEdit.Value := FValue;
      DoOnChange;
    end
    else if not IsMinLimitReached then
    begin
      FValue      := FMinValue;
      FEdit.Value := FValue;
      DoOnChange;
    end;

  if KeyCode = KeyPageUp then
  begin
    FValue      := FMaxValue;
    FEdit.Value := FValue;
    DoOnChange;
  end;

  if KeyCode = KeyPageDown then
  begin
    FValue      := FMinValue;
    FEdit.Value := FValue;
    DoOnChange;
  end;

  EnableButtons;
end;

procedure TlqSpinEdit.EditExit(Sender: TObject);
begin
  if FEdit.Text = '' then
  begin
    FValue      := 0;
    FEdit.Value := FValue;
  end
  else if (StrToInt(FEdit.Text) <= FMaxValue) and (StrToInt(FEdit.Text) >= FMinValue) then
    FValue      := FEdit.Value
  else
    FEdit.Value := FValue;
  EnableButtons;
end;

procedure TlqSpinEdit.MouseEnter(Sender: TObject);
var
  msgp: TlqMessageParams;
  b: boolean;
begin
  fillchar(msgp, sizeof(msgp), 0);
  if Sender is TlqEditInteger then
    with Sender as TlqEditInteger do
      if Assigned(Parent) then
        b := Enabled and lqApplication.ShowHint and (FShowHint or (FParentShowHint and Parent.Parent.ShowHint)) and (FHint <> '')
      else
        b := Enabled and lqApplication.ShowHint and FShowHint and (FHint <> '');
  if Sender is TlqButton then
    with Sender as TlqButton do
      if Assigned(Parent) then
        b := Enabled and lqApplication.ShowHint and (FShowHint or (FParentShowHint and Parent.Parent.ShowHint)) and (FHint <> '')
      else
        b := Enabled and lqApplication.ShowHint and FShowHint and (FHint <> '');

  msgp.user.Param1 := Ord(b);
  lqPostMessage(Sender, lqApplication, LQM_HINTTIMER, msgp);
end;

procedure TlqSpinEdit.MouseMove(Sender: TObject; AShift: TShiftState; const AMousePos: TPoint);
var
  msgp: TlqMessageParams;
begin
  fillchar(msgp, sizeof(msgp), 0);
  msgp.user.Param1 := 2;
  msgp.user.Param2 := AMousePos.x+10;
  msgp.user.Param3 := AMousePos.y+2;

  { Only send message if really needed. }
  if Sender is TlqEditInteger then
    with Sender as TlqEditInteger do
      if Assigned(Parent) then
      begin
        if lqApplication.ShowHint and (FShowHint or (FParentShowHint and Parent.Parent.ShowHint)) and (FHint <> '') then
          lqPostMessage(Sender, lqApplication, LQM_HINTTIMER, msgp);
      end
      else
        if lqApplication.ShowHint and FShowHint and (FHint <> '') then
          lqPostMessage(Sender, lqApplication, LQM_HINTTIMER, msgp);
  if Sender is TlqButton then
    with Sender as TlqButton do
      if Assigned(Parent) then
      begin
        if lqApplication.ShowHint and (FShowHint or (FParentShowHint and Parent.Parent.ShowHint)) and (FHint <> '') then
          lqPostMessage(Sender, lqApplication, LQM_HINTTIMER, msgp);
      end
      else
        if lqApplication.ShowHint and FShowHint and (FHint <> '') then
          lqPostMessage(Sender, lqApplication, LQM_HINTTIMER, msgp);
end;

procedure TlqSpinEdit.MouseExit(Sender: TObject);
begin
  if Sender is TlqEditInteger then
    with Sender as TlqEditInteger do
      if FShowHint then
        lqApplication.HideHint;
  if Sender is TlqButton then
    with Sender as TlqButton do
      if FShowHint then
        lqApplication.HideHint;
end;

procedure TlqSpinEdit.TimerStep(Sender: TObject);
begin
  if FUp then
  begin
    if FValue + FTempIncrement <= FMaxValue then
    begin
      FValue       := FValue + FTempIncrement;
      FEdit.Value := FValue;
      if FSteps <= FSpeedUpSteps then
        Inc(FSteps);
      if FSteps > FSpeedUpSteps then
        FTempIncrement := LargeIncrement;
      DoOnChange;
      if FValue= FMaxValue then
        DisableTimer;
    end
    else if not IsMaxLimitreached then
    begin
      FValue := FMaxValue;
      FEdit.Value := FValue;
      DoOnChange;
      DisableTimer;
    end;
  end
  else if FDown then
  begin
    if FValue - FTempIncrement >= FMinValue then
    begin
      FValue       := FValue - FTempIncrement;
      FEdit.Value := FValue;
      if FSteps <= FSpeedUpSteps then
        Inc(FSteps);
      if FSteps > FSpeedUpSteps then
        FTempIncrement := LargeIncrement;
      DoOnChange;
      if FValue= FMinValue then
        DisableTimer;
    end
    else
    begin
      FValue := FMinValue;
      FEdit.Value := FValue;
      DoOnChange;
      DisableTimer;
    end;
  end;
end;

constructor TlqSpinEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FEdit := CreateEditInteger(Self, 0, 0, Width - FButtonWidth, Height);

  FMaxValue       := 100;
  FMinValue       := 0;
  FIncrement      := 1;
  FLargeIncrement := 10;
  FValue          := 0;
  FUp             := False;
  FDown           := False;

  FEdit.Value := FValue;

  FButtonUp.OnClick := @ButtonUpClick;
  FButtonDown.OnClick := @ButtonDownClick;
  FButtonUp.OnMouseDown := @ButtonUpMouseDown;
  FButtonUp.OnMouseUp := @ButtonUpMouseUp;
  FButtonDown.OnMouseDown := @ButtonDownMouseDown;
  FButtonDown.OnMouseUp := @ButtonDownMouseUp;
  FEdit.OnKeyPress  := @EditKeyPress;
  FEdit.OnExit      := @EditExit;
  FEdit.OnMouseEnter:= @MouseEnter;
  FButtonUp.OnMouseEnter:= @MouseEnter;
  FButtonDown.OnMouseEnter:= @MouseEnter;
  FEdit.OnMouseMove:= @MouseMove;
  FButtonUp.OnMouseMove:= @MouseMove;
  FButtonDown.OnMouseMove:= @MouseMove;
  FEdit.OnMouseExit:= @MouseExit;
  FButtonUp.OnMouseExit:= @MouseExit;
  FButtonDown.OnMouseExit:= @MouseExit;

  FTimer.OnTimer := @TimerStep;
  EnableButtons;
end;

end.

