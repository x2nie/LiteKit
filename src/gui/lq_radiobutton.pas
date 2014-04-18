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
      Defines a Radio Button control.
}

unit lq_radiobutton;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  lq_base,
  lq_main,
  lq_widget;

type

  TlqRadioButton = class(TlqWidget)
  private
    FAutoSize: boolean;
    FChecked: boolean;
    FFont: TlqFont;
    FGroupIndex: integer;
    FOnChange: TNotifyEvent;
    FText: string;
    FBoxLayout: TBoxLayout;
    FBoxSize: integer;
    FIsPressed: boolean;
    FImgTextSpacing: integer;
    function    GetBoxLayout: TBoxLayout;
    function    GetFontDesc: string;
    procedure   SetBoxLayout(const AValue: TBoxLayout);
    procedure   SetAutoSize(const AValue: boolean);
    procedure   SetChecked(const AValue: boolean);
    procedure   SetFontDesc(const AValue: string);
    procedure   SetText(const AValue: string);
    procedure   DoAdjustWidth;
    function    FindNeighbour(direction: TFocusSearchDirection): TlqRadioButton;
  protected
    procedure   HandlePaint; override;
    procedure   HandleLMouseDown(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleLMouseUp(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); override;
    procedure   HandleKeyRelease(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    property    Font: TlqFont read FFont;
  published
    property    Align;
    property    AutoSize: boolean read FAutoSize write SetAutoSize default False;
    property    BackgroundColor;
    property    Checked: boolean read FChecked write SetChecked default False;
    property    Enabled;
    property    FontDesc: string read GetFontDesc write SetFontDesc;
    property    Hint;
    property    BoxLayout: TBoxLayout read GetBoxLayout write SetBoxLayout default tbLeftBox;
    property    GroupIndex: integer read FGroupIndex write FGroupIndex;
    property    ParentShowHint;
    property    ShowHint;
    property    TabOrder;
    property    Text: string read FText write SetText;
    property    TextColor;
    property    OnChange: TNotifyEvent read FOnChange write FOnChange;
    property    OnShowHint;
  end;
  
  
function CreateRadioButton(AOwner: TComponent; x, y: TlqCoord; AText: string): TlqRadioButton;


implementation


function CreateRadioButton(AOwner: TComponent; x, y: TlqCoord; AText: string): TlqRadioButton;
begin
  Result := TlqRadioButton.Create(AOwner);
  Result.Top    := y;
  Result.Left   := x;
  Result.Text   := AText;
  Result.Width  := Result.Font.TextWidth(Result.Text) + 24;
end;

{ TlqRadioButton }

function TlqRadioButton.GetBoxLayout: TBoxLayout;
begin
  Result := FBoxLayout;
end;

function TlqRadioButton.GetFontDesc: string;
begin
  Result := FFont.FontDesc;
end;

procedure TlqRadioButton.SetBoxLayout(const AValue: TBoxLayout);
begin
  if FBoxLayout = AValue then
    Exit; //==>
  FBoxLayout := AValue;
  RePaint;
end;

procedure TlqRadioButton.SetAutoSize(const AValue: boolean);
begin
  if FAutoSize = AValue then
    Exit; //==>
  FAutoSize := AValue;
  if FAutoSize then
    DoAdjustWidth;
  Repaint;
end;

procedure TlqRadioButton.SetChecked(const AValue: boolean);
var
  i: integer;
  wg: TlqWidget;
begin
  if FChecked = AValue then
    Exit; //==>
  FChecked := AValue;
  
  // Clear other radio buttons in the same group
  if FChecked and (Parent <> nil) then
  begin
    for i := 0 to Parent.ComponentCount-1 do
      if (Parent.Components[i] is TlqWidget) then
      begin
        wg := TlqWidget(Parent.Components[i]);
        if (wg <> nil) and (wg <> self) and (wg is TlqRadioButton) and
            (TlqRadioButton(wg).GroupIndex = GroupIndex) then
        begin
          TlqRadioButton(wg).Checked := False;
        end;
      end;  { if }
  end;  { if }

  RePaint;

  if Assigned(FOnChange) then
      FOnChange(self);
end;

procedure TlqRadioButton.SetFontDesc(const AValue: string);
begin
  FFont.Free;
  FFont := lqGetFont(AValue);
  RePaint;
end;

procedure TlqRadioButton.SetText(const AValue: string);
begin
  if FText = AValue then
    Exit; //==>
  FText := AValue;
  if AutoSize then
    DoAdjustWidth;
  RePaint;
end;

procedure TlqRadioButton.DoAdjustWidth;
begin
  if AutoSize then
  begin
    Width := Font.TextWidth(FText) + 24; // 24 is extra padding for image
    UpdateWindowPosition;
  end;
end;

procedure TlqRadioButton.HandlePaint;
var
  r: TlqRect;
  img: TlqImage;
  ix: integer;
  LFlags: TlqTextFlags;
begin
  inherited HandlePaint;
  Canvas.ClearClipRect;
  Canvas.SetColor(FBackgroundColor);
  Canvas.FillRectangle(0, 0, Width, Height);
  Canvas.SetFont(Font);
  Canvas.SetLineStyle(1, lsSolid);

  if FBoxLayout = tbLeftBox then
    r.SetRect(2, ((Height - FBoxSize) div 2), FBoxSize, FBoxSize)
  else
    r.SetRect(Width - FBoxSize - 2, ((Height - FBoxSize) div 2), FBoxSize, FBoxSize);
  if r.top < 0 then
    r.top := 0;

  // calculate which image to paint.
  if Enabled then
  begin
    ix := Ord(FChecked);
    if FIsPressed then
      Inc(ix, 2);
  end
  else
    ix := (2 + (Ord(FChecked) * 2)) - Ord(FChecked);

  // paint the radio button
  img := lqImages.GetImage('sys.radiobuttons');    // Do NOT localize
  Canvas.DrawImagePart(r.Left, r.Top, img, ix*FBoxSize, 0, FBoxSize, FBoxSize);

  r := GetClientRect;
  { max focus rectangle and text boundry }
  InflateRect(r, -1, -1);
  { exclude the radiobutton image and spacing from rectangle }
  if FBoxLayout = tbLeftBox then
  begin
    r.Left  := r.Left + FBoxSize+1 + FImgTextSpacing;   { +1 or -1 is so checkbox and radiobutton text align }
    r.Width := r.Width - FBoxSize-1 - FImgTextSpacing;
  end
  else
    r.Width := r.Width - FBoxSize-1 - FImgTextSpacing;

  Canvas.SetTextColor(FTextColor);
  Canvas.SetClipRect(r);

  if Enabled then
    LFlags := [txtLeft, txtVCenter]
  else
    LFlags := [txtLeft, txtVCenter, txtDisabled];
  Canvas.DrawText(r, FText, LFlags);   { internally this still calls lqStyle.DrawString(), so theming will be applied }

  if FFocused then
  begin
    Canvas.ClearClipRect;
    { adjust focusrect-to-text margin }
    if FBoxLayout = tbLeftBox then
    begin
      r.Left := r.Left - 2;
      r.Width := r.Width + 2;
    end
    else
    begin
      r.Width := r.Width + 2;
    end;
    { undo the 2px focusrect-to-text margin, so we simply use the clip rect }
    lqStyle.DrawFocusRect(Canvas, r);
  end;
end;

procedure TlqRadioButton.HandleLMouseDown(x, y: integer;
  shiftstate: TShiftState);
begin
  inherited HandleLMouseDown(x, y, shiftstate);
  FIsPressed := True;
  Repaint;
end;

procedure TlqRadioButton.HandleLMouseUp(x, y: integer; shiftstate: TShiftState);
begin
  inherited HandleLMouseUp(x, y, shiftstate);
  FIsPressed := False;
  if not FChecked then
    Checked := true
  else
    RePaint;
end;

procedure TlqRadioButton.HandleKeyPress(var keycode: word;
  var shiftstate: TShiftState; var consumed: boolean);
var
  nbr: TlqRadioButton;
begin
  case keycode of
    keyUp, keyLeft:
      begin
        consumed := True;
        nbr := FindNeighbour(fsdPrev);
        if nbr = Self then
          nbr := FindNeighbour(fsdLast);
        nbr.SetFocus;
        nbr.Checked := True;
      end;
    keyDown, keyRight:
      begin
        consumed := True;
        nbr := FindNeighbour(fsdNext);
        if nbr = Self then
          nbr := FindNeighbour(fsdFirst);
        nbr.SetFocus;
        nbr.Checked := True;
      end;
  end;

  if consumed then
    Exit; //==>

  inherited HandleKeyPress(keycode, shiftstate, consumed);
end;

procedure TlqRadioButton.HandleKeyRelease(var keycode: word;
  var shiftstate: TShiftState; var consumed: boolean);
begin
  if (keycode = keySpace) then
  begin
    consumed := True;
    Checked := true;
  end;

  if consumed then
    Exit; //==>

  inherited HandleKeyRelease(keycode, shiftstate, consumed);
end;

function TlqRadioButton.FindNeighbour(direction: TFocusSearchDirection): TlqRadioButton;
var
  i: integer;
  wg: TlqWidget;
  bestdtab: integer;
  FoundIt: boolean;
begin
  Result := Self;
  if (Parent <> nil) then
  begin
    FoundIt := False;
    if direction in [fsdLast, fsdPrev] then
      bestdtab := Low(integer)
    else
      bestdtab := High(integer);
    
    for i := 0 to Parent.ComponentCount-1 do
    begin
      if (Parent.Components[i] is TlqWidget) then
      begin
        wg := TlqWidget(Parent.Components[i]);
        if (wg <> nil) and (wg is TlqRadioButton) and
           wg.Visible and wg.Enabled and wg.Focusable and
           (TlqRadioButton(wg).GroupIndex = GroupIndex) then
        begin
          case direction of
            fsdFirst:
              if (wg.TabOrder < bestdtab) then
              begin
                Result   := TlqRadioButton(wg);
                bestdtab := wg.TabOrder;
              end;

            fsdLast:
              if (wg.TabOrder >= bestdtab) then
              begin
                Result   := TlqRadioButton(wg);
                bestdtab := wg.TabOrder;
              end;

            fsdNext:
              if wg = Self then
                FoundIt := True
              else
              begin
                if ((wg.TabOrder > Self.TabOrder) and (wg.TabOrder < bestdtab)) or
                   ((wg.TabOrder = Self.TabOrder) and FoundIt) then
                begin
                  Result   := TlqRadioButton(wg);
                  bestdtab := wg.TabOrder;
                end;
              end;

            fsdPrev:
              if wg = Self then
                FoundIt := True
              else
              begin
                if ((wg.TabOrder < Self.TabOrder) and (wg.TabOrder >= bestdtab)) or
                   ((wg.TabOrder = Self.TabOrder) and not FoundIt) then
                begin
                  Result   := TlqRadioButton(wg);
                  bestdtab := wg.TabOrder;
                end;
              end;
          end; { case }
        end; { if }
      end; { if is TlqWidget }
    end;  { for ComponentCount }
  end;  { if }
end;

constructor TlqRadioButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FText       := 'RadioButton';
  FFont       := lqGetFont('#Label1');
  FHeight     := FFont.Height + 4;
  FWidth      := 120;
  FTextColor  := Parent.TextColor;
  FBackgroundColor := Parent.BackgroundColor;
  FFocusable  := True;
  FBoxSize    := 12;
  FChecked    := False;
  FGroupIndex := 0;
  FIsPressed  := False;
  FAutoSize   := False;
  FOnChange   := nil;
  FBoxLayout  := tbLeftBox;
  FImgTextSpacing := 6;
end;

destructor TlqRadioButton.Destroy;
begin
  FFont.Free;
  inherited Destroy;
end;

end.

