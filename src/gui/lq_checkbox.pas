{
    LiteKit  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2011 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing LiteKit.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Defines a CheckBox control. Also known as a Check Button control.
}

unit lq_checkbox;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  lq_base,
  lq_main,
  lq_widget;
  
type

  TlqBaseCheckBox = class(TlqWidget)
  private
    FChecked: boolean;
    FOnChange: TNotifyEvent;
    FReadOnly: Boolean;
    FText: string;
    FFont: TlqFont;
    FBoxLayout: TBoxLayout;
    FBoxSize: integer;
    FImgTextSpacing: integer;
    FIsPressed: boolean;
    function    GetBoxLayout: TBoxLayout;
    function    GetFontDesc: string;
    procedure   SetBoxLayout(const AValue: TBoxLayout);
    procedure   SetChecked(const AValue: boolean);
    procedure   SetFontDesc(const AValue: string);
    procedure   SetReadOnly(const AValue: Boolean);
    procedure   SetText(const AValue: string);
    procedure   DoOnChange;
  protected
    procedure   HandlePaint; override;
    procedure   HandleLMouseDown(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleLMouseUp(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleKeyRelease(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); override;
    property    Checked: boolean read FChecked write SetChecked default False;
    property    FontDesc: string read GetFontDesc write SetFontDesc;
    property    BoxLayout: TBoxLayout read GetBoxLayout write SetBoxLayout default tbLeftBox;
    property    ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
    property    Text: string read FText write SetText;
    property    OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    property    Font: TlqFont read FFont;
  end;


  TlqCheckBox = class(TlqBaseCheckBox)
  published
    property    Align;
    property    BackgroundColor;
    property    BoxLayout;
    property    Checked;
    property    Enabled;
    property    FontDesc;
    property    Height;
    property    Hint;
    property    Left;
    property    MaxHeight;
    property    MaxWidth;
    property    MinHeight;
    property    MinWidth;
    property    ParentShowHint;
    property    ReadOnly;
    property    ShowHint;
    property    TabOrder;
    property    Text;
    property    TextColor;
    property    Top;
    property    Width;
    property    OnChange;
    property    OnEnter;
    property    OnExit;
    property    OnShowHint;
  end;


function CreateCheckBox(AOwner: TComponent; x, y: TlqCoord; AText: string): TlqCheckBox;


implementation


function CreateCheckBox(AOwner: TComponent; x, y: TlqCoord; AText: string): TlqCheckBox;
begin
  Result := TlqCheckBox.Create(AOwner);
  Result.Top    := y;
  Result.Left   := x;
  Result.Text   := AText;
  Result.Width  := Result.Font.TextWidth(Result.Text) + 24;
end;

{ TlqBaseCheckBox }

procedure TlqBaseCheckBox.SetChecked(const AValue: boolean);
begin
  if ReadOnly then
    Exit; //==>
  if FChecked = AValue then
    Exit; //==>
  FChecked := AValue;
  RePaint;
  if not (csDesigning in ComponentState) then
    DoOnChange;
end;

function TlqBaseCheckBox.GetBoxLayout: TBoxLayout;
begin
  Result := FBoxLayout;
end;

function TlqBaseCheckBox.GetFontDesc: string;
begin
  Result := FFont.FontDesc;
end;

procedure TlqBaseCheckBox.SetBoxLayout(const AValue: TBoxLayout);
begin
  if FBoxLayout = AValue then
    Exit; //==>
  FBoxLayout := AValue;
  RePaint;
end;

procedure TlqBaseCheckBox.SetFontDesc(const AValue: string);
begin
  FFont.Free;
  FFont := lqGetFont(AValue);
  { TODO: Implement AutoSize property, then adjust width here if True }
  RePaint;
end;

procedure TlqBaseCheckBox.SetReadOnly(const AValue: Boolean);
begin
  if FReadOnly=AValue then exit;
  FReadOnly:=AValue;
  RePaint;
end;

procedure TlqBaseCheckBox.SetText(const AValue: string);
begin
  if FText = AValue then
    Exit; //==>
  FText := AValue;
  RePaint;
end;

procedure TlqBaseCheckBox.DoOnChange;
begin
  if Assigned(FOnChange) then
    FOnChange(self);
end;

procedure TlqBaseCheckBox.HandlePaint;
var
  r: TlqRect;
  ix: integer;
  img: TlqImage;
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
    if ReadOnly then
      ix := (2 + (Ord(FChecked) * 2)) - Ord(FChecked)
    else
    begin
      ix := Ord(FChecked);
      if FIsPressed then
        Inc(ix, 2);
    end;
  end
  else
    ix := (2 + (Ord(FChecked) * 2)) - Ord(FChecked);

  // paint the check (in this case a X)
  img := lqImages.GetImage('sys.checkboxes');    // Do NOT localize
  Canvas.DrawImagePart(r.Left, r.Top, img, ix*FBoxSize, 0, FBoxSize, FBoxSize);

  r := GetClientRect;
  { max focus rectangle and text boundry }
  InflateRect(r, -1, -1);
  { exclude the checkbox image and spacing from rectangle }
  if FBoxLayout = tbLeftBox then
  begin
    r.Left  := r.Left + FBoxSize + FImgTextSpacing;
    r.Width := r.Width - FBoxSize - FImgTextSpacing;
  end
  else
    r.Width := r.Width - FBoxSize - FImgTextSpacing;

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

procedure TlqBaseCheckBox.HandleLMouseDown(x, y: integer; shiftstate: TShiftState);
begin
  inherited HandleLMouseDown(x, y, shiftstate);
  FIsPressed := True;
  Repaint;
end;

procedure TlqBaseCheckBox.HandleLMouseUp(x, y: integer; shiftstate: TShiftState);
begin
  inherited HandleLMouseUp(x, y, shiftstate);
  FIsPressed := False;
  Checked := not FChecked;
end;

procedure TlqBaseCheckBox.HandleKeyRelease(var keycode: word;
  var shiftstate: TShiftState; var consumed: boolean);
begin
  if (keycode = keySpace) or (keycode = keyReturn) or (keycode = keyPEnter) then
  begin
    consumed := True;
    Checked := not FChecked;
  end;

  if consumed then
    Exit; //==>

  inherited HandleKeyRelease(keycode, shiftstate, consumed);
end;

constructor TlqBaseCheckBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FText       := 'CheckBox';
  FFont       := lqGetFont('#Label1');
  FHeight     := FFont.Height + 4;
  FWidth      := 120;
  FTextColor  := Parent.TextColor;
  FBackgroundColor := Parent.BackgroundColor;
  FFocusable  := True;
  FBoxSize    := 13;
  FImgTextSpacing := 6;
  FChecked    := False;
  FIsPressed  := False;
  FOnChange   := nil;
  FBoxLayout  := tbLeftBox;
  FReadOnly   := False;
end;

destructor TlqBaseCheckBox.Destroy;
begin
  FFont.Free;
  inherited Destroy;
end;

end.

