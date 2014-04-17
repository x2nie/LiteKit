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
      Defines a Splitter control.
}

unit lq_splitter;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  lq_base,
  lq_main,
  lq_widget;
  

type
  NaturalNumber = 1..High(Integer);

  TlqSnapEvent = procedure(Sender: TObject; const AClosed: boolean) of object;

  TlqSplitter = class(TlqWidget)
  private
    FAutoSnap: Boolean;
    FColorGrabBar: TlqColor;
    FControl: TlqWidget;
    FDownPos: TPoint;
    FMinSize: NaturalNumber;
    FMaxSize: Integer;
    FNewSize: Integer;
    FOldSize: Integer;
    FSplit: Integer;
    FMouseOver: Boolean;
    FOnSnap: TlqSnapEvent;
    procedure   CalcSplitSize(X, Y: Integer; out NewSize, Split: Integer);
    function    FindControl: TlqWidget;
    procedure   SetColorGrabBar(const AValue: TlqColor);
    procedure   UpdateControlSize;
    procedure   UpdateSize(const X, Y: Integer);
  protected
    procedure   DoOnSnap(const AClosed: Boolean);
    function    DoCanResize(var NewSize: Integer): Boolean; virtual;
    procedure   HandleLMouseDown(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleLMouseUp(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleMouseMove(x, y: integer; btnstate: word; shiftstate: TShiftState); override;
    procedure   HandleMouseEnter; override;
    procedure   HandleMouseExit; override;
    procedure   HandleDoubleClick(x, y: integer; button: word; shiftstate: TShiftState); override;
    procedure   HandlePaint; override;
    procedure   StopSizing; dynamic;
    Procedure   DrawGrabBar(ARect: TlqRect); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  published
    property    Align;
    property    AutoSnap: boolean read FAutoSnap write FAutoSnap default True;
    property    ColorGrabBar: TlqColor read FColorGrabBar write SetColorGrabBar default clSplitterGrabBar;
    property    OnSnap: TlqSnapEvent read FOnSnap write FOnSnap;
  end;

function CreateSplitter(AOwner: TComponent; ALeft, ATop, AWidth, AHeight: TlqCoord;
         AnAlign: TAlign): TlqSplitter;

implementation

const
  cSplitterWidth = 8;


function CreateSplitter(AOwner: TComponent; ALeft, ATop, AWidth, AHeight: TlqCoord;
         AnAlign: TAlign): TlqSplitter;
begin
  Result        := TlqSplitter.Create(AOwner);
  Result.Left   := ALeft;
  Result.Top    := ATop;
  Result.Width  := AWidth;
  Result.Height := AHeight;
  Result.Align  := AnAlign;
end;

{ TlqSplitter }

procedure TlqSplitter.CalcSplitSize(X, Y: Integer; out NewSize, Split: Integer);
var
  S: Integer;
begin
  if Align in [alLeft, alRight] then
    Split := X - FDownPos.X
  else
    Split := Y - FDownPos.Y;
  S := 0;
  case Align of
    alLeft:   S := FControl.Width  + Split;
    alRight:  S := FControl.Width  - Split;
    alTop:    S := FControl.Height + Split;
    alBottom: S := FControl.Height - Split;
  end;
  NewSize := S;
  if S < FMinSize then
    NewSize := FMinSize
  else if S > FMaxSize then
    NewSize := FMaxSize;
  if S <> NewSize then
  begin
    if Align in [alRight, alBottom] then
      S := S - NewSize
    else
      S := NewSize - S;
    Inc(Split, S);
  end;
end;

function TlqSplitter.FindControl: TlqWidget;
var
  i: Integer;
  wg: TlqWidget;
  p: TPoint;
  r: TlqRect;
begin
  Result := nil;
  case Align of
    alLeft:   p := Point(Left-2, Top + (Height div 2));
    alRight:  p := Point(Right+2, Top + (Height div 2));
    alTop:    p := Point(Left + (Width div 2), Top-2);
    alBottom: p := Point(Left + (Width div 2), Bottom+2);
  else
    Exit;
  end;

  for i := 0 to Parent.ComponentCount-1 do
  begin
    wg := TlqWidget(Parent.Components[i]);
    if (wg <> nil) and wg.Visible and wg.Enabled then
    begin
      Result := wg;
      r := Result.GetBoundsRect;
      if (r.Width = 0) then
        if Align in [alTop, alLeft] then
          Dec(r.Left)
        else
          Inc(r.Width);
      if (r.Height = 0) then
        if Align in [alTop, alLeft] then
          Dec(r.Top)
        else
          Inc(r.Height);
      if PtInRect(r, p) then Exit;
    end;
  end;
  Result := nil;
end;

procedure TlqSplitter.SetColorGrabBar(const AValue: TlqColor);
begin
  if FColorGrabBar = AValue then
    Exit; //==>
  FColorGrabBar := AValue;
  Repaint;
end;

procedure TlqSplitter.UpdateControlSize;
begin
  if FNewSize <> FOldSize then
  begin
    case Align of
      alLeft, alRight:
         FControl.SetPosition(FControl.Left, FControl.Top, FNewSize, FControl.Height);
      alTop, alBottom:
         FControl.SetPosition(FControl.Left, FControl.Top, FControl.Width, FNewSize);
    end;
    Parent.Realign;
    // if Assigned(FOnMoved) then FOnMoved(Self);
    FOldSize := FNewSize;
  end;
end;

procedure TlqSplitter.UpdateSize(const X, Y: Integer);
begin
  CalcSplitSize(X, Y, FNewSize, FSplit);
end;

procedure TlqSplitter.DoOnSnap(const AClosed: Boolean);
begin
  if Assigned(FOnSnap) then
    FOnSnap(self, AClosed);
end;

function TlqSplitter.DoCanResize(var NewSize: Integer): Boolean;
begin
  // Result := CanResize(NewSize); // omit onCanResize call
  Result := True;
  if Result and (NewSize <= FMinSize) and FAutoSnap then
  begin
    NewSize := 0;
    DoOnSnap(NewSize = 0);
  end;
end;

procedure TlqSplitter.HandleLMouseDown(x, y: integer; shiftstate: TShiftState);
var
  i: integer;
  wg: TlqWidget;
begin
  inherited HandleLMouseDown(x, y, shiftstate);

  FControl := FindControl;
  FDownPos := Point(X, Y);
  
  if Assigned(FControl) then
  begin
    if Align in [alLeft, alRight] then
    begin
      FMaxSize := Parent.Width - FMinSize;
      for i := 0 to Parent.ComponentCount-1 do
      begin
        wg := TlqWidget(Parent.Components[i]);
        if wg.Visible and (wg.Align in [alLeft, alRight]) then
          Dec(FMaxSize, Width);
      end;
      Inc(FMaxSize, FControl.Width);
    end
    else
    begin
      FMaxSize := Parent.Height - FMinSize;
      for i := 0 to Parent.ComponentCount-1 do
      begin
        wg := TlqWidget(Parent.Components[i]);
        if (wg.Align in [alTop, alBottom]) then
          Dec(FMaxSize, Height);
      end;
      Inc(FMaxSize, FControl.Height);
    end;
    UpdateSize(X, Y);

    CaptureMouse;

    {AllocateLineDC;
    with ValidParentForm(Self) do
      if ActiveControl <> nil then
      begin
        FActiveControl := ActiveControl;
        FOldKeyDown := TWinControlAccess(FActiveControl).OnKeyDown;
        TWinControlAccess(FActiveControl).OnKeyDown := FocusKeyDown;
      end;
    if ResizeStyle in [rsLine, rsPattern] then DrawLine;}
  end;
end;

procedure TlqSplitter.HandleLMouseUp(x, y: integer; shiftstate: TShiftState);
begin
  inherited HandleLMouseUp(x, y, shiftstate);
  if Assigned(FControl) then
  begin
    ReleaseMouse;
    // if ResizeStyle in [rsLine, rsPattern] then DrawLine;
    UpdateControlSize;
    {writeln('LT: ', FControl.Left, ':', FControl.Width, '  ', Self.Left, ':', Self.Width);
    writeln('RB: ', FControl.Top, ':', FControl.Height, '  ', Self.Top, ':', Self.Height);}
    StopSizing;
  end;
end;

procedure TlqSplitter.HandleMouseMove(x, y: integer; btnstate: word;
  shiftstate: TShiftState);
var
  NewSize, Split: Integer;
begin
  inherited HandleMouseMove(x, y, btnstate, shiftstate);
  if (ssLeft in shiftstate) and Assigned(FControl) then
  begin
    CalcSplitSize(X, Y, NewSize, Split);
    if DoCanResize(NewSize) then
    begin
      // if ResizeStyle in [rsLine, rsPattern] then DrawLine;
      FNewSize := NewSize;
      FSplit := Split;
      // if ResizeStyle = rsUpdate then
      UpdateControlSize;
      // if ResizeStyle in [rsLine, rsPattern] then DrawLine;
    end;
  end;
end;

procedure TlqSplitter.HandleMouseEnter;
begin
  FMouseOver := True;
  if Align in [alBottom, alTop] then
    MouseCursor := mcSizeNS
  else
    MouseCursor := mcSizeEW;
  Repaint;
end;

procedure TlqSplitter.HandleMouseExit;
begin
  FMouseOver := False;
  if FControl = nil then
    MouseCursor := mcDefault;
  Repaint;
end;

procedure TlqSplitter.HandleDoubleClick(x, y: integer; button: word;
  shiftstate: TShiftState);
begin
  inherited HandleDoubleClick(x, y, button, shiftstate);
  if FAutoSnap then
  begin
    if FNewSize = 0 then
    begin
      FNewSize := FMinSize+1;
      DoCanResize(FNewSize);
    end
    else
    begin
      FNewSize := 0;
      DoCanResize(FNewSize);
    end;
  end;
end;

procedure TlqSplitter.HandlePaint;
var
  lRect: TlqRect;
begin
  Canvas.SetColor(clWindowBackground);
  Canvas.FillRectangle(GetClientRect);

  { just to make it's borders more visible in the designer }
  if csDesigning in ComponentState then
  begin
    Canvas.SetColor(clInactiveWgFrame);
    Canvas.SetLineStyle(1, lsDash);
    Canvas.DrawRectangle(0, 0, Width, Height);
  end;

  case Align of
    alRight,
    alLeft:
        begin
          lRect.Top    := Height div 4;
          lRect.SetBottom(Height div 4 * 3);
          lRect.Left   := 1;
          lRect.SetRight(6);
        end;

    alTop,
    alBottom:
        begin
          lRect.Left   := Width div 4;
          lRect.SetRight(Width div 4 * 3);
          lRect.Top    := 1;
          lRect.SetBottom(6);
        end;
    end;
  DrawGrabBar(lRect);
end;

procedure TlqSplitter.StopSizing;
begin
  if Assigned(FControl) then
  begin
    // if FLineVisible then DrawLine;
    FControl := nil;
    {ReleaseLineDC;
    if Assigned(FActiveControl) then
    begin
      TWinControlAccess(FActiveControl).OnKeyDown := FOldKeyDown;
      FActiveControl := nil;
    end;}
  end;
  {if Assigned(FOnMoved) then
    FOnMoved(Self);}
end;

procedure TlqSplitter.DrawGrabBar(ARect: TlqRect);
var
  lFillRect: TlqRect;
  lSaveColor: TlqColor;
begin
  lSaveColor := Canvas.Color;

  // Draw the outline of the rectangle
  Canvas.Color := clGray;
  Canvas.DrawRectangle(ARect);

  // If the mouse is over the splitter bar, then fill the grab bar part
  // with colour.
  if FMouseOver then
  begin
    lFillRect := ARect;
    InflateRect(lFillRect, -1, -1);
    Canvas.Color := FColorGrabBar;
    Canvas.FillRectangle(lFillRect);
  end;

  // Draw a shadow around the inside of the grab bar
  Canvas.Color := clWhite;
  Canvas.DrawLine(ARect.Left+1, ARect.Top+1, ARect.Right, ARect.Top+1);
  Canvas.DrawLine(ARect.Left+1, ARect.Top+1, ARect.Left+1, ARect.Bottom);

  // Draw some texture inside the grab bar
  Canvas.SetLineStyle(1, lsDot);
  if Align in [alLeft, alRight] then
  begin
    Canvas.DrawLine(ARect.Left+3, ARect.Top+15, ARect.Left+3, ARect.Bottom-15);
    Canvas.Color := clGray;
    Canvas.DrawLine(ARect.Left+4, ARect.Top+16, ARect.Left+4, ARect.Bottom-16);
  end
  else
  begin
    Canvas.DrawLine(ARect.Left+15, ARect.Top+3, ARect.Right-15, ARect.Top+3);
    Canvas.Color := clGray;
    Canvas.DrawLine(ARect.Left+16, ARect.Top+4, ARect.Right-16, ARect.Top+4);
  end;

  Canvas.SetLineStyle(1, lsSolid);
  Canvas.Color := clBlack;

  { TODO : Improve the look of the triangles }
  case Align of
    alRight:
        begin
          // Draw the top triangle
          Canvas.FillTriangle(ARect.Left+2, ARect.Top+5,
                              ARect.Left+2, ARect.Top+10,
                              ARect.Left+4, ARect.Top+7);
          // Draw the bottom triangle
          Canvas.FillTriangle(ARect.Left+2, ARect.Bottom-5,
                              ARect.Left+2, ARect.Bottom-10,
                              ARect.Left+4, ARect.Bottom-7);
        end;

    alLeft:
        begin
          // Draw the top triangle
          Canvas.FillTriangle(ARect.Right-2, ARect.Top+5,
                              ARect.Right-2, ARect.Top+10,
                              ARect.Right-4, ARect.Top+7);
          // Draw the bottom triangle
          Canvas.FillTriangle(ARect.Right-2, ARect.Bottom-5,
                              ARect.Right-2, ARect.Bottom-10,
                              ARect.Right-4, ARect.Bottom-7);
        end;

    alBottom:
        begin
          // Draw the left triangle
          Canvas.FillTriangle(ARect.Left+5,   ARect.Top+2,
                              ARect.Left+10,  ARect.Top+2,
                              ARect.Left+7,   ARect.Top+4);
          // Draw the right triangle
          Canvas.FillTriangle(ARect.Right-5,  ARect.Top+2,
                              ARect.Right-10, ARect.Top+2,
                              ARect.Right-7,  ARect.Top+4);
        end;

    alTop:
        begin
          // Draw the left triangle
          Canvas.FillTriangle(ARect.Left+5,   ARect.Bottom-1,
                              ARect.Left+10,  ARect.Bottom-1,
                              ARect.Left+7,   ARect.Bottom-4);
          // Draw the right triangle
          Canvas.FillTriangle(ARect.Right-5,  ARect.Bottom-1,
                              ARect.Right-10, ARect.Bottom-1,
                              ARect.Right-7,  ARect.Bottom-4);
        end;
  end;

  Canvas.Color := lSaveColor;
end;

constructor TlqSplitter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAutoSnap := True;
  Height := 100;
  Align := alLeft;
  Width := cSplitterWidth;
  FMinSize := 30;
  // FResizeStyle := rsPattern;
  FOldSize := -1;
  FMouseOver := False;
  FColorGrabBar := clSplitterGrabBar;
end;

destructor TlqSplitter.Destroy;
begin
  inherited Destroy;
end;

end.
