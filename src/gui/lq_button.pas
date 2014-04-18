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
      Defines a push button control.

    TODO:
      * multi-line button text. It must take into account image position as well.
}

unit lq_button;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  lq_base,
  lq_main,
  lq_widget,
  lq_command_intf;

type

  TImageLayout = (ilImageLeft, ilImageTop, ilImageRight, ilImageBottom);


  TlqBaseButton = class(TlqWidget, ICommandHolder)
  private
    FCommand: ICommand;
    FImageLayout: TImageLayout;
    FFlat: Boolean;
    FImageName: string;
    FClicked: Boolean;
    FShowImage: Boolean;
    FClickOnPush: Boolean;    { Used for group buttons where click happens on "down" state. Normal buttons, the click happens on "release" state }
    FGroupIndex: integer;
    FAllowAllUp: boolean;
    FModalResult: TlqModalResult;
    function    GetFontDesc: string;
    procedure   SetDefault(const AValue: boolean);
    procedure   SetEmbedded(const AValue: Boolean);
    procedure   SetFlat(const AValue: Boolean);
    procedure   SetFontDesc(const AValue: string);
    procedure   SetImageLayout(const AValue: TImageLayout);
    procedure   SetImageName(const AValue: string);
    procedure   SetText(const AValue: string);
    procedure   SetDown(AValue: Boolean);
    procedure   SetImageMargin(const Value: integer);
    procedure   SetImageSpacing(const Value: integer);
    function    GetAllowDown: Boolean;
    procedure   SetAllowDown(const Value: Boolean);
    procedure   SetAllowAllUp(const Value: boolean);
    procedure   DoPush;
    procedure   DoRelease(x, y: integer);
    procedure   SetAllowMultiLineText(const AValue: boolean);
  protected
    FImageMargin: integer;
    FImageSpacing: integer;
    FEmbedded: Boolean;
    FDown: Boolean;
    FImage: TlqImage;
    FText: string;
    FFont: TlqFont;
    FDefault: boolean;
    FState: integer;  // 0 - normal  // 1 - hover
    FAllowMultiLineText: boolean;
    procedure   SetShowImage(AValue: Boolean);
    procedure   CalculatePositions(var ImageX, ImageY, TextX, TextY : integer);
    procedure   HandlePaint; override;
    procedure   HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); override;
    procedure   HandleKeyRelease(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); override;
    procedure   HandleLMouseDown(X, Y: integer; ShiftState: TShiftState); override;
    procedure   HandleLMouseUp(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleMouseExit; override;
    procedure   HandleMouseEnter; override;
    { When buttons are in a toggle state (GroupIndex > 0), are all buttons in the group
      allowed to be up. }
    property    AllowAllUp: boolean read FAllowAllUp write SetAllowAllUp default False;
    { Buttons behave like toggle buttons. This is an alias for GroupIndex > 0 }
    property    AllowDown: Boolean read GetAllowDown write SetAllowDown;
    property    AllowMultiLineText: boolean read FAllowMultiLineText write SetAllowMultiLineText default False;
    property    Default: boolean read FDefault write SetDefault default False;
    property    Down: Boolean read FDown write SetDown default False;
    { The button will not show focus. It might also have a different down state (look).
      This is similar to Focusable = False, but the appearance of the down state might differ. }
    property    Embedded: Boolean read FEmbedded write SetEmbedded default False;
    property    Flat: Boolean read FFlat write SetFlat default False;
    property    FontDesc: string read GetFontDesc write SetFontDesc;
    { Used in combination with AllowDown and AllowAllUp. Allows buttons in the same
      group to work together. }
    property    GroupIndex: integer read FGroupIndex write FGroupIndex default 0;
    property    ImageMargin: integer read FImageMargin write SetImageMargin default 3;
    property    ImageName: string read FImageName write SetImageName;
    property    ImageSpacing: integer read FImageSpacing write SetImageSpacing default -1;
    property    ImageLayout: TImageLayout read FImageLayout write SetImageLayout default ilImageLeft;
    property    ModalResult: TlqModalResult read FModalResult write FModalResult default mrNone;
    property    ShowImage: Boolean read FShowImage write SetShowImage default True;
    property    Text: string read FText write SetText;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   Click;
    function    GetCommand: ICommand;   // ICommandHolder interface
    procedure   SetCommand(ACommand: ICommand); // ICommandHolder interface
    property    Font: TlqFont read FFont;
  end;
  
  
  { A standard push button component.

    If you want toolbar style buttons you need to set the following properties:
    AllowAllUp = True; AllowDown = True;
    and each button's GroupIndex must be greater than 0, but not the same as any
    other button.

    If you want toggle buttons (only one button may be down at a time):
    Set AllowAllUp = False and AllowDown = True
    AllowDown = True will automatically set the GroupIndex = 1. If you want more
    than one set of toggle buttons in a Parent, you need to manually set the
    GroupIndex property instead. All buttons with the same GroupIndex work
    together. }
  TlqButton = class(TlqBaseButton)
  published
    property    Align;
    property    AllowAllUp;
    property    AllowDown;
    property    AllowMultiLineText;
    property    BackgroundColor default clButtonFace;
    property    Default;
    property    Down;
    property    Embedded;
    property    Enabled;
    property    Flat;
    property    FontDesc;
    property    GroupIndex;
    property    Height;
    property    Hint;
    property    ImageLayout;
    property    ImageMargin;
    property    ImageName;
    property    ImageSpacing;
    property    Left;
    property    MaxHeight;
    property    MaxWidth;
    property    MinHeight;
    property    MinWidth;
    property    ModalResult;
    property    ParentShowHint;
    property    ShowHint;
    property    ShowImage;
    property    TabOrder;
    property    Text;
    property    TextColor;
    property    Top;
    property    Width;
    property    OnClick;
    property    OnDragEnter;
    property    OnDragLeave;
    property    OnDragDrop;
    property    OnDragStartDetected;
    property    OnMouseDown;
    property    OnMouseExit;
    property    OnMouseEnter;
    property    OnMouseMove;
    property    OnMouseUp;
    property    OnShowHint;
  end;


function CreateButton(AOwner: TComponent; x, y, w: TlqCoord; AText: string;
  AOnClickEvent: TNotifyEvent; AImage: string = ''): TlqButton;


implementation

uses
  lq_form; {$Note Try and remove this lq_form dependency.}

function CreateButton(AOwner: TComponent; x, y, w: TlqCoord; AText: string;
  AOnClickEvent: TNotifyEvent; AImage: string): TlqButton;
begin
  Result         := TlqButton.Create(AOwner);
  Result.Text    := AText;
  Result.SetPosition(x, y, w, Result.Height); // font was used to calculate height.
  Result.OnClick := AOnClickEvent;
  Result.ImageName := AImage;
  Result.UpdateWindowPosition;
end;

{ TlqBaseButton }

procedure TlqBaseButton.SetDown(AValue: Boolean);
begin
  if AValue <> FDown then
  begin
    FDown := AValue;
    if AllowDown then
      RePaint;
  end;
end;

procedure TlqBaseButton.SetShowImage(AValue: Boolean);
begin
  if AValue <> FShowImage then
  begin
    FShowImage := AValue;
    if (FImage <> nil) and ShowImage then
      RePaint;
  end;
end;

procedure TlqBaseButton.CalculatePositions(var ImageX, ImageY, TextX, TextY: integer);
var
  textWidth, textHeight: integer;
  w: integer;
begin
  if text = '' then
  begin
    textWidth := 0;
    textHeight := 0;
  end
  else
  begin
    textWidth := FFont.TextWidth (Text);
    textHeight := FFont.Height;
    // Only single line texts will be placed correctly.
    // Normally FFont.TextHeight should be used (not yet implemented)
  end;
  if FImageLayout in [ilImageLeft, ilImageRight] then
  begin
    TextY := (Height - textHeight) div 2;
    // center vertically
    if FShowImage and assigned (FImage) then
    begin
      ImageY := (Height - FImage.Height) div 2;
      // horizontal places if image and text
      if FImageMargin = -1 then
      begin  // Free space between border and image is the same as between border and text
        if FImageSpacing = -1 then // free space between image/text = border/text = border/image
        begin
          w := (Width - FImage.Width - textWidth) div 3;
          if w < 3 then  // minimal margin from border for rectangle/focusrect/...
            w := 3;
          if FImageLayout = ilImageLeft then
          begin
            ImageX := w;
            TextX := Width - w - textWidth;
          end
          else // if FImageLayout = ilImageRight then
          begin
            ImageX := Width - w - FImage.width;
            TextX := w;
          end;
        end
        else // fixed space between image/text
        begin
          w := (Width - FImageSpacing - FImage.width - textWidth) div 2;
          if w < 3 then  // minimal margin from border for rectangle/focusrect/...
            w := 3;
          if FImageLayout = ilImageLeft then
          begin
            ImageX := w;
            TextX := w + FImage.width + FImageSpacing;
          end
          else // if FImageLayout = ilImageRight then
          begin
            ImageX := width - w - FImage.Width;
            TextX := w;
          end;
        end;
      end
      else  // Fixed image
      begin
        if FImageLayout = ilImageLeft then
        begin
          ImageX := FImageMargin + 3;
          if FImageSpacing = -1 then
          begin
            w := (Width - FImage.Width - ImageX - textWidth) div 2;
            if w < 0 then
              w := 0;
          end
          else
            w := FImageSpacing;
          TextX := ImageX + FImage.width + w;
        end
        else // if FImageLayout = ilImageRight then
        begin
          ImageX := Width - FImageMargin - 3 - FImage.width;
          if FImageSpacing = -1 then
          begin
            w := (Width - FImageMargin - FImage.width - textWidth) div 2;
            if w < 3 then
              w := 3;
            TextX := w;
          end
          else
          begin
            textX := ImageX - textWidth - FImageSpacing;
            if textX < 3 then
              textX := 3;
          end;
        end;
      end;
    end
    else
    begin  // no image,
      ImageY := 0;
      ImageX := 0;
      TextX := (Width - textWidth) div 2;
    end;
  end
  else // if ImageLayout in [ilImageTop, ilImageBottom] then
  begin
    TextX := (Width - textWidth) div 2;
    // center horizontaly
    if FShowImage and assigned (FImage) then
    begin
      ImageX := (Width - FImage.Width) div 2;
      // vertical places if image and text
      if FImageMargin = -1 then
      begin  // Free space between border and image is the same as between border and text
        if FImageSpacing = -1 then // free space between image/text = border/text = border/image
        begin
          w := (Height - FImage.Height - textHeight) div 3;
          if w < 3 then  // minimal margin from border for rectangle/focusrect/...
            w := 3;
          if FImageLayout = ilImageTop then
          begin
            ImageY := w;
            TextY := Height - w - textHeight;
          end
          else // if FImageLayout = ilImageBottom then
          begin
            ImageY := Height - w - FImage.Height;
            TextY := w;
          end;
        end
        else // fixed space between image/text
        begin
          w := (Height - FImageSpacing - FImage.Height - textHeight) div 2;
          if w < 3 then  // minimal margin from border for rectangle/focusrect/...
            w := 3;
          if FImageLayout = ilImageTop then
          begin
            ImageY := w;
            TextY := w + FImage.Height + FImageSpacing;
          end
          else // if FImageLayout = ilImageRight then
          begin
            ImageY := Height - w - FImage.Height;
            TextY := w;
          end;
        end;
      end
      else  // Fixed image
      begin
        if FImageLayout = ilImageTop then
        begin
          ImageY := FImageMargin + 3;
          if FImageSpacing = -1 then
          begin
            w := (Height - FImage.Height - ImageY - textHeight) div 2;
            if w < 0 then
              w := 0;
            end
          else
            w := FImageSpacing;
          TextY := ImageY + FImage.Height + w;
        end
        else // if FImageLayout = ilImageRight then
        begin
          ImageY := Height - FImageMargin - 3 - FImage.Height;
          if FImageSpacing = -1 then
          begin
            w := (Height - FImageMargin - FImage.Height - textHeight) div 2;
            if w < 3 then
              w := 3;
            TextY := w;
          end
          else
          begin
            textY := ImageY - textHeight - FImageSpacing;
            if textY < 3 then
              textY := 3;
           end;
        end;
      end;
    end
    else
    begin  // no image,
      ImageY := 0;
      ImageX := 0;
      TextY := (Height - textHeight) div 2;
    end;
  end;
end;

procedure TlqBaseButton.SetText(const AValue: string);
begin
  if FText = AValue then
    Exit;
  FText := AValue;
  RePaint;
end;

procedure TlqBaseButton.SetImageName(const AValue: string);
begin
  FImageName := AValue;
  FImage     := lqImages.GetImage(FImageName);
  Repaint;
end;

function TlqBaseButton.GetFontDesc: string;
begin
  Result := FFont.FontDesc;
end;

procedure TlqBaseButton.SetDefault(const AValue: boolean);
var
  i: integer;
  wg: TlqWidget;
begin
  if FDefault = AValue then
    Exit; //==>
  FDefault := AValue;

  // Clear other buttons Default state
  if FDefault and (Parent <> nil) then
  begin
    for i := 0 to Parent.ComponentCount-1 do
    begin
      wg := TlqWidget(Parent.Components[i]);
      if (wg <> nil) and (wg <> self) and (wg is TlqBaseButton) then
      begin
        TlqBaseButton(wg).Default := False;
      end;
    end;  { for }
  end;  { if }

  RePaint;
end;

procedure TlqBaseButton.SetEmbedded(const AValue: Boolean);
begin
  if FEmbedded = AValue then
    Exit;
  FEmbedded := AValue;
end;

procedure TlqBaseButton.SetFlat(const AValue: Boolean);
begin
  if FFlat = AValue then
    Exit; //==>
  FFlat := AValue;
  if FFlat then
    FDefault := False;  // you can't have it all!
end;

procedure TlqBaseButton.SetFontDesc(const AValue: string);
begin
  FFont.Free;
  FFont := lqGetFont(AValue);
  RePaint;
end;

constructor TlqBaseButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FText         := 'Button';
  FFont         := lqGetFont('#Label1');
  Height        := FFont.Height + 8;
  Width         := 80;
  FFocusable    := True;
  FTextColor    := Parent.TextColor;
  FBackgroundColor := clButtonFace;
  OnClick       := nil;
  FDown         := False;
  FClicked      := False;
  FDown         := False;
  FClickOnPush  := False;
  FGroupIndex   := 0;
  FImage        := nil;
  FImageName    := '';
  FShowImage    := True;
  FImageLayout  := ilImageLeft;
  FImageMargin  := 3;   // image is 3 pixels from edge of button. -1 will centre image.
  FImageSpacing := -1;  // text is centered in remaining space
  FModalResult  := mrNone;
  FEmbedded     := False;
  FDefault      := False;
  FAllowAllUp   := False;
  FState        := 0;
  FAllowMultiLineText := False;
  FIgnoreDblClicks := True;
end;

destructor TlqBaseButton.Destroy;
begin
  FImage := nil;
  FText  := '';
  FFont.Free;
  inherited Destroy;
end;

procedure TlqBaseButton.HandlePaint;
var
  tx, ty, ix, iy: integer;
  r: TlqRect;
  border: TRect;
  offset: TPoint;
  lBtnFlags: TlqButtonFlags;
  clr: TlqColor;
  img: TlqImage;
  lTextFlags: TlqTextFlags;
begin
//  inherited HandlePaint;
  Canvas.ClearClipRect;

  r.SetRect(0, 0, Width, Height);
  border := lqStyle.GetButtonBorders;

  lBtnFlags := [];
  if FDown then
    Include(lBtnFlags, btfIsPressed);

  if FFocused and (not FEmbedded) then
    Include(lBtnFlags, btfHasFocus);

  if FEmbedded then
    Include(lBtnFlags, btfIsEmbedded);

  // In the UI Designer we want the button more visible
  if not (csDesigning in ComponentState) then
  begin
    if FFlat and (FState = 1) then  // mouse over
      Include(lBtnFlags, btfHover)
    else if FFlat then
      Include(lBtnFlags, btfFlat);

    if (not FFlat) and (not FDown) and lqStyle.HasButtonHoverEffect then
    begin
      if FState = 1 then
        Include(lBtnFlags, btfHover);
    end;
  end
  else
  begin
    { while in the designer we want hover effect all the time }
    if FFlat then
      Include(lBtnFlags, btfHover);
  end;

  if (not FFlat) and FDefault then
    Include(lBtnFlags, btfIsDefault);

  if FBackgroundColor <> clButtonFace then
  begin
    clr := lqColorToRGB(clButtonFace);
    lqSetNamedColor(clButtonface, FBackgroundColor);
    lqStyle.DrawButtonFace(Canvas, r, lBtnFlags);
    lqSetNamedColor(clButtonface, clr);
  end
  else
    lqStyle.DrawButtonFace(Canvas, r, lBtnFlags);

  if FFocused and (not FEmbedded) then
  begin
    InflateRect(r, -border.Left, -border.Top);
    lqStyle.DrawFocusRect(Canvas, r);
  end;

  Canvas.SetTextColor(FTextColor);
  Canvas.SetColor(clText1);

  Canvas.SetClipRect(r);
  Canvas.SetFont(Font);

  if FDown then
    offset := lqStyle.GetButtonShift
  else
    offset := Point(0, 0);

  CalculatePositions(ix, iy, tx, ty);

  if FShowImage and Assigned(FImage) then
  begin
    if Enabled then
      Canvas.DrawImage(ix+offset.x, iy+offset.y, FImage)
    else
    begin
      img := FImage.CreateDisabledImage;
      Canvas.DrawImage(ix+offset.x, iy+offset.y, img);
      img.Free;
    end;

  end;

  { EXPERIMENTAL: multi-line button text
      Only in this condition do we support multi-line text }
  if AllowMultiLineText and (FImageLayout = ilImageLeft) then
  begin
    r.SetRect(0, 0, Width, Height);
    InflateRect(r, -border.Left, -border.Top);   { same as focus rectangle }
    if FShowImage and Assigned(FImage) then
    begin
      ix := FImageMargin + FImage.Width;
      if FImageSpacing > 0 then
        ix += FImageSpacing;
      OffsetRect(r, ix, 0);
      r.Width -= ix;
    end;
    if FDown then
     OffsetRect(r, offset.x, offset.y);

    lTextFlags := [txtHCenter, txtVCenter];
    if not Enabled then
      lTextFlags += [txtDisabled];
    Canvas.DrawText(r, Text, lTextFlags);  { DrawText does use lqStyle }
  end
  else
    lqStyle.DrawString(Canvas, tx+offset.x, ty+offset.y, Text, Enabled);
end;

procedure TlqBaseButton.DoPush;
var
  n: integer;
  c: TComponent;
begin
  FClickOnPush := (not FDown) and AllowDown;

  // search the other buttons in the group
  for n := 0 to Parent.ComponentCount - 1 do
  begin
    c := Parent.Components[n];
    if (c <> self) and (c is TlqBaseButton) then
      with TlqBaseButton(c) do
        if GroupIndex = self.GroupIndex then
          Down := False;
  end;

  FDown    := True;
  FClicked := True;

  RePaint;
  if FClickOnPush then
    Click;
end;

procedure TlqBaseButton.DoRelease(x, y: integer);
var
  r: TlqRect;
begin
  r.SetRect(0, 0, Width, Height);
  if AllowDown then
  begin
    if FDown and (not FClickOnPush) and FAllowAllUp then
    begin
      FDown := False;
      RePaint;
      lqApplication.ProcessMessages;
      if PtInRect(r, Point(x, y)) and FOnClickPending then
        Click;
    end;
  end
  else
  begin
    if FDown and FClicked then
    begin
      FDown := False;
      RePaint;
      lqApplication.ProcessMessages;
      if PtInRect(r, Point(x, y)) and FOnClickPending then
        Click;
    end;
  end;

  FClickOnPush := False;
  FClicked     := False;
end;

procedure TlqBaseButton.SetAllowMultiLineText(const AValue: boolean);
begin
  if FAllowMultiLineText = AValue then exit;
  FAllowMultiLineText := AValue;
  Repaint;
end;

procedure TlqBaseButton.HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean);
begin
  if (keycode = keyReturn) or (keycode = keySpace) or (keycode = keyPEnter) then
  begin
    FOnClickPending := True;
    DoPush;
    Consumed := True;
  end
  else
    inherited;
end;

procedure TlqBaseButton.HandleKeyRelease(var keycode: word; var shiftstate: TShiftState; var consumed: boolean);
begin
  if (keycode = keyReturn) or (keycode = keySpace) or (keycode = keyPEnter) then
  begin
    DoRelease(1, 1); // fake co-ordinates so it executes the Click
    Consumed := True;
    FOnClickPending := False;
  end
  else
    inherited;
end;

procedure TlqBaseButton.HandleLMouseDown(X, Y: integer; ShiftState: TShiftState);
begin
  inherited;
  if (csDesigning in ComponentState) then
    Exit;
  CaptureMouse;
  DoPush;
end;

procedure TlqBaseButton.HandleLMouseUp(x, y: integer; shiftstate: TShiftState);
begin
//  inherited;
  if (csDesigning in ComponentState) then
    Exit;
  ReleaseMouse;
  DoRelease(x, y);
end;

procedure TlqBaseButton.HandleMouseExit;
begin
  inherited HandleMouseExit;
  if (csDesigning in ComponentState) then
    Exit;
  if Enabled then
    FState := 0;
  if FDown and (not AllowDown) then
  begin
    FDown := False;
    Repaint;
  end
  else if FFlat or lqStyle.HasButtonHoverEffect then
  begin
    if Enabled then
      Repaint;
  end;
end;

procedure TlqBaseButton.HandleMouseEnter;
begin
  inherited HandleMouseEnter;
  if (csDesigning in ComponentState) then
    Exit;
  if Enabled then
    FState := 1;
  if FClicked and (not AllowDown) then
  begin
    FDown := True;
    Repaint;
  end
  else if FFlat or lqStyle.HasButtonHoverEffect then
  begin
    if Enabled then
      Repaint;
  end;
end;

procedure TlqBaseButton.Click;
var
  pform: TlqForm;
begin
  if not Enabled then
    Exit; //==>

  if (not AllowDown) then
  begin
    FDown    := False;
    FClicked := False;
  end;

  pform := WidgetParentForm(self);
  if pform <> nil then
    pform.ModalResult := ModalResult;

  if Assigned(FCommand) then    // ICommand takes preference to OnClick
    FCommand.Execute
  else
  begin
    if Assigned(OnClick) then
      OnClick(self);
  end;
end;

function TlqBaseButton.GetCommand: ICommand;
begin
  Result := FCommand;
end;

procedure TlqBaseButton.SetCommand(ACommand: ICommand);
begin
  FCommand := ACommand;
end;

procedure TlqBaseButton.SetImageMargin(const Value: integer);
begin
  FImageMargin := Value;
  Repaint;
end;

procedure TlqBaseButton.SetImageSpacing(const Value: integer);
begin
  FImageSpacing := Value;
  Repaint;
end;

procedure TlqBaseButton.SetImageLayout(const AValue: TImageLayout);
begin
  if FImageLayout <> AValue then
    begin
    FImageLayout := AValue;
    Repaint;  //Isn't Invalidate better ?
    end;
end;

function TlqBaseButton.GetAllowDown: Boolean;
begin
  Result := GroupIndex > 0;
end;

procedure TlqBaseButton.SetAllowDown(const Value: Boolean);
begin
  GroupIndex := 1;
end;

procedure TlqBaseButton.SetAllowAllUp(const Value: boolean);
begin
  FAllowAllUp := Value;
end;

end.

