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
      Defines a window that gets used to display help hints (aka a HintWindow)
}

unit lq_hint;

{$mode objfpc}{$H+}

{.$Define Debug}

interface

uses
  Classes,
  SysUtils,
  lq_base,
  lq_main,
  lq_form;
  
type
  TlqHintWindow = class(TlqBaseForm)
  private
    FFont: TlqFont;
    FTime: Integer;
    FShadow: Integer;
    FBorder: Integer;
    FMargin: Integer;
    FTimer: TlqTimer;
    FHintTextRec: TlqRect;
    //FText: TlqString;
    procedure   FormShow(Sender: TObject);
    procedure   FormHide(Sender: TObject);
    function    GetText: TlqString;
    procedure   SetText(const AValue: TlqString);
    procedure   HintTimerFired(Sender: TObject);
    procedure   SetShadow(AValue: Integer);
    procedure   SetBorder(AValue: Integer);
    procedure   SetTime(AValue: Integer);
    procedure   SetShadowColor(AValue: TlqColor);
    function    GetFontDesc: string;
    procedure   SetFontDesc(const AValue: string);
  protected
    procedure   HandleShow; override;
    procedure   HandlePaint; override;
    procedure   PaintBorder; virtual;
    procedure   PaintHintText; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    property    Font: TlqFont read FFont;
    property    Text: TlqString read GetText write SetText;
    property    Shadow: Integer read FShadow write SetShadow default 0;
    property    Border: Integer read FBorder write SetBorder default 1;
    property    Margin: Integer read FMargin write FMargin default 3;
    property    ShadowColor: TlqColor write SetShadowColor default clGray;
    property    Time: Integer read FTime write SetTime;
  published
    property    BackgroundColor;
    property    FontDesc: string read GetFontDesc write SetFontDesc;
    property    TextColor;
    //property    OnActivate;
    property    OnClose;
    //property    OnCloseQuery;
    property    OnCreate;
    //property    OnDeactivate;
    property    OnDestroy;
    property    OnHide;
    property    OnPaint;
    property    OnResize;
    property    OnShow;
  end;


  TlqHintWindowClass = class of TlqHintWindow;
  

var
  HintWindowClass: TlqHintWindowClass = TlqHintWindow;


implementation


type
  TlqHintShadow = class(TlqBaseForm)
  public
    constructor Create(AOwner: TComponent); override;
  end;
  
  
var
  uShadowForm: TlqHintShadow;


{ TlqHintWindow }

procedure TlqHintWindow.FormShow(Sender: TObject);
begin
  FTimer.Enabled:= True;
end;

procedure TlqHintWindow.FormHide(Sender: TObject);
begin
  if Assigned(uShadowForm) then
    uShadowForm.Hide;
end;

function TlqHintWindow.GetText: TlqString;
begin
  Result := FText;
end;

procedure TlqHintWindow.SetText(const AValue: TlqString);
begin
  FText := AValue;
end;

procedure TlqHintWindow.HintTimerFired(Sender: TObject);
begin
  {$IFDEF DEBUG}
  DebugLn('DEBUG:  TlqHintWindow.HintTimerFired timer fired');
  {$ENDIF}
  FTimer.Enabled := False;
  Hide;
end;

procedure TlqHintWindow.SetShadow(AValue: Integer);
begin
  if FShadow <> AValue then
    FShadow := AValue;
end;

procedure TlqHintWindow.SetBorder(AValue: Integer);
begin
  if FBorder <> AValue then
    FBorder := AValue;
end;

procedure TlqHintWindow.SetTime(AValue: Integer);
begin
  if FTime <> AValue then
  begin
    FTime := AValue;
    FTimer.Interval := FTime;
  end;
end;

procedure TlqHintWindow.SetShadowColor(AValue: Tlqcolor);
begin
  if uShadowForm.BackgroundColor <> AValue then
    uShadowForm.BackgroundColor := AValue;
end;

function TlqHintWindow.GetFontDesc: string;
begin
  Result := FFont.FontDesc;
end;

procedure TlqHintWindow.SetFontDesc(const AValue: string);
begin
  FFont.Free;
  FFont := lqGetFont(AValue);
end;

procedure TlqHintWindow.HandleShow;
begin
  // This is so the Shadow Window is below the Hint Window.
  if Shadow > 0 then
  begin
    uShadowForm.SetPosition(Left+Shadow, Top+Shadow, Width, Height);
    uShadowForm.Show;
  end;
  inherited HandleShow;
end;

procedure TlqHintWindow.HandlePaint;
begin
  inherited HandlePaint;  // background is set
  Canvas.ClearClipRect;
  Canvas.Font := FFont;
  // Do we need to resize?
  PaintBorder;
  if FBorder > 0 then
    Canvas.SetClipRect(lqRect(FBorder, FBorder, Width-(FBorder*2), Height-(FBorder*2)));
  PaintHintText;
end;

procedure TlqHintWindow.PaintBorder;
var
  i: integer;
begin
  if FBorder = 0 then  // no border
    Exit;
  Canvas.Color := clBlack;
  for i := 0 to FBorder-1 do
  begin
    Canvas.DrawRectangle(i, i, Width-(i*2), Height-(i*2));
  end;
end;

procedure TlqHintWindow.PaintHintText;
begin
  FHintTextRec.SetRect(FBorder, FBorder, Width-(FBorder*2), Height-(FBorder*2));
  Canvas.TextColor := FTextColor;
  Canvas.DrawText(FHintTextRec, Text, [txtHCenter, txtVCenter, txtWrap]);
end;

constructor TlqHintWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Name := 'F_Hint';
  WindowPosition := wpUser;
  WindowType := wtPopup;
  Sizeable := False;
  BackgroundColor:= clHintWindow; //clBlack;  // This becomes the hint border so don't set to clHintWindow
  FFont := lqGetFont('#Label1');
  FMargin := 3;
  FBorder := 1;
  FShadow := 0; // no shadow by default
  FTime := 5000; // show hint for 5 seconds then close
  FHintTextRec.SetRect(FBorder, FBorder, Width-(FBorder*2), Height-(FBorder*2));
  FTimer := TlqTimer.Create(FTime);
  FTimer.OnTimer := @HintTimerFired;
  uShadowForm:= TlqHintShadow.Create(nil);
  OnClick := @HintTimerFired;
  OnShow := @FormShow;
  OnHide := @FormHide;
end;

destructor TlqHintWindow.Destroy;
begin
  FTimer.Free;
  FFont.Free;
  inherited Destroy;
  uShadowForm.Free;
end;

constructor TlqHintShadow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Name := 'F_Shadow';
  WindowPosition := wpUser;
  WindowType := wtPopup;
  Sizeable := False;
  BackgroundColor := clGray;
end;


end.

