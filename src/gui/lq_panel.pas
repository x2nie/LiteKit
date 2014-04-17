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
      Defines a Panel control. Also known as a Bevel or Frame control.
      This control can also draw itself like a GroupBox component.
}

unit lq_panel;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  lq_base,
  lq_main,
  lq_widget;

type

  TPanelShape = (bsBox, bsFrame, bsTopLine, bsBottomLine, bsLeftLine,
    bsRightLine, bsSpacer, bsVerDivider);

  TPanelStyle = (bsLowered, bsRaised, bsFlat);

  TPanelBorder = (bsSingle, bsDouble);


  TlqAbstractPanel = class(TlqWidget)
  private
    FPanelStyle: TPanelStyle;
    FPanelBorder: TPanelBorder;
    FParentBackgroundColor: Boolean;
    procedure   SetPanelStyle(const AValue: TPanelStyle);
    procedure   SetPanelBorder(const AValue: TPanelBorder);
    procedure   SetParentBackgroundColor(const AValue: Boolean);
  protected
    procedure   HandlePaint; override;
    property    Style: TPanelStyle read FPanelStyle write SetPanelStyle default bsRaised;
    property    BorderStyle: TPanelBorder read FPanelBorder write SetPanelBorder default bsSingle;
    property    ParentBackgroundColor: Boolean read FParentBackgroundColor write SetParentBackgroundColor default False;
  public
    constructor Create(AOwner: TComponent); override;
    function    GetClientRect: TlqRect; override;
  end;
  

  TlqBevel = class(TlqAbstractPanel)
  private
    FPanelShape: TPanelShape;
    procedure   SetPanelShape(const AValue: TPanelShape);
    procedure   DrawBox;        //  bsBox
    procedure   DrawFrame;      //  bsFrame
    procedure   DrawTopLine;    //  bsTopLine
    procedure   DrawBottomLine; //  bsBottomLine
    procedure   DrawLeftLine;   //  bsLeftLine
    procedure   DrawRightLine;  //  bsRightLine
    procedure   DrawSpacer;     //  bsSpacer
    procedure   DrawVerDivider; //  bsVerDivider
  protected
    procedure   HandlePaint; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property    AcceptDrops;
    property    Align;
    property    BackgroundColor;
    property    BorderStyle;
    property    Enabled;
    property    Height;
    property    Hint;
    property    Left;
    property    MaxHeight;
    property    MaxWidth;
    property    MinHeight;
    property    MinWidth;
    property    ParentBackgroundColor;
    property    ParentShowHint;
    property    Shape: TPanelShape read FPanelShape write SetPanelShape default bsBox;
    property    ShowHint;
    property    Style;
    property    Top;
    property    Width;
    property    OnClick;
    property    OnDoubleClick;
    property    OnDragDrop;
    property    OnDragEnter;
    property    OnDragLeave;
    property    OnDragStartDetected;
    property    OnMouseDown;
    property    OnMouseMove;
    property    OnMouseScroll;
    property    OnMouseUp;
    property    OnPaint;
    property    OnShowHint;
  end;
  

  TlqPanel = class(TlqAbstractPanel)
  private
    FAlignment: TAlignment;
    FLayout: TLayout;
    FWrapText: boolean;
    FLineSpace: integer;
    FMargin: integer;
    FText: string;
    function    GetAlignment: TAlignment;
    procedure   SetAlignment(const AValue: TAlignment);
    function    GetLayout: TLayout;
    procedure   SetLayout(const AValue: TLayout);
    function    GetText: string;
    procedure   SetText(const AValue: string);
    function    GetLineSpace: integer;
    procedure   SetLineSpace(const AValue: integer);
    function    GetMargin: integer;
    procedure   SetMargin(const AValue: integer);
    function    GetWrapText: boolean;
    procedure   SetWrapText(const AValue: boolean);
  protected
    FFont: TlqFont;
    function    GetFontDesc: string; virtual;
    procedure   SetFontDesc(const AValue: string); virtual;
    procedure   HandlePaint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    function    GetClientRect: TlqRect; override;
    property    Font: TlqFont read FFont;
  published
    property    AcceptDrops;
    property    Align;
    property    Alignment: TAlignment read GetAlignment write SetAlignment default taCenter;
    property    BackgroundColor;
    property    BorderStyle;
    property    Enabled;
    property    FontDesc: string read GetFontDesc write SetFontDesc;
    property    Height;
    property    Hint;
    property    Layout: TLayout read GetLayout write SetLayout default tlCenter;
    property    Left;
    property    LineSpace: integer read GetLineSpace write SetLineSpace default 2;
    property    Margin: integer read GetMargin write SetMargin default 2;
    property    MaxHeight;
    property    MaxWidth;
    property    MinHeight;
    property    MinWidth;
    property    ParentBackgroundColor;
    property    ParentShowHint;
    property    ShowHint;
    property    Style;
    property    Text: string read GetText write SetText;
    property    TextColor;
    property    Top;
    property    Width;
    property    WrapText: boolean read GetWrapText write SetWrapText default False;
    property    OnClick;
    property    OnDoubleClick;
    property    OnDragDrop;
    property    OnDragEnter;
    property    OnDragLeave;
    property    OnDragStartDetected;
    property    OnMouseDown;
    property    OnMouseMove;
    property    OnMouseScroll;
    property    OnMouseUp;
    property    OnPaint;
    property    OnShowHint;
  end;
  

  TlqGroupBox = class(TlqAbstractPanel)
  private
    FAlignment: TAlignment;
    FMargin: integer;
    FText: string;
    function    GetAlignment: TAlignment;
    procedure   SetAlignment(const AValue: TAlignment);
    function    GetText: string;
    procedure   SetText(const AValue: string);
    function    GetFontDesc: string;
    procedure   SetFontDesc(const AValue: string);
    function    GetMargin: integer;
    procedure   SetMargin(const AValue: integer);
  protected
    FFont: TlqFont;
    procedure   HandlePaint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    function    GetClientRect: TlqRect; override;
    property    Font: TlqFont read FFont;
  published
    property    AcceptDrops;
    property    Align;
    property    Alignment: TAlignment read GetAlignment write SetAlignment default taLeftJustify;
    property    BackgroundColor;
    property    BorderStyle;
    property    Enabled;
    property    FontDesc: string read GetFontDesc write SetFontDesc;
    property    Height;
    property    Hint;
    property    Left;
    property    Margin: integer read GetMargin write SetMargin default 2;
    property    MaxHeight;
    property    MaxWidth;
    property    MinHeight;
    property    MinWidth;
    property    ParentShowHint;
    property    ShowHint;
    property    Style;
    property    Text: string read GetText write SetText;
    property    TextColor;
    property    Top;
    property    Width;
    property    OnClick;
    property    OnDoubleClick;
    property    OnDragDrop;
    property    OnDragEnter;
    property    OnDragLeave;
    property    OnDragStartDetected;
    property    OnMouseDown;
    property    OnMouseMove;
    property    OnMouseScroll;
    property    OnMouseUp;
    property    OnPaint;
    property    OnShowHint;
  end;


  { A panel that could replace a TlqForm. Very handly for embedding
    "forms" inside other forms. You should also be able to design such
    frames with the UI designer too. }
  TlqFrame = class(TlqAbstractPanel)
  private
    FOnClose: TNotifyEvent;
    FOnShow: TNotifyEvent;
    FOnCreate: TNotifyEvent;
  protected
    WindowTitle: TlqString;
    procedure   HandleShow; override;
  public
    procedure   AfterConstruction; override;
    procedure   AfterCreate; virtual;
    procedure   Close;
    procedure   Show;
  published
    property    AcceptDrops;
    property    Align;
    property    BackgroundColor;
    property    BorderStyle;
    property    Enabled;
    property    Height;
    property    Hint;
    property    Left;
    property    MaxHeight;
    property    MaxWidth;
    property    MinHeight;
    property    MinWidth;
    property    ParentBackgroundColor;
    property    ParentShowHint;
    property    ShowHint;
    property    Style;
    property    Top;
    property    Width;
    property    OnClick;
    property    OnDoubleClick;
    property    OnDragDrop;
    property    OnDragEnter;
    property    OnDragLeave;
    property    OnDragStartDetected;
    property    OnClose: TNotifyEvent read FOnClose write FOnClose;
    property    OnCreate: TNotifyEvent read FOnCreate write FOnCreate;
    property    OnMouseDown;
    property    OnMouseMove;
    property    OnMouseUp;
    property    OnPaint;
    property    OnShow: TNotifyEvent read FOnShow write FOnShow;
    property    OnShowHint;
  end;

  TlqFrameClass = class of TlqFrame;


  TlqImagePanel = class(TlqWidget)
  private
    FImage: TlqImage;
    FOwnsImage: Boolean;
    FScaleImage: Boolean;
    procedure   SetImage(const AValue: TlqImage);
    procedure   SetScaleImage(const AValue: Boolean);
  protected
    procedure   HandlePaint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    property    Image: TlqImage read FImage write SetImage;
    property    OwnsImage: Boolean read FOwnsImage write FOwnsImage;
    property    ScaleImage: Boolean read FScaleImage write SetScaleImage;
  end;



function CreateBevel(AOwner: TComponent; ALeft, ATop, AWidth, AHeight: TlqCoord; AShape: TPanelShape;
         AStyle: TPanelStyle): TlqBevel;

function CreatePanel(AOwner: TComponent; ALeft, ATop, AWidth, AHeight: TlqCoord; AText: string;
         AStyle: TPanelStyle = bsRaised; AALignment: TAlignment= taCenter; ALayout: TLayout= tlCenter;
         AMargin: integer= 2; ALineSpace: integer= 2): TlqPanel;

function CreateGroupBox(AOwner: TComponent; ALeft, ATop, AWidth, AHeight: TlqCoord; AText: string;
         AStyle: TPanelStyle; AALignment: TAlignment= taCenter; AMargin: integer= 2): TlqGroupBox;


implementation

function CreateBevel(AOwner: TComponent; ALeft, ATop, AWidth, AHeight: TlqCoord; AShape: TPanelShape;
         AStyle: TPanelStyle): TlqBevel;
begin
  Result        := TlqBevel.Create(AOwner);
  Result.Left   := ALeft;
  Result.Top    := ATop;
  Result.Width  := AWidth;
  Result.Height := AHeight;
  Result.Shape  := AShape;
  Result.Style  := AStyle;
end;

function CreatePanel(AOwner: TComponent; ALeft, ATop, AWidth, AHeight: TlqCoord; AText: string;
         AStyle: TPanelStyle; AALignment: TAlignment= taCenter; ALayout: TLayout= tlCenter;
         AMargin: integer= 2; ALineSpace: integer= 2): TlqPanel;
begin
  Result          := TlqPanel.Create(AOwner);
  Result.Left     := ALeft;
  Result.Top      := ATop;
  Result.Width    := AWidth;
  Result.Height   := AHeight;
  Result.FText    := AText;
  Result.Style    := AStyle;
  Result.FAlignment:= AAlignment;
  Result.FLayout   := ALayout;
  Result.FMargin   := AMargin;
  Result.FLineSpace:= ALineSpace;
end;

function CreateGroupBox(AOwner: TComponent; ALeft, ATop, AWidth, AHeight: TlqCoord; AText: string;
         AStyle: TPanelStyle; AALignment: TAlignment= taCenter; AMargin: integer= 2): TlqGroupBox;
begin
  Result            := TlqGroupBox.Create(AOwner);
  Result.Left       := ALeft;
  Result.Top        := ATop;
  Result.Width      := AWidth;
  Result.Height     := AHeight;
  Result.FText      := AText;
  Result.Style      := AStyle;
  Result.FAlignment := AAlignment;
  Result.FMargin    := AMargin;
end;

{ TlqFrame }

procedure TlqFrame.HandleShow;
begin
  inherited HandleShow;
  HandleAlignments(0, 0);
  if Assigned(FOnShow) then
    FOnShow(self);
end;

procedure TlqFrame.AfterConstruction;
begin
  AfterCreate;
  inherited AfterConstruction;
  if Assigned(FOnCreate) then
    FOnCreate(self);
end;

procedure TlqFrame.AfterCreate;
begin
  // do nothing here
end;

procedure TlqFrame.Close;
begin
  HandleHide;
  if Assigned(FOnClose) then
    FOnClose(self);
end;

procedure TlqFrame.Show;
begin
  HandleShow;
end;

{TlqAbstractPanel}

function TlqAbstractPanel.GetClientRect: TlqRect;
begin
  Result.SetRect(2, 2, Width - 4, Height - 4);
end;

procedure TlqAbstractPanel.SetPanelStyle(const AValue: TPanelStyle);
begin
  if FPanelStyle <> AValue then
  begin
    FPanelStyle := AValue;
    Repaint;
  end;
end;

procedure TlqAbstractPanel.SetPanelBorder(const AValue: TPanelBorder);
begin
  if FPanelBorder <> AValue then
  begin
    FPanelBorder := AValue;
    Repaint;
  end;
end;

procedure TlqAbstractPanel.SetParentBackgroundColor(const AValue: Boolean);
begin
  if FParentBackgroundColor = AValue then exit;
  FParentBackgroundColor := AValue;
  RePaint;
end;

procedure TlqAbstractPanel.HandlePaint;
begin
  inherited HandlePaint;
  if FParentBackgroundColor then
    Canvas.Clear(Parent.BackgroundColor)
  else
    Canvas.Clear(BackgroundColor);
end;

constructor TlqAbstractPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPanelStyle      := bsRaised;
  FPanelBorder     := bsSingle;
  Width            := 80;
  Height           := 80;
  FFocusable       := True;  // otherwise children can't get focus
  FParentBackgroundColor := False;
  FIsContainer     := True;
end;

{TlqBevel}

procedure TlqBevel.SetPanelShape(const AValue: TPanelShape);
begin
  if FPanelShape <> AValue then
  begin
    FPanelShape := AValue;
    Repaint;
  end;
end;

procedure TlqBevel.DrawBox;
begin
  if FPanelBorder = bsSingle then
    Canvas.SetLineStyle(1, lsSolid)
  else
    Canvas.SetLineStyle(2, lsSolid);

  { top }
  if FPanelBorder = bsSingle then
    Canvas.DrawLine(0, 0, Width - 1, 0)
  else
    Canvas.DrawLine(0, 1, Width - 1, 1);

  { left }
  if FPanelBorder = bsSingle then
    Canvas.DrawLine(0, 1, 0, Height - 1)
  else
    Canvas.DrawLine(1, 1, 1, Height - 1);

  if Style = bsRaised then
    Canvas.SetColor(clShadow1)
  else
    Canvas.SetColor(clHilite2);

  { right, then bottom }
  Canvas.DrawLine(Width - 1, 0, Width - 1, Height - 1);
  Canvas.DrawLine(0, Height - 1, Width, Height - 1);
end;

procedure TlqBevel.DrawFrame;
begin
  Canvas.SetLineStyle(1, lsSolid);

  Canvas.DrawLine(0, 0, Width - 1, 0);
  Canvas.DrawLine(0, 1, 0, Height - 1);
  Canvas.DrawLine(Width - 2, 1, Width - 2, Height - 1);
  Canvas.DrawLine(1, Height - 2, Width - 1, Height - 2);

  if Style = bsRaised then
    Canvas.SetColor(clShadow2)
  else
    Canvas.SetColor(clHilite2);

  Canvas.DrawLine(1, 1, Width - 2, 1);
  Canvas.DrawLine(1, 2, 1, Height - 2);
  Canvas.DrawLine(Width - 1, 0, Width - 1, Height - 1);
  Canvas.DrawLine(0, Height - 1, Width, Height - 1);
end;

procedure TlqBevel.DrawTopLine;
begin
  Canvas.SetLineStyle(1, lsSolid);
  Canvas.DrawLine(0, 0, Width, 0);

  if Style = bsRaised then
    Canvas.SetColor(clShadow2)
  else
    Canvas.SetColor(clHilite2);

  Canvas.DrawLine(0, 1, Width, 1);
end;

procedure TlqBevel.DrawBottomLine;
begin
  Canvas.SetLineStyle(1, lsSolid);
  Canvas.DrawLine(0, Height - 2, Width, Height - 2);

  if Style = bsRaised then
    Canvas.SetColor(clShadow2)
  else
    Canvas.SetColor(clHilite2);

  Canvas.DrawLine(0, Height - 1, Width, Height - 1);
end;

procedure TlqBevel.DrawLeftLine;
begin
  Canvas.SetLineStyle(1, lsSolid);
  Canvas.DrawLine(0, 1, 0, Height - 1);

  if Style = bsRaised then
    Canvas.SetColor(clShadow2)
  else
    Canvas.SetColor(clHilite2);

  Canvas.DrawLine(1, 1, 1, Height - 1);
end;

procedure TlqBevel.DrawRightLine;
begin
  Canvas.SetLineStyle(1, lsSolid);
  Canvas.DrawLine(Width - 2, 0, Width - 2, Height - 1);

  if Style = bsRaised then
    Canvas.SetColor(clShadow2)
  else
    Canvas.SetColor(clHilite2);

  Canvas.DrawLine(Width - 1, 0, Width - 1, Height - 1);
end;

procedure TlqBevel.DrawSpacer;
begin
  // To make it more visible in the UI Designer
  if csDesigning in ComponentState then
  begin
    Canvas.SetColor(clInactiveWgFrame);
    Canvas.SetLineStyle(1, lsDash);
    Canvas.DrawRectangle(0, 0, Width, Height);
  end;
end;

procedure TlqBevel.DrawVerDivider;

  procedure PaintLine(px, py: integer);
  begin
    if Style = bsRaised then
      Canvas.SetColor(clHilite2)
    else
      Canvas.SetColor(clShadow1);

    Canvas.DrawLine(px, py, px+2, py);
    Canvas.DrawLine(px, py, px, Height);

    if Style = bsRaised then
      Canvas.SetColor(clShadow1)
    else
      Canvas.SetColor(clHilite2);

    Canvas.DrawLine(px+1, Height - 1, px+3, Height - 1);
    Canvas.DrawLine(px+2, py, px+2, Height);
  end;

begin
  PaintLine(0, 0);
  if FPanelBorder = bsDouble then
    PaintLine(3, 0);
end;

procedure TlqBevel.HandlePaint;
begin
  inherited HandlePaint;

  if Style = bsRaised then
    Canvas.SetColor(clHilite2)
  else
    Canvas.SetColor(clShadow1);

  case Shape of
    bsBox:          DrawBox;
    bsFrame:        DrawFrame;
    bsTopLine:      DrawTopLine;
    bsBottomLine:   DrawBottomLine;
    bsLeftLine:     DrawLeftLine;
    bsRightLine:    DrawRightLine;
    bsSpacer:       DrawSpacer;
    bsVerDivider:   DrawVerDivider;
  end;
end;

constructor TlqBevel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPanelShape := bsBox;
end;

{TlqPanel}

function TlqPanel.GetAlignment: TAlignment;
begin
  Result := FAlignment;
end;

procedure TlqPanel.SetAlignment(const AValue: TAlignment);
begin
  if FAlignment <> AValue then
  begin
    FAlignment := AValue;
    Repaint;
  end;
end;

function TlqPanel.GetLayout: TLayout;
begin
  Result := FLayout;
end;

procedure TlqPanel.SetLayout(const AValue: TLayout);
begin
  if FLayout <> AValue then
  begin
    FLayout := AValue;
    Repaint;
  end;
end;

function TlqPanel.GetText: string;
begin
  Result := FText;
end;

procedure TlqPanel.SetText(const AValue: string);
begin
  if FText <> AValue then
  begin
    FText := AValue;
    Repaint;
  end;
end;

function TlqPanel.GetFontDesc: string;
begin
  Result := FFont.FontDesc;
end;

procedure TlqPanel.SetFontDesc(const AValue: string);
begin
  FFont.Free;
  FFont := fpgGetFont(AValue);
  Repaint;
end;

function TlqPanel.GetLineSpace: integer;
begin
  Result := FLineSpace;
end;

procedure TlqPanel.SetLineSpace(const AValue: integer);
begin
  if FLineSpace <> AValue then
  begin
    FLineSpace := AValue;
    Repaint;
  end;
end;

function TlqPanel.GetMargin: integer;
begin
  Result := FMargin;
end;

procedure TlqPanel.SetMargin(const AValue: integer);
begin
  if FMargin <> AValue then
  begin
    FMargin := AValue;
    Repaint;
  end;
end;

function Tlqpanel.GetWrapText: boolean;
begin
  Result := FWrapText;
end;

procedure Tlqpanel.SetWrapText(const AValue: boolean);
begin
  if FWrapText <> AValue then
  begin
    FWrapText := AValue;
    Repaint;
  end;
end;

procedure TlqPanel.HandlePaint;
var
  lTxtFlags: TlqTextFlags;
begin
  inherited HandlePaint;

  if Style <> bsFlat then
  begin
    if FPanelBorder = bsSingle then
      Canvas.SetLineStyle(1, lsSolid)
    else
      Canvas.SetLineStyle(2, lsSolid);

    if Style = bsRaised then
      Canvas.SetColor(clHilite2)
    else
      Canvas.SetColor(clShadow1);

    if FPanelBorder = bsSingle then
    begin
      Canvas.DrawLine(0, 0, Width - 1, 0);
      Canvas.DrawLine(0, 1, 0, Height - 1);
    end
    else
    begin
      Canvas.DrawLine(0, 1, Width - 1, 1);
      Canvas.DrawLine(1, 1, 1, Height - 1);
    end;

    if Style = bsRaised then
      Canvas.SetColor(clShadow1)
    else
      Canvas.SetColor(clHilite2);

    Canvas.DrawLine(Width - 1, 0, Width - 1, Height - 1);
    Canvas.DrawLine(0, Height - 1, Width, Height - 1);
  end;

  Canvas.SetTextColor(FTextColor);
  Canvas.SetFont(Font);

  lTxtFlags:= [];
  if not Enabled then
    Include(lTxtFlags, txtDisabled);

  if FWrapText then
    Include(lTxtFlags, txtWrap);

  case FAlignment of
    taLeftJustify:
      Include(lTxtFlags, txtLeft);
    taRightJustify:
      Include(lTxtFlags, txtRight);
    taCenter:
      Include(lTxtFlags, txtHCenter);
  end;

  case FLayout of
    tlTop:
      Include(lTxtFlags, txtTop);
    tlBottom:
      Include(lTxtFlags, txtBottom);
    tlCenter:
      Include(lTxtFlags, txtVCenter);
  end;

  Canvas.DrawText(FMargin, FMargin, Width - FMargin * 2, Height - FMargin * 2, FText, lTxtFlags, FLineSpace);
end;

constructor TlqPanel.Create(Aowner: TComponent);
begin
  inherited Create(AOwner);
  FText             := 'Panel';
  FFont             := fpgGetFont('#Label1');
  FPanelStyle       := bsRaised;
  FWidth            := 80;
  FHeight           := 80;
  FAlignment        := taCenter;
  FLayout           := tlCenter;
  FWrapText         := False;
  FLineSpace        := 2;
  FMargin           := 2;
end;

destructor TlqPanel.Destroy;
begin
  FText := '';
  FFont.Free;
  inherited Destroy;
end;

function TlqPanel.GetClientRect: TlqRect;
begin
  if Style = bsFlat then
    Result.SetRect(0, 0, Width, Height)
  else
    Result := inherited GetClientRect;
end;

{TlqGroupBox}

function TlqGroupBox.GetAlignment: TAlignment;
begin
  Result := FAlignment;
end;

procedure TlqGroupBox.SetAlignment(const AValue: TAlignment);
begin
  if FAlignment <> AValue then
  begin
    FAlignment := AValue;
    Repaint;
  end;
end;

function TlqGroupBox.GetText: string;
begin
  Result := FText;
end;

procedure TlqGroupBox.SetText(const AValue: string);
begin
  if FText <> AValue then
  begin
    FText := AValue;
    Repaint;
  end;
end;

function TlqGroupBox.GetFontDesc: string;
begin
  Result := FFont.FontDesc;
end;

procedure TlqGroupBox.SetFontDesc(const AValue: string);
begin
  FFont.Free;
  FFont := fpgGetFont(AValue);
  Repaint;
end;

function TlqGroupBox.GetMargin: integer;
begin
  Result := FMargin;
end;

procedure TlqGroupBox.SetMargin(const AValue: integer);
begin
  if FMargin <> AValue then
  begin
    FMargin := AValue;
    Repaint;
  end;
end;

function TlqGroupBox.GetClientRect: TlqRect;
var
  h: integer;
begin
  h := FFont.Height + 4;
  Result.SetRect(2, h, Width - 4, Height - (h + 2));
end;

procedure TlqGroupBox.HandlePaint;
var
  r: TlqRect;
  w: integer;
  lTxtFlags: TlqTextFlags;
begin
  inherited HandlePaint;

  Canvas.Clear(Parent.BackgroundColor);
  Canvas.ClearClipRect;
  r.SetRect(0, 5, Width, Height);
  Canvas.SetClipRect(r);
  Canvas.Clear(FBackgroundColor);
  
  lTxtFlags := TextFlagsDflt;
  if not Enabled then
    Include(lTxtFlags, txtDisabled);

//  Canvas.ClearClipRect;

  //  Canvas.SetLineStyle(2, lsSolid);
  //  Canvas.SetColor(clWindowBackground);
  //  Canvas.DrawRectangle(1, 1, Width - 1, Height - 1);
  if FPanelBorder = bsSingle then
    Canvas.SetLineStyle(1, lsSolid)
  else
    Canvas.SetLineStyle(2, lsSolid);

  if Style = bsRaised then
    Canvas.SetColor(clHilite2)
  else
    Canvas.SetColor(clShadow2);

  if FPanelBorder = bsSingle then
  begin
    Canvas.DrawLine(0, 5, Width - 1, 5);
    Canvas.DrawLine(0, 6, 0, Height - 1);
  end
  else
  begin
    Canvas.DrawLine(0, 6, Width - 1, 6);
    Canvas.DrawLine(1, 6, 1, Height - 1);
  end;

  if Style = bsRaised then
    Canvas.SetColor(clShadow2)
  else
    Canvas.SetColor(clHilite2);

  Canvas.DrawLine(Width - 1, 5, Width - 1, Height - 1);
  Canvas.DrawLine(0, Height - 1, Width, Height - 1);

  Canvas.SetTextColor(FTextColor);
  Canvas.SetFont(Font);
  
  case FAlignment of
    taLeftJustify:
      begin
        w := FFont.TextWidth(FText) + FMargin * 2;
        r.SetRect(5, 0, w, FFont.Height + FMargin);
        Canvas.SetClipRect(r);
        Canvas.Clear(FBackgroundColor);

        if Style = bsRaised then
          Canvas.SetColor(clHilite2)
        else
          Canvas.SetColor(clShadow2);

        if FPanelBorder = bsSingle then
        begin
          Canvas.DrawLine(5, 0, w + 5, 0);
          Canvas.DrawLine(5, 0, 5, 6);
        end
        else
        begin
          Canvas.DrawLine(5, 1, w + 5, 1);
          Canvas.DrawLine(6, 0, 6, 7);
        end;

        if Style = bsRaised then
          Canvas.SetColor(clShadow2)
        else
          Canvas.SetColor(clHilite2);

        Canvas.DrawLine(w + 5, 0, w + 5, 6);
        Canvas.DrawText(FMargin + 5, 0, FText, lTxtFlags);
      end;
    taRightJustify:
      begin
        w := Width - FFont.TextWidth(FText) - (FMargin * 2) - 5;
        r.SetRect(w, 0, FFont.TextWidth(FText) + FMargin * 2, FFont.Height + FMargin);
        Canvas.SetClipRect(r);
        Canvas.Clear(FBackgroundColor);

        if Style = bsRaised then
          Canvas.SetColor(clHilite2)
        else
          Canvas.SetColor(clShadow2);

        if FPanelBorder = bsSingle then
        begin
          Canvas.DrawLine(w, 0, Width - 5, 0);
          Canvas.DrawLine(w, 0, w, 6);
        end
        else
        begin
          Canvas.DrawLine(w, 1, Width - 5, 1);
          Canvas.DrawLine(w + 1, 0, w + 1, 7);
        end;

        if Style = bsRaised then
          Canvas.SetColor(clShadow2)
        else
          Canvas.SetColor(clHilite2);

        Canvas.DrawLine(Width - 6, 0, Width - 6, 6);
        Canvas.DrawText(Width - FFont.TextWidth(FText) - FMargin - 5, 0, FText, lTxtFlags);
      end;
    taCenter:
      begin
        w := (Width - FFont.TextWidth(FText) - FMargin * 2) div 2;
        r.SetRect(w, 0, FFont.TextWidth(FText) + FMargin * 2, FFont.Height + FMargin);
        Canvas.SetClipRect(r);
        Canvas.Clear(FBackgroundColor);

        if Style = bsRaised then
          Canvas.SetColor(clHilite2)
        else
          Canvas.SetColor(clShadow2);

        if FPanelBorder = bsSingle then
        begin
          Canvas.DrawLine(w, 0, w + FFont.TextWidth(FText) + FMargin * 2, 0);
          Canvas.DrawLine(w, 0, w, 6);
        end
        else
        begin
          Canvas.DrawLine(w, 1, w + FFont.TextWidth(FText) + FMargin * 2, 1);
          Canvas.DrawLine(w + 1, 0, w + 1, 7);
        end;

        if Style = bsRaised then
          Canvas.SetColor(clShadow2)
        else
          Canvas.SetColor(clHilite2);

        Canvas.DrawLine(w + FFont.TextWidth(FText) + FMargin * 2 - 1, 0, w + FFont.TextWidth(FText) + FMargin * 2 - 1, 6);
        Canvas.DrawText(w + FMargin, 0, FText, lTxtFlags);
      end;
    end;
end;

constructor TlqGroupBox.Create(Aowner: TComponent);
begin
  inherited Create(AOwner);
  FText             := 'Group box';
  FFont             := fpgGetFont('#Label1');
  FPanelStyle       := bsRaised;
  FWidth            := 80;
  FHeight           := 80;
  FFocusable        := True;  // otherwise children can't get focus
  FBackgroundColor  := Parent.BackgroundColor;
  FAlignment        := taLeftJustify;
  FMargin           := 2;
end;

destructor TlqGroupBox.Destroy;
begin
  FFont.Free;
  inherited Destroy;
end;

{ TlqImagePanel }

procedure TlqImagePanel.SetImage(const AValue: TlqImage);
begin
  if FOwnsImage and Assigned(FImage) then
    FImage.Free;
  FImage := AValue;
  Repaint;
end;

procedure TlqImagePanel.SetScaleImage(const AValue: Boolean);
begin
  if FScaleImage = AValue then
    Exit;
  FScaleImage := AValue;
  if Assigned(FImage) then
    Repaint;
end;

procedure TlqImagePanel.HandlePaint;
var
  x: integer;
  y: integer;
begin
  inherited HandlePaint;
  Canvas.Clear(BackgroundColor);
  if Assigned(FImage) then
  begin
    x := (Width - FImage.Width) div 2;
    y := (Height - FImage.Height) div 2;
    if ScaleImage then
      Canvas.StretchDraw(0, 0, Width, Height, FImage)
    else
      Canvas.DrawImage(x, y, FImage);
  end;
end;

constructor TlqImagePanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FImage := nil;
  FOwnsImage := False;
end;

destructor TlqImagePanel.Destroy;
begin
  if FOwnsImage then
    FImage.Free;
  inherited Destroy;
end;



end.

