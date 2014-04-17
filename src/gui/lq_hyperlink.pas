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
      A hyperlink label component. When the user clicks the label, a
      web browser is opened with the URL specified.
}


unit lq_hyperlink;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  Sysutils,
  lq_base,
  lq_main,
  lq_label;

type

  TlqHyperlink = class(TlqCustomLabel)
  private
    FHotTrackColor: TlqColor;
    FOldColor: TlqColor;
    FOldFont: TlqString;
    FHTFont: TlqString;
    FUrl: TlqString;
    procedure   SetHotTrackColor(const AValue: TlqColor);
    procedure   SetHotTrackFont(const AValue: TlqString);
    procedure   SetURL(const Value: TlqString);
  protected
    procedure   HandleMouseEnter; override;
    procedure   HandleMouseExit; override;
    procedure   HandleLMouseDown(x, y: integer; shiftstate: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure   GoHyperLink;
  published
    property    Align;
    property    Alignment;
    property    Autosize;
    property    BackgroundColor;
    property    Enabled;
    property    FontDesc;
    property    Height;
    property    Hint;
    property    HotTrackColor: TlqColor read FHotTrackColor write SetHotTrackColor default clBlue;
    property    HotTrackFont: TlqString read FHTFont write SetHotTrackFont;
    property    Layout;
    property    Left;
    property    Parent;
    property    ParentShowHint;
    property    ShowHint;
    property    Text;
    property    TextColor default clBlue;
    property    URL: TlqString read FUrl write SetURL;
    property    Top;
    property    Width;
    property    WrapText;
    property    OnClick;
    property    OnShowHint;
end;



implementation

uses
  lq_utils;


{ TlqHyperlink }

constructor TlqHyperlink.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width           := 120;
  FHotTrackColor  := clBlue;
  TextColor       := clBlue;
  FUrl            := 'http://opensoft.homeip.net/fpgui/';
  FText           := 'fpGUI website';
  FHTFont         := 'Arial-8:antialias=true:underline:bold';
  FontDesc        := 'Arial-8:antialias=true:underline';
end;

procedure TlqHyperlink.SetURL(const Value: TlqString);
begin
  if FUrl <> Value then
    FUrl := Value;
end;

procedure TlqHyperlink.SetHotTrackFont(const AValue: TlqString);
begin
  if FHTFont = AValue then
    Exit;
  FHTFont := AValue;
end;

procedure TlqHyperlink.SetHotTrackColor(const AValue: TlqColor);
begin
  if FHotTrackColor = AValue then
    Exit;
  FHotTrackColor := AValue;
end;

procedure TlqHyperlink.GoHyperLink;
begin
  if URL <> '' then
    fpgOpenURL(URL);
end;

procedure TlqHyperlink.HandleMouseEnter;
begin
  inherited HandleMouseEnter;
  FOldColor   := TextColor;
  TextColor   := FHotTrackColor;
  FOldFont    := FontDesc;
  FontDesc    := FHTFont;
  MouseCursor := mcHand;
end;

procedure TlqHyperlink.HandleMouseExit;
begin
  inherited HandleMouseExit;
  TextColor   := FOldColor;
  MouseCursor := mcDefault;
  FontDesc    := FOldFont;
end;

procedure TlqHyperlink.HandleLMouseDown(x, y: integer; shiftstate: TShiftState);
begin
  inherited HandleLMouseDown(x, y, shiftstate);
  if not Assigned(OnClick) then
    GoHyperlink;
end;


end.

