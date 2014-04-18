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
      This unit implements a class that provides an icon for an application
      in the system tray.
}
unit lq_trayicon;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  lq_base,
  lq_main,
  lq_widget,
  lq_menu,
  lq_interface;

type

  TlqMessageIconType = (mitNoIcon, mitInformation, mitWarning, mitCritical);


  TlqSystemTrayIcon = class(TlqWidget)
  private
    FPopupMenu: TlqPopupMenu;
    FImageName: TlqString;
    FImage: TlqImage;
    FOnMessageClicked: TNotifyEvent;
    procedure   SetImageName(AValue: TlqString);
  protected
    FSysTrayHandler: TlqSystemTrayHandler;
    procedure   SetVisible(const AValue: boolean); override;
    procedure   HandlePaint; override;
    procedure   HandleRMouseUp(x, y: integer; shiftstate: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    function    IsSystemTrayAvailable: boolean;
    function    SupportsMessages: boolean;
    procedure   Show;
    procedure   Hide;
    procedure   ShowMessage(const ATitle: TlqString; const AMessage: TlqString; const AMessageIcon: TlqMessageIconType; const AMillisecondsTimeoutHint: Word = 10000);
  published
    property    BackgroundColor;
    property    Hint;
    property    ImageName: TlqString read FImageName write SetImageName;
    property    PopupMenu: TlqPopupMenu read FPopupMenu write FPopupMenu;
    property    ShowHint;
    property    OnClick;
    property    OnMessageClicked: TNotifyEvent read FOnMessageClicked write FOnMessageClicked;
    property    OnPaint;
  end;


implementation

{ TlqSystemTrayIcon }

procedure TlqSystemTrayIcon.SetImageName(AValue: TlqString);
begin
  if FImageName = AValue then
    Exit;
  FImageName := AValue;
  Repaint;
end;

procedure TlqSystemTrayIcon.SetVisible(const AValue: boolean);
begin
  if FVisible = AValue then
    Exit;
  FVisible := AValue;
  if FVisible then
    Show
  else
    Hide;
end;

procedure TlqSystemTrayIcon.HandlePaint;
var
  img: TlqImage;
begin
  inherited HandlePaint;
  Canvas.Clear(FBackgroundColor);
  if FImageName <> '' then
  begin
    { request a reference to already loaded image }
    img := lqImages.GetImage(FImageName);
    if Assigned(img) then
    begin
      FCanvas.DrawImage((Width-img.Width) div 2, (Height-img.Height) div 2, img);
    end;
  end;
end;

procedure TlqSystemTrayIcon.HandleRMouseUp(x, y: integer; shiftstate: TShiftState);
begin
  inherited HandleRMouseUp(x, y, shiftstate);
  if Assigned(FPopupMenu) then
  begin
    FPopupMenu.ShowAt(self, x, y, True);
  end;
end;

constructor TlqSystemTrayIcon.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWidth := 20;
  FHeight := 20;
  FVisible := False;

  FHint := '';
  FImage := nil;
  FImageName := '';
  FPopupMenu := nil;

  FSysTrayHandler := TlqSystemTrayHandler.Create(self);
end;

function TlqSystemTrayIcon.IsSystemTrayAvailable: boolean;
begin
  Result := FSysTrayHandler.IsSystemTrayAvailable;
end;

function TlqSystemTrayIcon.SupportsMessages: boolean;
begin
  { TODO : As far as I know Mac OS X doesn't support this, though they do now
    have this Notifications functionility since 10.8 }
  Result := FSysTrayHandler.SupportsMessages;
end;

procedure TlqSystemTrayIcon.Show;
begin
  FSysTrayHandler.Show;
  FVisible := True;
end;

procedure TlqSystemTrayIcon.Hide;
begin
  { TODO : TlqSystemTrayIcon.Hide not implemented yet! }
//  FVisible := False;
//  FSysTrayHandler.Hide;
end;

procedure TlqSystemTrayIcon.ShowMessage(const ATitle: TlqString;
  const AMessage: TlqString; const AMessageIcon: TlqMessageIconType;
  const AMillisecondsTimeoutHint: Word);
begin
  { TODO : TlqSystemTrayIcon.ShowMessage not implemented yet! }
end;

end.

