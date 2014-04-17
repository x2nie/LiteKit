{
  A very quick and basic style implementation. It took all of 10 minutes.
  To apply this style, follow these instructions:

    1) (optional) Check if a style was specified via a command line parameter
    2) If (1) was false, set the new default which will instantiate the new
       style class and automatically free the old one.
    3) Assign our new style instance to the fpgStyle variable


  Example:

    procedure MainProc;
    var
      frm: TMainForm;
    begin
      fpgApplication.Initialize;

      { Set our new style as the default (before we create any forms), unless
        a the end-user specified a different style via the command line. }
      if not gCommandLineParams.IsParam('style') then
        if fpgStyleManager.SetStyle('Demo Style') then
          fpgStyle := fpgStyleManager.Style;

      frm := TMainForm.Create(nil);
      try
        frm.Show;
        fpgApplication.Run;
      finally
        frm.Free;
      end;
    end;

}
unit mystyle;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lq_main, lq_base;

type

  TMyStyle = class(TlqStyle)
  public
    constructor Create; override;
    { General }
    procedure   DrawControlFrame(ACanvas: TlqCanvas; x, y, w, h: TlqCoord); override;
    { Buttons }
    procedure   DrawButtonFace(ACanvas: TlqCanvas; x, y, w, h: TlqCoord; AFlags: TlqButtonFlags); override;
    { Menus }
    procedure   DrawMenuRow(ACanvas: TlqCanvas; r: TlqRect; AFlags: TlqMenuItemFlags); override;
    procedure   DrawMenuBar(ACanvas: TlqCanvas; r: TlqRect; ABackgroundColor: TlqColor); override;
  end;


implementation

uses
  lq_stylemanager
  ;

{ TMyStyle }

constructor TMyStyle.Create;
begin
  inherited Create;
  fpgSetNamedColor(clWindowBackground, TlqColor($eeeeec));
end;

procedure TMyStyle.DrawControlFrame(ACanvas: TlqCanvas; x, y, w, h: TlqCoord);
var
  r: TlqRect;
begin
  r.SetRect(x, y, w, h);
  ACanvas.SetColor(clShadow1);
  ACanvas.Clear(clYellow);
  ACanvas.DrawRectangle(r);
end;

procedure TMyStyle.DrawButtonFace(ACanvas: TlqCanvas; x, y, w, h: TlqCoord; AFlags: TlqButtonFlags);
var
  r: TlqRect;
begin
  r.SetRect(x, y, w, h);

  if btfIsDefault in AFlags then
  begin
    ACanvas.SetColor(TlqColor($7b7b7b));
    ACanvas.SetLineStyle(1, lsSolid);
    ACanvas.DrawRectangle(r);
    InflateRect(r, -1, -1);
    Exclude(AFlags, btfIsDefault);
    fpgStyle.DrawButtonFace(ACanvas, r.Left, r.Top, r.Width, r.Height, AFlags);
    Exit; //==>
  end;

  // Clear the canvas
  ACanvas.SetColor(clWindowBackground);
  ACanvas.FillRectangle(r);

  if (btfFlat in AFlags) and not (btfIsPressed in AFlags) then
    Exit; // no need to go further

  // outer rectangle
  ACanvas.SetLineStyle(1, lsSolid);
  ACanvas.SetColor(TlqColor($a6a6a6));
  ACanvas.DrawRectangle(r);

  // so we don't paint over the border
  InflateRect(r, -1, -1);
  // now paint the face of the button
  if (btfIsPressed in AFlags) then
  begin
    ACanvas.GradientFill(r, TlqColor($cccccc), TlqColor($e4e4e4), gdVertical);
  end
  else
  begin
    ACanvas.GradientFill(r, TlqColor($fafafa), TlqColor($e2e2e2), gdVertical);
    ACanvas.SetColor(TlqColor($cccccc));
    ACanvas.DrawLine(r.Right, r.Top, r.Right, r.Bottom);   // right
    ACanvas.DrawLine(r.Right, r.Bottom, r.Left, r.Bottom);   // bottom
  end;
end;

procedure TMyStyle.DrawMenuRow(ACanvas: TlqCanvas; r: TlqRect; AFlags: TlqMenuItemFlags);
begin
  inherited DrawMenuRow(ACanvas, r, AFlags);
  if (mifSelected in AFlags) and not (mifSeparator in AFlags) then
    ACanvas.GradientFill(r, TlqColor($fec475), TlqColor($fb9d24), gdVertical);
end;

procedure TMyStyle.DrawMenuBar(ACanvas: TlqCanvas; r: TlqRect; ABackgroundColor: TlqColor);
var
  FLightColor: TlqColor;
  FDarkColor: TlqColor;
begin
  // a possible future theme option
  FLightColor := TlqColor($f0ece3);  // color at top of menu bar
  FDarkColor  := TlqColor($beb8a4);  // color at bottom of menu bar
  ACanvas.GradientFill(r, FLightColor, FDarkColor, gdVertical);

  // inner bottom line
  ACanvas.SetColor(clShadow1);
  ACanvas.DrawLine(r.Left, r.Bottom-1, r.Right+1, r.Bottom-1);   // bottom
  // outer bottom line
  ACanvas.SetColor(clWhite);
  ACanvas.DrawLine(r.Left, r.Bottom, r.Right+1, r.Bottom);   // bottom
end;


initialization
  fpgStyleManager.RegisterClass('Demo Style', TMyStyle);

end.

