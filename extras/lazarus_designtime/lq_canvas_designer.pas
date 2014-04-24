unit lq_canvas_designer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  lq_base, lq_main,
  Forms, Controls, Graphics;

type

  { TlqGDICanvas }

  TlqLazCanvas = class(TlqCanvasBase)
  private
    FDrawing: boolean;
    FBufferBitmap: TBitmap;
    FDrawWindow: TlqWindow;  //Widget to draw to
    {Fgc: TlqDCHandle;     //Graphic Context for canvas drawing operation
    FBufgc: TlqDCHandle;  //Buffer, storage GC
    FWinGC: TlqDCHandle;  //Windowed GC }
    FBackgroundColor: TlqColor;
    FCurFontRes: TlqFontResource;
    FClipRect: TlqRect;
    FClipRectSet: Boolean;
    FWindowsColor: longword;
    //FBrush: HBRUSH;
    //FPen: HPEN;
    //FClipRegion: HRGN;
    FIntLineStyle: integer;
    FBufWidth: Integer;
    FBufHeight: Integer;
    function GetBufferCanvas: TCanvas;
    procedure   TryFreeBackBuffer;
    function GetBufferBitmap: TBitmap;
  protected
    procedure   DoSetFontRes(fntres: TlqFontResourceBase); override;
    procedure   DoSetTextColor(cl: TlqColor); override;
    procedure   DoSetColor(cl: TlqColor); override;
    procedure   DoSetLineStyle(awidth: integer; astyle: TlqLineStyle); override;
    procedure   DoGetWinRect(out r: TlqRect); override;
    procedure   DoFillRectangle(x, y, w, h: TlqCoord); override;
    procedure   DoXORFillRectangle(col: TlqColor; x, y, w, h: TlqCoord); override;
    procedure   DoFillTriangle(x1, y1, x2, y2, x3, y3: TlqCoord); override;
    procedure   DoDrawRectangle(x, y, w, h: TlqCoord); override;
    procedure   DoDrawLine(x1, y1, x2, y2: TlqCoord); override;
    procedure   DoDrawImagePart(x, y: TlqCoord; img: TlqImageBase; xi, yi, w, h: integer); override;
    procedure   DoDrawString(x, y: TlqCoord; const txt: string); override;
    procedure   DoSetClipRect(const ARect: TlqRect); override;
    function    DoGetClipRect: TlqRect; override;
    procedure   DoAddClipRect(const ARect: TlqRect); override;
    procedure   DoClearClipRect; override;
    procedure   DoBeginDraw(AWin: TlqWindowBase; ABuffered: boolean); override;
    procedure   DoPutBufferToScreen(x, y, w, h: TlqCoord); override;
    procedure   DoEndDraw; override;
    function    GetPixel(X, Y: integer): TlqColor; override;
    procedure   SetPixel(X, Y: integer; const AValue: TlqColor); override;
    procedure   DoDrawArc(x, y, w, h: TlqCoord; a1, a2: Extended); override;
    procedure   DoFillArc(x, y, w, h: TlqCoord; a1, a2: Extended); override;
    procedure   DoDrawPolygon(Points: PPoint; NumPts: Integer; Winding: boolean = False); override;
    //property    DCHandle: TlqDCHandle read Fgc;

  public

    constructor Create(AWin: TlqWindowBase); override;
    destructor  Destroy; override;
    property    Canvas : TCanvas read GetBufferCanvas;
    property    Bitmap : TBitmap read GetBufferBitmap;     
  end;

implementation

uses math, lq_stringutils;

{ misc }
function lqColorToWin(col: TlqColor): TColor;
var
  c: dword;
begin
  c      := lqColorToRGB(col);
  //swapping bytes (Red and Blue colors)
  Result := ((c and $FF0000) shr 16) or (c and $00FF00) or ((c and $0000FF) shl 16);
end;

function WinColorTofpgColor(col: longword): TlqColor;
var
  t: TRGBTriple;
begin
  { Windown Color is BBGGRR format }
  t.Blue := (col and $FF0000) shr 16;
  t.Green := (col and $00FF00) shr 8;
  t.Red := (col and $0000FF);
  t.Alpha := $FF;

  Result := RGBTripleTofpgColor(t);
end;

{ Use CenterPoint to get the Center-Point of any rectangle. It is primarily
  for use with, and in, other routines such as Quadrant, and RadialPoint. }
function CenterPoint(Rect: TRect): TPoint;
var
  Tmp:  Longint;
begin
  with Rect do
  begin
    if Right < Left then
    begin
      Tmp   := Right;
      Right := Left;
      Left  := Tmp;
    end;

    if Bottom < Top then
    begin
      Tmp    := Bottom;
      Bottom := Top;
      Top    := Tmp;
    end;

    Result.X := Left + (Right - Left) div 2;
    Result.Y := Top + (Bottom - Top) div 2;
  end;
end;

{ Use LineEndPoint to get the End-Point of a line of any given Length at
  any given angle with any given Start-Point. It is primarily for use in
  other routines such as RadialPoint. The angle is in 1/16th of a degree.
  For example, a full circle equals 5760 (16*360).  Zero degrees is at the
  3'o clock position. }
function LineEndPoint(StartPoint: TPoint; Angle, Length: Extended): TPoint;
begin
  if Angle > 360*16 then
    Angle := Frac(Angle / 360*16) * 360*16;


  if Angle < 0 then
    Angle := 360*16 - abs(Angle);

  Result.Y := StartPoint.Y - Round(Length*Sin(DegToRad(Angle/16)));
  Result.X := StartPoint.X + Round(Length*Cos(DegToRad(Angle/16)));
end;

{ Use EllipseRadialLength to get the Radial-Length of non-rotated ellipse at
  any given Eccentric( aka Radial ) Angle. It is primarily for use in other
  routines such as RadialPoint. The Eccentric angle is in 1/16th of a degree.
  For example, a full circle equals 5760 (16*360).  Zero degrees is at the
  3'o clock position. }
function EllipseRadialLength(Rect: TRect; EccentricAngle: Extended): Longint;
var
  a, b, R: Extended;
begin
  a := (Rect.Right - Rect.Left) div 2;
  b := (Rect.Bottom - Rect.Top) div 2;
  R := Sqr(a)*Sqr(b);
  R := Sqrt(R / ((Sqr(b)*Sqr(Cos(DegToRad(EccentricAngle/16))))
        + (Sqr(a)*Sqr(Sin(DegToRad(EccentricAngle/16))))));
  Result := integer(Trunc(R));
end;

{ Use RadialPoint to get the Radial-Point at any given Eccentric( aka Radial )
  angle on any non-rotated ellipse. It is primarily for use in Angles2Coords.
  The EccentricAngle is in 1/16th of a degree. For example, a full circle
  equals 5760 (16*360).  Zero degrees is at the 3'o clock position. }
function RadialPoint(EccentricAngle: Extended; Rect: TRect): TPoint;
var
  R: Longint;
Begin
  R := EllipseRadialLength(Rect, EccentricAngle);
  Result := LineEndPoint(CenterPoint(Rect), EccentricAngle, R);
end;

{ Use Angles2Coords to convert an Eccentric(aka Radial) Angle and an
  Angle-Length, such as are used in X-Windows and GTK, into the coords,
  for Start and End Radial-Points, such as are used in the Windows API Arc
  Pie and Chord routines. The angles are 1/16th of a degree. For example, a
  full circle equals 5760 (16*360). Positive values of Angle and AngleLength
  mean counter-clockwise while negative values mean clockwise direction.
  Zero degrees is at the 3'o clock position. }
procedure Angles2Coords(X, Y, Width, Height: Integer; Angle1, Angle2: Extended;
    var SX, SY, EX, EY: Integer);
var
  aRect: TRect;
  SP, EP: TPoint;
begin
  aRect := Classes.Rect(X, Y, X+Width, Y+Height);
  SP := RadialPoint(Angle1, aRect);
  if Angle2 + Angle1 > 360*16 then
    Angle2 := (Angle2 + Angle1) - 360*16
  else
    Angle2 := Angle2 + Angle1;
  EP := RadialPoint(Angle2, aRect);
  SX := SP.X;
  SY := SP.Y;
  EX := EP.X;
  EY := EP.Y;
end;

{ TlqGDICanvas }

constructor TlqLazCanvas.Create(AWin: TlqWindowBase);
begin
  inherited Create(AWin);
  FDrawing      := False;
  FDrawWindow   := TlqWindow(AWin);
end;

destructor TlqLazCanvas.Destroy;
begin
  if FDrawing then
    DoEndDraw;
  TryFreeBackBuffer;
  inherited;
end;

procedure TlqLazCanvas.DoBeginDraw(AWin: TlqWindowBase; ABuffered: boolean);
var
  ARect: TlqRect;
  //bmsize: Windows.TSIZE;
begin
  if FDrawing and ABuffered and assigned(FBufferBitmap) then
  begin
    // check if the dimensions are ok
    {$IFNDEF wince}
    //GetBitmapDimensionEx(FBufferBitmap, bmsize);
    {$ENDIF}
    FDrawWindow := TlqWindow(AWin);
    {DoGetWinRect(ARect);
    if (bmsize.cx <> (ARect.Right-ARect.Left+1)) or
       (bmsize.cy <> (ARect.Bottom-ARect.Top+1)) then  }
      DoEndDraw;
  end;

  if not FDrawing then
  begin
    FDrawWindow := TlqWindow(AWin);

    {FWinGC      := Windows.GetDC(FDrawWindow.FWinHandle);

    if ABuffered then
    begin
      DoGetWinRect(ARect);
      if (FastDoubleBuffer = False) or (FBufferBitmap = 0)
        or (FBufWidth <> ARect.Width) or (FBufHeight <> ARect.Height) then
      begin
        TryFreeBackBuffer;
//        DoGetWinRect(ARect);
        FBufferBitmap := Windows.CreateCompatibleBitmap(FWinGC, ARect.Width, ARect.Height);
        FBufgc        := CreateCompatibleDC(FWinGC);
        Fgc           := FBufgc;
      end;
      SelectObject(FBufgc, FBufferBitmap);
    end
    else
    begin
      FBufferBitmap := 0;
      Fgc           := FWinGC;
    end;

    SetTextAlign(Fgc, TA_TOP);
    SetBkMode(Fgc, TRANSPARENT);

    FBrush      := CreateSolidBrush(0);
    FPen        := CreatePen(PS_SOLID, 0, 0); // defaults to black
    FClipRegion := CreateRectRgn(0, 0, 1, 1);
    }

    TryFreeBackBuffer;
    FBufferBitmap := TBitmap.Create;
    FBufferBitmap.PixelFormat:= pf24bit;
    FBufferBitmap.Monochrome:=False;
    FBufferBitmap.Transparent:=False;
    FBufferBitmap.setsize(FDrawWindow.Width, FDrawWindow.Height);
    FBufferBitmap.BeginUpdate(True);

    Canvas.Brush.Color:= clBlack;
    Canvas.Brush.Style:= bsSolid;
    Canvas.FillRect(10,10,30,30);

    FColor           := lqColorToWin(clText1);
    FLineStyle       := lsSolid;
    FLineWidth       := 1;
    FBackgroundColor := lqColorToWin(clBoxColor);
  end;

  FDrawing := True;
end;

procedure TlqLazCanvas.DoEndDraw;
begin
  if FDrawing then
  begin
    {DeleteObject(FBrush);
    DeleteObject(FPen);
    DeleteObject(FClipRegion);

    TryFreeBackBuffer;

    Windows.ReleaseDC(FDrawWindow.FWinHandle, FWingc); }
    Canvas.Brush.Color:= clRed;
    Canvas.Brush.Style:= bsDiagCross;
    Canvas.FillRect(10,10,15,15);
    Canvas.Pen.Color:=clYellow;
    Canvas.Line(1,1, FWindow.Width, FWindow.Height);
    FBufferBitmap.EndUpdate();
    FDrawing    := False;
    //FDrawWindow := nil;
  end;
end;

function TlqLazCanvas.GetPixel(X, Y: integer): TlqColor;
var
  c: longword;
begin
  C := Canvas.Pixels[X,Y];
  //c := Windows.GetPixel(FWinGC, X, Y);
  //if c = CLR_INVALID then
    //Writeln('fpGFX/GDI: TlqGDICanvas.GetPixel returned an invalid color');
  Result := WinColorTofpgColor(c);
end;

procedure TlqLazCanvas.SetPixel(X, Y: integer; const AValue: TlqColor);
begin
  Canvas.Pixels[X,y] := lqColorToWin(AValue);
end;

procedure TlqLazCanvas.DoDrawArc(x, y, w, h: TlqCoord; a1, a2: Extended);
var
  SX, SY, EX, EY: Longint;
begin

  {Stupid GDI can't tell the difference between 0 and 360 degrees!!}
  if a2 = 0 then
    Exit; //==>
  {Stupid GDI must be told in which direction to draw}
  {$IFNDEF wince}
  {if a2 < 0 then
    Canvas.SetArcDirection(FGc, AD_CLOCKWISE)
  else
    Windows.SetArcDirection(FGc, AD_COUNTERCLOCKWISE);}
  {$ENDIF}
  Angles2Coords(x, y, w, h, a1*16, a2*16, SX, SY, EX, EY);

  Canvas.Arc(x, y, x+w, y+h, SX, SY, EX, EY);

end;

procedure TlqLazCanvas.DoFillArc(x, y, w, h: TlqCoord; a1, a2: Extended);
var
  SX, SY, EX, EY: Longint;
begin
  {Stupid GDI can't tell the difference between 0 and 360 degrees!!}
  //if a2 = 0 then
  //  Exit; //==>
  {Stupid GDI must be told in which direction to draw}
  {$IFNDEF wince}
  {if a2 < 0 then
    Windows.SetArcDirection(FGc, AD_CLOCKWISE)
  else
    Windows.SetArcDirection(FGc, AD_COUNTERCLOCKWISE);}
  {$ENDIF}
  Angles2Coords(x, y, w, h, a1*16, a2*16, SX, SY, EX, EY);
  {$IFNDEF wince}
  Canvas.Pie(x, y, x+w, y+h, SX, SY, EX, EY);
  {$ENDIF}
end;

procedure TlqLazCanvas.DoDrawPolygon(Points: PPoint; NumPts: Integer; Winding: boolean);
//var
//  pts: array of TPoint;
begin
  Canvas.Polygon( Points, NumPts);
end;

procedure TlqLazCanvas.DoPutBufferToScreen(x, y, w, h: TlqCoord);
begin
///  if FBufferBitmap > 0 then
///    BitBlt(FWinGC, x, y, w, h, Fgc, x, y, SRCCOPY);
end;

procedure TlqLazCanvas.DoAddClipRect(const ARect: TlqRect);
//var
//  rg: HRGN;
begin
  {rg           := CreateRectRgn(ARect.Left, ARect.Top, ARect.Left+ARect.Width, ARect.Top+ARect.Height);
  FClipRect    := ARect;
  FClipRectSet := True;
  CombineRgn(FClipRegion, rg, FClipRegion, RGN_AND);
  SelectClipRgn(Fgc, FClipRegion);
  DeleteObject(rg);}
//  Canvas.Region.AddRectangle();
end;

procedure TlqLazCanvas.DoClearClipRect;
begin
  //SelectClipRgn(Fgc, 0);
  //FClipRectSet := False;
end;

procedure TlqLazCanvas.DoDrawLine(x1, y1, x2, y2: TlqCoord);
begin
  Canvas.Line(x1, y1, x2, y2);
end;

procedure TlqLazCanvas.DoDrawRectangle(x, y, w, h: TlqCoord);
var
  wr: TRect;
  r: TlqRect;

{$IFDEF WinCE}
// *** copied from Lazarus
function FrameRect(DC: HDC; const ARect: TRect; hBr: HBRUSH) : integer;
begin
//roozbeh....works for now!
  Result := Integer(DrawFocusRect(DC,Arect));
end;
{$ENDIF}

begin
  if FLineStyle = lsSolid then
  begin
    wr.Left   := x;
    wr.Top    := y;
    wr.Right  := x + w;
    wr.Bottom := y + h;
    {$IFDEF WinCE}
    FrameRect(Fgc, wr, FBrush);
    {$ELSE}
    Canvas.Rectangle( x, y, w, h); // this handles 1x1 rectangles
    {$ENDIF}
  end
  else
  begin
    r.SetRect(x, y, w, h);
    DoDrawLine(r.Left, r.Top, r.Right, r.Top);
    DoDrawLine(r.Right, r.Top, r.Right, r.Bottom);
    DoDrawLine(r.Right, r.Bottom, r.Left, r.Bottom);
    DoDrawLine(r.Left, r.Bottom, r.Left, r.Top);
  end;
end;

procedure TlqLazCanvas.DoDrawString(x, y: TlqCoord; const txt: string);
var
  WideText: widestring;
begin
  if UTF8Length(txt) < 1 then
    Exit; //==>     ,

  WideText := Utf8Decode(txt);
  Canvas.TextOut(x, y, WideText);
  Canvas.TextOut(0, 0, WideText);
  //debug
  Canvas.Pen.Color:= clRed;
  Canvas.Line(0,self.FWindow.Height, self.FWindow.Width,0);
end;

procedure TlqLazCanvas.DoFillRectangle(x, y, w, h: TlqCoord);
var
  wr: TRect;
begin
  wr.Left   := x;
  wr.Top    := y;
  wr.Right  := x + w;
  wr.Bottom := y + h;
  Canvas.FillRect( x, y, w, h);
end;

procedure TlqLazCanvas.DoFillTriangle(x1, y1, x2, y2, x3, y3: TlqCoord);
var
  pts: array[1..3] of TPoint;
begin
  pts[1].X := x1;
  pts[1].Y := y1;
  pts[2].X := x2;
  pts[2].Y := y2;
  pts[3].X := x3;
  pts[3].Y := y3;
  Canvas.Polygon( pts);
end;

function TlqLazCanvas.DoGetClipRect: TlqRect;
begin
  Result := FClipRect;
end;

procedure TlqLazCanvas.DoGetWinRect(out r: TlqRect);
var
  wr: TRect;
begin
  ///GetClientRect(FDrawWindow.FWinHandle, wr);
  r.Top := 0;
  r.Left:= 0;
  r.Width := FWindow.Width;
  r.Height:= FWindow.Height;
  {r.Top     := wr.Top;
  r.Left    := wr.Left;
  r.Width   := wr.Right - wr.Left + 1;
  r.Height  := wr.Bottom - wr.Top + 1; }
end;

procedure TlqLazCanvas.DoSetClipRect(const ARect: TlqRect);
begin
  {FClipRectSet := True;
  FClipRect    := ARect;
  DeleteObject(FClipRegion);
  FClipRegion  := CreateRectRgn(ARect.Left, ARect.Top, ARect.Left+ARect.Width, ARect.Top+ARect.Height);
  SelectClipRgn(Fgc, FClipRegion);}

end;

procedure TlqLazCanvas.DoSetColor(cl: TlqColor);
begin
  //DeleteObject(FBrush);
  FWindowsColor := lqColorToWin(cl);
  {FBrush := CreateSolidBrush(FWindowsColor);
  DoSetLineStyle(FLineWidth, FLineStyle);
  SelectObject(Fgc, FBrush); }
  CAnvas.Brush.Color:=FWindowsColor;
end;

procedure TlqLazCanvas.DoSetLineStyle(awidth: integer; astyle: TlqLineStyle);
const
  cDot: array[1..2] of DWORD = (1, 1);
  cDash: array[1..4] of DWORD = (4, 2, 4, 2);
var
  lw: integer;
  //logBrush: TLogBrush;
begin
  FLineWidth := awidth;

  {logBrush.lbStyle := BS_SOLID;
  logBrush.lbColor := FWindowsColor;
  logBrush.lbHatch := 0;
  DeleteObject(FPen); }
  case AStyle of
    lsDot:
      begin
        {$IFNDEF wince}
        //FPen := ExtCreatePen(PS_GEOMETRIC or PS_ENDCAP_FLAT or PS_USERSTYLE, FLineWidth, logBrush, Length(cDot), @cDot);
        {$ENDIF}
        Canvas.Pen.Style:= psDot;
      end;
    lsDash:
      begin
        {$IFNDEF wince}
        //FPen := ExtCreatePen(PS_GEOMETRIC or PS_ENDCAP_FLAT or PS_USERSTYLE, FLineWidth, logBrush, Length(cDash), @cDash);
        {$ENDIF}
        Canvas.Pen.Style:= psDash;
      end;
    lsSolid:
      begin
        //FPen := CreatePen(PS_SOLID, FLineWidth, FWindowsColor);
        Canvas.Pen.Style:= psSolid;
      end;
    else
      begin
        //FPen := CreatePen(PS_SOLID, FLineWidth, FWindowsColor);
        Canvas.Pen.Style:= psSolid;
      end;
  end;
  //SelectObject(Fgc, FPen);
end;

procedure TlqLazCanvas.DoSetTextColor(cl: TlqColor);
begin
  //Windows.SetTextColor(Fgc, lqColorToWin(cl));
  Canvas.Font.Color := lqColorToWin(cl);
end;

procedure TlqLazCanvas.TryFreeBackBuffer;
begin
  {if FBufferBitmap > 0 then
    DeleteObject(FBufferBitmap);
  FBufferBitmap := 0;

  if FBufgc > 0 then
    DeleteDC(FBufgc);
  FBufgc := 0;}
  FreeAndNil(FBufferBitmap);
end;

function TlqLazCanvas.GetBufferCanvas: TCanvas;
begin
  result := FBufferBitmap.Canvas;
end;

procedure TlqLazCanvas.DoSetFontRes(fntres: TlqFontResourceBase);
begin
  if fntres = nil then
    Exit; //==>
  FCurFontRes := TlqFontResource(fntres);
  //Windows.SelectObject(Fgc, FCurFontRes.Handle);
///  Canvas.Font.Name:= FCurFontRes.;
end;

procedure TlqLazCanvas.DoDrawImagePart(x, y: TlqCoord; img: TlqImageBase; xi, yi, w, h: integer);
const
  DSTCOPY     = $00AA0029;
  ROP_DSPDxax = $00E20746;
var
///  tmpdc: HDC;
  rop: longword;
begin
  if img = nil then
    Exit; //==>
 {
  tmpdc := CreateCompatibleDC(wapplication.display);
  SelectObject(tmpdc, TlqGDIImage(img).BMPHandle);

  if TlqGDIImage(img).FIsTwoColor then
    rop := PATCOPY
  else
    rop := SRCCOPY;

  if TlqGDIImage(img).MaskHandle > 0 then
    MaskBlt(Fgc, x, y, w, h, tmpdc, xi, yi, TlqGDIImage(img).MaskHandle, xi, yi, MakeRop4(rop, DSTCOPY))
  else
    BitBlt(Fgc, x, y, w, h, tmpdc, xi, yi, rop);

  DeleteDC(tmpdc); }
end;

procedure TlqLazCanvas.DoXORFillRectangle(col: TlqColor; x, y, w, h: TlqCoord);
{var
  hb: HBRUSH;
  nullpen: HPEN;}
begin
{  hb      := CreateSolidBrush(lqColorToWin(lqColorToRGB(col)));
  nullpen := CreatePen(PS_NULL, 0, 0);

  SetROP2(Fgc, R2_XORPEN);
  SelectObject(Fgc, hb);
  SelectObject(Fgc, nullpen);

  Canvas.Rectangle(Fgc, x, y, x + w + 1, y + h + 1);

  SetROP2(Fgc, R2_COPYPEN);
  DeleteObject(hb);
  SelectObject(Fgc, FPen); }
end;

function TlqLazCanvas.GetBufferBitmap: TBitmap;
begin
  result := FBufferBitmap;
end;

end.

