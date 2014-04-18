Unit CanvasFontManager;

{$mode objfpc}{$H+}

interface

uses
  Classes
  ,lq_base
  ,lq_main
  ,lq_widget
  ;

Const
  // This defines the fraction of a pixel that
  // font character widths will be given in
  DefaultTopicFontName = 'Arial';
  DefaultTopicFontSize: integer = 10;
  DefaultTopicFixedFontName = 'Courier New';
  DefaultTopicFixedFontSize: integer = 10;

var
  DefaultTopicFont: string;
  DefaultTopicFixedFont: string;

type
  {Standard Font Attributes}
  TFontAttributes = set of (faBold, faItalic, faUnderScore, faOutline, faStrikeOut);

  {Standard Font pitches}
  TFontPitch=(fpFixed,fpProportional);


  TCanvasFontManager = class(TObject)
  private
    FWidget: TlqWidget;
    FCanvas: TlqCanvas;
    FFont: TlqFont;
    function    GetCurrentFont: TlqFont;
    procedure   SetDefaultFont(const AValue: TlqFont);
  protected
    FDefaultFont: TlqFont;
  public
    constructor Create(ACanvas: TlqCanvas; AWidget: TlqWidget); reintroduce;
    destructor  Destroy; override;
    function    AverageCharWidth: longint;
    function    CharDescender: longint;
    function    CharHeight: longint;
    function    CharWidth( const C: TlqChar ): longint;  // Retrieve the width of the given char, in the current font
    function    IsFixed: boolean;
    function    MaximumCharWidth: longint;
    procedure   DrawString(var Point: TPoint; const Length: longint; const S: PChar);
    procedure   SetFont(const AFontDesc: TlqString);
    property    Canvas: TlqCanvas read FCanvas;
    property    CurrentFont: TlqFont read GetCurrentFont;
    property    DefaultFont: TlqFont read FDefaultFont write SetDefaultFont;
    property    Widget: TlqWidget read FWidget;
  end;


// Get the font attributes of a LiteKit font
function GeTlquiFontAttributes(const AFont: TlqFont): TFontAttributes;
function GeTlquiFont(const AFontNameSize: string; const Attrs: TFontAttributes): TlqFont;
procedure ApplyFontAttributes(var AFontDesc: string; const Attrs: TFontAttributes);


implementation

uses
  SysUtils
  ,ACLStringUtility
  ,nvUtilities
  ,lq_stringutils
  ,SettingsUnit
  ;


function GeTlquiFontAttributes(const AFont: TlqFont): TFontAttributes;
var
  s: string;
  facename: string;
  cp: integer;
  c: char;
  token: string;
  prop, propval: string;
  lDesc: string;
  lFontSize: integer;

  function NextC: char;
  begin
    Inc(cp);
    if cp > length(lDesc) then
      c := #0
    else
      c := lDesc[cp];
    Result := c;
  end;

  procedure NextToken;
  begin
    token := '';
    while (c <> #0) and (c in [' ', 'a'..'z', 'A'..'Z', '_', '0'..'9']) do
    begin
      token := token + c;
      NextC;
    end;
  end;

begin
  Result := [];
  cp := 0;
  lDesc := AFont.FontDesc;

  // find fontface
  NextC;
  NextToken;

  // find font size
  if c = '-' then
  begin
    NextC;
    NextToken;
    lFontSize := StrToIntDef(token, DefaultTopicFontSize);
  end;

  // find font attributes
  while c = ':' do
  begin
    NextC;
    NextToken;
    prop    := UpperCase(token);
    propval := '';
    if c = '=' then
    begin
      NextC;
      NextToken;
      propval := UpperCase(token);
    end;
    // convert fontdesc attributes to standard font attributes
    if prop = 'BOLD' then
      include(Result, faBold)
    else if prop = 'ITALIC' then
      include(Result, faItalic)
    else if prop = 'UNDERLINE' then
      include(Result, faUnderScore)
    else if prop = 'OUTLINE' then
      include(Result, faOutline)
    else if prop = 'STRIKEOUT' then
      include(Result, faStrikeOut)
  end;
end;

function GeTlquiFont(const AFontNameSize: string; const Attrs: TFontAttributes): TlqFont;
var
  s: string;
begin
  s := AFontNameSize;
  ApplyFontAttributes(s, Attrs);
  Result := lqGetFont(s);
end;

// Add attributes to font name
procedure ApplyFontAttributes(var AFontDesc: string; const Attrs: TFontAttributes);
begin
  if faItalic in Attrs then
    if Pos(':Italic', AFontDesc) = 0 then
      AFontDesc := AFontDesc + ':Italic';

  if faBold in Attrs then
    if Pos(':Bold', AFontDesc) = 0 then
      AFontDesc := AFontDesc + ':Bold';

  if faOutline in Attrs Then
    if Pos(':Outline', AFontDesc) = 0 then
      AFontDesc := AFontDesc + ':Outline';

  if faStrikeOut in Attrs Then
    if Pos(':Strikeout', AFontDesc) = 0 then
      AFontDesc := AFontDesc + ':Strikeout';

  if faUnderScore in Attrs Then
    if Pos(':Underscore', AFontDesc) = 0 then
      AFontDesc := AFontDesc + ':Underscore';
end;

// Provide font name substitutes for some common bitmap fonts found in INF files
function SubstituteBitmapFontToOutline( const FaceName: string ): string;
begin
  if StringsSame( FaceName, 'Helv' ) then
    result := DefaultTopicFontName
  else if StringsSame( FaceName, 'Helvetica' ) then
    result := DefaultTopicFontName
  else if StringsSame( FaceName, 'Tms Rmn' ) then
    result := 'Times New Roman'
  else if StringsSame( FaceName, 'System Proportional' ) then
    result := DefaultTopicFontName
  else if StringsSame( FaceName, 'System Monospaced' ) then
    result := DefaultTopicFixedFontName
  else if StringsSame( FaceName, 'System VIO' ) then
    result := DefaultTopicFixedFontName
  else
    result := FaceName; // no substitution
end;

// Look for the best match for the given face, size and attributes.
// If FixedWidth is set then makes sure that the result is fixed
procedure FindBestFontMatch( const FaceName: string; const PointSize: longint;
    const Attributes: TFontAttributes; const FixedWidth: boolean; var FontDesc: string );
var
  sl: TStringList;
  i: integer;
begin
  FontDesc := '';
  sl := lqApplication.GetFontFaceList;
  for i := 0 to sl.Count-1 do
  begin
    if Pos(FaceName, sl[i]) > 0 then
      FontDesc := sl[i] + '-' + IntToStr(PointSize);
  end;

  ApplyFontAttributes(FontDesc, Attributes);

  // if nothing found, use default font of LiteKit
  if FontDesc = '' then
    FontDesc := lqApplication.DefaultFont.FontDesc;
end;


{ TCanvasFontManager }

constructor TCanvasFontManager.Create(ACanvas: TlqCanvas; AWidget: TlqWidget);
begin
  inherited Create;
  FCanvas := ACanvas;
  FWidget := AWidget;
  FDefaultFont := lqGetFont(DefaultTopicFont);
  FCanvas.Font := FDefaultFont;
  FFont := nil;
end;

destructor TCanvasFontManager.Destroy;
begin
  FCanvas.Font := lqApplication.DefaultFont;
  FDefaultFont.Free;
  FFont.Free;
  inherited Destroy;
end;

procedure TCanvasFontManager.SetDefaultFont(const AValue: TlqFont);
begin
  if FDefaultFont = AValue then
    exit;
  FDefaultFont.Free;
  FDefaultFont := AValue;
end;

function TCanvasFontManager.GetCurrentFont: TlqFont;
begin
  Result := FCanvas.Font as TlqFont;
end;

// Set the current font for the canvas to match the given
// spec, creating or re-using fonts as needed.
procedure TCanvasFontManager.SetFont(const AFontDesc: TlqString);
begin
  if FCanvas.Font.FontDesc = AFontDesc then
    Exit; // nothing to do so exit

  if FDefaultFont.FontDesc = AFontDesc then
  begin
    FCanvas.Font := FDefaultFont;
    Exit;
  end;

  if Assigned(FFont) then
    FFont.Free;
  FFont := lqGetFont(AFontDesc);
  FCanvas.Font := FFont;
end;

function TCanvasFontManager.CharWidth( const C: TlqChar ): longint;
begin
  Result := FCanvas.Font.TextWidth(C);
end;

function TCanvasFontManager.AverageCharWidth: longint;
begin
  Result := FCanvas.Font.TextWidth('c');
end;

function TCanvasFontManager.MaximumCharWidth: longint;
begin
  Result := FCanvas.Font.TextWidth('W');
end;

function TCanvasFontManager.CharHeight: longint;
begin
  Result := FCanvas.Font.Height;
end;

function TCanvasFontManager.CharDescender: longint;
begin
  Result := FCanvas.Font.Descent;
end;

function TCanvasFontManager.IsFixed: boolean;
begin
  Result := FCanvas.Font.IsFixedWidth;
end;

procedure TCanvasFontManager.DrawString(var Point: TPoint; const Length: longint; const S: PChar);
var
  t: TlqString;
begin
  t := s;
  //case Settings.Encoding of
  //  encUTF8:      t := IPFToUTF8(t);
  //  encCP437:     t := CP437ToUTF8(t);
  //  encCP850:     t := CP850ToUTF8(t);
  //  encIBMGraph:  t := IBMGraphToUTF8(t);
  //else
  //  t := IPFToUTF8(t);
  //end;
  FCanvas.DrawString(Point.X, Point.Y, t);
  Point.x := Point.X + Canvas.Font.TextWidth(t);
end;


initialization
  DefaultTopicFont := DefaultTopicFontName + '-' + IntToStr(DefaultTopicFontSize);
  DefaultTopicFixedFont := DefaultTopicFixedFontName + '-' + IntToStr(DefaultTopicFixedFontSize);

end.

