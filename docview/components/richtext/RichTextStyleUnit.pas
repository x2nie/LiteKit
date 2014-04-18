Unit RichTextStyleUnit;

{$mode objfpc}{$H+}

Interface

uses
  Classes, lq_base, lq_main, CanvasFontManager, RichTextDocumentUnit;

type
  TTextDrawStyle = record
    FontNameSize: TlqString;
    FontAttributes: TFontAttributes;
    Color: TlqColor;
    BackgroundColor: TlqColor;
    Alignment: TTextAlignment;
    Wrap: boolean;
    LeftMargin: longint;
    RightMargin: longint;
  end;

  TMarginSizeStyle = ( msAverageCharWidth, msMaximumCharWidth, msSpecifiedChar );

  TRichTextSettings = class( TlqComponent )
  protected
    FHeading1Font: TlqFont;
    FHeading2Font: TlqFont;
    FHeading3Font: TlqFont;
    FFixedFont: TlqFont;
    FNormalFont: TlqFont;
    FDefaultBackgroundColor: TlqColor;
    FDefaultColor: TlqColor;
    FDefaultAlignment: TTextAlignment;
    FDefaultWrap: boolean;
    FAtLeastOneWordBeforeWrap: boolean;
    FMarginSizeStyle: TMarginSizeStyle;
    FMarginChar: longint;
    FOnChange: TNotifyEvent;
    FMargins: TRect;
    FUpdateCount: longint;
    FChangesPending: boolean;
    Procedure Change;
    Procedure SetNormalFont( NewFont: TlqFont );
    Procedure SetFixedFont( NewFont: TlqFont );
    Procedure SetHeading1Font( NewFont: TlqFont );
    Procedure SetHeading2Font( NewFont: TlqFont );
    Procedure SetHeading3Font( NewFont: TlqFont );
    Procedure SetDefaultColor( NewColor: TlqColor );
    Procedure SetDefaultBackgroundColor( NewColor: TlqColor );
    Procedure SetDefaultAlignment( Alignment: TTextAlignment );
    Procedure SetDefaultWrap( Wrap: boolean );
    Procedure SetAtLeastOneWordBeforeWrap( NewValue: boolean );
    Procedure SetMarginSizeStyle( NewValue: TMarginSizeStyle );
    Procedure SetMarginChar( NewValue: longint );
    Procedure SetMargins( const NewMargins: TRect );
    function GetMargin_Left: longint;
    Procedure SetMargin_Left( NewValue: longint );
    function GetMargin_Bottom: longint;
    Procedure SetMargin_Bottom( NewValue: longint );
    function GetMargin_Right: longint;
    Procedure SetMargin_Right( NewValue: longint );
    function GetMargin_Top: longint;
    Procedure SetMargin_Top( NewValue: longint );
    Procedure SetupComponent;
    Procedure AssignFont(var AFont: TlqFont; NewFont: TlqFont);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    property Margins: TRect read FMargins write SetMargins;
    property Heading1Font: TlqFont read FHeading1Font write SetHeading1Font;
    property Heading2Font: TlqFont read FHeading2Font write SetHeading2Font;
    property Heading3Font: TlqFont read FHeading3Font write SetHeading3Font;
    property FixedFont: TlqFont read FFixedFont write SetFixedFont;
    property NormalFont: TlqFont read FNormalFont write SetNormalFont;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property DefaultBackgroundColor: TlqColor read FDefaultBackgroundColor write SetDefaultBackgroundColor;
    property DefaultColor: TlqColor read FDefaultColor write SetDefaultColor;
    property DefaultAlignment: TTextAlignment read FDefaultAlignment write SetDefaultAlignment;
    property DefaultWrap: boolean read FDefaultWrap write SetDefaultWrap default True;
    property AtLeastOneWordBeforeWrap: boolean read FAtLeastOneWordBeforeWrap write SetAtLeastOneWordBeforeWrap;
    property MarginSizeStyle: TMarginSizeStyle read FMarginSizeStyle write SetMarginSizeStyle;
    property MarginChar: longint read FMarginChar write SetMarginChar;
    // margins are exposed as individual properties here
    // since the Sibyl IDE cannot cope with editing a record property
    // within a class property (as in RichTextView)
    property Margin_Left: longint read GetMargin_Left write SetMargin_Left;
    property Margin_Bottom: longint read GetMargin_Bottom write SetMargin_Bottom;
    property Margin_Right: longint read GetMargin_Right write SetMargin_Right;
    property Margin_Top: longint read GetMargin_Top write SetMargin_Top;
  end;


  Procedure ApplyStyle( var Style: TTextDrawStyle;
                        FontManager: TCanvasFontManager );

  Procedure ApplyStyleTag( const Tag: TTag;
                           Var Style: TTextDrawStyle;
                           FontManager: TCanvasFontManager;
                           const ASettings: TRichTextSettings;
                           const X: longint );

  function GetDefaultStyle( const ASettings: TRichTextSettings ): TTextDrawStyle;



Implementation

uses
  SysUtils,
  ACLStringUtility,
  nvUtilities,
  SettingsUnit;


Procedure ApplyStyle(var Style: TTextDrawStyle; FontManager: TCanvasFontManager);
var
  s: string;
begin
ProfileEvent('DEBUG:  ApplyStyle >>>');
  assert(FontManager <> nil, 'FontManager should not have been nil');
  s := Style.FontNameSize;
  ApplyFontAttributes(s, Style.FontAttributes);
  FontManager.SetFont(s);
  FontManager.Canvas.TextColor := Style.Color;
ProfileEvent('DEBUG:  ApplyStyle <<<');
end;

Procedure ApplyStyleTag( Const Tag: TTag;
                         var Style: TTextDrawStyle;
                         FontManager: TCanvasFontManager;
                         const ASettings: TRichTextSettings;
                         const X: longint );
var
  MarginParam1: string;
  MarginParam2: string;
  NewMargin: longint;
  FontFaceName: string;
  FontSizeString: string;
  NewStyle: TTextDrawStyle;
  ParseIndex: longint;
  XSizeStr: string;
  YSizeStr: string;
  tmpFontParts : TStrings;

  MarginSize: longint;
  ParsePoint: longint;
begin
ProfileEvent('DEBUG:  ApplyStyleTag >>>');
  case Tag.TagType of
    ttBold:
      Include( Style.FontAttributes, faBold );

    ttBoldOff:
      Exclude( Style.FontAttributes, faBold );

    ttItalic:
      Include( Style.FontAttributes, faItalic );

    ttItalicOff:
      Exclude( Style.FontAttributes, faItalic );

    ttUnderline:
      Include( Style.FontAttributes, faUnderscore );

    ttUnderlineOff:
      Exclude( Style.FontAttributes, faUnderscore );

    ttFixedWidthOn:
      begin
        Style.FontNameSize := Copy(ASettings.FixedFont.FontDesc, 1, Pos(':', ASettings.FixedFont.FontDesc)-1);
        Style.FontAttributes := GeTlquiFontAttributes(ASettings.FixedFont);
      end;

    ttFixedWidthOff:
      begin
        Style.FontNameSize := Copy(ASettings.NormalFont.FontDesc, 1, Pos(':', ASettings.NormalFont.FontDesc)-1);
        Style.FontAttributes := GeTlquiFontAttributes(ASettings.NormalFont);
      end;

    ttHeading1:
      begin
        Style.FontNameSize := Copy(ASettings.Heading1Font.FontDesc, 1, Pos(':', ASettings.Heading1Font.FontDesc)-1);
        Style.FontAttributes := GeTlquiFontAttributes(ASettings.Heading1Font);
      end;

    ttHeading2:
      begin
        Style.FontNameSize := Copy(ASettings.Heading2Font.FontDesc, 1, Pos(':', ASettings.Heading2Font.FontDesc)-1);
        Style.FontAttributes := GeTlquiFontAttributes(ASettings.Heading2Font);
      end;

    ttHeading3:
      begin
        Style.FontNameSize := Copy(ASettings.Heading3Font.FontDesc, 1, Pos(':', ASettings.Heading3Font.FontDesc)-1);
        Style.FontAttributes := GeTlquiFontAttributes(ASettings.Heading3Font);
      end;

    ttHeading1Off,
    ttHeading2Off,
    ttHeading3Off:
      begin
        Style.FontNameSize := Copy(ASettings.NormalFont.FontDesc, 1, Pos(':', ASettings.NormalFont.FontDesc)-1);
        Style.FontAttributes := GeTlquiFontAttributes(ASettings.NormalFont);
      end;

    ttFont:
      begin
        tmpFontParts := TStringList.Create;
        StrExtractStringsQuoted(tmpFontParts, Tag.Arguments);
        FontFaceName := tmpFontParts[0];
        if tmpFontParts.Count=2 then
          FontSizeString := tmpFontParts[1];
        tmpFontParts.Free;

        NewStyle := Style;
        NewStyle.FontNameSize := FontFaceName;

        if Pos( 'x', FontSizeString ) > 0 then
        begin
          tmpFontParts := TStringList.Create;
          StrExtractStrings(tmpFontParts, FontSizeString, ['x'], #0);
          XSizeStr := tmpFontParts[0];
          YSizeStr := tmpFontParts[1];
          tmpFontParts.Destroy;
          // This probably needs to be enhanced to extract the font name and size first.
          NewStyle.FontNameSize := NewStyle.FontNameSize + '-' + YSizeStr;
        end
        else if (FontSizeString<>'') then
          // Same here
          NewStyle.FontNameSize := NewStyle.FontNameSize + '-' + FontSizeString;

        if ( NewStyle.FontNameSize <> '' ) then
          Style := NewStyle;
      end;

    ttFontOff:
      begin
        { TODO: Restore to previous font, not NormalFont, because previous font could have
           been something different to NormalFont }
        Style.FontNameSize := Copy(ASettings.NormalFont.FontDesc, 1, Pos(':', ASettings.NormalFont.FontDesc)-1);
        Style.FontAttributes := GeTlquiFontAttributes(ASettings.NormalFont);
      end;

    ttColor:
      GetTagColor( Tag.Arguments, Style.Color );

    ttColorOff,
    ttRedOff,
    ttGreenOff,
    ttBlackOff,
    ttBlueOff:
      Style.Color := ASettings.FDefaultColor;

    ttBackgroundColor:
      GetTagColor( Tag.Arguments, Style.BackgroundColor );

    ttBackgroundColorOff:
      Style.BackgroundColor := ASettings.FDefaultBackgroundColor;

    ttRed:
      Style.Color := clRed;

    ttBlue:
      Style.Color := clBlue;

    ttGreen:
      Style.Color := clGreen;

    ttBlack:
      Style.Color := clBlack;

    ttAlign:
      Style.Alignment := GetTagTextAlignment( Tag.Arguments, ASettings.FDefaultAlignment );

    ttNoWrap:
      Style.Wrap := False;

    ttNoWrapOff:
      Style.Wrap := True;

    ttSetLeftMargin,
    ttSetRightMargin:
      begin
        tmpFontParts := TStringList.Create;
        StrExtractStrings(tmpFontParts, Tag.Arguments, [' '], #0);
        MarginParam1 := tmpFontParts[0];

        ParsePoint := 1;
        if     ( Tag.TagType = ttSetLeftMargin )
           and ( MarginParam1 = 'here' ) then
        begin
          Style.LeftMargin := X;
        end
        else
        begin
          try
            MarginSize := StrToInt( MarginParam1 );
            if tmpFontParts.Count > 1 then   // do we have a second parameter
              MarginParam2 := tmpFontParts[1]
            else
              MarginParam2 := '';

            if MarginParam2 = 'pixels' then
              NewMargin := MarginSize
            else if MarginParam2 = 'deffont' then
              NewMargin := MarginSize * ASettings.NormalFont.TextWidth('w')  // .Width
            else
            begin
              case ASettings.MarginSizeStyle of
                msAverageCharWidth:
                  NewMargin := MarginSize * FontManager.AverageCharWidth;

                msMaximumCharWidth:
                  NewMargin := MarginSize * FontManager.MaximumCharWidth;

                msSpecifiedChar:
                  NewMargin := MarginSize * FontManager.CharWidth(Chr(ASettings.MarginChar));
              end;
            end;
          except
            NewMargin := 0;
          end;

          if Tag.TagType = ttSetLeftMargin then
            Style.LeftMargin := ASettings.Margins.Left + NewMargin
          else
            Style.RightMargin := ASettings.Margins.Right + NewMargin;
        end;
        tmpFontParts.Free;
      end;  { teSet[left|right]margin }

  end;  { case Tag.TagType }

  ApplyStyle( Style, FontManager );
ProfileEvent('DEBUG:  ApplyStyleTag <<<');
end;

function GetDefaultStyle( const ASettings: TRichTextSettings ): TTextDrawStyle;
begin
  FillChar(Result, SizeOf(TTextDrawStyle), 0);
  Result.FontNameSize := ASettings.NormalFont.FontDesc;
  Result.FontAttributes := [];
  Result.Alignment := ASettings.FDefaultAlignment;
  Result.Wrap := ASettings.FDefaultWrap;
  Result.Color := ASettings.FDefaultColor;
  Result.BackgroundColor := ASettings.FDefaultBackgroundColor;
  Result.LeftMargin := ASettings.Margins.Left;
  Result.RightMargin := ASettings.Margins.Right;
end;


Procedure TRichTextSettings.SetupComponent;
begin
  Name := 'RichTextSettings';

  FNormalFont   := lqGetFont(Settings.NormalFontDesc);
  FFixedFont    := lqGetFont(Settings.FixedFontDesc);
  FHeading1Font := lqGetFont(DefaultTopicFontName + '-20');
  FHeading2Font := lqGetFont(DefaultTopicFontName + '-14');
  FHeading3Font := lqGetFont(DefaultTopicFontName + '-10:bold');

  FDefaultColor := clBlack;
  FDefaultBackgroundColor := clBoxColor;

  FDefaultAlignment := taLeft;
  FDefaultWrap := true;
  FAtLeastOneWordBeforeWrap := false;

  { TODO: Add this property to DocView's Preferences dialog }
  FMarginSizeStyle := msAverageCharWidth;
  FMarginChar := Ord( ' ' );

  FMargins.Left   := 0;
  FMargins.Right  := 0;
  FMargins.Top    := 0;
  FMargins.Bottom := 0;

  FUpdateCount    := 0;
  FChangesPending := false;
end;

constructor TRichTextSettings.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetupComponent;
end;

destructor TRichTextSettings.Destroy;
begin
  FNormalFont.Free;
  FFixedFont.Free;
  FHeading1Font.Free;
  FHeading2Font.Free;
  FHeading3Font.Free;
  Inherited Destroy;
end;

Procedure TRichTextSettings.Change;
begin
  if FUpdateCount > 0 then
  begin
     FChangesPending := true;
     exit;
  end;

  if FOnChange <> nil then
    FOnChange( self );
end;

Procedure TRichTextSettings.SetDefaultAlignment( Alignment: TTextAlignment );
begin
  if Alignment = FDefaultAlignment then
    exit; // no change

  FDefaultAlignment := Alignment;
  Change;
end;

Procedure TRichTextSettings.SetDefaultWrap( Wrap: boolean );
begin
  if Wrap = FDefaultWrap then
    exit; // no change

  FDefaultWrap := Wrap;
  Change;
end;

Procedure TRichTextSettings.SetAtLeastOneWordBeforeWrap( NewValue: boolean );
begin
  if NewValue = FAtLeastOneWordBeforeWrap then
    exit; // no change

  FAtLeastOneWordBeforeWrap := NewValue;
  Change;
end;

Procedure TRichTextSettings.SetMarginChar( NewValue: longint );
begin
  if NewValue = FMarginChar then
    exit; // no change

  FMarginChar := NewValue;

  if FMarginSizeStyle <> msSpecifiedChar then
    // doesn't matter, will be ignored
    exit;
  Change;
end;

Procedure TRichTextSettings.SetMarginSizeStyle( NewValue: TMarginSizeStyle );
begin
  if NewValue = FMarginSizeStyle then
    exit; // no change

  FMarginSizeStyle := NewValue;
  Change;
end;

Function FontSame( FontA: TlqFont; FontB: TlqFont ): boolean;
begin
  if ( FontA = nil ) or ( FontB = nil ) then
    Result := False
  else
    Result := FontA.FontDesc = FontB.FontDesc;
end;

Procedure TRichTextSettings.AssignFont(var AFont: TlqFont; NewFont: TlqFont );
begin
  If NewFont = Nil Then
    NewFont := lqApplication.DefaultFont;

  if FontSame( NewFont, AFont ) then
  begin
    if AFont <> NewFont then { they are not the same instance }
      NewFont.Free;
    Exit; // no change needed
  end;

  AFont.Free;
  AFont := NewFont;

  Change;
End;

Procedure TRichTextSettings.SetHeading1Font( NewFont: TlqFont );
begin
  AssignFont( FHeading1Font, NewFont );
end;

Procedure TRichTextSettings.SetHeading2Font( NewFont: TlqFont );
begin
  AssignFont( FHeading2Font, NewFont );
End;

Procedure TRichTextSettings.SetHeading3Font( NewFont: TlqFont );
begin
  AssignFont( FHeading3Font, NewFont );
End;

Procedure TRichTextSettings.SetFixedFont( NewFont: TlqFont );
begin
  AssignFont( FFixedFont, NewFont );
end;

Procedure TRichTextSettings.SetNormalFont( NewFont: TlqFont );
begin
  AssignFont( FNormalFont, NewFont );
end;

Procedure TRichTextSettings.SetMargins( const NewMargins: TRect );
begin
  if NewMargins = FMargins then
    exit; // no change
  FMargins := NewMargins;
  Change;
end;

function TRichTextSettings.GetMargin_Left: longint;
begin
  Result := FMargins.Left;
end;

Procedure TRichTextSettings.SetMargin_Left( NewValue: longint );
begin
  FMargins.Left := NewValue;
end;

function TRichTextSettings.GetMargin_Bottom: longint;
begin
  Result := FMargins.Bottom;
end;

Procedure TRichTextSettings.SetMargin_Bottom( NewValue: longint );
begin
  FMargins.Bottom := NewValue;
end;

function TRichTextSettings.GetMargin_Right: longint;
begin
  Result := FMargins.Right;
end;

Procedure TRichTextSettings.SetMargin_Right( NewValue: longint );
begin
  FMargins.Right := NewValue;
end;

function TRichTextSettings.GetMargin_Top: longint;
begin
  Result := FMargins.Top;
end;

Procedure TRichTextSettings.SetMargin_Top( NewValue: longint );
begin
  FMargins.Top := NewValue;
end;

Procedure TRichTextSettings.SetDefaultColor( NewColor: TlqColor );
begin
  if NewColor = FDefaultColor then
    exit;
  FDefaultColor := NewColor;
  Change;
end;

Procedure TRichTextSettings.SetDefaultBackgroundColor( NewColor: TlqColor );
begin
  if NewColor = FDefaultBackgroundColor then
    exit;
  FDefaultBackgroundColor := NewColor;
  Change;
end;

procedure TRichTextSettings.BeginUpdate;
begin
  inc( FUpdateCount );
end;

procedure TRichTextSettings.EndUpdate;
begin
  if FUpdateCount = 0 then
    exit;

  dec( FUpdateCount );
  if FUpdateCount = 0 then
  begin
    if FChangesPending then
    begin
      Change;
      FChangesPending := false;
    end;
  end;
end;


initialization
  RegisterClasses([TRichTextSettings]);

end.

