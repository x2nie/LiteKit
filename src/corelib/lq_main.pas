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
      The main unit that ties everything together from CoreLib.
}

unit lq_main;

{$mode objfpc}{$H+}

{.$Define DEBUG}

// To enable the AggPas powered Canvas
{.$define AGGCanvas}

{ TODO : Implement font size adjustments for each platform. eg: linux=10pt & windows=8pt }

interface

uses
  Classes,
  SysUtils,
  lq_constants,
  lq_base,
  lq_interface,
  lq_impl;

type
  //TRANSFER TYPE
  TlqComponent = lq_base.TlqComponent;

type
  TOrientation = (orVertical, orHorizontal);

  TAlign = (alNone, alTop, alBottom, alLeft, alRight, alClient);
  TLayout = (tlTop, tlCenter, tlBottom);
  TBoxLayout = (tbLeftBox, tbRightBox);

  TAnchor  = (anLeft, anRight, anTop, anBottom);
  TAnchors = set of TAnchor;

  TlqButtonFlags = set of (btfIsEmbedded, btfIsDefault, btfIsPressed,
    btfIsSelected, btfHasFocus, btfHasParentColor, btfFlat, btfHover);

  TlqMenuItemFlags = set of (mifSelected, mifHasFocus, mifSeparator,
    mifEnabled, mifChecked, mifSubMenu);
    
  TlqTextFlags = set of (txtLeft, txtHCenter, txtRight, txtTop, txtVCenter,
    txtBottom, txtWrap, txtDisabled, txtAutoSize);

  TMouseButton = (mbLeft, mbRight, mbMiddle);

  TArrowDirection = (adUp, adDown, adLeft, adRight);

const
  AllAnchors = [anLeft, anRight, anTop, anBottom];
  TextFlagsDflt = [txtLeft, txtTop];
  

type
  { *******************************************
      Internal event properties: Event Types
    *******************************************}
  TIntKeyPressEvent = procedure(Sender: TObject; var keycode: word; var shiftstate: word;
                            var consumed: boolean) of object;
  TIntMouseEvent = procedure(Sender: TObject; x, y: TlqCoord; var button: word;
                          var shiftstate: word) of object;


  { *******************************************
      Public event properties: Event Types
    *******************************************}
  { Keyboard }
  TKeyEvent = procedure(Sender: TObject; AKey: Word; AShift: TShiftState) of object;
  TKeyCharEvent = procedure(Sender: TObject; AKeyChar: Char) of object;
  TKeyPressEvent = procedure(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState; var Consumed: boolean) of object;
  { Mouse }
  TMouseButtonEvent = procedure(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint) of object;
  TMouseMoveEvent = procedure(Sender: TObject; AShift: TShiftState; const AMousePos: TPoint) of object;
  TMouseWheelEvent = procedure(Sender: TObject; AShift: TShiftState; AWheelDelta: Single; const AMousePos: TPoint) of object;
  { Painting }
  TPaintEvent = procedure(Sender: TObject{; const ARect: TlqRect}) of object;
  { Exceptions }
  TExceptionEvent = procedure(Sender: TObject; E: Exception) of object;


type
  TSizeParams = record
    min_width: TlqCoord;
    max_width: TlqCoord;
    min_height: TlqCoord;
    max_height: TlqCoord;
  end;


  TlqFontResource = class(TlqFontResourceImpl)
  protected
    FFontDesc: string;
    FRefCount: integer;
  public
    constructor Create(const afontdesc: string);
    function    IncRefCount: integer;
    function    DecRefCount: integer;
    property    FontDesc: string read FFontDesc;
  end;


  TlqFont = class(TlqFontBase)
  public
    constructor Create(afontres: TlqFontResource; const afontdesc: string);
    destructor  Destroy; override;
  end;


  // forward declaration
  TlqCanvas = class;
  TlqTimer = class;


  TlqWindow = class(TlqWindowImpl)
  protected
    //procedure   SetParent(const AValue: TlqWindow); reintroduce;
    //function    GetParent: TlqWindow; reintroduce;
    function    GetCanvas: TlqCanvas; reintroduce;
    function    CreateCanvas: TlqCanvasBase; virtual;

    // designer
    procedure   DoUpdateWindowPosition; override;
    procedure   DoAllocateWindowHandle(AParent: TlqWindowBase); override;
    //procedure   DoReleaseWindowHandle; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    //property    Parent: TlqWindow read GetParent write SetParent;
    property    Canvas: TlqCanvas read GetCanvas;
    property    WinHandle;  // surface this property from TlqXXXImpl class in it's native format
  end;


  TlqImage = class(TlqImageImpl)
  private
    function    GetScanLine(Row: Integer): Pointer;
  public
    function    CreateDisabledImage: TlqImage;
    function    ImageFromSource: TlqImage;
    function    ImageFromRect(var ARect: TRect): TlqImage; overload;
    function    ImageFromRect(var ARect: TlqRect): TlqImage; overload;
    property    ScanLine[Row: Integer]: Pointer read GetScanLine;
  end;


  TlqImages = class
  private
    FImages: TStringList;
  public
    constructor Create;
    destructor  Destroy; override;
    function    AddImage(const imgid: string; img: TlqImage): boolean;
    function    DeleteImage(const imgid: string; freeimg: boolean): boolean;
    function    GetImage(const imgid: string): TlqImage;
    function    AddBMP(const imgid: string; bmpdata: pointer; bmpsize: integer): TlqImage;
    function    AddMaskedBMP(const imgid: string; bmpdata: pointer; bmpsize: integer; mcx, mcy: integer): TlqImage;
    procedure   ListImages(var sl: TStringList);
  end;


  TlqCanvas = class(TlqCanvasImpl)
  private
    function    AddLineBreaks(const s: TlqString; aMaxLineWidth: integer): string;
  public
    constructor Create(awin: TlqWindowBase); override;
    destructor  Destroy; override;

    // As soon as TlqStyle has moved out of CoreLib, these must go!
    procedure   DrawButtonFace(x, y, w, h: TlqCoord; AFlags: TlqButtonFlags); overload;
    procedure   DrawButtonFace(r: TlqRect; AFlags: TlqButtonFlags); overload;
    procedure   DrawControlFrame(x, y, w, h: TlqCoord); overload;
    procedure   DrawControlFrame(r: TlqRect); overload;
    procedure   DrawBevel(x, y, w, h: TlqCoord; ARaised: Boolean = True); overload;
    procedure   DrawBevel(r: TlqRect; ARaised: Boolean = True); overload;
    procedure   DrawDirectionArrow(x, y, w, h: TlqCoord; direction: TArrowDirection); overload;
    procedure   DrawDirectionArrow(r: TlqRect; direction: TArrowDirection); overload;
    procedure   DrawFocusRect(r: TlqRect);
    function    DrawText(x, y, w, h: TlqCoord; const AText: TlqString; AFlags: TlqTextFlags = TextFlagsDflt; ALineSpace: integer = 2): integer; overload;
    function    DrawText(x, y: TlqCoord; const AText: TlqString; AFlags: TlqTextFlags = TextFlagsDflt; ALineSpace: integer = 2): integer; overload;
    function    DrawText(r: TlqRect; const AText: TlqString; AFlags: TlqTextFlags = TextFlagsDflt; ALineSpace: integer = 2): integer; overload;
  end;


  { This is very basic for now, just to remind us of theming support. Later we
    will rework this to use a Style Manager like the previous LiteKit.
    Also support Bitmap based styles for easier theme implementations. }
  TlqStyle = class(TObject)
  public
    DefaultFont: TlqFont;
    FixedFont: TlqFont;
    MenuFont: TlqFont;
    MenuAccelFont: TlqFont;
    MenuDisabledFont: TlqFont;
    constructor Create; virtual;
    destructor  Destroy; override;
    { General }
    procedure   DrawControlFrame(ACanvas: TlqCanvas; x, y, w, h: TlqCoord); virtual; overload;
    procedure   DrawControlFrame(ACanvas: TlqCanvas; r: TlqRect); overload;
    function    GetControlFrameBorders: TRect; virtual;
    procedure   DrawBevel(ACanvas: TlqCanvas; x, y, w, h: TlqCoord; ARaised: Boolean = True); virtual;
    procedure   DrawDirectionArrow(ACanvas: TlqCanvas; x, y, w, h: TlqCoord; direction: TArrowDirection); virtual;
    procedure   DrawString(ACanvas: TlqCanvas; x, y: TlqCoord; AText: string; AEnabled: boolean = True); virtual;
    procedure   DrawFocusRect(ACanvas: TlqCanvas; r: TlqRect); virtual;
    { Buttons }
    procedure   DrawButtonFace(ACanvas: TlqCanvas; x, y, w, h: TlqCoord; AFlags: TlqButtonFlags); virtual; overload;
    procedure   DrawButtonFace(ACanvas: TlqCanvas; r: TlqRect; AFlags: TlqButtonFlags); overload;
    function    GetButtonBorders: TRect; virtual;
    function    GetButtonShift: TPoint; virtual;
    function    HasButtonHoverEffect: boolean; virtual;
    { Menus }
    procedure   DrawMenuBar(ACanvas: TlqCanvas; r: TlqRect; ABackgroundColor: TlqColor); virtual;
    procedure   DrawMenuRow(ACanvas: TlqCanvas; r: TlqRect; AFlags: TlqMenuItemFlags); virtual;
    procedure   DrawMenuItem(ACanvas: TlqCanvas; r: TlqRect; AFlags: TlqMenuItemFlags; AText: TlqString); virtual;
    procedure   DrawMenuItemSeparator(ACanvas: TlqCanvas; r: TlqRect); virtual;
    procedure   DrawMenuItemImage(ACanvas: TlqCanvas; x, y: TlqCoord; r: TlqRect; AFlags: TlqMenuItemFlags); virtual;
    function    GetSeparatorSize: integer; virtual;
    { Editbox }
    procedure   DrawEditBox(ACanvas: TlqCanvas; const r: TlqRect; const IsEnabled: Boolean; const IsReadOnly: Boolean; const ABackgroundColor: TlqColor); virtual;
  end;
  

  TMsgHookItem = class
    Dest: TObject;
    Listener: TObject;
    MsgCode: integer;
  end;


  TlqApplication = class(TlqApplicationImpl)
  private
    FHintPause: Integer;
    FShowHint: boolean;
    FOnException: TExceptionEvent;
    FStopOnException: Boolean;
    FHintWindow: TlqWindow;
    FHintTimer: TlqTimer;
    FHintWidget: TlqWindow;
    FHintPos: TPoint;
    FOnKeyPress: TKeyPressEvent;
    FStartDragDistance: integer;
    procedure   SetHintPause(const AValue: Integer);
    procedure   SetupLocalizationStrings;
    procedure   InternalMsgFreeMe(var msg: TlqMessageRec); message LQM_FREEME;
    procedure   InternalMsgHintTimer(var msg: TlqMessageRec); message LQM_HINTTIMER;
    procedure   CreateHintWindow;
    procedure   HintTimerFired(Sender: TObject);
    procedure   SetShowHint(const AValue: boolean);
    procedure   SetStartDragDistance(const AValue: integer);
  protected
    FDisplayParams: string;
    FScreenWidth: integer;
    FScreenHeight: integer;
    FDefaultFont: TlqFont;
    FFontResList: TList;
    FMessageHookList: TFPList;
    procedure   FreeFontRes(afontres: TlqFontResource);
    procedure   InternalInit;
    procedure   RunMessageLoop;
    procedure   WaitWindowMessage(atimeoutms: integer);
  public
    constructor Create(const AParams: string = ''); override;
    destructor  Destroy; override;
    procedure   CreateForm(InstanceClass: TComponentClass; out Reference);
    function    GetFont(const afontdesc: TlqString): TlqFont;
    procedure   ActivateHint(APos: TPoint; AHint: TlqString);
    procedure   RecreateHintWindow;
    procedure   Flush;
    procedure   HandleException(Sender: TObject);
    procedure   HideHint;
    procedure   Initialize;
    procedure   ProcessMessages;
    procedure   Run;
    procedure   SetMessageHook(AWidget: TObject; const AMsgCode: integer; AListener: TObject);
    procedure   ShowException(E: Exception);
    procedure   UnsetMessageHook(AWidget: TObject; const AMsgCode: integer; AListener: TObject);
    property    DefaultFont: TlqFont read FDefaultFont;
    property    HintPause: Integer read FHintPause write SetHintPause;
    property    HintWindow: TlqWindow read FHintWindow;
    property    ScreenWidth: integer read FScreenWidth;
    property    ScreenHeight: integer read FScreenHeight;
    property    ShowHint: boolean read FShowHint write SetShowHint default True;
    property    StartDragDistance: integer read FStartDragDistance write SetStartDragDistance default 5;
    property    StopOnException: Boolean read FStopOnException write FStopOnException;
    property    OnException: TExceptionEvent read FOnException write FOnException;
    property    OnKeyPress: TKeyPressEvent read FOnKeyPress write FOnKeyPress;
  end;


  TlqTimer = class(TlqTimerImpl)
  public
    { AInterval is in milliseconds. }
    constructor Create(AInterval: integer); override;
    destructor  Destroy; override;
  end;


  { Caret or text cursor, inverts painting over text and has blinking support. }
  TlqCaret = class(TObject)
  private
    FEnabled: boolean;
    FVisible: boolean;
    FInterval: integer;
    FCanvas: TlqCanvas;
    FTop: TlqCoord;
    FLeft: TlqCoord;
    FWidth: TlqCoord;
    FHeight: TlqCoord;
    FTimer: TlqTimer;
    procedure   OnTimerTime(Sender: TObject);
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   SetCaret(acanvas: TlqCanvas; x, y, w, h: TlqCoord);
    procedure   UnSetCaret(acanvas: TlqCanvas);
    procedure   InvertCaret;
    property    Width: integer read FWidth;
    property    Height: integer read FHeight;
  end;
  
  
  TlqClipboard = class(TlqClipboardImpl)
  end;

  
  TlqFileList = class(TlqFileListImpl)
  end;


  TlqMimeData = class(TlqMimeDataImpl)
  end;


  TlqDrag = class(TlqDragImpl)
  private
    FTarget: TlqWinHandle;
    procedure   SetMimeData(const AValue: TlqMimeDataBase);
  protected
    function    GetSource: TlqWindow; reintroduce;
  public
    constructor Create(ASource: TlqWindow);
    function    Execute(const ADropActions: TlqDropActions = [daCopy]; const ADefaultAction: TlqDropAction = daCopy): TlqDropAction; override;
    property    Source: TlqWindow read GetSource;
    property    Target: TlqWinHandle read FTarget write FTarget;
    property    MimeData: TlqMimeDataBase read FMimeData write SetMimeData;
  end;


var
  lqStyle:  TlqStyle;   { TODO -ograemeg : move this into lqApplication }
  lqCaret:  TlqCaret;   { TODO -ograemeg : move this into lqApplication }
  lqImages: TlqImages;  { TODO -ograemeg : move this into lqApplication }

  //LQHIDEALLWINDOW : Boolean = False;
  LQU_PREVENTWND  : Boolean = False;    //whether AllocateWinHandle required. True = all done by Designer
  DefaultCanvasClass: TlqCanvasBaseClass = nil;

// Application & Clipboard singletons
function  lqApplication: TlqApplication;
function  lqClipboard: TlqClipboard;


// Fonts (easy access function)
function  lqGetFont(const afontdesc: TlqString): TlqFont;
procedure lqFontDescValues(Proc: TGetStrProc);

// Message Queue  (easy access function)
procedure lqWaitWindowMessage;
procedure lqPostMessage(Sender, Dest: TObject; MsgCode: integer; var aparams: TlqMessageParams); overload;
procedure lqPostMessage(Sender, Dest: TObject; MsgCode: integer); overload;
procedure lqSendMessage(Sender, Dest: TObject; MsgCode: integer; var aparams: TlqMessageParams); overload;
procedure lqSendMessage(Sender, Dest: TObject; MsgCode: integer); overload;
procedure lqDeliverMessage(var msg: TlqMessageRec);
procedure lqDeliverMessages;
function  lqGetFirstMessage: PfpgMessageRec;
procedure lqDeleteFirstMessage;

// Color & Font routines
function  lqColorToRGB(col: TlqColor): TlqColor;
function  lqGetNamedColor(col: TlqColor): TlqColor;
procedure lqSetNamedColor(colorid, rgbvalue: longword);
function  lqIsNamedColor(col: TlqColor): boolean;
function  lqGetNamedFontDesc(afontid: string): string;
procedure lqSetNamedFont(afontid, afontdesc: string);
function  lqGetNamedFontList: TStringlist;

// Timers rountines
procedure lqInitTimers;
function  lqCheckTimers: Boolean;
procedure lqResetAllTimers;
function  lqClosestTimer(ctime: TDateTime; amaxtime: integer): integer;
function  lqGetTickCount: DWord;
procedure lqPause(MilliSeconds: Cardinal);

// Rectangle, Point & Size routines
function  InflateRect(var Rect: TRect; dx: Integer; dy: Integer): Boolean;
function  InflateRect(var Rect: TlqRect; dx: Integer; dy: Integer): Boolean;
function  OffsetRect(var Rect: TRect; dx: Integer; dy: Integer): Boolean;
function  OffsetRect(var Rect: TlqRect; dx: Integer; dy: Integer): Boolean;
function  CenterPoint(const Rect: TRect): TPoint;
function  CenterPoint(const Rect: TlqRect): TPoint;
function  lqRect(ALeft, ATop, AWidth, AHeight: integer): TlqRect;
function  lqRectToRect(const ARect: TlqRect): TRect;
function  lqPoint(const AX, AY: integer): TlqPoint;
function  lqSize(const AWidth, AHeight: integer): TlqSize;

// Debug rountines
procedure PrintRect(const Rect: TRect);
procedure PrintRect(const Rect: TlqRect);
procedure PrintCoord(const x, y: TlqCoord);
procedure PrintCoord(const pt: TPoint);
function  PrintCallTrace(const AClassName, AMethodName: string): IInterface;
procedure PrintCallTraceDbgLn(const AMessage: string);
procedure DumpStack;
procedure DumpStack(var AList: TStrings);

{ These methods are safe to use even on Windows gui applications. }
procedure DebugLn(const s1: TlqString);
procedure DebugLn(const s1, s2: TlqString);
procedure DebugLn(const s1, s2, s3: TlqString);
procedure DebugLn(const s1, s2, s3, s4: TlqString);
procedure DebugLn(const s1, s2, s3, s4, s5: TlqString);
function DebugMethodEnter(const s1: TlqString): IInterface;
procedure DebugSeparator;

// operator overloading of some useful structures
operator = (a: TRect; b: TRect): boolean;
operator = (const ASize1, ASize2: TlqSize) b: Boolean;
operator = (const APoint1, APoint2: TPoint) b: Boolean;
operator + (const APoint1, APoint2: TPoint) p: TPoint;
operator + (const APoint1, APoint2: TlqPoint) p: TlqPoint;
operator + (const APoint: TPoint; ASize: TlqSize) p: TPoint;
operator + (const APoint: TlqPoint; ASize: TlqSize) p: TlqPoint;
operator + (const ASize: TlqSize; APoint: TPoint) s: TlqSize;
operator + (const ASize: TlqSize; APoint: TlqPoint) s: TlqSize;
operator + (const ASize1, ASize2: TlqSize) s: TlqSize;
operator + (const APoint: TPoint; i: Integer) p: TPoint;
operator + (const APoint: TlqPoint; i: Integer) p: TlqPoint;
operator + (const ASize: TlqSize; i: Integer) s: TlqSize;
operator - (const APoint1, APoint2: TPoint) p: TPoint;
operator - (const APoint1, APoint2: TlqPoint) p: TlqPoint;
operator - (const APoint: TPoint; i: Integer) p: TPoint;
operator - (const APoint: TlqPoint; i: Integer) p: TlqPoint;
operator - (const ASize: TlqSize; const APoint: TPoint) s: TlqSize;
operator - (const ASize: TlqSize; const APoint: TlqPoint) s: TlqSize;
operator - (const ASize: TlqSize; i: Integer) s: TlqSize;
operator = (const AColor1, AColor2: TFPColor) b: Boolean;


implementation

uses
  strutils,
  math,
{$ifdef AGGCanvas}
  Agg2D,
{$endif}
{$IFDEF DEBUG}
  dbugintf,
{$ENDIF}
  lq_imgfmt_bmp,
  lq_stdimages,
  lq_translations,
  lq_widget,
  lq_dialogs,
  lq_hint,
  lq_extgraphics,
  lq_utils,
  lq_cmdlineparams,
  lq_imgutils,
  lq_stylemanager,
  lq_form,          //for InitInheritedComponent
  lq_style_win2k,   // :TODO This needs to be !removed
  lq_style_motif;   // :TODO This needs to be !removed

var
  lqTimers: TList;
  lqNamedColors: array[0..255] of TlqColor;
  lqNamedFonts: TList;
  uApplication: TlqApplication;
  uClipboard: TlqClipboard;
  uMsgQueueList: TList;
  uDebugText: ^Text;
  uDebugTextAllocated: Boolean;
  uDebugIndent: integer;

type

  TDebugMethodHelper = class(TInterfacedObject)
  private
    FMethod: string;
  public
    constructor Create(const AMethodName: string);
    destructor Destroy; override;
  end;


  TNamedFontItem = class
  public
    FontID: string;
    FontDesc: string;
    constructor Create(AFontID, AFontDesc: string);
  end;


  TWidgetFriend = class(TlqWidget);


{ TDebugMethodHelper }

constructor TDebugMethodHelper.Create(const AMethodName: string);
begin
  inherited Create;
  FMethod := AMethodName;
  DebugLn('>> ' + FMethod);
  uDebugIndent := uDebugIndent + 2;
end;

destructor TDebugMethodHelper.Destroy;
begin
  uDebugIndent := uDebugIndent - 2;
  DebugLn('<< ' + FMethod);
  inherited Destroy;
end;

  // so we can get access to the Protected section

constructor TNamedFontItem.Create(AFontID, AFontDesc: string);
begin
  FontID   := AFontID;
  FontDesc := AFontDesc;
end;

{$include lq_msgqueue.inc}

// Timer support

procedure lqInitTimers;
begin
  if lqTimers = nil then
    lqTimers := TList.Create;
end;

function lqCheckTimers: Boolean;
var
  i: integer;
  ctime: TDateTime;
begin
  if lqTimers = nil then
    Exit;
  ctime := now;
  i := lqTimers.Count;
  Result := i > 0;
  while i > 0 do
  begin
    dec(i);
    if lqTimers[i] = nil then
      lqTimers.Delete(i)
    else
      TlqTimer(lqTimers[i]).CheckAlarm(ctime);
  end;
end;

procedure lqResetAllTimers;
var
  i: integer;
begin
  if lqTimers = nil then
    Exit;
  for i := 0 to lqTimers.Count-1 do
    TlqTimer(lqTimers[i]).Reset;
end;

function lqClosestTimer(ctime: TDateTime; amaxtime: integer): integer;
var
  i: integer;
  t: TlqTimer;
  dt: TDateTime;
  tb: Boolean;
begin
  if lqTimers = nil then
    Exit;
  // returns -1 if no timers are pending
  dt := ctime + amaxtime * ONE_MILISEC;
  tb := False;

  for i := 0 to lqTimers.Count-1 do
  begin
    t := TlqTimer(lqTimers[i]);
    if (t <> nil) and t.Enabled and (t.NextAlarm < dt) then
    begin
      dt := t.NextAlarm;
      tb := True;
    end;
  end;

  if tb then
  begin
    Result := trunc(0.5 + (dt - ctime) / ONE_MILISEC);
    if Result < 0 then
      Result := 0;
  end
  else
    Result := -1;
end;

function lqGetTickCount: DWord;
begin
  Result := DWord(Trunc(Now * MSecsPerDay));
end;

{ blocking function for the caller, but still processes framework messages }
procedure lqPause(MilliSeconds: Cardinal);
var
  lStart: TDateTime;
begin
   lStart := Now * MSecsPerDay;
   repeat
     lqApplication.ProcessMessages;
   until ((Now*MSecsPerDay)-lStart) > MilliSeconds;
end;


function InflateRect(var Rect: TRect; dx: Integer; dy: Integer): Boolean;
begin
  if Assigned(@Rect) then
  begin
    with Rect do
    begin
      dec(Left, dx);
      dec(Top, dy);
      inc(Right, dx);
      inc(Bottom, dy);
    end;
    Result := True;
  end
  else
    Result := False;
end;

function InflateRect(var Rect: TlqRect; dx: Integer; dy: Integer): Boolean;
begin
  if Assigned(@Rect) then
  begin
    dec(Rect.Left, dx);
    dec(Rect.Top, dy);
    inc(Rect.Width, 2*dx);
    inc(Rect.Height, 2*dy);
    Result := True;
  end
  else
    Result := False;
end;

function OffsetRect(var Rect: TRect; dx: Integer; dy: Integer): Boolean;
begin
  if Assigned(@Rect) then
  begin
    with Rect do
    begin
      inc(Left, dx);
      inc(Top, dy);
      inc(Right, dx);
      inc(Bottom, dy);
    end;
    OffsetRect := True;
  end
  else
    OffsetRect := False;
end;

function OffsetRect(var Rect: TlqRect; dx: Integer; dy: Integer): Boolean;
begin
  if Assigned(@Rect) then
  begin
    with Rect do
    begin
      inc(Left, dx);
      inc(Top, dy);
    end;
    OffsetRect := True;
  end
  else
    OffsetRect := False;
end;

function CenterPoint(const Rect: TRect): TPoint;
begin
  Result.X := (Rect.Left + Rect.Right) div 2;
  Result.Y := (Rect.Top + Rect.Bottom) div 2;
end;

function CenterPoint(const Rect: TlqRect): TPoint;
begin
  Result.X := (Rect.Left + Rect.Right) div 2;
  Result.Y := (Rect.Top + Rect.Bottom) div 2;
end;

function lqRect(ALeft, ATop, AWidth, AHeight: integer): TlqRect;
begin
  Result.SetRect(ALeft, ATop, AWidth, AHeight);
end;

function lqRectToRect(const ARect: TlqRect): TRect;
begin
  Result.Left   := ARect.Left;
  Result.Top    := ARect.Top;
  Result.Right  := ARect.Right;
  Result.Bottom := ARect.Bottom;
end;

function lqPoint(const AX, AY: integer): TlqPoint;
begin
  Result.SetPoint(AX, AY);
end;

function lqSize(const AWidth, AHeight: integer): TlqSize;
begin
  Result.SetSize(AWidth, AHeight);
end;

procedure InitializeDebugOutput;
var
  DebugFileName: string;

  function GetDebugFileName: string;
  var
    EnvVarName: string;
  begin
    Result := '';
    // first try to find the log file name in the command line parameters
    if gCommandLineParams.IsParam('debuglog') then
      Result := gCommandLineParams.GetParam('debuglog')
    else
    begin
      // if not found yet, then try to find in the environment variable
      EnvVarName  := ApplicationName + '_debuglog';
      Result      := GetEnvironmentVariable(EnvVarName);
    end;
    if (Result <> '') then
      Result := lqExpandFileName(Result);
  end;

begin
  uDebugIndent := 0;
  uDebugText := nil;
  DebugFileName := GetDebugFileName;
  if (DebugFileName <> '') and
     (lqDirectoryExists(lqExtractFileDir(DebugFileName))) then
  begin
    new(uDebugText);
    try
      Assign(uDebugText^, DebugFileName);
      if lqFileExists(DebugFileName) then
        Append(uDebugText^)
      else
        Rewrite(uDebugText^);
    except
      Freemem(uDebugText);
      uDebugText := nil;
      // Add extra line ending: a dialog will be shown in Windows gui application
      writeln(StdOut, 'Cannot open file: ', DebugFileName+LineEnding);
    end;
  end;
  if uDebugText = nil then
  begin
    if TextRec(Output).Mode = fmClosed then
      uDebugText := nil
    else
      uDebugText := @Output;
    uDebugTextAllocated := False;
  end else
    uDebugTextAllocated := True;
end;

procedure CloseDebugOutput;
begin
  if uDebugTextAllocated then
  begin
    Close(uDebugText^);
    Dispose(uDebugText);
    uDebugTextAllocated := False;
  end;
  uDebugText := nil;
end;

procedure FinalizeDebugOutput;
begin
  CloseDebugOutput;
end;

procedure PrintRect(const Rect: TRect);
begin
  writeln('Rect left=', Rect.Left, ' top=', Rect.Top, ' right=', Rect.Right,
      ' bottom=', Rect.Bottom);
end;

procedure PrintRect(const Rect: TlqRect);
begin
  writeln('Rect left=', Rect.Left, ' top=', Rect.Top, ' right=', Rect.Right,
      ' bottom=', Rect.Bottom, ' width=', Rect.Width, ' height=', Rect.Height);
end;

procedure PrintCoord(const x, y: TlqCoord);
begin
  writeln('x=', x, '  y=', y);
end;

var
  iCallTrace: integer;

type
  TPrintCallTrace = class(TInterfacedObject)
  private
    FClassName: string;
    FMethodName: string;
    spacing: string;
  public
    constructor Create(const AClassName, AMethodName: string);
    destructor Destroy; override;
  end;

{ TPrintCallTrace }

constructor TPrintCallTrace.Create(const AClassname, AMethodName: string);
var
  i: integer;
begin
  inherited Create;
  spacing := '';
  inc(iCallTrace);
  for i := 0 to iCallTrace do
    spacing += '  ';
  FClassName := AClassName;
  FMethodName := AMethodName;
  {$IFDEF DEBUG}
  SendDebug(Format('%s>> %s.%s', [spacing, FClassName, FMethodName]));
  {$ENDIF}
end;

destructor TPrintCallTrace.Destroy;
begin
  {$IFDEF DEBUG}
  SendDebug(Format('%s<< %s.%s', [spacing, FClassName, FMethodName]));
  {$ENDIF}
  dec(iCallTrace);
  inherited Destroy;
end;

procedure PrintCoord(const pt: TPoint);
begin
  PrintCoord(pt.X, pt.Y);
end;

function PrintCallTrace(const AClassName, AMethodName: string): IInterface;
begin
  Result := TPrintCallTrace.Create(AClassName, AMethodName);
end;

procedure PrintCallTraceDbgLn(const AMessage: string);
var
  i: integer;
  s: string;
begin
  s := '';
  for i := 0 to iCallTrace+1 do
    s += '  ';
  writeln(s + AMessage);
end;

procedure DumpStack;
var
  lMessage: String;
  i: longint;
begin
  writeln(' Stack trace:');
//  Dump_Stack(StdOut, get_frame);

  Writeln(stdout,'An unhandled exception occurred at $',HexStr(Ptrint(ExceptAddr),sizeof(PtrInt)*2),' :');
  if ExceptObject is Exception then
   begin
     lMessage := Exception(ExceptObject).ClassName+' : '+Exception(ExceptObject).Message;
     Writeln(stdout,lMessage);
   end
  else
   Writeln(stdout,'Exception object ',ExceptObject.ClassName,' is not of class Exception.');
  Writeln(stdout,BackTraceStrFunc(ExceptAddr));
  if (ExceptFrameCount>0) then
    begin
      for i := 0 to ExceptFrameCount-1 do
        Writeln(stdout,BackTraceStrFunc(ExceptFrames[i]));
    end;
  Writeln(stdout,'');
end;

procedure DumpStack(var AList: TStrings);
var
  lMessage: String;
  i: longint;
begin
  AList.Add(' Stack trace:');
  AList.Add('An unhandled exception occurred at $' + HexStr(PtrInt(ExceptAddr),sizeof(PtrInt)*2) + ' :');
  if ExceptObject is Exception then
  begin
    lMessage := Exception(ExceptObject).ClassName+' : '+Exception(ExceptObject).Message;
    AList.Add(lMessage);
  end
  else
    AList.Add('Exception object ' + ExceptObject.ClassName + ' is not of class Exception.');
  AList.Add(BackTraceStrFunc(ExceptAddr));
  if (ExceptFrameCount>0) then
  begin
    for i := 0 to ExceptFrameCount-1 do
      AList.Add(BackTraceStrFunc(ExceptFrames[i]));
  end;
  AList.Add('');
end;

procedure DebugLn(const s1: TlqString);
var
  s: string;
begin
  if not Assigned(uDebugText) then
    Exit; //==>
  s := DupeString(' ', uDebugIndent);
  writeln(uDebugText^, s + lqConvertLineEndings(s1));
end;

procedure DebugLn(const s1, s2: TlqString);
begin
  DebugLn(s1 + ' ' + s2);
end;

procedure DebugLn(const s1, s2, s3: TlqString);
begin
  DebugLn(s1 + ' ' + s2  + ' ' + s3);
end;

procedure DebugLn(const s1, s2, s3, s4: TlqString);
begin
  DebugLn(s1 + ' ' + s2 + ' ' + s3 + ' ' + s4);
end;

procedure DebugLn(const s1, s2, s3, s4, s5: TlqString);
begin
  DebugLn(s1 + ' ' + s2 + ' ' + s3 + ' ' + s4 + ' ' + s5);
end;

function DebugMethodEnter(const s1: TlqString): IInterface;
begin
  Result := TDebugMethodHelper.Create(s1);
end;

procedure DebugSeparator;
begin
  DebugLn('>--------------------------<');
end;

operator = (a: TRect; b: TRect): boolean;
begin
  if    (a.Top    = b.Top)
    and (a.Left   = b.Left)
    and (a.Bottom = b.Bottom)
    and (a.Right  = b.Right) then
    Result := True
  else
    Result := False;
end;

operator = (const ASize1, ASize2: TlqSize) b: Boolean;
begin
  b := (ASize1.w = ASize2.w) and (ASize1.h = ASize2.h);
end;

operator = (const APoint1, APoint2: TPoint) b: Boolean;
begin
  b := (APoint1.X = APoint2.X) and (APoint1.Y = APoint2.Y);
end;

operator + (const APoint1, APoint2: TPoint) p: TPoint;
begin
  p.x := APoint1.x + APoint2.x;
  p.y := APoint1.y + APoint2.y;
end;

operator + (const APoint1, APoint2: TlqPoint) p: TlqPoint;
begin
  p.x := APoint1.x + APoint2.x;
  p.y := APoint1.y + APoint2.y;
end;

operator + (const APoint: TPoint; ASize: TlqSize) p: TPoint;
begin
  p.x := APoint.x + ASize.w;
  p.y := APoint.y + ASize.h;
end;

operator + (const APoint: TlqPoint; ASize: TlqSize) p: TlqPoint;
begin
  p.x := APoint.x + ASize.w;
  p.y := APoint.y + ASize.h;
end;

operator + (const ASize: TlqSize; APoint: TPoint) s: TlqSize;
begin
  s.w := ASize.w + APoint.x;
  s.h := ASize.h + APoint.y;
end;

operator + (const ASize: TlqSize; APoint: TlqPoint) s: TlqSize;
begin
  s.w := ASize.w + APoint.x;
  s.h := ASize.h + APoint.y;
end;

operator + (const ASize1, ASize2: TlqSize) s: TlqSize;
begin
  s.w := ASize1.w + ASize2.w;
  s.h := ASize1.h + ASize2.h;
end;

operator + (const APoint: TPoint; i: Integer) p: TPoint;
begin
  p.x := APoint.x + i;
  p.y := APoint.y + i;
end;

operator + (const APoint: TlqPoint; i: Integer) p: TlqPoint;
begin
  p.x := APoint.x + i;
  p.y := APoint.y + i;
end;

operator + (const ASize: TlqSize; i: Integer) s: TlqSize;
begin
  s.w := ASize.w + i;
  s.h := ASize.h + i;
end;

operator - (const APoint1, APoint2: TPoint) p: TPoint;
begin
  p.x := APoint1.x - APoint2.x;
  p.y := APoint1.y - APoint2.y;
end;

operator - (const APoint1, APoint2: TlqPoint) p: TlqPoint;
begin
  p.x := APoint1.x - APoint2.x;
  p.y := APoint1.y - APoint2.y;
end;

operator - (const APoint: TPoint; i: Integer) p: TPoint;
begin
  p.x := APoint.x - i;
  p.y := APoint.y - i;
end;

operator - (const APoint: TlqPoint; i: Integer) p: TlqPoint;
begin
  p.x := APoint.x - i;
  p.y := APoint.y - i;
end;

operator - (const ASize: TlqSize; const APoint: TPoint) s: TlqSize;
begin
  s.w := ASize.w - APoint.x;
  s.h := ASize.h - APoint.y;
end;

operator - (const ASize: TlqSize; const APoint: TlqPoint) s: TlqSize;
begin
  s.w := ASize.w - APoint.x;
  s.h := ASize.h - APoint.y;
end;

operator - (const ASize: TlqSize; i: Integer) s: TlqSize;
begin
  s.w := ASize.w - i;
  s.h := ASize.h - i;
end;

operator = (const AColor1, AColor2: TFPColor) b: Boolean;
begin
  b := (AColor1.Red = AColor2.Red)
        and (AColor1.Green = AColor2.Green)
        and (AColor1.Blue = AColor2.Blue)
        and (AColor1.Alpha = AColor2.Alpha);
end;

{ TlqTimer }

constructor TlqTimer.Create(AInterval: integer);
begin
  inherited Create(AInterval);
  lqTimers.Add(self);
end;

destructor TlqTimer.Destroy;
var
  i: integer;
begin
  i := lqTimers.IndexOf(self);
  if i > -1 then
    lqTimers[i] := nil; // we free the item in lqCheckTimers
  inherited Destroy;
end;


function lqApplication: TlqApplication;
begin
  if not Assigned(uApplication) then
    uApplication := TlqApplication.Create;
  result := uApplication;
end;


function lqClipboard: TlqClipboard;
begin
  if not Assigned(uClipboard) then
    uClipboard := TlqClipboard.Create;
  Result := uClipboard;
end;


function lqColorToRGB(col: TlqColor): TlqColor;
begin
  if (((col shr 24) and $FF) = $80) and ((col and $FFFFFF) <= $FF) then
    Result := lqNamedColors[col and $FF] or (col and $7F000000)// named color keeping alpha
  else
    Result := col;
end;


function lqGetNamedColor(col: TlqColor): TlqColor;
begin
  if lqIsNamedColor(col) then
    Result := col  // nothing to do, it is already a named color
  else
    Result := lqNamedColors[col and $FF];
end;

procedure lqSetNamedColor(colorid, rgbvalue: longword);
var
  i: longword;
begin
  if (colorid and cl_BaseNamedColor) = 0 then
    Exit;
  i := colorid and $FF;
  lqNamedColors[i] := rgbvalue;
end;

function lqIsNamedColor(col: TlqColor): boolean;
begin
  if (((col shr 24) and $FF) = $80) and ((col and $FFFFFF) <= $FF) then
    Result := True
  else
    Result := False;
end;

function lqGetNamedFontDesc(afontid: string): string;
var
  n: integer;
begin
  for n := 0 to lqNamedFonts.Count - 1 do
    if (lowercase(TNamedFontItem(lqNamedFonts[n]).FontID) = lowercase(afontid)) then
    begin // found
      Result := TNamedFontItem(lqNamedFonts[n]).FontDesc;
      Exit; //==>
    end;

  {$IFDEF DEBUG}
  SendDebug('GetNamedFontDesc error: "' + afontid + '" is missing. Default is used.');
  {$ENDIF}
  Result := LQ_DEFAULT_FONT_DESC;
end;

procedure lqSetNamedFont(afontid, afontdesc: string);
var
  n: integer;
begin
  n := 0;
  while (n < lqNamedFonts.Count) and (lowercase(TNamedFontItem(lqNamedFonts[n]).FontID) <> lowercase(afontid)) do
    Inc(n);

  if n < lqNamedFonts.Count then
    TNamedFontItem(lqNamedFonts[n]).FontDesc := afontdesc// already defined
  else
    lqNamedFonts.Add(TNamedFontItem.Create(afontid, afontdesc));
end;

function lqGetNamedFontList: TStringlist;
var
  n: integer;
  oFont: TNamedFontItem;
begin
  if lqNamedFonts.Count > 0 then
    Result := TStringList.Create
  else
    Exit; //==>
  
  for n := 0 to lqNamedFonts.Count-1 do
  begin
    oFont := TNamedFontItem(lqNamedFonts[n]);
    Result.Add(Format('#%s=%s', [oFont.FontID, oFont.FontDesc]));
  end;
end;

procedure lqWaitWindowMessage;
begin
  lqApplication.WaitWindowMessage(500);
end;

function lqGetFont(const afontdesc: TlqString): TlqFont;
begin
  Result := lqApplication.GetFont(afontdesc);
end;

procedure lqFontDescValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  for I := 0 to lqNamedFonts.Count-1 do
      Proc('#'+TNamedFontItem(lqNamedFonts[I]).FontID);
end;

constructor TlqApplication.Create(const AParams: string);
begin
  lqNamedFonts := TList.Create;
  FFontResList    := TList.Create;
  FDisplayParams  := AParams;
  FScreenWidth    := -1;
  FScreenHeight   := -1;
  FMessageHookList := TFPList.Create;
  FStopOnException := False;
  FHintWindow     := nil;   // popup window with Hint text
  FHintPause      := DEFAULT_HINT_PAUSE;
  FHintWidget     := nil;   // widget the mouse is over and whos hint text we need.
  FShowHint       := True;
  FStartDragDistance := 5; // pixels

  try
    inherited Create(AParams);
    if IsInitialized then
    begin
      FScreenWidth  := GetScreenWidth;
      FScreenHeight := GetScreenHeight;
    end;
  except
    on E: Exception do
      SysUtils.ShowException(ExceptObject, ExceptAddr);
  end;
end;

destructor TlqApplication.Destroy;
var
  i: integer;
begin
  if Assigned(FHintWindow) then
  begin
    HideHint;
    FHintWindow.Free;
    FHintWindow := nil;
  end;
  if Assigned(FHintTimer) then
  begin
  FHintTimer.Enabled := False;
  FHintTimer.OnTimer := nil;
  FHintTimer.Free;
  end;

  DestroyComponents;  // while message queue is still active

  for i := 0 to (lqNamedFonts.Count - 1) do
    TNamedFontItem(lqNamedFonts.Items[i]).Free;
  lqNamedFonts.Free;

  lqImages.Free;
  lqStyleManager.FreeStyleInstance;
  lqStyle := nil;
  lqCaret.Free;
  
  for i := lqTimers.Count-1 downto 0 do
    if lqTimers[i] <> nil then
      TlqTimer(lqTimers[i]).Free;
  lqTimers.Free;

  FDefaultFont.Free;

  for i := FFontResList.Count-1 downto 0 do
  begin
    TlqFontResource(FFontResList[i]).Free;
    FFontResList.Delete(i);
  end;
  FFontResList.Free;

  FreeAndNil(FModalFormStack);
  
  for i := 0 to FMessageHookList.Count-1 do
    TMsgHookItem(FMessageHookList[i]).Free;
  FreeAndNil(FMessageHookList);

  for i := uMsgQueueList.Count-1 downto 0 do
  begin
    TMessageListElement(uMsgQueueList[i]).Free;
    uMsgQueueList.Delete(i);
  end;
  uMsgQueueList.Free;
  
  inherited Destroy;
end;

function TlqApplication.GetFont(const afontdesc: TlqString): TlqFont;
var
  fr: TlqFontResource;
  n: integer;
  fdesc: TlqString;
begin
  fdesc := afontdesc;

  if copy(fdesc, 1, 1) = '#' then   // A # (hash) denotes a named font
    fdesc := lqGetNamedFontDesc(copy(afontdesc, 2, length(afontdesc)));

  Result := nil;

  for n := 0 to FFontResList.Count - 1 do
    if TlqFontResource(FFontResList[n]).FontDesc = fdesc then
    begin
      fr     := TlqFontResource(FFontResList[n]);
      Inc(fr.FRefCount);
      Result := TlqFont.Create(fr, afontdesc);
      Exit; //==>
    end;

  fr := TlqFontResource.Create(fdesc);

  if fr.HandleIsValid then
  begin
    FFontResList.Add(fr);
    Result := TlqFont.Create(fr, afontdesc);
  end
  else
  begin
    fr.Free;
    {$IFDEF DEBUG}
    SendDebug('fpGFX: Error opening font.');
    {$ENDIF}
  end;
end;

procedure TlqApplication.ActivateHint(APos: TPoint; AHint: TlqString);
var
  wnd: TlqHintWindow;
  w: Integer;
  h: Integer;
begin
  wnd := TlqHintWindow(FHintWindow);
  if Assigned(wnd) and wnd.Visible then
    Exit; //==>  Nothing to do

  wnd.Text := AHint;
  w := wnd.Font.TextWidth(AHint) + (wnd.Border * 2) + (wnd.Margin * 2);
  h := wnd.Font.Height + (wnd.Border * 2) + (wnd.Margin * 2);
  { prevents hint from going off the right screen edge }
  if (APos.X + w) > ScreenWidth then
  begin
    APos.X:= ScreenWidth - w;
    // just a few more sanity checks
    if APos.X < 0 then
      APos.X := 0;
    if w > ScreenWidth then
      w := ScreenWidth;
  end;
  wnd.SetPosition(APos.X, APos.Y, w, h);
  wnd.UpdateWindowPosition;
  wnd.Show;
end;

procedure TlqApplication.RecreateHintWindow;
begin
  if Assigned(FHintWindow) then
  begin
    HideHint;
    FHintWindow.Free;
    FHintWindow := nil;
  end;
  CreateHintWindow;
end;

procedure TlqApplication.Initialize;
begin
  { TODO : Remember to process parameters!! }
  if IsInitialized then
    InternalInit
  else
    raise Exception.Create('Failed to initialize the Application object!');
end;

procedure TlqApplication.Run;
begin
  repeat
    try
      RunMessageLoop;
    except
      HandleException(Self);
    end;
  until Terminated;
end;

procedure TlqApplication.SetupLocalizationStrings;
begin
  // setup internal FPC arrays with localized values
  ShortDayNames[1] := rsShortSun;
  ShortDayNames[2] := rsShortMon;
  ShortDayNames[3] := rsShortTue;
  ShortDayNames[4] := rsShortWed;
  ShortDayNames[5] := rsShortThu;
  ShortDayNames[6] := rsShortFri;
  ShortDayNames[7] := rsShortSat;
  
  LongDayNames[1] := rsLongSun;
  LongDayNames[2] := rsLongMon;
  LongDayNames[3] := rsLongTue;
  LongDayNames[4] := rsLongWed;
  LongDayNames[5] := rsLongThu;
  LongDayNames[6] := rsLongFri;
  LongDayNames[7] := rsLongSat;

  ShortMonthNames[1] := rsShortJan;
  ShortMonthNames[2] := rsShortFeb;
  ShortMonthNames[3] := rsShortMar;
  ShortMonthNames[4] := rsShortApr;
  ShortMonthNames[5] := rsShortMay;
  ShortMonthNames[6] := rsShortJun;
  ShortMonthNames[7] := rsShortJul;
  ShortMonthNames[8] := rsShortAug;
  ShortMonthNames[9] := rsShortSep;
  ShortMonthNames[10] := rsShortOct;
  ShortMonthNames[11] := rsShortNov;
  ShortMonthNames[12] := rsShortDec;

  LongMonthNames[1] := rsLongJan;
  LongMonthNames[2] := rsLongFeb;
  LongMonthNames[3] := rsLongMar;
  LongMonthNames[4] := rsLongApr;
  LongMonthNames[5] := rsLongMay;
  LongMonthNames[6] := rsLongJun;
  LongMonthNames[7] := rsLongJul;
  LongMonthNames[8] := rsLongAug;
  LongMonthNames[9] := rsLongSep;
  LongMonthNames[10] := rsLongOct;
  LongMonthNames[11] := rsLongNov;
  LongMonthNames[12] := rsLongDec;

  SetLength(TrueBoolStrs,1);
  SetLength(FalseBoolStrs,1);
  TrueBoolStrs[0]   := rsTrue;
  FalseBoolStrs[0]  := rsFalse;

  // Dialog box button captions
  cMsgDlgBtnText[mbOK]        := rsOK;
  cMsgDlgBtnText[mbCancel]    := rsCancel;
  cMsgDlgBtnText[mbYes]       := rsYes;
  cMsgDlgBtnText[mbNo]        := rsNo;
  cMsgDlgBtnText[mbAbort]     := rsAbort;
  cMsgDlgBtnText[mbRetry]     := rsRetry;
  cMsgDlgBtnText[mbIgnore]    := rsIgnore;
  cMsgDlgBtnText[mbAll]       := rsAll;
  cMsgDlgBtnText[mbNoToAll]   := rsNoToAll;
  cMsgDlgBtnText[mbYesToAll]  := rsYesToAll;
  cMsgDlgBtnText[mbHelp]      := rsHelp;
  cMsgDlgBtnText[mbClose]     := rsClose;
end;

procedure TlqApplication.SetHintPause(const AValue: Integer);
begin
  FHintPause := AValue;
  FHintTimer.Interval := FHintPause;
end;

procedure TlqApplication.InternalMsgFreeMe(var msg: TlqMessageRec);
begin
  if Assigned(msg.Sender) then
  begin
    if csDestroying in TComponent(msg.Sender).ComponentState then
      Exit;
    RemoveComponent(TlqWindowBase(msg.Sender));
    TlqWindowBase(msg.Sender).Free;
  end;
end;

procedure TlqApplication.InternalMsgHintTimer(var msg: TlqMessageRec);
begin
//  writeln('InternalMsgHintTimer msg');
  if (msg.Params.user.Param1 < 2) then
  begin
    { MouseEnter occured }
    FHintTimer.Enabled := Boolean(msg.Params.user.Param1);
    FHintWidget := TlqWindow(msg.Sender);
  end
  else
  begin
    { Handle mouse move information }
    FHintPos.X := msg.Params.user.Param2;
    FHintPos.Y := msg.Params.user.Param3;
    FHintWidget := TlqWindow(msg.Sender);
    if FHintTimer.Enabled then
      FHintTimer.Reset    // keep reseting to prevent hint from showing
    else
      HideHint;
  end;
end;

procedure TlqApplication.CreateHintWindow;
begin
  if LQU_PREVENTWND then
     exit;

  if not Assigned(FHintWindow) then
  begin
    FHintWindow := HintWindowClass.Create(nil);
    TlqHintWindow(FHintWindow).Visible := False;
  end;
end;

procedure TlqApplication.HintTimerFired(Sender: TObject);
var
  w: TlqWidget;
  lHint: TlqString;
begin
  w := nil;
  w := TlqWidget(FHintWidget);
  try
    if Assigned(w) then
    begin
//writeln('fpgApplication.HintTimerFired w = ', w.ClassName, ' - ', w.Name);
      TWidgetFriend(w).DoShowHint(lHint);
      ActivateHint(w.WindowToScreen(w, FHintPos), lHint);
    end;
  except
    // silence it!
    { TODO : FHintWidget probably went out of scope just as timer fired. Try
      and detect such cases better! }
  end;

  FHintTimer.Enabled := False;
end;

procedure TlqApplication.SetShowHint(const AValue: boolean);
begin
//writeln('>> SetShowHint to :', AValue);
  FShowHint := AValue;
end;

procedure TlqApplication.SetStartDragDistance(const AValue: integer);
begin
  if AValue < 0 then
    FStartDragDistance := 0
  else
    FStartDragDistance := AValue;
end;

procedure TlqApplication.FreeFontRes(afontres: TlqFontResource);
var
  n: integer;
begin
  for n := FFontResList.Count-1 downto 0 do
    if FFontResList[n] = Pointer(afontres) then
    begin
      TlqFontResource(FFontResList[n]).Free;
      FFontResList.Delete(n);
      Exit; //==>
    end;
end;

procedure TlqApplication.InternalInit;
begin
  FDefaultFont := GetFont(LQ_DEFAULT_FONT_DESC);
  lqInitTimers;


  { If the end-user passed in a style, try and create an instance of it }
  if gCommandLineParams.IsParam('style') then
    lqStyleManager.SetStyle(gCommandLineParams.GetParam('style'));
  lqStyle := lqStyleManager.Style;

  lqCaret      := TlqCaret.Create;
  lqImages     := TlqImages.Create;

  lqCreateStandardImages;

  // This will process Application and LiteKit Toolkit translation (*.po) files
  TranslateResourceStrings(ApplicationName, ExtractFilePath(ParamStr(0)), '');
  SetupLocalizationStrings;
  CreateHintWindow;

  FHintTimer := TlqTimer.Create(HintPause);
  FHintTimer.OnTimer := @HintTimerFired;
end;

procedure TlqApplication.Flush;
begin
  DoFlush;
end;

procedure TlqApplication.ProcessMessages;
begin
  Flush;
  while MessagesPending do
  begin
    WaitWindowMessage(250);
    Flush;
  end;
end;

procedure TlqApplication.SetMessageHook(AWidget: TObject; const AMsgCode: integer; AListener: TObject);
var
  oItem: TMsgHookItem;
begin
  oItem := TMsgHookItem.Create;
  oItem.Dest := AWidget;
  oItem.Listener := AListener;
  oItem.MsgCode := AMsgCode;
  FMessageHookList.Add(oItem);
end;

procedure TlqApplication.UnsetMessageHook(AWidget: TObject;
  const AMsgCode: integer; AListener: TObject);
var
  oItem: TMsgHookItem;
  i: integer;
begin
  for i := 0 to FMessageHookList.Count-1 do
  begin
    oItem := TMsgHookItem(FMessageHookList.Items[i]);
    if (oItem.Dest = AWidget) and (oItem.Listener = AListener) and (oItem.MsgCode = AMsgCode) then
    begin
      FMessageHookList.Delete(i);
      oItem.Free;
      Exit;
    end;
  end;
end;

procedure TlqApplication.HandleException(Sender: TObject);
begin
  if not (ExceptObject is Exception) then
    SysUtils.ShowException(ExceptObject, ExceptAddr)
  else
  begin
    if not (ExceptObject is EAbort) then  // EAborts are silent. They show no message.
    begin
      if Assigned(FOnException) then
        FOnException(Sender, Exception(ExceptObject))
      else
      begin
//        SysUtils.ShowException(ExceptObject, ExceptAddr);
        ShowException(Exception(ExceptObject));
//        DumpStack;
      end;
    end;
  end;  { if/else }

  // Note: We should not terminate when we receive EAbort exceptions.
  if (not (ExceptObject is EAbort)) and StopOnException then
    Terminated := True;
end;

procedure TlqApplication.HideHint;
begin
  {$IFDEF DEBUG}
  SendDebug('HideHint');
  {$ENDIF}
  FHintTimer.Enabled := False;
  if Assigned(FHintWindow) and TlqHintWindow(FHintWindow).Visible then
    TlqHintWindow(FHintWindow).Hide;
end;

procedure TlqApplication.ShowException(E: Exception);
begin
  TlqMessageDialog.Critical('An unexpected error occurred.', E.Message);
end;

procedure TlqApplication.WaitWindowMessage(atimeoutms: integer);
begin
  if IsMultiThread then
    CheckSynchronize;  // execute the to-be synchronized method

  DoWaitWindowMessage(lqClosestTimer(now, atimeoutms));
  lqDeliverMessages;
  lqCheckTimers;
end;

procedure TlqApplication.RunMessageLoop;
begin
  WaitWindowMessage(2000);
end;

{ TlqFont }

constructor TlqFont.Create(afontres: TlqFontResource; const afontdesc: string);
begin
  FFontRes  := afontres;
  FFontDesc := afontdesc;
  afontres.IncRefCount;
end;

destructor TlqFont.Destroy;
begin
  if TlqFontResource(FFontRes).DecRefCount <= 0 then
    lqApplication.FreeFontRes(TlqFontResource(FFontRes));
  inherited Destroy;
end;

{ TlqFontResource }

constructor TlqFontResource.Create(const afontdesc: string);
begin
  inherited Create(afontdesc);
  FFontDesc := afontdesc;
  FRefCount := 0;
end;

function TlqFontResource.DecRefCount: integer;
begin
  Dec(FRefCount);
  Result := FRefCount;
end;

function TlqFontResource.IncRefCount: integer;
begin
  Inc(FRefCount);
  Result := FRefCount;
end;

{ TlqCanvas }

// Warning! This function is not supposed to handle existing line breaks,
// it is only supposed to insert new ones when appropriate. Also, this function
// simply inserts line breaks, it doesn't split text lines etc...
function TlqCanvas.AddLineBreaks(const s: TlqString; aMaxLineWidth: integer): string;
var
  i, n, ls: integer;
  sub: string;
  lw, tw: integer;
begin
  Result := '';
  ls := Length(s);
  lw := 0;
  i  := 1;
  while i <= ls do
  begin
    if (s[i] in txtWordDelims) then       // read the delimeter only
    begin
      sub := s[i];
      Inc(i);
    end else                              // read the whole word
    begin
      n := PosSetEx(txtWordDelims, s, i);
      if n > 0 then
      begin
        sub := Copy(s, i, n-i);
        i := n;
      end else
      begin
        sub := Copy(s, i, MaxInt);
        i := ls+1;
      end;
    end;
    tw := Font.TextWidth(sub);            // wrap if needed
    if (lw + tw > aMaxLineWidth) and (lw > 0) then
    begin
      lw := tw;
      Result := TrimRight(Result) + sLineBreak;
    end else
      Inc(lw, tw);
    Result += sub;
  end;
end;

constructor TlqCanvas.Create(awin: TlqWindowBase);
begin
  inherited Create(awin);

  FBeginDrawCount := 0;

  // options
  FBufferedDraw        := True; // transparent widgets must turn this off
  FPersistentResources := False;
end;

destructor TlqCanvas.Destroy;
begin
  if lqCaret.FCanvas = self then
    lqCaret.UnSetCaret(self);
  inherited Destroy;
end;

procedure TlqCanvas.DrawButtonFace(x, y, w, h: TlqCoord; AFlags: TlqButtonFlags);
begin
  lqStyle.DrawButtonFace(self, x, y, w, h, AFlags);
end;

procedure TlqCanvas.DrawButtonFace(r: TlqRect; AFlags: TlqButtonFlags);
begin
  DrawButtonFace(r.Left, r.Top, r.Width, r.Height, AFlags);
end;

procedure TlqCanvas.DrawControlFrame(x, y, w, h: TlqCoord);
begin
  lqStyle.DrawControlFrame(self, x, y, w, h);
end;

procedure TlqCanvas.DrawControlFrame(r: TlqRect);
begin
  DrawControlFrame(r.Left, r.Top, r.Width, r.Height);
end;

procedure TlqCanvas.DrawBevel(x, y, w, h: TlqCoord; ARaised: Boolean);
begin
  lqStyle.DrawBevel(self, x, y, w, h, ARaised);
end;

procedure TlqCanvas.DrawBevel(r: TlqRect; ARaised: Boolean);
begin
  DrawBevel(r.Left, r.Top, r.Width, r.Height, ARaised);
end;

procedure TlqCanvas.DrawDirectionArrow(x, y, w, h: TlqCoord; direction: TArrowDirection);
begin
  lqStyle.DrawDirectionArrow(self, x, y, w, h, direction);
end;

procedure TlqCanvas.DrawDirectionArrow(r: TlqRect; direction: TArrowDirection);
begin
  DrawDirectionArrow(r.Left, r.Top, r.Width, r.Height, direction);
end;

procedure TlqCanvas.DrawFocusRect(r: TlqRect);
begin
  lqStyle.DrawFocusRect(self, r);
end;

function TlqCanvas.DrawText(x, y, w, h: TlqCoord; const AText: TlqString;
    AFlags: TlqTextFlags; ALineSpace: integer): integer;
var
  wtxt, htxt, i, nw, nx, ny, l: integer;
  buf: TlqString;
  wraplst: TStringList;
  lEnabled: Boolean;
begin
  lEnabled := not (txtDisabled in AFlags);

  // calculate longest word width to autosize properly
  wtxt := 0;
  if ((txtAutoSize in AFlags) or (w = 0)) then
  begin
    i := 1;
    buf := ExtractSubstr(AText, i, txtWordDelims);
    while buf <> '' do
    begin
      wtxt := Max(wtxt, Font.TextWidth(buf));
      buf := ExtractSubstr(AText, i, txtWordDelims);
    end;
  end;
  nw := Max(wtxt, w);
  
  wraplst := TStringList.Create;
  wraplst.Text := AText;
  
  if (txtWrap in AFlags) then
  begin
    for i := 0 to wraplst.Count-1 do
      wraplst[i] := AddLineBreaks(wraplst[i], nw);
    // force line breaks
    wraplst.Text := wraplst.Text;
  end;

  htxt := (Font.Height * wraplst.Count) + (ALineSpace * Pred(wraplst.Count));
  
  // Now paint the actual text
  for i := 0 to wraplst.Count-1 do
  begin
    l :=  (Font.Height + ALineSpace) * i;
    wtxt := Font.TextWidth(wraplst[i]);

    // horizontal alignment
    if (txtRight in AFlags) then
      nx := x + w - wtxt
    else
    if (txtHCenter in AFlags) then
      nx := x + (w - wtxt) div 2
    else // txtLeft is default
      nx := x;
      
    // vertical alignment
    if (txtBottom in AFlags) then
      ny := y + l + h - htxt
    else if (txtVCenter in AFlags) then
      ny := y + l + ((h - htxt) div 2)
    else // txtTop is default
      ny := y + l;

    lqStyle.DrawString(self, nx, ny, wraplst[i], lEnabled);
  end;
  
  wraplst.Free;
  Result := htxt;
end;

function TlqCanvas.DrawText(x, y: TlqCoord; const AText: TlqString;
    AFlags: TlqTextFlags; ALineSpace: integer): integer;
begin
  Result := DrawText(x, y, 0, 0, AText, AFlags, ALineSpace);
end;

function TlqCanvas.DrawText(r: TlqRect; const AText: TlqString; AFlags: TlqTextFlags;
    ALineSpace: integer): integer;
begin
  Result := DrawText(r.Left, r.Top, r.Width, r.Height, AText, AFlags, ALineSpace);
end;

{ TlqWindow }

function TlqWindow.CreateCanvas: TlqCanvasBase;
begin
  Result := DefaultCanvasClass.Create(self);
end;

constructor TlqWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner); // initialize the platform internals

  FTop    := 0;
  FLeft   := 0;
  FWidth  := 16;
  FHeight := 16;
  FPrevWidth  := FWidth;
  FPrevHeight := FHeight;

  FMinWidth  := 2;
  FMinHeight := 2;

  FModalForWin := nil;

  if (AOwner <> nil) and (AOwner is TlqWindow) then
    FWindowType   := wtChild
  else
    FWindowType   := wtWindow;

  FCanvas := CreateCanvas;
end;

destructor TlqWindow.Destroy;
begin
  FCanvas.Free;
  inherited Destroy;
end;

{procedure TlqWindow.SetParent(const AValue: TlqWindow);
begin
  inherited SetParent(AValue);
end;

function TlqWindow.GetParent: TlqWindow;
begin
  result := TlqWindow(inherited GetParent);
end;}

function TlqWindow.GetCanvas: TlqCanvas;
begin
  Result := TlqCanvas(inherited GetCanvas);
end;


procedure TlqWindow.DoAllocateWindowHandle(AParent: TlqWindowBase);
begin
  if not LQU_PREVENTWND then
    inherited;

end;

procedure TlqWindow.DoUpdateWindowPosition;
begin
  if not LQU_PREVENTWND then
    inherited;

end;

{ TlqStyle }

constructor TlqStyle.Create;
begin
  // Setup font aliases
  lqSetNamedFont('Label1', LQ_DEFAULT_FONT_DESC);
  lqSetNamedFont('Label2', LQ_DEFAULT_FONT_DESC + ':bold');
  lqSetNamedFont('Edit1', LQ_DEFAULT_FONT_DESC);
  lqSetNamedFont('Edit2', 'Courier New-10');
  lqSetNamedFont('List', LQ_DEFAULT_FONT_DESC);
  lqSetNamedFont('Grid', LQ_DEFAULT_SANS + '-9');
  lqSetNamedFont('GridHeader', LQ_DEFAULT_SANS + '-9:bold');
  lqSetNamedFont('Menu', LQ_DEFAULT_FONT_DESC);
  lqSetNamedFont('MenuAccel', LQ_DEFAULT_FONT_DESC + ':underline');
  lqSetNamedFont('MenuDisabled', LQ_DEFAULT_FONT_DESC);

  {$Note Refactor this so under Windows it can detect the system colors instead.
    Also under Linux (KDE and Gnome) we should be able to detect the system colors.}
  lqSetNamedColor(clWindowBackground, $FFD5D2CD);
  lqSetNamedColor(clBoxColor, $FFFFFFFF);
  lqSetNamedColor(clShadow1, $FF848284);       // medium
  lqSetNamedColor(clShadow2, $FF424142);       // dark
  lqSetNamedColor(clHilite1, $FFE0E0E0);       // light
  lqSetNamedColor(clHilite2, $FFFFFFFF);       // white
  lqSetNamedColor(clText1, $FF000000);
  lqSetNamedColor(clText2, $FF000040);
  lqSetNamedColor(clText3, $FF800000);
  lqSetNamedColor(clText4, $FF404000);
  lqSetNamedColor(clSelection, $FF08246A);
  lqSetNamedColor(clSelectionText, $FFFFFFFF);
  lqSetNamedColor(clInactiveSel, $FF99A6BF);  // win 2000 buttonface = $D4D0C8
  lqSetNamedColor(clInactiveSelText, $FF000000);
  lqSetNamedColor(clScrollBar, $FFE8E4DB);
  lqSetNamedColor(clButtonFace, $FFD5D2CD);
  lqSetNamedColor(clListBox, $FFFFFFFF);
  lqSetNamedColor(clGridLines, $FFA0A0A0);
  lqSetNamedColor(clGridHeader, $FFD5D2CD);
  lqSetNamedColor(clWidgetFrame, $FF000000);
  lqSetNamedColor(clInactiveWgFrame, $FFA0A0A0);
  lqSetNamedColor(clTextCursor, $FF000000);
  lqSetNamedColor(clChoiceListBox, $FFE8E8E8);
  lqSetNamedColor(clUnset, $FF99A6BF);                   // dull (gray) blue
  lqSetNamedColor(clMenuText, $FF000000);
  lqSetNamedColor(clMenuDisabled, $FF909090);
  lqSetNamedColor(clHintWindow, $FFFFFFBF);
  lqSetNamedColor(clGridSelection, $FF08246A);           // same as clSelection
  lqSetNamedColor(clGridSelectionText, $FFFFFFFF);       // same as clSelectionText
  lqSetNamedColor(clGridInactiveSel, $FF99A6BF);         // same as clInactiveSel
  lqSetNamedColor(clGridInactiveSelText, $FF000000);     // same as clInactiveSelText
  lqSetNamedColor(clSplitterGrabBar, $FF839EFE);         // pale blue


  // Global Font Objects
  DefaultFont      := lqGetFont(lqGetNamedFontDesc('Label1'));
  FixedFont        := lqGetFont(lqGetNamedFontDesc('Edit2'));
  MenuFont         := lqGetFont(lqGetNamedFontDesc('Menu'));
  MenuAccelFont    := lqGetFont(lqGetNamedFontDesc('MenuAccel'));
  MenuDisabledFont := lqGetFont(lqGetNamedFontDesc('MenuDisabled'));
end;

destructor TlqStyle.Destroy;
begin
  DefaultFont.Free;
  FixedFont.Free;
  MenuFont.Free;
  MenuAccelFont.Free;
  MenuDisabledFont.Free;
  inherited Destroy;
end;

procedure TlqStyle.DrawButtonFace(ACanvas: TlqCanvas; x, y, w, h: TlqCoord; AFlags: TlqButtonFlags);
var
  r: TlqRect;
begin
  r.SetRect(x, y, w, h);

  if btfIsDefault in AFlags then
  begin
    ACanvas.SetColor(clBlack);
    ACanvas.SetLineStyle(1, lsSolid);
    ACanvas.DrawRectangle(r);
    InflateRect(r, -1, -1);
    Exclude(AFlags, btfIsDefault);
    lqStyle.DrawButtonFace(ACanvas, r.Left, r.Top, r.Width, r.Height, AFlags);
    Exit; //==>
  end;

  { Clear the rectangle with a color }
  ACanvas.SetColor(clButtonFace);
  ACanvas.SetLineStyle(1, lsSolid);
  ACanvas.FillRectangle(x, y, w, h);

  if (btfFlat in AFlags) and not (btfIsPressed in AFlags) then
    Exit; // no need to go further

  // Left and Top (outer)
  if (btfIsPressed in AFlags) then
  begin
    if (btfIsEmbedded in AFlags) then
      ACanvas.SetColor(clHilite2)
    else
    begin
      if (btfFlat in AFlags) or (btfHover in AFlags) then
        ACanvas.SetColor(clShadow1)  { light shadow }
      else
        ACanvas.SetColor(clShadow2); { dark shadow }
    end;
  end
  else
    ACanvas.SetColor(clHilite2);

  ACanvas.DrawLine(r.Left, r.Bottom, r.Left, r.Top);  // left
  ACanvas.DrawLine(r.Left, r.Top, r.Right, r.Top);    // top

  // Right and Bottom (outer)
  if (btfIsPressed in AFlags) then
  begin
    if (btfIsEmbedded in AFlags) then
      ACanvas.SetColor(clHilite1)
    else
    begin
      if (btfFlat in AFlags) or (btfHover in AFlags) then
        ACanvas.SetColor(clHilite2)  { light shadow }
      else
        ACanvas.SetColor(clShadow2); { dark shadow }
    end;
  end
  else
  begin
    if btfHover in AFlags then
      ACanvas.SetColor(clShadow1)  { light shadow }
    else
      ACanvas.SetColor(clShadow2); { dark shadow }
  end;

  ACanvas.DrawLine(r.Right, r.Top, r.Right, r.Bottom);   // right
  ACanvas.DrawLine(r.Right, r.Bottom, r.Left-1, r.Bottom);   // bottom

  if (btfFlat in AFlags) or (btfHover in AFlags) then
    exit; { "toolbar" style buttons need a nice thin/flat border }

  // Right and Bottom (inner)
  if btfIsPressed in AFlags then
  begin
    if (btfIsEmbedded in AFlags) then
      ACanvas.SetColor(clButtonFace)
    else
      ACanvas.SetColor(clHilite1);
  end
  else
    ACanvas.SetColor(clShadow1);

  ACanvas.DrawLine(r.Right-1, r.Top+1, r.Right-1, r.Bottom-1);   // right
  ACanvas.DrawLine(r.Right-1, r.Bottom-1, r.Left, r.Bottom-1);   // bottom
end;

procedure TlqStyle.DrawButtonFace(ACanvas: TlqCanvas; r: TlqRect; AFlags: TlqButtonFlags);
begin
  DrawButtonFace(ACanvas, r.Left, r.Top, r.Width, r.Height, AFlags);
end;

procedure TlqStyle.DrawControlFrame(ACanvas: TlqCanvas; x, y, w, h: TlqCoord);
var
  r: TlqRect;
begin
  r.SetRect(x, y, w, h);
  ACanvas.SetColor(clShadow1);
  ACanvas.DrawLine(r.Left, r.Bottom, r.Left, r.Top); // left (outer)
  ACanvas.DrawLine(r.Left, r.Top, r.Right, r.Top);   // top (outer)

  ACanvas.SetColor(clHilite2);
  ACanvas.DrawLine(r.Right, r.Top, r.Right, r.Bottom);   // right (outer)
  ACanvas.DrawLine(r.Right, r.Bottom, r.Left, r.Bottom); // bottom (outer)

  ACanvas.SetColor(clShadow2);
  ACanvas.DrawLine(r.Left+1, r.Bottom-1, r.Left+1, r.Top+1);   // left (inner)
  ACanvas.DrawLine(r.Left+1, r.Top+1, r.Right-1, r.Top+1);   // top (inner)

  ACanvas.SetColor(clHilite1);
  ACanvas.DrawLine(r.Right-1, r.Top+1, r.Right-1, r.Bottom-1);   // right (inner)
  ACanvas.DrawLine(r.Right-1, r.Bottom-1, r.Left+1, r.Bottom-1);   // bottom (inner)
end;

procedure TlqStyle.DrawControlFrame(ACanvas: TlqCanvas; r: TlqRect);
begin
  DrawControlFrame(ACanvas, r.Left, r.Top, r.Width, r.Height);
end;

procedure TlqStyle.DrawBevel(ACanvas: TlqCanvas; x, y, w, h: TlqCoord; ARaised: Boolean);
var
  r: TlqRect;
begin
  r.SetRect(x, y, w, h);
  ACanvas.SetColor(clWindowBackground);
  ACanvas.SetLineStyle(1, lsSolid);
  ACanvas.FillRectangle(x, y, w, h);
  
  if ARaised then
    ACanvas.SetColor(clHilite2)
  else
    ACanvas.SetColor(clShadow1);

  { top }
  ACanvas.DrawLine(r.Right-1, r.Top, r.Left, r.Top);

  { left }
  ACanvas.DrawLine(r.Left, r.Top, r.Left, r.Bottom);

  if ARaised then
    ACanvas.SetColor(clShadow1)
  else
    ACanvas.SetColor(clHilite2);

  { right, then bottom }
  ACanvas.DrawLine(r.Right, r.Top, r.Right, r.Bottom);
  ACanvas.DrawLine(r.Right, r.Bottom, r.Left-1, r.Bottom);
end;

procedure TlqStyle.DrawDirectionArrow(ACanvas: TlqCanvas; x, y, w, h: TlqCoord; direction: TArrowDirection);
var
{
  peekx: integer;
  peeky: integer;
  basex: integer;
  basey: integer;
  side: integer;
  margin: integer;
}
  rad: Extended;
  r: TRect;
  r2: TlqRect;
begin
{
  side   := (w div 4) + 1;
  margin := side + 1;

  if direction in [adUp, adDown] then  // vertical
  begin
    peekx := x + (w div 2);
    if direction = adDown then  // down
    begin
      peeky := y + h - margin;
      basey := peeky - side;
    end
    else
    begin                  // up
      peeky := y + margin;
      basey := peeky + side;
    end;
    ACanvas.FillTriangle(peekx, peeky, peekx + side, basey, peekx - side, basey);
  end
  else // horizontal
  begin
    peeky := y + (h div 2);
    if direction = adRight then  // right
    begin
      peekx := x + w - margin;
      basex := peekx - side;
    end
    else                   // left
    begin
      peekx := x + margin;
      basex := peekx + side;
    end;
    ACanvas.FillTriangle(peekx, peeky, basex, peeky - side, basex, peeky + side);
  end;
}

  r2.SetRect(x, y, w, h);
  r := lqRectToRect(r2);

  if direction = adRight then
    rad := DegToRad(0)
  else if direction = adUp then
    rad := DegToRad(90)
  else if direction = adLeft then
    rad := DegToRad(180)
  else
    rad := DegToRad(270);

  PaintTriangle(ACanvas, r, rad);
end;

procedure TlqStyle.DrawString(ACanvas: TlqCanvas; x, y: TlqCoord;
  AText: string; AEnabled: boolean);
begin
  if AText = '' then
    Exit; //==>
  if not AEnabled then
  begin
    ACanvas.SetTextColor(clHilite2);
    ACanvas.DrawString(x+1, y+1, AText);
    ACanvas.SetTextColor(clShadow1);
  end;
  ACanvas.DrawString(x, y, AText);
end;

procedure TlqStyle.DrawFocusRect(ACanvas: TlqCanvas; r: TlqRect);
var
  oldColor: TlqColor;
  oldLineWidth: integer;
  oldLineStyle: TlqLineStyle;
begin
  oldColor      := ACanvas.Color;
  oldLineWidth  := ACanvas.GetLineWidth;
  oldLineStyle  := ACanvas.LineStyle;

  ACanvas.SetColor(clText1);
  ACanvas.SetLineStyle(1, lsDot);
  ACanvas.DrawRectangle(r);

  // restore previous settings
  ACanvas.SetColor(oldColor);
  ACanvas.SetLineStyle(oldLineWidth, oldLineStyle);
end;

procedure TlqStyle.DrawMenuBar(ACanvas: TlqCanvas; r: TlqRect; ABackgroundColor: TlqColor);
begin
  ACanvas.Clear(ABackgroundColor);

  // inner bottom line
  ACanvas.SetColor(clShadow1);
  ACanvas.DrawLine(r.Left, r.Bottom-1, r.Right+1, r.Bottom-1);   // bottom
  // outer bottom line
  ACanvas.SetColor(clWhite);
  ACanvas.DrawLine(r.Left, r.Bottom, r.Right+1, r.Bottom);   // bottom
end;

procedure TlqStyle.DrawMenuRow(ACanvas: TlqCanvas; r: TlqRect; AFlags: TlqMenuItemFlags);
begin
  ACanvas.FillRectangle(r);
end;

procedure TlqStyle.DrawMenuItem(ACanvas: TlqCanvas; r: TlqRect;
  AFlags: TlqMenuItemFlags; AText: TlqString);
begin
  //
end;

procedure TlqStyle.DrawMenuItemSeparator(ACanvas: TlqCanvas; r: TlqRect);
begin
  ACanvas.SetColor(clShadow1);
  ACanvas.DrawLine(r.Left+1, r.Top+2, r.Right, r.Top+2);
  ACanvas.SetColor(clHilite2);
  ACanvas.DrawLine(r.Left+1, r.Top+3, r.Right, r.Top+3);
end;

procedure TlqStyle.DrawMenuItemImage(ACanvas: TlqCanvas; x, y: TlqCoord; r: TlqRect; AFlags: TlqMenuItemFlags);
var
  img: TlqImage;
  lx: TlqCoord;
  ly: TlqCoord;
begin
  if mifChecked in AFlags then
  begin
    img := lqImages.GetImage('stdimg.check');    // Do NOT localize
    if mifSelected in AFlags then
      img.Invert;  // invert modifies the original image, so we must restore it later
    ACanvas.DrawImage(x, y, img);
    if mifSelected in AFlags then
      img.Invert;  // restore image to original state
  end;
  if mifSubMenu in AFlags then
  begin
    img := lqImages.GetImage('sys.sb.right');    // Do NOT localize
    lx := (r.height div 2) - 3;
    lx := r.right-lx-2;
    ly := y + ((r.Height-img.Height) div 2);
    if mifSelected in AFlags then
      img.Invert;  // invert modifies the original image, so we must restore it later
    ACanvas.DrawImage(lx, ly, img);
    if mifSelected in AFlags then
      img.Invert;  // restore image to original state
  end;
end;

function TlqStyle.GetButtonBorders: TRect;
begin
  Result := Rect(3, 3, 3, 3);
end;

function TlqStyle.GetButtonShift: TPoint;
begin
  Result := Point(1, 1);
end;

function TlqStyle.HasButtonHoverEffect: boolean;
begin
  Result := False;
end;

function TlqStyle.GetControlFrameBorders: TRect;
begin
  Result := Rect(2, 2, 2, 2);
end;

function TlqStyle.GetSeparatorSize: integer;
begin
  Result := 2;
end;

procedure TlqStyle.DrawEditBox(ACanvas: TlqCanvas; const r: TlqRect; const IsEnabled: Boolean; const IsReadOnly: Boolean; const ABackgroundColor: TlqColor);
begin
  if IsEnabled and not IsReadOnly then
    ACanvas.SetColor(ABackgroundColor)
  else
    ACanvas.SetColor(clWindowBackground);
  ACanvas.FillRectangle(r);
end;


{ TlqCaret }

procedure TlqCaret.OnTimerTime(Sender: TObject);
begin
  if FEnabled then
    InvertCaret;
end;

constructor TlqCaret.Create;
begin
  FEnabled       := False;
  FInterval      := 500;  // blinking interval
  FCanvas        := nil;
  FTop           := 0;
  FLeft          := 0;
  FWidth         := 2;
  FHeight        := 8;
  FTimer         := TlqTimer.Create(FInterval);
  FTimer.OnTimer := @OnTimerTime;
end;

destructor TlqCaret.Destroy;
begin
  FCanvas := nil;
  FTimer.Free;
  inherited Destroy;
end;

procedure TlqCaret.SetCaret(ACanvas: TlqCanvas; x, y, w, h: TlqCoord);
begin
  FEnabled := True;
  FVisible := False;
  FCanvas  := ACanvas;
  FLeft    := x;
  FTop     := y;
  FWidth   := w;
  FHeight  := h;
  InvertCaret;

  FTimer.Enabled  := False;
  FTimer.Interval := FInterval;
  FTimer.Enabled  := True;
end;

procedure TlqCaret.UnSetCaret(ACanvas: TlqCanvas);
begin
  if (FCanvas = ACanvas) or (ACanvas = nil) then
  begin
    FTimer.Enabled := False;
    FEnabled := False;
    FCanvas  := nil;
  end;
end;

procedure TlqCaret.InvertCaret;
begin
  if FCanvas = nil then
    Exit; //==>

  // we could not be sure about the buffer contents!
  try
    FCanvas.BeginDraw(False);
    try
      // this works well on narrow characters like 'i' or 'l' in non-mono fonts
      FCanvas.XORFillRectangle($FFFFFF, FLeft, FTop, FWidth, FHeight);
      FVisible := not FVisible;
    finally
      FCanvas.EndDraw(FLeft, FTop, FWidth, FHeight);
    end;
  except
    {$Note This occurs every now and again with TlqMemo and CaretInvert painting! }
    // Investigate this.
    {$IFDEF DEBUG}
    SendDebug('TlqCaret.InvertCaret cause an exception');
    {$ENDIF}
  end;
end;

{ TlqImages }

constructor TlqImages.Create;
begin
  FImages := TStringList.Create;
end;

destructor TlqImages.Destroy;
var
  i: integer;
  img: TlqImage;
begin
  for i := FImages.Count-1 downto 0 do
  begin
    img := TlqImage(FImages.Objects[i]);
    FImages.Delete(i);
    img.Free;
  end;
  FImages.Free;
  inherited Destroy;
end;

function TlqImages.AddImage(const imgid: string; img: TlqImage): boolean;
var
  i: integer;
begin
  i := FImages.IndexOf(LowerCase(imgid));
  if i >= 0 then
  begin
    FImages.Strings[i] := LowerCase(imgid);
    FImages.Objects[i] := img;
    Result := False;
  end
  else
  begin
    FImages.AddObject(LowerCase(imgid), img);
    Result := True;
  end;
end;

function TlqImages.DeleteImage(const imgid: string; freeimg: boolean): boolean;
var
  i: integer;
begin
  i := FImages.IndexOf(LowerCase(imgid));
  if i >= 0 then
  begin
    if freeimg then
      TlqImage(FImages.Objects[i]).Free;
    FImages.Delete(i);
    Result := True;
  end
  else
    Result := False;
end;

function TlqImages.GetImage(const imgid: string): TlqImage;
var
  i: integer;
begin
  i := FImages.IndexOf(LowerCase(imgid));
  if i >= 0 then
    Result := TlqImage(FImages.Objects[i])
  else
    Result := nil;
end;

function TlqImages.AddBMP(const imgid: string; bmpdata: pointer; bmpsize: integer): TlqImage;
begin
  Result := CreateImage_BMP(bmpdata, bmpsize);
  if Result <> nil then
    AddImage(imgid, Result);
end;

function TlqImages.AddMaskedBMP(const imgid: string; bmpdata: pointer; bmpsize: integer;
  mcx, mcy: integer): TlqImage;
begin
  Result := AddBMP(imgid, bmpdata, bmpsize);
  if Result <> nil then
  begin
    Result.CreateMaskFromSample(mcx, mcy);
    Result.UpdateImage;
  end;
end;

procedure TlqImages.ListImages(var sl: TStringList);
begin
  if sl <> nil then
    sl.Assign(FImages);
end;


{ TlqImage }

function TlqImage.GetScanLine(Row: Integer): Pointer;
var
  pdest: Plongword;
begin
  if (Height = 0) or (Width = 0) then
  begin
    Result := nil;
    Exit;
  end;

  pdest := ImageData; // This is so that pointer math uses correct increment size
  Result := pdest + (Row * Width);
end;

function TlqImage.CreateDisabledImage: TlqImage;
begin
  Result := ImageFromSource;
  lqApplyGreyFilter(Result);
end;

function TlqImage.ImageFromSource: TlqImage;
var
  x, y: TlqCoord;
begin
  Result := TlqImage.Create;
  Result.AllocateImage(ColorDepth, Width, Height);
  for x := 0 to Width-1 do
  begin
    for y := 0 to Height-1 do
    begin
      Result.Colors[x, y] := Colors[x, y];
    end;
  end;
  if Masked then
    Result.CreateMaskFromSample(MaskPoint.X, MaskPoint.Y);
  Result.UpdateImage;
end;

function TlqImage.ImageFromRect(var ARect: TRect): TlqImage;
var
  x, y: TlqCoord;
  ix, iy: TlqCoord;
begin
  SortRect(ARect);
  Result := TlqImage.Create;
  Result.AllocateImage(ColorDepth, ARect.Right-ARect.Left, ARect.Bottom-ARect.Top);
  iy := -1;
  for y := ARect.Top to ARect.Bottom-1 do
  begin
    Inc(iy);
    ix := -1;
    for x := ARect.Left to ARect.Right-1 do
    begin
      Inc(ix);
      Result.Colors[ix, iy] := Colors[x, y];
    end;
  end;
  Result.UpdateImage;
end;

function TlqImage.ImageFromRect(var ARect: TlqRect): TlqImage;
var
  x, y: TlqCoord;
  ix, iy: TlqCoord;
begin
  SortRect(ARect);
  Result := TlqImage.Create;
  Result.AllocateImage(ColorDepth, ARect.Width, ARect.Height);
  iy := -1;
  for y := ARect.Top to ARect.Bottom do
  begin
    Inc(iy);
    ix := -1;
    for x := ARect.Left to ARect.Right do
    begin
      Inc(ix);
      Result.Colors[ix, iy] := Colors[x, y];
    end;
  end;
  Result.UpdateImage;
end;


{ TlqDrag }

procedure TlqDrag.SetMimeData(const AValue: TlqMimeDataBase);
begin
  if Assigned(FMimeData) then
    FMimeData.Free;
  FMimeData := AValue;
end;

function TlqDrag.GetSource: TlqWindow;
begin
  Result := TlqWindow(inherited GetSource);
end;

constructor TlqDrag.Create(ASource: TlqWindow);
begin
  inherited Create;
  FSource := ASource;
end;

function TlqDrag.Execute(const ADropActions: TlqDropActions;
  const ADefaultAction: TlqDropAction): TlqDropAction;
begin
  {$NOTE These exception messages need to become resource strings }
  if not Assigned(FMimeData) then
    raise Exception.Create(ClassName + ': No mimedata was set before starting the drag');
  if not Assigned(FSource) then
    raise Exception.Create(ClassName + ': No Source window was specified before starting the drag');
  if ADropActions = [] then
    raise Exception.Create(ClassName + ': No Drop Action was specified');
  Result := inherited Execute(ADropActions, ADefaultAction);
end;



procedure TlqApplication.CreateForm(InstanceClass: TComponentClass;
  out Reference);
var
  Instance: TComponent;
  ok: boolean;
  //AForm: TForm;
begin
  // Allocate the instance, without calling the constructor
  Instance := TComponent(InstanceClass.NewInstance);
  // set the Reference before the constructor is called, so that
  // events and constructors can refer to it
  TComponent(Reference) := Instance;

  ok:=false;
  try
    Instance.Create(Self);
    if Instance is TlqForm then
       if TlqForm(instance).Visible then
           TlqForm(instance).Show;
    ok:=true;
  finally
    if not ok then begin
      TComponent(Reference) := nil;
      //if FCreatingForm=Instance then
        //FCreatingForm:=nil;
    end;
  end;

end;

initialization
  uApplication    := nil;
  uClipboard      := nil;
  uMsgQueueList   := nil;
  lqTimers       := nil;
  lqCaret        := nil;
  lqImages       := nil;
  iCallTrace      := -1;
  InitializeDebugOutput;
  lqInitMsgQueue;
{$ifdef AGGCanvas}
  DefaultCanvasClass := TAgg2D;
{$else}
  DefaultCanvasClass := TlqCanvas;
{$endif}

finalization
  uClipboard.Free;
  uApplication.Free;
  FinalizeDebugOutput;

end.

