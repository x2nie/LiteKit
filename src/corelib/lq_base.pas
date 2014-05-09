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
      The Big Bang starts here!  The starting unit for LiteKit.
}

unit lq_base;

{$mode objfpc}{$H+}

// To enable the AggPas powered Canvas
{.$define AGGCanvas}

// For debug use only
{.$define GDEBUG}

interface

uses
  Classes,
  SysUtils,
  lq_impl,
  syncobjs, // TCriticalSection usage
  variants, contnrs;

type
  TlqCoord       = integer;     // we might use floating point coordinates in the future...
  TlqColor       = type longword;    // Always in AARRGGBB (Alpha, Red, Green, Blue) format!!
  TlqString      = type AnsiString;
  TlqChar        = type String[4];

  PPoint = ^TPoint;

  TRGBTriple = packed record
    Red: byte;
    Green: byte;
    Blue: byte;
    Alpha: byte;
  end;

  // Same declaration as in FPImage unit, but we don't use FPImage yet, so declare it here
  TFPColor = record
    Red: byte;
    Green: byte;
    Blue: byte;
    Alpha: byte;
  end deprecated;

  TWindowType = (wtChild, wtWindow, wtModalForm, wtPopup);

  TWindowAttribute = (waSizeable, waAutoPos, waScreenCenterPos, waStayOnTop,
      waFullScreen, waBorderless, waUnblockableMessages, waX11SkipWMHints,
      waOneThirdDownPos);
  TWindowAttributes = set of TWindowAttribute;

  TlqWindowState = (wsNormal, wsMinimized, wsMaximized);

  TMouseCursor = (mcDefault, mcArrow, mcCross, mcIBeam, mcSizeEW, mcSizeNS,
      mcSizeNWSE, mcSizeNESW, mcSizeSWNE, mcSizeSENW, mcMove, mcHourGlass,
      mcHand, mcDrag, mcNoDrop);

  TGradientDirection = (gdVertical,     // Fill vertical
                        gdHorizontal);  // Fill Horizontal

  TClipboardKeyType = (ckNone, ckCopy, ckPaste, ckCut);

  // If you have to convert this to an Integer, mrNone = 0 etc.
  TlqModalResult = (mrNone, mrOK, mrCancel, mrYes, mrNo, mrAbort, mrRetry,
      mrIgnore, mrAll, mrNoToAll, mrYesToAll, mrHelp);

  TlqDropAction = (daIgnore, daCopy, daMove, daLink, daAsk);
  TlqDropActions = set of TlqDropAction;

  TlqEditBorderStyle = (ebsNone, ebsDefault, ebsSingle);

  // in case we wanted to trap any LiteKit specific exceptions
  ELiteKitException = class(Exception);

  // For providing user feedback. No need to display backtrace information
  ELiteKitUserFeedbackException = class(ELiteKitException);



const
  MOUSE_LEFT       = 1;
  MOUSE_RIGHT      = 3;
  MOUSE_MIDDLE     = 2;

  // Platform independent messages used by LiteKit (TlqWidget)
  LQM_PAINT       = 1;
  LQM_ACTIVATE    = 2;
  LQM_DEACTIVATE  = 3;
  LQM_KEYPRESS    = 4;
  LQM_KEYRELEASE  = 5;
  LQM_KEYCHAR     = 6;
  LQM_MOUSEDOWN   = 7;
  LQM_MOUSEUP     = 8;
  LQM_MOUSEMOVE   = 9;
  LQM_DOUBLECLICK = 10;
  LQM_MOUSEENTER  = 11;
  LQM_MOUSEEXIT   = 12;
  LQM_CLOSE       = 13;
  LQM_SCROLL      = 14;
  LQM_RESIZE      = 15;
  LQM_MOVE        = 16;
  LQM_POPUPCLOSE  = 17;
  LQM_HINTTIMER   = 18;
  LQM_FREEME      = 19;
  LQM_DROPENTER   = 20;
  LQM_DROPEXIT    = 21;
  LQM_INVALIDATE  = 27;
  LQM_USER        = 50000;
  LQM_KILLME      = MaxInt;

  // The special keys, based on the well-known keyboard scan codes
  {$I keys.inc}


var
  {$IFDEF MSWINDOWS}
  LQ_DEFAULT_FONT_DESC: string = 'Arial-8:antialias=true';
  LQ_DEFAULT_SANS: string = 'Arial';
  {$ENDIF}
  {$IFDEF UNIX}
  LQ_DEFAULT_FONT_DESC: string = 'Liberation Sans-10:antialias=true';
  LQ_DEFAULT_SANS: string = 'Liberation Sans';
  {$ENDIF}

const
  UserNamedColorStart   = 128;
  {$I predefinedcolors.inc}

type
  TlqRect = object  // not class for static allocations
    Top: TlqCoord;
    Left: TlqCoord;
    Width: TlqCoord;
    Height: TlqCoord;
    procedure SetRect(aleft, atop, awidth, aheight: TlqCoord);
    function  Bottom: TlqCoord;
    function  Right: TlqCoord;
    procedure SetBottom(Value: TlqCoord);
    procedure SetRight(Value: TlqCoord);
  end;


  TlqPoint = object  // not class for static allocations
    X: integer;
    Y: integer;
    procedure SetPoint(AX, AY: integer);
    function  ManhattanLength: integer;      { See URL for explanation http://en.wikipedia.org/wiki/Taxicab_geometry }
    function  ManhattanLength(const PointB: TlqPoint): integer;
  end;


  TlqSize = object  // not class for static allocations
    W: integer;
    H: integer;
    procedure SetSize(AWidth, AHeight: integer);
  end;


  TlqMsgParmMouse = record
    x: TlqCoord;
    y: TlqCoord;
    Buttons: word;
    shiftstate: TShiftState;
    delta: Integer;
    timestamp: TDateTime;  // for future use
  end;


  TlqMsgParmKeyboard = record
    keycode: word;
    keychar: TlqChar;
    shiftstate: TShiftState;
  end;


  TlqMsgParmUser = record
    Param1: Integer;
    Param2: Integer;
    Param3: Integer;
    Param4: Integer;
  end;


  TlqMessageParams = record
    case integer of
      0: (mouse: TlqMsgParmMouse);
      1: (keyboard: TlqMsgParmKeyboard);
      2: (rect: TlqRect);
      3: (user: TlqMsgParmUser);
  end;


  TlqMessageRec = record
    MsgCode: integer;
    Sender: TObject;
    Dest: TObject;
    Params: TlqMessageParams;
    Stop: Boolean;
  end;
  PfpgMessageRec = ^TlqMessageRec;


  TlqLineStyle = (lsSolid, lsDash, lsDot, lsDashDot, lsDashDotDot);


  // forward declaration
  TlqWindowBase = class;
  TlqCanvasBase = class;


  TlqImageBase = class(TObject)
  private
    function    GetColor(x, y: TlqCoord): TlqColor;
    procedure   SetColor(x, y: TlqCoord; const AValue: TlqColor);
  protected
    FWidth: integer;
    FHeight: integer;
    FColorDepth: integer;
    FMasked: boolean;
    FImageData: pointer;
    FImageDataSize: integer;
    FMaskData: pointer;
    FMaskDataSize: integer;
    FMaskPoint: TPoint;
    procedure   DoFreeImage; virtual; abstract;
    procedure   DoInitImage(acolordepth, awidth, aheight: integer; aimgdata: Pointer); virtual; abstract;
    procedure   DoInitImageMask(awidth, aheight: integer; aimgdata: Pointer); virtual; abstract;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   Invert(IncludeMask: Boolean = False);
    procedure   FreeImage;
    procedure   AllocateImage(acolordepth, awidth, aheight: integer);
    procedure   AllocateMask;
    procedure   CreateMaskFromSample(x, y: TlqCoord);
    { Must always be called AFTER you populated the ImageData array. Then only does it allocate OS resources. }
    procedure   UpdateImage;
    { Internal representation of color data is always ARGB }
    property    ImageData: pointer read FImageData;
    property    ImageDataSize: integer read FImageDataSize;
    property    MaskData: pointer read FMaskData;
    property    MaskDataSize: integer read FMaskDataSize;
    property    Width: integer read FWidth;
    property    Height: integer read FHeight;
    property    ColorDepth: integer read FColorDepth;
    property    Masked: boolean read FMasked;
    property    MaskPoint: TPoint read FMaskPoint;
    property    Colors[x, y: TlqCoord]: TlqColor read GetColor write SetColor;
  end;


  TlqFontResourceBase = class(TObject)
  public
    function    GetAscent: integer; virtual; abstract;
    function    GetDescent: integer; virtual; abstract;
    function    GetHeight: integer; virtual; abstract;
    function    GetTextWidth(const txt: string): integer; virtual; abstract;
  end;


  TlqFontBase = class(TObject)
  protected
    FFontDesc: string;
    FFontRes: TlqFontResourceBase;
    function    GetIsFixedWidth: boolean; virtual;
  public
    function    TextWidth(const txt: TlqString): integer;
    function    Ascent: integer;
    function    Descent: integer;
    function    Height: integer;
    property    FontDesc: string read FFontDesc;
    property    FontRes: TlqFontResourceBase read FFontRes;
    property    Handle: TlqFontResourceBase read FFontRes;
    property    IsFixedWidth: boolean read GetIsFixedWidth;
  end;


  TlqCustomInterpolation = class(TObject)
  private
    FCanvas: TlqCanvasBase;
    FImage: TlqImageBase;
  protected
    procedure   Initialize(AImage: TlqImageBase; ACanvas: TlqCanvasBase); virtual;
    procedure   Execute(x, y, w, h: integer); virtual; abstract;
  public
    property    Canvas: TlqCanvasBase read FCanvas;
    property    Image: TlqImageBase read FImage;
  end;


  TlqBaseInterpolation = class(TlqCustomInterpolation)
  private
    xfactor: double;
    yfactor: double;
    xsupport: double;
    ysupport: double;
    tempimage: TlqImageBase;
    procedure   Horizontal(width: integer);
    procedure   Vertical(dx, dy, width, height: integer);
  protected
    procedure   Execute(x, y, w, h: integer); override;
    function    Filter(x : double): double; virtual; abstract;
    function    MaxSupport: double; virtual; abstract;
  public
    destructor  Destroy; override;
  end;


  TlqMitchelInterpolation = class(TlqBaseInterpolation)
  protected
    function    Filter(x: double): double; override;
    function    MaxSupport: double; override;
  end;


  TlqCanvasBase = class(TObject)
  private
    FFastDoubleBuffer: Boolean;
    FInterpolation: TlqCustomInterpolation;
    procedure SetInterpolation(const AValue: TlqCustomInterpolation);
  protected
    FBufferedDraw: boolean;
    FBeginDrawCount: integer;
    FWindow: TlqWindowBase;
    FColor: TlqColor;
    FTextColor: TlqColor;
    FLineWidth: integer;
    FLineStyle: TlqLineStyle;
    FFont: TlqFontBase;
    FPersistentResources: boolean;
    procedure   DoSetFontRes(fntres: TlqFontResourceBase); virtual; abstract;
    procedure   DoSetTextColor(cl: TlqColor); virtual; abstract;
    procedure   DoSetColor(cl: TlqColor); virtual; abstract;
    procedure   DoSetLineStyle(awidth: integer; astyle: TlqLineStyle); virtual; abstract;
    procedure   DoGetWinRect(out r: TlqRect); virtual; abstract;
    procedure   DoFillRectangle(x, y, w, h: TlqCoord); virtual; abstract;
    procedure   DoXORFillRectangle(col: TlqColor; x, y, w, h: TlqCoord); virtual; abstract;
    procedure   DoFillTriangle(x1, y1, x2, y2, x3, y3: TlqCoord); virtual; abstract;
    procedure   DoDrawRectangle(x, y, w, h: TlqCoord); virtual; abstract;
    procedure   DoDrawLine(x1, y1, x2, y2: TlqCoord); virtual; abstract;
    procedure   DoDrawImagePart(x, y: TlqCoord; img: TlqImageBase; xi, yi, w, h: integer); virtual; abstract;
    procedure   DoDrawString(x, y: TlqCoord; const txt: string); virtual; abstract;
    procedure   DoSetClipRect(const ARect: TlqRect); virtual; abstract;
    function    DoGetClipRect: TlqRect; virtual; abstract;
    procedure   DoAddClipRect(const ARect: TlqRect); virtual; abstract;
    procedure   DoClearClipRect; virtual; abstract;
    procedure   DoBeginDraw(awin: TlqWindowBase; buffered: boolean); virtual; abstract;
    procedure   DoPutBufferToScreen(x, y, w, h: TlqCoord); virtual; abstract;
    procedure   DoEndDraw; virtual; abstract;
    function    GetPixel(X, Y: integer): TlqColor; virtual; abstract;
    procedure   SetPixel(X, Y: integer; const AValue: TlqColor); virtual; abstract;
    procedure   DoDrawArc(x, y, w, h: TlqCoord; a1, a2: Extended); virtual; abstract;
    procedure   DoFillArc(x, y, w, h: TlqCoord; a1, a2: Extended); virtual; abstract;
    procedure   DoDrawPolygon(Points: PPoint; NumPts: Integer; Winding: boolean = False); virtual; abstract;
  public
    constructor Create(awin: TlqWindowBase); virtual;
    destructor  Destroy; override;
    procedure   DrawRectangle(x, y, w, h: TlqCoord); overload;
    procedure   DrawRectangle(r: TlqRect); overload;
    procedure   DrawLine(x1, y1, x2, y2: TlqCoord);
    procedure   DrawLineClipped(var x1, y1, x2, y2: TlqCoord; const AClipRect: TlqRect);
    procedure   ClipLine(var x1, y1, x2, y2: TlqCoord; const AClipRect: TlqRect; out FallsOutsideRegion: Boolean);
    procedure   DrawImage(x, y: TlqCoord; img: TlqImageBase);
    procedure   DrawImagePart(x, y: TlqCoord; img: TlqImageBase; xi, yi, w, h: integer);
    procedure   DrawArc(x, y, w, h: TlqCoord; a1, a2: double);
    procedure   DrawPolygon(const Points: array of TPoint; Winding: Boolean; StartIndex: Integer = 0; NumPts: Integer = -1);
    procedure   DrawPolygon(Points: PPoint; NumPts: Integer; Winding: boolean = False); virtual;
    procedure   DrawPolygon(const Points: array of TPoint);
    procedure   StretchDraw (x, y, w, h: TlqCoord; ASource: TlqImageBase);
    procedure   CopyRect(ADest_x, ADest_y: TlqCoord; ASrcCanvas: TlqCanvasBase; var ASrcRect: TlqRect);
    // x,y is the top/left corner of where the text output will start.
    procedure   DrawString(x, y: TlqCoord; const txt: string);
    procedure   FillRectangle(x, y, w, h: TlqCoord); overload;
    procedure   FillRectangle(r: TlqRect); overload;
    procedure   FillTriangle(x1, y1, x2, y2, x3, y3: TlqCoord);
    procedure   FillArc(x, y, w, h: TlqCoord; a1, a2: double);
    procedure   GradientFill(ARect: TlqRect; AStart, AStop: TlqColor; ADirection: TGradientDirection);
    procedure   XORFillRectangle(col: TlqColor; x, y, w, h: TlqCoord); overload;
    procedure   XORFillRectangle(col: TlqColor; r: TlqRect); overload;
    procedure   SetClipRect(const ARect: TlqRect);
    function    GetClipRect: TlqRect;
    function    GetLineWidth: integer;
    procedure   AddClipRect(const ARect: TlqRect);
    procedure   ClearClipRect;
    procedure   Clear(AColor: TlqColor);
    procedure   GetWinRect(out r: TlqRect);
    procedure   SetColor(AColor: TlqColor);
    procedure   SetTextColor(AColor: TlqColor);
    procedure   SetLineStyle(AWidth: integer; AStyle: TlqLineStyle);
    procedure   SetFont(AFont: TlqFontBase);
    procedure   BeginDraw; overload;
    procedure   BeginDraw(ABuffered: boolean); overload;
    procedure   EndDraw(x, y, w, h: TlqCoord); overload;
    procedure   EndDraw(ARect: TlqRect); overload;
    procedure   EndDraw; overload;
    procedure   FreeResources;
    property    Color: TlqColor read FColor write SetColor;
    property    TextColor: TlqColor read FTextColor write SetTextColor;
    property    Font: TlqFontBase read FFont write SetFont;
    property    Pixels[X, Y: integer]: TlqColor read GetPixel write SetPixel;
    property    InterpolationFilter: TlqCustomInterpolation read FInterpolation write SetInterpolation;
    property    FastDoubleBuffer: Boolean read FFastDoubleBuffer write FFastDoubleBuffer;
    property    LineStyle: TlqLineStyle read FLineStyle;
  end;

  TlqCanvasBaseClass = class of TlqCanvasBase;


  TlqComponent = class(TComponent)
  private
    FTagPointer: Pointer;
    FHelpContext: THelpContext;
    FHelpKeyword: TlqString;
    FHelpType: THelpType;
  protected
    procedure   SetHelpContext(const AValue: THelpContext); virtual;
    procedure   SetHelpKeyword(const AValue: TlqString); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    property    TagPointer: Pointer read FTagPointer write FTagPointer;
  published
    property    HelpContext: THelpContext read FHelpContext write SetHelpContext default 0;
    property    HelpKeyword: TlqString read FHelpKeyword write SetHelpKeyword;
    property    HelpType: THelpType read FHelpType write FHelpType default htKeyword;
  end;


  TlqWindowBase = class(TlqComponent)
  private
    //FParent: TlqWindowBase;
    procedure   SetMouseCursor(const AValue: TMouseCursor);
    function    ConstraintWidth(NewWidth: TlqCoord): TlqCoord;
    function    ConstraintHeight(NewHeight: TlqCoord): TlqCoord;
  protected
    FMouseCursor: TMouseCursor;
    FWindowType: TWindowType;
    FWindowAttributes: TWindowAttributes;
    FTop: TlqCoord;
    FLeft: TlqCoord;
    FWidth: TlqCoord;
    FHeight: TlqCoord;
    FPrevTop: TlqCoord;
    FPrevLeft: TlqCoord;
    FPrevWidth: TlqCoord;
    FPrevHeight: TlqCoord;
    FMinWidth: TlqCoord;
    FMinHeight: TlqCoord;
    FMaxHeight: TlqCoord;
    FMaxWidth: TlqCoord;
    FCanvas: TlqCanvasBase;
    FSizeIsDirty: Boolean;
    FPosIsDirty: Boolean;
    FMouseCursorIsDirty: Boolean;
    FOnDragStartDetected: TNotifyEvent;
    FDragActive: boolean;
    FWindowState: TlqWindowState;
    function    HandleIsValid: boolean; virtual; abstract;
    procedure   DoUpdateWindowPosition; virtual; abstract;
    procedure   DoAllocateWindowHandle(AParent: TlqWindowBase); virtual; abstract;
    procedure   DoReleaseWindowHandle; virtual; abstract;
    procedure   DoRemoveWindowLookup; virtual; abstract;
    procedure   DoSetWindowVisible(const AValue: Boolean); virtual; abstract;
    procedure   DoMoveWindow(const x: TlqCoord; const y: TlqCoord); virtual; abstract;
    function    DoWindowToScreen(ASource: TlqWindowBase; const AScreenPos: TPoint): TPoint; virtual; abstract;
    procedure   DoSetWindowTitle(const ATitle: string); virtual; abstract;
    procedure   DoSetMouseCursor; virtual; abstract;
    procedure   DoDNDEnabled(const AValue: boolean); virtual; abstract;
    procedure   DoAcceptDrops(const AValue: boolean); virtual; abstract;
    function    GetWindowState: TlqWindowState; virtual;
    procedure   SetWindowState(const AValue: TlqWindowState); virtual;
    procedure   DoDragStartDetected; virtual;
    //procedure   SetParent(const AValue: TlqWindowBase); virtual;
    //function    GetParent: TlqWindowBase; virtual;
    function    GetCanvas: TlqCanvasBase; virtual;
    procedure   AllocateWindowHandle; virtual; abstract;
    procedure   ReleaseWindowHandle;
    procedure   SetWindowTitle(const ATitle: string); virtual;
    procedure   SetTop(const AValue: TlqCoord);
    procedure   SetLeft(const AValue: TlqCoord);
    procedure   SetHeight(const AValue: TlqCoord);
    procedure   SetWidth(const AValue: TlqCoord);
    procedure   HandleMove(x, y: TlqCoord); virtual;
    procedure   HandleResize(AWidth, AHeight: TlqCoord); virtual;
    property    OnDragStartDetected: TNotifyEvent read FOnDragStartDetected write FOnDragStartDetected;
    property    WindowState: TlqWindowState read GetWindowState {write SetWindowState} default wsNormal;
  public
    // The standard constructor.
    constructor Create(AOwner: TComponent); override;
    procedure   AfterConstruction; override;
    // Make some setup before the window shows. Forms modify the window creation parameters.
    procedure   AdjustWindowStyle; virtual;
    // Make some setup before the window shows. Invoked after the window is created.
    procedure   SetWindowParameters; virtual;
    // general properties and functions
    function    Right: TlqCoord;
    function    Bottom: TlqCoord;
    procedure   UpdateWindowPosition;
    procedure   MoveWindow(const x: TlqCoord; const y: TlqCoord);
    function    WindowToScreen(ASource: TlqWindowBase; const AScreenPos: TPoint): TPoint;
    //function    HasParent: Boolean; override;
    function    GetClientRect: TlqRect; virtual;
    function    GetBoundsRect: TlqRect; virtual;
    procedure   ActivateWindow; virtual; abstract;
    procedure   CaptureMouse; virtual; abstract;
    procedure   ReleaseMouse; virtual; abstract;
    procedure   BringToFront; virtual; abstract;
    procedure   SetFullscreen(AValue: Boolean); virtual;
    property    HasHandle: boolean read HandleIsValid;
    property    WindowType: TWindowType read FWindowType write FWindowType;
    property    WindowAttributes: TWindowAttributes read FWindowAttributes write FWindowAttributes;
    property    Left: TlqCoord read FLeft write SetLeft;
    property    Top: TlqCoord read FTop write SetTop;
    property    Width: TlqCoord read FWidth write SetWidth;
    property    Height: TlqCoord read FHeight write SetHeight;
    property    MinWidth: TlqCoord read FMinWidth write FMinWidth;
    property    MinHeight: TlqCoord read FMinHeight write FMinHeight;
    property    MaxWidth: TlqCoord read FMaxWidth write FMaxWidth default 0;
    property    MaxHeight: TlqCoord read FMaxHeight write FMaxHeight default 0;
    property    Canvas: TlqCanvasBase read GetCanvas;
    //property    Parent: TlqWindowBase read GetParent write SetParent;
    property    MouseCursor: TMouseCursor read FMouseCursor write SetMouseCursor;
  end;


  TlqApplicationBase = class(TlqComponent)
  private
    FMainForm: TlqWindowBase;
    FTerminated: boolean;
    FCritSect: TCriticalSection;
    FHelpKey: word;
    FHelpFile: TlqString;
    function    GetForm(Index: Integer): TlqWindowBase;
    function    GetFormCount: integer;
    function    GetTopModalForm: TlqWindowBase;
    function    GetHelpFile: TlqString;
  protected
    FOnIdle: TNotifyEvent;
    FIsInitialized: Boolean;
    FModalFormStack: TList;
    function    DoGetFontFaceList: TStringList; virtual; abstract;
    procedure   DoWaitWindowMessage(atimeoutms: integer); virtual; abstract;
    function    MessagesPending: boolean; virtual; abstract;
    function    GetHelpViewer: TlqString; virtual;
  public
    constructor Create(const AParams: string); virtual; reintroduce;
    destructor  Destroy; override;
    function    GetFontFaceList: TStringList;
    procedure   PushModalForm(AForm: TlqWindowBase);
    procedure   PopModalForm;
    function    PrevModalForm: TlqWindowBase;
    function    RemoveWindowFromModalStack(AForm: TlqWindowBase): Integer;
    procedure   CreateForm(InstanceClass: TComponentClass; out Reference);
    function    GetScreenWidth: TlqCoord; virtual; abstract;
    function    GetScreenHeight: TlqCoord; virtual; abstract;
    function    Screen_dpi_x: integer; virtual; abstract;
    function    Screen_dpi_y: integer; virtual; abstract;
    function    Screen_dpi: integer; virtual; abstract;
    procedure   Terminate;
    procedure   Lock;
    procedure   Unlock;
    procedure   InvokeHelp;
    function    ContextHelp(const AHelpContext: THelpContext): Boolean;
    function    KeywordHelp(const AHelpKeyword: string): Boolean;
    property    FormCount: integer read GetFormCount;
    property    Forms[Index: Integer]: TlqWindowBase read GetForm;
    property    HelpContext;
    property    HelpFile: TlqString read GetHelpFile write FHelpFile;
    property    HelpKey: word read FHelpKey write FHelpKey default keyF1;
    property    IsInitialized: boolean read FIsInitialized;
    property    TopModalForm: TlqWindowBase read GetTopModalForm;
    property    MainForm: TlqWindowBase read FMainForm write FMainForm;
    property    Terminated: boolean read FTerminated write FTerminated;
    property    OnIdle: TNotifyEvent read FOnIdle write FOnIdle;
  end;


  TlqClipboardBase = class(TObject)
  protected
    FClipboardWndHandle: TlqWinHandle;
    function    DoGetText: TlqString; virtual; abstract;
    procedure   DoSetText(const AValue: TlqString); virtual; abstract;
    procedure   InitClipboard; virtual; abstract;
  public
    constructor Create; virtual;
    property    Text: TlqString read DoGetText write DoSetText;
  end;


  TFileEntryType = (etFile, etDir);
  TFileListSortOrder = (soNone, soFileName, soCSFileName, soFileExt, soSize, soTime);
  TFileModeString = string[9];


  // A simple data object
  TFileEntry = class(TObject)
  private
    FEntryType: TFileEntryType;
    FExtension: string;
    FName: string;
    FModTime: TDateTime;
    FSize: int64;
    FIsLink: boolean;
    FLinkTarget: string;
    FIsExecutable: boolean;
    FModeString: TFileModeString;
    FOwner: TlqString;
    FGroup: TlqString;
    FAttrString: TFileModeString;
  public
    constructor Create;
    property    Name: string read FName write FName;
    property    Extension: string read FExtension write FExtension;
    property    Size: int64 read FSize write FSize;
    property    EntryType: TFileEntryType read FEntryType write FEntryType;
    property    IsLink: boolean read FIsLink write FIsLink;
    property    LinkTarget: string read FLinkTarget write FLinkTarget;
    property    IsExecutable: boolean read FIsExecutable write FIsExecutable;
    property    ModTime: TDateTime read FModTime write FModTime;
    property    Mode: TFileModeString read FModeString write FModeString;
    property    Owner: TlqString read FOwner write FOwner;
    property    Group: TlqString read FGroup write FGroup;
    property    Attributes: TFileModeString read FAttrString write FAttrString;
  end;


  TlqFileListBase = class(TObject)
  private
    FEntries: TList;
    FDirectoryName: TlqString;
    FFileMask: TlqString;
    FShowHidden: boolean;
    FCurrentSpecialDir: integer;
    procedure   AddEntry(sr: TSearchRec);
    function    GetEntry(i: integer): TFileEntry;
    function    HasAttrib(fileAttrib, testAttrib: Integer): Boolean;
  protected
    FSpecialDirs: TStringList;
    FHasFileMode: boolean;
    function    InitializeEntry(sr: TSearchRec): TFileEntry; virtual;
    procedure   PopulateSpecialDirs(const aDirectory: TlqString); virtual;
  public
    constructor Create; virtual;
    destructor  Destroy; override;
    function    Count: integer;
    function    CurrentSpecialDir: integer;
    function    ReadDirectory(const aDirectory: TlqString = ''): boolean;
    procedure   Clear;
    procedure   Sort(AOrder: TFileListSortOrder);
    property    DirectoryName: TlqString read FDirectoryName;
    property    Entry[i: integer]: TFileEntry read GetEntry;
    property    FileMask: TlqString read FFileMask write FFileMask;
    property    HasFileMode: boolean read FHasFileMode;
    property    ShowHidden: boolean read FShowHidden write FShowHidden;
    property    SpecialDirs: TStringList read FSpecialDirs;
  end;


  TlqMimeDataItem = class(TObject)
  public
    format: TlqString;   { mime string type }
    data: Variant;
    constructor Create(const AFormat: TlqString; const AData: variant); reintroduce;
  end;


  TlqMimeDataBase = class(TObject)
  private
    { TODO: This is wrong, we must have one Data Storage object }
    FDataList: TObjectList;
    FUrlList: TList;
    function    GetItem(AIndex: Integer): TlqMimeDataItem;
    function    Geturls: TList;
    procedure   Seturls(const AValue: TList);
    function    GetText: TlqString;
    procedure   SetText(const AValue: TlqString);
    function    GetHTML: TlqString;
    procedure   SetHTML(const AValue: TlqString);
    function    GetCount: integer;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   Clear;
    function    HasFormat(const AMimeType: TlqString): boolean;
    function    Formats: TStrings;
    function    GetData(const AMimeType: TlqString): Variant;
    procedure   SetData(const AMimeType: TlqString; const AData: Variant);
    property    Items[AIndex: Integer]: TlqMimeDataItem read GetItem; default;
    property    urls: TList read Geturls write Seturls;
    property    Text: TlqString read GetText write SetText;
    property    HTML: TlqString read GetHTML write SetHTML;
    property    Count: integer read GetCount;
  end;


  TlqDragBase = class(TObject)
  protected
    FDragging: Boolean;
    FMimeData: TlqMimeDataBase;
  public
    constructor Create;
    destructor  Destroy; override;
    function    Execute(const ADropActions: TlqDropActions; const ADefaultAction: TlqDropAction = daCopy): TlqDropAction; virtual; abstract;
  end;
  
  
  { TlqBaseTimer }

  TlqBaseTimer = class(TObject)
  private
    FNextAlarm: TDateTime;
    FInterval: integer;
    FOnTimer: TNotifyEvent;
    procedure   SetInterval(const AValue: integer);
  protected
    FEnabled: boolean;
    procedure   SetEnabled(const AValue: boolean); virtual;
  public
    constructor Create(AInterval: integer); virtual;
    destructor  Destroy; override;
    procedure   CheckAlarm(ACurrentTime: TDateTime);
    procedure   Reset;
    procedure   Pause(ASeconds: integer);
    property    Enabled: boolean read FEnabled write SetEnabled;
    property    NextAlarm: TDateTime read FNextAlarm;
    { Interval is in milliseconds. }
    property    Interval: integer read FInterval write SetInterval;
    property    OnTimer: TNotifyEvent read FOnTimer write FOnTimer;
  end;




{ ********  Helper functions  ******** }
{ Keyboard }
function  KeycodeToText(AKey: Word; AShiftState: TShiftState): string;
function  CheckClipboardKey(AKey: Word;  AShiftstate: TShiftState): TClipboardKeyType;

{ Color }
function  lqColorToRGBTriple(const AColor: TlqColor): TRGBTriple;
function  lqColorToFPColor(const AColor: TlqColor): TFPColor; deprecated;
function  RGBTripleTofpgColor(const AColor: TRGBTriple): TlqColor;
function  FPColorTofpgColor(const AColor: TFPColor): TlqColor; deprecated;
function  lqGetRed(const AColor: TlqColor): byte;
function  lqGetGreen(const AColor: TlqColor): byte;
function  lqGetBlue(const AColor: TlqColor): byte;
function  lqGetAlpha(const AColor: TlqColor): byte;
function  lqGetAvgColor(const AColor1, AColor2: TlqColor): TlqColor;
function  lqColor(const ARed, AGreen, ABlue: byte): TlqColor;
function  lqColor(const ARed, AGreen, ABlue, AAlpha: byte): TlqColor;
function  lqDarker(const AColor: TlqColor; APercent: Byte = 50): TlqColor;
function  lqLighter(const AColor: TlqColor; APercent: Byte = 50): TlqColor;


{ Points }
function  PtInRect(const ARect: TlqRect; const APoint: TPoint): Boolean;
procedure SortRect(var ARect: TRect);
procedure SortRect(var ARect: TlqRect);
procedure SortRect(var left, top, right, bottom: integer);



implementation

uses
  lq_main,  // needed for lqApplication & lqNamedColor
  lq_utils, // needed for lqFileList
  lq_constants,
  lq_form,  // needed for .()fpgApplicationCreateForms
  typinfo,
  process,
  {$IFDEF GDEBUG}
  dbugintf,
  {$ENDIF}
  dateutils;


const
  NoDefault = $80000000;
  tkPropsWithDefault = [tkInteger, tkChar, tkSet, tkEnumeration];


function KeycodeToText(AKey: Word; AShiftState: TShiftState): string;

  function GetASCIIText: String;
  var
    c: Char;
  begin
    result := '';
    c := Chr(AKey and $ff);
    case c of
      #13:  Result := Result + rsKeyEnter;
      #127: Result := Result + rsKeyDel;
      else
        Result := Result + c;
    end;
  end;

var
  s: String;
begin
  SetLength(Result, 0);

  { The order of these three are imprortant - don't change them }
  if ssCtrl in AShiftState then
    Result := Result + rsKeyCtrl;
  if ssAlt in AShiftState then
    Result := Result + rsKeyAlt;
  if ssShift in AShiftState then
    Result := Result + rsKeyShift;
  if ssMeta in AShiftState then
    Result := Result + rskeyMeta;

  if (AKey > Ord(' ')) and (AKey < 255) then
  begin
    Result := Result + GetASCIIText;
    Exit; //==>
  end;

  case AKey of
    keyNul:           s := 'Null';
    keyBackSpace:     s := rsKeyBksp;
    keyTab:           s := rsKeyTab;
    keyLinefeed:      s := 'Linefeed';
    keyReturn:        s := rsKeyEnter;
    keyEscape:        s := rsKeyEsc;
    Ord(' '):         s := rsKeySpace;
    keyDelete:        s := rsKeyDel;
    keyVoid:          s := 'Void';
    keyBreak:         s := 'Break';
    keyScrollForw:    s := 'ScrollForw';
    keyScrollBack:    s := 'ScrollBack';
    keyBoot:          s := 'Boot';
    keyCompose:       s := 'Compose';
    keySAK:           s := 'SAK';
    keyUndo:          s := 'Undo';
    keyRedo:          s := 'Redo';
    keyMenu:          s := 'Menu';
    keyCancel:        s := 'Cancel';
    keyPrintScreen:   s := 'PrtScr';
    keyExecute:       s := 'Exec';
    keyFind:          s := 'Find';
    keyBegin:         s := 'Begin';
    keyClear:         s := 'Clear';
    keyInsert:        s := rsKeyIns;
    keySelect:        s := 'Select';
    keyMacro:         s := 'Macro';
    keyHelp:          s := 'Help';
    keyDo:            s := 'Do';
    keyPause:         s := 'Pause';
    keySysRq:         s := 'SysRq';
    keyModeSwitch:    s := 'ModeSw';
    keyUp:            s := rsKeyUp;
    keyDown:          s := rsKeyDown;
    keyLeft:          s := rsKeyLeft;
    keyRight:         s := rsKeyRight;
    keyPrior:         s := rsKeyPgUp;
    keyNext:          s := rsKeyPgDn;
    keyHome:          s := rsKeyHome;
    keyEnd:           s := rsKeyEnd;
    keyF0..keyF64:    s := 'F' + IntToStr(AKey - keyF0);
    keyP0..keyP9:     s := 'KP' + Chr(AKey - keyP0 + Ord('0'));
    keyPA..keyPF:     s := 'KP' + Chr(AKey - keyPA + Ord('A'));
    keyPPlus, keyPMinus, keyPSlash, keyPStar, keyPEqual, keyPSeparator,
      keyPDecimal, keyPParenLeft, keyPParenRight, keyPSpace, keyPEnter,
      keyPTab:        s := 'KP' + GetASCIIText;
    keyPPlusMinus:    s := 'KPPlusMinus';
    keyPBegin:        s := 'KPBegin';
    keyPF1..keyPF9:   s := 'KPF' + IntToStr(AKey - keyPF1);
    keyShiftL:        s := 'ShiftL';
    keyShiftR:        s := 'ShiftR';
    keyCtrlL:         s := 'CtrlL';
    keyCtrlR:         s := 'CtrlR';
    keyAltL:          s := 'AltL';
    keyAltR:          s := 'AltR';
    keyMetaL:         s := 'MetaL';
    keyMetaR:         s := 'MetaR';
    keySuperL:        s := 'SuperL';
    keySuperR:        s := 'SuperR';
    keyHyperL:        s := 'HyperL';
    keyHyperR:        s := 'HyperR';
    keyAltGr:         s := 'AltGr';
    keyCaps:          s := 'Caps';
    keyNum:           s := 'Num';
    keyScroll:        s := 'Scroll';
    keyShiftLock:     s := 'ShiftLock';
    keyCtrlLock:      s := 'CtrlLock';
    keyAltLock:       s := 'AltLock';
    keyMetaLock:      s := 'MetaLock';
    keySuperLock:     s := 'SuperLock';
    keyHyperLock:     s := 'HyperLock';
    keyAltGrLock:     s := 'AltGrLock';
    keyCapsLock:      s := 'CapsLock';
    keyNumLock:       s := 'NumLock';
    keyScrollLock:    s := 'ScrollLock';
    keyDeadRing:      s := 'DeadRing';
    keyDeadCaron:     s := 'DeadCaron';
    keyDeadOgonek:    s := 'DeadOgonek';
    keyDeadIota:      s := 'DeadIota';
    keyDeadDoubleAcute:     s := 'DeadDoubleAcute';
    keyDeadBreve:           s := 'DeadBreve';
    keyDeadAboveDot:        s := 'DeadAboveDot';
    keyDeadBelowDot:        s := 'DeadBelowDot';
    keyDeadVoicedSound:     s := 'DeadVoicedSound';
    keyDeadSemiVoicedSound: s := 'DeadSemiVoicedSound';
    keyDeadAcute:           s := 'DeadAcute';
    keyDeadCedilla:         s := 'DeadCedilla';
    keyDeadCircumflex:      s := 'DeadCircumflex';
    keyDeadDiaeresis:       s := 'DeadDiaeresis';
    keyDeadGrave:           s := 'DeadGrave';
    keyDeadTilde:           s := 'DeadTilde';
    keyDeadMacron:          s := 'DeadMacron';

    keyEcuSign:       s := 'Ecu';
    keyColonSign:     s := 'Colon';
    keyCruzeiroSign:  s := 'Cruzeiro';
    keyFFrancSign:    s := 'FFranc';
    keyLiraSign:      s := 'Lira';
    keyMillSign:      s := 'Mill';
    keyNairaSign:     s := 'Naira';
    keyPesetaSign:    s := 'Peseta';
    keyRupeeSign:     s := 'Rupee';
    keyWonSign:       s := 'Won';
    keyNewSheqelSign: s := 'NewShequel';
    keyDongSign:      s := 'Dong';
    keyEuroSign:      s := 'Euro';
  else
    s := '#' + IntToHex(AKey, 4);
  end;
  Result := Result + s;
end;

function CheckClipboardKey(AKey: Word; AShiftstate: TShiftState): TClipboardKeyType;
var
  c: string;
begin
//  writeln('CheckClipboardKey');
  Result := ckNone;

  if AKey = keyInsert then
  begin
    if (AShiftstate = [ssCtrl]) then
      Result := ckCopy
    else if (AShiftstate = [ssShift]) then
      Result := ckPaste;
  end
  else if (AKey = keyDelete) and (AShiftstate = [ssShift]) then
    Result := ckCut
  else if (AShiftstate = [ssCtrl]) then
  begin
    c := KeycodeToText(AKey, []);   // case is not important
//    Writeln('Key: ', c);
    if c = 'C' then
      Result := ckCopy
    else if c = 'V' then
      Result := ckPaste
    else if c = 'X' then
      Result := ckCut;
  end  { if/else }
end;

function lqColorToRGBTriple(const AColor: TlqColor): TRGBTriple;
begin
  with Result do
  begin
    Red   := lqGetRed(AColor);
    Green := lqGetGreen(AColor);
    Blue  := lqGetBlue(AColor);
    Alpha := lqGetAlpha(AColor);
  end
end;

function lqColorToFPColor(const AColor: TlqColor): TFPColor; deprecated;
begin
  with Result do
  begin
    Red   := lqGetRed(AColor);
    Green := lqGetGreen(AColor);
    Blue  := lqGetBlue(AColor);
    Alpha := lqGetAlpha(AColor);
  end
end;

function RGBTripleTofpgColor(const AColor: TRGBTriple): TlqColor;
begin
  Result := AColor.Blue or (AColor.Green shl 8) or (AColor.Red shl 16) or (AColor.Alpha shl 24);
end;

function FPColorTofpgColor(const AColor: TFPColor): TlqColor; deprecated;
begin
  Result := AColor.Blue or (AColor.Green shl 8) or (AColor.Red shl 16) or (AColor.Alpha shl 24);
end;

function lqGetRed(const AColor: TlqColor): byte;
var
  c: TlqColor;
begin
  c := lqColorToRGB(AColor);
  // AARRGGBB format
  Result := (c shr 16) and $FF;
end;

function lqGetGreen(const AColor: TlqColor): byte;
var
  c: TlqColor;
begin
  c := lqColorToRGB(AColor);
  // AARRGGBB format
  Result := (c shr 8) and $FF;
end;

function lqGetBlue(const AColor: TlqColor): byte;
var
  c: TlqColor;
begin
  c := lqColorToRGB(AColor);
  // AARRGGBB format
  Result := c and $FF;
end;

function lqGetAlpha(const AColor: TlqColor): byte;
var
  c: TlqColor;
begin
  c := lqColorToRGB(AColor);
  // AARRGGBB format
  Result := (c shr 24) and $FF;
end;

function lqGetAvgColor(const AColor1, AColor2: TlqColor): TlqColor;
var
  c1, c2: TRGBTriple;
  avg: TRGBTriple;
begin
  c1 := lqColorToRGBTriple(AColor1);
  c2 := lqColorToRGBTriple(AColor2);
  avg.Red   := c1.Red + (c2.Red - c1.Red) div 2;
  avg.Green := c1.Green + (c2.Green - c1.Green) div 2;
  avg.Blue  := c1.Blue + (c2.Blue - c1.Blue) div 2;
  avg.Alpha := c1.Alpha + (c2.Alpha - c1.Alpha) div 2;
  Result := RGBTripleTofpgColor(avg);
end;

function lqColor(const ARed, AGreen, ABlue: byte): TlqColor;
begin
  { color is always fully opaque }
  Result := ABlue or (AGreen shl 8) or (ARed shl 16) or ($FF shl 24);
end;

function lqColor(const ARed, AGreen, ABlue, AAlpha: byte): TlqColor;
begin
  Result := ABlue or (AGreen shl 8) or (ARed shl 16) or (AAlpha shl 24);
end;

function lqDarker(const AColor: TlqColor; APercent: Byte): TlqColor;
var
  lColor: TRGBTriple;
begin
  lColor.Red := lqGetRed(AColor);
  lColor.Green := lqGetGreen(AColor);
  lColor.Blue := lqGetBlue(AColor);
  lColor.Red := Round(lColor.Red*APercent/100);
  lColor.Green := Round(lColor.Green*APercent/100);
  lColor.Blue := Round(lColor.Blue*APercent/100);
  Result := RGBTripleTofpgColor(lColor);
end;

function lqLighter(const AColor: TlqColor; APercent: Byte): TlqColor;
var
  lColor: TRGBTriple;
begin
  lColor.Red := lqGetRed(AColor);
  lColor.Green := lqGetGreen(AColor);
  lColor.Blue := lqGetBlue(AColor);
  lColor.Red := Round((lColor.Red*APercent/100) + (255 - APercent/100*255));
  lColor.Green := Round((lColor.Green*APercent/100) + (255 - APercent/100*255));
  lColor.Blue := Round((lColor.Blue*APercent/100) + (255 - APercent/100*255));
  Result := RGBTripleTofpgColor(lColor);
end;

function PtInRect(const ARect: TlqRect; const APoint: TPoint): Boolean;
begin
  Result := (APoint.x >= ARect.Left) and
            (APoint.y >= ARect.Top) and
            (APoint.x <= ARect.Right) and
            (APoint.y <= ARect.Bottom);
end;

procedure SortRect(var ARect: TRect);
begin
  with ARect do
    SortRect(left, top, right, bottom);
end;

procedure SortRect(var ARect: TlqRect);
var
  r: TlqCoord;
  b: TlqCoord;
begin
  r := ARect.Right;
  b := ARect.Bottom;
  SortRect(ARect.Left, ARect.Top, r, b);
  ARect.SetRight(r);
  ARect.SetBottom(b);
end;

procedure SortRect(var left, top, right, bottom: integer);
var
  r: integer;
begin
  if left > right then
  begin
    r       := left;
    left    := right;
    right   := r;
  end;
  if top > bottom then
  begin
    r       := top;
    top     := bottom;
    bottom  := r;
  end;
end;

// This function uses RTTI to automatically set the default values of properties.
// That means we don't have to do it in the constructor anymore! :-)
procedure SetDefaults(Obj: TObject);
var
  PropInfos: PPropList;
  Count, Loop: Integer;
begin
  PropInfos := nil;
  { Find out how many properties we'll be considering }
  Count := GetPropList(Obj.ClassInfo, tkPropsWithDefault, nil);
  { Allocate memory to hold their RTTI data }
  GetMem(PropInfos, Count * SizeOf(PPropInfo));
  try
    { Get hold of the property list in our new buffer }
    GetPropList(Obj.ClassInfo, tkPropsWithDefault, PropInfos);
    { Loop through all the selected properties }
    for Loop := 0 to Count - 1 do
    begin
      with PropInfos^[Loop]^ do
      begin
        { If there is supposed to be a default value... }
        if Default <> NoDefault then
          { ...then jolly well set it }
          SetOrdProp(Obj, PropInfos^[Loop], Default)
      end;
    end;
  finally
    FreeMem(PropInfos, Count * SizeOf(PPropInfo));
  end;
end;

{ TlqRect }

procedure TlqRect.SetRect(aleft, atop, awidth, aheight: TlqCoord);
begin
  Left   := aleft;
  Top    := atop;
  Width  := awidth;
  Height := aheight;
end;

function TlqRect.Bottom: TlqCoord;
begin
  Result := Top + Height - 1;
end;

function TlqRect.Right: TlqCoord;
begin
  Result := Left + Width - 1;
end;

procedure TlqRect.SetBottom(Value: TlqCoord);
begin
  Height := Value - Top + 1;
end;

procedure TlqRect.SetRight(Value: TlqCoord);
begin
  Width := Value - Left + 1;
end;


{ TlqPoint }

procedure TlqPoint.SetPoint(AX, AY: integer);
begin
  X := AX;
  Y := AY;
end;

function TlqPoint.ManhattanLength: integer;
begin
  Result := Abs(X) + Abs(Y);
end;

function TlqPoint.ManhattanLength(const PointB: TlqPoint): integer;
begin
  Result := Abs(PointB.X-X) + Abs(PointB.Y-Y);
end;


{ TlqSize }

procedure TlqSize.SetSize(AWidth, AHeight: integer);
begin
  W := AWidth;
  H := AHeight;
end;


{ TlqWindowBase }

procedure TlqWindowBase.SetMouseCursor(const AValue: TMouseCursor);
begin
  if FMouseCursor = AValue then
    Exit; //==>
  FMouseCursor := AValue;
  DoSetMouseCursor;
end;

function TlqWindowBase.ConstraintWidth(NewWidth: TlqCoord): TlqCoord;
begin
  Result := NewWidth;
  if (MaxWidth >= MinWidth) and (Result > MaxWidth) and (MaxWidth > 0) then
    Result := MaxWidth;
  if Result < MinWidth then
    Result := MinWidth;
end;

function TlqWindowBase.ConstraintHeight(NewHeight: TlqCoord): TlqCoord;
begin
  Result := NewHeight;
  if (MaxHeight >= MinHeight) and (Result > MaxHeight) and (MaxHeight > 0) then
    Result := MaxHeight;
  if Result < MinHeight then
    Result := MinHeight;
end;

function TlqWindowBase.GetWindowState: TlqWindowState;
begin
  Result := FWindowState;
end;

procedure TlqWindowBase.SetWindowState(const AValue: TlqWindowState);
begin
  // do nothing
end;

procedure TlqWindowBase.DoDragStartDetected;
begin
  if Assigned(FOnDragStartDetected) then
    FOnDragStartDetected(self);
end;

{procedure TlqWindowBase.SetParent(const AValue: TlqWindowBase);
begin
  FParent := AValue;
end;

function TlqWindowBase.GetParent: TlqWindowBase;
begin
  result := FParent;
end;}

function TlqWindowBase.GetCanvas: TlqCanvasBase;
begin
  Result := FCanvas;
end;

{//FIXME: Move it to lq_main
procedure TlqWindowBase.AllocateWindowHandle;
begin
  DoAllocateWindowHandle(FParent);
  if FMouseCursorIsDirty then
    DoSetMouseCursor;
end; }

procedure TlqWindowBase.ReleaseWindowHandle;
begin
  if HasHandle then
  begin
    Canvas.FreeResources;
    DoReleaseWindowHandle;
  end;
  DoRemoveWindowLookup;
end;

procedure TlqWindowBase.SetWindowTitle(const ATitle: string);
begin
  DoSetWindowTitle(ATitle);
end;

procedure TlqWindowBase.SetTop(const AValue: TlqCoord);
begin
  HandleMove(Left, AValue);
end;

procedure TlqWindowBase.SetLeft(const AValue: TlqCoord);
begin
  HandleMove(AValue, Top);
end;

procedure TlqWindowBase.SetHeight(const AValue: TlqCoord);
begin
  HandleResize(Width, AValue);
end;

procedure TlqWindowBase.SetWidth(const AValue: TlqCoord);
begin
  HandleResize(AValue, Height);
end;

procedure TlqWindowBase.HandleMove(x, y: TlqCoord);
begin
  if FTop <> y then
  begin
    if not (csLoading in ComponentState) then
      FPrevTop := FTop
    else
      FPrevTop := y;
    FTop := y;
    FPosIsDirty := FPosIsDirty or (FTop <> FPrevTop);
  end;

  if FLeft <> x then
  begin
    if not (csLoading in ComponentState) then
      FPrevLeft := FLeft
    else
      FPrevLeft := x;
    FLeft := x;
    FPosIsDirty := FPosIsDirty or (FLeft <> FPrevLeft);
  end;
end;

procedure TlqWindowBase.HandleResize(AWidth, AHeight: TlqCoord);
begin
  if FWidth <> AWidth then
  begin
    if not (csLoading in ComponentState) then
      FPrevWidth := FWidth
    else
      FPrevWidth := AWidth;
    FWidth := ConstraintWidth(AWidth);
    FSizeIsDirty := FSizeIsDirty or (FWidth <> FPrevWidth);
  end;

  if FHeight <> AHeight then
  begin
    if not (csLoading in ComponentState) then
      FPrevHeight := FHeight
    else
      FPrevHeight := AHeight;
    FHeight := ConstraintHeight(AHeight);
    FSizeIsDirty := FSizeIsDirty or (FHeight <> FPrevHeight);
  end;
end;

constructor TlqWindowBase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMouseCursor := mcDefault;
  FMouseCursorIsDirty := False;
  FPosIsDirty := True;
  FSizeIsDirty := True;
  FMaxWidth := 0;
  FMaxHeight := 0;
  FDragActive := False;
  FWindowState := wsNormal;
end;

procedure TlqWindowBase.AfterConstruction;
begin
  inherited AfterConstruction;
  { There is a neater way by using RTTI to set default property values all
    automatically. No need to duplicate the efforts and manually set the
    property default values in the constructor. This code is now the same for
    each TlqWindowBase descendant (which includes GUI widgets) }
//  SetDefaults(self);
end;

procedure TlqWindowBase.AdjustWindowStyle;
begin
  // does nothing here
end;

procedure TlqWindowBase.SetWindowParameters;
begin
  // does nothing
end;

function TlqWindowBase.Right: TlqCoord;
begin
  Result := FLeft + FWidth - 1;
end;

function TlqWindowBase.Bottom: TlqCoord;
begin
  Result := FTop + FHeight - 1;
end;

procedure TlqWindowBase.UpdateWindowPosition;
begin
  DoUpdateWindowPosition;
end;

procedure TlqWindowBase.MoveWindow(const x: TlqCoord; const y: TlqCoord);
begin
  Left  := x;
  Top   := y;
  DoMoveWindow(x, y);
end;

function TlqWindowBase.WindowToScreen(ASource: TlqWindowBase; const AScreenPos: TPoint): TPoint;
begin
  Result := DoWindowToScreen(ASource, AScreenPos);
end;

function TlqWindowBase.GetClientRect: TlqRect;
begin
  Result.SetRect(0, 0, Width, Height);
end;

function TlqWindowBase.GetBoundsRect: TlqRect;
begin
  Result.SetRect(Left, Top, Width+1, Height+1);
end;

procedure TlqWindowBase.SetFullscreen(AValue: Boolean);
begin
  if AValue then
    Include(FWindowAttributes, waFullScreen)
  else
    Exclude(FWindowAttributes, waFullScreen);
  // now decendants must override this and implement the actualy fullscreen part
end;

{ TlqCanvasBase }

procedure TlqCanvasBase.SetInterpolation(const AValue: TlqCustomInterpolation);
begin
  FInterpolation.Free;
  FInterpolation := AValue;
end;

constructor TlqCanvasBase.Create(awin: TlqWindowBase);
begin
  FBufferedDraw := True;
  FFastDoubleBuffer := True;
  FWindow := awin;
end;

destructor TlqCanvasBase.Destroy;
begin
  FInterpolation.Free;
  inherited Destroy;
end;

procedure TlqCanvasBase.DrawRectangle(x, y, w, h: TlqCoord);
begin
  DoDrawRectangle(x, y, w, h);
end;

procedure TlqCanvasBase.DrawRectangle(r: TlqRect);
begin
  DoDrawRectangle(r.Left, r.Top, r.Width, r.Height);
end;

procedure TlqCanvasBase.DrawLine(x1, y1, x2, y2: TlqCoord);
begin
  DoDrawLine(x1, y1, x2, y2);
end;

procedure TlqCanvasBase.DrawLineClipped(var x1, y1, x2, y2: TlqCoord;
  const AClipRect: TlqRect);
var
  OutOfRegion: boolean;
begin
  ClipLine(X1, Y1, X2, Y2, AClipRect, OutOfRegion);
  if not OutOfRegion then
    DrawLine(X1, Y1, X2, Y2);                { Draw the new line!            }
end;

{ DrawLineClipped - This procedure clips a line to the AClipRect boundaries and
 then calls the DrawLine procedure with the clipped coordinates.  If the line
 lies completely outside of the clip boundary, then the Line routine is not
 called.  This procedure uses the well known Cohen-Sutherland line clipping
 algorithm to clip each coordinate.

 Use this if you did not what to change the Canvas.ClipRegion for some reason.
 For a detailed explanation see:
   http://www.nondot.org/~sabre/graphpro/line6.html                           }
procedure TlqCanvasBase.ClipLine(var x1, y1, x2, y2: TlqCoord;
  const AClipRect: TlqRect; out FallsOutsideRegion: Boolean);
CONST
  CodeBottom = 1; CodeTop    = 2;             { BitFields for output codes }
  CodeLeft   = 4; CodeRight  = 8;

  FUNCTION CompOutCode(X, Y : INTEGER) : integer;  { Nested function }
  VAR Code : integer;
  BEGIN
    Code := 0;
    IF      Y > AClipRect.Bottom THEN Code := CodeBottom
    ELSE IF Y < AClipRect.Top THEN Code := CodeTop;
    IF      X > AClipRect.Right THEN Code := Code+CodeRight
    ELSE IF X < AClipRect.Left THEN Code := Code+CodeLeft;
    Result := Code;
  END;

VAR
  OutCode0,         { The code of the first endpoint  }
  OutCode1,         { The code of the second endpoint }
  OutCodeOut : integer;
  X, Y : INTEGER;
BEGIN
  FallsOutsideRegion := False;
  OutCode0 := CompOutCode(X1, Y1);            { Compute the original codes   }
  OutCode1 := CompOutCode(X2, Y2);

  WHILE (OutCode0 <> 0) OR (OutCode1 <> 0) DO { While not Trivially Accepted }
  BEGIN
    IF (OutCode0 AND OutCode1) <> 0 THEN      { Trivial Reject }
    begin
      FallsOutsideRegion := True;
      Exit;   //==>
    end
    ELSE
    BEGIN        { Failed both tests, so calculate the line segment to clip }
      IF OutCode0 > 0 THEN
        OutCodeOut := OutCode0    { Clip the first point }
      ELSE
        OutCodeOut := OutCode1;   { Clip the last point  }

      IF (OutCodeOut AND CodeBottom) = CodeBottom THEN
      BEGIN               { Clip the line to the bottom of the viewport     }
        Y := AClipRect.Bottom;
        X := X1+LONGINT(X2-X1)*LONGINT(Y-Y1) DIV (Y2 - Y1);
      END
      ELSE IF (OutCodeOut AND CodeTop) = CodeTop THEN
      BEGIN               { Clip the line to the top of the viewport        }
        Y := AClipRect.Top;
        X := X1+LONGINT(X2-X1)*LONGINT(Y-Y1) DIV (Y2 - Y1);
      END
      ELSE IF (OutCodeOut AND CodeRight) = CodeRight THEN
      BEGIN               { Clip the line to the right edge of the viewport }
        X := AClipRect.Right;
        Y := Y1+LONGINT(Y2-Y1)*LONGINT(X-X1) DIV (X2-X1);
      END
      ELSE IF (OutCodeOut AND CodeLeft) = CodeLeft THEN
      BEGIN               { Clip the line to the left edge of the viewport  }
        X := AClipRect.Left;
        Y := Y1+LONGINT(Y2-Y1)*LONGINT(X-X1) DIV (X2-X1);
      END;

      IF (OutCodeOut = OutCode0) THEN       { Modify the first coordinate   }
      BEGIN
        X1 := X; Y1 := Y;                   { Update temporary variables    }
        OutCode0 := CompOutCode(X1, Y1);    { Recalculate the OutCode       }
      END
      ELSE                                  { Modify the second coordinate  }
      BEGIN
        X2 := X; Y2 := Y;                   { Update temporary variables    }
        OutCode1 := CompOutCode(X2, Y2);    { Recalculate the OutCode       }
      END;
    END;
  END;  { while }
end;

procedure TlqCanvasBase.DrawImage(x, y: TlqCoord; img: TlqImageBase);
begin
  if img = nil then
    Exit; //==>
  DrawImagePart(x, y, img, 0, 0, img.Width, img.Height);
end;

procedure TlqCanvasBase.DrawImagePart(x, y: TlqCoord; img: TlqImageBase; xi,
  yi, w, h: integer);
begin
  DoDrawImagePart(x, y, img, xi, yi, w, h);
end;

procedure TlqCanvasBase.DrawArc(x, y, w, h: TlqCoord; a1, a2: double);
begin
  DoDrawArc(x, y, w, h, a1, a2);
end;

{ Use Polygon to draw a closed, many-sided shape on the canvas, using the value
  of Canvas.Color. The shape is always filled.
  The Points parameter is an array of points that give the vertices of the
  polygon.
  Winding determines how the polygon is filled. When Winding is True, Polygon
  fills the shape using the Winding fill algorithm. When Winding is False,
  Polygon uses the even-odd (alternative) fill algorithm.
  StartIndex gives the index of the first point in the array to use. All points
  before this are ignored.
  NumPts indicates the number of points to use, starting at StartIndex.
  If NumPts is -1 (the default), Polygon uses all points from StartIndex to the
  end of the array.
  The first point is always connected to the last point.
  To draw a polygon on the canvas, without filling it, use the Polyline method,
  specifying the first point a second time at the end. }
procedure TlqCanvasBase.DrawPolygon(const Points: array of TPoint;
  Winding: Boolean; StartIndex: Integer; NumPts: Integer);
var
  NPoints: integer;
begin
  if NumPts<0 then
    NPoints:=High(Points)-StartIndex+1
  else
    NPoints:=NumPts;
  if NPoints<=0 then exit;
  DrawPolygon(@Points[StartIndex],NPoints,Winding);
end;

procedure TlqCanvasBase.DrawPolygon(Points: PPoint; NumPts: Integer;
  Winding: boolean);
begin
  if NumPts<=0 then exit;
  DoDrawPolygon(Points,NumPts,Winding);
end;

procedure TlqCanvasBase.DrawPolygon(const Points: array of TPoint);
begin
  DrawPolygon(Points, True, Low(Points), High(Points) - Low(Points) + 1);
end;

procedure TlqCanvasBase.StretchDraw(x, y, w, h: TlqCoord; ASource: TlqImageBase);
var
  FreeInterpolation: boolean;
  IP: TlqCustomInterpolation;
begin
  FreeInterpolation := not Assigned(FInterpolation);
  if FreeInterpolation then
    IP := TlqMitchelInterpolation.Create
  else
    IP := FInterpolation;
  try
    IP.Initialize(ASource, self);
    IP.Execute(x, y, w, h);
  finally
    if FreeInterpolation then
      IP.Free;
  end;
end;

procedure TlqCanvasBase.CopyRect(ADest_x, ADest_y: TlqCoord; ASrcCanvas: TlqCanvasBase;
  var ASrcRect: TlqRect);
var
  x, sx, y, sy: TlqCoord;
begin
  SortRect(ASrcRect);
  // X position of source
  for sx := ASrcRect.Left to ASrcRect.Right do
  begin
    x := ADest_x + (sx - ASrcRect.Left);  // calc dest x
    // Y position of source
    for sy := ASrcRect.Top to ASrcRect.Bottom do
    begin
      y := ADest_y + (sy - ASrcRect.Top); // calc dest y
      Pixels[x, y] := ASrcCanvas.Pixels[sx, sy];
    end;
  end;
end;

procedure TlqCanvasBase.DrawString(x, y: TlqCoord; const txt: string);
var
  underline: integer;
begin
  DoDrawString(x, y, txt);

  { What was not handled: underline }
  if Pos('UNDERLINE', UpperCase(Font.FontDesc)) > 0 then
  begin
    underline := (Font.Descent div 2) + 1;
    if underline = 0 then
      underline := 1;
    if underline >= Font.Descent then
      underline := Font.Descent - 1;

    DoSetLineStyle(1, lsSolid);
    DoSetColor(TextColor);
    DoDrawLine(x, y+Font.Height-underline, x+Font.TextWidth(txt), y+Font.Height-underline);
  end;
end;

procedure TlqCanvasBase.FillRectangle(x, y, w, h: TlqCoord);
begin
  DoFillRectangle(x, y, w, h);
end;

procedure TlqCanvasBase.FillRectangle(r: TlqRect);
begin
  DoFillRectangle(r.Left, r.Top, r.Width, r.Height);
end;

procedure TlqCanvasBase.FillTriangle(x1, y1, x2, y2, x3, y3: TlqCoord);
begin
  DoFillTriangle(x1, y1, x2, y2, x3, y3);
end;

procedure TlqCanvasBase.FillArc(x, y, w, h: TlqCoord; a1, a2: double);
begin
  DoFillArc(x, y, w, h, a1, a2);
end;

procedure TlqCanvasBase.GradientFill(ARect: TlqRect; AStart, AStop: TlqColor;
  ADirection: TGradientDirection);
var
  RGBStart: TRGBTriple;
  RGBStop: TRGBTriple;
  RDiff, GDiff, BDiff: Integer;
  count: Integer;
  i: Integer;
  newcolor: TRGBTriple;
begin
  RGBStart := lqColorToRGBTriple(AStart);
  RGBStop  := lqColorToRGBTriple(AStop);

  if ADirection = gdVertical then
    count := ARect.Bottom - ARect.Top
  else
    count := ARect.Right - ARect.Left;

  RDiff := RGBStop.Red - RGBStart.Red;
  GDiff := RGBStop.Green - RGBStart.Green;
  BDiff := RGBStop.Blue - RGBStart.Blue;

//  Changing;
  for i := 0 to count do
  begin
    newcolor.Red    := RGBStart.Red + (i * RDiff) div count;
    newcolor.Green  := RGBStart.Green + (i * GDiff) div count;
    newcolor.Blue   := RGBStart.Blue + (i * BDiff) div count;
    SetColor(RGBTripleTofpgColor(newcolor));

    // We have to overshoot by 1 pixel as DrawLine paints 1 pixel short (by design)
    if ADirection = gdHorizontal then
      DrawLine(ARect.Left+i, ARect.Top, ARect.Left+i, ARect.Bottom+1)
    else
      DrawLine(ARect.Left, ARect.Top+i, ARect.Right+1, ARect.Top+i);
  end;
//  Changed;
end;

procedure TlqCanvasBase.XORFillRectangle(col: TlqColor; x, y, w, h: TlqCoord);
begin
  DoXORFillRectangle(col, x, y, w, h);
end;

procedure TlqCanvasBase.XORFillRectangle(col: TlqColor; r: TlqRect);
begin
  DoXORFillRectangle(col, r.Left, r.Top, r.Width, r.Height);
end;

procedure TlqCanvasBase.SetClipRect(const ARect: TlqRect);
begin
  DoSetClipRect(ARect);
end;

function TlqCanvasBase.GetClipRect: TlqRect;
begin
  Result := DoGetClipRect;
end;

function TlqCanvasBase.GetLineWidth: integer;
begin
  Result := FLineWidth;
end;

procedure TlqCanvasBase.AddClipRect(const ARect: TlqRect);
begin
  DoAddClipRect(ARect);
end;

procedure TlqCanvasBase.ClearClipRect;
begin
  DoClearClipRect;
end;

procedure TlqCanvasBase.Clear(AColor: TlqColor);
var
  lCol:     TlqColor;
  lWinRect: TlqRect;
begin
  lCol := FColor;
  DoSetColor(AColor);
  DoGetWinRect(lWinRect);
  DoFillRectangle(0, 0, lWinRect.Width, lWinRect.Height);
  DoSetColor(lCol);
end;

procedure TlqCanvasBase.GetWinRect(out r: TlqRect);
begin
  DoGetWinRect(r);
end;

procedure TlqCanvasBase.SetColor(AColor: TlqColor);
begin
  FColor := AColor;
  DoSetColor(FColor);
end;

procedure TlqCanvasBase.SetTextColor(AColor: TlqColor);
begin
  FTextColor := AColor;
  DoSetTextColor(FTextColor);
end;

procedure TlqCanvasBase.SetLineStyle(AWidth: integer; AStyle: TlqLineStyle);
begin
  FLineWidth := AWidth;
  FLineStyle := AStyle;
  DoSetLineStyle(FLineWidth, FLineStyle);
end;

procedure TlqCanvasBase.SetFont(AFont: TlqFontBase);
begin
  if AFont = nil then
    exit;
  FFont := AFont;
  DoSetFontRes(AFont.FFontRes);
end;

procedure TlqCanvasBase.BeginDraw;
begin
  BeginDraw(FBufferedDraw);
end;

procedure TlqCanvasBase.BeginDraw(ABuffered: boolean);
begin
  if FBeginDrawCount < 1 then
  begin
    DoBeginDraw(FWindow, ABuffered);

    SetColor(clText1);
    SetTextColor(clText1);
    SetFont(lqApplication.DefaultFont);
    SetLineStyle(1, lsSolid);

    FBeginDrawCount := 0;
  end;
  Inc(FBeginDrawCount);
end;

procedure TlqCanvasBase.EndDraw(x, y, w, h: TlqCoord);
begin
  if FBeginDrawCount > 0 then
  begin
    Dec(FBeginDrawCount);
    if FBeginDrawCount = 0 then
    begin
      DoPutBufferToScreen(x, y, w, h);

      if not FPersistentResources then
        DoEndDraw;
    end;
  end;  { if }
end;

procedure TlqCanvasBase.EndDraw(ARect: TlqRect);
begin
  EndDraw(ARect.Left, ARect.Top, ARect.Width, ARect.Height);
end;

procedure TlqCanvasBase.EndDraw;
begin
  EndDraw(0, 0, FWindow.Width, FWindow.Height);
end;

procedure TlqCanvasBase.FreeResources;
begin
  DoEndDraw;
  FBeginDrawCount := 0;
end;

{ TlqFontBase }

function TlqFontBase.GetIsFixedWidth: boolean;
begin
  // very crude but handy as a fallback option
  if (Pos('mono', Lowercase(FFontDesc)) > 0) or
     (Pos('courier', Lowercase(FFontDesc)) > 0) or
     (Pos('fixed', Lowercase(FFontDesc)) > 0) then
    Result := True
  else
    Result := False;
end;

function TlqFontBase.TextWidth(const txt: TlqString): integer;
begin
  if Length(txt) = 0 then
    Result := 0
  else
    Result := FFontRes.GetTextWidth(txt);
end;

function TlqFontBase.Ascent: integer;
begin
  Result := FFontRes.GetAscent;
end;

function TlqFontBase.Descent: integer;
begin
  Result := FFontRes.GetDescent;
end;

function TlqFontBase.Height: integer;
begin
  Result := FFontRes.GetHeight;
end;

{ TlqCustomInterpolation }

procedure TlqCustomInterpolation.Initialize(AImage: TlqImageBase;
  ACanvas: TlqCanvasBase);
begin
  FImage  := AImage;
  FCanvas := ACanvas;
end;

{ TlqBaseInterpolation }

type
  TlqInterpolationContribution = record
    weight: double;
    place: integer;
  end;

function ColorRound(c: double): word;
begin
  if c > $FFFF then
    result := $FFFF
  else if c < 0.0 then
    result := 0
  else
    result := round(c);
end;


procedure TlqBaseInterpolation.Horizontal(width: integer);
var
  x, y, r: integer;
  start, stop, maxcontribs: integer;
  center, re, gr, bl, density: double;
  contributions: array[0..10] of TlqInterpolationContribution;
  dif, w, gamma, a: double;
  c: TlqColor;
  rgb: TRGBTriple;
begin
  for x := 0 to Width - 1 do
  begin
    center := x * xfactor;
    start  := round(center - xsupport);
    if start < 0 then
      start := 0;
    stop := round(center + xsupport);
    if stop >= image.Width then
      stop := image.Width - 1;
    density     := 0.0;
    maxcontribs := -1;
    for r := start to stop do
    begin
      dif := r - center;
      w   := Filter(dif);
      if w > 0.0 then
      begin
        Inc(maxcontribs);
        with contributions[maxcontribs] do
        begin
          weight  := w;
          density := density + w;
          place   := r;
        end;
      end;
    end;
    if (density <> 0.0) and (density <> 1.0) then
    begin
      density := 1.0 / density;
      for r := 0 to maxcontribs do
        contributions[r].weight := contributions[r].weight * density;
    end;
    for y := 0 to image.Height - 1 do
    begin
      gamma := 0.0;
      re    := 0.0;
      gr    := 0.0;
      bl    := 0.0;
      for r := 0 to maxcontribs do
        with contributions[r] do
        begin
          c   := image.colors[place, y];
          rgb := lqColorToRGBTriple(c);
          a     := weight; // * rgb.Alpha / $FFFF;
          re    := re + a * rgb.Red;
          gr    := gr + a * rgb.Green;
          bl    := bl + a * rgb.Blue;
          gamma := gamma + a;
        end;  { with }
      with rgb do
      begin
        red   := ColorRound(re);
        green := ColorRound(gr);
        blue  := ColorRound(bl);
//        alpha := ColorRound(gamma * $FFFF);
      end;
      tempimage.colors[x, y] := RGBTripleTofpgColor(rgb);
    end;
  end;
end;

procedure TlqBaseInterpolation.Vertical(dx, dy, width, height: integer);
var
  x, y, r: integer;
  start, stop, maxcontribs: integer;
  center, re, gr, bl, density: double;
  contributions: array[0..10] of TlqInterpolationContribution;
  dif, w, gamma, a: double;
  c: TlqColor;
  rgb: TRGBTriple;
begin
  for y := 0 to Height - 1 do
  begin
    center := y * yfactor;
    start  := round(center - ysupport);
    if start < 0 then
      start := 0;
    stop := round(center + ysupport);
    if stop >= tempimage.Height then
      stop := tempimage.Height - 1;
    density     := 0.0;
    maxcontribs := -1;
    for r := start to stop do
    begin
      dif := r - center;
      w   := Filter(dif);
      if w > 0.0 then
      begin
        Inc(maxcontribs);
        with contributions[maxcontribs] do
        begin
          weight  := w;
          density := density + w;
          place   := r;
        end;
      end;
    end;
    if (density <> 0.0) and (density <> 1.0) then
    begin
      density := 1.0 / density;
      for r := 0 to maxcontribs do
        contributions[r].weight := contributions[r].weight * density;
    end;
    for x := 0 to Width - 1 do
    begin
      gamma := 0.0;
      re    := 0.0;
      gr    := 0.0;
      bl    := 0.0;
      for r := 0 to maxcontribs do
        with contributions[r] do
        begin
          c := tempimage.colors[x, place];
          rgb := lqColorToRGBTriple(c);
          a     := weight;// * rgb.alpha / $FFFF;
          re    := re + a * rgb.red;
          gr    := gr + a * rgb.green;
          bl    := bl + a * rgb.blue;
          gamma := gamma + a;
        end;  { width }
      with rgb do
      begin
        red   := ColorRound(re);
        green := ColorRound(gr);
        blue  := ColorRound(bl);
//        alpha := ColorRound(gamma * $FFFF);
      end;
      Canvas.Pixels[x + dx, y + dy] := RGBTripleTofpgColor(rgb);
    end;
  end;
end;

procedure TlqBaseInterpolation.Execute(x, y, w, h: integer);
begin
  tempimage := TlqImageBase.Create;
  tempimage.AllocateImage(image.ColorDepth, w, image.Height);

  xfactor   := image.Width / w;
  yfactor   := image.Height / h;
  if xfactor > 1.0 then
    xsupport := MaxSupport
  else
    xsupport := xfactor * MaxSupport;
  if yfactor > 1.0 then
    ysupport := MaxSupport
  else
    ysupport := yfactor * MaxSupport;
  Horizontal(w);
  Vertical(x, y, w, h);
end;

destructor TlqBaseInterpolation.Destroy;
begin
  tempimage.Free;
  inherited Destroy;
end;

{ TlqMitchelInterpolation }

function TlqMitchelInterpolation.Filter(x: double): double;
const
  B  = (1.0/3.0);
  C  = (1.0/3.0);
  P0 = ((  6.0- 2.0*B       )/6.0);
  P2 = ((-18.0+12.0*B+ 6.0*C)/6.0);
  P3 = (( 12.0- 9.0*B- 6.0*C)/6.0);
  Q0 = ((       8.0*B+24.0*C)/6.0);
  Q1 = ((     -12.0*B-48.0*C)/6.0);
  Q2 = ((       6.0*B+30.0*C)/6.0);
  Q3 = ((     - 1.0*B- 6.0*C)/6.0);
begin
  if (x < -2.0) then
    result := 0.0
  else if (x < -1.0) then
    result := Q0-x*(Q1-x*(Q2-x*Q3))
  else if (x < 0.0) then
    result := P0+x*x*(P2-x*P3)
  else if (x < 1.0) then
    result := P0+x*x*(P2+x*P3)
  else if (x < 2.0) then
    result := Q0+x*(Q1+x*(Q2+x*Q3))
  else
  result := 0.0;
end;

function TlqMitchelInterpolation.MaxSupport: double;
begin
  result := 2.0;
end;


{ TlqImageBase }

function TlqImageBase.GetColor(x, y: TlqCoord): TlqColor;
var
  p: Plongword;
begin
  p := FImageData;
  Inc(p, (FWidth * y) + x);
  Result := TlqColor(p^);
//  write(IntToHex(Result, 6) + ' ');
end;

procedure TlqImageBase.SetColor(x, y: TlqCoord; const AValue: TlqColor);
var
  p: Plongword;
begin
  p := FImageData;
  Inc(p, (FWidth * y) + x);
  p^ := AValue;
//  write(IntToHex(AValue, 6) + ' ');
end;

constructor TlqImageBase.Create;
begin
  FWidth      := 0;
  FHeight     := 0;
  FColorDepth := 0;

  FImageData     := nil;
  FImageDataSize := 0;
  FMaskData      := nil;
  FMaskDataSize  := 0;
  FMasked        := False;
  FMaskPoint     := Point(0, 0);
end;

destructor TlqImageBase.Destroy;
begin
  FreeImage;
  inherited Destroy;
end;

procedure TlqImageBase.Invert(IncludeMask: Boolean);
var
  p: ^byte;
  n: integer;
begin
  if FImageData = nil then
    Exit; //==>

  p := FImageData;
  for n := 1 to FImageDataSize do
  begin
    p^ := p^ xor $FF;
    Inc(p);
  end;

  if IncludeMask then
  begin
    if FMaskData <> nil then
    begin
      p := FMaskData;
      for n := 1 to FMaskDataSize do
      begin
        p^ := p^ xor $FF;
        Inc(p);
      end;
    end;
  end;
end;

procedure TlqImageBase.FreeImage;
begin
  if FImageData <> nil then
    FreeMem(FImageData, FImageDataSize);
  FImageData     := nil;
  FImageDataSize := 0;
  if FMaskData <> nil then
    FreeMem(FMaskData, FMaskDataSize);
  FMaskData     := nil;
  FMaskDataSize := 0;
  FMasked       := False;
  FWidth        := 0;
  FHeight       := 0;
//  DoFreeImage;
end;

procedure TlqImageBase.AllocateImage(acolordepth, awidth, aheight: integer);
var
  dww: integer;
begin
  FreeImage;
  FWidth      := awidth;
  FHeight     := aheight;
  FColorDepth := acolordepth;

  // Real bitmap
  if FColorDepth = 1 then
    dww := (awidth + 31) div 32
  else
    dww := FWidth;

  FImageDataSize := dww * FHeight * 4;
  FImageData := nil;
  GetMem(FImageData, FImageDataSize);
  if FImageData = nil then
    raise Exception.Create('Failed to allocate ' + IntToStr(FImageDataSize) + 'bytes of memory for FImageData');
end;

procedure TlqImageBase.AllocateMask;
var
  dww: integer;
begin
  if (FWidth < 1) or (FHeight < 1) then
    Exit; //==>

  FMasked := True;
  if FMaskData <> nil then
    FreeMem(FMaskData);

  dww           := (FWidth + 31) div 32;
  FMaskDataSize := dww * FHeight * 4;
  GetMem(FMaskData, FMaskDataSize);
end;

procedure TlqImageBase.CreateMaskFromSample(x, y: TlqCoord);
var
  p: ^longword;
  pmsk: ^byte;
  c, n: longword;
  linecnt: integer;
  pixelcnt: integer;
  bit: byte;
  msklinelen: integer;
  row, col: integer;
begin
  if FColorDepth = 1 then
    Exit; //==>

  if (FImageData = nil) then
    Exit; //==>

{$ifdef AGGCanvas}
  p := FImageData;
  if x < 0 then
    Inc(p, FWidth - 1)
  else
    Inc(p, x);
  if y < 0 then
    Inc(p, FWidth * (FHeight - 1))
  else
    Inc(p, FWidth * y);

  c := p^;  // the sample

  for row := 0 to FHeight-1 do
  begin
    for col := 0 to FWidth-1 do
    begin
      n := PLongWord(FImageData)[row * FWidth + col];
      if n = c then
        { set Alpha value 100% transparent }
        PLongWord(FImageData)[row * FWidth + col] := n and $00FFFFFF;
    end;
  end;

{$else}

  AllocateMask;
  FMaskPoint := Point(x, y);

  p := FImageData;
  if x < 0 then
    Inc(p, FWidth - 1)
  else
    Inc(p, x);
  if y < 0 then
    Inc(p, FWidth * (FHeight - 1))
  else
    Inc(p, FWidth * y);

  c := p^;  // the sample

  msklinelen := FWidth div 32;
  if (FWidth and $1F) > 0 then
    Inc(msklinelen);

  msklinelen := msklinelen shl 2;

  p       := FImageData;
  linecnt := 0;

  repeat
    pixelcnt := 0;
    bit      := $80;
    pmsk     := FMaskData;
    Inc(pmsk, linecnt * msklinelen);

    repeat
      if bit = $80 then
        pmsk^ := 0;

      if p^ <> c then
        pmsk^ := pmsk^ or bit;

      Inc(p);
      Inc(pixelcnt);

      if bit = 1 then
      begin
        bit := $80;
        Inc(pmsk);
      end
      else
        bit := bit shr 1;
    until pixelcnt >= FWidth;

    Inc(linecnt);
  until linecnt >= FHeight;
{$endif}
end;

procedure TlqImageBase.UpdateImage;
begin
  if FImageData <> nil then
    DoInitImage(FColorDepth, FWidth, FHeight, FImageData);

  if FMaskData <> nil then
    DoInitImageMask(FWidth, FHeight, FMaskData);
end;

{ TlqApplicationBase }

function TlqApplicationBase.GetTopModalForm: TlqWindowBase;
begin
  Result := nil;
  if (FModalFormStack <> nil) and (FModalFormStack.Count > 0) then
    Result := TlqWindowBase(FModalFormStack.Items[FModalFormStack.Count-1]);
end;

function TlqApplicationBase.GetHelpFile: TlqString;
begin
  Result := FHelpFile;
  //if Result = '' then
  //begin
    { TODO : Should we extend this to try the <applicationname>.inf as a help file? }
  //end;
end;

function TlqApplicationBase.GetHelpViewer: TlqString;
var
  ext: TlqString;
begin
  // Default location is in same directory as current running application
  // This location might change in the future.
  ext := lqExtractFileExt(ParamStr(0));
  Result := lqExtractFilePath(ParamStr(0)) + LQ_HELPVIEWER + ext;
end;

constructor TlqApplicationBase.Create(const AParams: string);
begin
  inherited Create(nil);
  FModalFormStack := TList.Create;
  FCritSect := TCriticalSection.Create;
  FHelpKey := keyF1;
  FHelpType := htContext;
end;

destructor TlqApplicationBase.Destroy;
begin
  FCritSect.Free;
  inherited Destroy;
end;

function TlqApplicationBase.GetFormCount: integer;
begin
  Result := ComponentCount;
end;

function TlqApplicationBase.GetForm(Index: Integer): TlqWindowBase;
begin
  Result := TlqWindowBase(Components[Index]);
end;

function TlqApplicationBase.GetFontFaceList: TStringList;
begin
  Result := DoGetFontFaceList;
end;

procedure TlqApplicationBase.PushModalForm(AForm: TlqWindowBase);
var
  StackIndex: Integer;
begin
  if FModalFormStack = nil then
    Exit;
  StackIndex := FModalFormStack.IndexOf(AForm);
  if StackIndex = -1 then
    FModalFormStack.Add(AForm)
  //else move to top of stack?
end;

procedure TlqApplicationBase.PopModalForm;
begin
  if FModalFormStack = nil then
    Exit;
  if FModalFormStack.Count > 0 then
    FModalFormStack.Delete(FModalFormStack.Count-1);
end;

function TlqApplicationBase.PrevModalForm: TlqWindowBase;
begin
  Result := nil;
  if FModalFormStack = nil then
    Exit;
  if FModalFormStack.Count < 2 then
    Exit;

  Result := TlqWindowBase(FModalFormStack.Items[FModalFormStack.Count-2]);
end;

function TlqApplicationBase.RemoveWindowFromModalStack (AForm: TlqWindowBase): Integer;
begin
  Result := FModalFormStack.Remove(AForm);
end;

procedure TlqApplicationBase.CreateForm(InstanceClass: TComponentClass; out Reference);
var
  Instance: TComponent;
  ok: boolean;
  AForm: TlqForm;
begin
  // Allocate the instance, without calling the constructor
  Instance := TComponent(InstanceClass.NewInstance);
  // set the Reference before the constructor is called, so that
  // events and constructors can refer to it
  TComponent(Reference) := Instance;

  ok:=false;
  try
    Instance.Create(Self);
    ok:=true;
  finally
    if not ok then
    begin
      TComponent(Reference) := nil;
    end;
  end;

  if (Instance is TlqForm) then
  begin
    AForm := TlqForm(Instance);
    if FMainForm = nil then
      FMainForm := AForm;
  end;
end;

procedure TlqApplicationBase.Terminate;
var
  i: integer;
begin
  // make sure all forms are closed before main form
  for i := FormCount - 1 downto 0 do
    if Forms[i] <> MainForm then
      lqSendMessage(Self, Forms[i], LQM_CLOSE); // SendMessage waits for it to complete. Post doesn't.
  Terminated := True;
end;

procedure TlqApplicationBase.Lock;
begin
  FCritSect.Enter;
end;

procedure TlqApplicationBase.Unlock;
begin
  FCritSect.Leave;
end;

procedure TlqApplicationBase.InvokeHelp;
begin
  { TODO -oGraeme -cHelp System : We should probably try ActiveForm and ActiveWidget help first. }
  if HelpType = htKeyword then
    KeywordHelp(HelpKeyword)
  else
    ContextHelp(HelpContext);
end;

function TlqApplicationBase.ContextHelp(const AHelpContext: THelpContext): Boolean;
var
  p: TProcess;
begin
  Result := False;
  if not lqFileExists(GetHelpViewer) then
    raise ELiteKitUserFeedbackException.Create(rsfailedtofindhelpviewer);
  p := TProcess.Create(nil);
  try
    if lqFileExists(HelpFile) then
    begin
      if AHelpContext = 0 then
        p.CommandLine := GetHelpViewer + ' ' + HelpFile
      else
        p.CommandLine := GetHelpViewer + ' ' + HelpFile + ' -n ' + IntToStr(AHelpContext);
        {$ifdef GDEBUG}
        senddebug(p.CommandLine);
        {$endif}
    end
    else
      p.CommandLine := GetHelpViewer;
    Result := True;
    p.Execute;
  finally
    p.Free;
  end;
end;

function TlqApplicationBase.KeywordHelp(const AHelpKeyword: string): Boolean;
var
  p: TProcess;
begin
  Result := False;
  if not lqFileExists(GetHelpViewer) then
    raise ELiteKitUserFeedbackException.Create(rsfailedtofindhelpviewer);
  p := TProcess.Create(nil);
  try
    if lqFileExists(HelpFile) then
    begin
      p.CommandLine := GetHelpViewer + ' ' + HelpFile + ' -s ' + AHelpKeyword;
      {$ifdef GDEBUG}
      senddebug(p.CommandLine);
      {$endif}
    end
    else
      p.CommandLine := GetHelpViewer;
    Result := True;
    p.Execute;
  finally
    p.Free;
  end;
end;

{ TlqClipboardBase }

constructor TlqClipboardBase.Create;
begin
  inherited Create;
  InitClipboard;
end;

// Helper functions for TFileEntry and TlqFileListBase

function StringMatches(const astr, apat: string): boolean;
var
  pati, si: longint;
begin
  result := True;
  pati := 1;
  si := 1;
  while result and (si <= length(astr)) and (pati <= length(apat)) do
  begin
    if (apat[pati] = '?') or (apat[pati] = astr[si]) then
    begin
      inc(si);
      inc(pati);
    end
    else if (apat[pati] = '*') then
    begin
      while (pati <= length(apat)) and (apat[pati] in ['?','*']) do
        inc(pati);
      if pati > length(apat) then
      begin
        si := length(astr)+1;
        Break;   // * at the end
      end;

      while (si <= length(astr)) and (astr[si] <> apat[pati]) do
        inc(si);
      if si > length(astr) then
        result := False;
    end
    else
    begin
      result := False;
    end;
  end;

  result := result and (si > length(astr));
end;

// multiple patterns separated with ;
function FileNameMatches(const astr, apats: string): boolean;
var
  cpat: string;
  p: integer;
  s: string;
  astrupper: string;
begin
  astrupper := UpperCase(astr);
  result := False;
  s := apats;
  repeat
    cpat := '';
    p := pos(';',s);
    if p > 0 then
    begin
      cpat := copy(s, 1, p-1);
      delete(s, 1, p);
    end
    else
    begin
      cpat := s;
      s := '';
    end;  { if/else }
    cpat := UpperCase(trim(cpat));
    if cpat <> '' then
      result := StringMatches(astrupper, cpat);
  until result or (cpat = '');
end;

{ TFileEntry }

constructor TFileEntry.Create;
begin
  {FAttributes := 0;
  FMode := 0;}
  FAttrString := '';
  FModeString := '';
  FSize := 0;
  FIsLink := False;
  FIsExecutable := false;
  FEntryType := etFile;
end;

{ TlqFileListBase }

procedure TlqFileListBase.AddEntry(sr: TSearchRec);
var
  e: TFileEntry;
begin
  e := InitializeEntry(sr);
  if Assigned(e) then
    FEntries.Add(e);
end;

function TlqFileListBase.HasAttrib(fileAttrib, testAttrib: Integer): Boolean;
begin
  { HasAttrib() tests whether or not a file (with attributes fileAttrib) has the
  testAttrib attribute bit set. }
  Result := (fileAttrib and testAttrib) <> 0;
end;

function TlqFileListBase.GetEntry(i: integer): TFileEntry;
begin
  if (i < 0) or (i > FEntries.Count-1) then
    Result := nil
  else
    Result := TFileEntry(FEntries[i]);
end;

function TlqFileListBase.InitializeEntry(sr: TSearchRec): TFileEntry;
var
  e: TFileEntry;
begin
  e := TFileEntry.Create;
  e.Name        := lqFromOSEncoding(sr.Name);
  e.Extension   := lqExtractFileExt(e.Name);
  e.Size        := sr.Size;
  // e.Attributes  := sr.Attr; // this is incorrect and needs to improve!
  e.ModTime     := FileDateToDateTime(sr.Time);

  if HasAttrib(sr.Attr, faDirectory) then
    e.EntryType := etDir
  else
    e.EntryType := etFile;

  if (e.Name = '.') or
     ((e.Name = '..') and (FDirectoryName = '/')) or
     (not FShowHidden and (Copy(e.Name, 1, 1) = '.') and (Copy(e.Name, 2, 1) <> '.')) or
//       (not FShowHidden and HasAttrib(sr.Attr, faHidden)) or
     ((e.EntryType = etFile) and not FileNameMatches(e.Name, FFileMask)) then
  begin
    // do not add this entry
    e.Free;
    Result := nil;
  end else
    Result := e;
end;

procedure TlqFileListBase.PopulateSpecialDirs(const aDirectory: TlqString);
{Sets up FSpecialDirs list}
var
  i, n, sp: integer;
begin
  // FSpecialDirs under Windows will be all available drive letters.
  // FSpecialDirs under Linux is the root (/)

  // find insert position in FSpecialDirs where we can insert all parts
  // of aDirectory.
  i := 0;
  // We have to use UpperCase() because under Windows aDirectory's drive
  // letter could be lower case, but Win API returns initial drive letters
  // in upper case, so the second test could never be false causing
  // Index out of bounds error further down.
  while (i < FSpecialDirs.Count)
    and (UpperCase(FSpecialDirs.Strings[i][1]) < UpperCase(aDirectory[1])) do
      Inc(i);

  sp := Pos(DirectorySeparator, aDirectory) + 1;
  n := sp;
  while n < Length(aDirectory) do
  begin
    if aDirectory[n] = DirectorySeparator then
    begin
      Inc(i);
      FSpecialDirs.Insert(i, Copy(aDirectory, 1, n-1));
    end;
    Inc(n);
  end;

  if (n > sp) then
  begin
    Inc(i);
    FSpecialDirs.Insert(i, ExcludeTrailingPathDelimiter(aDirectory))
  end;

  FCurrentSpecialDir := i;
end;

constructor TlqFileListBase.Create;
begin
  FEntries := TList.Create;
  FFileMask := '*';
  FDirectoryName := '';
  FSpecialDirs := TStringList.Create;
end;

destructor TlqFileListBase.Destroy;
begin
  Clear;
  FSpecialDirs.Free;
  FEntries.Free;
  inherited Destroy;
end;

function TlqFileListBase.Count: integer;
begin
  Result := FEntries.Count;
end;

function TlqFileListBase.CurrentSpecialDir: integer;
begin
  Result := FCurrentSpecialDir;
end;

function TlqFileListBase.ReadDirectory(const aDirectory: TlqString = ''): boolean;
var
  SearchRec: TSearchRec;
  dir: TlqString; //  to prevent FDirectoryName from having incorrect value
begin
  Result:=False;
  // default parameter value is current directory
  if aDirectory <> '' then
    dir := lqExpandFileName(aDirectory)
  else
    dir := lqGetCurrentDir;

  // vvzh: now we have to use SetCurrentDir in order to make ExpandFileName work
  if not lqSetCurrentDir(dir) then
    Exit; //==>

  // Add PathDelim to end if it doesn't yet exist
  FDirectoryName := IncludeTrailingPathDelimiter(dir);
  PopulateSpecialDirs(FDirectoryName);

  Clear;
  try
    // The extra 'or' includes Normal attribute files under Windows. faAnyFile doesn't return those.
    // Reported to FPC as bug 9440 in Mantis.
    if lqFindFirst(FDirectoryName + AllFilesMask, faAnyFile or $00000080, SearchRec) = 0 then
    begin
      AddEntry(SearchRec);
      while lqFindNext(SearchRec) = 0 do
      begin
        AddEntry(SearchRec);
      end;
    end;
    Result:=True;
  finally
    SysUtils.FindClose(SearchRec);
  end;
end;

procedure TlqFileListBase.Clear;
var
  n: integer;
begin
  for n := 0 to FEntries.Count-1 do
    TFileEntry(FEntries[n]).Free;
  FEntries.Clear;
end;

procedure TlqFileListBase.Sort(AOrder: TFileListSortOrder);
var
  newl: TList;
  n: integer;
  i: integer;
  e: TFileEntry;

  function IsBefore(newitem, item: TFileEntry): boolean;
  begin
    //if newitem.etype = etDir then writeln('dir: ',newitem.name,' (',item.name,')');
    if (newitem.EntryType = etDir) and (item.EntryType <> etDir) then
    begin
      result := true;
    end
    else if (newitem.EntryType <> etDir) and (item.EntryType = etDir) then
    begin
      result := false;
    end
    else if (newitem.EntryType = etDir) and (newitem.Name = '..') then
    begin
      result := true;
    end
    else if (item.EntryType = etDir) and (item.Name = '..') then
    begin
      result := false;
    end
    else
      case AOrder of
        soFileName   : result := UpperCase(newitem.Name) < UpperCase(item.Name);
        soCSFileName : result := newitem.Name < item.Name;
        soFileExt    : result := UpperCase(newitem.Extension+' '+newitem.Name) < UpperCase(item.Extension+' '+item.Name);
        soSize       : result := newitem.size < item.size;
        soTime       : result := newitem.modtime < item.modtime;
      else
        result := False;
      end;
  end;

begin
  newl := TList.Create;
  for n := 0 to FEntries.Count-1 do
  begin
    e := TFileEntry(FEntries[n]);
    i := 0;
    while (i < newl.Count) and not IsBefore(e,TFileEntry(newl[i])) do inc(i);
    newl.Insert(i,e);
  end;
  FEntries.Free;
  FEntries := newl;
end;

{ TlqComponent }

procedure TlqComponent.SetHelpContext(const AValue: THelpContext);
begin
  if not (csLoading in ComponentState) then
    FHelpType := htContext;
  if FHelpContext = AValue then
    Exit; //==>
  FHelpContext := AValue;
end;

procedure TlqComponent.SetHelpKeyword(const AValue: TlqString);
begin
  if not (csLoading in ComponentState) then
    FHelpType := htKeyword;
  if FHelpKeyword = AValue then
    Exit; //==>
  FHelpKeyword := AValue;
end;

constructor TlqComponent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHelpType     := htKeyword;
  FHelpContext  := 0;
  FHelpKeyword  := '';
  FTagPointer   := nil;
end;

{ TlqMimeDataItem }

constructor TlqMimeDataItem.Create(const AFormat: TlqString; const AData: variant);
begin
  inherited Create;
  format := AFormat;
  data := AData;
end;


{ TlqMimeDataBase }

function TlqMimeDataBase.Geturls: TList;
begin
  { TODO: We should only return data related to MIME type:  text/uri-list }
  Result := nil;
end;

function TlqMimeDataBase.GetItem(AIndex: Integer): TlqMimeDataItem;
begin
  Result := TlqMimeDataItem(FDataList[AIndex]);
end;

procedure TlqMimeDataBase.Seturls(const AValue: TList);
begin
  if AValue = nil then
    raise Exception.Create('Source URI list must not be nil');

  if Assigned(FUrlList) then
    FUrlList.Free;

  { We take ownership of AValue. Can we do this? }
  FUrlList := AValue;
//  FFormats.Clear;
//  Formats.Add('text/uri-list');
end;

function TlqMimeDataBase.GetText: TlqString;
var
  i: integer;
  s: string;
begin
  { TODO: if no text/plain, but we have HTML, we must strip all tags and return that }
  for i := 0 to Count-1 do
  begin
    if Items[i].format = 'text/plain' then
    begin
      s := Items[i].data;
      Result := s;
      break;
    end;
  end;
end;

procedure TlqMimeDataBase.SetText(const AValue: TlqString);
var
  i: integer;
  r: TlqMimeDataItem;
begin
  { remove existing 'text/plain' first }
  for i := Count-1 downto 0 do
  begin
    r := Items[i];
    if r.format = 'text/plain' then
    begin
      FDataList.Remove(r);
      break;
    end;
  end;
  { now add new structure }
  r := TlqMimeDataItem.Create('text/plain', AValue);
  FDataList.Add(r);
end;

function TlqMimeDataBase.GetHTML: TlqString;
var
  i: integer;
  s: string;
begin
  { TODO: if data was HTML, we must strip all tags - regex will make this easy }
  for i := 0 to Count-1 do
  begin
    if Items[i].format = 'text/html' then
    begin
      s := Items[i].data;
      Result := s;
      break;
    end;
  end;
end;

procedure TlqMimeDataBase.SetHTML(const AValue: TlqString);
var
  i: integer;
  r: TlqMimeDataItem;
begin
  { remove existing 'text/html' first }
  for i := Count-1 downto 0 do
  begin
    r := Items[i];
    if r.format = 'text/html' then
    begin
      FDataList.Remove(r);
      break;
    end;
  end;
  { now add new structure }
  r := TlqMimeDataItem.Create('text/html', AValue);
  FDataList.Add(r);
end;

function TlqMimeDataBase.GetCount: integer;
begin
  Result := FDataList.Count;
end;

constructor TlqMimeDataBase.Create;
begin
  inherited Create;
  FDataList := TObjectList.Create;
end;

destructor TlqMimeDataBase.Destroy;
begin
  FDataList.Free;
  inherited Destroy;
end;

procedure TlqMimeDataBase.Clear;
begin
  FUrlList.Clear;
  FDataList.Clear;
end;

function TlqMimeDataBase.HasFormat(const AMimeType: TlqString): boolean;
var
  i: integer;
begin
  Result := False;
  for i := 0 to Count-1 do
  begin
    Result := Items[i].format = AMimeType;
    if Result then
      break;
  end;
end;

function TlqMimeDataBase.Formats: TStrings;
var
  i: integer;
  r: TlqMimeDataItem;
  s: string;
begin
  if Count = 0 then
    Result := nil
  else
  begin
    Result := TStringList.Create;
    for i := 0 to Count-1 do
    begin
      s := Items[i].format;
      Result.Add(s);
    end;
  end;
end;

function TlqMimeDataBase.GetData(const AMimeType: TlqString): Variant;
var
  i: integer;
begin
  for i := 0 to Count-1 do
  begin
    if Items[i].format = AMimeType then
    begin
      Result := Items[i].data;
      break;
    end;
  end;
end;

procedure TlqMimeDataBase.SetData(const AMimeType: TlqString; const AData: Variant);
var
  i: integer;
  r: TlqMimeDataItem;
begin
  { remove existing mime type first }
  for i := Count-1 downto 0 do
  begin
    r := Items[i];
    if r.format = AMimeType then
    begin
      FDataList.Remove(r);
      break;
    end;
  end;
  { now add new structure }
  r := TlqMimeDataItem.Create(AMimeType, AData);
  FDataList.Add(r);
end;


{ TlqDragBase }

constructor TlqDragBase.Create;
begin
  inherited Create;
  FDragging := False;
end;

destructor TlqDragBase.Destroy;
begin
  FMimeData.Free;
  inherited Destroy;
end;


{ TlqBaseTimer }

procedure TlqBaseTimer.SetInterval(const AValue: integer);
begin
  FInterval := AValue;
  FNextAlarm := Now + (FInterval * ONE_MILISEC);
end;

procedure TlqBaseTimer.SetEnabled(const AValue: boolean);
begin
  if AValue and (FInterval <= 0) then
     Exit;
  if (not FEnabled) and AValue then
    FNextAlarm := now + (interval * ONE_MILISEC);
  FEnabled := AValue;
end;

constructor TlqBaseTimer.Create(AInterval: integer);
begin
  inherited Create;
  FInterval := AInterval;
  FEnabled  := False;
  OnTimer   := nil;
end;

destructor TlqBaseTimer.Destroy;
begin
  Enabled := False;
  inherited Destroy;
end;

procedure TlqBaseTimer.CheckAlarm(ACurrentTime: TDateTime);
begin
  if not FEnabled then
    Exit; //==>

  if FNextAlarm <= ACurrentTime then
  begin
    // set the next alarm point
    if Interval > 0 then
      while FNextAlarm <= ACurrentTime do
        FNextAlarm += (Interval * ONE_MILISEC);

    if Assigned(FOnTimer) then
      FOnTimer(self);
  end;
end;

procedure TlqBaseTimer.Reset;
begin
  Enabled := False;
  Enabled := True;
end;

procedure TlqBaseTimer.Pause(ASeconds: integer);
begin
  if Enabled then
  begin
    FNextAlarm := IncSecond(Now, ASeconds);
  end;
end;



end.





