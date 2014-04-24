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
      Defines a Page Control and Tab Sheets.
}

unit lq_tab;

{$mode objfpc}{$H+}

{
  TODO:
    * Tab Styles (tab, button, flat button, angled)
    * Better keyboard support
    * Focus rectangle drawn on tabs itself
    * FindNextPage() must be implemented
    * Popup menu for tab selection. Should occur with RClick on tabs.
}

interface

uses
  Classes,
  SysUtils,
  lq_base,
  lq_main,
  lq_widget,
  lq_button,
  lq_menu;
  
type
  // forward declaration
  TlqPageControl = class;
  
  TlqTabStyle    = (tsTabs, tsButtons, tsFlatButtons);
  TlqTabPosition = (tpTop, tpBottom, tpLeft, tpRight, tpNone);
  TlqTabOption   = (to_PMenuClose, to_PMenuShowAvailTabs);

  TlqTabOptions = set of TlqTabOption;


  TlqTabSheet = class(TlqWidget)
  private
    FPageControl: TlqPageControl;
    //FText: string;
    FTabVisible: boolean;
    function    GetPageControl: TlqPageControl;
    function    GetPageIndex: Integer;
    function    GetText: string;
    procedure   SetPageIndex(const AValue: Integer);
    procedure   SetText(const AValue: string);
    procedure   SetPageControl(APageControl: TlqPageControl);
  protected
    procedure   HandlePaint; override;
    procedure   SetName(const NewName: TComponentName); override;
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateWithTitle(AOwner: TComponent; const AText: TlqString = ''); virtual;
    destructor  Destroy; override;
    procedure   AfterConstruction; override;
    property    PageIndex: Integer read GetPageIndex write SetPageIndex;
    property    PageControl: TlqPageControl read FPageControl write SetPageControl;
    property    TabVisible: boolean read FTabVisible write FTabVisible;
  published
    property    BackgroundColor;
    property    Enabled;
    property    Text: string read GetText write SetText;
    property    OnPaint;
  end;


  TTabSheetChange = procedure(Sender: TObject; NewActiveSheet: TlqTabSheet) of object;
  TTabSheetClosing = procedure(Sender: TObject; ATabSheet: TlqTabSheet) of object;
  
  
  TlqPageControl = class(TlqWidget)
  private
    FFont: TlqFont;
    FActivePage: TlqTabSheet;
    FMargin: integer;
    FFixedTabWidth: integer;
    FFixedTabHeight: Integer;
    FOnClosingTabSheet: TTabSheetClosing;
    FPages: TList;
    FActivePageIndex: integer;
    FOnChange: TTabSheetChange;
    FRightButton: TlqButton;         // bottom/right
    FLeftButton: TlqButton;          // left/top
    FFirstTabButton: TlqTabSheet;    // when tabs don't fit in screen this is the first button on screen when tabs are scrolled
    FSortPages: boolean;
    FStyle: TlqTabStyle;
    FTabPosition: TlqTabPosition;
    FPopupMenu: TlqPopupMenu;
    FTabOptions: TlqTabOptions;
    FLastRClickPos: TlqPoint;
    FUpdateCount: Integer;
    FActiveTabColor: TlqColor;
    function    GetActivePageIndex: integer;
    function    GetPage(AIndex: integer): TlqTabSheet;
    function    GetPageCount: Integer;
    procedure   InsertPage(var APage: TlqTabSheet; SuppressOnChangeEvent: boolean = False);
    procedure   RemovePage(const APage: TlqTabSheet);
    procedure   SetActivePageIndex(const AValue: integer);
    procedure   SetActivePage(const AValue: TlqTabSheet);
    procedure   PositionTabSheets;
    procedure   PositionTabSheet(var APage: TlqTabSheet);
    function    MaxButtonWidthSum: integer;
    function    MaxButtonHeightSum: integer;
    function    MaxButtonWidth: integer;
    function    ButtonHeight: integer;
    function    ButtonWidth(AText: string): integer;
    procedure   SetFixedTabWidth(const AValue: integer);
    procedure   SetFixedTabHeight(const AValue: integer);
    function    GetTabText(AText: string): string;
    procedure   LeftButtonClick(Sender: TObject);
    procedure   RightButtonClick(Sender: TObject);
    function    FindNextPage(ACurrent: TlqTabSheet; AForward: boolean): TlqTabSheet;
    procedure   SetSortPages(const AValue: boolean);
    procedure   SetStyle(const AValue: TlqTabStyle);
    procedure   SetTabPosition(const AValue: TlqTabPosition);
    procedure   DoPageChange(ATabSheet: TlqTabSheet);
    procedure   DoTabSheetClosing(ATabSheet: TlqTabSheet);
    function    DrawTab(const rect: TlqRect; const Selected: Boolean = False; const Mode: Integer = 1): TlqRect;
    procedure   pmCloseTab(Sender: TObject);
    function    GetActiveTabColor: TlqColor;
    procedure   SetActiveTabColor(AValue: TlqColor);
  protected
    procedure   SetBackgroundColor(const AValue: TlqColor); override;
    procedure   OrderSheets; // currently using bubblesort
    procedure   RePaintTitles; virtual;
    procedure   HandlePaint; override;
    procedure   HandleShow; override;
    procedure   HandleLMouseUp(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleRMouseUp(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); override;
    procedure   RePaint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   BeginUpdate;
    procedure   EndUpdate;
    function    TabSheetAtPos(const x, y: integer): TlqTabSheet;
    function    AppendTabSheet(ATitle: string): TlqTabSheet;
    procedure   RemoveTabSheet(ATabSheet: TlqTabSheet);
    property    PageCount: Integer read GetPageCount;
    property    ActivePage: TlqTabSheet read FActivePage write SetActivePage;
    property    Pages[AIndex: integer]: TlqTabSheet read GetPage;
    property    OnChange: TTabSheetChange read FOnChange write FOnChange;
    property    OnClosingTabSheet: TTabSheetClosing read FOnClosingTabSheet write FOnClosingTabSheet;
  published
    property    ActivePageIndex: integer read GetActivePageIndex write SetActivePageIndex default 0;
    property    ActiveTabColor: TlqColor read GetActiveTabColor write SetActiveTabColor default clWindowBackground;
    property    Align;
    property    BackgroundColor;
    property    Enabled;
    property    FixedTabWidth: integer read FFixedTabWidth write SetFixedTabWidth default 0;
    property    FixedTabHeight: integer read FFixedTabHeight write SetFixedTabHeight default 21;
    property    Hint;
    property    Options: TlqTabOptions read FTabOptions write FTabOptions;
    property    ParentShowHint;
    property    ShowHint;
    property    SortPages: boolean read FSortPages write SetSortPages default False;
    property    Style: TlqTabStyle read FStyle write SetStyle default tsTabs;
    property    TabOrder;
    property    TabPosition: TlqTabPosition read FTabPosition write SetTabPosition default tpTop;
    property    TextColor;
    property    OnShowHint;
  end;


implementation

uses
  lq_stringutils;
  
  
// compare function used by FPages.Sort

function SortCompare(Item1, Item2: Pointer): integer;
begin
  Result := CompareText(TlqTabSheet(Item1).Text, TlqTabSheet(Item2).Text);
end;

{ TlqTabSheet }

function TlqTabSheet.GetPageControl: TlqPageControl;
begin
  if Owner is TlqPageControl then
    Result := TlqPageControl(Owner)
  else
    Result := nil;
end;

function TlqTabSheet.GetPageIndex: Integer;
begin
  if PageControl <> nil then
    Result := PageControl.FPages.IndexOf(Self)
  else
    Result := -1;
end;

function TlqTabSheet.GetText: string;
begin
  Result := FText;
end;

procedure TlqTabSheet.SetPageIndex(const AValue: Integer);
begin
  if PageControl <> nil then
  begin
    PageControl.FPages.Move(PageIndex, AValue);
    PageControl.RePaint;//Titles;
  end;
end;

procedure TlqTabSheet.SetText(const AValue: string);
begin
  if FText = AValue then
    Exit; //==>
  FText := AValue;
  if PageControl <> nil then
    PageControl.Invalidate;
end;

procedure TlqTabSheet.HandlePaint;
begin
  inherited HandlePaint;
  Canvas.Clear(FBackgroundColor);
end;

procedure TlqTabSheet.SetName(const NewName: TComponentName);
var
  old: String;
begin
  old := NewName;
  inherited SetName(NewName);
  if (csDesigning in ComponentState) then
  begin
    if (Text = '') or (Text = old) then
      Text := NewName;
  end;
end;

constructor TlqTabSheet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FText := '';
  FTabVisible:= True;
  FFocusable := True;
  FBackgroundColor := Parent.BackgroundColor;
  FTextColor := Parent.TextColor;
  FIsContainer := True;
end;

constructor TlqTabSheet.CreateWithTitle(AOwner: TComponent; const AText: TlqString);
begin
  Create(AOwner);
  FText := AText;
end;

destructor TlqTabSheet.Destroy;
begin
  if FPageControl <> nil then
    FPageControl.RemovePage(self);
  inherited Destroy;
end;

procedure TlqTabSheet.AfterConstruction;
begin
  if (Owner <> nil) and (Owner is TlqPageControl) then
  begin
    FPageControl:=TlqPageControl(Owner);
    FPageControl.InsertPage(self, True);
  end;
  inherited AfterConstruction;
end;

procedure TlqTabSheet.SetPageControl(APageControl: TlqPageControl);
begin
  FPageControl := APageControl;
  if APageControl <> nil then
    FPageControl.InsertPage(Self);
end;

  
{ TlqPageControl }

function TlqPageControl.GetActivePageIndex: integer;
begin
  Result := FActivePageIndex;
end;

function TlqPageControl.GetPage(AIndex: integer): TlqTabSheet;
begin
  Result := nil;
  if (AIndex >= 0) and (AIndex < FPages.Count) then
    Result := TlqTabSheet(FPages[AIndex]);
end;

function TlqPageControl.GetPageCount: Integer;
begin
  Result := FPages.Count;
end;

procedure TlqPageControl.InsertPage(var APage: TlqTabSheet; SuppressOnChangeEvent: boolean = False);
begin
  if FPages.IndexOf(APage) <> -1 then
    Exit; //==>   The page has already been added.
  FPages.Add(APage);
  PositionTabSheets;
  { TODO: This behaviour could maybe be controlled by a Options property }
  if FPages.Count=1 then
  begin
    if SuppressOnChangeEvent then
      Loading;
    ActivePage := APage;
    if SuppressOnChangeEvent then
      Loaded;
  end;
end;

procedure TlqPageControl.RemovePage(const APage: TlqTabSheet);
var
  i: integer;
begin
  if APage = nil then
    Exit; // ==>
  if FPages.Count =0 then
    Exit; // ==>
  
  if FPages.Count > 1 then              
  begin
     i:=FPages.IndexOf(APage);
     FPages.Remove(APage);
    APage.PageControl:=nil;
    APage.Visible:=false;
    if i = ActivePageIndex then
    begin	    
      if i > FPages.Count-1 then
         ActivePage:=TlqTabSheet(FPages.Last)
      else if i = 0 then
        ActivePage:= TlqTabSheet(FPages.First)
      else
        ActivePage:=TlqTabSheet(FPages[i]);
    end
    else if i < ActivePageIndex then
      ActivePage:=TlqTabSheet(Pages[i-1]);	      
  end      
  else
  begin
    FPages.Remove(APage);
    APage.PageControl := nil;
    APage.Visible := False;
    ActivePage := nil;
  end;
end;

procedure TlqPageControl.SetActivePageIndex(const AValue: integer);
begin
  if FPages.Count = 0 then
    exit;
  if (AValue >= 0) or (AValue < FPages.Count) then
    ActivePage := TlqTabSheet(FPages[AValue]);
end;

procedure TlqPageControl.SetActivePage(const AValue: TlqTabSheet);
begin
  if FActivePage = AValue then
    Exit; //==>
  FActivePage := AValue;
  ActiveWidget := AValue;
  if AValue <> nil then
    FActivePageIndex := FPages.IndexOf(AValue);
  RePaint;
  DoPageChange(FActivePage);
end;

procedure TlqPageControl.PositionTabSheets;
var
  i: integer;
  t: TlqTabSheet;
begin
  for i := 0 to FPages.Count-1 do
  begin
    t := TlqTabSheet(FPages[i]);
    PositionTabSheet(t);
    t.Anchors := [anLeft, anTop, anRight, anBottom];
  end;
end;

procedure TlqPageControl.PositionTabSheet(var APage: TlqTabSheet);
var
  r: TRect;
  w: integer;
  wd: integer;  { width delta }
  h: integer;
  hd: integer;  { height delta }
  msg: TlqMessageParams;
begin
  // PageControl has bevelled edges in some themes
  r := lqStyle.GetControlFrameBorders;

  { Calculate and set Width and Height }
  if TabPosition in [tpTop, tpBottom] then
  begin
    w := Width - (FMargin*2) - r.Left - r.Right;
    wd := APage.Width - w;
    APage.Width   := w;
    h := Height - ButtonHeight - (FMargin*2) - r.Top - r.Bottom;
    hd := APage.Height - h;
    APage.Height  := h;
  end
  else if TabPosition in [tpLeft, tpRight] then
  begin
    w := Width - MaxButtonWidth - (FMargin*2) - r.Left - r.Right;
    wd := APage.Width - w;
    APage.Width   := w;
    h := Height - (FMargin*2) - r.Top - r.Bottom;
    hd := APage.Height - h;
    APage.Height  := h;
  end
  else
  begin   // tpNone
    w := Width - (FMargin*2) - r.Left - r.Right;
    wd := APage.Width - w;
    APage.Width   := w;
    h := Height - (FMargin*2) - r.Top - r.Bottom;
    hd := APage.Height - h;
    APage.Height  := h;
  end;

  { Calculate and set Top and Left }
  if TabPosition = tpTop then
  begin
    APage.Left    := FMargin + r.Left;
    APage.Top     := ButtonHeight + FMargin + r.Top;
  end
  else if TabPosition = tpBottom then
  begin
    APage.Left    := FMargin + r.Left;
    APage.Top     := FMargin + r.Top;
  end
  else if TabPosition = tpLeft then
  begin
    APage.Left    := MaxButtonWidth + FMargin + r.Left;
    APage.Top     := FMargin + r.Top;
  end
  else if TabPosition = tpRight then
  begin
    APage.Left    := FMargin + r.Left;
    APage.Top     := FMargin + r.Top;
  end;

  if TabPosition in [tpNone] then
  begin
    APage.Left    := FMargin + r.Left;
    APage.Top     := FMargin + r.Top;
  end;

  APage.UpdateWindowPosition; { Internal state is now resolved }
end;

function TlqPageControl.MaxButtonWidthSum: integer;
var
  i: integer;
  t: TlqTabSheet;
begin
  {$IFDEF DEBUG}writeln(Classname + '.MaxButtonWidthSum');{$ENDIF}
  Result := 0;
  
  for i := 0 to FPages.Count-1 do
  begin
    t := TlqTabSheet(FPages[i]);
    Result := Result + ButtonWidth(t.Text);
  end;
end;

function TlqPageControl.MaxButtonHeightSum: integer;
begin
  result := PageCount * ButtonHeight;
end;

function TlqPageControl.MaxButtonWidth: integer;
var
  t: TlqTabSheet;
  i: integer;
begin
  Result := 0;
  if FixedTabWidth > 0 then
  begin
    Result := FixedTabWidth;
  end
  else
  begin
    for i := 0 to FPages.Count-1 do
    begin
      t := TlqTabSheet(FPages[i]);
      if ButtonWidth(t.Text) > Result then
        Result := ButtonWidth(t.Text);
    end;
  end;
end;

function TlqPageControl.ButtonHeight: integer;
begin
  if FFixedTabHeight > 0 then
    result := FFixedTabHeight
  else
    result := FFont.Height + 10;   { TODO: correct this }
end;

function TlqPageControl.ButtonWidth(AText: string): integer;
begin
  if FFixedTabWidth > 0 then
    result := FFixedTabWidth
  else
    result := FFont.TextWidth(AText) + 10;
end;

procedure TlqPageControl.SetFixedTabWidth(const AValue: integer);
begin
  if FFixedTabWidth = AValue then
    Exit; //==>
  if AValue > 5 then
  begin
    FFixedTabWidth := AValue;
    RePaint;
  end;
end;

procedure TlqPageControl.SetFixedTabHeight(const AValue: integer);
begin
  if FFixedTabHeight = AValue then
    Exit; //==>
  if AValue > 5 then
  begin
    FFixedTabHeight := AValue;
    RePaint;
  end;
end;

function TlqPageControl.GetTabText(AText: string): string;
var
  s, s1: string;
  i: integer;
begin
  {$IFDEF DEBUG}writeln(Classname + '.GetTabText');{$ENDIF}
  Result  := AText;
  s       := AText;
  s1      := '';
  i       := 1;
  if FFixedTabWidth > 0 then
  begin
    while FFont.TextWidth(s1) < (FFixedTabWidth-10) do
    begin
      if Length(s1) = Length(s) then
        Break;
      s1 := UTF8Copy(s, 1, i);
      inc(i);
    end;
    if FFont.TextWidth(s1) > (FFixedTabWidth-10) then
      UTF8Delete(s1, UTF8Length(s1), 1);
    if Length(s1) > 0 then
      s1 := Trim(s1);
    Result := s1;
  end;
end;

procedure TlqPageControl.LeftButtonClick(Sender: TObject);
begin
  {$IFDEF DEBUG}writeln(Classname + '.LeftButtonClick');{$ENDIF}
  if FFirstTabButton <> nil then
  begin
    if TlqTabSheet(FPages.First) <> FFirstTabButton then
    begin
      FFirstTabButton := TlqTabSheet(FPages[FPages.IndexOf(FFirstTabButton)-1]);
      RePaint;
    end;
  end;
end;

procedure TlqPageControl.RightButtonClick(Sender: TObject);
begin
  {$IFDEF DEBUG}writeln(Classname + '.RightButtonClick');{$ENDIF}
  if FFirstTabButton <> nil then
  begin
    if TlqTabSheet(FPages.Last) <> FFirstTabButton then
    begin
      FFirstTabButton := TlqTabSheet(FPages[FPages.IndexOf(FFirstTabButton)+1]);
      RePaint;
    end;
  end;
end;

function TlqPageControl.FindNextPage(ACurrent: TlqTabSheet; AForward: boolean): TlqTabSheet;
begin
  // To be completed
  result := nil;
end;

procedure TlqPageControl.SetSortPages(const AValue: boolean);
begin
  if FSortPages = AValue then
    Exit; //==>
  FSortPages := AValue;
  RePaint;
end;

procedure TlqPageControl.SetStyle(const AValue: TlqTabStyle);
begin
  if FStyle = AValue then
    Exit; //==>
  FStyle := AValue;
  Invalidate;
end;

procedure TlqPageControl.SetTabPosition(const AValue: TlqTabPosition);
begin
  if FTabPosition = AValue then
    Exit; //==>
  FTabPosition := AValue;
  RePaint;
end;

procedure TlqPageControl.DoPageChange(ATabSheet: TlqTabSheet);
begin
  if (csLoading in ComponentState) then
    Exit;
  if (csDesigning in ComponentState) then
    Exit;
  if Assigned(FOnChange) then
    FOnChange(self, ATabSheet);
end;

procedure TlqPageControl.DoTabSheetClosing(ATabSheet: TlqTabSheet);
begin
  if (csLoading in ComponentState) then
    Exit;
  if (csDesigning in ComponentState) then
    Exit;
  if Assigned(FOnClosingTabSheet) then
    FOnClosingTabSheet(self, ATabSheet);
end;

{ Mode = 1 means the background tabs. Mode = 2 means the Active Tab }
function TlqPageControl.DrawTab(const rect: TlqRect; const Selected: Boolean = False; const Mode: Integer = 1): TlqRect;
var
  r: TlqRect;
begin
  r := rect;
  if Selected then
  begin
    Result := rect;
    InflateRect(Result, 2, 2);
    Exit; //==>
  end;

  if Mode = 2 then
  begin
    r.Height -= 1;
    Canvas.SetColor(ActiveTabColor);
  end
  else
    Canvas.SetColor(BackgroundColor);

  case TabPosition of
    tpTop:
      begin
        Canvas.FillRectangle(r.Left+1, r.Top+1, r.Width-3, r.Height-2);     // fill tab background
        Canvas.SetColor(clHilite2);
        Canvas.DrawLine(r.Left, r.Bottom-2 , r.Left, r.Top+2);        // left edge
        Canvas.DrawLine(r.Left, r.Top+2 , r.Left+2, r.Top);           // left rounder edge
        Canvas.DrawLine(r.Left+2,  r.Top, r.Right-1, r.Top);          // top edge
        Canvas.SetColor(clShadow1);
        Canvas.DrawLine(r.Right-1, r.Top+1, r.Right-1, r.Bottom-1);   // right inner edge
        Canvas.SetColor(clShadow2);
        Canvas.DrawLine(r.Right-1, r.Top+1, r.Right, r.Top+2);        // right rounded edge (1px)
        Canvas.DrawLine(r.Right, r.Top+2, r.Right, r.Bottom-1);       // right outer edge
      end;

    tpBottom:
      begin
        Canvas.FillRectangle(r.Left, r.Top+1, r.Width-2, r.Height-3);   // fill tab background
        Canvas.SetColor(clHilite2);
        Canvas.DrawLine(r.Left, r.Top, r.Left, r.Bottom-1);           // left edge
        Canvas.SetColor(clShadow2);
        Canvas.DrawLine(r.Left+2,  r.Bottom, r.Right-1, r.Bottom);    // bottom outer edge
        Canvas.SetColor(clShadow1);
        Canvas.DrawLine(r.Right-1, r.Bottom-1, r.Right-1, r.Top+1);   // right inner edge
        Canvas.DrawLine(r.Left+1,  r.Bottom-1, r.Right-1, r.Bottom-1);// bottom inner edge
        Canvas.SetColor(clShadow2);
        Canvas.DrawLine(r.Right-1, r.Bottom-1, r.Right, r.Bottom-2);  // right rounded edge (1px)
        Canvas.DrawLine(r.Right, r.Bottom-2, r.Right, r.Top+1);       // right outer edge
      end;

    tpLeft:
      begin
        if Mode = 2 then
        begin
          r.Width  := r.Width - 1;
          r.Height := r.Height + 2;
        end;
        with Canvas do
        begin
          FillRectangle(r.Left+1, r.Top+1, r.Width-2, r.Height-3);
          SetColor(clHilite2);
          DrawLine(r.Left, r.Bottom-2, r.Left, r.Top+2);
          DrawLine(r.Left, r.Top+2, r.Left+2, r.Top);
          DrawLine(r.Left+2, r.Top, r.Right-1, r.Top);
          SetColor(clShadow1);
          DrawLine(r.Left+2, r.Bottom-1, r.Right-1, r.Bottom-1);
          SetColor(clShadow2);
          DrawLine(r.Left+1, r.Bottom-1, r.Left+3, r.Bottom);
          DrawLine(r.Left+2, r.Bottom, r.Right, r.Bottom);
        end;
      end;

    tpRight:
      begin
        if Mode = 2 then
        begin
          r.Height := r.Height + 2;
        end;
        with Canvas do
        begin
          FillRectangle(r.Left+1, r.Top+1, r.Width-2, r.Height-3);
          SetColor(clHilite2);
          DrawLine(r.Left+1, r.Top, r.Right-2, r.Top);
          SetColor(clShadow1);
          DrawLine(r.Right-2,r.Top,r.Right-1,r.Top+1);
          DrawLine(r.Left+2, r.Bottom-1, r.Right-2, r.Bottom-1);
          DrawLine(r.Right-3, r.Bottom-1, r.Right-1, r.Bottom-3);
          DrawLine(r.Right-1, r.Bottom-3, r.Right-1, r.Top);
          SetColor(clShadow2);
          DrawLine(r.Left+2,r.Bottom,r.Right-3, r.Bottom);
          DrawLine(r.Right-3, r.Bottom, r.Right, r.Bottom-3);
          DrawLine(r.Right, r.Top+2, r.Right, r.Bottom-2);
        end;
      end;
  end;  { case }
end;

procedure TlqPageControl.pmCloseTab(Sender: TObject);
var
  ts: TlqTabSheet;
begin
  ts := TabSheetAtPos(FLastRClickPos.x, FLastRClickPos.y);
  if not Assigned(ts) then
    ts := ActivePage;
  if ts = nil then
    exit;
  RemovePage(ts);
  DoTabSheetClosing(ts);
  ts.Free;
end;

function TlqPageControl.GetActiveTabColor: TlqColor;
begin
  Result := FActiveTabColor;
end;

procedure TlqPageControl.SetActiveTabColor(AValue: TlqColor);
begin
  if FActiveTabColor <> AValue then
  begin
    FActiveTabColor := AValue;
    RePaint;
  end;
end;

procedure TlqPageControl.SetBackgroundColor(const AValue: TlqColor);
var
  lWasMatch: boolean;
begin
  lWasMatch := FBackgroundColor = FActiveTabColor;
  inherited SetBackgroundColor(AValue);
  if lWasMatch then
    ActiveTabColor := FBackgroundColor;
end;

procedure TlqPageControl.OrderSheets;
begin
  FPages.Sort(@SortCompare);
  FActivePageIndex := FPages.IndexOf(ActivePage);
end;

procedure TlqPageControl.RePaintTitles;
const
  TabHeight = 21;
var
  TabW, TabH: Integer;
  r2: TlqRect;
  r3: TlqRect;
  h: TlqTabSheet;
  lp: integer;
  toffset: integer;
  TextLeft, TextTop: Integer;
  dx: integer;
  lTxtFlags: TlqTextFlags;
  ActivePageVisible: Boolean;
begin
  if not HasHandle then
    Exit; //==>
    
  if PageCount = 0 then
    Exit; //==>

  TabW:=FixedTabWidth;
  TabH:=FixedTabHeight;
  ActivePageVisible := false;
  If TabH = 0 then
    TabH := TabHeight;
  h := TlqTabSheet(FPages.First);
  if h = nil then
    Exit; //==>
  
  Canvas.SetTextColor(TextColor);
  lTxtFlags := [];
  if not Enabled then
    Include(lTxtFlags, txtDisabled);


  if TabPosition in [tpTop, tpBottom] then
  begin
    if MaxButtonWidthSum > (Width-(FMargin*2)) then
    begin
      if FFirstTabButton = nil then
        FFirstTabButton := h
      else
        h := FFirstTabButton;
      if TabPosition = tpTop then
      begin
        FLeftButton.SetPosition(Width - (FRightButton.Width * 2), FMargin, FRightButton.Height, FRightButton.Height);
        FRightButton.SetPosition(Width - FRightButton.Width, FMargin, FRightButton.Height, FRightButton.Height);
      end
      else
      begin
        FLeftButton.SetPosition(Width - (FRightButton.Width * 2), Height - ButtonHeight - FMargin, FRightButton.Height, FRightButton.Height);
        FRightButton.SetPosition(Width - FRightButton.Width, Height - ButtonHeight - FMargin, FRightButton.Height, FRightButton.Height);
      end;
      FLeftButton.Visible   := True;
      FRightButton.Visible  := True;
    end
    else
    begin
      FLeftButton.Visible   := False;
      FRightButton.Visible  := False;
    end;
  end;

  if TabPosition in [tpLeft, tpRight] then
  begin
    if MaxButtonHeightSum > (Height-(FMargin*2)) then
    begin
      if FFirstTabButton = nil then
        FFirstTabButton := h
      else
        h := FFirstTabButton;
      if TabPosition = tpLeft then
      begin
        FLeftButton.SetPosition(MaxButtonWidth - (FRightButton.Width * 2), Height - ButtonHeight - FMargin, FRightButton.Height, FRightButton.Height);
        FRightButton.SetPosition(MaxButtonWidth - FRightButton.Width, Height - ButtonHeight - FMargin, FRightButton.Height, FRightButton.Height);
      end
      else
      begin
        FLeftButton.SetPosition(Width - MaxButtonWidth, Height - ButtonHeight - FMargin, FRightButton.Height, FRightButton.Height);
        FRightButton.SetPosition(Width - MaxButtonWidth + FRightButton.Width, Height - ButtonHeight - FMargin, FRightButton.Height, FRightButton.Height);
      end;
      FLeftButton.Visible   := True;
      FRightButton.Visible  := True;
    end
    else
    begin
      FLeftButton.Visible   := False;
      FRightButton.Visible  := False;
    end;
  end;

  case TabPosition of
    tpNone:
      begin
        while h <> nil do
        begin
          if h <> ActivePage then
            h.Visible:=false
          else
            h.Visible:=True;
          h.SetPosition(FMargin+2, FMargin+2 , Width - (FMargin*2) - 4, Height - ((FMargin+2)*2));
          if h <> TlqTabSheet(FPages.Last) then
            h := TlqTabSheet(FPages[FPages.IndexOf(h)+1])
          else
            h := nil;
        end;
        r2.Left    := 0;
        r2.Top     := 0;
        r2.Width   := Width;
        r2.Height  := Height;
        Canvas.DrawButtonFace(r2, []);
      end;

    tpBottom:
      begin
        lTxtFlags += TextFlagsDflt;
        lp := 0;
        r2.SetRect(2, Height - ButtonHeight-3, 50, 21);
        while h <> nil do
        begin
          if h <> ActivePage then
          begin
            toffset := 2;
            h.Visible := False;
          end
          else
          begin
            toffset := 4;
            h.Visible := True;
            h.SetPosition(FMargin+2, FMargin+2 , Width - (FMargin*2) - 4, Height - r2.Height - (FMargin+2)*2);
          end;
          // paint tab button
          r2.Width := ButtonWidth(h.Text);
          r3 := DrawTab(r2, h = ActivePage);
          // paint text on non-active tabs
          if h <> ActivePage then
          Canvas.DrawText(lp + (ButtonWidth(h.Text) div 2) - FFont.TextWidth(GetTabText(h.Text)) div 2,
              Height-r2.Height-toffset, GetTabText(h.Text), lTxtFlags);

          r2.Left := r2.Left + r2.Width;
          lp := lp + ButtonWidth(h.Text);
          if h <> TlqTabSheet(FPages.Last) then
            h := TlqTabSheet(FPages[FPages.IndexOf(h)+1])
          else
            h := nil;
        end;
        // Draw Page Control body rectangle (client area)
        r2.Left    := 0;
        r2.Top     := 0;
        r2.Width   := Width;
        r2.Height  := Height - r2.Height;
        Canvas.DrawButtonFace(r2, []);
        // Draw text of ActivePage, because we didn't before.
        DrawTab(r3, false, 2);
        Canvas.DrawText(r3.Left+4, r3.Top+5, r3.Width, r3.Height, ActivePage.Text, lTxtFlags);
      end;

    tpTop:
      begin
        lTxtFlags += TextFlagsDflt;
        lp := 0;
        r2.SetRect(2, 2, 50, 21);
        while h <> nil do
        begin
          if h <> ActivePage then
          begin
            toffset := 4;
            h.Visible := False;
          end
          else
          begin
            toffset := 2;
            h.Visible := True;
            h.SetPosition(FMargin+2, FMargin+2 + r2.Height, Width - (FMargin*2) - 4, Height - r2.Height - ((FMargin+2)*2));
          end;
          // paint tab button
          r2.Width := ButtonWidth(h.Text);
          r3 := DrawTab(r2, h = ActivePage);

          // paint text on non-active tabs
          if h <> ActivePage then
            Canvas.DrawText(lp + (ButtonWidth(h.Text) div 2) - FFont.TextWidth(GetTabText(h.Text)) div 2,
                FMargin+toffset, GetTabText(h.Text), lTxtFlags);
          r2.Left := r2.Left + r2.Width;
          lp := lp + ButtonWidth(h.Text);
          if h <> TlqTabSheet(FPages.Last) then
            h := TlqTabSheet(FPages[FPages.IndexOf(h)+1])
          else
            h := nil;
        end;
        // Draw Page Control body rectangle (client area)
        r2.Left    := 0;
        r2.Top     := r2.Top + r2.Height-2;
        r2.Width   := Width;
        r2.Height  := Height - r2.Height;
        Canvas.DrawButtonFace(r2, []);

        // Draw text of ActivePage, because we didn't before.
        DrawTab(r3, false, 2);
        Canvas.DrawText(r3.Left+4, r3.Top+3, r3.Width, r3.Height, ActivePage.Text, lTxtFlags);
      end;

    tpRight:
      begin
        lTxtFlags += [txtVCenter, txtLeft];
        lp := 0;
        TabW := MaxButtonWidth;
        r2.SetRect(Width - 2 - TabW, 2, TabW, 21);
        while h <> nil do
        begin
          if h <> ActivePage then
          begin
            toffset := 4;
            h.Visible := False;
          end
          else
          begin
            toffset := 2;
            h.Visible := True;
            { set tab content page (client area) size }
            h.SetPosition(FMargin+2, FMargin+2, Width - ((FMargin+2)*2) - TabW, Height - ((FMargin+2)*2));
          end;
          // paint tab button
          r3 := DrawTab(r2, h = ActivePage);

          // paint text on non-active tabs
          if h <> ActivePage then
            Canvas.DrawText(r2.left+toffset, r2.Top, r2.Width, r2.Height, GetTabText(h.Text), lTxtFlags);
          r2.Top += r2.Height;
          lp := r2.Top;
          if h <> TlqTabSheet(FPages.Last) then
            h := TlqTabSheet(FPages[FPages.IndexOf(h)+1])
          else
            h := nil;
        end;
        // Draw Page Control body rectangle (client area)
        r2.Left    := 0;
        r2.Top     := 0;
        r2.Width   := Width - TabW;
        r2.Height  := Height;
        Canvas.DrawButtonFace(r2, []);

        // Draw text of ActivePage, because we didn't before.
        DrawTab(r3, false, 2);
        Canvas.DrawText(r3.left+toffset, r3.Top, r3.Width, r3.Height, ActivePage.Text, lTxtFlags);
      end;

    tpLeft:
      begin
        lTxtFlags += [txtVCenter, txtLeft];
        lp := 0;
        TabW := MaxButtonWidth;
        r2.SetRect(2, 2, TabW, 21);
        while h <> nil do
        begin
          if h <> ActivePage then
          begin
            toffset := 4;
            h.Visible := False;
          end
          else
          begin
            toffset := 2;
            h.Visible := True;
            { set tab content page (client area) size }
            h.SetPosition(FMargin+2+TabW, FMargin+2, Width - ((FMargin+2)*2) - TabW, Height - ((FMargin+2)*2));
          end;
          // paint tab button
          r3 := DrawTab(r2, h = ActivePage);

          // paint text on non-active tabs
          if h <> ActivePage then
            Canvas.DrawText(r2.left+toffset, r2.Top, r2.Width, r2.Height, GetTabText(h.Text), lTxtFlags);
          r2.Top += r2.Height;
          lp := r2.Top;
          if h <> TlqTabSheet(FPages.Last) then
            h := TlqTabSheet(FPages[FPages.IndexOf(h)+1])
          else
            h := nil;
        end;
        // Draw Page Control body rectangle (client area)
        r2.Left    := TabW;
        r2.Top     := 0;
        r2.Width   := Width - TabW;
        r2.Height  := Height;
        Canvas.DrawButtonFace(r2, []);

        // Draw text of ActivePage, because we didn't before.
        DrawTab(r3, false, 2);
        Canvas.DrawText(r3.left+toffset, r3.Top, r3.Width, r3.Height, ActivePage.Text, lTxtFlags);
      end;
  end; { case }

end;

procedure TlqPageControl.HandlePaint;
begin
  if SortPages then
    OrderSheets;
  Canvas.ClearClipRect;
  Canvas.Clear(FBackgroundColor);
  
  // To make it more visible in the UI Designer
  if csDesigning in ComponentState then
  begin
    Canvas.SetColor(clInactiveWgFrame);
    Canvas.DrawRectangle(0, 0, Width, Height);
    if PageCount = 0 then
    begin
      Canvas.SetTextColor(clText1);
      Canvas.DrawString(2, 2, Name + ': ' + Classname);
    end;
  end;
  RePaintTitles;
end;

procedure TlqPageControl.HandleShow;
begin
  inherited HandleShow;
  FLeftButton.Visible := False;
  FRightButton.Visible := False;
end;

procedure TlqPageControl.HandleLMouseUp(x, y: integer; shiftstate: TShiftState);
var
  ts: TlqTabSheet;
begin
//  debugln('>> TlqPageControl.HandleLMouseUp');
  ts := TlqTabSheet(FPages.First);
  if ts = nil then
    exit; //==>  { This means there are no tabs }
  
  ts := TabSheetAtPos(x, y);
  
  if Assigned(ts) then
    ActivePage := ts;

  inherited HandleLMouseUp(x, y, shiftstate);
end;

procedure TlqPageControl.HandleRMouseUp(x, y: integer; shiftstate: TShiftState);
var
  ts: TlqTabSheet;
  s: TlqString;
begin
  inherited HandleRMouseUp(x, y, shiftstate);

  { store the position for later usage }
  FLastRClickPos := lqPoint(x,y);

  if to_PMenuClose in FTabOptions then
  begin
    ts := TabSheetAtPos(x, y);
    {$NOTE TODO: This text needs to become a resource string }
    if Assigned(ts) then
      s := Format('Close "%s" Tab', [ts.Text])
    else
      s := 'Close Tab';
      
    if not Assigned(FPopupMenu) then
    begin
      FPopupMenu := TlqPopupMenu.Create(self);
      FPopupMenu.AddMenuItem(s, '', @pmCloseTab);
    end
    else
    begin
      FPopupMenu.MenuItem(0).Text := s;    { This is dangerous but works for now }
    end;
    FPopupMenu.ShowAt(self, x, y);
  end;
end;

procedure TlqPageControl.HandleKeyPress(var keycode: word;
  var shiftstate: TShiftState; var consumed: boolean);
var
  i: integer;
begin
  i := ActivePageIndex;
  if ssAlt in shiftstate then
  case keycode of
    keyLeft:
        begin
          if ActivePage <> TlqTabSheet(FPages.First) then
          begin
            ActivePage := TlqTabSheet(FPages[i-1]);
            consumed := True;
          end;
        end;

    keyRight:
        begin
          if ActivePage <> TlqTabSheet(FPages.Last) then
          begin
            ActivePage := TlqTabSheet(FPages[i+1]);
            consumed := True;
          end;
        end;
  end;  { case/else }
  if not consumed then
    inherited HandleKeyPress(keycode, shiftstate, consumed);
end;

procedure TlqPageControl.RePaint;
begin
  if FUpdateCount > 0 then
    Exit;
  inherited RePaint;
end;

constructor TlqPageControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFont   := lqStyle.DefaultFont;
  FPages  := TList.Create;
  Width   := 150;
  Height  := 100;
  FIsContainer := True;
  FTabOptions  := [];
  FActivePageIndex := 0;

  FTextColor        := Parent.TextColor;
  FBackgroundColor  := Parent.BackgroundColor;
  FActiveTabColor   := FBackgroundColor;
  FFocusable        := True;
  FOnChange         := nil;
  FFixedTabWidth    := 0;
  FFixedTabHeight   := 21;
  FFirstTabButton   := nil;
  FStyle            := tsTabs;
  FTabPosition      := tpTop;
  FMargin           := 1;
  FSortPages        := False;

  FLeftButton := TlqButton.Create(self);
  FLeftButton.Text      := '<';
  FLeftButton.Height    := 20;
  FLeftButton.Width     := 20;
  FLeftButton.OnClick   := @LeftButtonClick;

  FRightButton := TlqButton.Create(self);
  FRightButton.Text     := '>';
  FRightButton.Height   := 20;
  FRightButton.Width    := 20;
  FRightButton.OnClick  := @RightButtonClick;
end;

destructor TlqPageControl.Destroy;
var i: integer;
begin
  FOnChange := nil;
  for i:=0 to FPages.Count-1 do
    TlqTabSheet(FPages[i]).PageControl:=nil;
  FPages.Free;
  ActiveWidget := nil;
  FFirstTabButton := nil;
  inherited Destroy;
end;

procedure TlqPageControl.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TlqPageControl.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount <= 0 then
    RePaint;
end;

function TlqPageControl.TabSheetAtPos(const x, y: integer): TlqTabSheet;
var
  h: TlqTabSheet;
  lp: integer;  // left position
  bw: integer;  // button width
  bh: integer;  // button height
  p1, p2: integer;    // tab boundaries for mouse click to take affect
begin
  Result := nil;
  h := TlqTabSheet(FPages.First);

  lp := FMargin;
  if MaxButtonWidthSum > (Width-(FMargin*2)) then
    h := FFirstTabButton;

  case TabPosition of
    tpTop:
      begin
        p1 := FMargin;
        p2 := ButtonHeight;
      end;

    tpBottom:
      begin
        p1 := Height - FMargin - ButtonHeight;
        p2 := Height - FMargin;
      end;

    tpRight:
      begin
        p1 := Width - MaxButtonWidth;
        p2 := Width;
      end;

    tpLeft:
      begin
        p1 := FMargin;
        p2 := FMargin + MaxButtonWidth;
      end;
  end;

  if TabPosition in [tpTop, tpBottom] then
  begin
    if (y > p1) and (y < p2) then
    begin
       while h <> nil do
       begin
          bw := ButtonWidth(h.Text);  // initialize button width
          if (x > lp) and (x < lp + bw) then
          begin
            if h <> ActivePage then
              Result := h;
            exit;
          end;  { if }
          lp := lp + bw;
          if h <> TlqTabSheet(FPages.Last) then
            h := TlqTabSheet(FPages[FPages.IndexOf(h)+1])
          else
            h := nil;
       end;  { while }
    end;  { if }
  end;

  if TabPosition in [tpLeft, tpRight] then
  begin
    if (x > p1) and (x < p2) then
    begin
      while h <> nil do
      begin
        bh := ButtonHeight;  // initialize button height
        if (y > lp) and (y < lp + bh) then
        begin
          if h <> ActivePage then
            Result := h;
          exit;
        end;  { if }
        lp := lp + bh;
        if h <> TlqTabSheet(FPages.Last) then
          h := TlqTabSheet(FPages[FPages.IndexOf(h)+1])
        else
          h := nil;
      end;  { while }
    end;  { if }
  end;
end;

function TlqPageControl.AppendTabSheet(ATitle: string): TlqTabSheet;
begin
  Result := TlqTabSheet.Create(self);
  Result.Text := ATitle;
  InsertPage(Result);
end;

procedure TlqPageControl.RemoveTabSheet(ATabSheet: TlqTabSheet);
begin
  RemovePage(ATabSheet);
end;

end.

