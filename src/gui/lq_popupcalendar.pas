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
      A calendar component. Soon it would be possible to use it in a
      popup windows like Calender Combobox, or directly in a Form.
}

unit lq_popupcalendar;

{$mode objfpc}{$H+}

{.$Define DEBUG} // while developing the component

{
    *****************************************************************
    **********   This is still under heavy development!   ***********
    *****************************************************************
}


{ todo: Support highlighting special days. }
{ todo: Support custom colors. }
{ todo: Create a TlqDateTimeEdit component with options for Date, Time or Date & Time. }
{ todo: Changing months and checking min/max limits takes into account the
        original date, not the selected day in the grid. It should use the
        selected day in grid. }
{ todo: Paint days out of min/max range in grey. }

interface

uses
  SysUtils,
  Classes,
  lq_base,
  lq_main,
  lq_widget,
  lq_popupwindow,
  lq_edit,
  lq_button,
  lq_combobox,
  lq_basegrid,
  lq_grid,
  lq_dialogs,
  lq_menu,
  lq_hyperlink,
  lq_panel;

type

  TlqOnDateSetEvent = procedure(Sender: TObject; const ADate: TDateTime) of object;
  TlqOnCheckboxChangedEvent = procedure(Sender: TObject; const AIsChecked: Boolean) of object;

  TYearSelectForm = class(TlqPopupWindow)
  private
    {@VFD_HEAD_BEGIN: YearSelectForm}
    btnMinus10: TlqButton;
    btnPlus10: TlqButton;
    Bevel1: TlqBevel;
    Label1: TlqHyperlink;
    Label2: TlqHyperlink;
    Label3: TlqHyperlink;
    Label4: TlqHyperlink;
    Label5: TlqHyperlink;
    Label6: TlqHyperlink;
    Label7: TlqHyperlink;
    Label8: TlqHyperlink;
    Label9: TlqHyperlink;
    Label10: TlqHyperlink;
    {@VFD_HEAD_END: YearSelectForm}
    FYear: Word;
    FOriginalYear: Word;
    FMinYear: Word;
    FMaxYear: Word;
    procedure   YearClicked(Sender: TObject);
    procedure   SetYear(const AValue: Word);
    procedure   Minus10Clicked(Sender: TObject);
    procedure   Plus10Clicked(Sender: TObject);
  protected
    procedure   HandlePaint; override;
  public
    constructor CreateCustom(AOwner: TComponent; const MinYear, MaxYear: Word);
    procedure   AfterConstruction; override;
    procedure   AfterCreate;
    property    Year: Word read FYear write SetYear;
    property    MinYear: Word read FMinYear;
    property    MaxYear: Word read FMaxYear;
  end;


  TlqPopupCalendar = class(TlqPopupWindow)
  private
    {@VFD_HEAD_BEGIN: lqPopupCalendar}
    edtYear: TlqEdit;
    btnYearUp: TlqButton;
    btnYearDown: TlqButton;
    edtMonth: TlqEdit;
    btnMonthUp: TlqButton;
    btnMonthDown: TlqButton;
    btnToday: TlqButton;
    grdName1: TlqStringGrid;
    {@VFD_HEAD_END: lqPopupCalendar}
    FMonthOffset: integer;
    FDate: TDateTime;
    FMaxDate: TDateTime;
    FMinDate: TDateTime;
    FWeekStartDay: integer;
    FCallerWidget: TlqWidget;
    FOnValueSet: TlqOnDateSetEvent;
    FCloseOnSelect: boolean;
    FThisMonthDays: array[0..6,0..5] of boolean;
    FWeeklyHoliday: integer;
    FDayColor: TlqColor;
    FHolidayColor: TlqColor;
    FSelectedColor: TlqColor;
    FSingleClickSelect: boolean;
    FMonthsPopupMenu: TlqPopupMenu;
    FYearPopupWindow: TYearSelectForm;
    procedure   YearPopupWindowClose(Sender: TObject);
    function    GetDateElement(Index: integer): Word;
    procedure   PopulateDays;
    procedure   CalculateMonthOffset;
    function    CalculateCellDay(const ACol, ARow: Integer): Integer;
    procedure   SetDateElement(AIndex: integer; const AValue: Word);
    procedure   SetDateValue(const AValue: TDateTime);
    procedure   SetMaxDate(const AValue: TDateTime);
    procedure   SetMinDate(const AValue: TDateTime);
    procedure   SetWeekStartDay(const AValue: integer);
    procedure   SetWeeklyHoliday(const AValue: integer);
    procedure   SetDayColor(const AValue: TlqColor);
    procedure   SetHolidayColor(const AValue: TlqColor);
    procedure   SetSelectedColor(const AValue: TlqColor);
    procedure   SetCloseOnSelect(const AValue: boolean);
    procedure   UpdateCalendar;
    procedure   btnYearUpClicked(Sender: TObject);
    procedure   btnYearDownClicked(Sender: TObject);
    procedure   btnMonthUpClicked(Sender: TObject);
    procedure   btnMonthDownClicked(Sender: TObject);
    procedure   btnTodayClicked(Sender: TObject);
    procedure   grdName1Clicked(Sender: TObject);
    procedure   grdName1DoubleClick(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
    procedure   grdName1KeyPress(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState; var Consumed: boolean);
    procedure   grdName1DrawCell(Sender: TObject; const ARow, ACol: Integer; const ARect: TlqRect; const AFlags: TlqGridDrawState; var ADefaultDrawing: boolean);
    procedure   edtMonthClicked(Sender: TObject);
    procedure   edtYearClicked(Sender: TObject);
    procedure   miMonthClicked(Sender: TObject);
    procedure   TearDown;
    procedure   SetSingleClickSelect(const AValue: boolean);
    procedure   ClosePopupMenusWindows;
  protected
    FntNorm, FntBold: TlqFont;
    FOrigFocusWin: TlqWidget;
    procedure   HandlePaint; override;
    procedure   HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); override;
    procedure   HandleShow; override;
    procedure   HandleHide; override;
    procedure   ShowDefaultPopupMenu; virtual;
    property    CallerWidget: TlqWidget read FCallerWidget write FCallerWidget;
  public
    constructor Create(AOwner: TComponent; AOrigFocusWin: TlqWidget); reintroduce;
    destructor  Destroy; override;
    procedure   AfterCreate;
    property    CloseOnSelect: boolean read FCloseOnSelect write SetCloseOnSelect default True;
    property    SingleClickSelect: boolean read FSingleClickSelect write SetSingleClickSelect default False;
    property    Day: Word index 1 read GetDateElement write SetDateElement;
    property    Month: Word index 2 read GetDateElement write SetDateElement;
    property    Year: Word index 3 read GetDateElement write SetDateElement;
    property    OnValueSet: TlqOnDateSetEvent read FOnValueSet write FOnValueSet;
  published
    property    DateValue: TDateTime read FDate write SetDateValue;
    property    MinDate: TDateTime read FMinDate write SetMinDate;
    property    MaxDate: TDateTime read FMaxDate write SetMaxDate;
    property    WeekStartDay: integer read FWeekStartDay write SetWeekStartDay default 0;
    property    WeeklyHoliday: integer read FWeeklyHoliday write SetWeeklyHoliday default -1;
    property    DayColor: TlqColor read FDayColor write SetDayColor;
    property    HolidayColor: TlqColor read FHolidayColor write SetHolidayColor;
    property    SelectedColor: TlqColor read FSelectedColor write SetSelectedColor;
  end;
  
  
  TlqCalendarCombo = class(TlqBaseStaticCombo)
  private
    FDate: TDateTime;
    FDateFormat: string;
    FMaxDate: TDateTime;
    FMinDate: TDateTime;
    FWeekStartDay: integer;
    FWeeklyHoliday: integer;
    FDayColor: TlqColor;
    FHolidayColor: TlqColor;
    FSelectedColor: TlqColor;
    FCloseOnSelect: boolean;
    FSingleClickSelect: boolean;
    procedure   SetDateFormat(const AValue: string);
    procedure   SetDateValue(const AValue: TDateTime);
    procedure   SetMaxDate(const AValue: TDateTime);
    procedure   SetMinDate(const AValue: TDateTime);
    procedure   SetWeekStartDay(const AValue: integer);
    procedure   SetWeeklyHoliday(const AValue: integer);
    procedure   SetDayColor(const AValue: TlqColor);
    procedure   SetHolidayColor(const AValue: TlqColor);
    procedure   SetSelectedColor(const AValue: TlqColor);
    procedure   SetCloseOnSelect(const AValue: boolean);
    procedure   SetSingleClickSelect(const AValue: boolean);
  protected
    function    GetText: string; override;
    procedure   SetText(const AValue: string); override;
    procedure   InternalOnValueSet(Sender: TObject; const ADate: TDateTime); virtual;
    function    HasText: boolean; override;
    procedure   DoDropDown; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property    Align;
    property    BackgroundColor;
    { Clicking on calendar Today button will close the popup calendar by default }
    property    CloseOnSelect: boolean read FCloseOnSelect write SetCloseOnSelect default True;
    property    DateFormat: string read FDateFormat write SetDateFormat;
    property    DateValue: TDateTime read FDate write SetDateValue;
    property    DayColor: TlqColor read FDayColor write SetDayColor;
    property    Enabled;
    property    FontDesc;
    property    Hint;
    property    HolidayColor: TlqColor read FHolidayColor write SetHolidayColor;
    property    MaxDate: TDateTime read FMaxDate write SetMaxDate;
    property    MinDate: TDateTime read FMinDate write SetMinDate;
    property    ParentShowHint;
    property    ReadOnly;
    property    SelectedColor: TlqColor read FSelectedColor write SetSelectedColor;
    property    SingleClickSelect: boolean read FSingleClickSelect write SetSingleClickSelect default False;
    property    ShowHint;
    property    WeeklyHoliday: integer read FWeeklyHoliday write SetWeeklyHoliday default -1;
    property    WeekStartDay: integer read FWeekStartDay write SetWeekStartDay default 0;
    property    TabOrder;
    property    OnChange;
    property    OnCloseUp;
    property    OnDropDown;
    property    OnEnter;
    property    OnExit;
    property    OnShowHint;
  end;


  TlqCalendarCheckCombo = class(TlqCalendarCombo)
  private
    FChecked: boolean;
    FCheckBoxRect: TlqRect;
    FCheckboxChanged: TlqOnCheckboxChangedEvent;
    procedure   SetChecked(const AValue: Boolean);
    procedure   DoCheckboxChanged;
  protected
    procedure   DoDrawText(const ARect: TlqRect); override;
    procedure   InternalOnValueSet(Sender: TObject; const ADate: TDateTime); override;
    procedure   HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); override;
    procedure   HandlePaint; override;
    procedure   HandleResize(AWidth, AHeight: TlqCoord); override;
    procedure   HandleLMouseDown(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleLMouseUp(x, y: integer; shiftstate: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property    Checked: Boolean read FChecked write SetChecked;
    property    OnCheckboxChanged: TlqOnCheckboxChangedEvent read FCheckboxChanged write FCheckboxChanged;
    property    OnKeyPress;
  end;


{@VFD_NEWFORM_DECL}


implementation

uses
  dateutils,
  lq_scrollbar,
  lq_constants;

type
  // friend class to get access to Protected methods
  TPopupMenuFriend = class(TlqPopupMenu);


{@VFD_NEWFORM_IMPL}

procedure TYearSelectForm.YearClicked(Sender: TObject);
begin
  FYear := StrToInt(TlqHyperlink(Sender).Text);
  Close;
end;

procedure TYearSelectForm.SetYear(const AValue: Word);

  function IsInRange(const AYear: word): boolean;
  begin
    // always one year less on either side (min and max) so we don't go over
    // any possible month limits.
    Result := (AYear >= MinYear) and (AYear <= MaxYear);
  end;

begin
  if FYear = AValue then exit;
  FYear := AValue;
  if FOriginalYear = 0 then
    FOriginalYear := FYear;
  Label1.Text := IntToStr(FYear-4);
  Label1.Enabled := IsInRange(FYear-4);
  Label2.Text := IntToStr(FYear-3);
  Label2.Enabled := IsInRange(FYear-3);
  Label3.Text := IntToStr(FYear-2);
  Label3.Enabled := IsInRange(FYear-2);
  Label4.Text := IntToStr(FYear-1);
  Label4.Enabled := IsInRange(FYear-1);
  Label5.Text := IntToStr(FYear);
  if FYear = FOriginalYear then
    Label5.FontDesc := '#Label2'
  else
    Label5.FontDesc := '#Label1';
  Label5.Enabled := IsInRange(FYear);
  Label6.Text := IntToStr(FYear+1);
  Label6.Enabled := IsInRange(FYear+1);
  Label7.Text := IntToStr(FYear+2);
  Label7.Enabled := IsInRange(FYear+2);
  Label8.Text := IntToStr(FYear+3);
  Label8.Enabled := IsInRange(FYear+3);
  Label9.Text := IntToStr(FYear+4);
  Label9.Enabled := IsInRange(FYear+4);
  Label10.Text := IntToStr(FYear+5);
  Label10.Enabled := IsInRange(FYear+5);
end;

procedure TYearSelectForm.Minus10Clicked(Sender: TObject);
begin
  SetYear(FYear-10);
end;

procedure TYearSelectForm.Plus10Clicked(Sender: TObject);
begin
  SetYear(FYear+10);
end;

procedure TYearSelectForm.HandlePaint;
begin
//  inherited HandlePaint;
  Canvas.BeginDraw;
  Canvas.Clear(BackgroundColor);
//  Canvas.SetColor(clWindowBackground);
//  Canvas.DrawRectangle(0, 0, Width, Height);  // black rectangle border
  Canvas.DrawButtonFace(0, 0, Width, Height, []);  // 3d rectangle inside black border
  Canvas.EndDraw;
end;

constructor TYearSelectForm.CreateCustom(AOwner: TComponent; const MinYear, MaxYear: Word);
begin
  Create(AOwner);
  FYear := 0;
  FOriginalYear := 0;
  FMinYear := MinYear;
  FMaxYear := MaxYear;
end;

procedure TYearSelectForm.AfterConstruction;
begin
  inherited AfterConstruction;
  AfterCreate;
end;

procedure TYearSelectForm.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: YearSelectForm}
  Name := 'YearSelectForm';
  SetPosition(439, 401, 130, 122);
//  WindowTitle := 'YearSelectForm';
//  Hint := '';
//  Sizeable := False;

  btnMinus10 := TlqButton.Create(self);
  with btnMinus10 do
  begin
    Name := 'btnMinus10';
    SetPosition(4, 4, 24, 24);
    Text := '';
    FontDesc := '#Label1';
    Hint := '';
    ImageMargin := -1;
    ImageName := 'sys.sb.left';
    ImageSpacing := 0;
    TabOrder := 1;
    OnClick := @Minus10Clicked;
  end;

  btnPlus10 := TlqButton.Create(self);
  with btnPlus10 do
  begin
    Name := 'btnPlus10';
    SetPosition(104, 4, 24, 24);
    Text := '';
    FontDesc := '#Label1';
    Hint := '';
    ImageMargin := -1;
    ImageName := 'sys.sb.right';
    ImageSpacing := 0;
    TabOrder := 2;
    OnClick := @Plus10Clicked;
  end;

  Bevel1 := TlqBevel.Create(self);
  with Bevel1 do
  begin
    Name := 'Bevel1';
    SetPosition(64, 32, 2, 85);
    Hint := '';
    Style := bsLowered;
  end;

  Label1 := TlqHyperlink.Create(self);
  with Label1 do
  begin
    Name := 'Label1';
    SetPosition(8, 32, 44, 16);
    Alignment := taCenter;
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Label';
    TextColor := clText1;
    HotTrackFont := '#Label1';
    OnClick := @YearClicked;
  end;

  Label2 := TlqHyperlink.Create(self);
  with Label2 do
  begin
    Name := 'Label2';
    SetPosition(8, 48, 44, 16);
    Alignment := taCenter;
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Label';
    TextColor := clText1;
    HotTrackFont := '#Label1';
    OnClick := @YearClicked;
  end;

  Label3 := TlqHyperlink.Create(self);
  with Label3 do
  begin
    Name := 'Label3';
    SetPosition(8, 64, 44, 16);
    Alignment := taCenter;
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Label';
    TextColor := clText1;
    HotTrackFont := '#Label1';
    OnClick := @YearClicked;
  end;

  Label4 := TlqHyperlink.Create(self);
  with Label4 do
  begin
    Name := 'Label4';
    SetPosition(8, 80, 44, 16);
    Alignment := taCenter;
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Label';
    TextColor := clText1;
    HotTrackFont := '#Label1';
    OnClick := @YearClicked;
  end;

  Label5 := TlqHyperlink.Create(self);
  with Label5 do
  begin
    Name := 'Label5';
    SetPosition(8, 96, 44, 16);
    Alignment := taCenter;
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Label';
    TextColor := clText1;
    HotTrackFont := '#Label1';
    OnClick := @YearClicked;
  end;

  Label6 := TlqHyperlink.Create(self);
  with Label6 do
  begin
    Name := 'Label6';
    SetPosition(76, 32, 44, 16);
    Alignment := taCenter;
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Label';
    TextColor := clText1;
    HotTrackFont := '#Label1';
    OnClick := @YearClicked;
  end;

  Label7 := TlqHyperlink.Create(self);
  with Label7 do
  begin
    Name := 'Label7';
    SetPosition(76, 48, 44, 16);
    Alignment := taCenter;
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Label';
    TextColor := clText1;
    HotTrackFont := '#Label1';
    OnClick := @YearClicked;
  end;

  Label8 := TlqHyperlink.Create(self);
  with Label8 do
  begin
    Name := 'Label8';
    SetPosition(76, 64, 44, 16);
    Alignment := taCenter;
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Label';
    TextColor := clText1;
    HotTrackFont := '#Label1';
    OnClick := @YearClicked;
  end;

  Label9 := TlqHyperlink.Create(self);
  with Label9 do
  begin
    Name := 'Label9';
    SetPosition(76, 80, 44, 16);
    Alignment := taCenter;
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Label';
    TextColor := clText1;
    HotTrackFont := '#Label1';
    OnClick := @YearClicked;
  end;

  Label10 := TlqHyperlink.Create(self);
  with Label10 do
  begin
    Name := 'Label10';
    SetPosition(76, 96, 44, 16);
    Alignment := taCenter;
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Label';
    TextColor := clText1;
    HotTrackFont := '#Label1';
    OnClick := @YearClicked;
  end;

  {@VFD_BODY_END: YearSelectForm}
  {%endregion}
end;


procedure TlqPopupCalendar.PopulateDays;
var
  r, c: integer;
  lCellDay: Integer;
begin
  grdName1.BeginUpdate;
  for r := -1 to 5 do
    for c := 0 to 6 do
    begin
      if r = -1 then
        grdName1.ColumnTitle[c] := ShortDayNames[Succ((c+FWeekStartDay) mod 7)]  // ShortDayNames is 1-based indexing
      else
      begin
        lCellDay := CalculateCellDay(c, r);
        grdName1.Cells[c, r] := IntToStr(lCellDay);
      end;
    end;
  grdName1.EndUpdate;
end;

procedure TlqPopupCalendar.grdName1DoubleClick(Sender: TObject;
  AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
begin
  ClosePopupMenusWindows;
  TearDown;
end;

procedure TlqPopupCalendar.grdName1KeyPress(Sender: TObject;
  var KeyCode: word; var ShiftState: TShiftState; var Consumed: boolean);
begin
  // Pass the grid event on to the TlqPopupCalender instance.
  HandleKeyPress(KeyCode, ShiftState, consumed);
  Consumed := True;
end;

procedure TlqPopupCalendar.grdName1DrawCell(Sender: TObject; const ARow, ACol: Integer; const ARect: TlqRect;
          const AFlags: TlqGridDrawState; var ADefaultDrawing: boolean);
begin
  with grdName1 do
  begin
    if FThisMonthDays[ACol,ARow] then
      if (ACol = FocusCol) and (ARow = FocusRow) then
        Canvas.SetTextColor(FSelectedColor)
      else
        Canvas.SetTextColor(FDayColor)
    else
      if (ACol = FocusCol) and (ARow = FocusRow) then
        Canvas.SetTextColor(FSelectedColor)
      else
        Canvas.SetTextColor(clShadow1);
    if FWeeklyHoliday >= FWeekStartDay then
      if ACol = FWeeklyHoliday - FWeekStartDay then
      begin
        Canvas.Font := FntBold;
        Canvas.SetTextColor(FHolidayColor);
      end
      else
        Canvas.Font := FntNorm
    else
      if (FWeeklyHoliday > -1) and (ACol = FWeeklyHoliday - FWeekStartDay + 7) then
      begin
        Canvas.Font := FntBold;
        Canvas.SetTextColor(FHolidayColor);
      end
      else
        Canvas.Font := FntNorm;
  end;
end;

procedure TlqPopupCalendar.edtMonthClicked(Sender: TObject);
begin
  ClosePopupMenusWindows;
  ShowDefaultPopupMenu;
end;

procedure TlqPopupCalendar.edtYearClicked(Sender: TObject);
begin
  ClosePopupMenusWindows;
  if not Assigned(FYearPopupWindow) then
  begin
    FYearPopupWindow := TYearSelectForm.CreateCustom(nil, YearOf(MinDate), YearOf(MaxDate));
    FYearPopupWindow.OnClose  := @YearPopupWindowClose;
    FYearPopupWindow.Year := Year;
  end;

  FYearPopupWindow.ShowAt(self, edtYear.Left, edtYear.Bottom);
end;

procedure TlqPopupCalendar.miMonthClicked(Sender: TObject);
var
  itm: TlqMenuItem;
begin
  itm := Sender as TlqMenuItem;
  SetDateElement(2 {month index}, itm.Tag);
end;

procedure TlqPopupCalendar.TearDown;
var
  lD: Word;
  s: string;
  d: TDateTime;
begin
  s := grdName1.Cells[grdName1.FocusCol, grdName1.FocusRow];
  if s = '' then
    Exit; //==>
  lD := StrToInt(s);
  if (grdName1.FocusRow = 0) and (lD > 7) then
    Exit; // clicked in previous month
  if (grdName1.FocusRow >= 4) and (lD < 15) then
    Exit; // clicked in next month
  d := EncodeDate(Year, Month, lD);
  if (d >= FMinDate) and (d <= FMaxDate) then
  begin
    DateValue := d;
    if Assigned(OnValueSet) then
      OnValueSet(self, DateValue);
    {$IFDEF DEBUG}
    writeln('Selected date: ', FormatDateTime('yyyy-mm-dd', DateValue));
    {$ENDIF}
    if CloseOnSelect then
      Close;
  end;
end;

procedure TlqPopupCalendar.SetSingleClickSelect(const AValue: boolean);
begin
  if FSingleClickSelect = AValue then exit;
  FSingleClickSelect := AValue;
end;

procedure TlqPopupCalendar.ClosePopupMenusWindows;
begin
  if Assigned(FMonthsPopupMenu) then
  begin
    FMonthsPopupMenu.Close;
    FreeAndNil(FMonthsPopupMenu);
  end;

  if Assigned(FYearPopupWindow) then
  begin
    FYearPopupWindow.Close;
    FreeAndNil(FYearPopupWindow);
  end;
end;

procedure TlqPopupCalendar.YearPopupWindowClose(Sender: TObject);
begin
  Year := FYearPopupWindow.Year;
end;

function TlqPopupCalendar.GetDateElement(Index: integer): Word;
var
  lD, lM, lY: Word;
begin
  DecodeDate(FDate, lY, lM, lD);
  case Index of
    1: Result := lD;
    2: Result := lM;
    3: Result := lY;
  end;
end;

procedure TlqPopupCalendar.CalculateMonthOffset;
var
  lD, lM, lY: Word;
  lTheFirst: TDateTime;
begin
  DecodeDate(FDate, lY, lM, lD);
  lTheFirst := EncodeDate(lY, lM, 1);
  FMonthOffset := 2 - DayOfWeek(lTheFirst);
end;

function TlqPopupCalendar.CalculateCellDay(const ACol, ARow: Integer): Integer;
begin
  if (FMonthOffset + FWeekStartDay) > 1 then
    Result :=  FMonthOffset - 7 + FWeekStartDay + ACol + ARow * 7
  else
    Result :=  FMonthOffset + FWeekStartDay + ACol + ARow * 7;
  if Result < 1 then
  begin
    if Month > 1 then
      Result := MonthDays[IsLeapYear(Year), Pred(Month)] + Result
    else
      Result := 31 + Result;
    FThisMonthDays[ACol,ARow] := False;
  end
  else
    if Result > MonthDays[IsLeapYear(Year), Month] then
    begin
      Result := Result - MonthDays[IsLeapYear(Year), Month];
      FThisMonthDays[ACol,ARow] := False;
    end
    else
      FThisMonthDays[ACol,ARow] := True;
end;

procedure TlqPopupCalendar.SetDateElement(AIndex: integer; const AValue: Word);
var
  lD, lM, lY: Word;
  lDate: TDateTime;
  d: Word;
begin
  if AValue > 0 then
  begin
    DecodeDate(FDate, lY, lM, lD);
    case AIndex of
      1:  lD := AValue;
      2:  begin
            lM := AValue;
            d := DaysInAMonth(lY, lM);
            if lD > d then // If original day value is larger than days in new month
              lD := d;
          end;
      3:  lY := AValue;
    end;
    try
      lDate := EncodeDate(lY, lM, lD);
      SetDateValue(lDate);
    except
      // do nothing!  Not nice?
    end;
  end;
end;

{ If AValue is out of range (min or max), then set it to the limit value }
procedure TlqPopupCalendar.SetDateValue(const AValue: TDateTime);
var
  lDate: TDateTime;
begin
  if FDate = AValue then
    Exit; //==>
  lDate := AValue;

  if (trunc(AValue) >= trunc(FMinDate)) then
    // do nothing - test passed
  else
    lDate := FMinDate;
    
  if (trunc(AValue) <= trunc(FMaxDate)) then
    // do nothing - test passed
  else
    lDate := FMaxDate;
    
  FDate := lDate;
  UpdateCalendar;
end;

procedure TlqPopupCalendar.SetMaxDate(const AValue: TDateTime);
begin
  if FMaxDate = AValue then
    Exit; //==>
  FMaxDate := AValue;

  // correct min/max values
  if FMinDate > AValue then
    FMinDate := IncMonth(AValue, -12); // one year less

  if FDate > FMaxDate then
  begin
    FDate := FMaxDate;
    UpdateCalendar;
  end;
end;

procedure TlqPopupCalendar.SetMinDate(const AValue: TDateTime);
begin
  if FMinDate = AValue then
    Exit; //==>
  FMinDate := AValue;

  // correct min/max values
  if AValue > FMaxDate then
    FMaxDate := IncMonth(AValue, 12); // one year more

  if FDate < FMinDate then
  begin
    FDate := FMinDate;
    UpdateCalendar;
  end;
end;

procedure TlqPopupCalendar.SetWeekStartDay(const AValue: integer);
begin
  if FWeekStartDay <> AValue then
    FWeekStartDay := AValue;
end;

procedure TlqPopupCalendar.SetWeeklyHoliday(const AValue: integer);
begin
  if FWeeklyHoliday <> AValue then
    FWeeklyHoliday := AValue;
end;

procedure TlqPopupCalendar.SetDayColor(const AValue: TlqColor);
begin
  if FDayColor <> AValue then
    FDayColor := AValue;
end;

procedure TlqPopupCalendar.SetHolidayColor(const AValue: TlqColor);
begin
  if FHolidayColor <> AValue then
    FHolidayColor := AValue;
end;

procedure TlqPopupCalendar.SetSelectedColor(const AValue: TlqColor);
begin
  if FSelectedColor <> AValue then
    FSelectedColor := AValue;
end;

procedure TlqPopupCalendar.SetCloseOnSelect(const AValue: boolean);
begin
  if FCloseOnSelect = AValue then
    Exit;
  FCloseOnSelect := AValue;
end;

procedure TlqPopupCalendar.UpdateCalendar;
var
  lD, lM, lY: Word;
begin
  try
    grdName1.BeginUpdate;
    if (FDate >= FMinDate) and (FDate <= FMaxDate) then
    begin
      CalculateMonthOffset;
      PopulateDays;
      edtYear.Text := IntToStr(Year);
      edtMonth.Text := LongMonthNames[Month];
      DecodeDate(FDate, lY, lM, lD);

      grdName1.FocusCol := (lD - FMonthOffset - FWeekStartDay) mod 7;
      grdName1.FocusRow := (lD - FMonthOffset - FWeekStartDay) div 7;
      if (FMonthOffset + FWeekStartDay) > 1 then
        grdName1.FocusRow := grdName1.FocusRow + 1;
//      grdName1.Invalidate;
    end;
  finally
    grdName1.EndUpdate;
  end;
end;

procedure TlqPopupCalendar.btnYearUpClicked(Sender: TObject);
var
  d: TDateTime;
begin
  ClosePopupMenusWindows;
  d := IncMonth(FDate, 12);
  if d <= FMaxDate then
    DateValue := d;
end;

procedure TlqPopupCalendar.btnYearDownClicked(Sender: TObject);
var
  d: TDateTime;
begin
  ClosePopupMenusWindows;
  d := IncMonth(FDate, -12);
  if d >= FMinDate then
    DateValue := d;
end;

procedure TlqPopupCalendar.btnMonthUpClicked(Sender: TObject);
var
  d: TDateTime;
begin
  ClosePopupMenusWindows;
  d := IncMonth(FDate);
  if d <= FMaxDate then
    DateValue := d;
end;

procedure TlqPopupCalendar.btnMonthDownClicked(Sender: TObject);
var
  d: TDateTime;
begin
  ClosePopupMenusWindows;
  d := IncMonth(FDate, -1);
  if d >= FMinDate then
    DateValue := d;
end;

procedure TlqPopupCalendar.btnTodayClicked(Sender: TObject);
begin
  ClosePopupMenusWindows;
  if Now >= FMinDate then
  begin
    DateValue := Now;
    TearDown;
  end;
end;

procedure TlqPopupCalendar.grdName1Clicked(Sender: TObject);
begin
  ClosePopupMenusWindows;
  if FSingleClickSelect then
    TearDown;
end;

procedure TlqPopupCalendar.HandlePaint;
begin
  Canvas.BeginDraw;
  inherited HandlePaint;
  if PopupFrame then
    Canvas.SetClipRect(lqRect(1, 1, Width-2, Height-2));
  Canvas.Clear(clWindowBackground);
  Canvas.ClearClipRect;
  Canvas.EndDraw;
end;

procedure TlqPopupCalendar.HandleKeyPress(var keycode: word;
  var shiftstate: TShiftState; var consumed: boolean);
begin
  case keycode of
    keyUp:
        begin
          if (ssCtrl in shiftstate) then
          begin
            btnYearUpClicked(nil);    // Ctrl+Up Arrow
            consumed := True;
          end;
        end;
    keyDown:
        begin
          if (ssCtrl in shiftstate) then
          begin
            btnYearDownClicked(nil);  // Ctrl+Down Arrow
            consumed := True;
          end;
        end;
    keyLeft:
        begin
          if (ssCtrl in shiftstate) then
          begin
            btnMonthDownClicked(nil); // Ctrl+Left Arrow
            consumed := True;
          end;
        end;
    keyRight:
        begin
          if (ssCtrl in shiftstate) then
          begin
            btnMonthUpClicked(nil);   // Ctrl+Right Arrow
            consumed := True;
          end;
        end;
    keyPageUp:
        begin
          if (ssCtrl in shiftstate) then
            btnYearUpClicked(nil)   // Ctrl+PageUp
          else
            btnMonthUpClicked(nil); // PageUp
          consumed := True;
        end;
    keyPageDown:
        begin
          if (ssCtrl in shiftstate) then
            btnYearDownClicked(nil)     // Ctrl+PageDown
          else
            btnMonthDownClicked(nil);   // PageDown
          consumed := True;
        end;
  end;
  
  if not consumed then
  begin
    if keycode = keyEnter then
    begin
      consumed := True;
      TearDown;
    end
    else if keycode = keyEscape then
    begin
      consumed := True;
      Close;
    end;
  end;
  
  if not consumed then
    inherited HandleKeyPress(keycode, shiftstate, consumed);
end;

procedure TlqPopupCalendar.HandleShow;
begin
  inherited HandleShow;
  grdName1.SetFocus;
  {$IFDEF DEBUG}
  writeln('Min: ', FormatDateTime('yyyy-mm-dd', MinDate),
          '   Max: ', FormatDateTime('yyyy-mm-dd', MaxDate));
  {$ENDIF}
end;

procedure TlqPopupCalendar.HandleHide;
begin
  FocusRootWidget := FOrigFocusWin;
  FOrigFocusWin := nil;
  inherited HandleHide;
  if Assigned(FocusRootWidget) then
    FocusRootWidget.SetFocus;
end;

procedure TlqPopupCalendar.ShowDefaultPopupMenu;
var
  itm: TlqMenuItem;
  pt: TPoint;
begin
  if not Assigned(FMonthsPopupMenu) then
  begin
    FMonthsPopupMenu := TlqPopupMenu.Create(nil);
    itm := FMonthsPopupMenu.AddMenuItem(rslongjan, '', @miMonthClicked);
    itm.Tag := 1;
    itm := FMonthsPopupMenu.AddMenuItem(rslongfeb, '', @miMonthClicked);
    itm.Tag := 2;
    itm := FMonthsPopupMenu.AddMenuItem(rslongmar, '', @miMonthClicked);
    itm.Tag := 3;
    itm := FMonthsPopupMenu.AddMenuItem(rslongapr, '', @miMonthClicked);
    itm.Tag := 4;
    itm := FMonthsPopupMenu.AddMenuItem(rsLongMay, '', @miMonthClicked);
    itm.Tag := 5;
    itm := FMonthsPopupMenu.AddMenuItem(rslongjun, '', @miMonthClicked);
    itm.Tag := 6;
    itm := FMonthsPopupMenu.AddMenuItem(rslongjul, '', @miMonthClicked);
    itm.Tag := 7;
    itm := FMonthsPopupMenu.AddMenuItem(rslongaug, '', @miMonthClicked);
    itm.Tag := 8;
    itm := FMonthsPopupMenu.AddMenuItem(rslongsep, '', @miMonthClicked);
    itm.Tag := 9;
    itm := FMonthsPopupMenu.AddMenuItem(rslongoct, '', @miMonthClicked);
    itm.Tag := 10;
    itm := FMonthsPopupMenu.AddMenuItem(rslongnov, '', @miMonthClicked);
    itm.Tag := 11;
    itm := FMonthsPopupMenu.AddMenuItem(rslongdec, '', @miMonthClicked);
    itm.Tag := 12;
  end;

  // translate Edit coordinates
  pt := WindowToScreen(self, Point(edtMonth.Left, edtMonth.Bottom));
  TPopupMenuFriend(FMonthsPopupMenu).PrepareToShow;  // forces height calculation
  // If dropdown will not fit below Edit, then we place it above
  if (pt.y + FMonthsPopupMenu.Height) > lqApplication.ScreenHeight then
    pt.y := pt.y - edtMonth.Height - FMonthsPopupMenu.Height;

//  SetDefaultPopupMenuItemsState;
  FMonthsPopupMenu.ShowAt(nil, pt.x, pt.y);
end;

constructor TlqPopupCalendar.Create(AOwner: TComponent; AOrigFocusWin: TlqWidget);
begin
  inherited Create(AOwner);
  FntNorm:= lqApplication.GetFont('arial-9');
  FntBold:= lqApplication.GetFont('arial-9:bold');

  FOrigFocusWin := AOrigFocusWin;
  AfterCreate;
  FDate := Date;
  FWeekStartDay := 0;
  FWeeklyHoliday := -1;
  FDayColor := clText1;
  FHolidayColor := clText1;
  FSelectedColor := clWhite;
  FMonthOffset := 0;
  FCloseOnSelect := True;
  FSingleClickSelect := False;
  UpdateCalendar;
end;

destructor TlqPopupCalendar.Destroy;
begin
  if Assigned(FMonthsPopupMenu) then
    FMonthsPopupMenu.Free;
  if Assigned(FYearPopupWindow) then
    FYearPopupWindow.Free;
  FntBold.Free;
  FntNorm.Free;
  inherited Destroy;
end;

procedure TlqPopupCalendar.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: lqPopupCalendar}
  Name := 'fpgPopupCalendar';
  SetPosition(370, 182, 233, 142);
  Hint := '';

  edtYear := TlqEdit.Create(self);
  with edtYear do
  begin
    Name := 'edtYear';
    SetPosition(0, 0, 37, 22);
    AutoSize := False;
    BorderStyle := ebsSingle;
    Hint := '';
    TabOrder := 1;
    Text := '';
    FontDesc := '#Edit1';
    IgnoreMouseCursor := True;
    Focusable := False;
    OnClick := @edtYearClicked;
  end;

  btnYearUp := TlqButton.Create(self);
  with btnYearUp do
  begin
    Name := 'btnYearUp';
    SetPosition(37, 0, 13, 11);
    Text := '';
    Embedded := True;
    FontDesc := '#Label1';
    Hint := '';
    ImageMargin := 0;
    ImageName := 'sys.sb.up';
    TabOrder := 2;
    Focusable := False;
    OnClick := @btnYearUpClicked;
  end;

  btnYearDown := TlqButton.Create(self);
  with btnYearDown do
  begin
    Name := 'btnYearDown';
    SetPosition(37, 11, 13, 11);
    Text := '';
    Embedded := True;
    FontDesc := '#Label1';
    Hint := '';
    ImageMargin := 0;
    ImageName := 'sys.sb.down';
    TabOrder := 3;
    Focusable := False;
    OnClick := @btnYearDownClicked;
  end;

  edtMonth := TlqEdit.Create(self);
  with edtMonth do
  begin
    Name := 'edtMonth';
    SetPosition(50, 0, 100, 22);
    AutoSize := False;
    BorderStyle := ebsSingle;
    Hint := '';
    TabOrder := 4;
    Text := '';
    FontDesc := '#Edit1';
    IgnoreMouseCursor := True;
    Focusable := False;
    OnClick  := @edtMonthClicked;
  end;

  btnMonthUp := TlqButton.Create(self);
  with btnMonthUp do
  begin
    Name := 'btnMonthUp';
    SetPosition(150, 0, 13, 11);
    Text := '';
    Embedded := True;
    FontDesc := '#Label1';
    Hint := '';
    ImageMargin := 0;
    ImageName := 'sys.sb.up';
    TabOrder := 5;
    Focusable := False;
    OnClick := @btnMonthUpClicked;
  end;

  btnMonthDown := TlqButton.Create(self);
  with btnMonthDown do
  begin
    Name := 'btnMonthDown';
    SetPosition(150, 11, 13, 11);
    Text := '';
    Embedded := True;
    FontDesc := '#Label1';
    Hint := '';
    ImageMargin := 0;
    ImageName := 'sys.sb.down';
    TabOrder := 6;
    Focusable := False;
    OnClick := @btnMonthDownClicked;
  end;

  btnToday := TlqButton.Create(self);
  with btnToday do
  begin
    Name := 'btnToday';
    SetPosition(164, 0, 70, 22);
    Text := 'Today';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 7;
    Focusable := True;
    OnClick := @btnTodayClicked;
  end;

  grdName1 := TlqStringGrid.Create(self);
  with grdName1 do
  begin
    Name := 'grdName1';
    SetPosition(0, 23, 233, 119);
    AddColumn('Sun', 33, taCenter);
    AddColumn('Mon', 32, taCenter);
    AddColumn('Tue', 33, taCenter);
    AddColumn('Wed', 32, taCenter);
    AddColumn('Thu', 33, taCenter);
    AddColumn('Fri', 32, taCenter);
    AddColumn('Sat', 33, taCenter);
    FontDesc := '#Grid';
    HeaderFontDesc := '#GridHeader';
    Hint := '';
    RowCount := 6;
    RowSelect := False;
    TabOrder := 8;
    ScrollBarStyle := ssNone;
    OnClick  := @grdName1Clicked;
    OnDoubleClick := @grdName1DoubleClick;
    OnKeyPress := @grdName1KeyPress;
    OnDrawCell := @grdName1DrawCell;
  end;

  {@VFD_BODY_END: lqPopupCalendar}
  {%endregion}

  btnToday.Text := rsToday;
end;


{ TlqCalendarCombo }

procedure TlqCalendarCombo.SetDateValue(const AValue: TDateTime);
begin
  if ReadOnly then
    Exit;
  if FDate = AValue then
    Exit; //==>
  FDate := AValue;
  RePaint;
end;

procedure TlqCalendarCombo.SetMaxDate(const AValue: TDateTime);
begin
  if FMaxDate = AValue then
    Exit; //==>
  FMaxDate := AValue;

  // correct min/max values
  if FMinDate > AValue then
    FMinDate := IncMonth(AValue, -12); // one year less

  if FDate > FMaxDate then
  begin
    FDate := FMaxDate;
    Repaint;
  end;
end;

procedure TlqCalendarCombo.SetMinDate(const AValue: TDateTime);
begin
  if FMinDate = AValue then
    Exit; //==>
  FMinDate := AValue;
  
  // correct min/max values
  if AValue > FMaxDate then
    FMaxDate := IncMonth(AValue, 12); // one year more

  if FDate < FMinDate then
  begin
    FDate := FMinDate;
    Repaint;
  end;
end;

procedure TlqCalendarCombo.SetWeekStartDay(const AValue: integer);
begin
  if FWeekStartDay <> AValue then
    FWeekStartDay := AValue;
end;

procedure TlqCalendarCombo.SetWeeklyHoliday(const AValue: integer);
begin
  if FWeeklyHoliday <> AValue then
    FWeeklyHoliday := AValue;
end;

procedure TlqCalendarCombo.SetSelectedColor(const AValue: TlqColor);
begin
  if FSelectedColor <> AValue then
    FSelectedColor := AValue;
end;

procedure TlqCalendarCombo.SetDayColor(const AValue: TlqColor);
begin
  if FDayColor <> AValue then
    FDayColor := AValue;
end;

procedure TlqCalendarCombo.SetHolidayColor(const AValue: TlqColor);
begin
  if FHolidayColor <> AValue then
    FHolidayColor := AValue;
end;

procedure TlqCalendarCombo.SetText(const AValue: string);
begin
  try
    FDate := StrToDateTime(AValue);
  except
    on E: Exception do
    begin
      ShowMessage(E.Message);
    end;
  end;
end;

function TlqCalendarCombo.GetText: string;
begin
  Result := FormatDateTime(FDateFormat, FDate);
end;

procedure TlqCalendarCombo.SetCloseOnSelect(const AValue: boolean);
begin
  if FCloseOnSelect = AValue then
    Exit; //==>
  FCloseOnSelect := AValue;
end;

procedure TlqCalendarCombo.SetSingleClickSelect(const AValue: boolean);
begin
  if FSingleClickSelect = AValue then exit;
  FSingleClickSelect := AValue;
end;

function TlqCalendarCombo.HasText: boolean;
begin
  Result := FDate >= FMinDate;
end;

constructor TlqCalendarCombo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMinDate := EncodeDate(1900, 01, 01);
  FMaxDate := EncodeDate(2100, 01, 31);
  FWeeklyHoliday := -1;
  FDate := Now;
  FCloseOnSelect := True;
  FSingleClickSelect := False;
  DateFormat := ShortDateFormat;
end;

procedure TlqCalendarCombo.InternalOnValueSet(Sender: TObject;
  const ADate: TDateTime);
begin
  DateValue := ADate;
  if Assigned(OnChange) then
    OnChange(self);
  {$IFDEF DEBUG}
  writeln('New value: ', FormatDateTime(FDateFormat, ADate));
  {$ENDIF}
end;

procedure TlqCalendarCombo.SetDateFormat(const AValue: string);
var
  OldFormat: string;
begin
  if FDateFormat = AValue then
    Exit; //==>
  OldFormat := FDateFormat;
  FDateFormat := AValue;
  try
    FormatDateTime(FDateFormat, FDate);
    RePaint;
  except
    on E: Exception do
    begin
      FDateFormat := OldFormat;
      lqApplication.HandleException(self);
    end;
  end;
end;

procedure TlqCalendarCombo.DoDropDown;
var
  ddw: TlqPopupCalendar;
begin
  if (not Assigned(FDropDown)) or (not FDropDown.HasHandle) then
  begin
    FreeAndNil(FDropDown);  // safety measure
    FDropDown := TlqPopupCalendar.Create(nil, FocusRootWidget);
    ddw := TlqPopupCalendar(FDropDown);
    ddw.DontCloseWidget := self;
    { Set to false CloseOnSelect to leave opened popup calendar menu }
    ddw.CloseOnSelect := CloseOnSelect;
    ddw.SingleClickSelect := SingleClickSelect;
    ddw.CallerWidget  := self;

    if Assigned(OnDropDown) then
      OnDropDown(self);

    ddw.MinDate       := FMinDate;
    ddw.MaxDate       := FMaxDate;
    ddw.DateValue     := FDate;
    ddw.WeekStartDay  := FWeekStartDay;
    ddw.WeeklyHoliday := FWeeklyHoliday;
    ddw.DayColor        := FDayColor;
    ddw.HolidayColor    := FHolidayColor;
    ddw.SelectedColor   := FSelectedColor;
    ddw.ShowAt(Parent, Left, Top+Height);
    { I added this call to UpdateCalendar because sometimes after
      btnTodayClicked event, reopeing the dropdown menu gave an empty calendar }
    ddw.UpdateCalendar; //slapshot
    ddw.PopupFrame    := True;
    ddw.OnValueSet    := @InternalOnValueSet;
    ddw.OnClose       := @InternalOnClose;
  end
  else
  begin
    FBtnPressed := False;
    FDropDown.Close;
    FreeAndNil(FDropDown);
  end;
end;

{ TlqCalendarCheckCombo }

procedure TlqCalendarCheckCombo.SetChecked(const AValue: Boolean);
begin
  if AValue = FChecked then
    Exit; //==>
  FChecked := Avalue;
  Repaint;
end;

procedure TlqCalendarCheckCombo.DoCheckboxChanged;
begin
  if Assigned(FCheckboxChanged) then
    FCheckboxChanged(self, FChecked);
end;

procedure TlqCalendarCheckCombo.DoDrawText(const ARect: TlqRect);
var
  lRect: TlqRect;
var
  flags: TlqTextFlags;
begin
  lRect := ARect;
  lRect.Left := lRect.Left+FCheckBoxRect.Width + 1;
  lRect.Width := lRect.Width - (FCheckBoxRect.Width + 1);
  flags := [txtLeft, txtVCenter];
  if not Enabled then
    flags += [txtDisabled];
  if HasText then
  begin
    if not FChecked then
      Canvas.SetTextColor(clShadow1)
    else
    begin
      if Focused then
        Canvas.SetTextColor(clSelectionText)
      else
        Canvas.SetTextColor(TextColor);
    end;
    Canvas.DrawText(lRect, Text, flags)
  end
  else
  begin
    Canvas.SetTextColor(clShadow1);
    Canvas.DrawText(lRect, ExtraHint, flags);
  end;
end;

procedure TlqCalendarCheckCombo.InternalOnValueSet(Sender: TObject;
  const ADate: TDateTime);
begin
  inherited InternalOnValueSet(Sender, ADate);
  Checked := True;
end;

procedure TlqCalendarCheckCombo.HandleKeyPress(var keycode: word;
  var shiftstate: TShiftState; var consumed: boolean);
begin
  if keycode = keyEscape then
  begin
    consumed := True;
    Checked := False;
    DoCheckboxChanged;
  end;
  inherited HandleKeyPress(keycode, shiftstate, consumed);
end;

procedure TlqCalendarCheckCombo.HandlePaint;
var
  img: TlqImage;
  ix: integer;
begin
  inherited HandlePaint;
  // calculate which image to paint.
  if Enabled then
    ix := Ord(FChecked)
  else
    ix := (2 + (Ord(FChecked) * 2)) - Ord(FChecked);

  // paint the check (in this case a X)
  img := lqImages.GetImage('sys.checkboxes');    // Do NOT localize
  Canvas.DrawImagePart(FCheckBoxRect.Left, FCheckBoxRect.Top, img, ix*13, 0, 13, 13);
end;

procedure TlqCalendarCheckCombo.HandleResize(AWidth, AHeight: TlqCoord);
begin
  inherited HandleResize(AWidth, AHeight);
  FCheckBoxRect.Top := (AHeight - FCheckBoxRect.Height) div 2;
  OffsetRect(FCheckboxRect, 0, 3);  // frame border must be taken into consideration
end;

procedure TlqCalendarCheckCombo.HandleLMouseDown(x, y: integer;
  shiftstate: TShiftState);
begin
  if PtInRect(FCheckBoxRect, Point(x,y)) then
    // do nothing
  else
    inherited HandleLMouseDown(x, y, shiftstate);
end;

procedure TlqCalendarCheckCombo.HandleLMouseUp(x, y: integer;
  shiftstate: TShiftState);
begin
  if PtInRect(FCheckBoxRect, Point(x,y)) then
  begin
    Checked := not FChecked;
    DoCheckboxChanged;
    Repaint;
  end
  else
    inherited HandleLMouseUp(x, y, shiftstate);
end;

constructor TlqCalendarCheckCombo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FChecked := True;
  FCheckBoxRect.SetRect(2, 0, 17, 17);
  FCheckboxRect.Top := (FHeight - FCheckBoxRect.Height) div 2;
  OffsetRect(FCheckboxRect, 2, 3);  // frame border must be taken into consideration
end;


end.
