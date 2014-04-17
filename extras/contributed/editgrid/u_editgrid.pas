unit u_editgrid;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  lq_base,
  lq_main,
  lq_basegrid,
  lq_customgrid,
  lq_grid,
  lq_edit,
  lq_combobox,
  lq_editcombo,
  lq_checkbox,
  lq_popupcalendar;

type

  TEditType = (etNone, etText, etInteger, etFloat, etCurrency, etComboBox, etEditCombo, etCheckBox, etCalendar);
  TEditing =(edNone, edRow, edColumn);

  TlqColumnData = class(TObject)
  private
    FMaxSet: boolean;
    FMinSet: boolean;
  public
    constructor Create;
    destructor Destroy; override;
    property    MaxSet: boolean read FMaxSet write FMaxSet;
    property    MinSet: boolean read FminSet write FminSet;
  end;

  TlqNumericColumn = class(TlqColumnData)
  private
    FMaxLimit: boolean;
    FMinLimit: boolean;
    FDecimals: integer;
    FDecimalSeparator: TlqChar;
    FThousandSeparator: TlqChar;
    FShowThousand: boolean;
    FNegativeColor: TlqColor;
  public
    constructor Create;
    destructor  Destroy; override;
    property    MaxLimit: boolean read FMaxLimit write FMaxLimit;
    property    MinLimit: boolean read FMinLimit write FMinLimit;
    property    Decimals: integer read FDecimals write FDecimals;
    property    DecimalSeparator: TlqChar read FDecimalSeparator write FDecimalSeparator;
    property    ThousandSeparator: TlqChar read FThousandSeparator write FThousandSeparator;
    property    ShowThousand: boolean read FShowThousand write FShowThousand;
    property    NegativeColor: TlqColor read FNegativeColor write FNegativeColor;
  end;

  TlqIntegerColumn = class(TlqNumericColumn)
  private
    FMaxVal: integer;
    FMinVal: integer;
  public
    constructor Create;
    destructor  Destroy; override;
    property    MaxVal: integer read FMaxVal write FMaxVal;
    property    MinVal: integer read FMinVal write FMinVal;
  end;

  TlqFloatColumn = class(TlqNumericColumn)
  private
    FMaxVal: extended;
    FMinVal: extended;
    FFixedDecimals: integer;
  public
    constructor Create;
    destructor  Destroy; override;
    property    MaxVal: extended read FMaxVal write FMaxVal;
    property    MinVal: extended read FMinVal write FMinVal;
    property    FixedDecimals: integer read FFixedDecimals write FFixedDecimals;
  end;

  TlqCurrencyColumn = class(TlqNumericColumn)
  private
    FMaxVal: currency;
    FMinVal: currency;
  public
    constructor Create;
    destructor  Destroy; override;
    property    MaxVal: currency read FMaxVal write FMaxVal;
    property    MinVal: currency read FMinVal write FMinVal;
  end;

  TlqComboBoxColumn = class(TlqColumnData)
  private
    FItems: TStringList;
    FDropDownCount: integer;
  public
    constructor Create;
    destructor  Destroy; override;
    property    Items: TStringList read FItems write FItems;
    property    DropDownCount: integer read FDropDownCount write FDropDownCount;
  end;

  TlqEditComboColumn = class(TlqColumnData)
  private
    FItems: TStringList;
    FAutoComplete: boolean;
    FAllowNew: TAllowNew;
    FDropDownCount: integer;
  public
    constructor Create;
    destructor  Destroy; override;
    property    Items: TStringList read FItems write FItems;
    property    AutoComplete: boolean read FAutoComplete write FAutoComplete;
    property    AllowNew: TAllowNew read FAllowNew write FAllowNew;
    property    DropDownCount: integer read FDropDownCount write FDropDownCount;
  end;

  TlqCheckBoxColumn = class(TlqColumnData)
  private
    FChecked: string;
    FUnchecked: string;
    FBoxText: string;
  public
    constructor Create;
    destructor  Destroy; override;
    property    CheckedText: string read FChecked write FChecked;
    property    UncheckedText: string read FUnchecked write FUnchecked;
    property    BoxText: string read FBoxText write FBoxText;
  end;

  TDates = class
    FDate: TDateTime;
  end;

  TlqCalendarColumn = class(TlqColumnData)
  private
    FDatesList: TList;
    FGridDateFormat: string;
    FCalendarDateFormat: string;
    FDateValue: TDateTime;
    FMaxDate: TDateTime;
    FMinDate: TDateTime;
    FWeeklyHoliday: integer;
    FWeekStartDay: integer;
    FDayColor: TlqColor;
    FHolidayColor: TlqColor;
    FSingleClickSelect: boolean;
  public
    constructor Create;
    destructor  Destroy; override;
    property    DatesList: TList read FDatesList write FDatesList;
    property    GridDateFormat: string read FGridDateFormat write FGridDateFormat;
    property    CalendarDateFormat: string read FCalendarDateFormat write FCalendarDateFormat;
    property    DateValue: TDateTime read FDateValue write FDateValue;
    property    MaxDate: TDateTime read FMaxDate write FMaxDate;
    property    MinDate: TDateTime read FMinDate write FMinDate;
    property    WeeklyHoliday: integer read FWeeklyHoliday write FWeeklyHoliday;
    property    WeekStartDay: integer read FWeekStartDay write FWeekStartDay;
    property    DayColor: TlqColor read FDayColor write FDayColor;
    property    HolidayColor: TlqColor read FHolidayColor write FHolidayColor;
    property    SingleClickSelect: boolean read FSingleClickSelect write FSingleClickSelect;
  end;

  TlqEditColumn = class(TlqStringColumn)
  private
    FEditType: TEditType;
    FData: TlqColumnData;
  public
    property    EditType: TEditType read FEditType write FEditType;
    property    Data: TlqColumnData read FData write FData;
  end;

  TlqCustomEditgrid = class(TlqCustomStringGrid)
  private
    FDates: TDates;
    FCellEditText: TlqEdit;
    FCellEditInteger: TlqEditInteger;
    FCellEditFloat: TlqEditFloat;
    FCellEditCurrency: TlqEditCurrency;
    FCellComboBox: TlqComboBox;
    FCellEditCombo: TlqEditCombo;
    FCellCheckBox: TlqCheckBox;
    FCellCalendar: TlqCalendarCombo;
    FFocusRect: TlqRect;
    FEditing: boolean;
    FEditWay: TEditing;
    procedure   EditGridFocusChange(Sender: TObject; ARow,ACol: integer);
    procedure   EditGridDoubleClick(Sender: TObject; AButton: TMouseButton; AShift: TShiftState;
        const AMousePos: TPoint);
    procedure   EditGridMouseDown(Sender: TObject; AButton: TMouseButton; AShift: TShiftState;
        const AMousePos: TPoint);
    procedure   EditGridDrawCell(Sender: TObject; const ARow, ACol: Integer; const ARect: TlqRect;
        const AFlags: TlqGridDrawState; var ADefaultDrawing: boolean);
    function    GetColumnEditType(AIndex: integer): TEditType;
    function    GetTextColor(AIndex: integer): TlqColor;
    procedure   SetTextColor(AIndex: integer; const AValue: TlqColor);
    procedure   SetEditCell;
    procedure   CloseEditCell;
    procedure   SetReturnWay;
    procedure   SetTabWay(ShiftState: TShiftState);
    procedure   IniTextCell;
    procedure   FCellEditTextKeyPress(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState;
        var Consumed: boolean);
    function    GetNumericMaxLimit(AIndex: integer): boolean;
    procedure   SetNumericMaxLimit(AIndex: integer; const AValue: boolean);
    function    GetNumericMinLimit(AIndex: integer): boolean;
    procedure   SetNumericMinLimit(AIndex: integer; const AValue: boolean);
    function    GetNumericDecimals(AIndex: integer): integer;
    procedure   SetNumericDecimals(AIndex: integer; const AValue: integer);
    function    GetNumericDecimalSeparator(AIndex: integer): TlqChar;
    procedure   SetNumericDecimalSeparator(AIndex: integer; const AValue: TlqChar);
    function    GetNumericThousandSeparator(AIndex: integer): TlqChar;
    procedure   SetNumericThousandSeparator(AIndex: integer; const AValue: TlqChar);
    function    GetNumericShowThousand(AIndex: integer): boolean;
    procedure   SetNumericShowThousand(AIndex: integer; const AValue: boolean);
    function    GetNumericNegativeColor(AIndex: integer): TlqColor;
    procedure   SetNumericNegativeColor(AIndex: integer; const AValue: TlqColor);
    procedure   IniIntegerCell;
    procedure   FCellEditIntegerKeyPress(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState;
        var Consumed: boolean);
    function    GetMaxIntValue(AIndex: integer): integer;
    procedure   SetMaxIntValue(AIndex: integer; const AValue: integer);
    function    GetMinIntValue(AIndex: integer): integer;
    procedure   SetMinIntValue(AIndex: integer; const AValue: integer);
    procedure   IniFloatCell;
    procedure   FCellEditFloatKeyPress(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState;
        var Consumed: boolean);
    function    GetMaxFloatValue(AIndex: integer): extended;
    procedure   SetMaxFloatValue(AIndex: integer; const AValue: extended);
    function    GetMinFloatValue(AIndex: integer): extended;
    procedure   SetMinFloatValue(AIndex: integer; const AValue: extended);
    function    GetFloatFixedDecimals(AIndex: integer): integer;
    procedure   SetFloatFixedDecimals(AIndex: integer; const AValue: integer);
    procedure   IniCurrencyCell;
    procedure   FCellEditCurrencyKeyPress(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState;
        var Consumed: boolean);
    function    GetMaxCurrValue(AIndex: integer): currency;
    procedure   SetMaxCurrValue(AIndex: integer; const AValue: currency);
    function    GetMinCurrValue(AIndex: integer): currency;
    procedure   SetMinCurrValue(AIndex: integer; const AValue: currency);
    procedure   IniComboBoxCell;
    procedure   FCellComboBoxKeyPress(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState;
        var Consumed: boolean);
    function    GetComboBoxDropDownCount(AIndex: integer): integer;
    procedure   SetComboBoxDropDownCount(AIndex: integer; AValue: integer);
    procedure   IniEditComboCell;
    procedure   FCellEditComboKeyPress(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState;
        var Consumed: boolean);
    function    GetAutoComplete(AIndex: integer): boolean;
    procedure   SetAutoComplete(AIndex: integer; const AValue: boolean);
    function    GetAllowNew(AIndex: integer): TAllowNew;
    procedure   SetAllowNew(AIndex: integer; AValue: TAllowNew);
    function    GetEditComboDropDownCount(AIndex: integer): integer;
    procedure   SetEditComboDropDownCount(AIndex: integer; AValue: integer);
    procedure   IniCheckBoxCell;
    procedure   FCellCheckBoxKeyPress(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState;
        var Consumed: boolean);
    function    GetBoxCheckedText(AIndex: integer): string;
    procedure   SetBoxCheckedText(AIndex: integer; const AValue: string);
    function    GetBoxUncheckedText(AIndex: integer): string;
    procedure   SetBoxUncheckedText(AIndex: integer; const AValue: string);
    function    GetBoxDisplayText(AIndex: integer): string;
    procedure   SetBoxDisplayText(AIndex: integer; const AValue: string);
    procedure   IniCalendarCell;
    procedure   FCellCalendarKeyPress(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState;
        var Consumed: boolean);
    function    GetDates(AIndex: integer): TDateTime;
    procedure   SetDates(AIndex: integer; const AValue: TDateTime);
    function    GetDatesList(AIndex: integer): TList;
    procedure   SetDatesList(AIndex: integer; const AValue: TList);
    function    GetGridDateFormat(AIndex: integer): string;
    procedure   SetGridDateFormat(AIndex: integer; const AValue: string);
    function    GetCalendarDateFormat(AIndex: integer): string;
    procedure   SetCalendarDateFormat(AIndex: integer; const AValue: string);
    function    GetDateValue(AIndex: integer): TDateTime;
    procedure   SetDateValue(AIndex: integer; const AValue: TDateTime);
    function    GetMaxDate(AIndex: integer): TDateTime;
    procedure   SetMaxdate(AIndex: integer; const AValue: TDateTime);
    function    GetMinDate(AIndex: integer): TDateTime;
    procedure   SetMinDate(AIndex: integer; const AValue: TDateTime);
    function    GetWeeklyHoliday(AIndex: integer): integer;
    procedure   SetWeeklyHoliday(AIndex: integer; const AValue: integer);
    function    GetWeekStartDay(AIndex: integer): integer;
    procedure   SetWeekStartDay(AIndex: integer; const AValue: integer);
    function    GetDayColor(AIndex: integer): TlqColor;
    procedure   SetDayColor(AIndex: integer; const AValue: TlqColor);
    function    GetHolidayColor(AIndex: integer): TlqColor;
    procedure   SetHolidayColor(AIndex: integer; const AValue: TlqColor);
    function    GetSingleClickSelect(AIndex: integer): boolean;
    procedure   SetSingleClickSelect(AIndex: integer; const AValue: boolean);
  protected
    function    GetColumns(AIndex: Integer): TlqEditColumn; reintroduce;
    procedure   DrawCell(ARow, ACol: Integer; ARect: TlqRect; AFlags: TlqGridDrawState); override;
    procedure   HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); override;
    property    Columns[AIndex: Integer]: TlqEditColumn read GetColumns;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    property    EditWay: TEditing read FEditWay write FEditWay;
    function    AddColumn(ATitle: string; AWidth: integer; AEditType: TEditType = etNone;
        AAlignment: TAlignment = taLeftJustify; AbackgroundColor: TlqColor = clDefault; ATextColor: TlqColor = clDefault): TlqEditColumn; overload;
    property    ColumnEditType[AIndex: integer]: TEditType read GetColumnEditType;
    property    TextColor[AIndex: integer]: TlqColor read GetTextColor write SetTextColor;
    property    NumericMaxLimit[AIndex: integer]: boolean read GetNumericMaxLimit write SetNumericMaxLimit;
    property    NumericMinLimit[AIndex: integer]: boolean read GetNumericMinLimit write SetNumericMinLimit;
    property    NumericDecimals[AIndex: integer]: integer read GetNumericDecimals write SetNumericDecimals;
    property    NumericDecimalSeparator[AIndex: integer]: TlqChar read GetNumericDecimalSeparator write SetNumericDecimalSeparator;
    property    NumericThousandSeparator[AIndex: integer]: TlqChar read GetNumericThousandSeparator write SetNumericThousandSeparator;
    property    NumericShowThousand[AIndex: integer]: boolean read GetNumericShowThousand write SetNumericShowThousand;
    property    NumericNegativeColor[AIndex: integer]: Tlqcolor read GetNumericNegativeColor write SetNumericNegativeColor;
    property    MaxIntValue[AIndex: integer]: integer read GetMaxIntValue write SetMaxIntValue;
    property    MinIntValue[AIndex: integer]: integer read GetMinIntValue write SetMinIntValue;
    property    MaxFloatValue[AIndex: integer]: extended read GetMaxFloatValue write SetMaxFloatValue;
    property    MinFloatValue[AIndex: integer]: extended read GetMinFloatValue write SetMinFloatValue;
    property    FloatFixedDecimals[AIndex: integer]: integer read GetFloatFixedDecimals write SetFloatFixedDecimals;
    property    MaxCurrValue[AIndex: integer]: currency read GetMaxCurrValue write SetMaxCurrValue;
    property    MinCurrValue[AIndex: integer]: currency read GetMinCurrValue write SetMinCurrValue;
    procedure   AddComboItem(AIndex: integer; const AValue: string);
    property    ComboBoxDropDownCount[AIndex: integer]: integer read GetComboBoxDropDownCount write SetComboBoxDropDownCount;
    procedure   AddEditComboItem(AIndex: integer; const AValue: string);
    property    AutoComplete[AIndex: integer]: boolean read GetAutoComplete write SetAutoComplete;
    property    AllowNew[AIndex: integer]: TAllowNew read GetAllowNew write SetAllowNew;
    property    EditComboDropDownCount[AIndex: integer]: integer read GetEditComboDropDownCount write SetEditComboDropDownCount;
    property    BoxCheckedText[AIndex: integer]: string read GetBoxCheckedText write SetBoxCheckedText;
    property    BoxUncheckedText[AIndex: integer]: string read GetBoxUncheckedText write SetBoxUncheckedText;
    property    BoxDisplayText[AIndex: integer]: string read GetBoxDisplayText write SetBoxDisplayText;
    property    Dates[AIndex: integer]: TDateTime read GetDates write SetDates;
    property    DatesList[AIndex: integer]: TList read GetDatesList write SetDatesList;
    property    GridDateFormat[AIndex: integer]: string read GetGridDateFormat write SetGridDateFormat;
    property    CalendarDateFormat[AIndex: integer]: string read GetCalendarDateFormat write SetCalendarDateFormat;
    property    DateValue[AIndex: integer]: TDateTime read GetDateValue write SetDatevalue;
    property    MaxDate[AIndex: integer]: TDateTime read GetMaxDate write SetMaxDate;
    property    MinDate[AIndex: integer]: TDateTime read GetMinDate write SetMinDate;
    property    WeeklyHoliday[AIndex: integer]: integer read GetWeeklyHoliday write SetWeeklyHoliday;
    property    WeekStartDay[AIndex: integer]: integer read GetWeekStartDay write SetWeekStartDay;
    property    DayColor[AIndex: integer]: TlqColor read GetDayColor write SetDayColor;
    property    HolidayColor[AIndex: integer]: TlqColor read GetHolidayColor write SetHolidayColor;
    property    SingleClickSelect[AIndex: integer]: boolean read GetSingleClickSelect write SetSingleClickSelect;
   end;

  TlqEditGrid = class(TlqCustomEditgrid)
  public
    property    Font;
  published
    property    Align;
    property    AlternateBGColor;
    property    BackgroundColor;
    property    BorderStyle;
//    property    ColResizing;
    property    ColumnCount;
    property    Columns;
    property    ColumnWidth;
    property    DefaultColWidth;
    property    DefaultRowHeight;
    property    Enabled;
    property    FocusCol;
    property    FocusRow;
    property    FontDesc;
    property    HeaderFontDesc;
    property    HeaderHeight;
    property    HeaderStyle;
    property    Hint;
    property    Options;
    property    ParentShowHint;
    property    PopupMenu;
    property    RowCount;
    property    RowSelect;
    property    ScrollBarStyle;
    property    ShowGrid;
    property    ShowHeader;
    property    ShowHint;
    property    TabOrder;
    property    TopRow;
    property    VisibleRows;
    property    OnCanSelectCell;
    property    OnClick;
    property    OnDoubleClick;
    property    OnDrawCell;
    property    OnFocusChange;
    property    OnKeyPress;
    property    OnMouseDown;
    property    OnMouseEnter;
    property    OnMouseExit;
    property    OnMouseMove;
    property    OnMouseUp;
    property    OnRowChange;
    property    OnShowHint;
  end;

function CreateEditGrid(AOwner: TComponent; x, y, w, h: Tlqcoord; AColumnCount: integer = 0): TlqEditGrid;

implementation

uses
  lq_stringutils;

function CreateEditGrid(AOwner: TComponent; x, y, w, h: TlqCoord; AColumnCount: integer = 0): TlqEditGrid;
begin
  Result  := TlqEditGrid.Create(AOwner);
  Result.Left         := x;
  Result.Top          := y;
  Result.Width        := w;
  Result.Height       := h;
  Result.ColumnCount  := AColumnCount;
end;

constructor TlqColumnData.Create;
begin
  inherited Create;
  FMaxSet := False;
  FMinSet := False;
end;

destructor TlqColumnData.Destroy;
begin
  inherited Destroy;
end;

constructor TlqNumericColumn.Create;
begin
  inherited Create;
  FMaxLimit := False;
  FMinLimit := False;
  FDecimals := -1;
  FDecimalseparator := '.';
  FThousandSeparator := ' ';
  FShowThousand := True;
end;

destructor TlqNumericColumn.Destroy;
begin
  inherited Destroy;
end;

constructor TlqIntegerColumn.Create;
begin
  inherited Create;
  FMaxVal := 0;
  FMinVal := 0;
end;

destructor TlqIntegerColumn.Destroy;
begin
  inherited Destroy;
end;

constructor TlqFloatColumn.Create;
begin
  inherited Create;
  FMaxVal := 0;
  FMinVal := 0;
  FFixedDecimals := -1;
end;

destructor TlqFloatColumn.Destroy;
begin
  inherited Destroy;
end;

constructor TlqCurrencyColumn.Create;
begin
  inherited Create;
  FMaxVal := 0;
  FMinVal := 0;
end;

destructor TlqCurrencyColumn.Destroy;
begin
  inherited Destroy;
end;

constructor TlqComboBoxColumn.Create;
begin
  inherited Create;
  FItems := TStringList.Create;
end;

destructor TlqComboBoxColumn.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

constructor TlqEditComboColumn.Create;
begin
  inherited Create;
  FItems := TStringList.Create;
end;

destructor TlqEditComboColumn.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

constructor TlqCheckBoxColumn.Create;
begin
  inherited Create;
end;

destructor TlqCheckBoxColumn.Destroy;
begin
  inherited Destroy;
end;

constructor TlqCalendarColumn.Create;
begin
  inherited Create;
  FDatesList:= TList.Create;
end;

destructor TlqCalendarColumn.Destroy;
var
  i: integer;
begin
  inherited Destroy;
  if FDatesList.Count> 0 then
    for i := 0 to Pred(FDatesList.Count) do
      TDates(FDatesList[i]).Free;
  FDatesList.Free;
end;

procedure TlqCustomEditGrid.EditGridFocusChange(Sender: TObject; ARow,ACol: integer);
begin
  CloseEditCell;
end;

procedure TlqCustomEditGrid.EditGridDoubleClick(Sender: TObject; AButton: TMouseButton; AShift: TShiftState;
        const AMousePos: TPoint);
var
  lCol, lRow: integer;
begin
  MouseToCell(AMousePos.X, AMousePos.Y, lCol, lRow);
  case Columns[lCol].EditType of
    etText:
      IniTextCell;
    etInteger:
      IniIntegerCell;
    etFloat:
      IniFloatCell;
    etCurrency:
      IniCurrencyCell;
    etComboBox:
      IniComboBoxCell;
    etEditCombo:
      IniEditComboCell;
    etCheckBox:
      IniCheckBoxCell;
    etCalendar:
      IniCalendarCell;
  end;
  FEditing := True;
end;

procedure TlqCustomEditGrid.EditGridMouseDown(Sender: TObject; AButton: TMouseButton; AShift: TShiftState;
        const AMousePos: TPoint);
begin
  CloseEditCell;
end;

procedure TlqCustomEditGrid.EditGridDrawCell(Sender: TObject; const ARow, ACol: Integer; const ARect: TlqRect;
    const AFlags: TlqGridDrawState; var ADefaultDrawing: boolean);
var
  Txt, Deci: string;
  tSeparator,dseparator: TlqChar;
begin
  case Columns[Acol].EditType of
    etInteger:
      begin
        if Copy(Cells[ACol, ARow],1,1) = '-' then
        begin
          Txt:= UTF8Copy(Cells[ACol, ARow],2,Pred(UTF8Length(Cells[ACol, ARow])));
          Canvas.TextColor:= TlqNumericColumn(TlqEditColumn(Columns[ACol]).Data).NegativeColor;
        end
        else
          Txt:= Cells[ACol, ARow];
        if UTF8Length(Txt)> 3 then
        begin
          tSeparator:= TlqNumericColumn(TlqEditColumn(Columns[ACol]).Data).ThousandSeparator;
          if TlqNumericColumn(TlqEditColumn(Columns[ACol]).Data).ShowThousand then
          begin
            if UTF8Pos(tSeparator,Txt) = 0 then
            begin
              Txt:= UTF8Copy(Txt, 1, UTF8Length(txt) - 3) + tSeparator + UTF8Copy(Txt, UTF8Length(Txt) - 2, 3);
              while UTF8Pos(tSeparator,Txt) > 3 do
                Txt:= UTF8Copy(Txt, 1, UTF8Pos(tSeparator,Txt) - 4) + tSeparator
                    + UTF8Copy(Txt, UTF8Pos(tSeparator,Txt) - 3, UTF8Length(txt) - UTF8Pos(tSeparator,Txt) + 4);
            end;
          end
          else
            if UTF8Pos(tSeparator,Txt) > 0 then
              while UTF8Pos(tSeparator,Txt) > 0 do
                Txt:= UTF8Copy(txt, 1, Pred(UTF8Pos(tSeparator, txt)))
                      +UTF8Copy(txt, Succ(UTF8Pos(tSeparator, txt)), UTF8Length(txt) - UTF8Pos(tSeparator, txt));
          if Copy(Cells[ACol, ARow],1,1) = '-' then
            Cells[ACol, ARow] := '-' + Txt
          else
            Cells[ACol, ARow] := Txt;
        end;
      end;
    etFloat, etCurrency:
      begin
        if Copy(Cells[ACol, ARow],1,1) = '-' then
        begin
          Txt:= UTF8Copy(Cells[ACol, ARow],2,Pred(UTF8Length(Cells[ACol, ARow])));
          Canvas.TextColor:= TlqNumericColumn(TlqEditColumn(Columns[ACol]).Data).NegativeColor;
        end
        else
          Txt:= Cells[ACol, ARow];
        dSeparator:= TlqNumericColumn(TlqEditColumn(Columns[ACol]).Data).DecimalSeparator;
        if UTF8Pos(dSeparator,Txt) > 0 then
          Deci:= Copy(Cells[ACol, ARow], UTF8Pos(dSeparator,Txt), UTF8Length(Cells[ACol, ARow]) - Pred(UTF8Pos(dSeparator,Txt)));
        if (UTF8Length(Txt) - UTF8Length(Deci)) > 3 then
        begin
          tSeparator:= TlqNumericColumn(TlqEditColumn(Columns[ACol]).Data).ThousandSeparator;
          if TlqNumericColumn(TlqEditColumn(Columns[ACol]).Data).ShowThousand then
          begin
            if UTF8Pos(tSeparator,Txt) = 0 then
            begin
              Txt:= UTF8Copy(Txt, 1, UTF8Length(txt) - UTF8Length(Deci) - 3) + tSeparator + UTF8Copy(Txt, UTF8Length(Txt) - UTF8Length(Deci) - 2, 3) + Deci;
              while UTF8Pos(tSeparator,Txt) > 3 do
                Txt:= UTF8Copy(Txt, 1, UTF8Pos(tSeparator,Txt) - 4) + tSeparator
                    + UTF8Copy(Txt, UTF8Pos(tSeparator,Txt) - 3, UTF8Length(txt) - UTF8Length(Deci) - UTF8Pos(tSeparator,Txt) + 4) + Deci;
            end;
          end
          else
            if UTF8Pos(tSeparator,Txt) > 0 then
              while UTF8Pos(tSeparator,Txt) > 0 do
                Txt:= UTF8Copy(txt, 1, Pred(UTF8Pos(tSeparator, txt)))
                      +UTF8Copy(txt, Succ(UTF8Pos(tSeparator, txt)), UTF8Length(txt) - UTF8Length(Deci) - UTF8Pos(tSeparator, txt)) + Deci;
          if Copy(Cells[ACol, ARow],1,1) = '-' then
            Cells[ACol, ARow] := '-' + Txt
          else
            Cells[ACol, ARow] := Txt;
        end;
      end;
  end;
end;

function TlqCustomEditGrid.GetColumnEditType(AIndex: integer): TEditType;
begin
  Result := TlqEditColumn(Columns[AIndex]).EditType;
end;

function TlqCustomEditGrid.GetTextColor(AIndex: integer): TlqColor;
begin
  Result := TlqEditColumn(Columns[AIndex]).TextColor;
end;

procedure TlqCustomEditGrid.SetTextColor(AIndex: integer; const AValue: TlqColor);
begin
  TlqEditColumn(Columns[AIndex]).TextColor := AValue;
end;

procedure TlqCustomEditGrid.SetEditCell;
begin
  case Columns[FocusCol].EditType of
    etText:
      IniTextCell;
    etInteger:
      IniIntegerCell;
    etFloat:
      IniFloatCell;
    etCurrency:
      IniCurrencyCell;
    etComboBox:
      IniComboBoxCell;
    etEditCombo:
      IniEditComboCell;
    etCheckBox:
      IniCheckBoxCell;
    etCalendar:
      IniCalendarCell;
  end;
end;

procedure TlqCustomEditGrid.CloseEditCell;
var
  i: integer;
begin
  for i := 0 to Pred(ColumnCount) do
    case Columns[i].EditType of
      etText:
        if Assigned(FCellEditText) then
          begin
          FCellEditText.Text := '';
          FCellEditText.Visible := False;
          end;
      etInteger:
        if Assigned(FCellEditInteger) then
          begin
          FCellEditInteger.Text := '';
          FCellEditInteger.Visible := False;
          end;
      etFloat:
        if Assigned(FCellEditFloat) then
          begin
          FCellEditFloat.Text := '';
          FCellEditFloat.Visible := False;
          end;
      etCurrency:
        if Assigned(FCellEditCurrency) then
          begin
          FCellEditCurrency.Text := '';
          FCellEditCurrency.Visible := False;
          end;
      etComboBox:
        if Assigned(FCellComboBox) then
          begin
          FCellComboBox.Text := '';
          FCellComboBox.Visible := False;
          end;
      etEditCombo:
        if Assigned(FCellEditCombo) then
          begin
          FCellEditCombo.Text := '';
          FCellEditCombo.Visible := False;
          end;
      etCheckBox:
        if Assigned(FCellCheckBox) then
          begin
          FCellCheckBox.Text := '';
          FCellCheckBox.Visible := False;
          end;
      etCalendar:
        if Assigned(FCellCalendar) then
          begin
//          FCellCalendar.Text := '';
          FCellCalendar.Visible := False;
          end;
    end;
end;

procedure TlqCustomEditGrid.SetReturnWay;
begin
  case FEditWay of
    edNone:
      FEditing:= False;
    edColumn:
      if FocusCol < Pred(ColumnCount) then
        FocusCol := FocusCol + 1
      else
        FEditing:= False;
    edRow:
      if FocusRow < Pred(RowCount) then
        FocusRow := FocusRow + 1
      else
        FEditing:= False;
  end;
  SetFocus;
  if FEditing then
    SetEditCell;
end;

procedure TlqCustomEditGrid.SetTabWay(ShiftState: TShiftState);
begin
  if ssShift in ShiftState then
  begin
    if FocusCol > 0 then
      FocusCol := FocusCol - 1;
  end
  else
    if FocusCol < Pred(ColumnCount) then
      FocusCol := FocusCol + 1;
  FEditing := False;
  SetFocus;
end;

procedure TlqCustomEditGrid.IniTextCell;
var
  Pt: TPoint;
begin
  if Assigned(FCellEditText) then
    FCellEditText.Free;
  FCellEditText := TlqEdit.Create(Self.Parent);
  Pt.X := Left + FFocusRect.Left;
  Pt.Y := Top + FFocusRect.Top;
  with FCellEditText do
  begin
    Name := 'FCellEditText';
    SetPosition(Pt.X, Pt.Y, FFocusRect.Width, FFocusRect.Height);
    BorderStyle := ebsSingle;
    FontDesc := '#Grid';
    Text := Cells[FocusCol, FocusRow];
    OnKeyPress := @FCellEditTextKeyPress;
    SetFocus;
  end;
end;

procedure TlqCustomEditGrid.FCellEditTextKeyPress(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState;
          var Consumed: boolean);
begin
  if FCellEditText.Visible then
    case KeyCode of
      KeyReturn, KeyPEnter:
          begin
          Cells[FocusCol, FocusRow] := FCellEditText.Text;
          FCellEditText.Text := '';
          FCellEditText.Visible := False;
          SetReturnWay;
        end;
      KeyTab:
        begin
          FCellEditText.Text := '';
          FCellEditText.Visible := False;
          SetTabWay(ShiftState);
        end;
      KeyEscape:
        begin
        FCellEditText.Text := '';
        FCellEditText.Visible := False;
        FEditing := False;
        SetFocus;
        end;
    end;
end;

function TlqCustomEditGrid.GetNumericMaxLimit(AIndex: integer): boolean;
begin
  Result := TlqNumericColumn(TlqEditColumn(Columns[AIndex]).Data).MaxLimit;
end;

procedure TlqCustomEditGrid.SetNumericMaxLimit(AIndex: integer; const AValue: boolean);
begin
  TlqNumericColumn(TlqEditColumn(Columns[AIndex]).Data).MaxLimit := AValue;
end;

function TlqCustomEditGrid.GetNumericMinLimit(AIndex: integer): boolean;
begin
  Result := TlqNumericColumn(TlqEditColumn(Columns[AIndex]).Data).MinLimit;
end;

procedure TlqCustomEditGrid.SetNumericMinLimit(AIndex: integer; const AValue: boolean);
begin
  TlqNumericColumn(TlqEditColumn(Columns[AIndex]).Data).MinLimit := AValue;
end;

function TlqCustomEditGrid.GetNumericDecimals(AIndex: integer): integer;
begin
  Result := TlqNumericColumn(TlqEditColumn(Columns[AIndex]).Data).Decimals;
end;

procedure TlqCustomEditGrid.SetNumericDecimals(AIndex: integer; const AValue: integer);
begin
  TlqNumericColumn(TlqEditColumn(Columns[AIndex]).Data).Decimals := AValue;
end;

function TlqCustomEditGrid.GetNumericDecimalSeparator(AIndex: integer): TlqChar;
begin
  Result := TlqNumericColumn(TlqEditColumn(Columns[AIndex]).Data).DecimalSeparator;
end;

procedure TlqCustomEditGrid.SetNumericDecimalSeparator(AIndex: integer; const AValue: TlqChar);
begin
  TlqNumericColumn(TlqEditColumn(Columns[AIndex]).Data).DecimalSeparator := AValue
end;

function TlqCustomEditGrid.GetNumericThousandSeparator(AIndex: integer): TlqChar;
begin
  Result := TlqNumericColumn(TlqEditColumn(Columns[AIndex]).Data).ThousandSeparator;
end;

procedure TlqCustomEditGrid.SetNumericThousandSeparator(AIndex: integer; const AValue: TlqChar);
begin
  TlqNumericColumn(TlqEditColumn(Columns[AIndex]).Data).ThousandSeparator := AValue;
end;

function TlqCustomEditGrid.GetNumericShowThousand(AIndex: integer): boolean;
begin
  Result := TlqNumericColumn(TlqEditColumn(Columns[AIndex]).Data).ShowThousand;
end;

procedure TlqCustomEditGrid.SetNumericShowThousand(AIndex: integer; const AValue: boolean);
begin
  TlqNumericColumn(TlqEditColumn(Columns[AIndex]).Data).ShowThousand := AValue;
end;

function TlqCustomEditGrid.GetNumericNegativeColor(AIndex: integer): TlqColor;
begin
  Result := TlqNumericColumn(TlqEditColumn(Columns[AIndex]).Data).NegativeColor;
end;

procedure TlqCustomEditGrid.SetNumericNegativeColor(AIndex: integer; const AValue: TlqColor);
begin
  TlqNumericColumn(TlqEditColumn(Columns[AIndex]).Data).NegativeColor := AValue;
end;

procedure TlqCustomEditGrid.IniIntegerCell;
var
  Pt: TPoint;
begin
  if Assigned(FCellEditInteger) then
    FCellEditInteger.Free;
  FCellEditInteger := TlqEditInteger.Create(Self.Parent);
  Pt.X := Left + FFocusRect.Left;
  Pt.Y := Top + FFocusRect.Top;
  with FCellEditInteger do
  begin
    Name := 'FCellEditInteger';
    SetPosition(Pt.X, Pt.Y, FFocusRect.Width, FFocusRect.Height);
    BorderStyle := ebsSingle;
    FontDesc := '#Grid';
    Text := Cells[FocusCol, FocusRow];
    if TlqColumnData(TlqEditColumn(Columns[FocusCol]).Data).MaxSet then
      MaxValue := MaxIntValue[Focuscol];
    if TlqColumnData(TlqEditColumn(Columns[FocusCol]).Data).MinSet then
      MinValue := MinIntValue[FocusCol];
    CustomThousandSeparator := TlqNumericColumn(TlqEditColumn(Columns[FocusCol]).Data).ThousandSeparator;
    ShowThousand := TlqNumericColumn(TlqEditColumn(Columns[FocusCol]).Data).ShowThousand;
    NegativeColor := TlqNumericColumn(TlqEditColumn(Columns[FocusCol]).Data).NegativeColor;
    OnKeyPress := @FCellEditIntegerKeyPress;
    SetFocus;
  end;
end;

procedure TlqCustomEditGrid.FCellEditIntegerKeyPress(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState;
    var Consumed: boolean);
begin
  if FCellEditInteger.Visible then
    case KeyCode of
      KeyReturn, KeyPEnter:
        begin
          Cells[FocusCol, FocusRow] := FCellEditInteger.Text;
          FCellEditInteger.Text:= '';
          FCellEditInteger.Visible := False;
          SetReturnWay;
        end;
      KeyTab:
        begin
          FCellEditInteger.Text := '';
          FCellEditInteger.Visible := False;
          SetTabWay(ShiftState);
        end;
      KeyEscape:
        begin
          FCellEditInteger.Text := '';
          FCellEditInteger.Visible := False;
          FEditing := False;
          SetFocus;
        end;
    end;
end;

function TlqCustomEditGrid.GetMaxIntValue(AIndex: integer): integer;
begin
  Result := TlqIntegerColumn(TlqEditColumn(Columns[AIndex]).Data).MaxVal;
end;

procedure TlqCustomEditGrid.SetMaxIntValue(AIndex: integer; const AValue: integer);
begin
  with TlqIntegerColumn(TlqEditColumn(Columns[AIndex]).Data) do
  begin
    if AValue > MinVal then
      MaxVal := AValue
    else
      MaxVal := MinVal;
    MaxSet:= True;
  end;
end;

function TlqCustomEditGrid.GetMinIntValue(AIndex: integer): integer;
begin
  Result := TlqIntegerColumn(TlqEditColumn(Columns[AIndex]).Data).MinVal;
end;

procedure TlqCustomEditGrid.SetMinIntValue(AIndex: integer; const AValue: integer);
begin
  with TlqIntegerColumn(TlqEditColumn(Columns[AIndex]).Data) do
  begin
    if AValue < MaxVal then
      MinVal := AValue
    else
      MinVal := Maxval;
    MinSet := True;
  end;
end;

procedure TlqCustomEditGrid.IniFloatCell;
var
  Pt: TPoint;
begin
  if Assigned(FCellEditFloat) then
    FCellEditFloat.Free;
  FCellEditFloat := TlqEditFloat.Create(Self.Parent);
  Pt.X := Left + FFocusRect.Left;
  Pt.Y := Top + FFocusRect.Top;
  with FCellEditFloat do
  begin
    Name := 'FCellEditFloat';
    SetPosition(Pt.X, Pt.Y, FFocusRect.Width, FFocusRect.Height);
    BorderStyle := ebsSingle;
    FontDesc := '#Grid';
    Text := Cells[FocusCol, FocusRow];
    if TlqColumnData(TlqEditColumn(Columns[FocusCol]).Data).MaxSet then
      MaxValue := MaxFloatValue[Focuscol];
    if TlqColumnData(TlqEditColumn(Columns[FocusCol]).Data).MinSet then
      MinValue := MinFloatValue[FocusCol];
    Decimals := TlqNumericColumn(TlqEditColumn(Columns[FocusCol]).Data).Decimals;
    FixedDecimals := TlqFloatColumn(TlqEditColumn(Columns[FocusCol]).Data).FixedDecimals;
    CustomDecimalSeparator := TlqNumericColumn(TlqEditColumn(Columns[FocusCol]).Data).DecimalSeparator;
    CustomThousandSeparator := TlqNumericColumn(TlqEditColumn(Columns[FocusCol]).Data).ThousandSeparator;
    ShowThousand := TlqNumericColumn(TlqEditColumn(Columns[FocusCol]).Data).ShowThousand;
    OnKeyPress := @FCellEditFloatKeyPress;
    SetFocus;
  end;
end;

procedure TlqCustomEditGrid.FCellEditFloatKeyPress(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState;
    var Consumed: boolean);
begin
  if FCellEditFloat.Visible then
    case KeyCode of
      KeyReturn, KeyPEnter:
        begin
          with FCellEditFloat do
          begin
            if FixedDecimals > -1 then
            begin
              if UTF8Pos(CustomDecimalSeparator, Text) > 0 then
              begin
                if (UTF8Length(Text) - UTF8Pos(CustomDecimalSeparator, Text)) > FixedDecimals then
                  Text := Copy(Text, 1, UTF8Pos(CustomDecimalSeparator, Text) + FixedDecimals);
              end
              else
              begin
                if UTF8Pos(CustomDecimalSeparator, Text) = 0 then
                  Text := Text + CustomDecimalSeparator;
                while (UTF8Length(Text) - (UTF8Pos(CustomDecimalSeparator, Text)) < FixedDecimals) do
                  Text := Text +'0';
              end;
            end;
            if Decimals > -1 then
              if UTF8Pos(CustomDecimalSeparator, Text) > 0 then
                if (UTF8Length(Text) - UTF8Pos(CustomDecimalSeparator, Text)) > Decimals then
                  Text := Copy(Text, 1, UTF8Pos(CustomDecimalSeparator, Text) + Decimals);
          end;
          Cells[FocusCol, FocusRow] := FCellEditFloat.Text;
          FCellEditFloat.Text:= '';
          FCellEditFloat.Visible := False;
          SetReturnWay;
        end;
      KeyTab:
        begin
          FCellEditFloat.Text := '';
          FCellEditFloat.Visible := False;
          SetTabWay(ShiftState);
        end;
      KeyEscape:
        begin
          FCellEditFloat.Text := '';
          FCellEditFloat.Visible := False;
          FEditing := False;
          SetFocus;
        end;
    end;
end;

function TlqCustomEditGrid.GetMaxFloatValue(AIndex: integer): extended;
begin
  Result := TlqFloatColumn(TlqEditColumn(Columns[AIndex]).Data).MaxVal;
end;

procedure TlqCustomEditGrid.SetMaxFloatValue(AIndex: integer; const AValue: extended);
begin
  with TlqFloatColumn(TlqEditColumn(Columns[AIndex]).Data) do
  begin
    if AValue > MinVal then
      MaxVal := AValue
    else
      MaxVal := MinVal;
    MaxSet:= True;
  end;
end;

function TlqCustomEditGrid.GetMinFloatValue(AIndex: integer): extended;
begin
  Result := TlqFloatColumn(TlqEditColumn(Columns[AIndex]).Data).MinVal;
end;

procedure TlqCustomEditGrid.SetMinFloatValue(AIndex: integer; const AValue: extended);
begin
  with TlqFloatColumn(TlqEditColumn(Columns[AIndex]).Data) do
  begin
    if AValue < MaxVal then
      MinVal := AValue
    else
      MinVal := Maxval;
    MinSet := True;
  end;
end;

function TlqCustomEditGrid.GetFloatFixedDecimals(AIndex: integer): integer;
begin
  Result := TlqFloatColumn(TlqEditColumn(Columns[AIndex]).Data).FFixedDecimals;
end;

procedure TlqCustomEditGrid.SetFloatFixedDecimals(AIndex: integer; const AValue: integer);
begin
  TlqFloatColumn(TlqEditColumn(Columns[AIndex]).Data).FFixedDecimals := AValue;
end;

procedure TlqCustomEditGrid.IniCurrencyCell;
var
  Pt: TPoint;
begin
  if Assigned(FCellEditCurrency) then
    FCellEditCurrency.Free;
  FCellEditCurrency := TlqEditCurrency.Create(Self.Parent);
  Pt.X := Left + FFocusRect.Left;
  Pt.Y := Top + FFocusRect.Top;
  with FCellEditCurrency do
  begin
    Name := 'FCellEditCurrency';
    SetPosition(Pt.X, Pt.Y, FFocusRect.Width, FFocusRect.Height);
    BorderStyle := ebsSingle;
    FontDesc := '#Grid';
    Text := Cells[FocusCol, FocusRow];
    if TlqColumnData(TlqEditColumn(Columns[FocusCol]).Data).MaxSet then
      MaxValue := MaxCurrValue[Focuscol];
    if TlqColumnData(TlqEditColumn(Columns[FocusCol]).Data).MinSet then
      MinValue := MinCurrValue[FocusCol];
    CustomDecimalSeparator := TlqNumericColumn(TlqEditColumn(Columns[FocusCol]).Data).DecimalSeparator;
    CustomThousandSeparator := TlqNumericColumn(TlqEditColumn(Columns[FocusCol]).Data).ThousandSeparator;
    ShowThousand := TlqNumericColumn(TlqEditColumn(Columns[FocusCol]).Data).ShowThousand;
    OnKeyPress := @FCellEditCurrencyKeyPress;
    SetFocus;
  end;
end;

procedure TlqCustomEditGrid.FCellEditCurrencyKeyPress(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState;
    var Consumed: boolean);
begin
  if FCellEditCurrency.Visible then
    case KeyCode of
      KeyReturn, KeyPEnter:
        begin
          Cells[FocusCol, FocusRow] := FCellEditCurrency.Text;
          FCellEditCurrency.Text:= '';
          FCellEditCurrency.Visible := False;
          SetReturnWay;
        end;
      KeyTab:
        begin
          FCellEditCurrency.Text := '';
          FCellEditCurrency.Visible := False;
          SetTabWay(ShiftState);
        end;
      KeyEscape:
        begin
          FCellEditCurrency.Text := '';
          FCellEditCurrency.Visible := False;
          FEditing := False;
          SetFocus;
        end;
    end;
end;

function TlqCustomEditGrid.GetMaxCurrValue(AIndex: integer): currency;
begin
  Result := TlqCurrencyColumn(TlqEditColumn(Columns[AIndex]).Data).MaxVal;
end;

procedure TlqCustomEditGrid.SetMaxCurrValue(AIndex: integer; const AValue: currency);
begin
  with TlqCurrencyColumn(TlqEditColumn(Columns[AIndex]).Data) do
  begin
    if AValue > MinVal then
      MaxVal := AValue
    else
      MaxVal := MinVal;
    MaxSet:= True;
  end;
end;

function TlqCustomEditGrid.GetMinCurrValue(AIndex: integer): currency;
begin
  Result := TlqCurrencyColumn(TlqEditColumn(Columns[AIndex]).Data).MinVal;
end;

procedure TlqCustomEditGrid.SetMinCurrValue(AIndex: integer; const AValue: currency);
begin
  with TlqCurrencyColumn(TlqEditColumn(Columns[AIndex]).Data) do
  begin
    if AValue < MaxVal then
      MinVal := AValue
    else
      MinVal := Maxval;
    MinSet := True;
  end;
end;

procedure TlqCustomEditGrid.IniComboBoxCell;
var
  Pt: TPoint;
  i: integer;
begin
  if Assigned(FCellComboBox) then
    FCellComboBox.Free;
  FCellComboBox := TlqComboBox.Create(Self.Parent);
  Pt.X := Left + FFocusRect.Left;
  Pt.Y := Top + FFocusRect.Top;
  with FCellComboBox do
  begin
    Name := 'FCellComboBox';
    SetPosition(Pt.X, Pt.Y, FFocusRect.Width, FFocusRect.Height);
    BorderStyle := ebsSingle;
    FontDesc := '#Grid';
    Items.Assign(TlqComboBoxColumn(TlqEditColumn(Columns[FocusCol]).Data).FItems);
    for i := 0 to Pred(Items.Count) do
      if Items[i] = Cells[FocusCol, FocusRow] then
        Text := Items[i];
    DropDownCount := TlqComboBoxColumn(TlqEditColumn(Columns[FocusCol]).Data).DropDownCount;
    OnKeyPress := @FCellComboBoxKeyPress;
    SetFocus;
  end;
end;

procedure TlqCustomEditGrid.FCellComboBoxKeyPress(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState;
    var Consumed: boolean);
begin
  if FCellComboBox.Visible then
    case KeyCode of
      KeyReturn, KeyPEnter:
        begin
          Cells[FocusCol, FocusRow] := FCellComboBox.Text;
          FCellComboBox.Text:= '';
          FCellComboBox.Visible := False;
          SetReturnWay;
        end;
      KeyTab:
        begin
          FCellComboBox.Text := '';
          FCellComboBox.Visible := False;
          SetTabWay(ShiftState);
        end;
      KeyEscape:
        begin
          FCellComboBox.Text := '';
          FCellComboBox.Visible := False;
          FEditing := False;
          SetFocus;
        end;
    end;
end;

function TlqCustomEditGrid.GetComboBoxDropDownCount(AIndex: integer): integer;
begin
  Result := TlqComboBoxColumn(TlqEditColumn(Columns[AIndex]).Data).DropDownCount;
end;

procedure TlqCustomEditGrid.SetComboBoxDropDownCount(AIndex: integer; AValue: integer);
begin
  TlqComboBoxColumn(TlqEditColumn(Columns[AIndex]).Data).DropDownCount := AValue;
end;

procedure TlqCustomEditGrid.IniEditComboCell;
var
  Pt: TPoint;
  i: integer;
begin
  if Assigned(FCellEditCombo) then
    FCellEditCombo.Free;
  FCellEditCombo := TlqEditCombo.Create(Self.Parent);
  Pt.X := Left + FFocusRect.Left;
  Pt.Y := Top + FFocusRect.Top;
  with FCellEditCombo do
  begin
    Name := 'FCellEditCombo';
    SetPosition(Pt.X, Pt.Y, FFocusRect.Width, FFocusRect.Height);
    BorderStyle := ebsSingle;
    FontDesc := '#Grid';
    Items.Assign(TlqEditComboColumn(TlqEditColumn(Columns[FocusCol]).Data).FItems);
    for i := 0 to Pred(Items.Count) do
      if Items[i] = Cells[FocusCol, FocusRow] then
        Text := Items[i];
    AutoCompletion := TlqEditComboColumn(TlqEditColumn(Columns[FocusCol]).Data).AutoComplete;
    AllowNew := TlqEditComboColumn(TlqEditColumn(Columns[FocusCol]).Data).AllowNew;
    DropDownCount := TlqEditComboColumn(TlqEditColumn(Columns[FocusCol]).Data).DropDownCount;
    OnKeyPress := @FCellEditComboKeyPress;
    SetFocus;
  end;
end;

procedure TlqCustomEditGrid.FCellEditComboKeyPress(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState;
    var Consumed: boolean);
begin
  if FCellEditCombo.Visible then
    case KeyCode of
      KeyReturn, KeyPEnter:
        begin
          Cells[FocusCol, FocusRow] := FCellEditCombo.Text;
          if (AllowNew[FocusCol] = anAsk) or (AllowNew[FocusCol] = anYes) then
            TlqEditComboColumn(TlqEditColumn(Columns[FocusCol]).Data).FItems.Add(FCellEditCombo.Text);
          FCellEditCombo.Text:= '';
          FCellEditCombo.Visible := False;
          SetReturnWay;
        end;
      KeyTab:
        begin
          FCellEditCombo.Text := '';
          FCellEditCombo.Visible := False;
          SetTabWay(ShiftState);
        end;
      KeyEscape:
        begin
          FCellEditCombo.Text := '';
          FCellEditCombo.Visible := False;
          FEditing := False;
          SetFocus;
        end;
    end;
end;

function TlqCustomEditGrid.GetAutoComplete(AIndex: integer): boolean;
begin
  Result := TlqEditComboColumn(TlqEditColumn(Columns[AIndex]).Data).AutoComplete;
end;

procedure TlqCustomEditGrid.SetAutoComplete(AIndex: integer; const AValue: boolean);
begin
  TlqEditComboColumn(TlqEditColumn(Columns[AIndex]).Data).AutoComplete := AValue;
end;

function TlqCustomEditGrid.GetAllowNew(AIndex: integer): TAllowNew;
begin
  Result := TlqEditComboColumn(TlqEditColumn(Columns[AIndex]).Data).AllowNew;
end;

procedure TlqCustomEditGrid.SetAllowNew(AIndex: integer; AValue: TAllowNew);
begin
  TlqEditComboColumn(TlqEditColumn(Columns[AIndex]).Data).AllowNew := AValue;
end;

function TlqCustomEditGrid.GetEditComboDropDownCount(AIndex: integer): integer;
begin
  Result := TlqEditComboColumn(TlqEditColumn(Columns[AIndex]).Data).DropDownCount;
end;

procedure TlqCustomEditGrid.SetEditComboDropDownCount(AIndex: integer; AValue: integer);
begin
  TlqEditComboColumn(TlqEditColumn(Columns[AIndex]).Data).DropDownCount := AValue;
end;

procedure TlqCustomEditGrid.IniCheckBoxCell;
var
  Pt: TPoint;
begin
  if Assigned(FCellCheckBox) then
    FCellCheckBox.Free;
  FCellCheckBox := TlqCheckBox.Create(Self.Parent);
  Pt.X := Left + FFocusRect.Left;
  Pt.Y := Top + FFocusRect.Top;
  with FCellCheckBox do
  begin
    Name := 'FCellCheckBox';
    SetPosition(Pt.X, Pt.Y, FFocusRect.Width, FFocusRect.Height);
    BorderStyle := ebsSingle;
    FontDesc := '#Grid';
    if Cells[FocusCol, FocusRow] = TlqCheckBoxColumn(TlqEditColumn(Columns[FocusCol]).Data).CheckedText then
      FCellCheckBox.Checked:= True
    else
      FCellCheckBox.Checked:= False;
    Text := TlqCheckBoxColumn(TlqEditColumn(Columns[FocusCol]).Data).BoxText;
    OnKeyPress := @FCellCheckBoxKeyPress;
    SetFocus;
  end;
end;

procedure TlqCustomEditGrid.FCellCheckBoxKeyPress(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState;
    var Consumed: boolean);
begin
  if FCellCheckBox.Visible then
    case KeyCode of
      KeyReturn, KeyPEnter:
        begin
          if FCellCheckBox.Checked then
            Cells[FocusCol, FocusRow] := TlqCheckBoxColumn(TlqEditColumn(Columns[FocusCol]).Data).CheckedText
          else
            Cells[FocusCol, FocusRow] := TlqCheckBoxColumn(TlqEditColumn(Columns[FocusCol]).Data).UncheckedText;
          FCellCheckBox.Text:= '';
          FCellCheckBox.Visible := False;
          SetReturnWay;
        end;
      KeyTab:
        begin
          FCellCheckBox.Text := '';
          FCellCheckBox.Visible := False;
          SetTabWay(ShiftState);
        end;
      KeyEscape:
        begin
          FCellCheckBox.Text := '';
          FCellCheckBox.Visible := False;
          FEditing := False;
          SetFocus;
        end;
    end;
end;

function TlqCustomEditGrid.GetBoxCheckedText(AIndex: integer): string;
begin
  Result := TlqCheckBoxColumn(TlqEditColumn(Columns[AIndex]).Data).CheckedText;
end;

procedure TlqCustomEditGrid.SetBoxCheckedText(AIndex: integer; const AValue: string);
begin
  TlqCheckBoxColumn(TlqEditColumn(Columns[AIndex]).Data).CheckedText := AValue;
end;

function TlqCustomEditGrid.GetBoxUncheckedText(AIndex: integer): string;
begin
  Result := TlqCheckBoxColumn(TlqEditColumn(Columns[AIndex]).Data).UncheckedText;
end;

procedure TlqCustomEditGrid.SetBoxUncheckedText(AIndex: integer; const AValue: string);
begin
  TlqCheckBoxColumn(TlqEditColumn(Columns[AIndex]).Data).UncheckedText := AValue;
end;

function TlqCustomEditGrid.GetBoxDisplayText(AIndex: integer): string;
begin
  Result := TlqCheckBoxColumn(TlqEditColumn(Columns[AIndex]).Data).BoxText;
end;

procedure TlqCustomEditGrid.SetBoxDisplayText(AIndex: integer; const AValue: string);
begin
  TlqCheckBoxColumn(TlqEditColumn(Columns[AIndex]).Data).BoxText := AValue;
end;

procedure TlqCustomEditGrid.IniCalendarCell;
var
  Pt: TPoint;
begin
  if Assigned(FCellCalendar) then
    FCellCalendar.Free;
  FCellCalendar := TlqCalendarCombo.Create(Self.Parent);
  Pt.X := Left + FFocusRect.Left;
  Pt.Y := Top + FFocusRect.Top;
  with FCellCalendar do
  begin
    Name := 'FCellCalendar';
    SetPosition(Pt.X, Pt.Y, FFocusRect.Width, FFocusRect.Height);
    BorderStyle := ebsSingle;
    FontDesc := '#Grid';
    DateFormat := TlqCalendarColumn(TlqEditColumn(Columns[FocusCol]).Data).CalendarDateFormat;
    DateValue:= TDates(TlqCalendarColumn(TlqEditColumn(Columns[FocusCol]).Data).FDatesList[FocusRow]).FDate;
    WeeklyHoliday := TlqCalendarColumn(TlqEditColumn(Columns[FocusCol]).Data).WeeklyHoliday;
    WeekStartDay := TlqCalendarColumn(TlqEditColumn(Columns[FocusCol]).Data).WeekStartDay;
    DayColor := TlqCalendarColumn(TlqEditColumn(Columns[FocusCol]).Data).DayColor;
    HolidayColor := TlqCalendarColumn(TlqEditColumn(Columns[FocusCol]).Data).HolidayColor;
    SingleClickSelect := TlqCalendarColumn(TlqEditColumn(Columns[FocusCol]).Data).SingleClickSelect;
    OnKeyPress := @FCellCalendarKeyPress;
    SetFocus;
  end;
end;

procedure TlqCustomEditGrid.FCellCalendarKeyPress(Sender: TObject; var KeyCode: word; var ShiftState: TShiftState;
    var Consumed: boolean);
begin
  if FCellCalendar.Visible then
    case KeyCode of
      KeyReturn, KeyPEnter:
        begin
          with TlqCalendarColumn(TlqEditColumn(Columns[FocusCol]).Data) do
          begin
            TDates(FDatesList[FocusRow]).FDate := FCellCalendar.DateValue;
            Cells[FocusCol, FocusRow] := FormatDateTime(GridDateFormat, TDates(FDatesList[FocusRow]).FDate);
          end;
          //FCellCalendar.Text := '';
          FCellCalendar.Visible := False;
          SetReturnWay;
        end;
      KeyTab:
        begin
          //FCellCalendar.Text := '';
          FCellCalendar.Visible := False;
          SetTabWay(ShiftState);
        end;
      KeyEscape:
        begin
          //FCellCalendar.Text := '';
          FCellCalendar.Visible := False;
          FEditing := False;
          SetFocus;
        end;
    end;
end;

function TlqCustomEditGrid.GetDates(AIndex: integer): TDateTime;
begin
  Result := TDates(TlqCalendarColumn(TlqEditColumn(Columns[AIndex]).Data).DatesList[Succ(FocusRow)]).FDate;
end;

procedure TlqCustomEditGrid.SetDates(AIndex: integer; const AValue: TDateTime);
begin
  FDates  := TDates.Create;
  FDates.FDate:= AValue;
  TlqCalendarColumn(TlqEditColumn(Columns[AIndex]).Data).DatesList.Add(FDates);
end;

function TlqCustomEditGrid.GetDatesList(AIndex: integer): TList;
begin
  Result := TlqCalendarColumn(TlqEditColumn(Columns[AIndex]).Data).DatesList;
end;

procedure TlqCustomEditGrid.SetDatesList(AIndex: integer; const AValue: TList);
begin
  TlqCalendarColumn(TlqEditColumn(Columns[AIndex]).Data).DatesList := AValue;
end;

function TlqCustomEditGrid.GetGridDateFormat(AIndex: integer): string;
begin
  Result := TlqCalendarColumn(TlqEditColumn(Columns[AIndex]).Data).GridDateFormat;
end;

procedure TlqCustomEditGrid.SetGridDateFormat(AIndex: integer; const AValue: string);
begin
  TlqCalendarColumn(TlqEditColumn(Columns[AIndex]).Data).GridDateFormat:= AValue;
end;

function TlqCustomEditGrid.GetCalendarDateFormat(AIndex: integer): string;
begin
  Result := TlqCalendarColumn(TlqEditColumn(Columns[AIndex]).Data).CalendarDateFormat;
end;

procedure TlqCustomEditGrid.SetCalendarDateFormat(AIndex: integer; const AValue: string);
begin
  TlqCalendarColumn(TlqEditColumn(Columns[AIndex]).Data).CalendarDateFormat:= AValue;
end;

function TlqCustomEditGrid.GetDateValue(AIndex: integer): TDateTime;
begin
  Result := TlqCalendarColumn(TlqEditColumn(Columns[AIndex]).Data).DateValue;
end;

procedure TlqCustomEditGrid.SetDateValue(AIndex: integer; const AValue: TDateTime);
begin
  TlqCalendarColumn(TlqEditColumn(Columns[AIndex]).Data).DateValue:= AValue;
end;

function TlqCustomEditGrid.GetMaxDate(AIndex: integer): TDateTime;
begin
  Result := TlqCalendarColumn(TlqEditColumn(Columns[AIndex]).Data).MaxDate;
end;

procedure TlqCustomEditGrid.SetMaxdate(AIndex: integer; const AValue: TDateTime);
begin
  TlqCalendarColumn(TlqEditColumn(Columns[AIndex]).Data).MaxDate:= AValue;
end;

function TlqCustomEditGrid.GetMinDate(AIndex: integer): TDateTime;
begin
  Result := TlqCalendarColumn(TlqEditColumn(Columns[AIndex]).Data).MinDate;
end;

procedure TlqCustomEditGrid.SetMinDate(AIndex: integer; const AValue: TDateTime);
begin
  TlqCalendarColumn(TlqEditColumn(Columns[AIndex]).Data).MinDate:= AValue;
end;

function TlqCustomEditGrid.GetWeeklyHoliday(AIndex: integer): integer;
begin
  Result := TlqCalendarColumn(TlqEditColumn(Columns[AIndex]).Data).WeeklyHoliday;
end;

procedure TlqCustomEditGrid.SetWeeklyHoliday(AIndex: integer; const AValue: integer);
begin
  TlqCalendarColumn(TlqEditColumn(Columns[AIndex]).Data).WeeklyHoliday:= AValue;
end;

function TlqCustomEditGrid.GetWeekStartDay(AIndex: integer): integer;
begin
  Result := TlqCalendarColumn(TlqEditColumn(Columns[AIndex]).Data).WeekStartDay;
end;

procedure TlqCustomEditGrid.SetWeekStartDay(AIndex: integer; const AValue: integer);
begin
  TlqCalendarColumn(TlqEditColumn(Columns[AIndex]).Data).WeekStartDay:= AValue;
end;

function TlqCustomEditGrid.GetDayColor(AIndex: integer): TlqColor;
begin
  Result := TlqCalendarColumn(TlqEditColumn(Columns[AIndex]).Data).DayColor;
end;

procedure TlqCustomEditGrid.SetDayColor(AIndex: integer; const AValue: TlqColor);
begin
  TlqCalendarColumn(TlqEditColumn(Columns[AIndex]).Data).DayColor:= AValue;
end;

function TlqCustomEditGrid.GetHolidayColor(AIndex: integer): TlqColor;
begin
  Result := TlqCalendarColumn(TlqEditColumn(Columns[AIndex]).Data).HolidayColor;
end;

procedure TlqCustomEditGrid.SetHolidayColor(AIndex: integer; const AValue: TlqColor);
begin
  TlqCalendarColumn(TlqEditColumn(Columns[AIndex]).Data).HolidayColor:= AValue;
end;

function TlqCustomEditGrid.GetSingleClickSelect(AIndex: integer): boolean;
begin
  Result := TlqCalendarColumn(TlqEditColumn(Columns[AIndex]).Data).SingleClickSelect;
end;

procedure TlqCustomEditGrid.SetSingleClickSelect(AIndex: integer; const AValue: boolean);
begin
  TlqCalendarColumn(TlqEditColumn(Columns[AIndex]).Data).SingleClickSelect:= AValue;
end;

function TlqCustomEditGrid.GetColumns(AIndex: Integer): TlqEditColumn;
begin
  if (AIndex < 0) or (AIndex > ColumnCount-1) then
    Result := nil
  else
    Result := TlqEditColumn(FColumns.Items[AIndex]);
end;

procedure TlqCustomEditGrid.DrawCell(ARow, ACol: Integer; ARect: TlqRect; AFlags: TlqGridDrawState);
begin
  inherited DrawCell(ARow, ACol, ARect, AFlags);
  if (gdSelected in AFlags) then
    FFocusRect:= ARect;
end;

procedure TlqCustomEditGrid.HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean);
var
  i: integer;
begin
  inherited HandleKeyPress(keycode, shiftstate, consumed);
  if not Enabled then
    consumed := False
  else
  begin
    consumed := True;

    case keycode of
      keyInsert:
        for i := 0 to Pred(ColumnCount) do
          if Columns[i].EditType = etCalendar then
          begin
            FDates:= TDates.Create;
            FDates.FDate:= TlqCalendarColumn(TlqEditColumn(Columns[i]).Data).DateValue;
            TlqCalendarColumn(TlqEditColumn(Columns[i]).Data).FDatesList.Add(FDates);
          end;
      keyDelete:
        if RowCount > 0 then
          begin
          // specific warning and action should be performed in descendant
          end;
      keyReturn, keyPEnter, KeyF2:
        if RowCount > 0 then
          begin
          case Columns[FocusCol].EditType of
            etText:
              IniTextCell;
            etInteger:
              IniIntegerCell;
            etFloat:
              IniFloatCell;
            etCurrency:
              IniCurrencyCell;
            etComboBox:
              IniComboBoxCell;
            etEditCombo:
              IniEditComboCell;
            etCheckBox:
              IniCheckBoxCell;
            etCalendar:
              IniCalendarCell;
          end;
          FEditing := True;
          end;
      keyTab:
        begin
          if ssShift in ShiftState then
          begin
            if FocusCol > 0 then
              FocusCol := FocusCol - 1;
          end
          else
            if FocusCol < Pred(ColumnCount) then
              FocusCol := FocusCol + 1;
          SetFocus;
        end;
    else
      Consumed := False;
    end;
  end;
end;

constructor TlqCustomEditGrid.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  OnDoubleClick := @EditGridDoubleClick;
  OnFocusChange := @EditGridFocusChange;
  OnMouseDown := @EditGridMouseDown;
  OnDrawCell := @EditGridDrawCell;
  FEditing := False;
  FEditWay := edNone;
end;

destructor TlqCustomEditGrid.Destroy;
begin
  inherited Destroy;
end;

function TlqCustomEditGrid.AddColumn(ATitle: string; AWidth: integer; AEditType: TEditType = etNone;
        AAlignment: TAlignment = taLeftJustify; AbackgroundColor: TlqColor = clDefault; ATextColor: TlqColor = clDefault): TlqEditColumn;
begin
  Updating;
  Result := TlqEditColumn(inherited AddColumn(ATitle, AWidth, AAlignment, ABackgroundColor, ATextColor));
  with Result do
  begin
    FEditType := AEditType;
    case FEditType of
      etInteger:
        Result.FData:= TlqIntegerColumn.Create;
      etFloat:
        Result.FData:= TlqFloatColumn.Create;
      etCurrency:
        Result.FData:= TlqCurrencyColumn.Create;
      etComboBox:
        Result.FData:= TlqComboBoxColumn.Create;
      etEditCombo:
        Result.FData:= TlqEditComboColumn.Create;
      etCheckBox:
        Result.FData:= TlqCheckBoxColumn.Create;
      etCalendar:
        Result.FData := TlqCalendarColumn.Create;
      else
        Result.FData:= nil;
    end;
  end;

  if UpdateCount = 0 then
    Updated;
end;

procedure TlqCustomEditGrid.AddComboItem(AIndex: integer; const AValue: string);
begin
  TlqComboBoxColumn(TlqEditColumn(Columns[AIndex]).Data).FItems.Add(AValue);
end;

procedure TlqCustomEditGrid.AddEditComboItem(AIndex: integer; const AValue: string);
begin
  TlqEditComboColumn(TlqEditColumn(Columns[AIndex]).Data).FItems.Add(AValue);
end;

end.

