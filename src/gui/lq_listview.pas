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
      Defines a Listview control.
}

unit lq_listview;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  contnrs,
  AVL_Tree,
  lq_base,
  lq_main,
  lq_widget,
  lq_scrollbar,
  lq_imagelist;
  
type
  TlqListView    = class;
  TlqLVItem      = class;
  TlqLVColumns   = class;
  TlqLVColumn    = class;
  
  TlqLVColumnClickEvent = procedure(Listview: TlqListView; Column: TlqLVColumn; Button: Integer) of object;
  

  TlqLVColumn = class(TComponent)
  private
    FAlignment: TAlignment;
    FCaptionAlignment: TAlignment;
    FDown: Boolean;
    FAutoSize: Boolean;
    FCaption: String;
    FClickable: Boolean;
    FColumnIndex: Integer;
    FColumns: TlqLVColumns;
    FHeight: Integer;
    FResizable: Boolean;
    FVisible: Boolean;
    FWidth: Integer;
    Ref: Integer;
    procedure   SetAlignment(const AValue: TAlignment);
    procedure   SetAutoSize(const AValue: Boolean);
    procedure   SetCaption(const AValue: String);
    procedure   SetCaptionAlignment(const AValue: TAlignment);
    procedure   SetColumnIndex(const AValue: Integer);
    procedure   SetHeight(const AValue: Integer);
    procedure   SetResizable(const AValue: Boolean);
    procedure   SetVisible(const AValue: Boolean);
    procedure   SetWidth(const AValue: Integer);
  public
    constructor Create(AColumns: TlqLVColumns); reintroduce;
    destructor  Destroy; override;
    property    Caption: String read FCaption write SetCaption;
    property    CaptionAlignment: TAlignment read FCaptionAlignment write SetCaptionAlignment;
    property    Alignment: TAlignment read FAlignment write SetAlignment;
    property    AutoSize: Boolean read FAutoSize write SetAutoSize;
    property    Width: Integer read FWidth write SetWidth;
    property    Height: Integer read FHeight write SetHeight;
    property    Visible: Boolean read FVisible write SetVisible;
    property    ColumnIndex: Integer read FColumnIndex write SetColumnIndex;
    property    Clickable: Boolean read FClickable write FClickable;
    property    Resizable: Boolean read FResizable write SetResizable;
  end;
  

  TlqLVColumns = class(TPersistent)
  private
    FListView: TlqListView;
    FColumns: TObjectList;
    function    GetColumn(AIndex: Integer): TlqLVColumn;
    procedure   SetColumn(AIndex: Integer; const AValue: TlqLVColumn);
  public
    constructor Create(AListView: TlqListView);
    destructor  Destroy; override;
    function    Add(AColumn: TlqLVColumn): Integer;
    procedure   Clear;
    procedure   Delete(AIndex: Integer);
    procedure   Insert(AColumn: TlqLVColumn; AIndex: Integer);
    function    Count: Integer;
    property    Column[AIndex: Integer]: TlqLVColumn read GetColumn write SetColumn;
  end;


  TlqLVItemStates = (lisNoState, lisSelected, lisFocused, lisHotTrack);
  TlqLVItemState = set of TlqLVItemStates;
  
  TlqLVItemPaintPart = set of (lvppBackground, lvppIcon, lvppText, lvppFocused);
  
  TlqLVPaintColumnEvent = procedure(ListView: TlqListView; Canvas: TlqCanvas; Column: TlqLVColumn;
                                     ColumnIndex: Integer; Area: TlqRect; var PaintPart: TlqLVItemPaintPart) of object;
  TlqLVPaintItemEvent = procedure(ListView: TlqListView; Canvas: TlqCanvas; Item: TlqLVItem;
                                   ItemIndex: Integer; Area:TlqRect; var PaintPart: TlqLVItemPaintPart) of object;
  TlqLVItemSelectEvent = procedure(ListView: TlqListView; Item: TlqLVItem;
                                    ItemIndex: Integer; Selected: Boolean) of object;
  
  
  IfpgLVItemViewer = interface
    procedure ItemDeleted(AIndex: Integer);
    procedure ItemAdded(AIndex: Integer);
    procedure ItemChanged(AIndex: Integer);
    procedure ItemsUpdated;
  end;
  

  TlqLVItems = class(TObject)
  private
    FUpdateCount: Integer;
    FColumns: TlqLVColumns;
    FCurrentIndexOf: Integer;
    FViewers: TList;
    FItems: TObjectList;
    function    GetCapacity: Integer;
    function    GetItem(AIndex: Integer): TlqLVItem;
    procedure   SetCapacity(const AValue: Integer);
    procedure   SetItem(AIndex: Integer; const AValue: TlqLVItem);
    procedure   AddViewer(AValue: IfpgLVItemViewer);
    procedure   DeleteViewer(AValue: IfpgLVItemViewer);
    // interface method triggers
    procedure   DoChange(AItem: TlqLVItem);
    procedure   DoAdd(AItem: TlqLVItem);
    procedure   DoDelete(AItem: TlqLVItem);
    procedure   DoEndUpdate;
  public
    constructor Create(AViewer: IfpgLVItemViewer);
    destructor  Destroy; override;
    function    Add(AItem: TlqLVItem): Integer;
    function    Count: Integer;
    procedure   Clear;
    procedure   Delete(AIndex: Integer);
    function    IndexOf(AItem: TlqLVItem): Integer;
    procedure   InsertItem(AItem: TlqLVItem; AIndex: Integer);
    procedure   BeginUpdate;
    procedure   EndUpdate;
    procedure   Sort(Compare: TListSortCompare);
    property    Capacity: Integer read GetCapacity write SetCapacity;
    property    Columns: TlqLVColumns read FColumns;
    property    Item[AIndex: Integer]: TlqLVItem read GetItem write SetItem;
  end;
  
  
  TlqLVItem = class(TObject)
  private
    FCaption: String;
    FImageIndex: Integer;
    FItems: TlqLVItems;
    FSubItems: TStrings;
    FUserData: Pointer;
    function    GetSelected(ListView: TlqListView): Boolean;
    function    GetSubItems: TStrings;
    procedure   SetCaption(const AValue: String);
    procedure   SetImageIndex(const AValue: Integer);
    procedure   SetSelected(ListView: TlqListView; const AValue: Boolean);
    procedure   SubItemsChanged(Sender: TObject);
    function    SubItemCount: Integer;
  public
    constructor Create(Items: TlqLVItems); virtual;
    destructor  Destroy; override;
    property    Caption: String read FCaption write SetCaption;
    property    UserData: Pointer read FUserData write FUserData;
    property    ImageIndex: Integer read FImageIndex write SetImageIndex default -1;
    property    SubItems: TStrings read GetSubItems;
    property    Selected[ListView: TlqListView]: Boolean read GetSelected write SetSelected;
  end;


  TlqListViewSubItems = class(TStrings)
  private
    FList: TFPList;
    FOnChange: TNotifyEvent;
    function GetImageIndex(ASubIndex: Integer): Integer;
    procedure SetImageIndex(ASubIndex: Integer; const AValue: Integer);
    procedure DoChange;
  protected
    function    GetObject(Index: Integer): TObject; override;
    function    Get(Index: Integer): string; override;
    function    GetCount: Integer; override;
    procedure   Put(Index: Integer; const S: string); override;
    procedure   PutObject(Index: Integer; AObject: TObject); override;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   Delete(Index: Integer); override;
    procedure   Clear; override;
    procedure   Insert(Index: Integer; const S: string); override;
    property    ImageIndex[ASubIndex: Integer]: Integer read GetImageIndex write SetImageIndex;
    property    OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;
  

  { TlqListView }

  TlqListView = class(TlqWidget, IfpgLVItemViewer)
  private
    procedure SetShiftIsPressed(const AValue: Boolean);
  private
    FImages: array[TlqLVItemStates] of TlqImageList;
    FSubitemImages: array[TlqLVItemStates] of TlqImageList;
    FItemIndex: Integer;
    FMultiSelect: Boolean;
    FOnPaintColumn: TlqLVPaintColumnEvent;
    FOnSelectionChanged: TlqLVItemSelectEvent;
    FSelectionFollowsFocus: Boolean;
    FSelectionShiftStart: Integer;
    FOnColumnClick: TlqLVColumnClickEvent;
    FSelected: TAVLTree;
    FOldSelected: TAVLTree;
    FUpdateCount: Integer;
    FVScrollBar: TlqScrollBar;
    FHScrollBar: TlqScrollBar;
    FColumns: TlqLVColumns;
    FItems: TlqLVItems;
    FOnPaintItem: TlqLVPaintItemEvent;
    FShowHeaders: Boolean;
    FResizingColumn: TlqLVColumn;
    FMouseDownPoint: TPoint;
    FScrollBarNeedsUpdate: Boolean;
    FShiftIsPressed: Boolean;
    function    HasImages: Boolean;
    function    GetImages(AIndex: integer): TlqImageList;
    function    GetItemHeight: Integer;
    procedure   SetImages(AIndex: integer; const AValue: TlqImageList);
    procedure   SetItemIndex(const AValue: Integer);
    procedure   SetItems(const AValue: TlqLVItems);
    procedure   SetMultiSelect(const AValue: Boolean);
    procedure   SetOnColumnClick(const AValue: TlqLVColumnClickEvent);
    procedure   SetShowHeaders(const AValue: Boolean);
    function    SubItemGetImages(AIndex: integer): TlqImageList;
    procedure   SubItemSetImages(AIndex: integer; const AValue: TlqImageList);
    procedure   VScrollChange(Sender: TObject; Position: Integer);
    procedure   HScrollChange(Sender: TObject; Position: Integer);
    function    FindImageForState(AItemIndex: Integer;  AColumnIndex: Integer; AState: TlqLVItemState): TlqImage;
    // interface methods
    procedure   ItemDeleted(AIndex: Integer);
    procedure   ItemAdded(AIndex: Integer);
    procedure   ItemChanged(AIndex: Integer);
    procedure   ItemsUpdated;
    //
    function    GetVisibleColumnsWidth: Integer;
    function    GetItemAreaHeight: Integer;
    procedure   SelectionSetRangeEnabled(AStart, AEnd: Integer; AValue: Boolean);
    procedure   SelectionToggleRange(AStart, AEnd: Integer; const ShiftState: TShiftState; IgnoreStartIndex: Boolean);
    procedure   SelectionClear;
    function    ItemGetSelected(const AItem: TlqLVItem): Boolean;
    procedure   ItemSetSelected(const AItem: TlqLVItem; const AValue: Boolean);
    function    ItemGetFromPoint(const X, Y: Integer): TlqLVItem;
    function    ItemGetRect(AIndex: Integer): TlqRect;
    function    ItemIndexFromY(Y: Integer): Integer;
    function    HeaderHeight: Integer;
    procedure   DoRepaint;
    procedure   DoColumnClick(Column: TlqLVColumn; Button: Integer);
    procedure   HandleHeaderMouseMove(x, y: Integer; btnstate: word; Shiftstate: TShiftState);
    property    ShiftIsPressed: Boolean read FShiftIsPressed write SetShiftIsPressed;
  protected
    procedure   MsgPaint(var msg: TlqMessageRec); message LQM_PAINT;
    procedure   HandleMouseScroll(x, y: integer; shiftstate: TShiftState; delta: smallint); override;
    procedure   HandleLMouseDown(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleRMouseDown(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleLMouseUp(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleRMouseUp(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleMouseMove(x, y: integer; btnstate: word; shiftstate: TShiftState); override;
    procedure   HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); override;
    procedure   HandleKeyRelease(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); override;
    procedure   HandlePaint; override;
    procedure   HandleResize(awidth, aheight: TlqCoord); override;
    procedure   PaintHeaders; virtual;
    procedure   PaintItems; virtual;
    procedure   UpdateScrollBarPositions; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    function    GetClientRect: TlqRect; override;
    procedure   BeginUpdate;
    procedure   EndUpdate;
    procedure   MakeItemVisible(AIndex: Integer; PartialOK: Boolean = False);
    function    ItemAdd: TlqLVItem; deprecated;
    function    AddItem: TlqLVItem;
    function    NewItem: TlqLVItem;
  published
    property    Align;
    property    Columns: TlqLVColumns read FColumns;
    property    Enabled;
    property    HScrollBar: TlqScrollBar read FHScrollBar;
    property    Images: TlqImageList index Ord(lisNoState) read GetImages write SetImages;
    property    ImagesSelected: TlqImageList index Ord(lisSelected) read GetImages write SetImages;
    property    ImagesFocused: TlqImageList index Ord(lisFocused) read GetImages write SetImages;
    property    ImagesHotTrack: TlqImageList index Ord(lisHotTrack) read GetImages write SetImages;
    property    ItemHeight: Integer read GetItemHeight;
    property    ItemIndex: Integer read FItemIndex write SetItemIndex;
    property    Items: TlqLVItems read FItems write SetItems;
    property    Hint;
    property    MultiSelect: Boolean read FMultiSelect write SetMultiSelect;
    property    ParentShowHint;
    property    SelectionFollowsFocus: Boolean read FSelectionFollowsFocus write FSelectionFollowsFocus;
    property    SubItemImages: TlqImageList index Ord(lisNoState) read SubItemGetImages write SubItemSetImages;
    property    SubItemImagesSelected: TlqImageList index Ord(lisSelected) read SubItemGetImages write SubItemSetImages;
    property    SubItemImagesFocused: TlqImageList index Ord(lisFocused) read SubItemGetImages write SubItemSetImages;
    property    SubItemImagesHotTrack: TlqImageList index Ord(lisHotTrack) read SubItemGetImages write SubItemSetImages;

    property    ShowHeaders: Boolean read FShowHeaders write SetShowHeaders;
    property    ShowHint;
    property    TabOrder;
    property    VScrollBar: TlqScrollBar read FVScrollBar;
    property    OnColumnClick: TlqLVColumnClickEvent read FOnColumnClick write SetOnColumnClick;
    property    OnPaintColumn: TlqLVPaintColumnEvent read FOnPaintColumn write FOnPaintColumn;
    property    OnPaintItem: TlqLVPaintItemEvent read FOnPaintItem write FOnPaintItem;
    property    OnSelectionChanged: TlqLVItemSelectEvent read FOnSelectionChanged write FOnSelectionChanged;
    property    OnShowHint;
  end;
  
  
implementation

uses
  lq_constants;
  

type
  // used to access protected methods
  TlqScrollbarFriend = class(TlqScrollbar)
  end;

  PfpgLVSubitemRec = ^TlqLVSubitemRec;
  TlqLVSubitemRec = record
    AText: String;
    AObject: TObject;
    AImageIndex: Integer;
  end;


procedure CopyAVLTree(From, To_: TAVLTree; Clear: Boolean = False);
            procedure AddNodeNodes(ANode: TAVLTreeNode);
            begin
              if ANode = nil then
                Exit; // ==>
              To_.Add(ANode.Data);
              AddNodeNodes(ANode.Left);
              AddNodeNodes(ANode.Right);
            end;
begin
  if Clear then
    To_.Clear;
  AddNodeNodes(From.Root);
end;

{ TlqLVItems }

function Min(AInt, BInt: Integer): Integer;
begin
  if AInt < Bint then
    Result := AInt
  else Result := BInt;
end;

function Max(AInt, BInt: Integer): INteger;
begin
  if AInt > Bint then
    Result := AInt
  else Result := BInt;
end;

function TlqLVItems.GetItem(AIndex: Integer): TlqLVItem;
begin
  Result := TlqLVItem(FItems.Items[AIndex]);
end;

function TlqLVItems.GetCapacity: Integer;
begin
  Result := FItems.Capacity;
end;

procedure TlqLVItems.SetCapacity(const AValue: Integer);
begin
  FItems.Capacity := AValue;
end;

procedure TlqLVItems.SetItem(AIndex: Integer; const AValue: TlqLVItem);
begin
  FItems.Items[AIndex] := AValue;
end;

procedure TlqLVItems.AddViewer(AValue: IfpgLVItemViewer);
begin
  if AValue <> nil then
    FViewers.Add(AValue);
end;

procedure TlqLVItems.DeleteViewer(AValue: IfpgLVItemViewer);
var
  AIndex: Integer;
begin
  AIndex := FViewers.IndexOf(AValue);
  if AIndex > -1 then
  begin
    FViewers.Delete(AIndex);
  end;
  if FViewers.Count = 0 then
    Free;
end;

procedure TlqLVItems.DoChange(AItem: TlqLVItem);
var
  I: Integer;
  AIndex: Integer;
begin
  if FUpdateCount > 0 then
    Exit;
  AIndex := IndexOf(AItem);
  for I := 0 to FViewers.Count -1 do
  begin
    IfpgLVItemViewer(FViewers.Items[I]).ItemChanged(AIndex);
  end;
end;

procedure TlqLVItems.DoAdd(AItem: TlqLVItem);
var
  I: Integer;
  AIndex: Integer;
begin
  if FUpdateCount > 0 then
    Exit;
  AIndex := IndexOf(AItem);
  for I := 0 to FViewers.Count -1 do
  begin
    IfpgLVItemViewer(FViewers.Items[I]).ItemAdded(AIndex);
  end;
end;

procedure TlqLVItems.DoDelete(AItem: TlqLVItem);
var
  I: Integer;
  AIndex: Integer;
begin
  if FUpdateCount > 0 then
    Exit;
  AIndex := IndexOf(AItem);
  for I := 0 to FViewers.Count -1 do
  begin
    IfpgLVItemViewer(FViewers.Items[I]).ItemDeleted(AIndex);
  end;
end;

procedure TlqLVItems.DoEndUpdate;
var
  I: Integer;
begin
  if FUpdateCount > 0 then
    Exit;
  for I := 0 to FViewers.Count -1 do
  begin
    IfpgLVItemViewer(FViewers.Items[I]).ItemsUpdated;
  end;
end;

constructor TlqLVItems.Create(AViewer: IfpgLVItemViewer);
begin
  FItems := TObjectList.Create;
  FViewers := TList.Create;
  AddViewer(AViewer);
end;

destructor TlqLVItems.Destroy;
begin
  FItems.Free;
  FViewers.Free;
  inherited Destroy;
end;

function TlqLVItems.Add(AItem: TlqLVItem): Integer;
begin
  Result := Count;
  InsertItem(AItem, Count);
  DoAdd(AItem);
end;

function TlqLVItems.Count: Integer;
begin
  Result := FItems.Count;
end;

procedure TlqLVItems.Clear;
var
  i: integer;
begin
  for i :=  FItems.Count-1 downto 0 do
    Delete(i);
  FItems.Clear;
end;

procedure TlqLVItems.Delete(AIndex: Integer);
begin
  DoDelete(GetItem(AIndex));
  FItems.Delete(AIndex);
end;

function TlqLVItems.IndexOf(AItem: TlqLVItem): Integer;
begin
  Result := -1;
  // this checks for a index close to the old one whic can speed up
  // search significantly when we are using indexof in a for loop
  if (FCurrentIndexOf > 100) and (FCurrentIndexOf < Count-2) then
  begin
    if FItems.Items[FCurrentIndexOf] = AItem then
      Result := FCurrentIndexOf
    else if FItems.Items[FCurrentIndexOf+1] = AItem then
      Result := FCurrentIndexOf+1
    else if FItems.Items[FCurrentIndexOf-1] = AItem then
      Result := FCurrentIndexOf-1
  end;
  if Result = -1 then
    Result := FItems.IndexOf(AItem);
  FCurrentIndexOf := Result;
end;

procedure TlqLVItems.InsertItem(AItem: TlqLVItem; AIndex: Integer);
begin
  if AItem.InheritsFrom(TlqLVItem) then
    FItems.Insert(AIndex, AItem)
  else
    raise Exception.CreateFmt(rsErrItemOfWrongType, ['TlqLVItem']);
end;

procedure TlqLVItems.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TlqLVItems.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount < 0 then
    FUpdateCount := 0;
  if FUpdateCount = 0 then
    DoEndUpdate;
end;

procedure TlqLVItems.Sort(Compare: TListSortCompare);
begin
  BeginUpdate;
  FItems.Sort(Compare);
  EndUpdate;
end;

{ TlqLVItem }

procedure TlqLVItem.SetCaption(const AValue: String);
begin
  if FCaption=AValue then
    Exit;
  FCaption:=AValue;
  if Assigned(FItems) then
    FItems.DoChange(Self);
end;

procedure TlqLVItem.SetImageIndex(const AValue: Integer);
begin
  if FImageIndex=AValue then exit;
  FImageIndex:=AValue;
  if Assigned(FItems) then
    FItems.DoChange(Self);
end;

function TlqLVItem.GetSelected(ListView: TlqListView): Boolean;
begin
  Result := ListView.ItemGetSelected(Self);
end;

function TlqLVItem.GetSubItems: TStrings;
begin
  if FSubItems = nil then
  begin
    FSubItems := TlqListViewSubitems.Create;
    TlqListViewSubitems(FSubItems).OnChange := @SubItemsChanged;
  end;
  Result := FSubItems;
end;

procedure TlqLVItem.SetSelected(ListView: TlqListView; const AValue: Boolean);
begin
  ListView.ItemSetSelected(Self, AValue);
end;

procedure TlqLVItem.SubItemsChanged(Sender: TObject);
begin
  if Assigned(FItems) then
    FItems.DoChange(Self);
end;

function TlqLVItem.SubItemCount: Integer;
begin
  Result := 0;
  if FSubItems = nil then
    Exit;
  Result := FSubItems.Count;
end;

constructor TlqLVItem.Create(Items: TlqLVItems);
begin
  FItems := Items;
  ImageIndex := -1;
end;

destructor TlqLVItem.Destroy;
begin
  if Assigned(FSubItems) then
    FSubItems.Free;
  inherited Destroy;
end;

{ TlqListView }

procedure TlqListView.SetShowHeaders(const AValue: Boolean);
begin
  if FShowHeaders=AValue then
    Exit;
  FShowHeaders:=AValue;
  DoRePaint;
end;

function TlqListView.SubItemGetImages(AIndex: integer): TlqImageList;
begin
  Result :=  FSubItemImages[TlqLVItemStates(AIndex)];
end;

procedure TlqListView.SubItemSetImages(AIndex: integer;
  const AValue: TlqImageList);
begin
  if AValue = FSubItemImages[TlqLVItemStates(AIndex)] then
    Exit; // ==>

  FSubItemImages[TlqLVItemStates(AIndex)] := AValue;
  if HasHandle then
    Invalidate;
end;
  

procedure TlqListView.VScrollChange(Sender: TObject; Position: Integer);
begin
  DoRepaint;
end;

procedure TlqListView.HScrollChange(Sender: TObject; Position: Integer);
begin
  DoRepaint;
end;

function TlqListView.FindImageForState(AItemIndex: Integer; AColumnIndex: Integer; AState: TlqLVItemState): TlqImage;
var
  PreferredState: TlqLVItemStates = lisHotTrack;
  State: TlqLVItemStates;
  Item: TlqLVItem;
  ImgList: TlqImageList;
  ImgIndex: Integer;
  ImagesArray: Array[TlqLVItemStates] of TlqImageList;
begin
  Result := nil;
  Item := Items.Item[AItemIndex];

  ImgIndex:=-1;;
  if AColumnIndex = 0 then
  begin
    ImagesArray := FImages;
    ImgIndex := Item.ImageIndex;
  end
  else
  begin
    ImagesArray := FSubItemImages;
    if (Item.SubItemCount > 0) and (AColumnIndex <= Item.SubItemCount) then
      ImgIndex := TlqListViewSubItems(Item.SubItems).ImageIndex[AColumnIndex-1];
  end;

  // later we will make the state preference order configurable
  while (not (PreferredState in AState)) and (PreferredState <> lisNoState) do
    Dec(PreferredState);

  State := lisNoState; //to remove compiler warning
  for State := PreferredState downto lisNoState do
  begin
    if ImagesArray[State] = nil then
      continue;
    ImgList := ImagesArray[State];

    if (ImgIndex <> -1) and (ImgIndex < ImgList.Count) then
    begin
      Result := ImgList.Items[ImgIndex].Image;
    end
    else
    begin
      if AColumnIndex = 0 then
        ImgIndex := AItemIndex
      else
        ImgIndex := AColumnIndex-1;
      if ImgIndex < ImgList.Count then
        Result := ImgList.Items[ImgIndex].Image;
    end;
    break;
  end;

end;

procedure TlqListView.SetItems(const AValue: TlqLVItems);
begin
  if AValue = FItems then
    Exit;
  AValue.AddViewer(Self);
  FItems.DeleteViewer(Self);
  Fitems := AValue;
end;

procedure TlqListView.SetMultiSelect(const AValue: Boolean);
begin
  if FMultiSelect=AValue then
    Exit;
  FMultiSelect:=AValue;
end;

procedure TlqListView.SetOnColumnClick(const AValue: TlqLVColumnClickEvent);
begin
  if FOnColumnClick=AValue then
    Exit;
  FOnColumnClick:=AValue;
end;

procedure TlqListView.SetShiftIsPressed(const AValue: Boolean);
begin
  if AValue = FShiftIsPressed then
    Exit;
  FShiftIsPressed:=AValue;
  if AValue then
  begin
    if FItems.Count = 0 then
      Exit;
    FSelectionShiftStart := FItemIndex;
    // ensure start index is at least 0
    if FSelectionShiftStart = -1 then
      Inc(FSelectionShiftStart);
    CopyAVLTree(FSelected, FOldSelected, True);
  end
  else
  begin
    FSelectionShiftStart := -1;
    FOldSelected.Clear;
  end;
end;

function TlqListView.HasImages: Boolean;
var
  State: TlqLVItemStates;
begin
  Result := False;
  for State := lisNoState to lisHotTrack do
    if FImages[State] <> nil then
      Exit(True);

end;

function TlqListView.GetItemHeight: Integer;
begin
  Result := Canvas.Font.Height + 4;
end;

function TlqListView.GetImages(AIndex: integer): TlqImageList;
begin
  Result :=  FImages[TlqLVItemStates(AIndex)];
end;

procedure TlqListView.SetImages(AIndex: integer; const AValue: TlqImageList);
begin
  if AValue = FImages[TlqLVItemStates(AIndex)] then
    Exit; // ==>

  FImages[TlqLVItemStates(AIndex)] := AValue;
  if HasHandle then
    Invalidate;
end;


procedure TlqListView.SetItemIndex(const AValue: Integer);
begin
  if FItemIndex=AValue then
    Exit;
  if (AValue >= -1) and (AValue < FItems.Count) then
    FItemIndex:=AValue;
end;

procedure TlqListView.ItemDeleted(AIndex: Integer);
begin
  if FUpdateCount = 0 then
    DoRePaint;
end;

procedure TlqListView.ItemAdded(AIndex: Integer);
begin
  if FUpdateCount = 0 then
    DoRePaint;
end;

procedure TlqListView.ItemChanged(AIndex: Integer);
begin
  if FUpdateCount = 0 then
    DoRePaint;
end;

procedure TlqListView.ItemsUpdated;
begin
  DoRepaint;
end;

function TlqListView.GetClientRect: TlqRect;
begin
  Result.Top := 2;
  Result.Left := 2;
  Result.SetRight(Width - 2);
  Result.SetBottom(Height - 2);
end;

function TlqListView.GetVisibleColumnsWidth: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FColumns.Count-1 do
    if FColumns.Column[I].Visible then
      Inc(Result, FColumns.Column[I].Width);
end;

function TlqListView.GetItemAreaHeight: Integer;
begin
  Result := Height - 4;
  if ShowHeaders then
    Dec(Result, HeaderHeight);
  if FHScrollBar.Visible then
    Dec(Result,FHScrollBar.Height);
end;


procedure TlqListView.SelectionSetRangeEnabled(AStart, AEnd: Integer; AValue: Boolean);
var
  TmpI: LongInt;
  I: LongInt;
begin
  if AStart > AEnd then
  begin
    TmpI := AStart;
    AStart := AEnd;
    AEnd := TmpI;
  end;
  CopyAVLTree(FOldSelected, FSelected, True);
  if (AStart < 0) or (AEnd > FItems.Count-1) then
    Exit;
  for I := AStart to AEnd do
  begin
    if I <> FSelectionShiftStart then
      ItemSetSelected(FItems.Item[I], AValue);
  end;
end;

procedure TlqListView.SelectionToggleRange(AStart, AEnd: Integer;
  const ShiftState: TShiftState; IgnoreStartIndex: Boolean);
var
 TmpI: Integer;
 I: LongInt;
begin
  TmpI := AStart;
  if AStart > AEnd then
  begin
    AStart := AEnd;
    AEnd := TmpI;
  end;
  if not FMultiSelect then
  begin
    SelectionClear;
    ItemSetSelected(FItems.Item[TmpI], True);
    Exit;
  end;
  if ssShift in ShiftState then
    for I := AStart to AEnd do
    begin
      if not(IgnoreStartIndex and (I = TmpI))
      then ItemSetSelected(FItems.Item[I], not ItemGetSelected(FItems.Item[I]));
    end;
end;

procedure TlqListView.SelectionClear;
var
  Item: TlqLVItem;
begin
  while FSelected.Root <> nil do
  begin
    Item := TlqLVItem(FSelected.Root.Data);
    FSelected.Delete(FSelected.Root);
    if Assigned(FOnSelectionChanged) then
      FOnSelectionChanged(Self, Item, Items.IndexOf(Item), False);
  end;
end;


function TlqListView.ItemGetSelected(const AItem: TlqLVItem): Boolean;
begin
  Result := FSelected.Find(Pointer(AItem)) <> nil;
end;

procedure TlqListView.ItemSetSelected(const AItem: TlqLVItem; const AValue: Boolean);
var
  Node: TAVLTreeNode;
  Changed: Boolean;
begin

  Changed := False;
  Node := FSelected.Find(AItem);

  if AValue then
  begin
    if Node = nil then
    begin
      FSelected.Add(Pointer(AItem));
      Changed := True;
    end;
  end
  else
  begin
    if Node <> nil then
    begin
      FSelected.Delete(Node);
      Changed := True;
    end;
  end;
  
  if Changed and Assigned(FOnSelectionChanged) then
    FOnSelectionChanged(Self, AItem, Items.IndexOf(AItem), AValue);
end;

function TlqListView.ItemGetFromPoint(const X, Y: Integer): TlqLVItem;
var
  Index: Integer;
  ItemTop: Integer;
begin
  Result := nil;
  ItemTop := (FVScrollBar.Position + Y) -2;
  if ShowHeaders then
    Dec(ItemTop, HeaderHeight);
  Index := ItemTop div ItemHeight;
  if Index < 0 then
    Exit;
  if Index >= FItems.Count then
    Exit;
  if FHScrollBar.Position - 2 + X > GetVisibleColumnsWidth then
    Exit;

  Result := FItems.Item[Index];
end;

function TlqListView.ItemGetRect(AIndex: Integer): TlqRect;
begin
  Result.Top := 2 + (AIndex * ItemHeight) - FVScrollBar.Position;
  if ShowHeaders then
    Inc(Result.Top, HeaderHeight);
  Result.Height := ItemHeight;
  Result.Left := 2 - FHScrollBar.Position;
  Result.Width := GetVisibleColumnsWidth;
end;

function TlqListView.ItemIndexFromY(Y: Integer): Integer;
var
  TopPos: Integer;
begin
  if ShowHeaders and (Y < HeaderHeight) then
    Exit(-1);

  TopPos := (FVScrollBar.Position + Y) - 2;
  if ShowHeaders then
    Dec(TopPos, HeaderHeight);
  Result := TopPos div ItemHeight;
  if Result > Fitems.Count-1 then
    Result := -1;
end;

function TlqListView.HeaderHeight: Integer;
begin
  Result := Canvas.Font.Height + 10;
end;

procedure TlqListView.DoRepaint;
begin
  if FUpdateCount = 0 then
    RePaint;
end;

procedure TlqListView.DoColumnClick(Column: TlqLVColumn; Button: Integer);
begin
  if not Column.Clickable then
    Exit;
  if Assigned(FOnColumnClick) then
    FOnColumnClick(Self, Column, Button);

  Column.FDown := True;
  Repaint;
end;

procedure TlqListView.HandleHeaderMouseMove(x, y: Integer; btnstate: word;
  Shiftstate: TShiftState);
var
  I: Integer;
  curLeft: Integer;
  curRight: Integer;
  Column: TlqLVColumn;
  LastColumn: TlqLVColumn;
  HeaderX: Integer; // this is X from the headers point of view
  NewMouseCursor: TMouseCursor;
begin
  curLeft := 0;

  HeaderX := FHScrollBar.Position - 2 + X;
  NewMouseCursor := MouseCursor;
  LastColumn := nil;
  for I := 0 to FColumns.Count-1 do
  begin
    Column := FColumns.Column[I];
    if not Column.Visible then
      Continue;
    curRight := curLeft + Column.Width-1;
    if Column.Resizable or (Assigned(LastColumn) and LastColumn.Resizable) then
    begin
      if (FResizingColumn <> nil) and (FResizingColumn = Column) then
      begin
        FResizingColumn.Width :=  (x + FHScrollBar.Position)- curLeft;
        DoRepaint;
        Break;
      end
      else begin
        if (HeaderX >= curLeft) and (HeaderX <= curRight) then // we are within this columns space
        begin
          if ((LastColumn <> nil) and (LastColumn.Resizable) and (HeaderX - curLeft < 5))
          or (Column.Resizable) and (curRight - HeaderX < 5)
          then
          begin
            NewMouseCursor := mcSizeEW;
            Break;
          end;
        end
        else
          NewMouseCursor := mcDefault;
      end;
    end;
    LastColumn := Column;
    Inc(curLeft, Column.Width);
  end;
  if not Assigned(FResizingColumn) and Assigned(LastColumn) and LastColumn.Resizable then
    if (HeaderX - curLeft < 5) and (HeaderX - curLeft >= 0) then
      NewMouseCursor := mcSizeEW;
      
  if FResizingColumn <> nil then
    NewMouseCursor := mcSizeEW;
  
  if NewMouseCursor <> MouseCursor then
    MouseCursor := NewMouseCursor;

end;

procedure TlqListView.MsgPaint(var msg: TlqMessageRec);
begin
  // Optimises painting and prevents Begin[End]Draw and OnPaint event firing
  // in not needed.
  if FUpdateCount = 0 then
    inherited MsgPaint(msg);
end;

procedure TlqListView.HandleMouseScroll(x, y: integer;
  shiftstate: TShiftState; delta: smallint);
var
  cRect: TlqRect;
begin
  cRect := GetClientRect;
  if FShowHeaders then
    Inc(cRect.Top, HeaderHeight);
  if FHScrollBar.Visible then
    Dec(cRect.Height, FHScrollBar.Height);
  if FVScrollBar.Visible then
    Dec(cRect.Width,  FVScrollBar.Width);


  if not PtInRect(cRect, Point(X,Y)) then
    Exit;

  TlqScrollbarFriend(FVScrollBar).HandleMouseScroll(x, y, shiftstate, delta);
end;

procedure TlqListView.HandleLMouseDown(x, y: integer; shiftstate: TShiftState);
var
  Item: TlqLVItem;
  cRect: TlqRect;
  curLeft, curRight: Integer;
  I: Integer;
  Column: TlqLVColumn;
  LastColumn: TlqLVColumn;
  HeaderX: Integer;
begin
  inherited HandleLMouseDown(x, y, shiftstate);
  ShiftIsPressed := ssShift in shiftstate;
  cRect := GetClientRect;
  
  FMouseDownPoint := Point(X,Y);
  
  if not PtInRect(cRect, Point(X,Y)) then
    Exit;
  
  if FShowHeaders then
  begin
    if (Y < HeaderHeight + cRect.Top)  then
    begin
      LastColumn := nil;
      HeaderX := FHScrollBar.Position - 2 + x;

      curLeft := 0;
      for I := 0 to FColumns.Count-1 do
      begin
        Column := FColumns.Column[I];
        if Column.Visible then
        begin
          curRight := curLeft + Column.Width-1;
          if (HeaderX <= curRight) and (HeaderX >= curLeft) then
          begin
            if (MouseCursor = mcSizeEW) then
            begin
              if Column.Resizable and (curRight - HeaderX < 5) then
                FResizingColumn := Column
              else
                if Assigned(LastColumn) and LastColumn.Resizable and (HeaderX - curLeft < 5) then
                  FResizingColumn := LastColumn
            end
            else // only perform a mouse click if we aren't resizing
              DoColumnClick(Column, 1);
          end;
          Inc(curLeft, Column.Width);
        end;
        LastColumn := Column;
      end;
      if not Assigned(FResizingColumn) and Assigned(LastColumn) and LastColumn.Resizable then
        if (HeaderX - curLeft < 5) and (HeaderX - curLeft >= 0) then
          FResizingColumn := LastColumn;
    end;

    Inc(cRect.Top, HeaderHeight);
  end;
  
  if FHScrollBar.Visible then
    Dec(cRect.Height, FHScrollBar.Height);
  if FVScrollBar.Visible then
    Dec(cRect.Width,  FVScrollBar.Width);
  
  if not PtInRect(cRect, Point(X,Y)) then
    Exit;

  Item := ItemGetFromPoint(X, Y);
  if not FMultiSelect then
    SelectionClear;
  if Item <> nil then
  begin
    FItemIndex := ItemIndexFromY(Y);
    MakeItemVisible(FItemIndex);
    if FMultiSelect then
    begin
      if not ((ssCtrl in shiftstate) or (ssShift in shiftstate)) then
      begin
        SelectionClear;
        ItemSetSelected(Item, True);
      end
      else begin
        if ssCtrl in shiftstate then
          ItemSetSelected(Item,  not ItemGetSelected(Item));
        if ssShift in shiftstate then
          SelectionSetRangeEnabled(FSelectionShiftStart, FItemIndex, True);
      end
    end
    else ItemSetSelected(Item, True);
  end;
  DoRepaint;
end;

procedure TlqListView.HandleRMouseDown(x, y: integer; shiftstate: TShiftState);
var
  I: Integer;
  cLeft, cRight: Integer;
  cRect: TlqRect;
  Column: TlqLVColumn;
begin
  inherited HandleRMouseDown(x, y, shiftstate);
  ShiftIsPressed := ssShift in shiftstate;
  cRect := GetClientRect;

  if not PtInRect(cRect, Point(X,Y)) then
    Exit;

  if FShowHeaders then
  begin
    if (Y < HeaderHeight + cRect.Top) then
    begin
      cLeft := cRect.Left - FHScrollBar.Position;
      for I := 0 to FColumns.Count-1 do
      begin
        Column := FColumns.Column[I];
        if Column.Visible then
        begin
          cRight := cLeft + Column.Width-1;
          if (X <= cRight) and (X >= cLeft) then
            DoColumnClick(Column, 3);
          Inc(cLeft, Column.Width);
        end;
      end;
    end;
    Inc(cRect.Top, HeaderHeight);
  end;
  
  if FVScrollBar.Visible then
    Dec(cRect.Width, FVScrollBar.Width);
  if FHScrollBar.Visible then
    Dec(cRect.Height, FHScrollBar.Height);
end;

procedure TlqListView.HandleLMouseUp(x, y: integer; shiftstate: TShiftState);
var
  I: Integer;
begin
  inherited HandleLMouseUp(x, y, shiftstate);
  for I := 0 to FColumns.Count-1 do
  begin
    FColumns.Column[I].FDown := False;
  end;
  
  FResizingColumn := nil;
  DoRepaint;
end;

procedure TlqListView.HandleRMouseUp(x, y: integer; shiftstate: TShiftState);
var
  I: Integer;
begin
  inherited HandleRMouseUp(x, y, shiftstate);
  for I := 0 to FColumns.Count-1 do
  begin
    FColumns.Column[I].FDown := False;
  end;
  DoRepaint;
end;

procedure TlqListView.HandleMouseMove(x, y: integer; btnstate: word;
  shiftstate: TShiftState);
var
  cRect: TlqRect;
begin
  inherited HandleMouseMove(x, y, btnstate, shiftstate);

  cRect := GetClientRect;

  if not PtInRect(cRect, Point(X,Y)) and (FResizingColumn = nil) then
    Exit;

  if ((Y < (cRect.Top + HeaderHeight)) and ShowHeaders) or (FResizingColumn <> nil) then
  begin
    HandleHeaderMouseMove(x, y, btnstate, shiftstate);
  end
  else
    if (MouseCursor <> mcDefault) and (FResizingColumn = nil) then
      MouseCursor := mcDefault;
  
  //if FVScrollBar.Visible then Dec(cRect.Width, FVScrollBar.Width);
  //if FHScrollBar.Visible then Dec(cRect.Height, FHScrollBar.Height);
end;

procedure TlqListView.HandleKeyPress(var keycode: word;
  var shiftstate: TShiftState; var consumed: boolean);
var
  iIndex: Integer;
  OldIndex: Integer;
  procedure CheckMultiSelect;
  begin
    if FMultiSelect then begin
      if (ssShift in shiftstate) or (FSelectionShiftStart > -1) then
      begin
        SelectionSetRangeEnabled(FSelectionShiftStart, FItemIndex, True);
      end
      else if ssCtrl in shiftstate then
      begin
        SelectionToggleRange(FItemIndex, FItemIndex, shiftstate, False);
      end;
    end;
  end;
  procedure CheckSelectionFocus;
  begin
    if ((ssShift in shiftstate) or (ssCtrl in shiftstate)) then
      Exit;
    SelectionClear;
    if FSelectionFollowsFocus and (FItemIndex > -1) then
      ItemSetSelected(FItems.Item[FItemIndex], True);
  end;
begin
  consumed := True;
  OldIndex := FItemIndex;
  ShiftIsPressed := ssShift in shiftstate;
  //WriteLn('Got key: ',IntToHex(keycode, 4));
  case keycode of
    keyUp:
    begin
      if ItemIndex > 0 then
        ItemIndex := ItemIndex-1;
      MakeItemVisible(ItemIndex);
      if OldIndex <> ItemIndex then
        CheckSelectionFocus;
      CheckMultiSelect;
    end;
    keyDown:
    begin
      ItemIndex := ItemIndex+1;
      MakeItemVisible(ItemIndex);
      if OldIndex <> ItemIndex then
        CheckSelectionFocus;
      CheckMultiSelect;
    end;
    keyLeft:
    begin
      FHScrollBar.Position := FHScrollBar.Position - FHScrollBar.ScrollStep;
    end;
    keyRight:
    begin
      FHScrollBar.Position := FHScrollBar.Position + FHScrollBar.ScrollStep;
    end;
    keyHome:
    begin
      ItemIndex := 0;
      MakeItemVisible(ItemIndex);
      if OldIndex <> ItemIndex then
        CheckSelectionFocus;
      CheckMultiSelect;
    end;
    keyEnd:
    begin
      ItemIndex := FItems.Count-1;
      MakeItemVisible(ItemIndex);
      if OldIndex <> ItemIndex then
        CheckSelectionFocus;
      CheckMultiSelect;
    end;
    keyPageUp:
    begin
      iIndex := ItemIndex - (GetItemAreaHeight div ItemHeight);
      if iIndex < 0 then
        iIndex := 0;
      ItemIndex := iIndex;
      MakeItemVisible(ItemIndex);
      if OldIndex <> ItemIndex then
        CheckSelectionFocus;
      CheckMultiSelect;
    end;
    keyPageDown:
    begin
      iIndex := ItemIndex + (GetItemAreaHeight div ItemHeight);
      if iIndex > FItems.Count-1 then
        iIndex := FItems.Count-1;
      ItemIndex := iIndex;
      MakeItemVisible(ItemIndex);
      if OldIndex <> ItemIndex then
        CheckSelectionFocus;
      CheckMultiSelect
    end;
  else
    consumed := False;
    inherited HandleKeyPress(keycode, shiftstate, consumed);
    Exit;
  end;
  DoRepaint;

end;

procedure TlqListView.HandleKeyRelease(var keycode: word;
  var shiftstate: TShiftState; var consumed: boolean);
var
  CacheShiftPressed: Boolean;
begin
  CacheShiftPressed := ShiftIsPressed;

  ShiftIsPressed := ssShift in shiftstate;

  consumed := CacheShiftPressed <> ShiftIsPressed;

end;

procedure TlqListView.HandlePaint;
var
  ClipRect: TlqRect;
begin
  //if FScrollBarNeedsUpdate then
    UpdateScrollBarPositions;
  lqStyle.DrawControlFrame(Canvas, 0, 0, Width, Height);
  
  ClipRect.SetRect(2, 2, Width-4, Height-4);
  Canvas.SetClipRect(ClipRect);

  // This paints the small square remaining below the vscrollbar
  // and to the right of the hscrollbar
  if FVScrollBar.Visible and FHScrollBar.Visible then
  begin
    Canvas.Color := clButtonFace;
    Canvas.FillRectangle(Width - 2 - FVScrollBar.Width,
                         Height - 2 - FHScrollBar.Height,
                         Width - 2,
                         Height - 2);
  end;
  
  if FVScrollBar.Visible then
    Dec(ClipRect.Width, FVScrollBar.Width);
  if FHScrollBar.Visible then
    Dec(ClipRect.Height, FhScrollBar.Height);
    
  Canvas.SetClipRect(ClipRect);
  PaintItems;
  if ShowHeaders then
    PaintHeaders;
end;

procedure TlqListView.HandleResize(awidth, aheight: TlqCoord);
begin
  inherited HandleResize(awidth, aheight);
  FScrollBarNeedsUpdate := FScrollBarNeedsUpdate or FSizeIsDirty;
end;

procedure TlqListView.PaintHeaders;
var
  I: Integer;
  cLeft,
  cTop: Integer;
  Column: TlqLVColumn;
  Flags: TlqButtonFlags;
  ClipRect: TlqRect;
  cRect: TlqRect;
  PaintPart: TlqLVItemPaintPart;
  tWidth,
  tLeft: Integer;
begin
  cLeft := 2;
  ClipRect.Top := 2;
  ClipRect.Left := 2;
  ClipRect.Height := HeaderHeight;
  ClipRect.Width := Width -4;
  Canvas.SetClipRect(ClipRect);
  
  if FHScrollBar.Visible then Dec(cLeft, FHScrollBar.Position);
  cTop := 2;
  for I := 0 to Columns.Count-1 do
  begin
    Column := Columns.Column[I];
    if Column.Visible then
    begin
      Flags := [btfIsEmbedded];
      if Column.FDown then Flags := Flags + [btfIsPressed];
      cRect.Top := cTop;
      cRect.Left := cLeft;
      cRect.Width := Column.Width;
      cRect.Height := HeaderHeight;
      lqStyle.DrawButtonFace(Canvas,cLeft, cRect.Top, cRect.Width, cRect.Height, Flags);
      PaintPart := [lvppText];
      
      if Assigned(FOnPaintColumn) then
        FOnPaintColumn(Self, Canvas, Column, I, cRect, PaintPart);
        
      if lvppText in PaintPart then
      begin
        tLeft := cLeft;
        tWidth := Canvas.Font.TextWidth(Column.Caption);
        case Column.CaptionAlignment of
          taRightJustify: Inc(tLeft, Column.Width - tWidth - 5);
          taCenter: Inc(tLeft, (Column.Width - tWidth - 5) div 2);
          taLeftJustify: Inc(tLeft, 5);
        end;
        lqStyle.DrawString(Canvas, tLeft, cTop+5, Column.Caption, Enabled);
      end;
      Inc(cLeft, Column.Width);
    end;
  end;
  if cLeft < FWidth-2 then
  begin
    Canvas.SetColor(clButtonFace);
    Canvas.FillRectangle(cLeft, cTop, cLeft+(Width-3-cLeft), Canvas.Font.Height+10);
  end;
end;

procedure TlqListView.PaintItems;
var
  FirstIndex,
  LastIndex: Integer;
  I, J : Integer;
  PaintPart: TlqLVItemPaintPart;
  ItemRect: TlqRect;
  ItemState: TlqLVItemState;
  Item: TlqLVItem;
  TheText: String;
  TheTextColor: TlqColor;
  oClipRect: TlqRect;
  iColumnClipRect: TlqRect;
  ColumnIndex: Integer;
  cBottom: Integer;
  vBottom: Integer;
  tLeft,
  tWidth: Integer;
  Image, TmpImage: TlqImage;
begin
  FirstIndex := (FVScrollBar.Position) div ItemHeight;
  LastIndex := (FVScrollBar.Position+GetItemAreaHeight) div ItemHeight;

  if LastIndex > FItems.Count-1 then
    LastIndex := FItems.Count-1;
    
  cBottom := 2 + ((LastIndex+1 - FirstIndex) * ItemHeight);
  
  if ShowHeaders then
    Inc(cBottom, HeaderHeight);
    
  oClipRect := Canvas.GetClipRect;

  for I := FirstIndex to LastIndex do
  begin
    ItemState := [];
    Image := nil;
    PaintPart := [lvppBackground, lvppIcon, lvppText];
    ItemRect := ItemGetRect(I);
    
    if  (I = FirstIndex)
    and (ShowHeaders)
    and (ItemRect.Top < 2 + HeaderHeight) then
      Dec(cBottom, (2 + HeaderHeight) - ItemRect.Top);
    
    Item := FItems.Item[I];
    if Item.Selected[Self] then
      Include(ItemState, lisSelected);
    if FItemIndex = I then
    begin
      Include(ItemState, lisFocused);
      Include(PaintPart, lvppFocused);
    end;

    if lisSelected in (ItemState) then
    begin
      if Focused then
        Canvas.Color := clSelection
      else
        Canvas.Color := clInactiveSel;
    end
    else
      Canvas.Color := clListBox;

    Canvas.FillRectangle(ItemRect);
    Exclude(PaintPart, lvppBackground);
    TheTextColor := Canvas.TextColor;
    if Assigned(FOnPaintItem) then
      FOnPaintItem(Self, Canvas, Item, I, ItemRect, PaintPart);

    if lvppFocused in PaintPart then
    begin
      if lisSelected in ItemState then
        Canvas.Color := TlqColor(not clSelection)
      else
        Canvas.Color := clSelection;

      Canvas.SetLineStyle(1, lsDot);
      Canvas.DrawRectangle(ItemRect);
    end;

    if (lvppText in PaintPart) or (lvppIcon in PaintPart) then
    begin
      if lisSelected in ItemState then
        Canvas.TextColor := clSelectionText;
      for J := 0 to FColumns.Count-1 do
      begin
        if FColumns.Column[J].Visible then
        begin
          iColumnClipRect.Left := Max(ItemRect.Left, oClipRect.Left);
          iColumnClipRect.Top := Max(ItemRect.Top, oClipRect.Top);
          iColumnClipRect.SetRight(Min(ItemRect.Left+FColumns.Column[J].Width, oClipRect.Right));
          iColumnClipRect.SetBottom(Min(ItemRect.Bottom, oClipRect.Bottom));
          Canvas.SetClipRect(iColumnClipRect);
          if FColumns.Column[J].ColumnIndex <> -1 then
            ColumnIndex := FColumns.Column[J].ColumnIndex
          else
            ColumnIndex := J;
          if lvppText in PaintPart then begin
            if ColumnIndex = 0 then
              TheText := Item.Caption
            else if Item.SubItemCount >= ColumnIndex then
              TheText := Item.SubItems.Strings[ColumnIndex-1]
            else
              TheText := '';
          end;

          tLeft := ItemRect.Left;

          Image := FindImageForState(I, J, ItemState);
          if (lvppIcon in PaintPart) and Assigned(Image) then
          begin
            TmpImage := Image;
            if not Enabled then
              TmpImage := Image.CreateDisabledImage;
            Canvas.DrawImage(ItemRect.Left,ItemRect.Top, TmpImage);
            if Not Enabled then
              TmpImage.Free;
          end;


          if lvppText in PaintPart then
          begin
            tWidth := Canvas.Font.TextWidth(TheText);
            case FColumns.Column[J].Alignment of
              taRightJustify:
                  Inc(tLeft, FColumns.Column[J].Width - tWidth - 5);
              taCenter:
                  Inc(tLeft, (FColumns.Column[J].Width - tWidth - 5) div 2);
              taLeftJustify:
               begin
                 Inc(tLeft, 5);
                 if Image <> nil then
                   Inc(tLeft,  Max(Image.Width, ItemHeight)-5);
               end;
            end;
            lqStyle.DrawString(Canvas, tLeft, ItemRect.Top+2, TheText, Enabled);
          end;

          Inc(ItemRect.Left, FColumns.Column[J].Width);
          //WriteLn(ItemRect.Left,' ', ItemRect.Top, ' ', ItemRect.Right, ' ', ItemRect.Bottom);
        end;
      end;
    end;
    
    Canvas.SetClipRect(oClipRect);
    
    Canvas.TextColor := TheTextColor;
  end;


  vBottom := Height - 2;
  if FHScrollBar.Visible then
    Dec(vBottom, FHScrollBar.Height);
    
  // the painted items haven't fully covered the visible area
  if vBottom > cBottom then
  begin
    ItemRect.Left := 2;
    ItemRect.Top := cBottom;
    ItemRect.SetBottom(vBottom);
    ItemRect.Width := Width - 4;
    Canvas.SetColor(clListBox);
    Canvas.FillRectangle(ItemRect);
  end;
  if GetVisibleColumnsWidth < oClipRect.Width then
  begin
    ItemRect.Left := GetVisibleColumnsWidth+2;
    ItemRect.SetRight(oClipRect.Right);
    ItemRect.Top := oClipRect.Top;
    ItemRect.Height := oClipRect.Height;
    Canvas.SetColor(clListBox);
    Canvas.FillRectangle(ItemRect);
  end;
end;

procedure TlqListView.UpdateScrollBarPositions;
var
  BevelSize: Integer;
  I: Integer;
  MaxH,
  MaxV: Integer;
begin
  MaxH := 0;
  MaxV := 0;
  BevelSize := 2;
  
  for I := 0 to Columns.Count -1 do
  begin
    if Columns.Column[I].Visible then
      Inc(MaxH, Columns.Column[I].Width);
  end;
  
  MaxV := (FItems.Count+2) * ItemHeight - (Height);
  if ShowHeaders then
    Inc(MaxV, HeaderHeight);
  if FVScrollBar.Visible then
    Inc(MaxH, FVScrollBar.Width);
  
  FHScrollBar.Top := Height - FHScrollBar.Height - (BevelSize );
  FHScrollBar.Left := BevelSize;
  FHScrollBar.Width := Width - (BevelSize * 2);

  
  FVScrollBar.Top := BevelSize;
  FVScrollBar.Left := Width - FVScrollBar.Width - (BevelSize );
  FVScrollBar.Height := Height - FVScrollBar.Top - BevelSize;
  
  if FVScrollBar.Visible and FHScrollBar.Visible then
  begin
    FHScrollBar.Width := FHScrollBar.Width - FVScrollBar.Width;
    FVScrollBar.Height := FVScrollBar.Height - FHScrollBar.Height;
  end;

  FHScrollBar.Max := MaxH-(Width-(BevelSize * 2));
  FVScrollBar.Max := MaxV;
  
  if FVScrollBar.Max = 0 then
    FVScrollBar.SliderSize := 1
  else
  begin
    if (FVScrollBar.Max + FVScrollBar.Height) > 0 then
      FVScrollBar.SliderSize := FVScrollBar.Height / (FVScrollBar.Max + FVScrollBar.Height)
    else
      FVScrollBar.SliderSize := 0.5;
  end;
  FVScrollBar.RepaintSlider;

  if FHScrollBar.Max = 0 then
    FHScrollBar.SliderSize := 1
  else
  begin
    if (FHScrollBar.Max + FHScrollBar.Width) > 0 then
      FHScrollBar.SliderSize := FHScrollBar.Width / (FHScrollBar.Max + FHScrollBar.Width)
    else
      FHScrollBar.SliderSize := 0.5;
  end;
  FHScrollBar.RepaintSlider;


  if FHScrollBar.Visible then
    FHScrollBar.UpdateWindowPosition;
  if FVScrollBar.Visible then
    FVScrollBar.UpdateWindowPosition;

  FScrollBarNeedsUpdate := False;
end;

constructor TlqListView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWidth       := 120;
  FHeight      := 80;
  Focusable := True;
  FShowHeaders := True;

  FVScrollBar := TlqScrollBar.Create(Self);
  FVScrollBar.Orientation := orVertical;
  FVScrollBar.OnScroll := @VScrollChange;
  FVScrollBar.ScrollStep := 18;
  FVScrollBar.Position := 0;
  
  FHScrollBar := TlqScrollBar.Create(Self);
  FHScrollBar.Orientation := orHorizontal;
  FHScrollBar.OnScroll := @HScrollChange;
  FHScrollBar.ScrollStep := 18;
  FHScrollBar.Position := 0;
  
  FColumns := TlqLVColumns.Create(Self);

  FItems := TlqLVItems.Create(Self);
  FSelected := TAVLTree.Create;
  FOldSelected := TAVLTree.Create;
  FSelectionShiftStart := -1;
  FSelectionFollowsFocus := True;
  FItemIndex := -1;
  FScrollBarNeedsUpdate := True;
end;

destructor TlqListView.Destroy;
begin
  FItems.DeleteViewer(Self);
  FSelected.Free;
  FOldSelected.Free;
  FColumns.Free;
  inherited Destroy;
end;

procedure TlqListView.BeginUpdate;
begin
  Inc(FUpdateCount);
  FItems.BeginUpdate;
end;

procedure TlqListView.EndUpdate;
begin
  FItems.EndUpdate;
  Dec(FUpdateCount);
  if FUpdateCount < 0 then
    FUpdateCount := 0;
  if FUpdateCount = 0 then
    DoRePaint;
end;

procedure TlqListView.MakeItemVisible(AIndex: Integer; PartialOK: Boolean);
var
  iTop,
  iBottom: integer;
  tVisible, bVisible: Boolean;
begin
  if AIndex = -1 then
    Exit;
  iTop := AIndex * ItemHeight;
  iBottom := iTop + ItemHeight;

  tVisible := (iTop >= FVScrollBar.Position) and (iTop < FVScrollBar.Position + GetItemAreaHeight);
  bVisible := (iBottom >= FVScrollBar.Position) and (iBottom < FVScrollBar.Position + GetItemAreaHeight);

  if PartialOK and (bVisible or tVisible) then
    Exit;
  
  if bVisible and tVisible then
    Exit;
  
  if (iBottom >= FVScrollBar.Position + GetItemAreaHeight) then
    FVScrollBar.Position := iBottom - GetItemAreaHeight
  else
    FVScrollBar.Position := iTop;
end;

function TlqListView.ItemAdd: TlqLVItem;
begin
  Result := AddItem;
end;

function TlqListView.AddItem: TlqLVItem;
begin
  Result := TlqLVItem.Create(FItems);
  FItems.Add(Result);
end;

function TlqListView.NewItem: TlqLVItem;
begin
  Result := TlqLVItem.Create(FItems);
end;

{ TlqLVColumns }

function TlqLVColumns.GetColumn(AIndex: Integer): TlqLVColumn;
begin
  Result := TlqLVColumn(FColumns.Items[AIndex]);
end;

procedure TlqLVColumns.SetColumn(AIndex: Integer; const AValue: TlqLVColumn);
begin
  FColumns.Items[AIndex] := AValue;
end;

constructor TlqLVColumns.Create(AListView: TlqListView);
begin
  FListView := AListView;
  FColumns := TObjectList.Create;
end;

destructor TlqLVColumns.Destroy;
begin
  Clear; // needed since Colums can be share between ListViews
  FColumns.Free;
  inherited Destroy;
end;

function TlqLVColumns.Add(AColumn: TlqLVColumn): Integer;
begin
  Result := Count;
  Insert(AColumn, Count);
end;

procedure TlqLVColumns.Clear;
var
  i: integer;
begin
  for  i := FColumns.Count-1 downto 0 do
    Delete(i);
  FColumns.Clear;
end;

procedure TlqLVColumns.Delete(AIndex: Integer);
begin
  Dec(Column[AIndex].Ref);
  FColumns.OwnsObjects := Column[AIndex].Ref < 1;
  FColumns.Delete(AIndex);
end;

procedure TlqLVColumns.Insert(AColumn: TlqLVColumn; AIndex: Integer);
begin
  FColumns.Insert(AIndex, AColumn);
  Inc(AColumn.Ref);
end;

function TlqLVColumns.Count: Integer;
begin
  Result := FColumns.Count;
end;

{ TlqLVColumn }

procedure TlqLVColumn.SetCaption(const AValue: String);
begin
  if FCaption=AValue then
    Exit;
  FCaption:=AValue;
end;

procedure TlqLVColumn.SetCaptionAlignment(const AValue: TAlignment);
begin
  if FCaptionAlignment=AValue then exit;
  FCaptionAlignment:=AValue;
  if Assigned(FColumns) and Assigned(FColumns.FListView) then
    FColumns.FListView.DoRepaint;

end;

procedure TlqLVColumn.SetColumnIndex(const AValue: Integer);
begin
  if FColumnIndex=AValue then
    Exit;
  FColumnIndex:=AValue;
end;

procedure TlqLVColumn.SetHeight(const AValue: Integer);
begin
  if FHeight=AValue then Exit;
  FHeight:=AValue;
end;

procedure TlqLVColumn.SetResizable(const AValue: Boolean);
begin
  if FResizable=AValue then exit;
  FResizable:=AValue;
end;

procedure TlqLVColumn.SetVisible(const AValue: Boolean);
begin
  if FVisible=AValue then exit;
  FVisible:=AValue;
end;

procedure TlqLVColumn.SetAutoSize(const AValue: Boolean);
begin
  if FAutoSize=AValue then exit;
  FAutoSize:=AValue;
end;

procedure TlqLVColumn.SetAlignment(const AValue: TAlignment);
begin
  if FAlignment=AValue then exit;
  FAlignment:=AValue;
  if Assigned(FColumns)and Assigned(FColumns.FListView) then
    FColumns.FListView.DoRepaint;
end;

procedure TlqLVColumn.SetWidth(const AValue: Integer);
begin
  if FWidth=AValue then exit;
  FWidth:=AValue;
  if FWidth < 1 then
    FWidth := 1;
end;

constructor TlqLVColumn.Create(AColumns: TlqLVColumns);
begin
  FVisible := True;
  FColumnIndex := -1;
  FColumns := AColumns;
  FClickable := True;
  FAlignment := taLeftJustify;
  FCaptionAlignment := taLeftJustify;
end;

destructor TlqLVColumn.Destroy;
begin
  inherited Destroy;
end;

{ TlqListViewSubitems }

function TlqListViewSubitems.GetImageIndex(ASubIndex: Integer): Integer;
begin
  Result := PfpgLVSubitemRec(FList.Items[ASubIndex])^.AImageIndex;
end;

procedure TlqListViewSubitems.SetImageIndex(ASubIndex: Integer;
  const AValue: Integer);
begin
  PfpgLVSubitemRec(FList.Items[ASubIndex])^.AImageIndex:=AValue;
  DoChange;
end;

procedure TlqListViewSubitems.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TlqListViewSubitems.GetObject(Index: Integer): TObject;
begin
  Result:=PfpgLVSubitemRec(FList.Items[Index])^.AObject;
end;

function TlqListViewSubitems.Get(Index: Integer): string;
begin
  Result:=PfpgLVSubitemRec(FList.Items[Index])^.AText;
end;

function TlqListViewSubitems.GetCount: Integer;
begin
  Result:=FList.Count;
end;

procedure TlqListViewSubitems.Put(Index: Integer; const S: string);
begin
  PfpgLVSubitemRec(FList.Items[Index])^.AText:=S;
end;

procedure TlqListViewSubitems.PutObject(Index: Integer; AObject: TObject);
var
  SubItem: PfpgLVSubitemRec;
begin
  SubItem := PfpgLVSubitemRec(FList.Items[Index]);
  SubItem^.AObject := AObject;
  DoChange;
end;

procedure TlqListViewSubitems.Insert(Index: Integer; const S: string);
var
  SubItem: PfpgLVSubitemRec;
begin
  SubItem:= New(PfpgLVSubitemRec);
  SubItem^.AText:=S;
  SubItem^.AImageIndex:=-1;
  SubItem^.AObject := nil;
  FList.Insert(Index, SubItem);
  DoChange;
end;

constructor TlqListViewSubitems.Create;
begin
  FList:= TFPList.Create;
end;

destructor TlqListViewSubItems.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

procedure TlqListViewSubitems.Delete(Index: Integer);
var
  SubItem: PfpgLVSubitemRec;
begin
  SubItem := PfpgLVSubitemRec(FList.Items[Index]);
  SubItem^.AText := '';
  Dispose(SubItem);
  FList.Delete(Index);
end;

procedure TlqListViewSubItems.Clear;
var
  i: LongInt;
begin
  for i := FList.Count-1 downto 0 do
    Delete(i);
end;

end.
