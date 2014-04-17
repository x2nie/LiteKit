{
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2011 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Defines a basic Treeview control and Node classes. The treeview
      keeps track of the nodes in a double-linked list structure.
      Each Node as .prev and .next property pointing to it's neighbours.
}

unit lq_tree;

{$mode objfpc}{$H+}

{
  TODO:
    * Lots! :-)
    * Columns need to be reworked. We don't want coluns per node levels. Instead
      we want a main column covering the tree. Then extra columns for user
      text and data.
    * Implement event handlers the user can hook into and do custom drawing.
    * TlqTreeNode.HasChildren property is not fully implemented yet. The
      property is not update when you do .Append() or .AppendText() calls. It
      such cases .Count is prefered.
}

{.$Define Debug}

interface

uses
  Classes,
  SysUtils,
  lq_base,
  lq_main,
  lq_widget,
  lq_imagelist,
  lq_scrollbar,
  lq_menu;

type

  TlqNodeAttachMode = (naAdd, naAddFirst, naAddChild, naAddChildFirst, naInsert);

  PfpgTreeColumnWidth = ^TlqTreeColumnWidth;
  TlqTreeColumnWidth = record
    next: PfpgTreeColumnWidth;
    width: word;
  end;

  // forward declaration
  TlqTreeView = class;
  TlqTreeNode = class;

  TlqTreeNodeFindMethod = procedure(ANode: TlqTreeNode; var AFound: boolean) of object;
  TlqTreeExpandEvent = procedure(Sender: TObject; ANode: TlqTreeNode) of object;
  TlqStateImageClickedEvent = procedure(Sender: TObject; ANode: TlqTreeNode) of object;


  TlqTreeNode = class(TObject)
  private
    FCollapsed: boolean;
    FData: Pointer;
    FFirstSubNode: TlqTreeNode; // the subnodes - for list implementation
    FImageIndex: integer;
    FInactSelColor: TlqColor;
    FInactSelTextColor: TlqColor;
    FLastSubNode: TlqTreeNode;
    FNext: TlqTreeNode;
    FParent: TlqTreeNode;
    FPrev: TlqTreeNode;
    FSelColor: TlqColor;
    FSelTextColor: TlqColor;
    FStateImageIndex: integer;
    FText: TlqString;
    FTextColor: TlqColor;
    FHasChildren: Boolean;
    FTree: TlqTreeView;
    procedure   SetCollapsed(const AValue: boolean);
    procedure   SetInactSelColor(const AValue: TlqColor);
    procedure   SetInactSelTextColor(const AValue: TlqColor);
    procedure   SetParent(const AValue: TlqTreeNode);
    procedure   SetSelColor(const AValue: TlqColor);
    procedure   SetSelTextColor(const AValue: TlqColor);
    procedure   SetText(const AValue: TlqString);
    procedure   SetTextColor(const AValue: TlqColor);
    procedure   DoRePaint;
    procedure   SetHasChildren(const AValue: Boolean);
    procedure   DoTreeCheck(ANode: TlqTreeNode);
    procedure   SetStateImageIndex(const AValue: integer);
  public
    constructor Create;
    destructor  Destroy; override;
    // node related
    function    AppendText(AText: TlqString): TlqTreeNode;
    function    Count: integer;
    function    CountRecursive: integer;
    function    FindSubNode(AText: string; ARecursive: Boolean): TlqTreeNode; overload;
    function    FindSubNode(ATreeNodeFindMethod: TlqTreeNodeFindMethod): TlqTreeNode; overload;
    function    FindSubNode(AData: TObject; ARecursive: Boolean): TlqTreeNode; overload;
    function    GetMaxDepth: integer;
    function    GetMaxVisibleDepth: integer;
    procedure   Append(var ANode: TlqTreeNode);
    procedure   Clear;  // remove all nodes recursively
    procedure   Collapse;
    procedure   Expand;
    procedure   Remove(var aNode: TlqTreeNode);
    procedure   MoveTo(Destination: TlqTreeNode; Mode: TlqNodeAttachMode);
    procedure   UnregisterSubNode(aNode: TlqTreeNode);
    // parent color settings
    function    ParentInactSelColor: TlqColor;
    function    ParentInactSelTextColor: TlqColor;
    function    ParentSelColor: TlqColor;
    function    ParentSelTextColor: TlqColor;
    function    ParentTextColor: TlqColor;
    // general properties
    property    Collapsed: boolean read FCollapsed write SetCollapsed;
    property    Data: Pointer read FData write FData;
    property    FirstSubNode: TlqTreeNode read FFirstSubNode;
    property    ImageIndex: integer read FImageIndex write FImageIndex;
    property    StateImageIndex: integer read FStateImageIndex write SetStateImageIndex;
    property    LastSubNode: TlqTreeNode read FLastSubNode;
    property    Next: TlqTreeNode read FNext write FNext;
    property    Parent: TlqTreeNode read FParent write SetParent;
    property    Prev: TlqTreeNode read FPrev write FPrev;
    property    Text: TlqString read FText write SetText;
    { determines the + or - image in the treeview }
    property    HasChildren: Boolean read FHasChildren write SetHasChildren;
    // color settings
    property    InactSelColor: TlqColor read FInactSelColor write SetInactSelColor;
    property    InactSelTextColor: TlqColor read FInactSelTextColor write SetInactSelTextColor;
    property    SelColor: TlqColor read FSelColor write SetSelColor;
    property    SelTextColor: TlqColor read FSelTextColor write SetSelTextColor;
    property    TextColor: TlqColor read FTextColor write SetTextColor;
  end;


  TlqTreeView = class(TlqWidget)
  private
    FImageList: TlqImageList;
    FStateImageList: TlqImageList;
    FColumnHeight: integer; // height of the column header
    FDefaultColumnWidth: word;
    FIndentNodeWithNoImage: boolean;
    FFirstColumn: PfpgTreeColumnWidth; // the list for column widths
    FFont: TlqFont;
    FHScrollbar: TlqScrollbar;
    FMoving: boolean;
    FMovingCol: integer;
    FMovingPos: integer;
    FNoImageIndent: integer;
    FOnChange: TNotifyEvent;
    FOnExpand: TlqTreeExpandEvent;
    FOnStateImageClicked: TlqStateImageClickedEvent;
    FRootNode: TlqTreeNode;
    FScrollWheelDelta: integer;
    FSelection: TlqTreeNode; // currently selected node
    FShowColumns: boolean;
    FShowImages : boolean;
    FTreeLineColor: TlqColor;
    FTreeLineStyle: TlqLineStyle;
    FVScrollbar: TlqScrollbar;
    FXOffset: integer; // for repaint and scrollbar-calculation
    FYOffset: integer;
    FUpdateCount: integer;
    function    GetFontDesc: string;
    function    GetRootNode: TlqTreeNode;
    procedure   SetDefaultColumnWidth(const AValue: word);
    procedure   SetFontDesc(const AValue: string);
    procedure   SetSelection(const AValue: TlqTreeNode);
    procedure   SetShowColumns(const AValue: boolean);
    procedure   SetShowImages(const AValue: boolean);
    procedure   SetTreeLineColor(const AValue: TlqColor);
    procedure   SetTreeLineStyle(const AValue: TlqLineStyle);
    procedure   SetIndentNodeWithNoImage(const AValue: boolean);
    function    VisibleWidth: integer;
    function    VisibleHeight: integer;
    function    GetNodeHeightSum: integer;
    function    MaxNodeWidth: integer;
    function    GetNodeHeight: integer;
    // width of a node inclusive of image and state image
    function    GetNodeWidth(ANode: TlqTreeNode): integer;
    function    NodeIsVisible(ANode: TlqTreeNode): boolean;
    // returns the node-top in pixels
    function    GetAbsoluteNodeTop(ANode: TlqTreeNode): integer;
    function    GetColumnLeft(AIndex: integer): integer;
    procedure   PreCalcColumnLeft;
    procedure   VScrollbarScroll(Sender: TObject; position: integer);
    procedure   HScrollbarScroll(Sender: TObject; position: integer);
    procedure   UpdateScrollbars;
    procedure   ResetScrollbar;
    procedure   ClearColumnLeft;
    procedure   FreeAllTreeNodes;
  protected
    FColumnLeft: TList;
    FPopupMenu: TlqPopupMenu;
    procedure   HandleResize(awidth, aheight: TlqCoord); override;
    procedure   HandleLMouseUp(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleLMouseDown(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleRMouseUp(x, y: integer; shiftstate: TShiftState); override;
    procedure   HandleDoubleClick(x, y: integer; button: word; shiftstate: TShiftState); override;
    procedure   HandleKeyPress(var keycode: word; var shiftstate: TShiftState; var consumed: boolean); override;
    procedure   HandleMouseScroll(x, y: integer; shiftstate: TShiftState; delta: smallint); override;
    procedure   HandleShow; override;
    procedure   HandlePaint; override;
    procedure   DrawHeader(ACol: integer; ARect: TlqRect; AFlags: integer); virtual;
    procedure   DoChange; virtual;
    procedure   DoExpand(ANode: TlqTreeNode); virtual;
    // the nodes between the given node and the direct next node
    function    SpaceToVisibleNext(aNode: TlqTreeNode): integer;
    function    StepToRoot(aNode: TlqTreeNode): integer;
    procedure   RePaint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   SetColumnWidth(AIndex, AWidth: word);
    // the width of a column - aIndex of the rootnode = 0
    function    GetColumnWidth(AIndex: word): word;
    function    GetNodeAt(const X, Y: integer): TlqTreeNode;
    procedure   GotoNextNodeUp;
    procedure   GotoNextNodeDown;
    procedure   FullCollapse;
    procedure   FullExpand;
    // any next node, even if node is collapsed
    function    NextNode(ANode: TlqTreeNode): TlqTreeNode;
    function    PrevNode(ANode: TlqTreeNode): TlqTreeNode;
    // only visual (visible) nodes
    function    NextVisualNode(ANode: TlqTreeNode): TlqTreeNode;
    function    PrevVisualNode(ANode: TlqTreeNode): TlqTreeNode;
    procedure   BeginUpdate;
    procedure   EndUpdate;
    property    Font: TlqFont read FFont;
    // Invisible node that starts the tree
    property    RootNode: TlqTreeNode read GetRootNode;
    property    Selection: TlqTreeNode read FSelection write SetSelection;
    property    ImageList: TlqImageList read FImageList write FImageList;
    property    StateImageList: TlqImageList read FStateImageList write FStateImageList;
    property    PopupMenu: TlqPopupMenu read FPopupMenu write FPopupMenu;
  published
    property    Align;
    property    BackgroundColor default clListBox;
    property    DefaultColumnWidth: word read FDefaultColumnWidth write SetDefaultColumnWidth default 15;
    property    Enabled;
    property    FontDesc: string read GetFontDesc write SetFontDesc;
    property    IndentNodeWithNoImage: boolean read FIndentNodeWithNoImage write SetIndentNodeWithNoImage default True;
    property    NoImageIndent: integer read FNoImageIndent write FNoImageIndent default 16;
    property    ParentShowHint;
    property    ScrollWheelDelta: integer read FScrollWheelDelta write FScrollWheelDelta default 15;
    property    ShowColumns: boolean read FShowColumns write SetShowColumns default False;
    property    Hint;
    property    ShowHint;
    property    ShowImages: boolean read FShowImages write SetShowImages default False;
    property    TabOrder;
    property    TextColor;
    property    TreeLineColor: TlqColor read FTreeLineColor write SetTreeLineColor default clShadow1;
    property    TreeLineStyle: TlqLineStyle read FTreeLineStyle write SetTreeLineStyle default lsDot;
    property    OnChange: TNotifyEvent read FOnChange write FOnChange;
    property    OnExpand: TlqTreeExpandEvent read FOnExpand write FOnExpand;
    property    OnDoubleClick;
    property    OnShowHint;
    property    OnStateImageClicked: TlqStateImageClickedEvent read FOnStateImageClicked write FOnStateImageClicked;
  end;


implementation

{.$IFDEF DEBUG}
uses
  dbugintf;
{.$ENDIF}

type
  PColumnLeft = ^integer;


{ TlqTreeNode }

procedure TlqTreeNode.SetInactSelColor(const AValue: TlqColor);
begin
  if AValue <> FInactSelColor then
  begin
    FInactSelColor := AValue;
    DoRePaint;
  end;
end;

procedure TlqTreeNode.SetCollapsed(const AValue: boolean);
begin
  if aValue <> FCollapsed then
  begin
    FCollapsed := AValue;
    DoRePaint;
  end;
end;

procedure TlqTreeNode.SetInactSelTextColor(const AValue: TlqColor);
begin
  if AValue <> FInactSelTextColor then
  begin
    FInactSelTextColor := AValue;
    DoRePaint;
  end;
end;

procedure TlqTreeNode.SetParent(const AValue: TlqTreeNode);
begin
  if aValue <> FParent then
  begin
    if FParent <> nil then
      FParent.UnRegisterSubNode(self); // unregisteres
    FParent := aValue;
    if FParent <> nil then
    begin
      DoRePaint;
    end;
  end;
end;

procedure TlqTreeNode.SetSelColor(const AValue: TlqColor);
begin
  if FSelColor <> aValue then
  begin
    FSelColor := aValue;
    DoRePaint;
  end;
end;

procedure TlqTreeNode.SetSelTextColor(const AValue: TlqColor);
begin
  if FTextColor <> aValue then
  begin
    FSelTextColor := aValue;
    DoRePaint;
  end;
end;

procedure TlqTreeNode.SetText(const AValue: TlqString);
begin
  if AValue <> FText then
  begin
    FText := aValue;
    DoRePaint;
  end;
end;

procedure TlqTreeNode.SetTextColor(const AValue: TlqColor);
begin
  if FTextColor <> aValue then
  begin
    FTextColor := aValue;
    DoRePaint;
  end;
end;

procedure TlqTreeNode.DoRePaint;
begin
//  FTree.Invalidate;
end;

procedure TlqTreeNode.SetHasChildren(const AValue: Boolean);
begin
  if FHasChildren <> AValue then
  begin
    FHasChildren := AValue;
    DoRePaint;
  end;
end;

procedure TlqTreeNode.DoTreeCheck(ANode: TlqTreeNode);
begin
  if ANode.FTree <> FTree then
    raise Exception.Create('Nodes must be of the same tree');
end;

procedure TlqTreeNode.SetStateImageIndex(const AValue: integer);
begin
  if FStateImageIndex = AValue then
    exit;
  FStateImageIndex := AValue;
  DoRepaint;
end;

constructor TlqTreeNode.Create;
begin
  FData           := nil;
  FFirstSubNode   := nil;
  FLastSubNode    := nil;
  FText           := '';
  FImageIndex     := -1;
  FStateImageIndex := -1;
  FCollapsed      := True;
  FHasChildren    := False;

  FParent     := nil;
  FNext       := nil;
  FPrev       := nil;

  FSelColor           := clUnset;
  FSelTextColor       := clUnset;
  FTextColor          := clUnset;
  FInactSelColor      := clUnset;
  FInactSelTextColor  := clUnset;
end;

destructor TlqTreeNode.Destroy;
begin
  if FParent <> nil then
    FParent.UnregisterSubNode(self);
  Clear;
  FData       := nil;
  FParent     := nil;
  FNext       := nil;
  FPrev       := nil;
  FFirstSubNode   := nil;
  FLastSubNode    := nil;
  inherited Destroy;
end;

procedure TlqTreeNode.UnregisterSubNode(aNode: TlqTreeNode);
var
  h: TlqTreeNode;
begin
  h := FFirstSubNode;
  while h <> nil do
  begin
    if h = aNode then
    begin
      if h = FFirstSubNode then
        FFirstSubNode := FFirstSubNode.Next;
      if h = FLastSubNode then
        FLastSubNode := FLastSubNode.Prev;
      if h.prev <> nil then
        h.prev.next := h.next;
      if h.next <> nil then
        h.next.prev := h.prev;
      exit;
    end;
    h := h.next;
  end;
end;

procedure TlqTreeNode.Append(var ANode: TlqTreeNode);
begin
  DoTreeCheck(ANode);
  ANode.Parent := self;
  ANode.Next   := nil;

  if FFirstSubNode = nil then
    FFirstSubNode := ANode;

  ANode.Prev := FLastSubNode;

  if FLastSubNode <> nil then
    FLastSubNode.Next := ANode;

  FLastSubNode := ANode;
  FHasChildren := True;
end;

function TlqTreeNode.FindSubNode(AText: string; ARecursive: Boolean): TlqTreeNode;
var
  h: TlqTreeNode;
begin
  result := nil;
  if ARecursive then
  begin
    h := FirstSubNode;
    while h <> nil do
    begin
      if h.Text = AText then
      begin
        result := h;
        Exit; //==>
      end;
      if h.count > 0 then
      begin
        result := h.FindSubNode(AText, ARecursive);
        if result <> nil then
          Exit; //==>
      end;
      h := h.next;
    end;  { while }
  end
  else
  begin
    h := FirstSubNode;
    while h <> nil do
    begin
      if h.Text = AText then
      begin
        result := h;
        break;
      end;
      h := h.next;
    end;
  end;  { if/else }
end;

function TlqTreeNode.FindSubNode(ATreeNodeFindMethod: TlqTreeNodeFindMethod): TlqTreeNode;
var
  lFound: Boolean;
  h: TlqTreeNode;
begin
  result := nil;
  lFound := False;
  if not Assigned(ATreeNodeFindMethod) then
    Exit; //==>

  h := FirstSubNode;
  while h <> nil do
  begin
    ATreeNodeFindMethod(h, lFound);
    if lFound then
    begin
      result := h;
      Exit; //==>
    end;
    if h.Count > 0 then
    begin
      result := h.FindSubNode(ATreeNodeFindMethod);
      if result <> nil then
        Exit; //==>
    end;
    h := h.next;
  end;
end;

function TlqTreeNode.FindSubNode(AData: TObject; ARecursive: Boolean): TlqTreeNode;
var
  h: TlqTreeNode;
begin
  result := nil;
  if ARecursive then
  begin
    h := FirstSubNode;
    while h <> nil do
    begin
      if TObject(h.Data) = AData then
      begin
        result := h;
        Exit; //==>
      end;
      if h.count > 0 then
      begin
        result := h.FindSubNode(AData, ARecursive);
        if result <> nil then
          Exit; //==>
      end;
      h := h.next;
    end;  { while }
  end
  else
  begin
    h := FirstSubNode;
    while h <> nil do
    begin
      if TObject(h.Data) = AData then
      begin
        result := h;
        break;
      end;
      h := h.next;
    end;
  end;  { if/else }
end;

function TlqTreeNode.AppendText(AText: TlqString): TlqTreeNode;
var
  h: TlqTreeNode;
begin
  {$IFDEF DEBUG}
  SendDebug(Classname + '.AppendText');
  {$ENDIF}
  h := TlqTreeNode.Create;
  h.FTree := FTree;
  h.Text := AText;
  Append(h);
  result := h;
end;

function TlqTreeNode.GetMaxDepth: integer;
var
  h: TlqTreeNode;
  a: integer;
  t: integer;
begin
  {$IFDEF DEBUG}
  SendDebug(Classname + '.GetMaxDepth');
  {$ENDIF}
  h := FirstSubNode;
  result := 1;
  a := 0;
  while h <> nil do
  begin
    t := h.GetMaxDepth;
    if t > a then
      a := t;
    h := h.next;
  end;
  result := result + a;
end;

function TlqTreeNode.GetMaxVisibleDepth: integer;
var
  h: TlqTreeNode;
  a: integer;
  t: integer;
begin
  {$IFDEF DEBUG}
  SendDebug(Classname + '.GetMaxVisibleDepth');
  {$ENDIF}
  result := 1;
  h := FirstSubNode;
  if h.Collapsed then
    exit;
  a := 0;
  while h <> nil do
  begin
    t := h.GetMaxDepth;
    if t > a then
      a := t;
    h := h.next;
  end;
  result := result + a;
end;

procedure TlqTreeNode.Collapse;
begin
  Collapsed := True;
end;

procedure TlqTreeNode.Expand;
begin
  Collapsed := False;
end;

function TlqTreeNode.Count: integer;
var
  h: TlqTreeNode;
  i: integer;
begin
  h := FirstSubNode;
  i := 0;
  while h <> nil do
  begin
    inc(i);
    h := h.next;
  end;
  result := i;
end;

function TlqTreeNode.CountRecursive: integer;
var
  h: TlqTreeNode;
  i: integer;
begin
  h := FFirstSubNode;
  i := 0;
  while h <> nil do
  begin
    inc(i); // current node
    i := i + h.CountRecursive; // increases i by the count of the subnodes of the subnode
    h := h.next;
  end;
  result := i;
end;

procedure TlqTreeNode.Remove(var aNode: TlqTreeNode);
begin
  if FirstSubNode = aNode then
  begin
    FFirstSubNode := aNode.next;
    if FFirstSubNode <> nil then
      FFirstSubNode.Prev := nil;
  end
  else
  begin
    if aNode.prev <> nil then
      aNode.Prev.next := aNode.next;
  end;

  if LastSubNode = aNode then
  begin
    FLastSubNode := aNode.prev;
    if FLastSubNode <> nil then
      FLastSubNode.next := nil;
  end
  else
  begin
    if aNode.next <> nil then
      aNode.next.prev := aNode.prev;
  end;

  aNode.prev := nil;
  aNode.next := nil;
  aNode.parent := nil;
end;

procedure TlqTreeNode.MoveTo(Destination: TlqTreeNode; Mode: TlqNodeAttachMode);
begin
  if Destination = nil then
    Exit;
  DoTreeCheck(Destination);

  Parent.Remove(self);
  case Mode of
    naAdd:
        begin
          Destination.Parent.Append(self);
        end;
    naAddFirst:
        begin
          Next := Destination.Parent.FirstSubNode;
          Next.Prev := self;
          Destination.Parent.FFirstSubNode := self;
          Parent := Destination.Parent;
        end;
    naAddChild:
        begin
          Destination.Append(self);
        end;
    naAddChildFirst:
        begin
          Next := Destination.FirstSubNode;
          if Assigned(Destination.FirstSubNode) then
            Destination.FirstSubNode.Prev := self;
          Destination.FFirstSubNode := self;
          Parent := Destination;
          if Destination.LastSubNode = nil then
            Destination.FLastSubNode := self;
        end;
    naInsert:
        begin
          Prev := Destination.Prev;
          Next := Destination;
          Parent := Destination.Parent;
          Destination.Prev := self;
          if Prev = nil then
            Parent.FFirstSubNode := self
          else
            Prev.Next := self;
        end;
  end;  { case }
end;

procedure TlqTreeNode.Clear;
var
  n: TlqTreeNode;
  tn: TlqTreeNode;
begin
  n := LastSubNode;
  while n <> nil do
  begin
    tn := n;
    n := n.prev;
    Remove(tn);
    tn.Free;
  end;
  FHasChildren := False;
end;

function TlqTreeNode.ParentTextColor: TlqColor;
begin
  if TextColor <> clUnset then
    result := TextColor
  else
  begin
    if parent <> nil then
      result := parent.ParentTextColor
    else
      result := clText1;
  end;
end;

function TlqTreeNode.ParentSelTextColor: TlqColor;
begin
  if SelTextColor <> clUnset then
    result := SelTextColor
  else
  begin
    if parent <> nil then
      result := parent.ParentSelTextColor
    else
      result := clSelectionText;
  end;
end;

function TlqTreeNode.ParentSelColor: TlqColor;
begin
  if SelColor <> clUnset then
    result := SelColor
  else
  begin
    if parent <> nil then
      result := parent.ParentSelColor
    else
      result := clSelection;
  end;
end;

function TlqTreeNode.ParentInactSelTextColor: TlqColor;
begin
  if InactSelTextColor <> clUnset then
    result := InactSelTextColor
  else
  begin
    if Parent <> nil then
      Result := Parent.ParentInactSelTextColor
    else
      Result := clInactiveSelText;
  end;
end;

function TlqTreeNode.ParentInactSelColor: TlqColor;
begin
  if InactSelColor <> clUnset then
    result := InactSelColor
  else
  begin
    if Parent <> nil then
      result := parent.ParentInactSelColor
    else
      result := clInactiveSel;
  end;
end;

{ TlqTreeview }

procedure TlqTreeview.VScrollbarScroll(Sender: TObject; position: integer);
begin
  {$IFDEF DEBUG}
  SendDebug(Classname + '.VScrollbarMove');
  {$ENDIF}
  FYOffset := Position;
  RePaint;
end;

function TlqTreeview.GetFontDesc: string;
begin
  Result := FFont.FontDesc;
end;

function TlqTreeview.GetRootNode: TlqTreeNode;
begin
  if FRootNode = nil then
  begin
    FRootNode := TlqTreeNode.Create;
    FRootNode.FTree := self;
    FRootNode.Collapsed := False;
  end;
  FRootNode.TextColor     := clText1;
  FRootnode.SelTextColor  := clSelectionText;
  FRootnode.SelColor      := clSelection;
  Result := FRootNode;
end;

procedure TlqTreeview.SetDefaultColumnWidth(const AValue: word);
begin
  if (aValue <> FDefaultColumnWidth) and (aValue > 3) then
  begin
    FDefaultColumnWidth := AValue;
    RePaint;
  end;
end;

procedure TlqTreeview.SetFontDesc(const AValue: string);
begin
  FFont.Free;
  FFont := fpgGetFont(AValue);
  RePaint;
end;

procedure TlqTreeview.SetSelection(const AValue: TlqTreeNode);
var
  n: TlqTreeNode;
  dy: integer;    // y delta - absolute top node
  nh: integer;    // node height
  vh: integer;    // visible height
begin
  if AValue <> FSelection then
  begin
    FSelection := AValue;
    if AValue <> nil then
    begin
      n := AValue.Parent;
      while n <> nil do
      begin
        if n.Collapsed then
        begin
          n.Expand;
          DoExpand(n);
        end;
        n := n.parent;
      end;
      UpdateScrollbars;
    end;

    dy := GetAbsoluteNodeTop(FSelection);
    nh := GetNodeHeight;
    vh := VisibleHeight;
    if dy + nh - FVScrollbar.Position > vh then
    begin
      if FVScrollBar.Max = 0 then    // the first time and no expansion happened before.
        FVScrollBar.Max := dy + Height;
      FVScrollbar.Position := dy + nh - (vh div 2);
      FYOffset := FVScrollbar.Position;
      UpdateScrollBars;
      if FHScrollbar.Visible then    // HScrollbar appeared so we need to adjust position again
      begin
        FVScrollbar.Position := FVScrollbar.Position + FHScrollbar.Height;
        FYOffset := FVScrollbar.Position;
        UpdateScrollBars;
      end;
    end;

    if dy - FVScrollbar.Position < 0 then
    begin
      FVScrollbar.Position := dy;
      FYOffset := FVScrollbar.Position;
      UpdateScrollbars;
    end;
  end;
end;

procedure TlqTreeview.SetShowColumns(const AValue: boolean);
begin
  if FShowColumns <> aValue then
  begin
    FShowColumns := aValue;
    RePaint;
  end;
end;

procedure TlqTreeview.SetShowImages(const AValue: boolean);
begin
  if AValue <> FShowImages then
  begin
    FShowImages := AValue;
    UpdateScrollbars;
    RePaint;
  end;
end;

procedure TlqTreeview.SetTreeLineColor(const AValue: TlqColor);
begin
  if FTreeLineColor = AValue then
    Exit; //==>
  FTreeLineColor := AValue;
  RePaint;
end;

procedure TlqTreeview.SetTreeLineStyle(const AValue: TlqLineStyle);
begin
  if FTreeLineStyle = AValue then
    Exit; //==>
  FTreeLineStyle := AValue;
  RePaint;
end;

procedure TlqTreeView.SetIndentNodeWithNoImage(const AValue: boolean);
begin
  if AValue <> FIndentNodeWithNoImage then
  begin
    FIndentNodeWithNoImage := AValue;
    UpdateScrollbars;
    RePaint;
  end;
end;

function TlqTreeview.VisibleWidth: integer;
begin
  Result := Width - 2; // border width = 2 pixels
  if FVScrollbar.Visible then
     dec(Result, FVScrollbar.Width);
end;

function TlqTreeview.VisibleHeight: integer;
begin
  Result := Height - 2; // border width = 2 pixels
  if FShowColumns then
    dec(Result, FColumnHeight);
  if FHScrollbar.Visible then
    dec(Result, FHScrollbar.Height);
end;

function TlqTreeview.GetNodeHeightSum: integer;
var
  h: TlqTreeNode;
  i: integer;
begin
  h := RootNode;
  i := -1;
  while h <> nil do
  begin
    inc(i);
    if (not h.Collapsed) and (h.Count > 0) then
    begin
      h := h.FirstSubNode;
    end
    else
    begin
      if h.next <> nil then
        h := h.next
      else
      begin
        while h.next = nil do
        begin
          h := h.parent;
          if h = nil then
          begin
            result := i;
            exit;
          end;
        end;
        h := h.next;
      end;
    end;
  end;
  result := i;
end;

function TlqTreeview.MaxNodeWidth: integer;
var
  h: TlqTreeNode;
  w: integer;
  r: integer;
begin
  {$IFDEF DEBUG}
  SendDebug(Classname + '.MaxNodeWidth');
  {$ENDIF}
  result := 0;
  h := RootNode.FirstSubNode;
  r := 0;
  while h <> nil do
  begin
    w := GetColumnLeft(StepToRoot(h));
    if r < w + GetNodeWidth(h) then
      r := w + GetNodeWidth(h);
    if (not h.collapsed) and (h.count > 0) then
      h := h.FirstSubNode
    else
    begin
      if h.next <> nil then
        h := h.next
      else
      begin
        while h.next = nil do
        begin
          h := h.parent;
          if h = nil then
          begin
            result := r + 4;
            exit;
          end;
        end;  { while }
        h := h.next;
      end;
    end;  { if/else }
  end;  { while }
end;

function TlqTreeview.GetNodeHeight: integer;
begin
  Result := FFont.Height + 2;
end;

function TlqTreeview.GetNodeWidth(ANode: TlqTreeNode): integer;
var
  lImageItem: TlqImageItem;
begin
  {$IFDEF DEBUG}
  SendDebug(Classname + '.GetNodeWidth');
  {$ENDIF}
  if ANode = nil then
    Result := 0
  else
  begin
    Result := FFont.TextWidth(ANode.Text) + 4;
    if ShowImages and ((ImageList <> nil) or (StateImageList <> nil)) then
    begin
      if ImageList <> nil then
      begin
        if ANode.ImageIndex > -1 then
        begin
          lImageItem := ImageList.Items[ANode.ImageIndex];
          if lImageItem <> nil then
            result += lImageItem.Image.Width + 2;
        end
        else if IndentNodeWithNoImage then
          result += NoImageIndent + 2;
      end
      else if IndentNodeWithNoImage then
        result += NoImageIndent + 2;

      if StateImageList <> nil then
      begin
        if ANode.StateImageIndex > -1 then
        begin
          lImageItem := StateImageList.Items[ANode.StateImageIndex];
          if lImageItem <> nil then
            result += lImageItem.Image.Width + 2;
        end
        else if IndentNodeWithNoImage then
          result += NoImageIndent + 2;
      end;
    end;
  end;  { if/else }
end;

function TlqTreeview.NodeIsVisible(ANode: TlqTreeNode): boolean;
begin
  Result := True;
  if ANode = nil then
  begin
    Result := False;
    exit;
  end;
  ANode := ANode.Parent;
  while ANode <> nil do
  begin
    if ANode.Collapsed and (ANode.Parent <> nil) then
      Result := False;
    ANode := ANode.Parent;
  end;
end;

function TlqTreeview.GetAbsoluteNodeTop(ANode: TlqTreeNode): integer;
var
  i: integer;
begin
  i := 0;
  while (ANode <> nil) and (ANode <> RootNode) do
  begin
    ANode := PrevVisualNode(ANode);
    inc(i);
  end;
  result := (i - 1) * GetNodeHeight;
end;

function TlqTreeview.GetColumnLeft(AIndex: integer): integer;
begin
  if FColumnLeft = nil then
    PreCalcColumnLeft;

  if AIndex < 0 then
    Result := 0
  else
  begin
    if AIndex > FColumnLeft.Count - 1 then
      result := PColumnLeft(FColumnLeft[FColumnLeft.Count - 1])^
    else
      result := PColumnLeft(FColumnLeft[AIndex])^;
  end;
end;

function TlqTreeview.GetColumnWidth(AIndex: word): word;
var
  h: PfpgTreeColumnWidth;
  i: integer;
begin
{$IFDEF DEBUG}
  SendDebug(Classname + '.GetColumnWidth');
{$ENDIF}
  h := FFirstColumn;
  i := 0;
  if h = nil then // not found
  begin
    result := DefaultColumnWidth;
    exit;
  end;
  while i < aIndex do
  begin
    if h = nil then // not found - returns the default
    begin
      result := DefaultColumnWidth;
      exit;
    end;
    h := h^.next;
    inc(i);
  end;
  if h <> nil then
    result := h^.width
  else // not found -> returns the default
    result := DefaultColumnWidth;
end;

function TlqTreeView.GetNodeAt(const X, Y: integer): TlqTreeNode;
var
  col: integer;
  lTop: integer;
  lLeft: integer;
  i, i1: integer;
  cancel: boolean;
  last, node: TlqTreeNode;
  w: integer;
  lNodeXOffset: integer;
begin
  if ShowColumns then
    col := FColumnHeight
  else
    col := 0;

  Result := nil;
  i := 0;
  lTop := y - col - 1 + FYOffset;
  lLeft := x + FXOffset;
  cancel := False;
  last := RootNode;

  while not ((((i - 1) * GetNodeHeight) <= lTop) and ((i * GetNodeHeight) >= lTop)) do
  begin
    node := NextVisualNode(last);
    if node = nil then
      exit; //==>
    if node = last then
    begin
      cancel := True;
      break;  //==>
    end;
    inc(i);
    last := node;
  end;

  if (not cancel) or (node <> nil) then
  begin
    // +/- or node-selection?
    i1  := StepToRoot(node);
    w   := GetColumnLeft(i1);
    lNodeXOffset := w - GetColumnWidth(i1) div 2 + 6;
    if lLeft > lNodeXOffset then  { we are in the actual treenode area }
    begin
      Result := node;
    end;
  end;
end;

procedure TlqTreeView.GotoNextNodeUp;
begin
  if Selection = RootNode.FirstSubNode then
    Exit;
  Selection := PrevNode(Selection);
end;

procedure TlqTreeView.GotoNextNodeDown;
var
  lNode: TlqTreeNode;
begin
  if (Selection = RootNode.LastSubNode) and (RootNode.LastSubNode.CountRecursive = 0) then
    Exit;

  lNode := NextNode(Selection);
  if lNode <> nil then
    Selection := lNode;
end;

procedure TlqTreeView.FullCollapse;
var
  n: TlqTreeNode;
begin
  n := NextNode(RootNode);
  repeat
    if n <> nil then
    begin
      n.Collapse;
    end;
    n := NextNode(n);
  until n = nil;
  Repaint;
end;

procedure TlqTreeView.FullExpand;
var
  n: TlqTreeNode;
begin
  n := NextNode(RootNode);
  repeat
    if n <> nil then
    begin
      n.Expand;
    end;
    n := NextNode(n);
  until n = nil;
  Repaint;
end;

procedure TlqTreeview.PreCalcColumnLeft;
var
  Aleft: TlqCoord;
  ACounter: integer;
  AColumnLeft: PColumnLeft;
begin
  {$IFDEF DEBUG}
  SendDebug(Classname + '.PreCalcColumnLeft');
  {$ENDIf}
  if FColumnLeft = nil then
	  FColumnLeft := TList.Create;

  ClearColumnLeft;  // Freeing memory

  Aleft := 0;
  for ACounter := 1 to RootNode.GetMaxDepth do
  begin
  	AColumnLeft := new(PColumnLeft);
  	AColumnLeft^ := Aleft;
  	FColumnLeft.Add(AColumnLeft);
  	Aleft := Aleft + GetColumnWidth(ACounter);
  end;
end;

procedure TlqTreeview.HScrollbarScroll(Sender: TObject; position: integer);
begin
  FXOffset := Position;
  RePaint;
end;

procedure TlqTreeview.UpdateScrollbars;
begin
  {$IFDEF DEBUG}
  SendDebug(Classname + '.UpdateScrollbars');
  {$ENDIF}
  FVScrollbar.Visible := VisibleHeight < (GetNodeHeightSum * GetNodeHeight);
  FVScrollbar.Min := 0;
  FVScrollbar.Max := (GetNodeHeightSum * GetNodeHeight) - VisibleHeight + FHScrollbar.Height;
  FVScrollbar.PageSize := (VisibleHeight div 4) * 3;  // three quarters of the height
  FVScrollbar.ScrollStep := GetNodeHeight;  // up/down buttons move the height of the font
  FHScrollbar.Min := 0;
  FHScrollbar.Max := MaxNodeWidth - VisibleWidth + FVScrollbar.Width;
  FHScrollbar.PageSize := (VisibleWidth div 4) * 3;  // three quarters of the height
  FHScrollbar.Visible := MaxNodeWidth > Width - 2;
  if not FVScrollbar.Visible then
  begin
    FVScrollbar.Position := 0;
    FVScrollBar.RepaintSlider;
    FYOffset := 0;
  end
  else
    FVScrollBar.RepaintSlider;

  if not FHScrollbar.Visible then
  begin
    FHScrollbar.Position := 0;
    FHScrollBar.RepaintSlider;
    FXOffset := 0;
  end
  else
    FHScrollBar.RepaintSlider;
end;

procedure TlqTreeview.ResetScrollbar;
const
  cSBarThickness = 16;
begin
  {$IFDEF DEBUG}
  SendDebug(Classname + '.ResetScrollbar');
  {$ENDIF}
  UpdateScrollBars;
  if FHScrollbar.Visible then
    FVScrollbar.SetPosition(Width - cSBarThickness-1, 1, cSBarThickness, Height - 2 - cSBarThickness)
  else
    FVScrollbar.SetPosition(Width - cSBarThickness-1, 1, 16, Height - 2);
  FHScrollbar.SetPosition(1, Height - cSBarThickness-1, Width - 2, cSBarThickness);
end;

procedure TlqTreeView.ClearColumnLeft;
var
  i: integer;
  AColumnLeft: PColumnLeft;
begin
  for i := 0 to FColumnLeft.Count - 1 do  // Freeing Memory
  begin
    AColumnLeft := FColumnLeft[i];
    Dispose(AColumnLeft);
  end;
  FColumnLeft.Clear;
end;

procedure TlqTreeView.FreeAllTreeNodes;
begin
  RootNode.Clear;
end;

procedure TlqTreeview.HandleResize(awidth, aheight: TlqCoord);
begin
  {$IFDEF DEBUG}
  SendDebug(Classname + '.HandleResize');
  {$ENDIF}
  inherited HandleResize(awidth, aheight);
  if (csLoading in ComponentState) then
    Exit; //==>
  if csUpdating in ComponentState then
    Exit; //==>
  if not (csLoading in ComponentState) then
    ResetScrollbar;
  RePaint;
end;

procedure TlqTreeview.HandleLMouseUp(x, y: integer; shiftstate: TShiftState);
var
  col: integer;
  i: integer;
  w: integer;
  i1: integer;
  last: TlqTreeNode;
  node: TlqTreeNode;
  cancel: boolean;
  OldSel: TlqTreeNode;
  lNodeXOffset: integer;
begin
  inherited HandleLMouseUp(x, y, shiftstate);

  node := nil;
  OldSel := Selection;
  if FMoving then // column resize
  begin
    FMoving := false;
    x := x + FXOffset;
    SetColumnWidth(FMovingCol, GetColumnWidth(FMovingCol) + x - FMovingPos);
    FMoving := false;
  end
  else
  begin
    if ShowColumns then
      col := FColumnHeight
    else
      col := 0;
    y := y - col - 1 + FYOffset;
    i := 0;
    x := x + FXOffset;
    cancel := False;
    last := RootNode;
    while not ((((i - 1) * GetNodeHeight) <= y) and ((i * GetNodeHeight) >= y)) do
    begin
      node := NextVisualNode(last);
      if node = nil then
        exit; //==>
      if node = last then
      begin
        cancel := True;
        break;  //==>
      end;
      inc(i);
      last := node;
    end;

    if (not cancel) or (node <> nil) then
    begin
      // +/- or node-selection?
      i1  := StepToRoot(node);
      w   := GetColumnLeft(i1);
      if (x >= w - GetColumnWidth(i1) div 2 - 3) and (x <= w - GetColumnWidth(i1) div 2 + 6) then
      // collapse or expand?
      begin // yes
        if (node.Count > 0) or node.HasChildren then
        begin
          if node.Collapsed then
          begin
            node.expand;
            DoExpand(node);
          end
          else
            node.Collapse;
          ResetScrollBar;
          RePaint;
        end;
      end
      else
      begin
        lNodeXOffset := w - GetColumnWidth(i1) div 2 + 6;
        if x > lNodeXOffset then  { we clicked in the actual treenode area }
        begin
          Selection := node;
          if (StateImageList <> nil) and ShowImages then
          begin
            { did we click on the state image }
            if (x > lNodeXOffset) and (x < (lNodeXOffset+18)) then
            begin
              if Assigned(OnStateImageClicked) then
                FOnStateImageClicked(self, node);
            end;
          end;
        end;
      end;
    end;
  end;
  if OldSel <> Selection then
  begin
    RePaint;
    DoChange;
  end;
end;

procedure TlqTreeview.HandleLMouseDown(x, y: integer; shiftstate: TShiftState);
var
  xpos: integer;
  i: integer;
begin
  inherited HandleLMouseDown(x, y, shiftstate);
  if ShowColumns then
  begin
    x := x + FXOffset;
    xpos := 0;
    i := 0;
    while xpos + 2 < x do
    begin
      inc(i);
      xpos := xpos + GetColumnWidth(i);
    end;
    if (x > xpos - 2) and (x < xpos + 2) then
    begin
      FMoving := True;
      FMovingPos := xpos;
      FMovingCol := i;
      SetColumnWidth(i, GetColumnWidth(i));
    end;
  end;
  RePaint;
end;

procedure TlqTreeView.HandleRMouseUp(x, y: integer; shiftstate: TShiftState);
begin
  inherited HandleRMouseUp(x, y, shiftstate);
  if Assigned(PopupMenu) then
    PopupMenu.ShowAt(self, x, y);
end;

procedure TlqTreeview.HandleDoubleClick(x, y: integer; button: word;
  shiftstate: TShiftState);
begin
  // to setup cursor co-ordinates and handle selection
  HandleLMouseUp(x, y, shiftstate);
  inherited HandleDoubleClick(x, y, button, shiftstate);
  if Selection <> nil then
  begin
    if Selection.Collapsed then
    begin
      Selection.Expand;
      DoExpand(Selection);
    end
    else
      Selection.Collapse;
    RePaint;
  end;
end;

procedure TlqTreeview.HandleShow;
begin
  if (csLoading in ComponentState) then
    Exit;
  if not (csLoading in ComponentState) then
    ResetScrollbar;
  inherited HandleShow;
end;

procedure TlqTreeview.HandlePaint;
var
  r: TlqRect;
  h: TlqTreeNode;
  i: integer;
  i1: integer;
  w: integer;
  YPos: integer;
  col: integer;
  ACenterPos: integer;
  x: integer;
  imgx: integer;
  y: integer;
  AImageItem: TlqImageItem;
  AVisibleHeight: integer;
begin
  {$IFDEF DEBUG}
  SendDebug(Classname + '.HandlePaint');
  {$ENDIF}
  if csUpdating in ComponentState then
    Exit;

//  inherited HandlePaint;

  Canvas.ClearClipRect;
  Canvas.Clear(BackgroundColor);
  Canvas.SetFont(FFont);

  // Limit painting in the UI Designer
  if csDesigning in ComponentState then
  begin
    Canvas.SetColor(clInactiveWgFrame);
    r.SetRect(0, 0, Width, Height);
    Canvas.DrawRectangle(r);
    Canvas.SetTextColor(clInactiveWgFrame);
    Canvas.DrawString(2, 2, Name + ': ' + Classname);
    Exit;
  end;

  if FFocused then
    Canvas.SetColor(clWidgetFrame)
  else
    Canvas.SetColor(clInactiveWgFrame);
  r.SetRect(0, 0, Width, Height);
  Canvas.DrawRectangle(r); // border

  i1 := 0;
  PreCalcColumnLeft;
  UpdateScrollbars;
  AVisibleHeight := VisibleHeight;


//  if not HasHandle then
//    Exit; //==>

  { TODO : Columns need to be redesigned completely }
  if ShowColumns then
  begin
    // Drawing column headers
    r.SetRect(1, 1, 0, FColumnHeight);
    for col := 1 to rootnode.getMaxDepth - 1 do
    begin
      r.Width := GetColumnWidth(col);
      DrawHeader(col, r, 0);
      inc(r.Left, r.Width);
      if r.Left >= VisibleWidth then
        Break;  // small optimization. Don't draw what we can't see
    end;
    // Fill remainder of the client area with one big header
    r.width := VisibleWidth - r.Left + 1;
    DrawHeader(col+1, r, 0);
  end;

  // Calculate the client area used for nodes and lines
  if ShowColumns then
  begin
    r.SetRect(1, 1 + FColumnHeight, VisibleWidth, VisibleHeight);
    col := FColumnHeight;
  end
  else
  begin
    r.SetRect(1, 1, VisibleWidth, VisibleHeight);
    col := 0;
  end;
  Canvas.ClearClipRect;
  Canvas.SetClipRect(r);

  // draw the nodes with lines
  h := RootNode.FirstSubNode;
  YPos := 0;
  while h <> nil do
  begin
    Canvas.SetTextColor(h.ParentTextColor);
    // lines with + or -
    w := GetColumnLeft(StepToRoot(h));
    ACenterPos := YPos - FYOffset + col + (GetNodeHeight div 2);
    YPos := YPos + GetNodeHeight;
    i := GetColumnLeft(StepToRoot(h)) + GetNodeWidth(h);
    imgx := 0;

    // only paint the node if it is fully visible
    if i > FXOffset then
    begin
      // State images must always be 16x16 pixels
      if (StateImageList <> nil) and ShowImages then
      begin
        AImageItem := StateImageList.Items[h.StateImageIndex];
        if Assigned(AImageItem) then
        begin
          Canvas.DrawImagePart(w - FXOffset + 1, ACenterPos - 8, AImageItem.Image, 0, 0, 16, 16);
          imgx := 18; // 16px max image width + 2px spacing or right
        end
        else
        begin
          if FIndentNodeWithNoImage then
            imgx := 18
          else
            imgx := 0;
        end;
      end;

      if (ImageList <> nil) and ShowImages then
      begin
        AImageItem := ImageList.Items[h.ImageIndex];
        if AImageItem <> nil then
        begin
          Canvas.DrawImagePart(w + imgx - FXOffset + 1, ACenterPos - 8, AImageItem.Image, 0, 0, 16, 16);
          imgx += AImageItem.Image.Width + 2; // 1px spacing on right before node text
        end
        else
        begin
          if FIndentNodeWithNoImage then
            imgx += FNoImageIndent + 2
          else
            imgx += 2;
        end;
      end;

      if h = Selection then // draw the selection rectangle and text
      begin
        if Focused then
        begin
          Canvas.SetColor(h.ParentSelColor);
          Canvas.SetTextColor(h.ParentSelTextColor);
        end
        else
        begin
          Canvas.SetColor(h.ParentInactSelColor);
          Canvas.SetTextColor(h.ParentInActSelTextColor);
        end;
        // draw selection rectangle
        Canvas.FillRectangle(w + imgx - FXOffset, ACenterPos - (GetNodeHeight div 2), GetNodeWidth(h) - imgx, GetNodeHeight);
      end;

      Canvas.DrawString(w + imgx - FXOffset + 2 { small spacing }, ACenterPos - (GetNodeHeight div 2), h.text);

      Canvas.SetTextColor(h.ParentTextColor);
      Canvas.SetLineStyle(1, FTreeLineStyle);

      if (h.Count > 0) or h.HasChildren then   // do we have subnodes?
      begin
        // small horizontal line above rectangle for first subnode (with children) only
        if (h <> RootNode.FirstSubNode) then
        begin
          if (h.Parent.FirstSubNode = h)  then
          begin
            Canvas.SetLineStyle(1, FTreeLineStyle);
            Canvas.DrawLine(w - FXOffset - GetColumnWidth(i1) div 2 + 1, ACenterPos - 7, w - FXOffset - GetColumnWidth(i1) div 2 + 1, ACenterPos - 3);
          end;
        end;

        // subnode rectangle around the "+" or "-"
        Canvas.SetColor(FTreeLineColor);
        Canvas.SetLineStyle(1, lsSolid);  // rectangle is always solid line style
        Canvas.DrawRectangle(w - FXOffset - GetColumnWidth(i1) div 2 - 3, ACenterPos - 3, 9, 9);

        Canvas.SetColor(clText1);

        if h.Collapsed {or h.HasChildren} then
        begin
          // draw a "+"
          Canvas.DrawLine(w - FXOffset - GetColumnWidth(i1) div 2 - 1, ACenterPos + 1, w - FXOffset - GetColumnWidth(i1) div 2 + 4, ACenterPos + 1);
          Canvas.DrawLine(w - FXOffset - GetColumnWidth(i1) div 2 + 1, ACenterPos - 1, w - FXOffset - GetColumnWidth(i1) div 2 + 1, ACenterPos + 4);
        end
        else
        begin
          // draw a "-"
          Canvas.DrawLine(w - FXOffset - GetColumnWidth(i1) div 2 - 1, ACenterPos + 1, w - FXOffset - GetColumnWidth(i1) div 2 + 4, ACenterPos + 1);
        end;

        Canvas.SetLineStyle(1, FTreeLineStyle);
      end
      else
      begin
        // short horizontal line for each node
        Canvas.SetColor(FTreeLineColor);
        Canvas.DrawLine(w - FXOffset - GetColumnWidth(i1) div 2 + 1,  ACenterPos + 1, w - FXOffset - 1,  ACenterPos + 1);
      end;

      Canvas.SetColor(FTreeLineColor);
      if h.prev <> nil then
      begin
        // line up to the previous node
        if (h.prev.count > 0) {or h.prev.HasChildren} then
        begin
          // take the previous subnode rectangle in account
          if (h.count > 0) or h.HasChildren then
            // we have a subnode rectangle
            Canvas.DrawLine(w - FXOffset - GetColumnWidth(i1) div 2 + 1, ACenterPos - 4, w - FXOffset - GetColumnWidth(i1) div 2 + 1, ACenterPos - (SpaceToVisibleNext(h.prev) * GetNodeHeight) + 5)
          else
            Canvas.DrawLine(w - FXOffset - GetColumnWidth(i1) div 2 + 1, ACenterPos, w - FXOffset - GetColumnWidth(i1) div 2 + 1, ACenterPos - (SpaceToVisibleNext(h.prev) * GetNodeHeight) + 5);
        end
        else
        begin
          // previous node has no subnodes
          if (h.count > 0) or h.HasChildren then
            // we have a subnode rectangle
            Canvas.DrawLine(w - FXOffset - GetColumnWidth(i1) div 2 + 1, ACenterPos - 3, w - FXOffset - GetColumnWidth(i1) div 2 + 1, ACenterPos - SpaceToVisibleNext(h.prev) * GetNodeHeight + 1)
          else
            Canvas.DrawLine(w - FXOffset - GetColumnWidth(i1) div 2 + 1, ACenterPos, w - FXOffset - GetColumnWidth(i1) div 2 + 1, ACenterPos - SpaceToVisibleNext(h.prev) * GetNodeHeight + 1);
        end;
      end
      else
      begin
        if (h.count > 0) or h.HasChildren then
          // take the subnode rectangle in account
          Canvas.DrawLine(w - FXOffset - GetColumnWidth(i1) div 2 + 1,ACenterPos - 3, w - FXOffset - GetColumnWidth(i1) div 2 + 1, ACenterPos - GetNodeHeight div 2 + 3)
        else
          Canvas.DrawLine(w - FXOffset - GetColumnWidth(i1) div 2 + 1, ACenterPos, w - FXOffset - GetColumnWidth(i1) div 2 + 1, ACenterPos - GetNodeHeight div 2 + 3);
      end;
    end;

    if ShowColumns then
      i := ACenterPos
    else
      i := ACenterPos + GetNodeHeight;

    if AVisibleHeight > i then
    begin
      if (h.count > 0) and (not h.Collapsed) then
      begin
        h := h.FirstSubNode;
        continue;
      end;

      if h.next <> nil then
        h := h.next // next node
      else
      begin
        while h.next = nil do // or recurse next node per parent
        begin
          h := h.parent;
          if (h = nil) or (h = rootnode) then
          begin
            break;  //==>
          end;
        end;  { while }
        h := h.next;
      end;  { if/else }
    end
    else
    begin
      // Draw Lines up to the parent nodes
      ACenterPos := ACenterPos + GetNodeHeight;
      while h <> RootNode do
      begin
        w := GetColumnLeft(StepToRoot(h));
        if h.next <> nil then
        begin
          h := h.next;
          if (h.prev.count > 0) {or h.prev.HasChildren} then
          begin
            x := w - FXOffset - GetColumnWidth(i1) div 2 + 1;
            y := GetAbsoluteNodeTop(h.prev) - FYOffset + 5 + (GetNodeHeight div 2);
            if ShowColumns then
              inc(y, FColumnHeight);
            Canvas.DrawLine(x, ACenterPos, x, y);
          end
          else
          begin
            x := w - FXOffset - GetColumnWidth(i1) div 2 + 1;
            y := GetAbsoluteNodeTop(h.prev) - FYOffset + 1 + (GetNodeHeight div 2);
            if ShowColumns then
              inc(y, FColumnHeight);
            Canvas.DrawLine(x, ACenterPos, x, y);
          end;
        end;
        h := h.parent;
      end;
      break;  //==>
    end;
  end; { while h <> nil }
end;

procedure TlqTreeview.DrawHeader(ACol: integer; ARect: TlqRect;
  AFlags: integer);
begin
  // Here we can implement a head style check
  Canvas.DrawButtonFace(ARect, [btfIsEmbedded]);
end;

procedure TlqTreeview.HandleKeyPress(var keycode: word;
  var shiftstate: TShiftState; var consumed: boolean);
var
  h: TlqTreeNode;
  OldSelection: TlqTreeNode;
begin
  OldSelection := Selection;
  if ShiftState = [] then
  begin
    case KeyCode of
      keyRight:
        begin
          Consumed := True;
          Selection.Expand;
          DoExpand(Selection);
          ResetScrollbar;
          RePaint;
        end;

      keyLeft:
        begin
          Consumed := True;
          Selection.Collapsed := true;
          ResetScrollbar;
          RePaint;
        end;

      keyUp:
        begin
          if Selection = nil then
            Selection := RootNode.FirstSubNode
          else
            if Selection <> RootNode then
            begin
              if NodeIsVisible(selection) then
              begin
                h := PrevVisualNode(Selection);
                if (h <> RootNode) and (h <> nil) then
                  Selection := h;
              end
              else
              begin
                Selection := RootNode.FirstSubNode;
              end;
            end;
            Consumed := True;
        end;

      keyDown:
        begin
          Consumed := True;
          if Selection = nil then
            Selection := RootNode.FirstSubNode
          else
          begin
            if NodeIsVisible(selection) then
            begin
              h := NextVisualNode(Selection);
              if (h <> nil) then
                Selection := h;
            end
            else
              Selection := RootNode.FirstSubNode;
          end;
        end;

      keyPageUp:
        begin
          FVScrollbar.PageUp;
        end;

      keyPageDown:
        begin
          FVScrollbar.PageDown;
        end;
      else
        Consumed := False;
    end;
  end;

  if Selection <> OldSelection then
  begin
    RePaint;
    DoChange;
  end;

  if not Consumed then
    inherited HandleKeyPress(keycode, shiftstate, consumed);
end;

procedure TlqTreeview.HandleMouseScroll(x, y: integer;
  shiftstate: TShiftState; delta: smallint);
var
  i: integer;
  dy: integer;
begin
  inherited HandleMouseScroll(x, y, shiftstate, delta);
  dy := (VisibleHeight div 3);  // mouse scrolling is 1/3 of the height
  if delta > 0 then // scrolling down
  begin
    inc(FYOffset, dy);  //FScrollWheelDelta);
    i := (GetNodeHeightSum * GetNodeHeight) - VisibleHeight + FHScrollbar.Height;
    if FYOffset > i then
      FYOffset := i;
    i := FVScrollbar.Position + dy;
    FVScrollbar.Position := i;
  end
  else
  begin  // scrolling up
    dec(FYOffset, dy); //FScrollWheelDelta);
    if FYOffset < 0 then
      FYOffset := 0;
    i := FVScrollbar.Position - dy;
    FVScrollbar.Position := i;
  end;
  UpdateScrollbars;
  RePaint;
end;

procedure TlqTreeview.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(self);
end;

procedure TlqTreeview.DoExpand(ANode: TlqTreeNode);
begin
  if Assigned(FOnExpand) then
    FOnExpand(self, ANode);
end;

function TlqTreeview.NextVisualNode(ANode: TlqTreeNode): TlqTreeNode;
  //----------------
  procedure _FindNextNode;
  begin
    if ANode.Next <> nil then
    begin
      result := ANode.Next;
    end
    else
    begin
      while ANode.Next = nil do
      begin
        ANode := ANode.Parent;
        if ANode = nil then
          exit;  //==>
      end;
      result := ANode.Next;
    end;
  end;

begin
  result := nil;
  if ANode.Collapsed then
  begin
    _FindNextNode;
  end
  else
  begin
    if ANode.Count > 0 then
    begin
      result := ANode.FirstSubNode;
    end
    else
      _FindNextNode;
  end;
end;

function TlqTreeview.PrevVisualNode(ANode: TlqTreeNode): TlqTreeNode;
var
  n: TlqTreeNode;
begin
  n := ANode;
  if ANode.Prev <> nil then
  begin
    result  := ANode.Prev;
    ANode   := ANode.Prev;
    while (not ANode.Collapsed) and (ANode.Count > 0) do
    begin
      result  := ANode.LastSubNode;
      ANode   := ANode.LastSubNode;
    end;
  end
  else
  begin
    if ANode.Parent <> nil then
      result := ANode.Parent
    else
      result := n;
  end;
end;

procedure TlqTreeView.BeginUpdate;
begin
  Inc(FUpdateCount);
  Updating;
end;

procedure TlqTreeView.EndUpdate;
begin
  if FUpdateCount > 0 then
  begin
    Dec(FUpdateCount);
    if FUpdateCount = 0 then
    begin
      Updated;
      RePaint;
    end;
  end;
end;

function TlqTreeView.NextNode(ANode: TlqTreeNode): TlqTreeNode;
  //----------------
  procedure _FindNextNode;
  begin
    if ANode.Next <> nil then
    begin
      result := ANode.Next;
    end
    else
    begin
      while ANode.Next = nil do
      begin
        ANode := ANode.Parent;  // back out one level depth
        if ANode = nil then
          exit;  //==>
      end;
      result := ANode.Next;
    end;
  end;

begin
  result := nil;
  if ANode.Count > 0 then
    result := ANode.FirstSubNode
  else
    _FindNextNode;
end;

function TlqTreeView.PrevNode(ANode: TlqTreeNode): TlqTreeNode;
var
  n: TlqTreeNode;
begin
  n := ANode;
  if ANode.Prev <> nil then
  begin
    result  := ANode.Prev;
    ANode   := ANode.Prev;
    while {(not ANode.Collapsed) and} (ANode.Count > 0) do
    begin
      result  := ANode.LastSubNode;
      ANode   := ANode.LastSubNode;
    end;
  end
  else
  begin
    if ANode.Parent <> nil then
      result := ANode.Parent
    else
      result := n;
  end;
end;

function TlqTreeview.SpaceToVisibleNext(aNode: TlqTreeNode): integer;
var
  h: TlqTreeNode;
  i: integer;
begin
  result := 0;
  i := 0;
  if aNode.next = nil then
    exit;
  h := aNode;
  while h <> aNode.next do
  begin
    inc(i);
    if (h.count > 0) and (not h.collapsed) then
    begin
      h := h.FirstSubNode;
    end
    else
    begin
      while h.next = nil do
        h := h.parent;
      h := h.next;
    end;
  end;
  result := i;
end;

function TlqTreeview.StepToRoot(aNode: TlqTreeNode): integer;
var
  i: integer;
begin
  i := -1;
  while aNode <> nil do
  begin
    aNode := aNode.parent;
    inc(i);
  end;
  result := i;
end;

procedure TlqTreeView.RePaint;
begin
  if csUpdating in ComponentState then
    Exit;
  inherited RePaint;
end;

constructor TlqTreeview.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRootNode     := nil;
  FSelection    := nil;
  FShowImages   := False;
  FShowColumns  := False;
  FDefaultColumnWidth := 15;
  FFirstColumn  := nil;
  FFont := fpgGetFont('#Label1');
  Width := 150;
  Height := 100;
  FUpdateCount := 0;

  FHScrollbar := TlqScrollbar.Create(self);
  FHScrollbar.Orientation := orHorizontal;
  FHScrollbar.OnScroll    := @HScrollbarScroll;
  FHScrollbar.Visible     := False;
  FHScrollbar.Position    := 0;
  FHScrollbar.SliderSize  := 0.5;

  FVScrollbar := TlqScrollbar.Create(self);
  FVScrollbar.Orientation := orVertical;
  FVScrollbar.OnScroll    := @VScrollbarScroll;
  FVScrollbar.Visible     := False;
  FVScrollbar.Position    := 0;
  FVScrollbar.SliderSize  := 0.2;

  FBackgroundColor  := clListBox;
  FTreeLineColor    := clShadow1; //clText1;
  FTreeLineStyle    := lsDot;
  FFocusable        := True;
  FMoving           := False;
  FXOffset          := 0;
  FYOffset          := 0;
  FColumnHeight     := FFont.Height + 2;
  FScrollWheelDelta := 15;
  FNoImageIndent    := 16;
  FIndentNodeWithNoImage := True;
end;

destructor TlqTreeView.Destroy;
begin
  if Assigned(FColumnLeft) then
  begin
    ClearColumnLeft;
    FColumnLeft.Free;
  end;
  FFont.Free;
  FreeAllTreeNodes;
  FRootNode.Free;
  inherited Destroy;
end;

procedure TlqTreeview.SetColumnWidth(AIndex, AWidth: word);
var
  h: PfpgTreeColumnWidth;
  n: PfpgTreeColumnWidth;
  i: word;
begin
  {$IFDEF DEBUG}
  SendDebug(Classname + '.SetColumnWidth');
  {$ENDIF}
  h := FFirstColumn;
  if h = nil then
  begin
    new(h);
    h^.width := FDefaultColumnWidth;
    h^.next := nil;
    FFirstColumn := h;
  end;
  i := 0;
  while i < AIndex do
  begin
    if h^.next = nil then
    begin
      new(n);
      h^.next := n;
      n^.width := DefaultColumnWidth;
      n^.next := nil;
    end;
    h := h^.next;
    inc(i);
  end;
  if h^.width <> AWidth then
  begin
    h^.width := AWidth;
    RePaint;
  end;
end;


end.

