program treeviewtest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  SysUtils,
  lq_base,
  lq_main,
  lq_form,
  lq_tree,
  lq_checkbox,
  lq_button,
  lq_imagelist,
  lq_label,
  lq_dialogs,
  lq_combobox,
  lq_utils,
  stateimages;

type

  TMainForm = class(TlqForm)
  private
    {@VFD_HEAD_BEGIN: MainForm}
    tree: TlqTreeView;
    cbShowImages: TlqCheckBox;
    cbIndentNode: TlqCheckBox;
    btnClear: TlqButton;
    Label1: TlqLabel;
    Label2: TlqLabel;
    Label3: TlqLabel;
    Label4: TlqLabel;
    Label5: TlqLabel;
    lblSource: TlqLabel;
    lblDestination: TlqLabel;
    btnSource: TlqButton;
    btnDest: TlqButton;
    btnMoveTo: TlqButton;
    cbMoveToTypes: TlqComboBox;
    Label6: TlqLabel;
    Label7: TlqLabel;
    Label8: TlqLabel;
    btnCollapseAll: TlqButton;
    btnExpandAll: TlqButton;
    btnShowChecked: TlqButton;
    {@VFD_HEAD_END: MainForm}
    FImagelist: TlqImageList;
    FStateImagelist: TlqImageList;
    FSrcNode: TlqTreeNode;
    FDestnode: TlqTreeNode;
    procedure   cbShowImagesChange(Sender: TObject);
    procedure   cbIndentNodeChange(Sender: TObject);
    procedure   btnClearClicked(Sender: TObject);
    procedure   TreeNodeChanged(Sender: TObject);
    procedure   PopulateTree;
    procedure   btnSourceClicked(Sender: TObject);
    procedure   btnDestinationClicked(Sender: TObject);
    procedure   btnMoveToClicked(Sender: TObject);
    procedure   btnCollapseAllClicked(Sender: TObject);
    procedure   btnExpandAllClicked(Sender: TObject);
    procedure   StateImageClicked(Sender: TObject; ANode: TlqTreeNode);
    procedure   btnShowCheckedClicked(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   AfterCreate; override;
  end;

{ TMainForm }

procedure TMainForm.cbIndentNodeChange(Sender: TObject);
begin
  tree.IndentNodeWithNoImage := cbIndentNode.Checked;
end;

procedure TMainForm.btnClearClicked(Sender: TObject);
var
  n: TlqTreeNode;
begin
  if tree.Selection <> nil then
  begin
    n := tree.Selection;
    tree.Selection.Clear;
    tree.Invalidate;
    tree.Selection := n;
  end;
end;

procedure TMainForm.TreeNodeChanged(Sender: TObject);
const
  cParent = 'Parent = %s';
  cPrev = 'Prev = %s';
  cNext = 'Next = %s';
  cFirstSubNode = 'FirstSubnode = %s';
  cLastSubNode = 'LastSubnode = %s';
var
  n: TlqTreeNode;

  procedure PrintNodeInfo(ALabel: TlqLabel; AFormat: string; ANode: TlqTreeNode);
  begin
    if ANode = nil then
      ALabel.Text := Format(AFormat, ['nil'])
    else
      ALabel.Text := Format(AFormat, [ANode.Text]);
  end;

begin
  n := Tree.Selection;
  PrintNodeInfo(Label1, cParent, n.Parent);
  PrintNodeInfo(Label2, cPrev, n.Prev);
  PrintNodeInfo(Label3, cNext, n.Next);
  PrintNodeInfo(Label4, cFirstSubNode, n.FirstSubNode);
  PrintNodeInfo(Label5, cLastSubNode, n.LastSubNode);
end;

procedure TMainForm.PopulateTree;
var
  n: TlqTreeNode;
  i: integer;
  n2: TlqTreeNode;
  s: TlqString;
begin
  n := tree.RootNode.AppendText('Node 1');

  n.ImageIndex := 0;
  n.StateImageIndex := 1;
  n2 := n.AppendText('Node 1.1');
  n2.ImageIndex := 1;
  n2.StateImageIndex := 1;
  n2 := n.AppendText('Node 1.2');
  n2.ImageIndex := 1;
  n2.StateImageIndex := 0;
  n := tree.RootNode.AppendText('Node 2');
  n.ImageIndex := 0;
  n.AppendText('Node 2.1').ImageIndex := 1;
  n := n.AppendText('Node 2.2 The quick brownfox jumps over the...');
  n.ImageIndex := 1;
  for i := 1 to 3 do
  begin
    s := Format('Node 2.2.%d', [i]);
    if i = 2 then
      n2 := n.AppendText(s)
    else
      n.AppendText(s);
  end;
  n2 := n.Parent.AppendText('Node 2.3');
  n2.StateImageIndex := 0;
  tree.RootNode.FirstSubNode.Next.Collapse;
  tree.RootNode.AppendText('Node 3').ImageIndex := 0;
  tree.Selection := n;
//  n := tree.RootNode.FindSubNode('Node 2.2.2', True);
  if Assigned(n2) then
  begin
    n2.AppendText('Child 1').AppendText('Child 2');
    n2.Collapsed := False;
  end;

  TreeNodeChanged(nil);
end;

procedure TMainForm.btnSourceClicked(Sender: TObject);
begin
  FSrcNode := tree.Selection;
  if Assigned(FSrcNode) then
    lblSource.Text := FSrcNode.Text
  else
    lblSource.Text := '--';
end;

procedure TMainForm.btnDestinationClicked(Sender: TObject);
begin
  FDestNode := tree.Selection;
  if Assigned(FDestNode) then
    lblDestination.Text := FDestNode.Text
  else
    lblDestination.Text := '--';
end;

procedure TMainForm.btnMoveToClicked(Sender: TObject);
const
  cEmpty = '--';
var
  i: integer;
begin
  if FSrcNode = FDestNode then
  begin
    TlqMessageDialog.Warning('', 'Source and Destination may not be the same');
    exit;
  end;
  if (FSrcNode = nil) or (FDestnode = nil) then
  begin
    TlqMessageDialog.Warning('', 'Both Source and Destintation needs to be set first');
    exit;
  end;
  i := cbMoveToTypes.FocusItem;
  FSrcNode.MoveTo(FDestnode, TlqNodeAttachMode(i)); // This cast is a hack! Do not do this in real-world apps!!
  tree.FullExpand;
  tree.Invalidate;
  // reset values
  FSrcNode := nil;
  FDestnode := nil;
  lblSource.Text := cEmpty;
  lblDestination.Text := cEmpty;

  TreeNodeChanged(nil);
end;

procedure TMainForm.StateImageClicked(Sender: TObject; ANode: TlqTreeNode);
begin
  case ANode.StateImageIndex of
    0:  ANode.StateImageIndex := 1;
    1:  ANode.StateImageIndex := 0;
  end;
  tree.Invalidate;
end;

procedure TMainForm.btnShowCheckedClicked(Sender: TObject);
var
  n: TlqTreeNode;
  s: string;
begin
  s := 'These are all the nodes that have checkboxes and are checked:' + LineEnding;
  n := tree.RootNode;
  while n <> nil do
  begin
    if n.StateImageIndex = 1 then
      s += LineEnding + n.Text;
    n := Tree.NextNode(n);
  end;
  ShowMessage(s);
end;

procedure TMainForm.cbShowImagesChange(Sender: TObject);
begin
  tree.ShowImages := cbShowImages.Checked;
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // create a image list
  FImagelist := TlqImageList.Create;
  if lqFileExists('../../../images/folder_16.bmp') then
    FImagelist.AddItemFromFile(SetDirSeparators('../../../images/folder_16.bmp'), 0);
  if lqFileExists('../../../images/menu_preferences_16.bmp') then
  begin
    FImagelist.AddItemFromFile(SetDirSeparators('../../../images/menu_preferences_16.bmp'), 1);
    FImagelist.Items[1].Image.CreateMaskFromSample(0, 0);
    FImagelist.Items[1].Image.UpdateImage;
  end;

  InitializeCustomImages;
  FStateImagelist := TlqImageList.Create;
  FStateImagelist.AddImage(lqImages.GetImage('usr.state0'));
  FStateImagelist.AddImage(lqImages.GetImage('usr.state1'));

end;

destructor TMainForm.Destroy;
var
  i: integer;
begin
  tree.ImageList := nil;
  tree.StateImageList := nil;
  FImagelist.Free;
  for i := FStateImageList.Count-1 downto 0 do
    FStateImageList[i].Image := nil;
  FStateImageList.Free;
  inherited Destroy;
end;

procedure TMainForm.AfterCreate;
begin
  inherited AfterCreate;
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(399, 184, 709, 333);
  WindowTitle := 'Treeview Test';
  Hint := '';
  WindowPosition := wpScreenCenter;

  tree := TlqTreeView.Create(self);
  with tree do
  begin
    Name := 'tree';
    SetPosition(8, 8, 284, 267);
    Anchors := [anLeft,anTop,anBottom];
    FontDesc := '#Label1';
    Hint := '';
    ScrollWheelDelta := 30;
    ShowImages := True;
    TabOrder := 0;
    ImageList := FImagelist;
    StateImageList := FStateImagelist;
    OnChange  := @TreeNodeChanged;
    OnStateImageClicked  := @StateImageClicked;
  end;

  cbShowImages := TlqCheckBox.Create(self);
  with cbShowImages do
  begin
    Name := 'cbShowImages';
    SetPosition(8, 283, 109, 20);
    Anchors := [anLeft,anBottom];
    Checked := True;
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 1;
    Text := 'Show images';
    OnChange := @cbShowImagesChange;
  end;

  cbIndentNode := TlqCheckBox.Create(self);
  with cbIndentNode do
  begin
    Name := 'cbIndentNode';
    SetPosition(120, 283, 179, 20);
    Anchors := [anLeft,anBottom];
    Checked := True;
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 2;
    Text := 'Indent node with no image';
    OnChange := @cbIndentNodeChange;
  end;

  btnClear := TlqButton.Create(self);
  with btnClear do
  begin
    Name := 'btnClear';
    SetPosition(8, 305, 144, 23);
    Anchors := [anLeft,anBottom];
    Text := 'Clear Selected Node';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 3;
    OnClick := @btnClearClicked;
  end;

  Label1 := TlqLabel.Create(self);
  with Label1 do
  begin
    Name := 'Label1';
    SetPosition(316, 24, 292, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Label';
  end;

  Label2 := TlqLabel.Create(self);
  with Label2 do
  begin
    Name := 'Label2';
    SetPosition(316, 40, 288, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Label';
  end;

  Label3 := TlqLabel.Create(self);
  with Label3 do
  begin
    Name := 'Label3';
    SetPosition(316, 56, 272, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Label';
  end;

  Label4 := TlqLabel.Create(self);
  with Label4 do
  begin
    Name := 'Label4';
    SetPosition(316, 72, 300, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Label';
  end;

  Label5 := TlqLabel.Create(self);
  with Label5 do
  begin
    Name := 'Label5';
    SetPosition(316, 88, 320, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Label';
  end;

  lblSource := TlqLabel.Create(self);
  with lblSource do
  begin
    Name := 'lblSource';
    SetPosition(316, 172, 140, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := '--';
  end;

  lblDestination := TlqLabel.Create(self);
  with lblDestination do
  begin
    Name := 'lblDestination';
    SetPosition(468, 172, 128, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := '--';
  end;

  btnSource := TlqButton.Create(self);
  with btnSource do
  begin
    Name := 'btnSource';
    SetPosition(316, 144, 80, 24);
    Text := 'Source';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 11;
    OnClick := @btnSourceClicked;
  end;

  btnDest := TlqButton.Create(self);
  with btnDest do
  begin
    Name := 'btnDest';
    SetPosition(468, 144, 80, 24);
    Text := 'Destination';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 12;
    OnClick := @btnDestinationClicked;
  end;

  btnMoveTo := TlqButton.Create(self);
  with btnMoveTo do
  begin
    Name := 'btnMoveTo';
    SetPosition(612, 144, 80, 24);
    Text := 'MoveTo';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 13;
    OnClick := @btnMoveToClicked;
  end;

  cbMoveToTypes := TlqComboBox.Create(self);
  with cbMoveToTypes do
  begin
    Name := 'cbMoveToTypes';
    SetPosition(612, 172, 96, 22);
    ExtraHint := '';
    FontDesc := '#List';
    Hint := '';
    Items.Add('naAdd');
    Items.Add('naAddFirst');
    Items.Add('naAddChild');
    Items.Add('naAddChildFirst');
    Items.Add('naInsert');
    FocusItem := 0;
    TabOrder := 14;
  end;

  Label6 := TlqLabel.Create(self);
  with Label6 do
  begin
    Name := 'Label6';
    SetPosition(300, 4, 348, 16);
    FontDesc := '#Label2';
    Hint := '';
    Text := 'Selected Node Information';
  end;

  Label7 := TlqLabel.Create(self);
  with Label7 do
  begin
    Name := 'Label7';
    SetPosition(300, 120, 348, 16);
    FontDesc := '#Label2';
    Hint := '';
    Text := 'Reorder Nodes (testing MovoTo() function)';
  end;

  Label8 := TlqLabel.Create(self);
  with Label8 do
  begin
    Name := 'Label8';
    SetPosition(300, 200, 308, 16);
    FontDesc := '#Label2';
    Hint := '';
    Text := 'Test various Tree functions';
  end;

  btnCollapseAll := TlqButton.Create(self);
  with btnCollapseAll do
  begin
    Name := 'btnCollapseAll';
    SetPosition(316, 224, 92, 24);
    Text := 'Collapse All';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 19;
    OnClick := @btnCollapseAllClicked;
  end;

  btnExpandAll := TlqButton.Create(self);
  with btnExpandAll do
  begin
    Name := 'btnExpandAll';
    SetPosition(416, 224, 92, 24);
    Text := 'Expand All';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 20;
    OnClick := @btnExpandAllClicked;
  end;

  btnShowChecked := TlqButton.Create(self);
  with btnShowChecked do
  begin
    Name := 'btnShowChecked';
    SetPosition(160, 304, 100, 24);
    Text := 'Show Checked';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 21;
    OnClick  := @btnShowCheckedClicked;
  end;

  {@VFD_BODY_END: MainForm}

  PopulateTree;
end;

procedure TMainForm.btnCollapseAllClicked(Sender: TObject);
begin
  tree.FullCollapse;
end;

procedure TMainForm.btnExpandAllClicked(Sender: TObject);
begin
  tree.FullExpand;
end;


procedure MainProc;
var
  frm: TMainForm;
begin
  lqApplication.Initialize;
  frm := TMainForm.Create(nil);
  try
    frm.Show;
    lqApplication.Run;
  finally
    frm.Free;
  end;
end;

begin
  MainProc;
end.


