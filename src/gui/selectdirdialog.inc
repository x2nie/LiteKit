{%mainunit lq_dialogs.pas}

{$IFDEF read_interface}

  TlqSelectDirDialog = class(TlqBaseDialog)
  private
    tv: TlqTreeView;
    FRootDir: TlqString;
    FShowHidden: Boolean;
    FImagelist: TlqImageList;
    function    GetAbsolutePath(Node: TlqTreeNode): TlqString;
    procedure   InitializeTreeview;
    procedure   SetRootDir(const AValue: TlqString);
    procedure   AddDirectories(Node: TlqTreeNode; Dir: TlqString);
    procedure   NodeExpanded(Sender: TObject; ANode: TlqTreeNode);
    function    GetSelectedDir: TlqString;
    procedure   SetSelectedDir(const AValue: TlqString);
    {$IFDEF MSWINDOWS}
    procedure   AddWindowsDriveLetters;
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   AfterCreate; override;
    { return the selected directory or set initial selected dir }
    property    SelectedDir: TlqString read GetSelectedDir write SetSelectedDir;
    { Directory the treeview starts from }
    property    RootDirectory: TlqString read FRootDir write SetRootDir;
  end;


{$ENDIF read_interface}



{$IFDEF read_implementation}

function TlqSelectDirDialog.GetAbsolutePath(Node: TlqTreeNode): TlqString;
var
  lResult: TlqString;
begin
  lResult := '';
  while Node <> nil do
  begin
  {$IFDEF UNIX}
    if (Node.Text = PathDelim) then
      lResult := Node.Text + lResult
    else if (Node.Text <> '') then
      lResult := Node.Text + PathDelim + lResult;
  {$ENDIF}
  {$IFDEF MSWINDOWS}
    if (Node.Text <> '') then
    begin
      if (Node.Text[Length(Node.Text)] = PathDelim) then
        lResult := Node.Text + lResult
      else
        lResult := Node.Text + PathDelim + lResult;
    end;
  {$ENDIF}
    Node := Node.Parent;
  end;
  Result := lResult;
end;

procedure TlqSelectDirDialog.InitializeTreeview;
begin
  { I'm not sure what we should set these to. Maybe another Config option? }
  {$IFDEF UNIX}
  RootDirectory := '/';
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  RootDirectory := 'C:\';
  {$ENDIF}
  tv.RootNode.Expand;
end;

procedure TlqSelectDirDialog.SetRootDir(const AValue: TlqString);
var
  RootNode: TlqTreeNode;
  lNode: TlqTreeNode;
begin
  { Clear the list }
  tv.RootNode.Clear;
  FRootDir := AValue;

  {$IFDEF MSWINDOWS}
  { Add Windows drive letters }
  AddWindowsDriveLetters;
  {$ENDIF}

  { Remove the path delimeter unless this is root. }
  if FRootDir = '' then
    FRootDir := PathDelim;
  if (FRootDir <> PathDelim) and (FRootDir[length(FRootDir)] = PathDelim) then
    FRootDir := copy(FRootDir, 1, length(FRootDir) - 1);
  { Find or Create the root node and add it to the Tree View. }
  RootNode := tv.RootNode.FindSubNode(FRootDir + PathDelim, False);
//  RootNode := TV.Items.FindTopLvlNode(FRootDir + PathDelim);
  if RootNode = nil then
//    RootNode := TV.Items.Add(nil, FRootDir);
    RootNode := tv.RootNode.AppendText(FRootDir);

  { Add the Subdirectories to Root nodes }
//  lNode := TV.Items.GetFirstNode;
  lNode := RootNode;
  while lNode <> nil do
  begin
    AddDirectories(lNode, lNode.Text);
    lNode := lNode.Next;
//    lNode := lNode.GetNextSibling;
  end;

  { Set the original root node as the selected node. }
  tv.Selection := RootNode;
end;

{ Adds Subdirectories to a passed node if they exist }
procedure TlqSelectDirDialog.AddDirectories(Node: TlqTreeNode; Dir: TlqString);
var
  FileInfo: TSearchRec;
  NewNode: TlqTreeNode;
  i: integer;
  FCurrentDir: TlqString;
  //used to sort the directories.
  SortList: TStringList;
begin
  if Dir <> '' then
  begin
    FCurrentDir := Dir;
    FCurrentDir := lqAppendPathDelim(FCurrentDir);
    i           := length(FCurrentDir);
    FCurrentDir := FCurrentDir + AllFilesMask;
    try
      if lqFindFirst(FCurrentDir, faAnyFile or $00000080, FileInfo) = 0 then
      begin
        try
          SortList        := TStringList.Create;
          SortList.Sorted := True;
          repeat
            // check if special file
            if (FileInfo.Name = '.') or (FileInfo.Name = '..') or (FileInfo.Name = '') then
              Continue;
            { If hidden files or directories must be filtered, we test for
              dot files, considered hidden under unix type OS's. }
            //if not FShowHidden then
              //if (FileInfo.Name[1] in ['.']) then
                //Continue;

            { if this is a directory then add it to the tree. }
            if ((faDirectory and FileInfo.Attr) > 0) then
            begin
              { If this is a hidden file and we have not been requested to show
                hidden files then do not add it to the list. }
              //if ((faHidden and FileInfo.Attr) > 0) and not FShowHidden then
                //continue;

              SortList.Add(FileInfo.Name);
            end;
          until lqFindNext(FileInfo) <> 0;
          for i := 0 to SortList.Count - 1 do
          begin
            NewNode := Node.AppendText(SortList[i]);
            NewNode.ImageIndex := 0;
//            NewNode := TV.Items.AddChild(Node, SortList[i]);
            // if subdirectories then indicate so.
{ Todo: Fix this by adding HasChildren to Treeview }
            NewNode.HasChildren := lqHasSubDirs(lqAppendPathDelim(Dir) + NewNode.Text, FShowHidden);
          end;
        finally
          SortList.Free;
        end;
      end;  { if FindFirst... }
    finally
      SysUtils.FindClose(FileInfo);
    end;
  end;  { if Dir... }
  //if Node.Level = 0 then
    //Node.Text := Dir;
end;

procedure TlqSelectDirDialog.NodeExpanded(Sender: TObject; ANode: TlqTreeNode);
begin
  if ANode.Count = 0 then
    AddDirectories(ANode, GetAbsolutePath(ANode));
end;

function TlqSelectDirDialog.GetSelectedDir: TlqString;
begin
  Result := '';
  if tv.Selection <> nil then
    Result := GetAbsolutePath(tv.Selection);
end;

procedure TlqSelectDirDialog.SetSelectedDir(const AValue: TlqString);
var
  s: TlqString;
  dir: TlqString;
  i: integer;
  p: integer;
  prevn, nextn: TlqTreeNode;
begin
  if AValue = '' then
    Exit;
  s := lqAppendPathDelim(AValue);
  prevn := tv.RootNode;
  nextn := prevn;
  while nextn <> nil do
  begin
    if s = '' then
      break;
    i := UTF8Pos(PathDelim, s);
    if i = 1 then
      dir := PathDelim
    else
      dir := UTF8Copy(s, 1, i-1);
    UTF8Delete(s, 1, i);    // delete leading dir + PathDelim
    if (prevn = tv.RootNode) and (pos(':', dir) > 0) then
      dir += PathDelim;     // Windows drive letter. eg: C:\  or  D:\  etc.
    nextn := prevn.FindSubNode(dir, True);
    if Assigned(nextn) then
    begin
      prevn := nextn;
      prevn.Expand;
      NodeExpanded(self, prevn);
    end;
  end;
  tv.Selection := prevn;
end;

{$IFDEF MSWINDOWS}
procedure TlqSelectDirDialog.AddWindowsDriveLetters;
const
  MAX_DRIVES = 25;
var
  n: integer;
  drvs: string;
begin
  // making drive list, skipping drives A: and B:
  n := 2;
  while n <= MAX_DRIVES do
  begin
    drvs := chr(n + Ord('A')) + ':\';
    if Windows.GetDriveType(PChar(drvs)) <> 1 then
      TV.RootNode.AppendText(drvs);
    Inc(n);
  end;
end;
{$ENDIF}

constructor TlqSelectDirDialog.Create(AOwner: TComponent);
var
  img: TlqImage;
begin
  inherited Create(AOwner);
  FShowHidden := False;
  FImagelist := TlqImageList.Create;
  img := lqImages.GetImage('stdimg.folder').ImageFromSource;
  FImageList.AddImage(img);
end;

destructor TlqSelectDirDialog.Destroy;
begin
  FImagelist.Free;
  inherited Destroy;
end;

procedure TlqSelectDirDialog.AfterCreate;
begin
  inherited AfterCreate;
  Name := 'fpgSelectDirDialog';
  SetPosition(20, 20, 300, 370);
  WindowTitle := rsSelectaDirectory;
  WindowPosition := wpOneThirdDown;

  tv := TlqTreeView.Create(self);
  with tv do
  begin
    Name := 'tv';
    SetPosition(FSpacing, FSpacing, 288, 322);
    Anchors     := [anTop, anLeft, anRight, anBottom];
    ImageList   := FImageList;
    ShowImages  := True;
    OnExpand    := @NodeExpanded;
  end;

  // reposition buttons
  btnCancel.Left  := Width-FDefaultButtonWidth-FSpacing;
  btnCancel.Top   := Height - FSpacing - btnCancel.Height;
  btnOK.Left      := btnCancel.Left-FDefaultButtonWidth-FSpacing;
  btnOK.Top       := btnCancel.Top;

  // now reset tab order
  tv.TabOrder         := 1;
  btnOK.TabOrder      := 2;
  btnCancel.TabOrder  := 3;

  InitializeTreeview;
end;


{$ENDIF read_implementation}


