program tabtest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  lq_main, lq_base, lq_widget, lq_form, lq_tab, lq_button,
  lq_label, lq_edit, lq_checkbox, lq_combobox;

type
  TMainForm = class(TlqForm)
  private
    btnQuit: TlqButton;
    pcMain: TlqPageControl;
    tsOne: TlqTabSheet;
    tsTwo: TlqTabSheet;
    tsThree: TlqTabSheet;
    tsFour: TlqTabSheet;
    btn2, btn3: TlqButton;
    chkSort: TlqCheckBox;
    cbTabPos: TlqComboBox;
    procedure   btnQuitClick(Sender: TObject);
    procedure   btn2Click(Sender: TObject);
    procedure   btn3Click(Sender: TObject);
    procedure   chkSortChange(Sender: TObject);
    procedure   cbTabPosChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TMainForm }

procedure TMainForm.btnQuitClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.btn2Click(Sender: TObject);
begin
  pcMain.ActivePageIndex := 0;
end;

procedure TMainForm.btn3Click(Sender: TObject);
var
  i: integer;
begin
  i := tsFour.PageIndex + 1;
  if i > pcMain.PageCount-1 then  // we reached the end so start from front
    i := 0;
    
  tsFour.PageIndex := i;
end;

procedure TMainForm.chkSortChange(Sender: TObject);
begin
  pcMain.SortPages := chkSort.Checked;
  btn3.Enabled := not chkSort.Checked;
end;

procedure TMainForm.cbTabPosChanged(Sender: TObject);
begin
  if cbTabPos.FocusItem = 0 then
    pcMain.TabPosition := tpTop
  else if cbTabPos.FocusItem = 1 then
    pcMain.TabPosition := tpBottom
  else if cbTabPos.FocusItem = 2 then
    pcMain.TabPosition := tpLeft
  else if cbTabPos.FocusItem = 3 then
    pcMain.TabPosition := tpRight
  else
    pcMain.TabPosition := tpNone;
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  WindowTitle := 'Tab control test';
  SetPosition(100, 100, 566, 350);
  
  btnQuit := CreateButton(self, 476, 320, 80, 'Quit', @btnQuitClick);
  btnQuit.ImageName := 'stdimg.Quit';
  btnQuit.ShowImage := True;
  btnQuit.Anchors := [anRight, anBottom];

  pcMain := TlqPageControl.Create(self);
  pcMain.Top      := 10;
  pcMain.Left     := 10;
  pcMain.Width    := Width - 20;
  pcMain.Height   := 300;
  pcMain.Anchors  := [anLeft, anTop, anRight, anBottom];
//  pcMain.FixedTabWidth:=150;

  // Tab One
  tsOne := TlqTabSheet.Create(pcMain);
  tsOne.Text := 'Tab One';
  CreateLabel(tsOne, 15, 50, 'TabSheet One');
  CreateLabel(tsOne, 15, 30, 'Resize form to see PageControl left/right buttons in action.');
  CreateEdit(tsOne, 15, 100, 150, 25);

  // Tab Two
  tsTwo := TlqTabSheet.Create(pcMain);
  tsTwo.Text := 'Tab Two';
  CreateLabel(tsTwo, 50, 50, 'TabSheet Two');
  CreateButton(tsTwo, 50, 100, 80, 'Button1', nil);

  // Tab Three
  tsThree := TlqTabSheet.Create(pcMain);
  tsThree.Text := 'Tab Three';
  CreateLabel(tsThree, 80, 50, 'TabSheet Three');
  
  // Tab Four
  tsFour := TlqTabSheet.Create(pcMain);
  tsFour.Text := 'This is one long text caption';
  tsFour.BackgroundColor := clMediumSeaGreen;
  
  pcMain.ActivePage := tsOne;

  btn2 := CreateButton(self, 10, 320, 80, 'Page 1', @btn2Click);
  btn2.Anchors := [anLeft, anBottom];

  btn3 := CreateButton(self, 100, 320, 80, 'Reorder Tab', @btn3Click);
  btn3.Anchors := [anLeft, anBottom];
  
  chkSort := CreateCheckBox(self, 190, 320, 'Sort Tabs');
  chkSort.Anchors := [anBottom, anLeft];
  chkSort.OnChange := @chkSortChange;
  
  cbTabPos := CreateComboBox(self, 300, 320, 80, nil);
  cbTabPos.Items.Add('tpTop');
  cbTabPos.Items.Add('tpBottom');
  cbTabPos.Items.Add('tpLeft');
  cbTabPos.Items.Add('tpRight');
  cbTabPos.Items.Add('tpNone');
  cbTabPos.FocusItem := 0;
  cbTabPos.Anchors := [anBottom, anLeft];
  cbTabPos.OnChange := @cbTabPosChanged;
end;

procedure MainProc;
var
  frm: TMainForm;
begin
  fpgApplication.Initialize;
  frm := TMainForm.Create(nil);
  frm.Show;
  fpgApplication.Run;
  frm.Free;
end;

begin
  MainProc;
end.

