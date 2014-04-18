unit frm_main;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, lq_main,
  lq_widget, lq_form, lq_label, lq_button,
  lq_listbox, lq_panel, lqui_db, db, {dbf, dbf_fields};

type

  TMainForm = class(TlqForm)
  private
    DataSet: TDBF;
    DataSource: TDataSource;
    procedure   btnQuitClicked(Sender: TObject);
    procedure   btnFirstClick(Sender: TObject);
    procedure   btnPrevClick(Sender: TObject);
    procedure   btnNextClick(Sender: TObject);
    procedure   btnLastClick(Sender: TObject);
    procedure   ButtonEnter(Sender: TObject);
    procedure   ButtonExit(Sender: TObject);
    procedure   FormShow(Sender: TObject);
  public
    {@VFD_HEAD_BEGIN: MainForm}
    btnQuit: TlqButton;
    btnFirst: TlqButton;
    btnPrev: TlqButton;
    btnNext: TlqButton;
    btnLast: TlqButton;
    lstName1: TlqListBox;
    dblblName: TlqDBLabel;
    dblblEMail: TlqDBLabel;
    lblName1: TlqLabel;
    lblName2: TlqLabel;
    pnlName1: TlqBevel;
    lblName3: TlqLabel;
    lblName4: TlqLabel;
    lblStatusBar: TlqLabel;
    {@VFD_HEAD_END: MainForm}
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

implementation

{@VFD_NEWFORM_IMPL}

procedure TMainForm.btnQuitClicked(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.btnFirstClick(Sender: TObject);
begin
  DataSet.First;
end;

procedure TMainForm.btnPrevClick(Sender: TObject);
begin
  DataSet.Prior;
end;

procedure TMainForm.btnNextClick(Sender: TObject);
begin
  DataSet.Next;
end;

procedure TMainForm.btnLastClick(Sender: TObject);
begin
  DataSet.Last;
end;

procedure TMainForm.ButtonEnter(Sender: TObject);
begin
  lblStatusBar.Text := TlqButton(Sender).Hint;
end;

procedure TMainForm.ButtonExit(Sender: TObject);
begin
  lblStatusBar.Text := '';
end;

procedure TMainForm.FormShow(Sender: TObject);
var
  x: Integer;
  s: string;
begin
  dblblName.DataSource    := DataSource;
  dblblName.DataField     := 'Name';
  dblblEMail.DataSource   := DataSource;
  dblblEMail.DataField    := 'Address';

  DataSet.Open;
  while not DataSet.EOF do
  begin
    SetLength(s, 0);
    for x := 0 to DataSet.FieldCount - 2 do
      s := s + DataSet.Fields[x].AsString + ', ';
    s := s + DataSet.Fields[DataSet.FieldCount - 1].AsString;
    lstName1.Items.Add(s);
    DataSet.Next;
  end;
  DataSet.First;
end;

constructor TMainForm.Create(AOwner: TComponent);
//var
//  fields: TDbfFieldDefs;
begin
  inherited Create(AOwner);
  DataSet             := TDBF.Create(Self);
  DataSet.TableName   := 'test.dbf';
  
  // If you wanted to create a new DBF table
{
  fields := TDbfFieldDefs.Create(self);
  fields.Add('Name', ftString, 50);
  fields.Add('Address', ftString, 150);
  DataSet.CreateTableEx(fields); // <== Now we have an empty db table

  DataSet.Open;
  
  Dataset.Insert;  // <== Start inserting data
  Dataset.FieldByName('Name').AsString := 'Graeme Geldenhuys';
  Dataset.FieldByName('Address').AsString := 'graemeg@nospam.co.za';
  DataSet.Post;
}
  
  DataSource          := TDataSource.Create(Self);
  DataSource.DataSet  := DataSet;

  OnShow :=@FormShow;
end;

destructor TMainForm.Destroy;
begin
  DataSet.Close;
  DataSource.Free;
  DataSet.Free;
  inherited Destroy;
end;

procedure TMainForm.AfterCreate;
begin
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(225, 208, 417, 315);
  WindowTitle := 'LiteKit DB controls test';
  WindowPosition := wpScreenCenter;
  Sizeable := False;

  btnQuit := TlqButton.Create(self);
  with btnQuit do
  begin
    Name := 'btnQuit';
    SetPosition(332, 264, 75, 24);
    Text := 'Quit';
    FontDesc := '#Label1';
    ImageName := 'stdimg.quit';
    OnClick := @btnQuitClicked;
  end;

  btnFirst := TlqButton.Create(self);
  with btnFirst do
  begin
    Name := 'btnFirst';
    SetPosition(8, 264, 30, 24);
    Text := '<<';
    FontDesc := '#Label1';
    ImageName := '';
    Hint := 'First record';
    OnClick := @btnFirstClick;
    OnMouseEnter := @ButtonEnter;
    OnMouseExit := @ButtonExit;
  end;

  btnPrev := TlqButton.Create(self);
  with btnPrev do
  begin
    Name := 'btnPrev';
    SetPosition(40, 264, 30, 24);
    Text := '<';
    FontDesc := '#Label1';
    ImageName := '';
    Hint := 'Previous record';
    OnClick := @btnPrevClick;
    OnMouseEnter := @ButtonEnter;
    OnMouseExit := @ButtonExit;
  end;

  btnNext := TlqButton.Create(self);
  with btnNext do
  begin
    Name := 'btnNext';
    SetPosition(72, 264, 30, 24);
    Text := '>';
    FontDesc := '#Label1';
    ImageName := '';
    Hint := 'Next record';
    OnClick := @btnNextClick;
    OnMouseEnter := @ButtonEnter;
    OnMouseExit := @ButtonExit;
  end;

  btnLast := TlqButton.Create(self);
  with btnLast do
  begin
    Name := 'btnLast';
    SetPosition(104, 264, 30, 24);
    Text := '>>';
    FontDesc := '#Label1';
    ImageName := '';
    Hint := 'Last record';
    OnClick := @btnLastClick;
    OnMouseEnter := @ButtonEnter;
    OnMouseExit := @ButtonExit;
  end;

  lstName1 := TlqListBox.Create(self);
  with lstName1 do
  begin
    Name := 'lstName1';
    SetPosition(8, 24, 400, 156);
    FontDesc := '#List';
  end;

  dblblName := TlqDBLabel.Create(self);
  with dblblName do
  begin
    Name := 'dblblName';
    SetPosition(104, 208, 304, 20);
  end;

  dblblEMail := TlqDBLabel.Create(self);
  with dblblEMail do
  begin
    Name := 'dblblEMail';
    SetPosition(104, 228, 304, 20);
  end;

  lblName1 := TlqLabel.Create(self);
  with lblName1 do
  begin
    Name := 'lblName1';
    SetPosition(20, 208, 80, 16);
    Text := 'Name:';
    FontDesc := '#Label1';
  end;

  lblName2 := TlqLabel.Create(self);
  with lblName2 do
  begin
    Name := 'lblName2';
    SetPosition(20, 228, 80, 16);
    Text := 'E-mail:';
    FontDesc := '#Label1';
  end;

  pnlName1 := TlqBevel.Create(self);
  with pnlName1 do
  begin
    Name := 'pnlName1';
    SetPosition(0, 296, 416, 18);
    Anchors := [anLeft,anRight,anBottom];
    Style := bsLowered;
  end;

  lblName3 := TlqLabel.Create(self);
  with lblName3 do
  begin
    Name := 'lblName3';
    SetPosition(8, 4, 400, 16);
    Text := 'Available DB Records:';
    FontDesc := '#Label1';
  end;

  lblName4 := TlqLabel.Create(self);
  with lblName4 do
  begin
    Name := 'lblName4';
    SetPosition(8, 188, 168, 16);
    Text := 'Current record:';
    FontDesc := '#Label2';
  end;

  lblStatusBar := TlqLabel.Create(pnlName1);
  with lblStatusBar do
  begin
    Name := 'lblStatusBar';
    SetPosition(5, 1, 404, 16);
    Anchors := [anLeft,anRight,anTop];
    Text := '';
    FontDesc := '#Label1';
  end;

  {@VFD_BODY_END: MainForm}
end;


end.
