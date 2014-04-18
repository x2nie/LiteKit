unit tabtest;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, gfxbase, lqfx, gui_edit, 
  gfx_widget, gui_form, gui_label, gui_button,
  gui_listbox, gui_memo, gui_combobox, gui_grid, 
  gui_dialogs, gui_checkbox, gui_tree, gui_trackbar, 
  gui_progressbar, gui_radiobutton, gui_tab, gui_menu,
  gui_bevel;

type

  TfraGeneral = class(TlqForm)
  public
    {@VFD_HEAD_BEGIN: fraGeneral}
    lblName1: TlqLabel;
    edtName1: TlqEdit;
    memName1: TlqMemo;
    btnName1: TlqButton;
    {@VFD_HEAD_END: fraGeneral}
    procedure AfterCreate; override;
  end;


  TfraAddress = class(TlqForm)
  public
    {@VFD_HEAD_BEGIN: fraAddress}
    edtName1: TlqEdit;
    lblName1: TlqLabel;
    lblName2: TlqLabel;
    lblName3: TlqLabel;
    cbName1: TlqComboBox;
    {@VFD_HEAD_END: fraAddress}
    procedure AfterCreate; override;
  end;


  TfrmTabTest = class(TlqForm)
  private
    procedure btnCloseClicked(Sender: TObject);
  public
    {@VFD_HEAD_BEGIN: frmTabTest}
    pcName1: TlqPageControl;
    btnClose: TlqButton;
    {@VFD_HEAD_END: frmTabTest}
    
    // these must be added manually for now, until the GUI Designer is improved.
    tsGeneral: TlqTabSheet;
    tsAddress: TlqTabSheet;
    tsAccounts: TlqTabSheet;
    fraGeneral: TfraGeneral;
    fraAddress: TfraAddress;
    procedure AfterCreate; override;
  end;


{@VFD_NEWFORM_DECL}

implementation

{@VFD_NEWFORM_IMPL}

procedure TfraAddress.AfterCreate;
begin
  {@VFD_BODY_BEGIN: fraAddress}
  Name := 'fraAddress';
  SetPosition(602, 485, 208, 127);
  WindowTitle := 'fraAddress';

  edtName1 := TlqEdit.Create(self);
  with edtName1 do
  begin
    Name := 'edtName1';
    SetPosition(64, 32, 120, 22);
    Text := '';
    FontDesc := '#Edit1';
  end;

  lblName1 := TlqLabel.Create(self);
  with lblName1 do
  begin
    Name := 'lblName1';
    SetPosition(4, 4, 156, 16);
    Text := 'TabSheet - Address';
    FontDesc := '#Label2';
  end;

  lblName2 := TlqLabel.Create(self);
  with lblName2 do
  begin
    Name := 'lblName2';
    SetPosition(8, 36, 44, 16);
    Text := 'Street';
    FontDesc := '#Label1';
  end;

  lblName3 := TlqLabel.Create(self);
  with lblName3 do
  begin
    Name := 'lblName3';
    SetPosition(8, 60, 48, 16);
    Text := 'City';
    FontDesc := '#Label1';
  end;

  cbName1 := TlqComboBox.Create(self);
  with cbName1 do
  begin
    Name := 'cbName1';
    SetPosition(64, 56, 120, 23);
    Items.Add('Somerset West');
    Items.Add('Cape Town');
    Items.Add('Durban');
    Items.Add('Jo''burg');
    Items.Add('Pretoria');
    FontDesc := '#List';
    FocusItem := 1;
  end;

  {@VFD_BODY_END: fraAddress}
end;


procedure TfraGeneral.AfterCreate;
begin
  {@VFD_BODY_BEGIN: fraGeneral}
  Name := 'fraGeneral';
  SetPosition(611, 290, 197, 165);
  WindowTitle := 'fraGeneral';

  lblName1 := TlqLabel.Create(self);
  with lblName1 do
  begin
    Name := 'lblName1';
    SetPosition(4, 4, 212, 16);
    Text := 'TabSheet - General';
    FontDesc := '#Label2';
  end;

  edtName1 := TlqEdit.Create(self);
  with edtName1 do
  begin
    Name := 'edtName1';
    SetPosition(12, 28, 144, 22);
    Anchors := [anLeft,anRight,anTop];
    Text := '';
    FontDesc := '#Edit1';
  end;

  memName1 := TlqMemo.Create(self);
  with memName1 do
  begin
    Name := 'memName1';
    SetPosition(12, 56, 172, 96);
    Anchors := [anLeft,anRight,anTop,anBottom];
    FontDesc := '#Edit1';
  end;

  btnName1 := TlqButton.Create(self);
  with btnName1 do
  begin
    Name := 'btnName1';
    SetPosition(160, 28, 19, 20);
    Anchors := [anRight,anTop];
    Text := '...';
    FontDesc := '#Label1';
    ImageName := '';
    ModalResult := 0;
  end;

  {@VFD_BODY_END: fraGeneral}
end;

procedure TfrmTabTest.btnCloseClicked(Sender: TObject);
begin
  Close;
end;

procedure TfrmTabTest.AfterCreate;
begin
  {@VFD_BODY_BEGIN: frmTabTest}
  Name := 'frmTabTest';
  SetPosition(293, 290, 275, 198);
  WindowTitle := 'Tab Design Test';

  pcName1 := TlqPageControl.Create(self);
  with pcName1 do
  begin
    Name := 'pcName1';
    SetPosition(8, 12, 258, 148);
    Anchors := [anLeft,anRight,anTop,anBottom];
    FixedTabWidth := 0;
    Style := tsTabs;
    TabPosition := tpTop;
    tsGeneral   := AppendTabSheet('General');
    tsAddress   := AppendTabSheet('Address');
    tsAccounts  := AppendTabSheet('EAddress');
    ActivePage  := tsGeneral;
  end;

  btnClose := TlqButton.Create(self);
  with btnClose do
  begin
    Name := 'btnClose';
    SetPosition(192, 168, 75, 24);
    Anchors := [anRight,anBottom];
    Text := 'Close';
    FontDesc := '#Label1';
    ImageName := '';
    ModalResult := 0;
    OnClick := @btnCloseClicked;
  end;

  {@VFD_BODY_END: frmTabTest}

  fraGeneral := TfraGeneral.Create(tsGeneral);
  fraGeneral.Align := alClient;
  fraAddress := TfraAddress.Create(tsAddress);
  fraAddress.Align := alClient;
end;

end.
