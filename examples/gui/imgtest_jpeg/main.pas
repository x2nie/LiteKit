unit main;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, lq_base, lq_main, lq_form, lq_panel, lq_button,
  lq_radiobutton, lq_dialogs, lq_imgfmt_jpg;

type

  TfrmMain = class(TlqForm)
  private
    {@VFD_HEAD_BEGIN: frmMain}
    Panel1: TlqPanel;
    RadioButton1: TlqRadioButton;
    RadioButton2: TlqRadioButton;
    RadioButton3: TlqRadioButton;
    RadioButton4: TlqRadioButton;
    Button1: TlqButton;
    Button2: TlqButton;
    {@VFD_HEAD_END: frmMain}
    FImage: TlqImage;
    FImageName: string;
    SizeSelect: integer;
    procedure Btn1Click(Sender: TObject);
    procedure Btn2Click(Sender: TObject);
    procedure rbChanged(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure LoadImage;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}
 var frmMain: TfrmMain;
  
implementation
  
{@VFD_NEWFORM_IMPL}

procedure TfrmMain.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: frmMain}
  Name := 'frmMain';
  SetPosition(321, 289, 330, 380);
  WindowTitle := 'JPEG Image Test';
  WindowPosition := wpOneThirdDown;
  Hint := '';

  Panel1 := TlqPanel.Create(self);
  with Panel1 do
  begin
    Name := 'Panel1';
    SetPosition(16, 260, 96, 112);
    FontDesc := '#Label1';
    Hint := '';
    Text := '';
  end;

  RadioButton1 := TlqRadioButton.Create(Panel1);
  with RadioButton1 do
  begin
    Name := 'RadioButton1';
    SetPosition(8, 8, 80, 20);
    Checked := True;
    FontDesc := '#Label1';
    GroupIndex := 0;
    Hint := '';
    TabOrder := 1;
    Text := 'Full size';
    Tag:=1;
    OnChange := @rbChanged;
  end;

  RadioButton2 := TlqRadioButton.Create(Panel1);
  with RadioButton2 do
  begin
    Name := 'RadioButton2';
    SetPosition(8, 32, 72, 20);
    FontDesc := '#Label1';
    GroupIndex := 0;
    Hint := '';
    TabOrder := 1;
    Text := '1/2 size';
    Tag:=2;
    OnChange := @rbChanged;
  end;

  RadioButton3 := TlqRadioButton.Create(Panel1);
  with RadioButton3 do
  begin
    Name := 'RadioButton3';
    SetPosition(8, 56, 72, 20);
    FontDesc := '#Label1';
    GroupIndex := 0;
    Hint := '';
    TabOrder := 2;
    Text := '1/4 size';
    Tag:=3;
    OnChange := @rbChanged;
  end;

  RadioButton4 := TlqRadioButton.Create(Panel1);
  with RadioButton4 do
  begin
    Name := 'RadioButton4';
    SetPosition(8, 80, 72, 20);
    FontDesc := '#Label1';
    GroupIndex := 0;
    Hint := '';
    TabOrder := 3;
    Text := '1/8 size';
    Tag:=4;
    OnChange := @rbChanged;
  end;

  Button1 := TlqButton.Create(self);
  with Button1 do
  begin
    Name := 'Button1';
    SetPosition(216, 260, 80, 24);
    Text := 'Open';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 2;
    OnClick:=@Btn1Click;
  end;

  Button2 := TlqButton.Create(self);
  with Button2 do
  begin
    Name := 'Button2';
    SetPosition(216, 304, 80, 24);
    Text := 'Close';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 3;
    OnClick:=@Btn2Click;
  end;

  {@VFD_BODY_END: frmMain}
  {%endregion}
end;

procedure TfrmMain.LoadImage;
begin
  WindowTitle := FImageName;
  FImage := LoadImage_JPG(FImageName, SizeSelect);
  RePaint;   
end;

constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnPaint := @FormPaint;
  SizeSelect := 1; // full size by default
end;

destructor TfrmMain.Destroy;
begin
  FImage.Free;
  inherited Destroy;
end;
  
procedure TfrmMain.Btn1Click(Sender: TObject);
var
  dlg: TlqFileDialog;
begin
  dlg := TlqFileDialog.Create(nil);
  try
    dlg.Filter := 'JPEG Image (*.jpg)|*.jpg';
    if dlg.RunOpenFile then
    begin	    
       FImageName:= dlg.FileName;
    end;  
  finally
    dlg.Free;
  end;
   LoadImage;
end;   
  
procedure TfrmMain.Btn2Click(Sender: TObject);
begin
  Close;
end;  

procedure TfrmMain.FormPaint(Sender: TObject);
begin
  Canvas.DrawImage(0, 0, FImage);
end;

procedure TfrmMain.rbChanged(Sender: TObject);
begin
  if Sender is TlqRadioButton then
    SizeSelect := TlqRadioButton(Sender).Tag;
  LoadImage;
end;
    
end.
