unit frm_main;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, lq_base, lq_main, lq_widget,
  lq_edit, lq_form, lq_label, lq_button,
  lq_dialogs, lq_menu, lq_checkbox,
  lq_panel, lq_colorwheel;

type

  TMainForm = class(TlqForm)
  private
    {@VFD_HEAD_BEGIN: MainForm}
    Button1: TlqButton;
    ColorWheel1: TlqColorWheel;
    ValueBar1: TlqValueBar;
    Bevel1: TlqBevel;
    Label1: TlqLabel;
    Label2: TlqLabel;
    Label3: TlqLabel;
    edH: TlqEdit;
    edS: TlqEdit;
    edV: TlqEdit;
    Label4: TlqLabel;
    Label5: TlqLabel;
    Label6: TlqLabel;
    edR: TlqEdit;
    edG: TlqEdit;
    edB: TlqEdit;
    Label7: TlqLabel;
    Label8: TlqLabel;
    Bevel2: TlqBevel;
    Label9: TlqLabel;
    chkCrossHair: TlqCheckBox;
    chkBGColor: TlqCheckBox;
    {@VFD_HEAD_END: MainForm}
    FViaRGB: Boolean; // to prevent recursive changes
    procedure btnQuitClicked(Sender: TObject);
    procedure chkCrossHairChange(Sender: TObject);
    procedure chkBGColorChange(Sender: TObject);
    procedure UpdateHSVComponents;
    procedure UpdateRGBComponents;
    procedure ColorChanged(Sender: TObject);
    procedure RGBChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

implementation


{@VFD_NEWFORM_IMPL}

procedure TMainForm.ColorChanged(Sender: TObject);
begin
  UpdateHSVComponents;
  if not FViaRGB then
    UpdateRGBComponents;
end;

procedure TMainForm.RGBChanged(Sender: TObject);
var
  rgb: TFPColor;
  c: TlqColor;
begin
  FViaRGB := True;  // revent recursive updates
  rgb.Red := StrToInt(edR.Text);
  rgb.Green := StrToInt(edG.Text);
  rgb.Blue := StrToInt(edB.Text);
  c := FPColorTofpgColor(rgb);
  ColorWheel1.SetSelectedColor(c);  // This will trigger ColorWheel and ValueBar OnChange event
  FViaRGB := False;
end;

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FViaRGB := False;
end;

procedure TMainForm.btnQuitClicked(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.chkCrossHairChange(Sender: TObject);
begin
  if chkCrossHair.Checked then
    ColorWheel1.CursorSize := 400
  else
    ColorWheel1.CursorSize := 5;
end;

procedure TMainForm.chkBGColorChange(Sender: TObject);
begin
  if chkBGColor.Checked then
  begin
    ColorWheel1.BackgroundColor := clBrown;
    ValueBar1.BackgroundColor := clBrown;
  end
  else
  begin
    ColorWheel1.BackgroundColor := clWindowBackground;
    ValueBar1.BackgroundColor := clWindowBackground;
  end;
end;

procedure TMainForm.UpdateHSVComponents;
begin
  edH.Text := IntToStr(ColorWheel1.Hue);
  edS.Text := FormatFloat('0.000', ColorWheel1.Saturation);
  edV.Text := FormatFloat('0.000', ValueBar1.Value);
  Bevel1.BackgroundColor := ValueBar1.SelectedColor;
end;

procedure TMainForm.UpdateRGBComponents;
var
  rgb: TFPColor;
  c: TlqColor;
begin
  c := ValueBar1.SelectedColor;
  rgb := lqColorToFPColor(c);
  edR.Text := IntToStr(rgb.Red);
  edG.Text := IntToStr(rgb.Green);
  edB.Text := IntToStr(rgb.Blue);
end;

procedure TMainForm.AfterCreate;
begin
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(349, 242, 537, 411);
  WindowTitle := 'ColorWheel test app';
  WindowPosition := wpUser;

  Button1 := TlqButton.Create(self);
  with Button1 do
  begin
    Name := 'Button1';
    SetPosition(447, 376, 80, 26);
    Anchors := [anRight,anBottom];
    Text := 'Quit';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 0;
    OnClick  := @btnQuitClicked;
  end;

  ColorWheel1 := TlqColorWheel.Create(self);
  with ColorWheel1 do
  begin
    Name := 'ColorWheel1';
    SetPosition(20, 20, 272, 244);
  end;

  ValueBar1 := TlqValueBar.Create(self);
  with ValueBar1 do
  begin
    Name := 'ValueBar1';
    SetPosition(304, 20, 52, 244);
    OnChange  := @ColorChanged;
  end;

  Bevel1 := TlqBevel.Create(self);
  with Bevel1 do
  begin
    Name := 'Bevel1';
    SetPosition(20, 288, 76, 56);
  end;

  Label1 := TlqLabel.Create(self);
  with Label1 do
  begin
    Name := 'Label1';
    SetPosition(116, 284, 52, 18);
    Alignment := taRightJustify;
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Hue';
  end;

  Label2 := TlqLabel.Create(self);
  with Label2 do
  begin
    Name := 'Label2';
    SetPosition(116, 316, 52, 18);
    Alignment := taRightJustify;
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Sat';
  end;

  Label3 := TlqLabel.Create(self);
  with Label3 do
  begin
    Name := 'Label3';
    SetPosition(116, 344, 52, 18);
    Alignment := taRightJustify;
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Val';
  end;

  edH := TlqEdit.Create(self);
  with edH do
  begin
    Name := 'edH';
    SetPosition(172, 280, 56, 26);
    TabOrder := 8;
    Text := '';
    FontDesc := '#Edit1';
    BackgroundColor := clWindowBackground;
  end;

  edS := TlqEdit.Create(self);
  with edS do
  begin
    Name := 'edS';
    SetPosition(172, 308, 56, 26);
    TabOrder := 9;
    Text := '';
    FontDesc := '#Edit1';
    BackgroundColor := clWindowBackground;
  end;

  edV := TlqEdit.Create(self);
  with edV do
  begin
    Name := 'edV';
    SetPosition(172, 336, 56, 26);
    TabOrder := 10;
    Text := '';
    FontDesc := '#Edit1';
    BackgroundColor := clWindowBackground;
  end;

  Label4 := TlqLabel.Create(self);
  with Label4 do
  begin
    Name := 'Label4';
    SetPosition(236, 284, 56, 18);
    Alignment := taRightJustify;
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Red';
  end;

  Label5 := TlqLabel.Create(self);
  with Label5 do
  begin
    Name := 'Label5';
    SetPosition(236, 316, 56, 18);
    Alignment := taRightJustify;
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Green';
  end;

  Label6 := TlqLabel.Create(self);
  with Label6 do
  begin
    Name := 'Label6';
    SetPosition(236, 344, 56, 18);
    Alignment := taRightJustify;
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Blue';
  end;

  edR := TlqEdit.Create(self);
  with edR do
  begin
    Name := 'edR';
    SetPosition(296, 280, 44, 26);
    TabOrder := 13;
    Text := '255';
    FontDesc := '#Edit1';
    OnExit  := @RGBChanged;
  end;

  edG := TlqEdit.Create(self);
  with edG do
  begin
    Name := 'edG';
    SetPosition(296, 308, 44, 26);
    TabOrder := 14;
    Text := '255';
    FontDesc := '#Edit1';
    OnExit := @RGBChanged;
  end;

  edB := TlqEdit.Create(self);
  with edB do
  begin
    Name := 'edB';
    SetPosition(296, 336, 44, 26);
    TabOrder := 15;
    Text := '255';
    FontDesc := '#Edit1';
    OnExit := @RGBChanged;
  end;

  Label7 := TlqLabel.Create(self);
  with Label7 do
  begin
    Name := 'Label7';
    SetPosition(108, 3, 80, 16);
    FontDesc := '#Label2';
    Hint := '';
    Text := 'ColorWheel';
  end;

  Label8 := TlqLabel.Create(self);
  with Label8 do
  begin
    Name := 'Label8';
    SetPosition(304, 3, 64, 16);
    FontDesc := '#Label2';
    Hint := '';
    Text := 'ValueBar';
  end;

  Bevel2 := TlqBevel.Create(self);
  with Bevel2 do
  begin
    Name := 'Bevel2';
    SetPosition(388, 8, 2, 260);
    Style := bsLowered;
  end;

  Label9 := TlqLabel.Create(self);
  with Label9 do
  begin
    Name := 'Label9';
    SetPosition(400, 3, 128, 16);
    Alignment := taCenter;
    FontDesc := '#Label2';
    Hint := '';
    Text := 'Custom Options';
  end;

  chkCrossHair := TlqCheckBox.Create(self);
  with chkCrossHair do
  begin
    Name := 'chkCrossHair';
    SetPosition(396, 32, 128, 20);
    FontDesc := '#Label1';
    TabOrder := 20;
    Text := 'Large CrossHair';
    OnChange  := @chkCrossHairChange;
  end;

  chkBGColor := TlqCheckBox.Create(self);
  with chkBGColor do
  begin
    Name := 'chkBGColor';
    SetPosition(396, 56, 132, 20);
    FontDesc := '#Label1';
    TabOrder := 21;
    Text := 'New BG Color';
    OnChange  := @chkBGColorChange;
  end;

  {@VFD_BODY_END: MainForm}

  // link the two components
  ColorWheel1.ValueBar := ValueBar1;
//  ColorWheel1.BackgroundColor := clFuchsia;
//  ValueBar1.BackgroundColor := clFuchsia;
//  ColorWheel1.CursorSize := 400;
end;


end.
