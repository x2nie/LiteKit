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
      This unit contains the Color Selection dialog.
}


{%mainunit lq_dialogs.pas}

{$IFDEF read_interface}

type

  TlqColorSelectDialog = class(TlqBaseDialog)
  private
    {@VFD_HEAD_BEGIN: ColorSelectDialog}
    pcColorSelect: TlqPageControl;
    tsColorWheel: TlqTabSheet;
    tsColorNames: TlqTabSheet;
    cbColorPalette: TlqComboBox;
    ColorListBox1: TlqColorListBox;
    Label1: TlqLabel;
    Label2: TlqLabel;
    ColorWheel: TlqColorWheel;
    ValueBar: TlqValueBar;
    edR: TlqSpinEdit;
    edG: TlqSpinEdit;
    edB: TlqSpinEdit;
    Label3: TlqLabel;
    Label4: TlqLabel;
    Label5: TlqLabel;
    pnlColorPreview: TlqBevel;
    {@VFD_HEAD_END: ColorSelectDialog}
    FViaRGB: Boolean;  // to prevent recursive changes
    function    GetSelectedColor: TlqColor;
    procedure   SetSelectedColor(const AValue: TlqColor);
    procedure   ColorChanged(Sender: TObject);
    procedure   RGBChanged(Sender: TObject);
    procedure   UpdateRGBComponents;
    procedure   PopulatePaletteColorCombo;
    procedure   cbColorPaletteChange(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure   AfterCreate; override;
    property    SelectedColor: TlqColor read GetSelectedColor write SetSelectedColor;
  end;


{$ENDIF read_interface}



{$IFDEF read_implementation}


function fpgSelectColorDialog(APresetColor: TlqColor): TlqColor;
var
  frm: TlqColorSelectDialog;
begin
  Result := APresetColor;
  frm := TlqColorSelectDialog.Create(nil);
  try
    frm.ColorWheel.SetSelectedColor(APresetColor);
    if frm.ShowModal = mrOK then
      Result := frm.SelectedColor;
  finally
    frm.Free;
  end;
end;

{ TlqColorSelectDialog }

function TlqColorSelectDialog.GetSelectedColor: TlqColor;
begin
  if pcColorSelect.ActivePageIndex = 0 then
    Result := ValueBar.SelectedColor
  else
    Result := ColorListBox1.Color;
end;

procedure TlqColorSelectDialog.SetSelectedColor(const AValue: TlqColor);
begin
  ColorWheel.SetSelectedColor(AValue);
end;

procedure TlqColorSelectDialog.ColorChanged(Sender: TObject);
begin
//  UpdateHSVComponents;
  if not FViaRGB then
    UpdateRGBComponents;
  pnlColorPreview.BackgroundColor := ValueBar.SelectedColor;
end;

procedure TlqColorSelectDialog.RGBChanged(Sender: TObject);
var
  rgb: TFPColor;
  c: TlqColor;
begin
  FViaRGB     := True;  // prevent recursive updates
  rgb.Red     := edR.Value;
  rgb.Green   := edG.Value;
  rgb.Blue    := edB.Value;
  c := FPColorTofpgColor(rgb);
  ColorWheel.SetSelectedColor(c);  // This will trigger ColorWheel and ValueBar OnChange event
  FViaRGB     := False;
end;

procedure TlqColorSelectDialog.UpdateRGBComponents;
var
  rgb: TFPColor;
  c: TlqColor;
begin
  c := ValueBar.SelectedColor;
  rgb := fpgColorToFPColor(c);
  edR.Value := rgb.Red;
  edG.Value := rgb.Green;
  edB.Value := rgb.Blue;
end;

procedure TlqColorSelectDialog.PopulatePaletteColorCombo;
begin
  cbColorPalette.Items.Clear;
  cbColorPalette.Items.Add('cpStandardColors');
  cbColorPalette.Items.Add('cpSystemColors');
  cbColorPalette.Items.Add('cpWebColors');
  cbColorPalette.FocusItem := 0;
  cbColorPalette.OnChange := @cbColorPaletteChange;
end;

procedure TlqColorSelectDialog.cbColorPaletteChange(Sender: TObject);
begin
  if cbColorPalette.Text = 'cpStandardColors' then
    ColorListBox1.ColorPalette := cpStandardColors
  else if cbColorPalette.Text = 'cpSystemColors' then
    ColorListBox1.ColorPalette := cpSystemColors
  else
    ColorListBox1.ColorPalette := cpWebColors;
  ColorListBox1.SetFocus;
end;

constructor TlqColorSelectDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FViaRGB := false;
end;


procedure TlqColorSelectDialog.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: ColorSelectDialog}
  Name := 'ColorSelectDialog';
  SetPosition(340, 164, 328, 375);
  WindowTitle := 'Color Select Dialog';
  Hint := '';
  WindowPosition := wpOneThirdDown;

  pcColorSelect := TlqPageControl.Create(self);
  with pcColorSelect do
  begin
    Name := 'pcColorSelect';
    SetPosition(4, 4, 320, 332);
    Anchors := [anLeft,anRight,anTop,anBottom];
    ActivePageIndex := 0;
    Hint := '';
    TabOrder := 1;
  end;

  tsColorWheel := TlqTabSheet.Create(pcColorSelect);
  with tsColorWheel do
  begin
    Name := 'tsColorWheel';
    SetPosition(3, 24, 314, 305);
    Text := 'Color Wheel';
  end;

  tsColorNames := TlqTabSheet.Create(pcColorSelect);
  with tsColorNames do
  begin
    Name := 'tsColorNames';
    SetPosition(3, 24, 314, 305);
    Text := 'Predefined';
  end;

  cbColorPalette := TlqComboBox.Create(tsColorNames);
  with cbColorPalette do
  begin
    Name := 'cbColorPalette';
    SetPosition(8, 24, 299, 22);
    Anchors := [anLeft,anRight,anTop];
    FontDesc := '#List';
    Hint := '';
    TabOrder := 1;
  end;

  ColorListBox1 := TlqColorListBox.Create(tsColorNames);
  with ColorListBox1 do
  begin
    Name := 'ColorListBox1';
    SetPosition(8, 72, 299, 224);
    Anchors := [anLeft,anRight,anTop,anBottom];
    Color := TlqColor($00FFFF);
    FontDesc := '#List';
    Hint := '';
    TabOrder := 2;
  end;

  Label1 := TlqLabel.Create(tsColorNames);
  with Label1 do
  begin
    Name := 'Label1';
    SetPosition(8, 6, 328, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Select a color palette';
  end;

  Label2 := TlqLabel.Create(tsColorNames);
  with Label2 do
  begin
    Name := 'Label2';
    SetPosition(8, 54, 328, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Available colors:';
  end;

  ColorWheel := TlqColorWheel.Create(tsColorWheel);
  with ColorWheel do
  begin
    Name := 'ColorWheel';
    SetPosition(8, 8, 204, 204);
  end;

  ValueBar := TlqValueBar.Create(tsColorWheel);
  with ValueBar do
  begin
    Name := 'ValueBar';
    SetPosition(240, 8, 64, 204);
    Value := 1;
    OnChange := @ColorChanged;
  end;

  edR := TlqSpinEdit.Create(tsColorWheel);
  with edR do
  begin
    Name := 'edR';
    SetPosition(92, 216, 52, 24);
    MaxValue := 255;
    MinValue := 0;
    OnChange := @RGBChanged;
  end;

  edG := TlqSpinEdit.Create(tsColorWheel);
  with edG do
  begin
    Name := 'edG';
    SetPosition(92, 244, 52, 24);
    MaxValue := 255;
    MinValue := 0;
    OnChange := @RGBChanged;
  end;

  edB := TlqSpinEdit.Create(tsColorWheel);
  with edB do
  begin
    Name := 'edB';
    SetPosition(92, 272, 52, 24);
    MaxValue := 255;
    MinValue := 0;
    OnChange := @RGBChanged;
  end;

  Label3 := TlqLabel.Create(tsColorWheel);
  with Label3 do
  begin
    Name := 'Label3';
    SetPosition(8, 220, 80, 16);
    Alignment := taRightJustify;
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Red';
  end;

  Label4 := TlqLabel.Create(tsColorWheel);
  with Label4 do
  begin
    Name := 'Label4';
    SetPosition(8, 248, 80, 16);
    Alignment := taRightJustify;
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Green';
  end;

  Label5 := TlqLabel.Create(tsColorWheel);
  with Label5 do
  begin
    Name := 'Label5';
    SetPosition(8, 276, 80, 16);
    Alignment := taRightJustify;
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Blue';
  end;

  pnlColorPreview := TlqBevel.Create(tsColorWheel);
  with pnlColorPreview do
  begin
    Name := 'pnlColorPreview';
    SetPosition(248, 232, 52, 52);
    Hint := '';
  end;

  {@VFD_BODY_END: ColorSelectDialog}
  {%endregion}

  // link colorwheel and valuebar
  ColorWheel.ValueBar := ValueBar;

  // position standard dialog buttons
  btnCancel.Left  := Width - FDefaultButtonWidth - FSpacing;
  btnCancel.Top   := Height - btnCancel.Height - FSpacing;
  btnOK.Left      := btnCancel.Left - FDefaultButtonWidth - 6;
  btnOK.Top       := btnCancel.Top;
  
  PopulatePaletteColorCombo;
end;


{$ENDIF read_implementation}

