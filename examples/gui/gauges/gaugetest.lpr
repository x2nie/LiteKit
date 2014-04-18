program gaugetest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, Sysutils,
  lq_base,
  lq_main,
  lq_form,
  lq_button,
  lq_progressbar,
  lq_trackbar,
  lq_label,
  lq_imgfmt_bmp,
  lq_edit,
  lq_panel,
  lq_gauge;

type

  TGaugeTest = class(TlqForm)
  public
    {@VFD_HEAD_BEGIN: GaugeTest}
    CloseBtn: TlqButton;
    Gauge: TlqGauge;
    MinusBtn: TlqButton;
    PlusBtn: TlqButton;
    ProgressBar: TlqProgressBar;
    TrackBar: TlqTrackBar;
    VertGauge: TlqGauge;
    TextGauge: TlqGauge;
    lblName1: TlqLabel;
    lblName2: TlqLabel;
    lblName3: TlqLabel;
    lblName4: TlqLabel;
    NeedleGauge: TlqGauge;
    PieGauge: TlqGauge;
    DialGauge: TlqGauge;
    lblName5: TlqLabel;
    lblName6: TlqLabel;
    lblName7: TlqLabel;
    SmallNeedle: TlqGauge;
    {@VFD_HEAD_END: GaugeTest}
    procedure   AfterCreate; override;
    procedure   OnCloseClick (Sender:TObject);
    procedure   OnPlusClick (Sender:TObject);
    procedure   OnMinusClick (Sender:TObject);
    procedure   OnTrackBarChange (Sender: TObject; APosition: integer);
  end;

{@VFD_NEWFORM_DECL}


procedure TGaugeTest.AfterCreate;
begin
  {@VFD_BODY_BEGIN: GaugeTest}
  Name := 'GaugeTest';
  SetPosition(83, 160, 595, 379);
  WindowTitle := 'Gauge Test';
  WindowPosition:= wpScreenCenter;
  Sizeable := False;

  CloseBtn := TlqButton.Create(self);
  with CloseBtn do
  begin
    Name := 'CloseBtn';
    SetPosition(463, 329, 75, 24);
    Anchors := [anRight,anBottom];
    Text := 'Close';
    FontDesc := '#Label1';
    ImageName := 'stdimg.close';
    ModalResult := mrOK;
    OnClick:= @OnCloseClick;
  end;

  Gauge := TlqGauge.Create(self);
  with Gauge do
  begin
    Name := 'Gauge';
    SetPosition(124, 104, 150, 25);
    Kind := gkHorizontalBar;
  end;

  MinusBtn := TlqButton.Create(self);
  with MinusBtn do
  begin
    Name := 'MinusBtn';
    SetPosition(116, 329, 24, 24);
    Anchors := [anLeft,anBottom];
    Text := '-';
    FontDesc := '#Label1';
    ImageName := '';
    OnClick:= @OnMinusClick;
  end;

  PlusBtn := TlqButton.Create(self);
  with PlusBtn do
  begin
    Name := 'PlusBtn';
    SetPosition(384, 329, 24, 24);
    Anchors := [anLeft,anBottom];
    Text := '+';
    FontDesc := '#Label1';
    ImageName := '';
    OnClick:= @OnPlusClick;
  end;

  ProgressBar := TlqProgressBar.Create(self);
  with ProgressBar do
  begin
    Name := 'ProgressBar';
    SetPosition(124, 16, 150, 22);
    ShowCaption := True;
  end;

  TrackBar := TlqTrackBar.Create(self);
  with TrackBar do
  begin
    Name := 'TrackBar';
    SetPosition(164, 325, 200, 30);
    Anchors := [anLeft,anBottom];
    OnChange := @OnTrackBarChange;
  end;

  VertGauge := TlqGauge.Create(self);
  with VertGauge do
  begin
    Name := 'VertGauge';
    SetPosition(352, 32, 25, 100);
    Kind := gkVerticalBar;
    ShowText := False;
  end;

  TextGauge := TlqGauge.Create(self);
  with TextGauge do
  begin
    Name := 'TextGauge';
    SetPosition(124, 60, 75, 25);
    Kind := gkText;
  end;

  lblName1 := TlqLabel.Create(self);
  with lblName1 do
  begin
    Name := 'lblName1';
    SetPosition(16, 20, 92, 16);
    Text := 'Progress Bar';
    FontDesc := '#Label1';
  end;

  lblName2 := TlqLabel.Create(self);
  with lblName2 do
  begin
    Name := 'lblName2';
    SetPosition(16, 64, 80, 16);
    Text := 'Text Gauge';
    FontDesc := '#Label1';
  end;

  lblName3 := TlqLabel.Create(self);
  with lblName3 do
  begin
    Name := 'lblName3';
    SetPosition(16, 108, 108, 16);
    Text := 'Horizontal Gauge';
    FontDesc := '#Label1';
  end;

  lblName4 := TlqLabel.Create(self);
  with lblName4 do
  begin
    Name := 'lblName4';
    SetPosition(324, 12, 96, 16);
    Text := 'Vertical Gauge';
    FontDesc := '#Label1';
  end;

  NeedleGauge := TlqGauge.Create(self);
  with NeedleGauge do
  begin
    Name := 'NeedleGauge';
    SetPosition(472, 40, 100, 50);
    Kind := gkNeedle;
  end;

  PieGauge := TlqGauge.Create(self);
  with PieGauge do
  begin
    Name := 'PieGauge';
    SetPosition(124, 156, 120, 120);
    Kind := gkPie;
  end;

  DialGauge := TlqGauge.Create(self);
  with DialGauge do
  begin
    Name := 'DialGauge';
    SetPosition(356, 156, 120, 120);
    Kind := gkDial;
  end;

  lblName5 := TlqLabel.Create(self);
  with lblName5 do
  begin
    Name := 'lblName5';
    SetPosition(152, 288, 80, 16);
    Text := 'Pie Gauge';
    FontDesc := '#Label1';
  end;

  lblName6 := TlqLabel.Create(self);
  with lblName6 do
  begin
    Name := 'lblName6';
    SetPosition(384, 288, 80, 16);
    Text := 'Dial Gauge';
    FontDesc := '#Label1';
  end;

  lblName7 := TlqLabel.Create(self);
  with lblName7 do
  begin
    Name := 'lblName7';
    SetPosition(476, 100, 100, 16);
    Text := 'Needle Gauge';
    FontDesc := '#Label1';
  end;

  SmallNeedle := TlqGauge.Create(self);
  with SmallNeedle do
  begin
    Name := 'SmallNeedle';
    SetPosition(504, 160, 64, 32);
    Kind := gkNeedle;
  end;

  {@VFD_BODY_END: GaugeTest}
end;

procedure TGaugeTest.OnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TGaugeTest.OnPlusClick(Sender: TObject);
begin
  TrackBar.Position:= TrackBar.Position + 5;
  TrackBar.Invalidate;
  OnTrackBarChange(self,TrackBar.Position);
end;

procedure TGaugeTest.OnMinusClick(Sender: TObject);
begin
  TrackBar.Position:= TrackBar.Position - 5;
  TrackBar.Invalidate;
  OnTrackBarChange(self,TrackBar.Position);
end;

procedure TGaugeTest.OnTrackBarChange(Sender: TObject; APosition: integer);
begin
  Gauge.Progress         := APosition;
  ProgressBar.Position   := APosition;
  VertGauge.Progress     := APosition;
  TextGauge.Progress     := APosition;
  NeedleGauge.Progress   := APosition;
  PieGauge.Progress      := APosition;
  DialGauge.Progress     := APosition;
  SmallNeedle.Progress   := APosition;
end;


procedure MainProc;
var
  frm: TGaugeTest;
begin
  lqApplication.Initialize;
  frm := TGaugeTest.Create(nil);
  try
    frm.Show;
    lqApplication.Run;
  finally
    frm.Free;
  end;
end;


begin
  MainProc
end.


