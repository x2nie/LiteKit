unit lq_mdi;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lq_base, lq_main, lq_widget, lq_scrollbar, lq_panel,
  lq_button;

type
	// forward declarations
	TlqMDIChildForm = class;


	TlqMDIWorkArea = class(TlqWidget)
	private
		FHorBar: TlqScrollbar;
		FVerBar: TlqScrollbar;
		FList: TList;
		FActiveWindow: TlqMDIChildForm;
		procedure InternalMsgFreeMe(var msg: TlqMessageRec); message FPGM_FREEME;
		procedure SetActiveWindow(AValue: TlqMDIChildForm);
		function GetChildWindowCount: integer;
	protected
		procedure HandlePaint; override;
		procedure PositionScrollBars;
	public
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
		function AddWindow(AWindowClass: TlqFrameClass): TlqFrame;
		property ActiveWindow: TlqMDIChildForm read FActiveWindow write SetActiveWindow;
		property ChildWindowCount: integer read GetChildWindowCount;
	end;


	TlqMDIChildForm = class(TlqWidget)
	private
		{@VFD_HEAD_BEGIN: MDIChildForm}
		Panel1: TlqPanel;
		bevLeft: TlqBevel;
		Bevel2: TlqBevel;
		bevBottom: TlqBevel;
		Bevel4: TlqBevel;
		bevRight: TlqBevel;
		Button1: TlqButton;
		Button2: TlqButton;
		Button3: TlqButton;
		Button4: TlqButton;
		bvlClientArea: TlqBevel;
		{@VFD_HEAD_END: MDIChildForm}
		FMDIWorkArea: TlqMDIWorkArea;
		FWindowTitle: TlqString;
		FIsMouseDown: boolean;
		FLastPos: TPoint;
		FActive: boolean;
		procedure SetWindowTitle(AValue: TlqString);
		procedure TitleMouseMove(Sender: TObject; AShift: TShiftState; const AMousePos: TPoint);
		procedure TitleMouseUp(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
		procedure TitleMouseDown(Sender: TObject; AButton: TMouseButton; AShift: TShiftState; const AMousePos: TPoint);
		procedure TitleMouseExit(Sender: TObject);
		procedure CloseMDIWindowClicked(Sender: TObject);
		procedure SetActive(AValue: boolean);
	protected
		property Active: boolean read FActive write SetActive;
	public
		constructor Create(AOwner: TlqMDIWorkArea); reintroduce;
		property WindowTitle: TlqString read FWindowTitle write SetWindowTitle;
		procedure SetClientFrame(AFrame: TlqFrame);
		procedure UpdateWindowTitle;
		procedure Close;
	end;

implementation

uses
	dbugintf;

{ TlqMDIChildForm }

procedure TlqMDIChildForm.TitleMouseMove(Sender: TObject; AShift: TShiftState;
	const AMousePos: TPoint);
var
  dx, dy: integer;
  pt: TPoint;
begin
	pt := WindowToScreen(self, AMousePos);
	if not FIsMouseDown then
	begin
		FLastPos := pt;
		Exit;
	end;

	dx := pt.X - FLastPos.X;
	dy := pt.Y - FLastPos.Y;
	Left := Left + dx;
	Top := Top + dy;
	FLastPos := pt;
	UpdateWindowPosition;
end;

procedure TlqMDIChildForm.TitleMouseUp(Sender: TObject; AButton: TMouseButton;
	AShift: TShiftState; const AMousePos: TPoint);
begin
	FIsMouseDown := False;
	Panel1.ReleaseMouse;
end;

procedure TlqMDIChildForm.TitleMouseDown(Sender: TObject; AButton: TMouseButton;
	AShift: TShiftState; const AMousePos: TPoint);
begin
	FMDIWorkArea.ActiveWindow := self;
	FIsMouseDown := True;
	FLastPos := Panel1.WindowToScreen(self, AMousePos);
	Panel1.CaptureMouse;
end;

procedure TlqMDIChildForm.TitleMouseExit(Sender: TObject);
begin
//	FIsMouseDown := False;
end;

procedure TlqMDIChildForm.CloseMDIWindowClicked(Sender: TObject);
begin
	Close;
end;

procedure TlqMDIChildForm.SetActive(AValue: boolean);
begin
	if FActive = AValue then
		Exit;
	FActive := AValue;
	if FActive then
	begin
		Panel1.BackgroundColor := clNavy;
		bevLeft.BackgroundColor := clNavy;
		bevBottom.BackgroundColor := clNavy;
		bevRight.BackgroundColor := clNavy;
		Bevel2.BackgroundColor := clNavy;
		Bevel4.BackgroundColor := clNavy;
	end
	else
	begin
		Panel1.BackgroundColor := clMedGray;
		bevLeft.BackgroundColor := clMedGray;
		bevBottom.BackgroundColor := clMedGray;
		bevRight.BackgroundColor := clMedGray;
		Bevel2.BackgroundColor := clMedGray;
		Bevel4.BackgroundColor := clMedGray;
	end;
end;

procedure TlqMDIChildForm.SetWindowTitle(AValue: TlqString);
begin
	if FWindowTitle = AValue then
		Exit;
	FWindowTitle := AValue;
	if not (csLoading in ComponentState) then
		Panel1.Text := FWindowTitle;
end;

constructor TlqMDIChildForm.Create(AOwner: TlqMDIWorkArea);
begin
	inherited Create(AOwner);
	FMDIWorkArea := AOwner;
	FIsMouseDown := False;
	FLastPos := Point(0,0);
	{@VFD_BODY_BEGIN: MDIChildForm}
	Name := 'MDIChildForm';
	SetPosition(369, 166, 300, 250);
	WindowTitle := 'ChildForm1';
	Hint := '';

	Panel1 := TlqPanel.Create(self);
	with Panel1 do
	begin
		Name := 'Panel1';
		SetPosition(0, 0, 301, 24);
		Anchors := [anLeft,anRight,anTop];
		BackgroundColor := TlqColor($0A0081);
		FontDesc := '#Label2';
		Hint := '';
		Text := 'Window Title';
		TextColor := TlqColor($FFFFFF);
		OnMouseDown := @TitleMouseDown;
		OnMouseUp := @TitleMouseUp;
		OnMouseMove := @TitleMouseMove;
		OnMouseExit  := @TitleMouseExit;
	end;

	bevLeft := TlqBevel.Create(self);
	with bevLeft do
	begin
		Name := 'bevLeft';
		SetPosition(0, 24, 3, 211);
		Anchors := [anLeft,anTop,anBottom];
		BackgroundColor := TlqColor($000080);
		Hint := '';
		Shape := bsSpacer;
	end;

	Bevel2 := TlqBevel.Create(self);
	with Bevel2 do
	begin
		Name := 'Bevel2';
		SetPosition(0, 235, 16, 16);
		Anchors := [anLeft,anBottom];
		BackgroundColor := TlqColor($000080);
		Hint := '';
	end;

	bevBottom := TlqBevel.Create(self);
	with bevBottom do
	begin
		Name := 'bevBottom';
		SetPosition(16, 248, 269, 3);
		Anchors := [anLeft,anRight,anBottom];
		BackgroundColor := TlqColor($000080);
		Hint := '';
		Shape := bsSpacer;
	end;

	Bevel4 := TlqBevel.Create(self);
	with Bevel4 do
	begin
		Name := 'Bevel4';
		SetPosition(285, 235, 16, 16);
		Anchors := [anRight,anBottom];
		BackgroundColor := TlqColor($000080);
		Hint := '';
	end;

	bevRight := TlqBevel.Create(self);
	with bevRight do
	begin
		Name := 'bevRight';
		SetPosition(297, 24, 3, 211);
		Anchors := [anRight,anTop,anBottom];
		BackgroundColor := TlqColor($000080);
		Hint := '';
		Shape := bsSpacer;
	end;

	Button1 := TlqButton.Create(Panel1);
	with Button1 do
	begin
		Name := 'Button1';
		SetPosition(3, 4, 16, 16);
		Text := '-';
		Embedded := True;
		FontDesc := '#Grid';
		Hint := '';
		ImageName := '';
		TabOrder := 1;
		TextColor := TlqColor($000000);
	end;

	Button2 := TlqButton.Create(Panel1);
	with Button2 do
	begin
		Name := 'Button2';
		SetPosition(251, 4, 16, 16);
		Anchors := [anRight,anTop];
		Text := '_';
		Embedded := True;
		FontDesc := '#Grid';
		Hint := '';
		ImageName := '';
		TabOrder := 1;
		TextColor := TlqColor($000000);
	end;

	Button3 := TlqButton.Create(Panel1);
	with Button3 do
	begin
		Name := 'Button3';
		SetPosition(267, 4, 16, 16);
		Anchors := [anRight,anTop];
		Text := 'o';
		Embedded := True;
		FontDesc := '#Grid';
		Hint := '';
		ImageName := '';
		TabOrder := 1;
		TextColor := TlqColor($000000);
	end;

	Button4 := TlqButton.Create(Panel1);
	with Button4 do
	begin
		Name := 'Button4';
		SetPosition(283, 4, 16, 16);
		Anchors := [anRight,anTop];
		Text := 'X';
		Embedded := True;
		FontDesc := '#Grid';
		Hint := '';
		ImageName := '';
		TabOrder := 1;
		TextColor := TlqColor($000000);
		OnClick := @CloseMDIWindowClicked;
	end;

	bvlClientArea := TlqBevel.Create(self);
	with bvlClientArea do
	begin
		Name := 'bvlClientArea';
		SetPosition(2, 24, 296, 224);
		Anchors := [anLeft,anRight,anTop,anBottom];
		Hint := '';
		Shape := bsSpacer;
	end;

	{@VFD_BODY_END: MDIChildForm}
	Name := 'MDIChildForm' + IntToStr(Random(MaxInt));
end;

procedure TlqMDIChildForm.SetClientFrame(AFrame: TlqFrame);
begin
//	AFrame.Owner := bvlClientArea;
	AFrame.Align := alClient;
	AFrame.Visible := True;
	UpdateWindowTitle;
end;

procedure TlqMDIChildForm.UpdateWindowTitle;
begin
	Panel1.Text := FWindowTitle;
end;

procedure TlqMDIChildForm.Close;
begin
	// We can't free ourselves, somebody else needs to do it
	fpgPostMessage(Self, FMDIWorkArea, FPGM_FREEME);
end;

{ TlqMDIWorkArea }

procedure TlqMDIWorkArea.InternalMsgFreeMe(var msg: TlqMessageRec);
var
	i: integer;
begin
	if Assigned(msg.Sender) then
	begin
		if csDestroying in TComponent(msg.Sender).ComponentState then
			Exit;
		RemoveComponent(TlqMDIChildForm(msg.Sender));
		i := FList.IndexOf(TlqMDIChildForm(msg.Sender));
		if i = -1 then
			raise Exception.Create('Could not find MDI Child Form');
		FList.Delete(i);
		if FList.Count >= i+1 then
			{ set focus to next child window after the one just deleted }
			ActiveWidget := TlqMDIChildForm(FList.Items[i])
		else if FList.Count > 0 then
			{ fallback to the first child window we created }
			ActiveWidget := TlqMDIChildForm(FList.Items[0])
		else
			{ there simply isn't any more child windows }
			ActiveWidget := nil;
		TlqMDIChildForm(msg.Sender).Free;
	end;
end;

procedure TlqMDIWorkArea.SetActiveWindow(AValue: TlqMDIChildForm);
var
  i: integer;
  w: TlqMDIChildForm;
begin
	if FActiveWindow = AValue then
		Exit;
	FActiveWindow := AValue;
	FActiveWindow.BringToFront;
	ActiveWidget := FActiveWindow;
	for i := 0 to FList.Count-1 do
	begin
		w := TlqMDIChildForm(FList[i]);
		w.Active := (w = AValue);
	end;
end;

function TlqMDIWorkArea.GetChildWindowCount: integer;
begin
	Result := FList.Count;
end;

procedure TlqMDIWorkArea.HandlePaint;
begin
	inherited HandlePaint;
	Canvas.Clear(clLtGray);
end;

procedure TlqMDIWorkArea.PositionScrollBars;
begin
	FHorBar.Left := Left;
	FHorBar.Top := Height - FHorBar.Height;
	FHorBar.Width := Width;
	FHorBar.Anchors := [anLeft, anBottom, anRight];
	FVerBar.Left := Width - FVerBar.Width;
	FVerBar.Top := 0;
	FVerBar.Height := Height;
	FVerBar.Anchors := [anRight, anTop, anBottom];
end;

constructor TlqMDIWorkArea.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
	FIsContainer := True;
	FHorBar := TlqScrollbar.Create(self);
	FHorBar.Visible := False;
	FHorBar.Orientation := orHorizontal;
	FVerBar := TlqScrollbar.Create(self);
	FVerBar.Visible := False;
	FVerBar.Orientation := orVertical;
	PositionScrollBars;
	FList := TList.Create;
	FActiveWindow := nil;
end;

destructor TlqMDIWorkArea.Destroy;
begin
	FList.Free;
	inherited Destroy;
end;

function TlqMDIWorkArea.AddWindow(AWindowClass: TlqFrameClass): TlqFrame;
var
	frm: TlqMDIChildForm;
begin
	frm := TlqMDIChildForm.Create(self);
	Result := AWindowClass.Create(frm.bvlClientArea);
	frm.SetClientFrame(Result);
	FList.Add(frm);
	ActiveWindow := frm;
end;

end.

