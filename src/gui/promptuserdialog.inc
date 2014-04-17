{%mainunit lq_dialogs.pas}

{$IFDEF read_interface}


  TlqPromptUserDialog = class(TlqBaseDialog)
  private
    lblTitle: TlqLabel;
    lblTitlePw: TlqLabel;
    function    GetUserID: TlqString;
    function    GetUserPassword: TlqString;
    procedure SetUserID(const AValue: TlqString);
  protected
    edUserID: TlqEdit;
    edPassword: TlqEdit;
    procedure   btnOKClick(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
    // Gof defines this Authenticate method a Hook operation in Template methods pattern.
    // Subclass will have to override and implement it. By default it returns True.
    function    Authenticate: boolean; virtual;
    procedure   Wiggle(const Seconds: integer = 1; const MovingPixels: integer = 5; const aSleep: integer = 75);
    property    UserID: TlqString read GetUserID write SetUserID;
    property    Password: TlqString read GetUserPassword;
  end;


  TlqPromptUserDbDialog = class(TlqPromptUserDialog)
  private
    lblDatabases: TlqLabel;
  protected
    aStringList: TStringList;
    cbDatabases: TlqComboBox;
    function    GetSelectedDatabase: TlqString; virtual;
    procedure   PopulateComboDb; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    property    Database: TlqString read GetSelectedDatabase;
  end;


{$ENDIF read_interface}



{$IFDEF read_implementation}


{ TlqPromptUserDialog }

function TlqPromptUserDialog.GetUserID: TlqString;
begin
  Result := edUserID.Text;
end;

function TlqPromptUserDialog.GetUserPassword: TlqString;
begin
  Result := edPassword.Text;
end;

procedure TlqPromptUserDialog.SetUserID(const AValue: TlqString);
begin
  edUserID.Text := AValue;
end;

procedure TlqPromptUserDialog.btnOKClick(Sender: TObject);
begin
  if Authenticate then
    inherited btnOKClick(Sender);
end;

constructor TlqPromptUserDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // reset dimensions
  Width     := 200;
  Height    := 140;
  MinWidth  := 200;
  MinHeight := 140;
  
  lblTitle := CreateLabel(self, 8, 8, rsUserName);
  edUserID := CreateEdit(self, 8, 28, 184, 0);
  edUserID.Anchors := [anLeft, anTop, anRight];

  lblTitlePw := CreateLabel(self, 8, 55, rsPassword);
  edPassword := CreateEdit(self, 8, 73, 184, 0);
  edPassword.Anchors := [anLeft, anTop, anRight];
  edPassword.PasswordMode := True;

  // reposition buttons
  btnCancel.Left  := Width-FDefaultButtonWidth-FSpacing;
  btnCancel.Top   := Height - FSpacing - btnCancel.Height;
  btnOK.Left      := btnCancel.Left-FDefaultButtonWidth-FSpacing;
  btnOK.Top       := btnCancel.Top;
  
  // now reset tab order
  edUserID.TabOrder     := 1;
  edPassword.TabOrder   := 2;
  btnOK.TabOrder        := 3;
  btnCancel.TabOrder    := 4;
end;

function TlqPromptUserDialog.Authenticate: boolean;
begin
  Result := True;
end;

procedure TlqPromptUserDialog.Wiggle(const Seconds: integer; const MovingPixels: integer; const aSleep: integer);
var
  MyTime: TDateTime;
begin
  MyTime := Now;
  repeat
    MoveWindow(Left - MovingPixels, Top);
    fpgApplication.ProcessMessages;
    Sleep(aSleep);
    MoveWindow(Left + MovingPixels * 2, Top);
    fpgApplication.ProcessMessages;
    Sleep(aSleep);
    MoveWindow(Left - MovingPixels, Top);
    fpgApplication.ProcessMessages;
  until SecondsBetween(Now, MyTime) >= Seconds;
end;

{ TlqPromptUserDbDialog }

function TlqPromptUserDbDialog.GetSelectedDatabase: TlqString;
begin
  Result := cbDatabases.Text;
end;

constructor TlqPromptUserDbDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // reset dimensions
  Width     := 200;
  Height    := 185;
  MinWidth  := 200;
  MinHeight := 185;

  aStringList := TStringList.Create;

  lblDatabases := CreateLabel(self, 8, 101, rsDatabase);
  cbDatabases := CreateComboBox(self, 8, 119, 184, aStringList);
  cbDatabases.Anchors := [anLeft, anTop, anRight];
  cbDatabases.Height := edUserID.Height;

  // reposition buttons
  btnCancel.Left  := Width-FDefaultButtonWidth-FSpacing;
  btnCancel.Top   := Height - FSpacing - btnCancel.Height;
  btnOK.Left      := btnCancel.Left-FDefaultButtonWidth-FSpacing;
  btnOK.Top       := btnCancel.Top;

  // now reset tab order
  edUserID.TabOrder     := 1;
  edPassword.TabOrder   := 2;
  cbDatabases.TabOrder  := 3;
  btnOK.TabOrder        := 5;
  btnCancel.TabOrder    := 6;
  
  PopulateComboDb;
end;

destructor TlqPromptUserDbDialog.Destroy;
begin
  aStringList.Free;
  inherited Destroy;
end;

procedure TlqPromptUserDbDialog.PopulateComboDb;
begin
  // do nothing. Derived classe will implement it
end;

{$ENDIF read_implementation}

