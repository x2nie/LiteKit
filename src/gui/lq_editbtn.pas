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
      This unit contains various "composite" components. Components that
      work together as a single component.
}

unit lq_editbtn;

{$mode objfpc}{$H+}

interface

uses
  Classes
  ,lq_base
  ,lq_main
  ,lq_widget
  ,lq_edit
  ,lq_button
  ,lq_panel
  ;

type

  TlqBaseEditButton = class(TlqAbstractPanel)
  private
    FOnButtonClick: TNotifyEvent;
    FReadOnly: Boolean;
    procedure SetReadOnly(const AValue: Boolean);
    function GetExtraHint: TlqString;
    procedure SetExtraHint(const AValue: TlqString);
  protected
    FEdit: TlqEdit;
    FButton: TlqButton;
    function  GetOnShowHint: THintEvent; override;
    procedure SetOnShowHint(const AValue: THintEvent); override;
    procedure SetHint(const AValue: TlqString); override;
    function  GetHint: TlqString; override;
    procedure InternalButtonClick(Sender: TObject); virtual;
    procedure HandleResize(AWidth, AHeight: TlqCoord); override;
    property  ExtraHint: TlqString read GetExtraHint write SetExtraHint;
    property  ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
    property  OnButtonClick: TNotifyEvent read FOnButtonClick write FOnButtonClick;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TFilenameSetEvent = procedure(Sender: TObject; const AOldValue, ANewValue: TlqString) of object;

  TlqFileNameEdit = class(TlqBaseEditButton)
  private
    FOnFilenameSet: TFilenameSetEvent;
    FFilter: TlqString;
    FInitialDir: TlqString;
    procedure SetFilter(const AValue: TlqString);
    procedure SetFileName(const AValue: TlqString);
    function GetFileName: TlqString;
    procedure DoFilenameSet(const AOld, ANew: TlqString);
  protected
    procedure HandlePaint; override;
    procedure InternalButtonClick(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property    Align;
    property    Enabled;
    property    ExtraHint;
    property    FileName: TlqString read GetFileName write SetFileName;
    property    InitialDir: TlqString read FInitialDir write FInitialDir;
    property    Filter: TlqString read FFilter write SetFilter;
    property    ReadOnly;
    property    TabOrder;
    property    OnButtonClick;
    property    OnShowHint;
    property    OnFilenameSet: TFilenameSetEvent read FOnFilenameSet write FOnFilenameSet;
  end;


  TlqDirectoryEdit = class(TlqBaseEditButton)
  private
    FRootDirectory: TlqString;
    function GetDirectory: TlqString;
    procedure SetDirectory(const AValue: TlqString);
  protected
    procedure HandlePaint; override;
    procedure InternalButtonClick(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property    Align;
    property    Directory: TlqString read GetDirectory write SetDirectory;
    property    Enabled;
    property    ExtraHint;
    property    RootDirectory: TlqString read FRootDirectory write FRootDirectory;
    property    ReadOnly;
    property    TabOrder;
    property    OnButtonClick;
    property    OnShowHint;
  end;


  TlqFontEdit = class(TlqBaseEditButton)
  protected
    function GetFontDesc: TlqString; virtual;
    procedure SetFontDesc(const AValue: TlqString); virtual;
    procedure HandlePaint; override;
    procedure InternalButtonClick(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property    Align;
    property    Enabled;
    property    ExtraHint;
    property    FontDesc: TlqString read GetFontDesc write SetFontDesc;
    property    ReadOnly;
    property    TabOrder;
    property    OnButtonClick;
    property    OnShowHint;
  end;


  TlqEditButton =  class(TlqBaseEditButton)
  protected
    function    GetText: TlqString;
    procedure   SetText(const AValue: TlqString);
    procedure   HandlePaint; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property    Align;
    property    Enabled;
    property    ExtraHint;
    property    ReadOnly;
    property    TabOrder;
    property    Text: TlqString read GetText write SetText;
    property    OnButtonClick;
    property    OnShowHint;
  end;


implementation

uses
  lq_constants
  ,lq_dialogs
  ,lq_utils
  ;

{ TlqEditButton }

function TlqEditButton.GetText: TlqString;
begin
  Result := FEdit.Text;
end;

procedure TlqEditButton.SetText(const AValue: TlqString);
begin
  FEdit.Text := AValue;
end;

procedure TlqEditButton.HandlePaint;
var
  img: TlqImage;
begin
  inherited HandlePaint;
  // only so that it looks pretty in the UI Designer
  if csDesigning in ComponentState then
  begin
    FEdit.Visible := False;
    FButton.Visible := False;
    Canvas.Clear(clBoxColor);
    lqStyle.DrawControlFrame(Canvas, 0, 0, Width - Height, Height);
    lqStyle.DrawButtonFace(Canvas, Width - Height, 0, Height, Height, [btfIsEmbedded]);
    Canvas.SetFont(lqApplication.DefaultFont);
    if Text <> '' then
    begin
      Canvas.TextColor := clText3;
      Canvas.DrawText(4, 0, Width - Height, Height, Text, [txtLeft, txtVCenter]);
    end
    else
    begin
      Canvas.TextColor := clShadow1;
      Canvas.DrawText(0, 0, Width - Height, Height, ClassName, [txtHCenter, txtVCenter]);
    end;
    img := lqImages.GetImage('stdimg.ellipse'); // don't free the img instance - we only got a reference
    if img <> nil then
      Canvas.DrawImage(Width-Height+((Height-img.Width) div 2), (Height-img.Height) div 2, img);
  end;
end;

constructor TlqEditButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FButton.ImageName := 'stdimg.ellipse';
end;


{ TlqBaseEditButton }

procedure TlqBaseEditButton.SetReadOnly(const AValue: Boolean);
begin
  if FReadOnly = AValue then
    Exit;
  FReadOnly := AValue;
  FEdit.ReadOnly := FReadOnly;
  FButton.Enabled := not FReadOnly;   // Buttons don't have ReadOnly property.
end;

function TlqBaseEditButton.GetExtraHint: TlqString;
begin
  Result := FEdit.ExtraHint;
end;

procedure TlqBaseEditButton.SetExtraHint(const AValue: TlqString);
begin
  FEdit.ExtraHint := AValue;
end;

function TlqBaseEditButton.GetOnShowHint: THintEvent;
begin
  // rewire the FEdit event to the parent (composite) component
  Result := FEdit.OnShowHint;
end;

procedure TlqBaseEditButton.SetOnShowHint(const AValue: THintEvent);
begin
  // rewire the FEdit event to the parent (composite) component
  FEdit.OnShowHint := AValue;
end;

procedure TlqBaseEditButton.SetHint(const AValue: TlqString);
begin
  FEdit.Hint := AValue;
end;

function TlqBaseEditButton.GetHint: TlqString;
begin
  Result := FEdit.Hint;
end;

procedure TlqBaseEditButton.InternalButtonClick(Sender: TObject);
begin
  // do nothing
  if Assigned(OnButtonClick) then
    OnButtonClick(self);
end;

procedure TlqBaseEditButton.HandleResize(AWidth, AHeight: TlqCoord);
begin
  inherited HandleResize(AWidth, AHeight);
  { resizing can now occur before the component is shown, so we need extra
    checks here, like are we still busy creating everything. }
  if not (csLoading in ComponentState) then
  begin
    if csDesigning in ComponentState then
    begin
      FEdit.Visible := False;
      FButton.Visible := False;
    end
    else
    begin
        FEdit.SetPosition(0, 0, AWidth - AHeight, AHeight);
        FButton.SetPosition(AWidth - AHeight, 0, AHeight, AHeight);
    end;
  end;
end;

constructor TlqBaseEditButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width           := 140;
  Height          := 24;
  FReadOnly       := False;

  FEdit := TlqEdit.Create(self);
  with FEdit do
  begin
    Name := 'FEdit';
    SetPosition(0, 0, self.Width - self.Height, self.Height);
    Text := '';
    FontDesc := '#Edit1';
    TabOrder := 0;
  end;

  FButton := TlqButton.Create(self);
  with FButton do
  begin
    Name := 'FButton';
    SetPosition(self.Width - self.Height, 0, self.Height, self.Height);
    Text := '';
    FontDesc := '#Label1';
    ImageMargin := -1;
    ImageName := 'stdimg.elipses';
    ImageSpacing := 0;
    TabOrder := 1;
    OnClick := @InternalButtonClick;
  end;
end;



{ TlqFileNameEdit }

constructor TlqFileNameEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFilter         := '';
  FButton.ImageName := 'stdimg.folderfile';
end;

procedure TlqFileNameEdit.SetFilter(const AValue: TlqString);
begin
  FFilter := AValue;
end;

procedure TlqFileNameEdit.SetFileName(const AValue: TlqString);
begin
  FEdit.Text := AValue;
end;

function TlqFileNameEdit.GetFileName: TlqString;
begin
  Result := FEdit.Text;
end;

procedure TlqFileNameEdit.DoFilenameSet(const AOld, ANew: TlqString);
begin
  if Assigned(FOnFilenameSet) then
    FOnFilenameSet(self, AOld, ANew);
end;

procedure TlqFileNameEdit.HandlePaint;
var
  img: TlqImage;
begin
  inherited HandlePaint;
  // only so that it looks pretty in the UI Designer
  if csDesigning in ComponentState then
  begin
    FEdit.Visible := False;
    FButton.Visible := False;
    Canvas.Clear(clBoxColor);
    lqStyle.DrawControlFrame(Canvas, 0, 0, Width - Height, Height);
    lqStyle.DrawButtonFace(Canvas, Width - Height, 0, Height, Height, [btfIsEmbedded]);
    Canvas.SetFont(lqApplication.DefaultFont);
    if Filename <> '' then
    begin
      Canvas.TextColor := clText3;
      Canvas.DrawText(4, 0, Width - Height, Height, Filename, [txtLeft, txtVCenter]);
    end
    else
    begin
      Canvas.TextColor := clShadow1;
      Canvas.DrawText(0, 0, Width - Height, Height, ClassName, [txtHCenter, txtVCenter]);
    end;
    img := lqImages.GetImage('stdimg.folderfile'); // don't free the img instance - we only got a reference
    if img <> nil then
      Canvas.DrawImage(Width-Height+((Height-img.Width) div 2), (Height-img.Height) div 2, img);
  end;
end;

procedure TlqFileNameEdit.InternalButtonClick(Sender: TObject);
var
  dlg: TlqFileDialog;
  old: TlqString;
begin
  old := FEdit.Text;
  dlg := TlqFileDialog.Create(nil);
  try
    if FileName = '' then
    begin
      if FInitialDir <> '' then
        dlg.InitialDir := FInitialDir;
    end
    else
    begin
      // Use path of existing filename
      dlg.InitialDir := lqExtractFilePath(FileName);
      if dlg.InitialDir = '' then    // FileName had no path
        dlg.InitialDir := FInitialDir;
    end;
    if FFilter = '' then
      dlg.Filter := rsAllFiles + ' (' + AllFilesMask + ')' + '|' + AllFilesMask
    else
      dlg.Filter :=  FFilter + '|' + rsAllFiles + ' (' + AllFilesMask + ')' + '|' + AllFilesMask;
    if dlg.RunOpenFile then
    begin
      FEdit.Text := dlg.FileName;
    end;
  finally
    dlg.Free;
  end;
  inherited InternalButtonClick(Sender);
  if old <> FEdit.Text then
    DoFilenameSet(old, FEdit.Text);
end;


{ TlqDirectoryEdit}

constructor TlqDirectoryEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FButton.ImageName := 'stdimg.folder';
end;

function TlqDirectoryEdit.GetDirectory: TlqString;
begin
  Result := FEdit.Text;
end;

procedure TlqDirectoryEdit.SetDirectory(const AValue: TlqString);
begin
  FEdit.Text := AValue;
end;

procedure TlqDirectoryEdit.HandlePaint;
var
  img: TlqImage;
begin
  inherited HandlePaint;
  // only so that it looks pretty in the UI Designer
  if csDesigning in ComponentState then
  begin
    FEdit.Visible := False;
    FButton.Visible := False;
    Canvas.Clear(clBoxColor);
    lqStyle.DrawControlFrame(Canvas, 0, 0, Width - Height, Height);
    lqStyle.DrawButtonFace(Canvas, Width - Height, 0, Height, Height, [btfIsEmbedded]);
    Canvas.SetFont(lqApplication.DefaultFont);
    if Directory <> '' then
    begin
      Canvas.TextColor := clText3;
      Canvas.DrawText(4, 0, Width - Height, Height, Directory, [txtLeft, txtVCenter]);
    end
    else
    begin
      Canvas.TextColor := clShadow1;
      Canvas.DrawText(0, 0, Width - Height, Height, ClassName, [txtHCenter, txtVCenter]);
    end;
    img := lqImages.GetImage('stdimg.folder'); // don't free the img instance - we only got a reference
    if img <> nil then
      Canvas.DrawImage(Width-Height+((Height-img.Width) div 2), (Height-img.Height) div 2, img);
  end;
end;

procedure TlqDirectoryEdit.InternalButtonClick(Sender: TObject);
var
  dlg: TlqSelectDirDialog;
begin
  dlg := TlqSelectDirDialog.Create(nil);
  try
    if FRootDirectory <> '' then
      dlg.RootDirectory := FRootDirectory;
    dlg.SelectedDir := Directory;
    if dlg.ShowModal = mrOK then
    begin
      FEdit.Text:= dlg.SelectedDir;
    end;
  finally
    dlg.Free;
  end;
  inherited InternalButtonClick(Sender);
end;


{ TlqFontEdit }

function TlqFontEdit.GetFontDesc: TlqString;
begin
  Result := FEdit.Text;
end;

procedure TlqFontEdit.SetFontDesc(const AValue: TlqString);
begin
  FEdit.Text := AValue;
end;

procedure TlqFontEdit.HandlePaint;
var
  img: TlqImage;
begin
  inherited HandlePaint;
  // only so that it looks pretty in the UI Designer
  if csDesigning in ComponentState then
  begin
    FEdit.Visible := False;
    FButton.Visible := False;
    Canvas.Clear(clBoxColor);
    lqStyle.DrawControlFrame(Canvas, 0, 0, Width - Height, Height);
    lqStyle.DrawButtonFace(Canvas, Width - Height, 0, Height, Height, [btfIsEmbedded]);
    Canvas.TextColor := clShadow1;
    Canvas.SetFont(lqApplication.DefaultFont);
    Canvas.DrawText(0, 0, Width - Height, Height, ClassName, [txtHCenter, txtVCenter]);
    img := lqImages.GetImage('stdimg.font'); // don't free the img instance - we only got a reference
    if img <> nil then
      Canvas.DrawImage(Width-Height+((Height-img.Width) div 2), (Height-img.Height) div 2, img);
  end;
end;

procedure TlqFontEdit.InternalButtonClick(Sender: TObject);
var
  f: TlqString;
begin
  f := FontDesc;
  if SelectFontDialog(f) then
    FontDesc := f;
  inherited InternalButtonClick(Sender);
end;

constructor TlqFontEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FButton.ImageName := 'stdimg.font';
end;


end.

