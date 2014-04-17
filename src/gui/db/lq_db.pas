{
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2010 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Database support classes. This serves as an example of how someone
      could implement DB aware components for fpGUI.
}

unit lq_db;

{$mode objfpc}{$H+}

// If enabled it outputs debug information for this unit
{.$Define DEBUG}

interface

uses
  Classes,
  db,
  lq_widget,
  lq_label{, lq_edit};
  
type

  TlqFieldDataLink = class(TDataLink)
  private
    FWidget: TlqWidget;
    FField: TField;
    FFieldName: string;
    FOnDataChange: TNotifyEvent;
    function    GetCanModify: Boolean;
    procedure   SetFieldName(const AFieldName: string);
    procedure   UpdateField;
  protected
    procedure   ActiveChanged; override;
    procedure   RecordChanged(AField: TField); override;
  public
    constructor Create(AWidget: TlqWidget);
    property    CanModify: Boolean read GetCanModify;
    property    Field: TField read FField;
    property    FieldName: string read FFieldName write SetFieldName;
    property    Widget: TlqWidget read FWidget write FWidget;
    property    OnDataChange: TNotifyEvent read FOnDataChange write FOnDataChange;
  end;
  
  
  TlqDBLabel = class(TlqCustomLabel)
  private
    FDataLink: TlqFieldDataLink;
    function    GetDataField: String;
    function    GetField: TField;
    procedure   SetDataField(const ADataField: String);
    function    GetDataSource: TDataSource;
    procedure   SetDataSource(ADataSource: TDataSource);
    procedure   DataChange(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    property    Field: TField read GetField;
  published
    property    AutoSize;
    property    BackgroundColor;
    property    DataField: string read GetDataField write SetDataField;
    property    DataSource: TDataSource read GetDataSource write SetDataSource;
    property    Enabled;
    property    FontDesc;
    property    Text;
    property    TextColor;
  end;

{
  // TlqEdit needs to be refactor some more first!!
  TlqDBEdit = class(TlqCustomEdit)
  private
    FDataLink: TlqFieldDataLink;
    function    GetDataField: string;
    function    GetDataSource: TDataSource;
    function    GetField: TField;
    function    GetReadOnly: Boolean;
    procedure   SetDataField(const ADataField: string);
    procedure   SetDataSource(const ADataSource: TDataSource);
    procedure   DataChange(Sender: TObject);
    procedure   SetReadOnly(const AValue: Boolean);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    property    Field: TField read GetField;
  published
    property    DataField: string read GetDataField write SetDataField;
    property    DataSource: TDataSource read GetDataSource write SetDataSource;
    property    Enabled;
    property    BackgroundColor;
    property    Color;
    property    FontDesc;
    property    Text;
    property    ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
  end;
}


implementation


{ TlqFieldDataLink }

function TlqFieldDataLink.GetCanModify: Boolean;
begin
  Result := not ReadOnly and (Field <> nil) and Field.CanModify;
end;

procedure TlqFieldDataLink.SetFieldName(const AFieldName: string);
begin
  if AFieldName <> FieldName then
  begin
    FFieldName := AFieldName;
    UpdateField;
  end;
end;

procedure TlqFieldDataLink.UpdateField;
begin
  {$IFDEF DEBUG} WriteLn('## UpdateField. DataSet: ', DataSource.DataSet.ClassName); {$ENDIF}
  FField := DataSource.DataSet.FindField(FieldName);
  if Assigned(OnDataChange) then
    OnDataChange(Self);
end;

procedure TlqFieldDataLink.ActiveChanged;
begin
  inherited ActiveChanged;
  UpdateField;
end;

procedure TlqFieldDataLink.RecordChanged(AField: TField);
begin
  inherited RecordChanged(AField);
  if Assigned(OnDataChange) then
    OnDataChange(Self);
end;

constructor TlqFieldDataLink.Create(AWidget: TlqWidget);
begin
  inherited Create;
  FWidget := AWidget;
end;


{ TlqDBLabel }

function TlqDBLabel.GetDataField: String;
begin
  Result := FDataLink.FieldName;
end;

function TlqDBLabel.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TlqDBLabel.SetDataField(const ADataField: String);
begin
  FDataLink.FieldName := ADataField;
end;

function TlqDBLabel.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TlqDBLabel.SetDataSource(ADataSource: TDataSource);
begin
  FDataLink.DataSource := ADataSource;
end;

procedure TlqDBLabel.DataChange(Sender: TObject);
begin
  {$IFDEF DEBUG} Write(Classname + '.DataChange'); {$ENDIF}
  if Assigned(FDataLink.Field) then
  begin
    Text := FDataLink.Field.DisplayText;
    {$IFDEF DEBUG} WriteLn(' new text: "', Text, '"'); {$ENDIF}
  end
  else
  begin
    Text := '';
    {$IFDEF DEBUG} WriteLn('DataLink has no data'); {$ENDIF}
  end;
end;

constructor TlqDBLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TlqFieldDataLink.Create(Self);
  FDataLink.OnDataChange := @DataChange;
end;

destructor TlqDBLabel.Destroy;
begin
  FDataLink.Free;
  inherited Destroy;
end;

end.

