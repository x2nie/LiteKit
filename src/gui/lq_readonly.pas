{
    LiteKit  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2011 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing LiteKit.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      A component that can set all components on a form and embedded
      containers as read-only or not. There are various events that
      fire during the process, to allow for maximum flexibility.
}

unit lq_readonly;

{$mode objfpc}{$H+}

interface

uses
  Classes
 ;

type
  TlqOnChangeReadOnlyEvent = procedure(pSender: TObject; pReadOnly: boolean) of object;
  TlqOnProcessEvent = procedure(pSender: TObject; var pReadOnly, pProcess: boolean) of object;
  TlqOnProcessFrmEvent = procedure(pFrame: TComponent; var pProcessDetails: boolean) of object;
  TlqOnGetParentEvent = procedure(var pParent: TComponent) of object;


  TlqReadOnly = class(TComponent)
  private
    FReadOnly: boolean;
    FOnChange: TlqOnChangeReadOnlyEvent;
    FOnProcess: TlqOnProcessEvent;
    FEnabled: boolean;
    FOnProcessFrm: TlqOnProcessFrmEvent;
    FProcessContainer: boolean;
    FOnGetParent: TlqOnGetParentEvent;
    procedure   SetEnabled(const AValue: boolean);
  protected
    function    GetReadOnly: boolean; virtual;
    procedure   SetReadOnly(const AValue: boolean); virtual;
    procedure   SetComponentsReadOnly(pReadOnly: boolean); virtual;
    function    GetParentForm: TComponent;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property    ReadOnly: boolean read GetReadOnly write SetReadOnly default false;
    property    Enabled: boolean read FEnabled write SetEnabled default false;
    property    ProcessContainer: boolean read FProcessContainer write FProcessContainer default false;
    property    OnChange: TlqOnChangeReadOnlyEvent read FOnChange write FOnChange;
    property    OnProcess: TlqOnProcessEvent read FOnProcess write FOnProcess;
    property    OnProcessFrm: TlqOnProcessFrmEvent read FOnProcessFrm write FOnProcessFrm;
    property    OnGetParent: TlqOnGetParentEvent read FOnGetParent write FOnGetParent;
  end;


implementation

uses
  lq_main
  ,lq_form
  ,lq_widget
  ,TypInfo
  ;

{ TlqReadOnly }

constructor TlqReadOnly.Create(AOwner: TComponent);
begin
  inherited;
  FReadOnly := false;
  FEnabled := false;
  FProcessContainer := false;
end;

function TlqReadOnly.GetParentForm: TComponent;
begin
  result := self;
  while true do
  begin
    if (result is TlqForm) and
      ((result.Owner is TlqApplication) or
      (result.Owner = nil)) then
      Break; //==>
    result := result.Owner;
  end;
  if Assigned(FOnGetParent) then
    FOnGetParent(result);
end;

function TlqReadOnly.GetReadOnly: boolean;
begin
  Result := FReadOnly;
end;

procedure TlqReadOnly.SetComponentsReadOnly(pReadOnly: boolean);
  procedure _SetComponentsReadOnly(pParent: TComponent);
  var
    i: integer;
    lComponent: TComponent;
    lReadOnly, lProcess: boolean;
  begin
    if pParent=nil then
      Exit; //==>
    for i := 0 to pParent.ComponentCount - 1 do
    begin
      lComponent := pParent.Components[i];
      if lComponent = self then
        Continue; //==>
      if IsPublishedProp(lComponent, 'ReadOnly') then
      begin
        lReadOnly := pReadOnly;
        lProcess := True;
        if Assigned(FOnProcess) then
          FOnProcess(lComponent, lReadOnly, lProcess);
        if lProcess then
          SetOrdProp(lComponent, 'ReadOnly', Ord(lReadOnly));
      end;
      if (lComponent is TlqWidget) and TlqWidget(lComponent).IsContainer then
      begin
        lProcess := FProcessContainer; // Now lProcess is: can I go Deep?
        if Assigned(FOnProcessFrm) then
          FOnProcessFrm(lComponent, lProcess);
        if lProcess then
          _SetComponentsReadOnly(lComponent);
      end;
    end;
  end;
begin
  _SetComponentsReadOnly(GetParentForm);
end;

procedure TlqReadOnly.SetEnabled(const AValue: boolean);
begin
  FEnabled := AValue;
end;

procedure TlqReadOnly.SetReadOnly(const AValue: boolean);
begin
  if not FEnabled then
    Exit; //==>
  if FReadOnly = AValue then
    Exit; //==>
  FReadOnly := AValue;
  SetComponentsReadOnly(FReadOnly);
  if Assigned(FOnChange) then
    FOnChange(Self, FReadOnly);
end;

end.

