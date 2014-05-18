{
  Description:
    Virtual Widget.
}
unit lq_vw;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  lq_base, lq_main, lq_widget;

type

  ELqVwError = class(Exception);

  { TlqItem }

  TvwItem = class(TlqComponent)
  private
    FItems: TList;
    FCaption: string;
    FEnabled: Boolean; // list of TMenuItem
    function GetCount: Integer;
    function GetItem(Index: Integer): TvwItem;
    procedure SetCaption(const Value: string);
    procedure SetEnabled(const Value: Boolean);
  public
    { Sub Item}
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TvwItem read GetItem; default;
  public
    { Self }
    constructor Create(AOwner: TComponent);
    //destructor Destroy; override;
    property Caption : string read FCaption write SetCaption;
    property Enabled : Boolean read FEnabled write SetEnabled;
  end;

implementation

{ TlqItem }

function TvwItem.GetCount: Integer;
begin
  Result := 0;
  if Assigned(FItems) then
    Result:= FItems.Count;
end;

function TvwItem.GetItem(Index: Integer): TvwItem;
begin
  if FItems = nil then
    raise ELqVwError.CreateFmt('rsIndexOutOfBounds',[ClassName,Index,-1]);
  Result := TvwItem(FItems[Index]);
end;

constructor TvwItem.Create(AOwner: TComponent);
begin

end;

procedure TvwItem.SetCaption(const Value: string);
begin
  if FCaption = Value then
    Exit;
  FCaption := Value;
end;

procedure TvwItem.SetEnabled(const Value: Boolean);
begin
  if FEnabled = Value then
    Exit;
  FEnabled := Value;
end;

end.

