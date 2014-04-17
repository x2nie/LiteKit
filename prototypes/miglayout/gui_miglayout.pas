unit gui_miglayout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lq_panel, lq_base;
  
type

  { TlqLayoutPanel }

  TlqLayoutPanel = class(TlqBevel)
  protected
    procedure   HandleResize(awidth, aheight: TlqCoord); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure   Add(AComponent: TComponent; AConstraint: string);
  end;

implementation

uses
  lq_main;

{ TlqLayoutPanel }

procedure TlqLayoutPanel.HandleResize(awidth, aheight: TlqCoord);
begin
  writeln('HandleResize');
  inherited HandleResize(awidth, aheight);
end;

constructor TlqLayoutPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Align := alClient;
  Shape := bsSpacer;
end;

procedure TlqLayoutPanel.Add(AComponent: TComponent; AConstraint: string);
begin
  //
end;

end.

