unit lq_propedits;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, propedits;
type

  { TFontDescPropertyEditor }

  TFontDescPropertyEditor = class(TStringPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    //function OrdValueToVisualValue(OrdValue: longint): string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const NewValue: ansistring); override;
  end;


implementation

uses lq_main;

{ TFontDescPropertyEditor }

function TFontDescPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paSortList, paValueList, paRevertable];
end;



procedure TFontDescPropertyEditor.GetValues(Proc: TGetStrProc);
begin
  lqFontDescValues(Proc);
end;

procedure TFontDescPropertyEditor.SetValue(const NewValue: ansistring);
begin
  inherited SetValue(NewValue);
end;

end.

