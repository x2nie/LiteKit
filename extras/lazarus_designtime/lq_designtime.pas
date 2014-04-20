{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lq_designtime;

interface

uses
  lq_descriptors, lq_designer, lq_canvas_designer, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('lq_descriptors', @lq_descriptors.Register);
  RegisterUnit('lq_designer', @lq_designer.Register);
end;

initialization
  RegisterPackage('lq_designtime', @Register);
end.
