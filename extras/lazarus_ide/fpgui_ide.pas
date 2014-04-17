{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit fpgui_ide;

interface

uses
  LiteKitLazIDEIntf, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('LiteKitLazIDEIntf',@LiteKitLazIDEIntf.Register);
end;

initialization
  RegisterPackage('fpgui_ide',@Register);
end.
