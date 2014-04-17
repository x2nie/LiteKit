unit pkghelpLiteKit;

{$mode objfpc}{$H+}

interface

uses
 Classes, SysUtils, HelpFPDoc;

procedure Register;

implementation

procedure Register;
begin
 // for Online help files
{
  RegisterFPDocHTMLHelpForPackage('LiteKit Help','LiteKit Help Database',
              'http://opensoft.homeip.net/fpgui/docs/','LiteKit');
}
 // for local help files
 RegisterFPDocHTMLHelpForPackage('LiteKit Help','LiteKit Help Database',
             'file://$PkgDir(LiteKitHelpIntegration)/html','LiteKitHelpIntegration', '../src');
//             'file://$PkgDir(LiteKit)/html','LiteKit','../src');

end;

end.
