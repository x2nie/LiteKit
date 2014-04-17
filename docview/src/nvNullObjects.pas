unit nvNullObjects;

{$mode objfpc}{$H+}

interface

uses
  contnrs, Classes, SysUtils, lq_main;

type
  EHelpBitmapException = class(Exception);

  THelpBitmap = class(TlqImage)
  public
    constructor CreateFromHelpFile( FileHandle: TFileStream; Offset: longint );
    procedure LoadFromResourceName(const AName: string);
  end;



implementation



{ THelpBitmap }

constructor THelpBitmap.CreateFromHelpFile(FileHandle: TFileStream; Offset: longint);
begin
  inherited Create;
end;

procedure THelpBitmap.LoadFromResourceName(const AName: string);
begin
  //
end;


end.

