{%mainunit lq_utils.pas}

uses
  Shellapi, Windows, lq_constants, lq_stringutils;
  
// GDI specific implementations of encoding functions

function fpgToOSEncoding(aString: TlqString): string;
begin
  Result := Utf8ToAnsi(aString);
end;

function fpgFromOSEncoding(aString: string): TlqString;
begin
  Result := AnsiToUtf8(aString);
end;

procedure fpgOpenURL(const aURL: TlqString);
begin
  try
    {$IFNDEF wince}
    ShellExecute(0, 'open', PChar(aURL), nil, nil, 1 {SW_SHOWNORMAL});
    {$ENDIF}
  except
    // do nothing
  end;
end;

function fpgFileSize(const AFilename: TlqString): integer;
var
  FindData: TWIN32FindDataW;
  FindHandle: THandle;
  Str: widestring;
begin
  // Don't assign the widestring to TSearchRec.name because it is of type
  // string, which will generate a conversion to the system encoding
  Str := UTF8Decode(AFilename);
  FindHandle:=Windows.FindFirstFileW(PWideChar(Str), FindData);
  if FindHandle=Windows.Invalid_Handle_value then
  begin
    Result:=-1;
    exit;
  end;
  Result := (int64(FindData.nFileSizeHigh) shl 32)+FindData.nFileSizeLow;
  Windows.FindClose(FindHandle);
end;

