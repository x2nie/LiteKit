{%mainunit lq_utils.pas}

uses
  Unix, BaseUnix, lq_constants, lq_stringutils;

// X11 specific filesystem implementations of encoding functions

function fpgToOSEncoding(aString: TlqString): string;
begin
  Result := aString;
end;

function fpgFromOSEncoding(aString: string): TlqString;
begin
  Result := aString;
end;

procedure fpgOpenURL(const aURL: TlqString);
var
  Helper: string;
begin
//TODO: Catch "which" command output to run the browser from there
  Helper := '';
  if fpsystem('which xdg-open') = 0 then
    Helper := 'xdg-open'
  else if FileExists('/usr/bin/sensible-browser') then
    Helper := '/usr/bin/sensible-browser'
  else if FileExists('/etc/alternatives/x-www-browser') then
    Helper := '/etc/alternatives/x-www-browser'
  else if fpsystem('which firefox') = 0 then
    Helper := 'firefox'
  else if fpsystem('which konqueror') = 0 then
    Helper := 'konqueror'
  else if fpsystem('which opera') = 0 then
    Helper := 'opera'
  else if fpsystem('which mozilla') = 0 then
     Helper := 'mozilla'
  else if fpsystem('which chrome') = 0 then
     Helper := 'chrome'
  else if fpsystem('which chromium') = 0 then
     Helper := 'chromium';

  if Helper <> '' then
    fpSystem(Helper + ' ' + aURL + '&');
end;

function fpgFileSize(const AFilename: TlqString): integer;
var
  st: baseunix.stat;
begin
  if not fpstat(pointer(AFilename),st) >= 0 then
    exit(-1);
  Result := st.st_size;
end;
