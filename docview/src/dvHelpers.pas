unit dvHelpers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lq_base;


function GetOwnHelpFileName: String;
// Given a filename, which may or may not contain a path or extension,
// finds the actual file. This can involve searching
// the help and bookshelf paths.
function FindHelpFile(const AFilename: TlqString): TlqString;

function SearchHelpPaths( const Filename: TlqString; var ResultFilename: TlqString; const IncludeAppDir: boolean ): boolean;

function GetApplicationDir: TlqString;
function SearchPath( PathEnvVar: TlqString; Filename: TlqString; var FilenameFound: string ): boolean;

implementation

uses
  lq_utils
  ,dvConstants
  ,nvUtilities
  ;


function GetOwnHelpFileName: String;
begin
  { TODO -oGraeme -cown help : Maybe later we will have different language versions }
  result := lqExtractFilePath(ParamStr(0)) + cDocViewHelpFile;
  if not lqFileExists(Result) then
  begin
     Result := FindHelpFile(cDocViewHelpFile);
  end;
end;

// Given a "filename" which may include a path, find it in various paths and extensions
function FindHelpFile(const AFilename: TlqString): TlqString;
var
  AlternativeFileName: TlqString;
  lFilename: TlqString;
begin
  lFilename := AFilename;
  if lFileName = OWN_HELP_MARKER then
  begin
    Result := GetOwnHelpFileName;
    exit;
  end;

  Result := '';

  AlternativeFileName := '';
  if lqExtractFileExt( lFilename ) = '' then
  begin
    lFilename := lqChangeFileExt(lFilename, '.inf');
    AlternativeFileName := lqChangeFileExt(lFilename, '.hlp');
  end;

  if lqExtractFilePath( lFileName ) <> '' then
  begin
    // Path specified; just see if it exists

    // expand out relative paths
    lFilename := lqExpandFileName( lFileName );
    AlternativeFilename := lqExpandFileName( AlternativeFilename );

    if lqFileExists( lFilename ) then
      Result := lFilename
    else if lqFileExists( AlternativeFilename ) then
      Result := AlternativeFilename;

  end
  else
  begin
    // Path not specified; search current
    if lqFileExists( lqExpandFileName( lFileName ) ) then
    begin
      Result := lqExpandFileName( lFileName );
      exit;
    end;

    if (AlternativeFilename <> '') and lqFileExists(lqExpandFileName(AlternativeFilename)) then
    begin
      Result := lqExpandFileName( AlternativeFilename );
      exit;
    end;

    // Search help paths
    if not SearchHelpPaths( lFileName,
                            Result,
                            false // don't search our app dir
                             ) then
    begin
      // Didn't find as specified or as .inf, try .hlp
      if AlternativeFilename <> '' then
      begin
        if not SearchHelpPaths( AlternativeFileName,
                                Result,
                                false // don't search our app dir
                                ) then
        begin
          Result := '';
        end;
      end;
    end;
  end;
//  Result := AFileName;
end;

Function SearchHelpPaths( const Filename: TlqString;
                          var ResultFilename: TlqString;
                          const IncludeAppDir: boolean ): boolean;
begin
  Result := SearchPath( HelpPathEnvironmentVar,
                        FileName,
                        ResultFilename );
  if not Result then
    Result := SearchPath( BookshelfEnvironmentVar,
                          FileName,
                          ResultFilename );
  if ( not Result ) and IncludeAppDir then
  begin
    ResultFilename := lqAppendPathDelim(GetApplicationDir)
                      + Filename;
    Result := lqFileExists( ResultFilename );
    if not Result then
      ResultFilename := '';
  end;

end;

function GetApplicationDir: TlqString;
begin
  Result := lqExtractFilePath(ParamStr(0));
end;

function SearchPath( PathEnvVar: TlqString; Filename: TlqString; var FilenameFound: string ): boolean;
var
  lFilename: string;
  lDir: TlqString;
  fl: TStrings;
  i: integer;
begin
  Result := False;
  FilenameFound := '';

  lDir := GetEnvironmentVariable(PathEnvVar);

  fl := TStringList.Create;
  ListFilesInDirectory(lDir, AllFilesMask, True, fl);
  TStringList(fl).Sort;
  for i := 0 to fl.Count-1 do
  begin
    lFilename := lqExtractFileName(fl[i]);
    if SameText(lFilename, Filename) then
    begin
      FilenameFound := fl[i];
      Result := True;
      Exit;
    end;
  end;
  fl.Free;
end;


end.

