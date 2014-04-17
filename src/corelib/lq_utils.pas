{
    LiteKit  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2010 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing LiteKit.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Abstracted OS specific function to work in a cross-platform manner.
}

unit lq_utils;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  lq_base;

// *** Platform specific functions ***

function  fpgToOSEncoding(aString: TlqString): string;
function  fpgFromOSEncoding(aString: string): TlqString;
procedure fpgOpenURL(const aURL: TlqString);
function  fpgFileSize(const AFilename: TlqString): integer;


// *** Common functions for all platforms ***

function fpgAddTrailingValue(const ALine, AValue: TlqString; ADuplicates: Boolean = True): TlqString;
function fpgAppendPathDelim(const Path: TlqString): TlqString;
function fpgHasSubDirs(const Dir: TlqString; AShowHidden: Boolean): Boolean;
function fpgAllFilesMask: TlqString;
function fpgConvertLineEndings(const s: TlqString): TlqString;
function fpgGetToolkitConfigDir: TlqString;
{ This is so that when we support LTR and RTL languages, the colon will be
  added at the correct place. }
function fpgAddColon(const AText: TlqString): TlqString;
function fpgIsBitSet(const AData: integer; const AIndex: integer): boolean;


 // RTL wrapper filesystem functions with platform independant encoding
 // These functions are common for all platforms and rely on fpgXXXPlatformEncoding

function fpgFindFirst(const Path: TlqString; Attr: longint; out Rslt: TSearchRec): longint;
function fpgFindNext(var Rslt: TSearchRec): longint;
function fpgGetCurrentDir: TlqString;
function fpgSetCurrentDir(const NewDir: TlqString): Boolean;
function fpgExpandFileName(const FileName: TlqString): TlqString;
function fpgFileExists(const FileName: TlqString): Boolean;
function fpgDeleteFile(const FileName: TlqString): Boolean;
function fpgDirectoryExists(const ADirectory: TlqString): Boolean;
function fpgExtractFileDir(const FileName: TlqString): TlqString;
function fpgExtractFilePath(const FileName: TlqString): TlqString;
function fpgExtractFileName(const FileName: TlqString): TlqString;
function fpgExtractFileExt(const FileName: TlqString): TlqString;
function fpgExtractRelativepath(const ABaseName, ADestName: TlqString): TlqString;
function fpgForceDirectories(const ADirectory: TlqString): Boolean;
function fpgChangeFileExt(const FileName, Extension: TlqString): TlqString;
function fpgGetAppConfigDir(const Global: Boolean): TlqString;
function fpgGetAppConfigFile(const Global: Boolean; const SubDir: Boolean): TlqString;
function fpgGetExecutableName: TlqString;
function fpgRenameFile(const OldName, NewName: TlqString): Boolean;


implementation

 { No USES clause is allowed here! Add it to the include file shown below. }


 // Platform specific encoding handling functions
{$I lq_utils_impl.inc}


function fpgAddTrailingValue(const ALine, AValue: TlqString; ADuplicates: Boolean = True): TlqString;
begin
  if ALine = '' then
  begin
    Result := ALine;
    Exit; //==>
  end;

  if ADuplicates then
  begin
    Result := ALine + AValue;
    Exit; //==>
  end;

  if (not SameText(Copy(ALine, Length(ALine) - Length(AValue) + 1, Length(AValue)), AValue)) then
    Result := ALine + AValue
  else
    Result := ALine;
end;

function fpgFindFirst(const Path: TlqString; Attr: longint; out Rslt: TSearchRec): longint;
begin
  Result    := FindFirst(fpgToOSEncoding(Path), Attr, Rslt);
  Rslt.Name := fpgFromOSEncoding(Rslt.Name);
end;

function fpgFindNext(var Rslt: TSearchRec): longint;
begin
  Result    := FindNext(Rslt);
  Rslt.Name := fpgFromOSEncoding(Rslt.Name);
end;

function fpgGetCurrentDir: TlqString;
begin
  Result := fpgFromOSEncoding(GetCurrentDir);
end;

function fpgSetCurrentDir(const NewDir: TlqString): Boolean;
begin
  Result := SetCurrentDir(fpgToOSEncoding(NewDir));
end;

function fpgExpandFileName(const FileName: TlqString): TlqString;
begin
  Result := fpgFromOSEncoding(ExpandFileName(fpgToOSEncoding(FileName)));
end;

function fpgFileExists(const FileName: TlqString): Boolean;
begin
  Result := FileExists(fpgToOSEncoding(FileName));
end;

function fpgDeleteFile(const FileName: TlqString): Boolean;
begin
  { Don't remove 'SysUtils.' prefix, it is required under Windows, other
    FPC tries to use Windows.DeleteFile API - which is wrong }
  Result := SysUtils.DeleteFile(fpgToOSEncoding(FileName));
end;

function fpgDirectoryExists(const ADirectory: TlqString): Boolean;
begin
  Result := DirectoryExists(fpgToOSEncoding(ADirectory));
end;

function fpgExtractFileDir(const FileName: TlqString): TlqString;
begin
  Result := ExtractFileDir(fpgToOSEncoding(FileName));
end;

function fpgExtractFilePath(const FileName: TlqString): TlqString;
begin
  Result := ExtractFilePath(fpgToOSEncoding(Filename));
end;

function fpgExtractFileName(const FileName: TlqString): TlqString;
begin
  Result := ExtractFileName(fpgToOSEncoding(Filename));
end;

function fpgExtractFileExt(const FileName: TlqString): TlqString;
begin
  Result := ExtractFileExt(fpgToOSEncoding(Filename));
end;

function fpgExtractRelativepath(const ABaseName, ADestName: TlqString): TlqString;
begin
  Result := ExtractRelativepath(fpgToOSEncoding(ABaseName), fpgToOSEncoding(ADestName));
end;

function fpgForceDirectories(const ADirectory: TlqString): Boolean;
begin
  Result := ForceDirectories(fpgToOSEncoding(ADirectory));
end;

function fpgChangeFileExt(const FileName, Extension: TlqString): TlqString;
begin
  Result := ChangeFileExt(fpgToOSEncoding(Filename), Extension);
end;

function fpgGetAppConfigDir(const Global: Boolean): TlqString;
begin
  Result := fpgFromOSEncoding(GetAppConfigDir(Global));
end;

function fpgGetAppConfigFile(const Global: Boolean; const SubDir: Boolean): TlqString;
begin
  Result := fpgFromOSEncoding(GetAppConfigFile(Global, SubDir));
end;

function fpgGetExecutableName: TlqString;
begin
  Result := fpgChangeFileExt(fpgExtractFileName(Paramstr(0)), '');
end;

function fpgRenameFile(const OldName, NewName: TlqString): Boolean;
begin
  Result := RenameFile(fpgToOSEncoding(OldName), fpgToOSEncoding(NewName));
end;

function fpgAppendPathDelim(const Path: TlqString): TlqString;
begin
  if (Path <> '') and (Path[length(Path)] <> PathDelim) then
    Result := Path + PathDelim
  else
    Result := Path;
end;

{function fpgHasSubDirs returns True if the directory passed has subdirectories}
function fpgHasSubDirs(const Dir: TlqString; AShowHidden: Boolean): Boolean;
var
  FileInfo: TSearchRec;
  FCurrentDir: TlqString;
begin
  //Assume No
  Result := False;
  if Dir <> '' then
  begin
    FCurrentDir := fpgAppendPathDelim(Dir);
    FCurrentDir := FCurrentDir + fpgAllFilesMask;
    try
      if fpgFindFirst(FCurrentDir, faAnyFile or $00000080, FileInfo) = 0 then
        repeat
          if FileInfo.Name = '' then
            Continue;

            // check if special file
          if ((FileInfo.Name = '.') or (FileInfo.Name = '..')) or
            // unix dot directories (aka hidden directories)
            ((FileInfo.Name[1] in ['.']) and AShowHidden) or
            // check Hidden attribute
            (((faHidden and FileInfo.Attr) > 0) and AShowHidden) then
            Continue;

          Result := ((faDirectory and FileInfo.Attr) > 0);

          //We found at least one non special dir, that's all we need.
          if Result then
            break;
        until fpgFindNext(FileInfo) <> 0;
    finally
      SysUtils.FindClose(FileInfo);
    end;
  end;
end;

function fpgAllFilesMask: TlqString;
begin
  { Since FPC 2.2.2 we have the AllFilesMask variable, which is part of the RTL }
  Result := AllFilesMask;
end;

function fpgConvertLineEndings(const s: TlqString): TlqString;
var
  i: integer;
  EndingStart: longint;
begin
  Result := s;
  i      := 1;
  while (i <= length(Result)) do
    if Result[i] in [#10, #13] then
    begin
      EndingStart := i;
      Inc(i);
      if (i <= length(Result)) and (Result[i] in [#10, #13]) and (Result[i] <> Result[i - 1]) then
        Inc(i);
      if (length(LineEnding) <> i - EndingStart) or (LineEnding <> copy(Result, EndingStart, length(LineEnding))) then
      begin
        // line end differs => replace with current LineEnding
        Result := copy(Result, 1, EndingStart - 1) + LineEnding + copy(Result, i, length(Result));
        i      := EndingStart + length(LineEnding);
      end;
    end
    else
      Inc(i);
end;

function fpgGetToolkitConfigDir: TlqString;
begin
  Result := fpgTrimR(fpgGetAppConfigDir(False), ApplicationName, True) + FPG_CONFIG_DIR;
end;

function fpgAddColon(const AText: TlqString): TlqString;
begin
  { TODO : Check language direction and add colon at appropriate end. This is very crude! }
  Result := AText + ':';
end;

function fpgIsBitSet(const AData: integer; const AIndex: integer): boolean;
begin
  Result := (AData and (1 shl AIndex) <> 0);
end;


end.

