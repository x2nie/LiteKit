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

function  lqToOSEncoding(aString: TlqString): string;
function  lqFromOSEncoding(aString: string): TlqString;
procedure lqOpenURL(const aURL: TlqString);
function  lqFileSize(const AFilename: TlqString): integer;


// *** Common functions for all platforms ***

function lqAddTrailingValue(const ALine, AValue: TlqString; ADuplicates: Boolean = True): TlqString;
function lqAppendPathDelim(const Path: TlqString): TlqString;
function lqHasSubDirs(const Dir: TlqString; AShowHidden: Boolean): Boolean;
function lqAllFilesMask: TlqString;
function lqConvertLineEndings(const s: TlqString): TlqString;
function lqGetToolkitConfigDir: TlqString;
{ This is so that when we support LTR and RTL languages, the colon will be
  added at the correct place. }
function lqAddColon(const AText: TlqString): TlqString;
function lqIsBitSet(const AData: integer; const AIndex: integer): boolean;


 // RTL wrapper filesystem functions with platform independant encoding
 // These functions are common for all platforms and rely on lqXXXPlatformEncoding

function lqFindFirst(const Path: TlqString; Attr: longint; out Rslt: TSearchRec): longint;
function lqFindNext(var Rslt: TSearchRec): longint;
function lqGetCurrentDir: TlqString;
function lqSetCurrentDir(const NewDir: TlqString): Boolean;
function lqExpandFileName(const FileName: TlqString): TlqString;
function lqFileExists(const FileName: TlqString): Boolean;
function lqDeleteFile(const FileName: TlqString): Boolean;
function lqDirectoryExists(const ADirectory: TlqString): Boolean;
function lqExtractFileDir(const FileName: TlqString): TlqString;
function lqExtractFilePath(const FileName: TlqString): TlqString;
function lqExtractFileName(const FileName: TlqString): TlqString;
function lqExtractFileExt(const FileName: TlqString): TlqString;
function lqExtractRelativepath(const ABaseName, ADestName: TlqString): TlqString;
function lqForceDirectories(const ADirectory: TlqString): Boolean;
function lqChangeFileExt(const FileName, Extension: TlqString): TlqString;
function lqGetAppConfigDir(const Global: Boolean): TlqString;
function lqGetAppConfigFile(const Global: Boolean; const SubDir: Boolean): TlqString;
function lqGetExecutableName: TlqString;
function lqRenameFile(const OldName, NewName: TlqString): Boolean;


implementation

 { No USES clause is allowed here! Add it to the include file shown below. }


 // Platform specific encoding handling functions
{$I lq_utils_impl.inc}


function lqAddTrailingValue(const ALine, AValue: TlqString; ADuplicates: Boolean = True): TlqString;
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

function lqFindFirst(const Path: TlqString; Attr: longint; out Rslt: TSearchRec): longint;
begin
  Result    := FindFirst(lqToOSEncoding(Path), Attr, Rslt);
  Rslt.Name := lqFromOSEncoding(Rslt.Name);
end;

function lqFindNext(var Rslt: TSearchRec): longint;
begin
  Result    := FindNext(Rslt);
  Rslt.Name := lqFromOSEncoding(Rslt.Name);
end;

function lqGetCurrentDir: TlqString;
begin
  Result := lqFromOSEncoding(GetCurrentDir);
end;

function lqSetCurrentDir(const NewDir: TlqString): Boolean;
begin
  Result := SetCurrentDir(lqToOSEncoding(NewDir));
end;

function lqExpandFileName(const FileName: TlqString): TlqString;
begin
  Result := lqFromOSEncoding(ExpandFileName(lqToOSEncoding(FileName)));
end;

function lqFileExists(const FileName: TlqString): Boolean;
begin
  Result := FileExists(lqToOSEncoding(FileName));
end;

function lqDeleteFile(const FileName: TlqString): Boolean;
begin
  { Don't remove 'SysUtils.' prefix, it is required under Windows, other
    FPC tries to use Windows.DeleteFile API - which is wrong }
  Result := SysUtils.DeleteFile(lqToOSEncoding(FileName));
end;

function lqDirectoryExists(const ADirectory: TlqString): Boolean;
begin
  Result := DirectoryExists(lqToOSEncoding(ADirectory));
end;

function lqExtractFileDir(const FileName: TlqString): TlqString;
begin
  Result := ExtractFileDir(lqToOSEncoding(FileName));
end;

function lqExtractFilePath(const FileName: TlqString): TlqString;
begin
  Result := ExtractFilePath(lqToOSEncoding(Filename));
end;

function lqExtractFileName(const FileName: TlqString): TlqString;
begin
  Result := ExtractFileName(lqToOSEncoding(Filename));
end;

function lqExtractFileExt(const FileName: TlqString): TlqString;
begin
  Result := ExtractFileExt(lqToOSEncoding(Filename));
end;

function lqExtractRelativepath(const ABaseName, ADestName: TlqString): TlqString;
begin
  Result := ExtractRelativepath(lqToOSEncoding(ABaseName), lqToOSEncoding(ADestName));
end;

function lqForceDirectories(const ADirectory: TlqString): Boolean;
begin
  Result := ForceDirectories(lqToOSEncoding(ADirectory));
end;

function lqChangeFileExt(const FileName, Extension: TlqString): TlqString;
begin
  Result := ChangeFileExt(lqToOSEncoding(Filename), Extension);
end;

function lqGetAppConfigDir(const Global: Boolean): TlqString;
begin
  Result := lqFromOSEncoding(GetAppConfigDir(Global));
end;

function lqGetAppConfigFile(const Global: Boolean; const SubDir: Boolean): TlqString;
begin
  Result := lqFromOSEncoding(GetAppConfigFile(Global, SubDir));
end;

function lqGetExecutableName: TlqString;
begin
  Result := lqChangeFileExt(lqExtractFileName(Paramstr(0)), '');
end;

function lqRenameFile(const OldName, NewName: TlqString): Boolean;
begin
  Result := RenameFile(lqToOSEncoding(OldName), lqToOSEncoding(NewName));
end;

function lqAppendPathDelim(const Path: TlqString): TlqString;
begin
  if (Path <> '') and (Path[length(Path)] <> PathDelim) then
    Result := Path + PathDelim
  else
    Result := Path;
end;

{function lqHasSubDirs returns True if the directory passed has subdirectories}
function lqHasSubDirs(const Dir: TlqString; AShowHidden: Boolean): Boolean;
var
  FileInfo: TSearchRec;
  FCurrentDir: TlqString;
begin
  //Assume No
  Result := False;
  if Dir <> '' then
  begin
    FCurrentDir := lqAppendPathDelim(Dir);
    FCurrentDir := FCurrentDir + lqAllFilesMask;
    try
      if lqFindFirst(FCurrentDir, faAnyFile or $00000080, FileInfo) = 0 then
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
        until lqFindNext(FileInfo) <> 0;
    finally
      SysUtils.FindClose(FileInfo);
    end;
  end;
end;

function lqAllFilesMask: TlqString;
begin
  { Since FPC 2.2.2 we have the AllFilesMask variable, which is part of the RTL }
  Result := AllFilesMask;
end;

function lqConvertLineEndings(const s: TlqString): TlqString;
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

function lqGetToolkitConfigDir: TlqString;
begin
  Result := lqTrimR(lqGetAppConfigDir(False), ApplicationName, True) + LQ_CONFIG_DIR;
end;

function lqAddColon(const AText: TlqString): TlqString;
begin
  { TODO : Check language direction and add colon at appropriate end. This is very crude! }
  Result := AText + ':';
end;

function lqIsBitSet(const AData: integer; const AIndex: integer): boolean;
begin
  Result := (AData and (1 shl AIndex) <> 0);
end;


end.

