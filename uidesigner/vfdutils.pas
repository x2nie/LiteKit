{
    fpGUI  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2010 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing fpGUI.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Some utility functions.
}

unit vfdutils;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  lq_widget,
  lq_form,
  lq_label,
  lq_edit,
  lq_button,
  lq_memo,
  lq_checkbox;


procedure SetWidgetText(wg: TlqWidget; txt: string);
function GetWidgetText(wg: TlqWidget; out txt: string): boolean;
{ generates a string based on Indentation Style specified in UI Designer }
function Ind(const ACount: integer): string;


implementation

uses
  lq_base,
  lq_iniutils,
  strutils;

var
  IndentCharacters: array[0..1] of TlqString = ('  ', #9);


procedure SetWidgetText(wg: TlqWidget; txt: string);
begin
  if wg is TlqForm then
    TlqForm(wg).WindowTitle  := txt
  else if wg is TlqLabel then
    TlqLabel(wg).Text        := txt
  else if wg is TlqEdit then
    TlqEdit(wg).Text         := txt
  else if wg is TlqMemo then
    TlqMemo(wg).Text         := txt
  else if wg is TlqButton then
    TlqButton(wg).Text       := txt
  else if wg is TlqCheckBox then
    TlqCheckBox(wg).Text     := txt;
end;

function GetWidgetText(wg: TlqWidget; out txt: string): boolean;
begin
  Result := True;
  if wg is TlqForm then
    txt := TlqForm(wg).WindowTitle
  else if wg is TlqLabel then
    txt    := TlqLabel(wg).Text
  else if wg is TlqEdit then
    txt    := TlqEdit(wg).Text
  else if wg is TlqMemo then
    txt    := TlqMemo(wg).Text
  else if wg is TlqButton then
    txt    := TlqButton(wg).Text
  else if wg is TlqCheckBox then
    txt    := TlqCheckBox(wg).Text
  else
  begin
    Result := False;
    txt    := '';
  end;
end;

function Ind(const ACount: integer): string;
begin
  Result := DupeString(IndentCharacters[gINI.ReadInteger('Options', 'IndentationType', 0)], ACount);
end;

end.

