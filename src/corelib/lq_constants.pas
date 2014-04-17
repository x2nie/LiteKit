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
      Constants used throughout fpGUI will be defined here. This includes
      language constants for localization.

      You only need to changes these defines if you want the default
      fpGUI language to be something other than English.

      Soon the lang_*.inc files will be auto generated from the
      actual *.po files. At which point only the .po files need
      to be maintained.
}

unit lq_constants;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

resourcestring

{ Define Compiler language symbol (eg: de for German) to include the correct
  language resource file otherwise the Default (English) resource file will
  be used. }

{.$DEFINE de}     // German
{.$DEFINE ru}     // Russian
{.$DEFINE fr}     // French
{.$DEFINE pt}     // Portuguese (Brazil)
{.$DEFINE af}     // Afrikaans
{.$DEFINE it}     // Italian
{.$DEFINE es}     // Spanish



{$IF defined(de)}
  {$I lang_de.}
  
{$ELSEIF defined(ru)}
  {$I lang_ru.}
  
{$ELSEIF defined(fr)}
  {$I lang_fr.}
  
{$ELSEIF defined(pt)}
  {$I lang_pt.}
  
{$ELSEIF defined(af)}
  {$I lang_af.}

{$ELSEIF defined(it)}
  {$I lang_it.}

{$ELSEIF defined(es)}
  {$I lang_es.}

{$ELSE}
  {$I lang_en.}
{$IFEND}


const

  { Double click support }
  DOUBLECLICK_MS = 320; // the max time between left-clicks for doubleclick in milliseconds
  DOUBLECLICK_DISTANCE = 5; // max distance between points when doing doubleclick in pixels

  ONE_MILISEC = 1/MSecsPerDay;

  DEFAULT_HINT_PAUSE = 500;   // in milliseconds

  { Default fpGUI help viewer }
  FPG_HELPVIEWER = 'docview';

  FPG_CONFIG_DIR = 'fpgui_toolkit' + PathDelim;
  FPG_BOOKMARKS_FILE = 'bookmarks.ini';
  FPG_BOOKMARK_SECTION = 'bookmarks';
  
  // Used for the internal message queue
  cMessageQueueSize = 2048;

  // version and name constants
  {$I VERSIONFILE_.}inc  // this includes the auto :generated  GUIVfp_ersion = 
  fpGUIName    = 'fpGUI Toolkit';
  fpGUIWebsite = 'http://fpgui.sourceforge.net/';

  txtWordDelims: set of char = [' ', #9, #13, #10];



implementation



end.

