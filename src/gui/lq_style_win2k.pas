{
    LiteKit  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2011 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing LiteKit.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      Implements the Windows 2000 look. Simple and functional without
      distractions.
}
unit lq_style_win2k;

{$mode objfpc}{$H+}

interface

uses
  lq_main
  ,lq_style
  ;

type

  TlqWin2000Style = class(TlqStyle)

  end;

implementation

uses
  lq_stylemanager
  ;


initialization
  lqStyleManager.RegisterClass(cDefaultStyle, TlqWin2000Style);   // TODO: This will change later
  lqStyleManager.RegisterClass('Win2000', TlqWin2000Style);


end.

