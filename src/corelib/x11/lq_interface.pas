{
    LiteKit  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2012 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing LiteKit.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      This unit defines alias types to bind each backend graphics library
      to lq_main without the need for IFDEF's
}

unit lq_interface;

{$mode objfpc}{$H+}

interface

uses
  lq_x11;

type
  TlqFontResourceImpl  = class(TlqX11FontResource);
  TlqImageImpl         = class(TlqX11Image);
  TlqCanvasImpl        = class(TlqX11Canvas);
  TlqWindowImpl        = class(TlqX11Window);
  TlqApplicationImpl   = class(TlqX11Application);
  TlqClipboardImpl     = class(TlqX11Clipboard);
  TlqFileListImpl      = class(TlqX11FileList);
  TlqMimeDataImpl      = class(TlqX11MimeData);
  TlqDragImpl          = class(TlqX11Drag);
  TlqTimerImpl         = class(TlqX11Timer);
  TlqSystemTrayHandler = class(TlqX11SystemTrayHandler);

implementation

end.

