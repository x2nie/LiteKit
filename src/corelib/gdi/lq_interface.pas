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
  lq_gdi;

type
  TlqFontResourceImpl  = class(TlqGDIFontResource);
  TlqImageImpl         = class(TlqGDIImage);
  TlqCanvasImpl        = class(TlqGDICanvas);
  TlqWindowImpl        = class(TlqGDIWindow);
  TlqApplicationImpl   = class(TlqGDIApplication);
  TlqClipboardImpl     = class(TlqGDIClipboard);
  TlqFileListImpl      = class(TlqGDIFileList);
  TlqMimeDataImpl      = class(TlqGDIMimeDataBase);
  TlqDragImpl          = class(TlqGDIDrag);
  TlqTimerImpl         = class(TlqGDITimer);
  TlqSystemTrayHandler = class(TlqGDISystemTrayIcon);

implementation

end.

