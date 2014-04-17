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
      This unit contains the Create New Directory dialog.
}

{%mainunit lq_dialogs.pas}

{$IFDEF read_interface}

  TlqNewDirDialog = class(TlqBaseDialog)
  private
    lblTitle: TlqLabel;
    edDirectory: TlqEdit;
    function    GetDirectory: TlqString;
  public
    constructor Create(AOwner: TComponent); override;
    property    Directory: TlqString read GetDirectory;
  end;


{$ENDIF read_interface}



{$IFDEF read_implementation}


{ TlqNewDirDialog }

function TlqNewDirDialog.GetDirectory: TlqString;
begin
  Result := edDirectory.Text;
end;

constructor TlqNewDirDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // reset dimensions
  Width     := 286;
  Height    := 97;
  MinWidth  := 286;
  MinHeight := 97;
  
  lblTitle := CreateLabel(self, 8, 8, rsEnterNewDirectory);
  edDirectory := CreateEdit(self, 8, 28, 270, 0);
  edDirectory.Anchors := [anLeft, anTop, anRight];

  // reposition buttons
  btnCancel.Left  := Width-FDefaultButtonWidth-FSpacing;
  btnCancel.Top   := Height - FSpacing - btnCancel.Height;
  btnOK.Left      := btnCancel.Left-FDefaultButtonWidth-FSpacing;
  btnOK.Top       := btnCancel.Top;
  
  // now reset tab order
  edDirectory.TabOrder  := 1;
  btnOK.TabOrder        := 2;
  btnCancel.TabOrder    := 3;
end;


{$ENDIF read_implementation}


