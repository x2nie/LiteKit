unit lq_dsgn_reg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure Register;

implementation
uses PropEdits,lq_main, lq_widget, lq_propedits,
  lq_button,lq_progressbar, lq_trackbar, lq_edit, lq_memo,
  lq_listbox, //lq_combobox,
  lq_menu, lq_panel, lq_tab  ;

procedure Register;
begin
  RegisterComponents('Standard',[TlqButton,TlqMenuBar, TlqPopupMenu, TlqEdit,
     TlqPanel, TlqGroupBox,
     TlqPageControl, TlqTabSheet, TlqImages]);

  RegisterPropertyEditor(TypeInfo(widestring), TlqWidget, 'Caption', TStringMultilinePropertyEditor);
  RegisterPropertyEditor(TypeInfo(widestring), TlqWidget, 'Text', TStringMultilinePropertyEditor);
///  RegisterPropertyEditor(TypeInfo(lq_main.TCursor), TlqWidget, 'Cursor', TCursorPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TlqWidget, 'FontDesc', TFontDescPropertyEditor);

end;


end.

