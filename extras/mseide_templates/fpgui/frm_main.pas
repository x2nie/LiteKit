unit frm_main;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, lq_base, lq_main, lq_form;

type

  TMainForm = class(TlqForm)
  private
    {@VFD_HEAD_BEGIN: MainForm}
    {@VFD_HEAD_END: MainForm}
  public
    procedure AfterCreate; override;
  end;

{@VFD_NEWFORM_DECL}

implementation

{@VFD_NEWFORM_IMPL}

procedure TMainForm.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: MainForm}
  Name := 'MainForm';
  SetPosition(374, 228, 490, 258);
  WindowTitle := 'MainForm';
  Hint := '';
  WindowPosition := wpOneThirdDown;

  {@VFD_BODY_END: MainForm}
  {%endregion}
end;


end.
