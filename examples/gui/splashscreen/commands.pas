{
  Here we define some commands that can be reused throughout a application.
  Command actions are kept separate from the UI code (Forms).
}
unit commands;

{$mode objfpc}{$H+}

interface

uses
  lq_command_intf,
  lq_memo;
  
type
  // non reference counted interface
  TNullInterfacedObject = class(TObject)
  protected
    function QueryInterface(const IID: TGUID; out Obj): longint; stdcall;
    function _AddRef: longint; stdcall;
    function _Release: longint; stdcall;
  end;


  TAddCommand = class(TInterfacedObject, ICommand)
  private
    FMemo: TlqMemo;
  public
    constructor Create(AMemo: TlqMemo); reintroduce;
    procedure   Execute;
  end;


  TExitCommand = class(TInterfacedObject, ICommand)
  public
    procedure   Execute;
  end;


  TShowSplashCommand = class(TInterfacedObject, ICommand)
  public
    procedure   Execute;
  end;


  TShowBorderlessForm = class(TInterfacedObject, ICommand)
  public
    procedure   Execute;
  end;


implementation

uses
  SysUtils, lq_main, frm_main, frm_splashscreen;

{ TNullInterfacedObject }

function TNullInterfacedObject.QueryInterface(const IID: TGUID; out Obj): longint; stdcall;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    result := integer(e_nointerface);
end;

function TNullInterfacedObject._AddRef: longint; stdcall;
begin
  Result := -1;
end;

function TNullInterfacedObject._Release: longint; stdcall;
begin
  Result := -1;
end;

{ TAddCommand }

constructor TAddCommand.Create(AMemo: TlqMemo);
begin
  inherited Create;
  FMemo := AMemo;
end;

procedure TAddCommand.Execute;
begin
  FMemo.Lines.Add('Hello ' + IntToStr(Random(500)));
end;

{ TExitCommand }

procedure TExitCommand.Execute;
begin
  lqApplication.Terminated := True;
end;

{ TShowSplashCommand }

procedure TShowSplashCommand.Execute;
begin
  frmSplash := TSplashForm.Create(nil);
  frmSplash.Show;
end;

{ TShowBorderlessForm }

procedure TShowBorderlessForm.Execute;
begin
  TBorderLessForm.Execute;
end;

end.

