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
      Style Manager is implemented as a Singleton. New styles will register
      with the style manager. The style manager can also be used to populate
      widgets like a ComboBox or ListBox with available styles.
}
unit lq_stylemanager;

{$mode objfpc}{$H+}

interface

uses
  Classes
  ,Contnrs
  ,lq_main
  ;
  
const
  cDefaultStyle = 'auto';   // TODO: This text needs to be a resource string for translation

type
  // A class reference for the TlqStyle descendants
  TlqStyleClass = class of TlqStyle;


  // A class to hold the style class mappings. The factory maintains
  // a list of these and uses the StyleClass property to create the objects.
  TlqStyleClassMapping = class(TObject)
  private
    FsMappingName: string;
    FStyleClass: TlqStyleClass;
  public
    constructor Create(const AMappingName: string; AStyleClass: TlqStyleClass); overload;
    property    MappingName: string read FsMappingName;
    property    StyleClass: TlqStyleClass read FStyleClass;
  end;


  // Style manager and factory class
  TlqStyleManager = class(TObject)
  private
    FList : TObjectList;
    FDefaultStyle: TlqStyle;
//    FUserStyle: TlqStyle;
    FDefaultStyleType: string;
    function    GetStyle: TlqStyle;
  public
    constructor Create;
    destructor  Destroy; override;
    property    Style: TlqStyle read GetStyle;
    function    SetStyle(const AStyleName: string): boolean;
    procedure   RegisterClass(const AStyleName: string; AStyleClass : TlqStyleClass);
    function    CreateInstance(const AStyleName: string): TlqStyle; overload;
    function    CreateInstance: TlqStyle; overload;
    procedure   FreeStyleInstance;
    procedure   AssignStyleTypes(const AStrings: TStrings);
  end;


{ Lazy-man's singleton }
function lqStyleManager: TlqStyleManager;


implementation

uses
  SysUtils
  ;

var
  uStyleManager: TlqStyleManager;


{ Creation is deferred to the first request }
function lqStyleManager: TlqStyleManager;
begin
  if uStyleManager = nil then
    uStyleManager := TlqStyleManager.Create;
  result := uStyleManager;
end;


{ TlqStyleClassMapping }

constructor TlqStyleClassMapping.Create(const AMappingName: string; AStyleClass: TlqStyleClass);
begin
  inherited Create;
  FsMappingName := AMappingName;
  FStyleClass   := AStyleClass;
end;


{ TlqStyleManager }

function TlqStyleManager.GetStyle: TlqStyle;
begin
  if not Assigned(FDefaultStyle) then
    FDefaultStyle := CreateInstance(FDefaultStyleType);
  Result := FDefaultStyle;
end;

constructor TlqStyleManager.Create;
begin
  inherited Create;
  FList := TObjectList.Create;
//  FUserStyle        := nil;
  FDefaultStyle     := nil;
  FDefaultStyleType := cDefaultStyle;    // will change later
end;

destructor TlqStyleManager.Destroy;
begin
  FreeStyleInstance;
  FList.Free;
  inherited Destroy;
end;

function TlqStyleManager.SetStyle(const AStyleName: string): boolean;
var
  i: integer;
begin
  Result := False;
  for i := 0 to FList.Count - 1 do
  begin
    if UpperCase(TlqStyleClassMapping(FList.Items[i]).MappingName) = UpperCase(AStyleName) then
    begin
      FDefaultStyleType := AStyleName;
      if Assigned(FDefaultStyle) then
        FDefaultStyle.Free;
      FDefaultStyle := CreateInstance;
      Result := True;
      Break; //==>
    end;
  end;

  Assert(FDefaultStyleType <> AStyleName,
      Format('<%s> does not identify a registered style class.', [AStyleName]));
end;

// Register a TStyle class for creation by the factory
procedure TlqStyleManager.RegisterClass(const AStyleName: string; AStyleClass: TlqStyleClass);
var
  i: integer;
begin
  for i := 0 to FList.Count - 1 do
    if UpperCase(TlqStyleClassMapping(FList.Items[i]).MappingName) = UpperCase(AStyleName) then
      Assert(false, Format('Style class <%s> already registered.', [AStyleName]));
  FList.Add(TlqStyleClassMapping.Create(AStyleName, AStyleClass));
//  writeln('Registering style: ' + AStyleName);
end;

// Call the factory to create an instance of TStyle
function TlqStyleManager.CreateInstance(const AStyleName: string): TlqStyle;
var
  i: integer;
begin
  result := nil;
  for i := 0 to FList.Count - 1 do
  begin
    if UpperCase(TlqStyleClassMapping(FList.Items[i]).MappingName) =
         UpperCase(AStyleName) then
    begin
      result := TlqStyleClassMapping(FList.Items[i]).StyleClass.Create;
      Break; //==>
    end;
  end;

  Assert(result <> nil, Format('<%s> does not identify a registered style class.', [AStyleName]));
end;

function TlqStyleManager.CreateInstance: TlqStyle;
begin
  result := CreateInstance(FDefaultStyleType);
end;

procedure TlqStyleManager.FreeStyleInstance;
begin
  FreeAndNil(FDefaultStyle);
end;

{ Assign the registered list of style names to a StringList.
  This can be used to populate a combobox with the registered style
  class types. }
procedure TlqStyleManager.AssignStyleTypes(const AStrings: TStrings);
var
  i: integer;
begin
  AStrings.Clear;
  for i := 0 to FList.Count - 1 do
    AStrings.Add(TlqStyleClassMapping(FList.Items[i]).MappingName);
end;


finalization
  uStyleManager.Free;

end.

