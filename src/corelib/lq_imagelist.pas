{
    LiteKit  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2010 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing LiteKit.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:

      A basic image list component for use by the GUI components like the
      treeview etc.
}

unit lq_imagelist;

{$mode objfpc}{$H+}

{.$define DEBUG}

interface

uses
  Classes,
  SysUtils,
  lq_base,
  lq_main;
  
type

  EItemExists = class(Exception);

  TlqImageList = class;  // forward declaration
  

  TlqImageItem = class(TObject)
  private
    FImage: TlqImage;
    FIndex: integer;
    FImageList: TlqImageList;
    procedure   SetImageList(AImageList: TlqImageList);
    procedure   SetIndex(AIndex: integer);
    procedure   SetImage(AImage: TlqImage);
  public
    constructor Create; overload;
    constructor Create(AImageList: TlqImageList; AIndex: integer; AImage: TlqImage); overload;
    constructor Create(AFileName: TlqString; AIndex: integer); overload;
    destructor  Destroy; override;
    property    Index: integer read FIndex write SetIndex;
    property    Image: TlqImage read FImage write SetImage;
    property    ImageList: TlqImageList read FImageList write SetImageList;
    procedure   LoadFromFile(AFileName: TlqString);
  end;


  TlqImageList = class(TObject)
  private
    FList: TList;
    function    GetFListIndex(AIndex: Integer): Integer;
    function    GetItem(AIndex: integer): TlqImageItem;
    procedure   SetItem(AIndex: integer; AItem: TlqImageItem);
    function    GetCount: integer;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   AddItemFromFile(AFileName: TlqString; AIndex: integer = -1);
    procedure   AddImage(AImage: TlqImage; AIndex: integer = -1);
    procedure   RemoveIndex(AIndex: integer);
    function    GetMaxItem: integer;
    procedure   Clear;
    property    Items[AIndex: integer]: TlqImageItem read GetItem write SetItem; default;
    property    Count: integer read GetCount;
  end;

  

implementation

uses
  lq_imgfmt_bmp,
  lq_utils;

{ TlqImageList }

function TlqImageList.GetFListIndex(AIndex: Integer): Integer;
var
  i: integer;
begin
  {$IFDEF DEBUG}
  writeln('TlqImageList.GetFListIndex');
  {$ENDIF}
  result := -1;
  for i := 0 to FList.Count - 1 do
    if TlqImageItem(FList[i]).Index = AIndex then
    begin
      result := i;
      Break;  //==>
    end;
end;

function TlqImageList.GetItem(AIndex: integer): TlqImageItem;
var
  AFindIndex: integer;
begin
  {$IFDEF DEBUG}
  writeln('TlqImageList.GetItem');
  {$ENDIF}
  result := nil;
  AFindIndex := GetFListIndex(AIndex);
  if AFindIndex > -1 then
    result := TlqImageItem(FList[AFindIndex]);
end;

procedure TlqImageList.SetItem(AIndex: integer; AItem: TlqImageItem);
begin
  if AItem = nil then
    Exit; //==>
    
  if GetItem(AIndex) = AItem then
    Exit; //==>
    
  RemoveIndex(AIndex);      // delete existing Item
  AItem.Index := AIndex;
  FList.Add(AItem);
end;

function TlqImageList.GetCount: integer;
begin
  Result := FList.Count;
end;

constructor TlqImageList.Create;
begin
  FList := TList.Create;
end;

destructor TlqImageList.Destroy;
var
  i: integer;
begin
  for i := FList.Count-1 downto 0 do
    TlqImageItem(FList[i]).Destroy;  // frees images
  FList.Destroy;
  inherited Destroy
end;

procedure TlqImageList.AddItemFromFile(AFileName: TlqString; AIndex: integer);
var
  AImageItem: TlqImageItem;
begin
  {$IFDEF DEBUG}
  writeln('TlqImageList.AddItemFromFile');
  {$ENDIF}
  
  if not fpgFileExists(AFileName) then
    Exit; //==>
  
  AImageItem := TlqImageItem.Create;
  AImageItem.LoadFromFile(AFileName);
  if AIndex > -1 then
    Items[AIndex] := AImageItem
  else
  begin
    FList.Add(AImageItem);
    AImageItem.Index := GetMaxItem+1;
  end;
end;

procedure TlqImageList.AddImage(AImage: TlqImage; AIndex: integer);
var
  AImageItem: TlqImageItem;
begin
  AImageItem := TlqImageItem.Create;
  AImageItem.Image := AImage;
  if AIndex > -1 then
    Items[AIndex] := AImageItem
  else
  begin
    FList.Add(AImageItem);
    AImageItem.Index := GetMaxItem+1;
  end;
end;

procedure TlqImageList.RemoveIndex(AIndex: integer);
begin
  {$IFDEF DEBUG}
  writeln('TlqImageList.RemoveIndex');
  {$ENDIF}
  AIndex := GetFListIndex(AIndex);
  if AIndex <> -1 then
  begin
    TlqImageItem(FList[AIndex]).Destroy;
    FList.Delete(AIndex);
  end;
end;

function TlqImageList.GetMaxItem: integer;
var
  i: integer;
begin
  result := -1;
  for i := 0 to FList.Count - 1 do
    if TlqImageItem(FList[i]).Index > result then
      result := TlqImageItem(FList[i]).Index;
end;

procedure TlqImageList.Clear;
begin
  FList.Clear;
end;

{ TlqImageItem }

procedure TlqImageItem.SetImageList(AImageList: TlqImageList);
begin
  if AImageList = nil then
  begin
    FImageList := nil;
  end
  else
  begin
    if FImageList <> nil then
      FImageList.RemoveIndex(Index);
    FImageList := AImageList;
    FImageList.Items[Index] := self;
  end;
end;

procedure TlqImageItem.SetIndex(AIndex: integer);
begin
  {$IFDEF DEBUG}
  writeln('TlqImageItem.SetIndex');
  {$ENDIF}
  if AIndex <> FIndex then
  begin
    if ImageList <> nil then
      ImageList.RemoveIndex(AIndex);
    FIndex := AIndex;
  end;
end;

procedure TlqImageItem.SetImage(AImage: TlqImage);
begin
  {$IFDEF DEBUG}
  writeln('TlqImageItem.SetImage');
  {$ENDIF}
  FImage := AImage;
end;

constructor TlqImageItem.Create;
begin
  ImageList := nil;
  FIndex    := -1;
  FImage    := nil;
end;

constructor TlqImageItem.Create(AImageList: TlqImageList; AIndex: integer;
    AImage: TlqImage);
begin
  if AImageList = nil then
    Exit; //==>
  FImage      := AImage;
  FIndex      := AIndex;
  FImageList  := nil;
  ImageList   := AImageList;
end;

constructor TlqImageItem.Create(AFileName: TlqString; AIndex: integer);
begin
  {$IFDEF DEBUG}
  writeln('TlqImageItem.Create(', AFileName, ',', AIndex, ')');
  {$ENDIF}
  Index := AIndex;
  LoadFromFile(AFileName);
end;

destructor TlqImageItem.Destroy;
begin
  if FImage <> nil then
     FImage.Free;
  inherited Destroy;
end;

procedure TlqImageItem.LoadFromFile(AFileName: TlqString);
begin
  {$IFDEF DEBUG}
  writeln('TlqImageItem.LoadFromFile');
  {$ENDIF}
  if FImage <> nil then
    FImage.Destroy;
  FImage := LoadImage_BMP(AFileName);
end;

end.

