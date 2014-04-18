{
    LiteKit  -  Free Pascal GUI Toolkit

    Copyright (C) 2006 - 2013 See the file AUTHORS.txt, included in this
    distribution, for details of the copyright.

    See the file COPYING.modifiedLGPL, included in this distribution,
    for details about redistributing LiteKit.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    Description:
      PNG image loading using the Free Pascal library: fcl-image
}

unit lq_imgfmt_png;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  Classes,
  lq_base,
  lq_main,
  FPImage,
  FPReadPNG;

function LoadImage_PNG(const AFileName: TlqString): TlqImage; overload;
function LoadImage_PNG(AStream: TStream): TlqImage; overload;
function LoadImage_PNG(const AImageData: Pointer; const AImageDataSize: LongWord): TlqImage; overload;
function LoadImage_PNG(AInstance: THandle; const AResName: String; AResType: PChar): TlqImage; overload;
function LoadImage_PNGcrop(const AMaxWidth, AMaxHeight: integer; const AFileName: TlqString): TlqImage;


implementation

uses
  lq_utils;

function FPImageToFPG(ASource: TFPCustomImage): TlqImage;
var
  i, j: integer;
  colorA: TFPColor;   // struct Red, Green, Blue, Alpha: word
  colorB: TlqColor;  // ONE long 32-bit-word
  xlocal, ylocal: integer;
begin
  Result := nil;
  if ASource=nil then Exit;

  xlocal := ASource.Width;
  ylocal := ASource.Height;
  Result := TlqImage.Create;
  Result.AllocateImage(32, xlocal, ylocal); // 32=colordepth
  for i := 0 to ylocal - 1 do
    for j := 0 to xlocal - 1 do
    begin
      colorA := ASource.Colors[j, i];
      colorB := (colorA.Blue shr 8) or (colorA.Green and $FF00) or ((colorA.Red and $FF) shl 16) or ((colorA.Alpha and $FF) shl 24);
      Result.Colors[j, i] := colorB;
    end;
  Result.UpdateImage;
end;

function LoadImage_PNG(const AFileName: TlqString): TlqImage;
var
  imga: TFPCustomImage;
begin
  Result := nil;
  if not lqFileExists(AFileName) then
    Exit; //==>

  imga := TFPMemoryImage.Create(0, 0);
  try
    imga.LoadFromFile(AFileName, TFPReaderPNG.Create); // auto size image
  except
    imga := nil;
  end;
  if imga <> nil then
  begin
    Result := FPImageToFPG(imga);
    imga.Free;
  end;
end;

function LoadImage_PNG(AStream: TStream): TlqImage;
var
  imga: TFPMemoryImage;
  reader: TFPReaderPNG;
begin
  Result := nil;
  if AStream = nil then
    Exit;

  imga := TFPMemoryImage.Create(0, 0);
  try
    try
      AStream.Position := 0;
      reader := TFPReaderPNG.Create;
      imga.LoadFromStream(AStream, reader); // auto size image
      Result := FPImageToFPG(imga);
    except
      on e: Exception do
      begin
        Result := nil;
        raise; // so we can report PNG errors (eg: missing resource name)
      end;
    end;
  finally
    reader.Free;
    imga.Free;
  end;
end;

function LoadImage_PNG(const AImageData: Pointer; const AImageDataSize: LongWord): TlqImage;
var
  s: TMemoryStream;
begin
  s := TMemoryStream.Create;
  try
    s.Write(AImageData^, AImageDataSize);
    Result := LoadImage_PNG(s);
  finally
    s.Free;
  end;
end;

function LoadImage_PNG(AInstance: THandle; const AResName: String; AResType: PChar): TlqImage;
var
  res: TResourceStream;
begin
  try
    res := TResourceStream.Create(AInstance, AResName, AResType);
    Result := LoadImage_PNG(res);
  finally
    if res <> nil then
      res.Free;
  end;
end;

function LoadImage_PNGcrop(const AMaxWidth, AMaxHeight: integer; const AFileName: TlqString): TlqImage;
var
  i, j: integer;
  colorA: TFPColor;   // struct Red, Green, Blue, Alpha: word
  colorB: TlqColor;  // ONE long 32-bit-word
  imga: TFPCustomImage;
  imgb: TlqImage;
  xlocal, ylocal: integer;
begin
  Result := nil;
  if not lqFileExists(AFileName) then
    Exit; //==>

          // Maximum image size of AMaxWidth by AMaxHeight.
          // Actual image imga.Width (AMaxWidth) and imga.Height (AMaxHeight).
          // Calculated to fit the image within required size: xlocal, ylocal
  imga := TFPMemoryImage.Create(0, 0);
  try
    imga.LoadFromFile(AFileName, TFPReaderPNG.Create); // auto size image
  except
    imga := nil;
    imgb := nil;
  end;
  if imga <> nil then
  begin
    if imga.Width > AMaxWidth then
      xlocal := AMaxWidth
    else
      xlocal := imga.Width;
    if imga.Height > AMaxHeight then
      ylocal := AMaxHeight
    else
      ylocal := imga.Height;
    imgb := TlqImage.Create;
    imgb.AllocateImage(32, xlocal, ylocal); // 32=colordepth
    for i := 0 to ylocal - 1 do
      for j := 0 to xlocal - 1 do
      begin
        colorA := imga.Colors[j, i];
        colorB := (colorA.Blue shr 8) or (colorA.Green and $FF00) or ((colorA.Red and $FF00) shl 8);
        imgb.Colors[j, i] := colorB;
      end;
    imgb.UpdateImage;
  end;
  imga.Free;
  Result := imgb;
end;


end.

