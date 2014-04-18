program stdimglist;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils,
  lq_base, lq_main, lq_form, lq_imgfmt_bmp, lq_button, u_reportimages;

type

  TMainForm = class(TlqForm)
  private
    btnClose: TlqButton;
    procedure   btnCloseClick(Sender: TObject);
  protected
    procedure   HandlePaint; override;
  public
    procedure   AfterCreate; override;
  end;

{ TMainForm }

procedure TMainForm.AfterCreate;
begin
  SetPosition(100,100,700,500);
  WindowTitle := 'LiteKit Standard Image Listing';
  WindowPosition := wpOneThirdDown;
  MinWidth := 200;
  MinHeight := 100;

  CreateReportImages;

  btnClose := CreateButton(self, Width-90, Height-35, 75, 'Quit', @btnCloseClick);
  btnClose.ImageName := 'stdimg.quit';
  btnClose.Anchors := [anRight, anBottom];
end;

procedure TMainForm.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.HandlePaint;
var
  n: integer;
  x: TlqCoord;
  y: TlqCoord;
  sl: TStringList;
  img: TlqImage;
begin
  Canvas.BeginDraw; // begin double buffering
  inherited HandlePaint;

  sl  := TStringList.Create;
  x   := 8;
  y   := 8;
  lqImages.ListImages(sl);
  
  for n := 0 to sl.Count-1 do
  begin
    Canvas.DrawString(x, y, sl[n]+':');
    
    img := TlqImage(sl.Objects[n]);
    if img <> nil then
      Canvas.DrawImage(x+130, y, img);

    inc(y, img.Height+8);
    if y > Height-32 then // largest images are 32 in height
    begin
      inc(x, 200);
      y := 8;
    end;
  end;

  Canvas.EndDraw;
  sl.Free;
end;

procedure MainProc;
var
  frm : TMainForm;
begin
  lqApplication.Initialize;
  frm := TMainForm.Create(nil);
  frm.Show;
  lqApplication.Run;
  frm.Free;
end;

begin
  MainProc;
end.
