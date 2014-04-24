unit lq_designer;

{$mode objfpc}{$H+}

//enable to integrate with ObjectInspector, need modified OI.
{.$Define OiImageIndex}
interface

uses
  LCLProc, LCLType, Classes, SysUtils, FormEditingIntf, LCLIntf, //Graphics,
  ProjectIntf, lq_base, lq_main, lq_widget, lq_form, {hd_edit, hd_memo}
  LResources;

type

  { TlqMediator }

  TlqMediator = class(TDesignerMediator)
  private
    FlpForm: TlqForm;
  public
    // needed by the lazarus form editor
    class function CreateMediator(TheOwner, aForm: TComponent): TDesignerMediator;
      override;
    class function FormClass: TComponentClass; override;
    procedure GetBounds(AComponent: TComponent; out CurBounds: TRect); override;
    procedure SetBounds(AComponent: TComponent; NewBounds: TRect); override;
    {procedure GetClientArea(AComponent: TComponent; out
            CurClientArea: TRect; out ScrollOffset: TPoint); override;}
    procedure Paint; override;
    procedure InitComponent(AComponent, NewParent: TComponent; NewBounds: TRect); override;
    function ComponentIsIcon(AComponent: TComponent): boolean; override;
    function ParentAcceptsChild(Parent: TComponent;
                Child: TComponentClass): boolean; override;
  public
    {$IfDef OiImageIndex}
    procedure OiNodeGetImageIndex(APersistent: TPersistent; var AIndex: integer); override;
    {$endif}
  public
    // needed by TlqWidget
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //procedure InvalidateRect(Sender: TObject; ARect: TRect; Erase: boolean);
    property pgfForm: TlqForm read FlpForm;
  end;



procedure Register;

implementation
uses Controls, PropEdits, lq_propedits,
  lq_button,lq_progressbar, lq_trackbar, lq_edit, lq_memo,
  lq_listbox, //lq_combobox,
  lq_menu,
  lq_canvas_designer,
  Graphics
  ;

procedure Register;
begin
  FormEditingHook.RegisterDesignerMediator(TlqMediator);
  (*RegisterComponents('Standard',[TlpTImer, TlpButton, TlpMemo, TlpEdit, TlpListBox,
  TlpProgressbar, TlpTrackbar, //TlpCombobox
  TlpMenuBar, TlpPopupMenu
  ]); *)
  RegisterComponents('Standard',[TlqButton,TlqMenuBar, TlqPopupMenu]);

  RegisterPropertyEditor(TypeInfo(widestring), TlqWidget, 'Caption', TStringMultilinePropertyEditor);
  RegisterPropertyEditor(TypeInfo(widestring), TlqWidget, 'Text', TStringMultilinePropertyEditor);
///  RegisterPropertyEditor(TypeInfo(lq_main.TCursor), TlqWidget, 'Cursor', TCursorPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TlqWidget, 'FontDesc', TFontDescPropertyEditor);

  lq_main.LQU_PREVENTWND := true;
  lq_main.DefaultCanvasClass := lq_canvas_designer.TlqLazCanvas;
  lqApplication.Initialize;
end;






{ TlqMediator }
type
    TlqWidgetAccess = class(TlqWidget)
    end;

class function TlqMediator.CreateMediator(TheOwner, aForm: TComponent
  ): TDesignerMediator;

var
  Mediator: TlqMediator;
begin
  ///pgfOpenDisplay('');
  //lqApplication.Initialize;
  //pgfDesigning := true;
  //lq_main.DefaultCanvasClass := lq_canvas_designer.TlqLazCanvas;
  //LQU_PREVENTWND := true;
  //lq_main.DefaultCanvasClass := lq_canvas_designer.TlqLazCanvas;
  //LQU_PREVENTWND := true;
  //lqApplication.Initialize;


  Result:=inherited CreateMediator(TheOwner, aForm);
  Mediator:=TlqMediator(Result);
  Mediator.FlpForm:=aForm as TlqForm;
  //Mediator.m_pgfForm.show();//allocate windowhandle

  //Mediator.m_pgfForm.FormDesigner:=Mediator;
end;

class function TlqMediator.FormClass: TComponentClass;
begin
  Result := TlqForm;
end;

procedure TlqMediator.GetBounds(AComponent: TComponent; out CurBounds: TRect);
var
  w: TlqWidget;
begin
  if AComponent is TlqWidget then
  begin
    w:=TlqWidget(AComponent);
    CurBounds:=Bounds(w.Left,w.Top,{w.Left +} w.Width, {w.Top +} w.Height);
  end else
    inherited GetBounds(AComponent,CurBounds);
end;

procedure TlqMediator.SetBounds(AComponent: TComponent; NewBounds: TRect);
begin
  if AComponent is TlqWidget then begin
    TlqWidget(AComponent).SetPosition(NewBounds.Left,NewBounds.Top,
      NewBounds.Right-NewBounds.Left,NewBounds.Bottom-NewBounds.Top);
  end else
  begin
    if ComponentIsIcon(AComponent) then
       SetComponentLeftTopOrDesignInfo(AComponent,NewBounds.Left,NewBounds.Top)
    else
    inherited SetBounds(AComponent,NewBounds);
  end;
end;

{procedure TlqMediator.GetClientArea(AComponent: TComponent; out
  CurClientArea: TRect; out ScrollOffset: TPoint);
begin
  inherited GetClientArea(AComponent, CurClientArea, ScrollOffset);
end;}

procedure TlqMediator.Paint;
var Bmp : TBitmap;
  LCanvas : TCanvas;

  procedure PaintFormGrid(AWidget: TlqWidget);
  var x, y : integer;
  const
    gx = 8; //TODO: Link to: EnvironmentOptions.GridSizeX
    gy = 8;
  begin
    for y := 0 to (Awidget.Height -1) div gy do
      for x := 0 to (Awidget.Width -1) div gx do
      begin
        LCanvas.Pixels[x *gx, y *gy] := clBlack;
      end;
  end;

  procedure PaintWidget(AWidget: TlqWidget);
  var
    i: Integer;
    Child: TlqWidget;
    msgp : TlqMessageParams;
    r : TRect;
    p : TPoint;
  begin
    if [csLoading, csDestroying] * AWidget.ComponentState <> [] then
       exit;
    //with LCLForm.Canvas do
    With LCanvas do
    begin
      // fill background
      {Brush.Style:=bsSolid;
      Brush.Color:= clBtnFace;
      FillRect(0,0,AWidget.Width,AWidget.Height);

      // outer frame
      Pen.Color:=clGray;
      Rectangle(0,0,AWidget.Width,AWidget.Height);
      }
      {// inner frame
      if AWidget.AcceptChildsAtDesignTime then begin
        Pen.Color:=clMaroon;
        Rectangle(AWidget.BorderLeft-1,AWidget.BorderTop-1,
                  AWidget.Width-AWidget.BorderRight+1,
                  AWidget.Height-AWidget.BorderBottom+1);
      end;
      // caption
      TextOut(5,2,AWidget.Caption);}

      //AWidget.Canvas.BeginDraw;
      //TlqWidgetAccess(AWidget).HandlePaint;
      //fillchar(msgp,sizeof(msgp),0);
      //pgfSendMessage(self, AWidget, PGFM_PAINT, msgp);
      AWidget.RePaint;

      //test canvas
      {AWidget.Canvas.DrawControlFrame(0,0,AWidget.Width, AWidget.Height);
      AWidget.Canvas.SetColor(clRed);
      AWidget.Canvas.DrawLine(0,AWidget.Height,AWidget.Width,0);

      if AWidget.Canvas.PaintTo(LCLForm.Canvas.Handle, 0,0, AWidget.Width, AWidget.Height) then
        TextOut(5,2,format('OK %d',[AWidget.WinHandle]) )
      else
        TextOut(5,2,'failpaint');

      bmp := TBitmap.Create;
      bmp.SetSize(AWidget.Width, AWidget.Height);
      AWidget.Canvas.PaintTo(bmp.Canvas.Handle, 0,0, AWidget.Width, AWidget.Height);
      bmp.SaveToFile('c:\'+AWidget.Name+'.bmp' );
      bmp.Free;}
///      AWidget.Canvas.PaintTo({LCLForm.Canvas.}Handle, 0,0, AWidget.Width, AWidget.Height);
      Draw(0,0, TlqLazCanvas(AWidget.Canvas).Bitmap);

      //AWidget.Canvas.EndDraw;
      //Pen.Color:=clGreen;
      //if csDesigning in AWidget.ComponentState then  TextOut(5,2,'design');
      //self.GetClientArea(Awidget, r, p );
      //r:= Rect(0,0, AWidget.Width, AWidget.Height);
      {Pen.Style:=psDot;
      Pen.Color:=clRed;
      //Rectangle(r);
      MoveTo(r.TopLeft);
      LineTo(r.BottomRight);}

      if AWidget is TlqForm then
         PaintFormGrid(AWidget);


      //TextOut(5,2,format('has%d,@%s',[AWidget.ChildCount, AWidget.ParentName]) ) ;

      // children
      if AWidget.ChildrenCount>0 then
      begin
        SaveHandleState;
        // clip client area
        //MoveWindowOrgEx(Handle,AWidget.BorderLeft,AWidget.BorderTop);
        MoveWindowOrgEx(Handle,0,0);
        //if IntersectClipRect(Handle, 0, 0, AWidget.Width-AWidget.BorderLeft-AWidget.BorderRight,
        //                     AWidget.Height-AWidget.BorderTop-AWidget.BorderBottom)<>NullRegion
        //then
        begin
          //for i:=0 to AWidget.ComponentCount-1 do
          //if (AWidget.Components[i] is TlqWidget) and (TlqWidget(AWidget.Components[i]).Parent = Awidget)  then
          for i:=0 to AWidget.ChildrenCount-1 do
          begin
            SaveHandleState;
            //Child:=TlqWidget(AWidget.Components[i]);
            Child:=AWidget.Children[i];
            // clip child area
            MoveWindowOrgEx(Handle,Child.Left,Child.Top);
            if IntersectClipRect(Handle,0,0,Child.Width,Child.Height)<>NullRegion then
              PaintWidget(Child);
            RestoreHandleState;
          end;
        end;
        RestoreHandleState;
      end;
    end;
  end;

begin
  {.$define LBuffered__Paint}

{$ifdef LBuffered__Paint}
  Bmp := TBitmap.Create;
  //Bmp.Monochrome:=False;
  Bmp.Transparent:=False;
  Bmp.SetSize(LCLForm.Width, LCLForm.Height);
  LCanvas := bmp.Canvas;
  //Bmp.BeginUpdate;
  //FlpForm.show();//allocate windowhandle
  PaintWidget(FlpForm);
  //FlpForm.Hide();

  //Bmp.EndUpdate;
  LCLForm.Canvas.Draw(0,0,Bmp);
  //lclForm.Canvas.Pen.Color:= clRed;
  //LCLForm.Canvas.TextOut(10,10, format('W=%d, H=%d',[bmp.Width, bmp.Height]));
  Bmp.Free;

{$else}
  LCanvas := LClForm.Canvas;
  PaintWidget(FlpForm);
{$endif}


//  m_pgfForm.Invalidate;
  inherited Paint;
end;

procedure TlqMediator.InitComponent(AComponent, NewParent: TComponent;
  NewBounds: TRect);
begin
  if AComponent is TlqWidget then
  begin
    if (NewBounds.Right - NewBounds.Left = 50) and //set by customformeditor.pas #1384 - 1385
       (NewBounds.Bottom - NewBounds.Top = 50) then
     newBounds := Bounds(newBounds.Left, newBounds.Top,
             TlqWidget(AComponent).Width, TlqWidget(AComponent).Height);

  end;
  inherited InitComponent(AComponent, NewParent, NewBounds);
end;

function TlqMediator.ComponentIsIcon(AComponent: TComponent): boolean;
begin
  Result:=not (AComponent is TlqWidget);
end;

function TlqMediator.ParentAcceptsChild(Parent: TComponent;
  Child: TComponentClass): boolean;
begin
  //result := true;
  Result:=(Parent is TlqWidget) //and ( wsAcceptsChildren in TlqWidget(Parent).WidgetStyle)
    and Child.InheritsFrom(TlqComponent)
    //or (not Child.InheritsFrom(TControl))
    //and (TlqWidget(Parent).AcceptChildsAtDesignTime);
end;

{$IfDef OiImageIndex}
procedure TlqMediator.OiNodeGetImageIndex(APersistent: TPersistent;
  var AIndex: integer);
begin
  if Assigned(APersistent) then
    begin
      {if (APersistent is TControl) and (csAcceptsControls in TControl(APersistent).ControlStyle) then
        Result := 3
      else
      if (APersistent is TControl) then
        Result := 2
      else
      if (APersistent is TComponent) then
        Result := 1
      else
      if (APersistent is TCollection) then
        Result := 4
      else
      if (APersistent is TCollectionItem) then
        Result := 5;}
      if APersistent is TlqWidget then
         AIndex := 2;
    end;
end;
{$endif}

constructor TlqMediator.Create(AOwner: TComponent);
begin

  //pgfOpenDisplay('');
  lq_main.DefaultCanvasClass := lq_canvas_designer.TlqLazCanvas;
  LQU_PREVENTWND := true;
  lqApplication.Initialize;  
  inherited Create(AOwner);
end;

destructor TlqMediator.Destroy;
begin
  //if FMyForm<>nil then FMyForm.Designer:=nil;
  //FMyForm:=nil;

  inherited Destroy;
end;

initialization
{$I lq_designtime.lrs}
end.

