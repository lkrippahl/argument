{*******************************************************************************
This file is part of Argument
This source file is public domain. It is provided as is, with no explicit
or implied warranties regarding anything whatsoever.
********************************************************************************
Author: Ludwig Krippahl
Date: ?.?.2009
Purpose:
  Main form for argument
Requirements:
Revisions:
To do:
  check conversion from Delphi and using new shared units
  call help
*******************************************************************************}

unit argmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls, Menus,argobjects, filebuffer,basetypes,
  stringutils,miscutils,Clipbrd;

const
  //V0: deprecated 27-09-09, changed to UTF8/lazarus
  SessionVersion=1;
  ControlBorder=10;


type

  { TMainForm }

  TMainForm = class(TForm)
    KeyEd: TEdit;
    OPNLObjectionsLbl: TLabel;
    OPNLReasonsLbl: TLabel;
    MainMenu1: TMainMenu;
    FileMi: TMenuItem;
    ObjectObjectionsMm: TMemo;
    ObjectTextMm: TMemo;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    OpenDialog1: TOpenDialog;
    OpenMi: TMenuItem;
    ObjectPropPnl: TPanel;
    SaveDialog1: TSaveDialog;
    SaveMi: TMenuItem;
    ClearMi: TMenuItem;
    SmallFontMi: TMenuItem;
    LargeFontMi: TMenuItem;
    ObjectsPb: TPaintBox;
    StatementsMm: TMemo;
    TextMm: TMemo;
    PageControl1: TPageControl;
    TextsTs: TTabSheet;
    DiagramTs: TTabSheet;
    procedure FormResize(Sender: TObject);
    procedure LargeFont1Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure SmallFontMiClick(Sender: TObject);
    procedure TextMmKeyPress(Sender: TObject; var Key: Char);
    procedure AddTextBtClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ObjectsPbPaint(Sender: TObject);
    procedure ObjectsPbMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ObjectsPbMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ObjectsPbDblClick(Sender: TObject);
    procedure ManualAddEdKeyPress(Sender: TObject; var Key: Char);
    procedure Save1Click(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure KeyEdKeyPress(Sender: TObject; var Key: Char);
    procedure KeyEdKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ObjectTextMmKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ObjectPropPnlExit(Sender: TObject);
    procedure Help1Click(Sender: TObject);
    procedure ClearObjects(Sender:TObject);
  private
    { Private declarations }
    FObjects:TAOObjects;
    FBmp:TBitMap;

    //information for dragging objects
    FGrabbed:TPoint;
    FObjectGrabbed:Integer;
    FDiagramKeys:Boolean;
    FBackGround:TColor;
    //Selected Object
    FSelObject:Integer;
    procedure RefreshTextsLb;
    procedure SetFontSize;
    procedure ResizeDiagramPage;
    procedure PaintObjects;
    function ObjectAt(Point:TPoint):Integer;
    procedure ObjectPbOnHint(Sender:TObject);
    procedure PrepareHint;
    //disables or enables all diagram controls for text
    procedure AddReason(s:string); //adds to ReasonsMm only
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{ TMainForm }

procedure TMainForm.FormResize(Sender: TObject);

var
   wid,hei:Integer;


begin
   PageControl1.Width:=ClientWidth-2*PageControl1.Left;;
   PageControl1.Height:=ClientHeight-2*PageControl1.Top;
   DiagramTs.Height:=PageControl1.ClientHeight;
   DiagramTs.Width:=PageControl1.ClientWidth;
   TextsTs.Height:=PageControl1.ClientHeight;
   TextsTs.Width:=PageControl1.ClientWidth;
   wid:=TextsTs.ClientRect.Right-TextsTs.ClientRect.Left;
   hei:=TextsTs.ClientRect.Bottom-TextsTs.ClientRect.Top;
   TextMm.Left:=ControlBorder;
   TextMm.Top:=ControlBorder;
   TextMm.Width:=wid div 2-ControlBorder-ControlBorder div 2;
   TextMm.Height:=hei-2*ControlBorder;
   StatementsMm.Top:=ControlBorder;
   StatementsMm.Width:=wid div 2-ControlBorder-ControlBorder div 2;
   StatementsMm.Left:=wid div 2 + ControlBorder div 2;
   StatementsMm.Height:=hei-2*ControlBorder;
   ResizeDiagramPage;
end;

procedure TMainForm.LargeFont1Click(Sender: TObject);
begin
  SetFontSize;
end;


procedure TMainForm.SetFontSize;
begin
  if SmallFontMi.Checked then
    begin
    MainForm.Font.Size:=20;
    LargeFontMi.Checked:=True;
    end
  else
    begin
    MainForm.Font.Size:=9;
    SmallFontMi.Checked:=True;
    end;
  FormResize(nil);
end;


procedure TMainForm.ResizeDiagramPage;
begin
  ObjectsPb.Left:=ControlBorder;
  ObjectsPb.Width:=DiagramTs.ClientWidth-ControlBorder*2;
  ObjectsPb.Top:=ControlBorder;
  ObjectsPb.Height:=DiagramTs.ClientHeight-ControlBorder*2;
  KeyEd.Width:=0;
  KeyEd.Height:=0;
  KeyEd.Left:=ControlBorder;
  KeyEd.Top:=ControlBorder;
  OPNLReasonsLbl.Top:=ControlBorder;
  OPNLReasonsLbl.Left:=ControlBorder;
  ObjectTextMm.Top:=OPNLReasonsLbl.Height+2*ControlBorder;
  ObjectTextMm.Left:=ControlBorder;
  ObjectTextMm.Width:=ObjectPropPnl.ClientWidth-2*ControlBorder;
  ObjectTextMm.Height:=(ObjectPropPnl.ClientHeight-
    5*ControlBorder-2*OPNLReasonsLbl.Height) div 2;
  OPNLObjectionsLbl.Left:=ControlBorder;
  OPNLObjectionsLbl.Top:=ObjectTextMm.Top+ObjectTextMm.Height+
    ControlBorder;
  ObjectObjectionsMm.Height:=ObjectTextMm.Height;
  ObjectObjectionsMm.Left:=ControlBorder;
  ObjectObjectionsMm.Width:=ObjectPropPnl.ClientWidth-2*ControlBorder;
  ObjectObjectionsMm.Top:=OPNLObjectionsLbl.Height+OPNLObjectionsLbl.Top+
    ControlBorder;

  FBmp.Width:=ObjectsPb.Width;
  FBmp.Height:=ObjectsPb.Height;
  ObjectsPb.Invalidate;
  PaintObjects;
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
  FormResize(Sender);
end;

procedure TMainForm.SmallFontMiClick(Sender: TObject);
begin

end;  

procedure TMainForm.TextMmKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key=#13) and (TextMm.SelLength>0) then
    begin
      AddTextBtClick(Sender);
      Key:=#0;
    end
end;

procedure TMainForm.AddTextBtClick(Sender: TObject);

var s:string;


procedure AddText(s:string);

begin
  AddReason(s);
  SetLength(FObjects,Length(FObjects)+1);
  FObjects[High(FObjects)]:=TAOBaseObject.Create(s);
  FObjects[High(FObjects)].LastPoint:=
    Point(10,10);
  FObjects[High(FObjects)].IsRoot:=True;
  PaintObjects;
end;

begin
  if TextMm.SelLength>0 then
    begin
    TextMm.CopyToClipboard;
    s:=ClipBoard.AsText;
    AddText(CleanString(s));
    end;
end;


procedure TMainForm.FormCreate(Sender: TObject);
begin
  FBackGround:=clBlack;
  FBmp:=TBitmap.Create;
  FBmp.Canvas.Brush.Color:=FBackGround;
  FDiagramKeys:=True;
  SetFontSize;

end;

procedure TMainForm.PaintObjects;

var f:Integer;

begin
  AOClearDrawn(FObjects);
  FBmp.Canvas.Brush.Color:=FBackground;
  FBmp.Canvas.FillRect(Rect(0,0,FBmp.Width,FBmp.Height));
  for f:=0 to High(FObjects) do
    if FObjects[f].IsRoot then
      FObjects[f].Draw(FObjects[f].LastPoint,FBmp);
  ObjectsPbPaint(nil);//ObjectsPb.Invalidate;
end;


procedure TMainForm.ObjectsPbPaint(Sender: TObject);
begin
  ObjectsPb.Canvas.Draw(0,0,FBmp);
end;

procedure TMainForm.ObjectsPbMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if FDiagramKeys then
  begin
  FGrabbed:=Point(X,Y);
  FObjectGrabbed:=ObjectAt(FGrabbed);
  if Button=MbRight then
    begin
    if FObjectGrabbed>=0 then
      begin
      AORemoveChild(FObjects,FObjects[FObjectGrabbed]);
      AORemoveConjunct(FObjects,FObjects[FObjectGrabbed]);
      FObjects[FObjectGrabbed].LastPoint:=Point(
        FObjects[FObjectGrabbed].LastPoint.X+10,
        FObjects[FObjectGrabbed].LastPoint.Y-10);
      FObjects[FObjectGrabbed].ObType:=AOUnknown;
      PaintObjects;
      end;
    end;
  end;
end;

function TMainForm.ObjectAt(Point: TPoint): Integer;

var
  f:Integer;
  r:TRect;

begin
  Result:=-1;
  for f:=High(FObjects) downto 0 do
      if FObjects[f].Drawn then
      begin
      r:=FObjects[f].SelfRect;
      if IsBetween(Point.X,r.Left,r.Right) and
        IsBetween(Point.Y,r.Top,R.Bottom) then
          begin
          Result:=f;
          Break;
          end;
      end
end;

procedure TMainForm.ObjectsPbMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);


var f:Integer;

procedure MoveObject(Ix:Integer);

var P:TPoint;

begin
  P:=FObjects[Ix].LastPoint;
  FObjects[Ix].LastPoint:=Point(P.X-FGrabbed.X+X,
    P.Y+Y-FGrabbed.Y);
end;

procedure SetHint;

begin
  if FObjectGrabbed<0 then
    begin
    ObjectsPb.Hint:='';
    ObjectsPb.ShowHint:=False;
    end
  else
    begin
    ObjectsPb.Hint:=FObjects[FObjectGrabbed].Text;
    ObjectsPb.ShowHint:=True;
    end;
end;

begin
  if FDiagramKeys then
  begin
  KeyEd.SetFocus;
  if Shift=[] then
    begin
    FObjectGrabbed:=ObjectAt(Point(X,Y));
    SetHint;
    end
  else if Shift=[ssLeft] then
    begin
    if (FObjectGrabbed>=0) and (FObjects[FObjectGrabbed].IsRoot)
      then MoveObject(FObjectGrabbed)
    else for f:=0 to High(FObjects) do
      if FObjects[f].IsRoot then MoveObject(f);
    FGrabbed.X:=X;
    FGrabbed.Y:=Y;
    PaintObjects;
    end
  end;
end;

procedure TMainForm.ObjectsPbDblClick(Sender: TObject);
begin
  if FDiagramKeys then
  begin
  if FSelObject>=0 then
    FObjects[FSelObject].Selected:=False;
  FSelObject:=ObjectAt(FGrabbed);
  if FSelObject>=0 then
    FObjects[FSelObject].Selected:=True;
  PaintObjects;
  end;
end;

procedure TMainForm.ManualAddEdKeyPress(Sender: TObject; var Key: Char);
begin
  if Key=#13 then
    begin
    Key:=#0;
    AddTextBtClick(Sender);
    end;
end;

procedure TMainForm.ObjectPbOnHint(Sender: TObject);
begin

end;

procedure TMainForm.Save1Click(Sender: TObject);

var
  Buf:TFIleBuffer;
  atext:TSimpleStrings;

begin
  SaveDialog1.Filter:='Argument Session|*.ags';
  if SaveDialog1.Execute then
    begin
    Buf:=TFileBuffer.Create;
    Buf.WriteInteger(SessionVersion);
    atext:=AsSimpleStrings(TextMm.Lines);
    Buf.WriteStrings(atext);
    AOWriteObjects(FObjects,Buf);
    Buf.FlushToFile(SaveDialog1.FileName);
    Buf.Free;
    end;
end;

procedure TMainForm.Open1Click(Sender: TObject);

var
  Buf:TFileBuffer;
  atext:TSimpleStrings;
  ver,f:Integer;

begin
  OpenDialog1.Filter:='Argument Session|*.ags';
  if OpenDialog1.Execute then
    begin
    Buf:=TFileBuffer.Create;
    Buf.LoadFromFile(OpenDialog1.FileName);
    ver:=Buf.GetInteger;
    Buf.ReadStrings(atext);
    //version 0 with Delphi
    //strings saved as ansi
    if ver=0 then
      for f:=0 to High(atext) do
        atext[f]:=ANSIToUTF8(atext[f]);
    TextMm.Lines.Clear;
    AppendToStringList(atext,TextMm.Lines);
    AOClearObjects(FObjects);
    AOReadObjects(FObjects,Buf);
    RefreshTextsLb;
    Buf.Free;
    PaintObjects;
    end;
end;

procedure TMainForm.KeyEdKeyPress(Sender: TObject; var Key: Char);

procedure SetAsReason(Reas,Conc:Integer);

begin
  if (Reas>=0) and (FObjects[Reas].ObType=AOUnknown) then
    FObjects[Reas].ObType:=AOReason;
  if (Conc>=0) and (FObjects[Conc].ObType=AOUnknown) then
    FObjects[Conc].ObType:=AOReason;
  if (Reas>=0) and (Conc>=0) and (Reas<>Conc) and FObjects[Reas].IsRoot
    and (FObjects[Conc].ObType=AOReason) and
      not FObjects[Conc].ConnectsTo(FObjects[Reas]) and
    not FObjects[Reas].ConnectsTo(FObjects[Conc]) then
    begin
    FObjects[Reas].IsRoot:=False;
    FObjects[Conc].AddChild(FObjects[Reas],0);
    end;
end;

procedure SetAsPremis(Premis:Integer);

begin
  if (Premis>=0) and not FObjects[Premis].HasChildren then
    begin
    if FObjects[Premis].ObType=AOReason then
      FObjects[Premis].ObType:=AOAssumption
    else FObjects[Premis].ObType:=AOReason;
    end;
end;

procedure SetConjunct(AdTo,AdFrom:Integer);

begin
  if (AdTo>=0) and (AdFrom>=0) and (AdTo<>AdFrom) and
    (FObjects[AdFrom].IsRoot) and
      not FObjects[AdTo].ConnectsTo(FObjects[AdFrom]) and
    not FObjects[AdFrom].ConnectsTo(FObjects[AdTo]) then
    begin
    FObjects[AdTo].AddConjunct(FObjects[AdFrom]);
    FObjects[AdFrom].IsRoot:=False;
    end;
end;

procedure DeleteObject(Ix:Integer);

var f:Integer;

begin
  if (Ix>=0) and (FObjects[Ix].IsRoot) and
    not FObjects[Ix].HasChildren then
    begin
    FObjects[Ix].Free;
    for f:=Ix to High(FObjects)-1 do
      FObjects[f]:=FObjects[f+1];
    SetLength(FObjects,Length(FObjects)-1);
    RefreshTextsLb;
    end;
end;

begin
  if FDiagramKeys then
  begin
  if (FObjectGrabbed>=0) then
  case Key of
    'c','r','R','C':SetAsReason(FObjectGrabbed,FSelObject);
    'p','P':SetAsPremis(FSelObject);
    'o','O':if FObjectGrabbed>=0 then
      FObjects[FObjectGrabbed].Open:=not FObjects[FObjectGrabbed].Open;
    '+':SetConjunct(FSelObject,FObjectGrabbed);
    'v':if FObjectGrabbed>=0 then
      AOToggleParentLink(FObjects,FObjects[FObjectGrabbed]);
    #8,#95:
      begin
      DeleteObject(FSelObject);
      FSelObject:=-1;
      FObjectGrabbed:=-1;
      end;
    end;
    case Key of
    'b','B':if FBackGround=clBlack then
               begin
               FBackGround:=clWhite;
               AOLineColor:=clBlack;
               AODefaultBorder:=clBlack;
               end
             else
                begin
                FBackGround:=clBlack;
                AOLineColor:=clWhite;
                AODefaultBorder:=clWhite;
                end;
    end;
  PaintObjects;
  end;
  Key:=#0;
end;

procedure TMainForm.RefreshTextsLb;

var f:Integer;

begin
  StatementsMm.Lines.Clear;
  for f:=0 to High(FObjects) do
    AddReason(FObjects[f].Text);
end;

procedure TMainForm.KeyEdKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if FDiagramKeys and (Key=27) then
    PrepareHint;
end;

procedure TMainForm.PrepareHint;

var X,Y,f:Integer;

begin
  f:=FObjectGrabbed;
  if f>=0 then
    begin
    X:=FObjects[f].LastPoint.X;
    Y:=FObjects[f].LastPoint.Y;
    if X+ObjectPropPnl.Width>DiagramTs.ClientWidth-ControlBorder then
      X:=DiagramTs.ClientWidth-ControlBorder-ObjectPropPnl.Width;
    if Y+ObjectPropPnl.Height>DiagramTs.ClientHeight-ControlBorder then
      Y:=DiagramTs.ClientHeight-ControlBorder-ObjectPropPnl.Height;
    ObjectPropPnl.Top:=Y;
    ObjectPropPnl.Left:=X;
    ObjectPropPnl.Visible:=True;
    FDiagramKeys:=False;
    ObjectTextMm.Text:=FObjects[f].Text;
    ObjectObjectionsMm.SetFocus;
    ObjectObjectionsMm.Lines.Text:=FObjects[f].Objections;
    ObjectObjectionsMm.CaretPos:=Point(0,0);
    end;
end;

procedure TMainForm.ObjectTextMmKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key=27 then
    begin
    Key:=0;
    FObjects[FObjectGrabbed].Text:=ObjectTextMm.Text;
    FObjects[FObjectGrabbed].Objections:=ObjectObjectionsMm.Text;
    ObjectPropPnl.Visible:=False;
    FDiagramKeys:=True;
    PaintObjects;
    end;
end;

procedure TMainForm.ObjectPropPnlExit(Sender: TObject);

var Key:Word;
begin
  Key:=27;
  ObjectTextMmKeyDown(Sender,Key,[]);
end;

procedure TMainForm.AddReason(s: string);
begin
  StatementsMm.Lines.Add('');
  StatementsMm.Lines.Add('- '+CleanString(s));
end;

procedure TMainForm.Help1Click(Sender: TObject);
begin
   //ShellExecute(0,'open',PChar(ExtractFilePath(Application.ExeName)+'Help.html'),nil,nil,0);
end;

procedure TMainForm.ClearObjects(Sender:TObject);

var f:Integer;

begin
  for f:=0 to High(FObjects) do
    FObjects[f].Free;
  FObjects:=nil;
  RefreshTextsLb;
  TextMm.Clear;
  PaintObjects;
end;

initialization
  {$I argmain.lrs}

end.

