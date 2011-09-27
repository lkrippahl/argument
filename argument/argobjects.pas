{*******************************************************************************
This file is part of Argument
This source file is public domain. It is provided as is, with no explicit
or implied warranties regarding anything whatsoever.
********************************************************************************
Author: Ludwig Krippahl
Date: ?.?.2009
Purpose:
  Argument diagram classes
Requirements:
Revisions:
To do:
  manipulating an array of objects is not as practical as a container class
  for the whole diagram
  also review the code (I don't remember most of it anymore...)
  comments
*******************************************************************************}

unit argobjects;

{$mode objfpc}{$H+}

interface

uses SysUtils,Graphics, Types, Classes, filebuffer,
  basetypes, stringutils;

const
  //Version is the same for all AObjects
  //0 deprecated on 27-9-06; added FOBjections:string to TAOAbstract
  //1 deprecated on 27-09-09, changed to utf8
  AOVersion=2;
  DefaultHeight=80;
  DefaultWidth=200;
  DefaultFont=12;
  AOUnknown=-1;
  AOReason=0;//a reason with children is a conclusion
  AOAssumption=1;

  //Inference steps
  AOInvalid=0;
  AOValid=1;


type
  // this is an abstract element
  // For the base class use TAOBaseObject
  TAOAbstract=class
  private
    FVersion:Integer;
    FIsRoot,FDrawn,FOpen,FSelected:Boolean;
    FLastPoint:TPoint;
    FText,FTitle,FObjections:string;
    FType:Integer;
    FSideLink:TAOAbstract;
  public
    Tag:Integer;
    property Text:string read FText write FText;
    property Objections:string read FObjections write FObjections;
    property ObType:Integer read FType write FType;
    property Selected:Boolean read FSelected write FSelected;
    property LastPoint:TPoint read FLastPoint write FLastPoint;
    property Version:Integer read FVersion;
    property Open:Boolean read FOpen write FOpen;
    property Drawn:Boolean read FDrawn write FDrawn;
    property IsRoot:Boolean read FIsRoot write FIsRoot;
    procedure WriteToBuffer(Buf:TFileBuffer);virtual;abstract;
    procedure ReadFromBuffer(Buf:TFileBuffer);virtual;abstract;
    procedure Draw(TopLeft:TPoint;Bmp:TBitmap);virtual;abstract;
    procedure AddChild(Child:TAOAbstract;Link:Integer);virtual;abstract;
    procedure AddConjunct(Element:TAOAbstract);virtual;abstract;
    function GetRect:TRect;virtual;abstract;
    function SelfRect:TRect;virtual;abstract;
    procedure MarkAsDrawn;virtual;abstract;
    procedure RemoveChild(Child:TAOAbstract);virtual;abstract;
    procedure RemoveConjunct(Child:TAOAbstract);virtual;abstract;
    function ChildrenTags:TIntegers;virtual;abstract;
    function SideLinkTag:Integer;virtual;abstract;
    function ConnectsTo(Ob:TAOAbstract):Boolean;virtual;abstract;
    function IsSupported:Boolean;virtual;abstract;
  end;
  TAOAbstracts=array of TAOAbstract;
  //Base object class for all objects. Does not draw frame

  TAOBaseObject=class(TAOAbstract)
  private
    FChildren:TAOAbstracts;
    FLinks:TIntegers;
    //for loading connections only
    procedure SetChild(Child:TAOAbstract;Ix:integer);
  public
    constructor Create(atext:string);
    procedure WriteToBuffer(Buf:TFileBuffer);override;
    procedure ReadFromBuffer(Buf:TFileBuffer);override;
    procedure Draw(TopLeft:TPoint;Bmp:TBitmap);override;
    procedure AddChild(Child:TAOAbstract;Link:Integer);override;
    procedure AddConjunct(Element:TAOAbstract);override;
    function GetRect:TRect;override;
    function SelfRect:TRect;override;
    procedure MarkAsDrawn;override;
    procedure RemoveChild(Child:TAOAbstract);override;
    procedure RemoveConjunct(Child:TAOAbstract);override;
    function ChildrenTags:TIntegers;override;
    function SideLinkTag:Integer;override;
    function HasChildren:Boolean;
    function ConnectsTo(Ob:TAOAbstract):Boolean;override;
    procedure RemoveLinks;
    function ChildIx(Child:TAOAbstract):Integer;
    function ChildByIx(Ix:Integer):TAOAbstract;
    function GetLink(Ix:Integer):Integer;
    procedure SetLink(Ix,L:Integer);
    function IsSupported:Boolean;override;
  end;
  TAOObjects=array of TAOBaseObject;
  procedure AORemoveChild(Objects:TAOObjects;Child:TAOBaseObject);
  procedure AORemoveConjunct(Objects:TAOObjects;Child:TAOBaseObject);
  procedure AOWriteLinksToBuffer(Objects:TAOObjects;Buf:TFileBuffer);
  procedure AOReadLinksFromBuffer(Objects:TAOObjects;Buf:TFileBuffer);
  procedure AOWriteObjects(Objects:TAOObjects;Buf:TFileBuffer);
  procedure AOReadObjects(out Objects:TAOObjects;Buf:TFileBuffer);
  procedure AOClearObjects(Objects:TAOOBjects);
  procedure AOClearDrawn(Objects:TAOOBjects);
  function AOGetParent(Objects:TAOOBjects;Child:TAOAbstract):TAOBaseObject;
  procedure AOToggleParentLink(Objects:TAOOBjects;Child:TAOAbstract);

var
  //global variables for colors. TO DO: put everything into class?
  AOLineColor:TColor=clWhite;
  AODefaultBorder:TColor=clWhite;

implementation

procedure AOClearDrawn(Objects:TAOOBjects);

var f:Integer;

begin
  for f:=0 to High(Objects) do Objects[f].Drawn:=False;
end;


function AOGetParent(Objects:TAOOBjects;Child:TAOAbstract):TAOBaseObject;

var f:Integer;

begin
  Result:=nil;
  for f:=0 to High(Objects) do
    begin
    if Objects[f].ChildIx(Child)>=0 then
      begin
      Result:=Objects[f];
      Break;
      end;
    end
end;

procedure AOToggleParentLink(Objects:TAOOBjects;Child:TAOAbstract);

var
  Parent:TAOBaseObject;
  Ix:Integer;

begin
  Parent:=AOGetParent(Objects,Child);
  if Parent<>nil then
    begin
    Ix:=Parent.ChildIx(Child);
    if Ix>=0 then
    case Parent.GetLink(Ix) of
       AOInvalid:Parent.SetLink(Ix,AOValid);
       AOValid:Parent.SetLink(Ix,AOInvalid);
    end;
    end;
end;

procedure AORemoveChild(Objects:TAOObjects;Child:TAOBaseObject);

var f:Integer;

begin
  for f:=0 to High(Objects) do
    Objects[f].RemoveChild(Child);
  Child.IsRoot:=True;
end;

procedure AORemoveConjunct(Objects:TAOObjects;Child:TAOBaseObject);

var f:Integer;

begin
  for f:=0 to High(Objects) do
    Objects[f].RemoveConjunct(Child);
  Child.IsRoot:=True;
end;

procedure AOWriteLinksToBuffer(Objects:TAOObjects;Buf:TFileBuffer);

var f:Integer;

begin
  Buf.WriteInteger(AOVersion);
  for f:=0 to High(Objects) do
    Objects[f].Tag:=f;
  for f:=0 to High(Objects) do
    begin
    Buf.WriteIntegers(Objects[f].ChildrenTags);
    Buf.WriteInteger(Objects[f].SideLinkTag);
    end;
end;


procedure AOReadLinksFromBuffer(Objects:TAOObjects;Buf:TFileBuffer);

var
  f,g:Integer;
  Ixs:TIntegers;


begin
  //skip version
  Buf.ReadInteger(f);
  for f:=0 to High(Objects) do
    begin
    Buf.ReadIntegers(Ixs);
    for g:=0 to High(Ixs) do
      Objects[f].SetChild(Objects[Ixs[g]],g);
    Buf.ReadInteger(g);
    if g>=0 then Objects[f].AddConjunct(Objects[g]);
    end;

end;

procedure AOWriteObjects(Objects:TAOObjects;Buf:TFileBuffer);

var f:Integer;

begin
  Buf.WriteInteger(AOVersion);
  Buf.WriteInteger(Length(Objects));
  for f:=0 to High(Objects) do
    Objects[f].WriteToBuffer(Buf);
  AOWriteLinksToBuffer(Objects,Buf);
end;

procedure AOReadObjects(out Objects:TAOObjects;Buf:TFileBuffer);

var f:Integer;

begin
  //skip version
  Buf.ReadInteger(f);
  Buf.ReadInteger(f);
  SetLength(Objects,f);
  for f:=0 to High(Objects) do
    begin
    Objects[f]:=TAOBaseObject.Create('');
    Objects[f].ReadFromBuffer(Buf);
    end;
  AOReadLinksFromBuffer(Objects,Buf);
end;

procedure AOClearObjects(Objects:TAOOBjects);

var f:Integer;

begin
  for f:=0 to High(Objects) do Objects[f].Free;
end;

{ TAOBaseObject }

procedure TAOBaseObject.AddChild(Child: TAOAbstract; Link: Integer);
begin
  AddToArray(Link,FLinks);
  SetLength(FChildren,Length(FChildren)+1);
  FChildren[High(FChildren)]:=Child;
  if FType=AOUnknown then FType:=AOReason;
  if Child.FType=AOUnknown then Child.FType:=AOReason;
end;

procedure TAOBaseObject.AddConjunct(Element: TAOAbstract);
begin
  if FSideLink=nil then FSideLink:=Element
  else FSideLink.AddConjunct(Element);
end;

function TAOBaseObject.ChildByIx(Ix: Integer): TAOAbstract;
begin
  Assert((Ix>=0) and (Ix<=High(FChildren)),'Index out of bounds ('+IntToStr(Ix)+')');
  Result:=FChildren[Ix];
end;

function TAOBaseObject.ChildIx(Child: TAOAbstract): Integer;

begin
  Result:=High(FChildren);
  while (Result>=0) and (FChildren[Result]<>Child) do
    Dec(Result);
end;

function TAOBaseObject.ChildrenTags: TIntegers;

var f:Integer;

begin
  SetLength(Result,Length(FChildren));
  for f:=0 to High(Result) do Result[f]:=FChildren[f].Tag;
end;

function TAOBaseObject.ConnectsTo(Ob: TAOAbstract): Boolean;

var f:Integer;

begin
  Result:=(FSideLink=Ob);
  if not Result and (FSideLink<>nil) then
    Result:=FSideLink.ConnectsTo(Ob);
  if not Result then
    for f:=0 to High(FChildren) do
      begin
      Result:=(FChildren[f]=Ob) or FChildren[f].ConnectsTo(Ob);
      if Result then Break;
      end;
end;

constructor TAOBaseObject.Create(atext: string);
begin
  inherited Create;
  FText:=atext;
  FVersion:=AOVersion;
  FType:=AOUnknown;
  FOpen:=True;
end;

procedure TAOBaseObject.Draw(TopLeft:TPoint;Bmp:TBitmap);

var DrawRect:TRect;

procedure SetFrame;

begin
  Bmp.Canvas.Pen.Width:=3;
  if FSelected then Bmp.Canvas.Pen.Width:=10;
  if (FType=AOAssumption) or (FType=AOUnknown) then
    begin
    Bmp.Canvas.Brush.Color:=clSilver;
    Bmp.Canvas.Pen.Color:=AODefaultBorder;
    Bmp.Canvas.FillRect(DrawRect);
    Bmp.Canvas.Polygon([DrawRect.TopLeft,
      Point(DrawRect.Right,DrawRect.Top),DrawRect.BottomRight,
      Point(DrawRect.Left,DrawRect.Bottom),DrawRect.TopLeft]);
    if FType=AOAssumption then FTitle:='Pressuposto'
      else FTitle:='?';
    end
  else if FChildren=nil then
    begin
    Bmp.Canvas.Brush.Color:=$FFFFE0;
    Bmp.Canvas.Pen.Color:=clAqua;
    Bmp.Canvas.RoundRect(DrawRect.Left,DrawRect.Top,DrawRect.Right,DrawRect.Bottom,
      DefaultWidth div 50,DefaultWidth div 50);
    FTitle:='Razão';
    end
  else
    begin
    Bmp.Canvas.Brush.Color:=clWhite;
    Bmp.Canvas.Pen.Color:=clRed;
    Bmp.Canvas.RoundRect(DrawRect.Left,DrawRect.Top,DrawRect.Right,DrawRect.Bottom,
      DefaultWidth div 50,DefaultWidth div 50);
    if IsSupported then FTitle:='Conclusão'
      else FTitle:='? Conclusão ?';
    end;
end;

procedure DrawText;

var
  s,s1,s2:string;
  charcount:Integer;

procedure CenterText(Y:Integer;s:string);

var Len:Integer;

begin
  Len:=Bmp.Canvas.TextWidth(s);
  Bmp.Canvas.TextOut((DrawRect.Right+DrawRect.Left-Len) div 2,Y,s);
end;

begin
  Bmp.Canvas.Font.Color:=clBlue;
  Bmp.Canvas.Font.Size:=Round(DefaultFont*1.2);
  Bmp.Canvas.Font.Style:=[fsBold];
  CenterText(DrawRect.Top+Round(DefaultFont*0.5), FTitle);
  if FObjections<>'' then
    begin
    Bmp.Canvas.Font.Color:=clRed;
    Bmp.Canvas.TextOut(DrawRect.Left+DefaultWidth div 20,DrawRect.Top+Round(DefaultFont*0.5),'!');
    end;
  Bmp.Canvas.Font.Color:=clBlack;
  Bmp.Canvas.Font.Size:=Round(DefaultFont);
  Bmp.Canvas.Font.Style:=[];
  if Bmp.Canvas.TextWidth(FText)<0.8*(DrawRect.Right-DrawRect.Left) then
    CenterText((DrawRect.Top+DrawRect.Bottom-DefaultFont) div 2, FText)
  else
    begin
    //s:=MUCleanString(FText);
    s:=FText;
    s1:=Copy(s,1,Length(s) div 2);
    s2:=Copy(s,Length(s) div 2,Length(FText));
    charcount:=Round(0.8*(DrawRect.Right-DrawRect.Left)/Bmp.Canvas.TextWidth('x'));
    Delete(s1,charcount+1,Length(s1));
    Delete(s2,1,Length(s2)-charcount);
    while (s2[1]<>' ') and (s2<>'') do Delete(s2,1,1);
    while (s1[Length(s1)]<>' ') and (s1<>'') do Delete(s1,Length(s1),1);
    CenterText((DrawRect.Top+DrawRect.Bottom-2*DefaultFont) div 2, s1);
    CenterText((DrawRect.Top+DrawRect.Bottom+2*DefaultFont) div 2, s2);

    end;
end;

procedure DrawLink(P,C:TRect;LinkType:Integer);

var s:string;
    x,y,h:Integer;

begin
  Bmp.Canvas.Pen.Color:=AOLineColor;
  Bmp.Canvas.Pen.Width:=4;
  x:=(C.Left+C.Right) div 2;
  y:=P.Top-DefaultHeight div 2;
  h:=DefaultHeight div 4;
  Bmp.Canvas.MoveTo(x,P.Top-DefaultHeight div 2);
  Bmp.Canvas.LineTo(x,C.Bottom);
  case LinkType of
    AOInvalid:
      begin
      Bmp.Canvas.Brush.Color:=clRed;
      s:='?';
      end;
    AOValid:
      begin
      Bmp.Canvas.Brush.Color:=clGreen;
      s:='OK';
      end;
    end;
  Bmp.Canvas.Pen.Width:=2;
  Bmp.Canvas.Ellipse(Rect(x-h,y-h,x+h,y+h));
  Bmp.Canvas.Font.Size:=h div 2;
  Bmp.Canvas.Font.Style:=[fsBold];
  Bmp.Canvas.Font.Color:=clWhite;
  Bmp.Canvas.TextOut(x-Bmp.Canvas.TextWidth(s) div 2,
   y-Bmp.Canvas.TextHeight(s) div 2,s);
end;

procedure DrawChildren;

var
  Rects:array of TRect;
  f:Integer;
  TotWid,Wid:Integer;

procedure HorizontalLine(R0,R1,R2:TRect);

begin
  Bmp.Canvas.Pen.Color:=AOLineColor;
  Bmp.Canvas.Pen.Width:=4;
  Bmp.Canvas.MoveTo((R1.Left+R1.Right) div 2,
      R0.Top-DefaultHeight div 2);
  Bmp.Canvas.LineTo((R2.Left+R2.Right) div 2,
      R0.Top-DefaultHeight div 2);
  Bmp.Canvas.MoveTo((R0.Left+R0.Right) div 2,
      R0.Top-DefaultHeight div 2);
  Bmp.Canvas.LineTo((R0.Left+R0.Right) div 2,
      R0.Top);
end;

begin
  if FChildren<>nil then
    begin
    TotWid:=0;
    SetLength(Rects,Length(FChildren));
    for f:=0 to High(FChildren) do
      begin
      Rects[f]:=FChildren[f].GetRect;
      TotWid:=Rects[f].Right-Rects[f].Left+TotWid;
      end;
    TotWid:=(DrawRect.Left+DrawRect.Right-TotWid-High(FChildren)*DefaultWidth div 5) div 2;

    for f:=0 to High(FChildren) do
      begin
      Wid:=Rects[f].Right-Rects[f].Left;
      Rects[f].Left:=TotWid;
      Rects[f].Right:=TotWid+Wid;
      Wid:=Rects[f].Bottom-Rects[f].Top;
      Rects[f].Top:=DrawRect.Top-Wid-DefaultHeight;
      Rects[f].Bottom:=Rects[f].Top+Wid;
      TotWid:=Rects[f].Right+DefaultWidth div 5;
      FChildren[f].Draw(Point(Rects[f].Left,Rects[f].Top),Bmp);
      end;
    HorizontalLine(DrawRect,Rects[0],Rects[High(Rects)]);
    for f:=0 to High(FChildren) do
      DrawLink(DrawRect,Rects[f],FLinks[f]);

    end;
end;


begin
  FDrawn:=True;
  FLastPoint:=TopLeft;
  DrawRect:=SelfRect;
  SetFrame;
  DrawText;
  if FSideLink<>nil then
    FSideLink.Draw(Point(DrawRect.Right,TopLeft.Y),Bmp);
  if FOpen then DrawChildren;
end;

function TAOBaseObject.GetLink(Ix: Integer): Integer;
begin
  Assert((Ix>=0) and (Ix<=High(FLinks)),'Index out of bounds ('+IntToStr(Ix)+')');
  Result:=FLinks[Ix];
end;

function TAOBaseObject.GetRect: TRect;

var Tmp:TRect;

begin
  Result:=SelfRect;
  if FSideLink<>nil then
    begin
    Tmp:=FSideLink.GetRect;
    Result.Right:=Result.Right+Tmp.Right-Tmp.Left;
    Result.Bottom:=Tmp.Bottom-Tmp.Top+Result.Top;
    end;
end;

function TAOBaseObject.HasChildren: Boolean;
begin
  Result:=FChildren<>nil;
end;

function TAOBaseObject.IsSupported: Boolean;

var f:Integer;

begin
  Result:=False;
  if (CleanString(FObjections)='') and
    ((FSideLink=nil) or FSideLink.IsSupported) then
      begin
      Result:=FChildren=nil;
      for f:=0 to High(FChildren) do
        if (FLinks[f]=AOValid) and (FChildren[f].IsSupported) then
          begin
          Result:=True;
          Break;
          end;
      end;
end;

procedure TAOBaseObject.MarkAsDrawn;

var f:Integer;

begin
  FDrawn:=True;
  if FSideLink<>nil then FSideLink.MarkAsDrawn;
  for f:=0 to High(FChildren) do
    FChildren[f].MarkAsDrawn;
end;

procedure TAOBaseObject.ReadFromBuffer(Buf: TFileBuffer);
begin
  Buf.ReadInteger(FVersion);
  Buf.ReadBoolean(FIsRoot);
  Buf.ReadBoolean(FDrawn);
  Buf.ReadBoolean(FOpen);
  Buf.ReadBoolean(FSelected);
  Buf.ReadInteger(FLastPoint.X);
  Buf.ReadInteger(FLastPoint.Y);
  Buf.ReadString(FText);
  Buf.ReadString(FTitle);
  Buf.ReadInteger(FType);
  Buf.ReadIntegers(FLinks);
  //Set length of children to set links
  //on AOReadLinksFromBuffer
  SetLength(FChildren,Length(FLinks));
  //Clear SideLink to set on AOReadLinksFromBuffer
  FSideLink:=nil;
  //27-9-06 Added FObjections
  if FVersion>0 then
    Buf.ReadString(FObjections);

  //built in Delphi, using ansi strings
  if FVersion<2 then
    begin
    FText:=ANSIToUTF8(FText);
    FTitle:=ANSIToUTF8(FTitle);
    FObjections:=ANSIToUTF8(FObjections);
    end;


  //reset version ID latest
  FVersion:=AOVersion;
end;

procedure TAOBaseObject.RemoveChild(Child: TAOAbstract);

var f,Pf:Integer;

begin
  Pf:=0;
  for f:=0 to High(FChildren) do
    if FChildren[f]<>Child then
      begin
      FChildren[Pf]:=FChildren[f];
      FLinks[Pf]:=FLinks[f];
      Inc(Pf);
      end;
  SetLength(FChildren,Pf);
  SetLength(FLinks,Pf);
end;

procedure TAOBaseObject.RemoveConjunct(Child: TAOAbstract);
begin
  if FSideLink=Child then
    FSideLink:=Child.FSideLink;
end;

procedure TAOBaseObject.RemoveLinks;
begin

end;

function TAOBaseObject.SelfRect: TRect;
begin
  Result.Left:=FLastPoint.X;
  Result.Right:=Result.Left+DefaultWidth;
  Result.Top:=FLastPoint.Y;
  Result.Bottom:=Result.Top+DefaultHeight;
end;

procedure TAOBaseObject.SetChild(Child: TAOAbstract; Ix: integer);
begin
  FChildren[Ix]:=Child;
end;

procedure TAOBaseObject.SetLink(Ix,L: Integer);
begin
  Assert((Ix>=0) and (Ix<=High(FLinks)),'Index out of bounds ('+IntToStr(Ix)+')');
  FLinks[Ix]:=L;
end;

function TAOBaseObject.SideLinkTag:Integer;
begin
  if FSideLink=nil then Result:=-1
  else Result:=FSideLink.Tag;
end;

procedure TAOBaseObject.WriteToBuffer(Buf: TFileBuffer);
begin
  Buf.WriteInteger(FVersion);
  Buf.WriteBoolean(FIsRoot);
  Buf.WriteBoolean(FDrawn);
  Buf.WriteBoolean(FOpen);
  Buf.WriteBoolean(FSelected);
  Buf.WriteInteger(FLastPoint.X);
  Buf.WriteInteger(FLastPoint.Y);
  Buf.WriteString(FText);
  Buf.WriteString(FTitle);
  Buf.WriteInteger(FType);
  Buf.WriteIntegers(FLinks);
  //version 1
  Buf.WriteString(FObjections);
end;

{ TAOAbstract }


end.
