unit Main; // vim:sw=2:

(* Copyright (c) 2019 by Stefan Vorkoetter

   This file is part of MusicRack.

   MusicRack is free software: you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free
   Software Foundation, either version 3 of the License, or (at your option)
   any later version.

   MusicRack is distributed in the hope that it will be useful, but WITHOUT ANY
   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
   details.

   You should have received a copy of the GNU General Public License along with
   MusicRack. If not, see <http://www.gnu.org/licenses/>. *)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, StdCtrls, Annotation, Open, Types;

type

  { TMainForm }

  TMainForm = class(TForm)

    LeftFiller: TImage;
    LeftImage: TImage;
    MiddleFiller: TImage;
    RightImage: TImage;
    RightFiller: TImage;

    FormCommandMenu: TPopupMenu;
      OpenMenuItem: TMenuItem;
      Separator1: TMenuItem;
      QuitMenuItem: TMenuItem;

    AnnotationCommandMenu: TPopupMenu;
      StyleMenuItem: TMenuItem;
	BoldMenuItem: TMenuItem;
	ItalicMenuItem: TMenuItem;
	UnderlineMenuItem: TMenuItem;
	StrikeoutMenuItem: TMenuItem;
      ColorMenuItem: TMenuItem;
        BlackMenuItem: TMenuItem;
	RedMenuItem: TMenuItem;
	GreenMenuItem: TMenuItem;
	BlueMenuItem: TMenuItem;
	GrayMenuItem: TMenuItem;
	HighlightMenuItem: TMenuItem;
      SizeMenuItem: TMenuItem;
        SmallMenuItem: TMenuItem;
	MediumMenuItem: TMenuItem;
	LargeMenuItem: TMenuItem;
	SubtitleMenuItem: TMenuItem;
	TitleMenuItem: TMenuItem;
      Separator4: TMenuItem;
      RemoveMenuItem: TMenuItem;
      Separator2: TMenuItem;
      OpenMenuItem1: TMenuItem;
      Separator3: TMenuItem;
      QuitMenuItem1: TMenuItem;

    EditMemo: TMemo;
    GPIOScanTimer: TTimer;
    ProgressLabel: TLabel;

    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

    procedure GPIOScanTimerTimer(Sender: TObject);

    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);

    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
			    Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
			  Shift: TShiftState; X, Y: Integer);

    procedure ImageDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure ImageDragDrop(Sender, Source: TObject; X, Y: Integer);

    procedure EditMemoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );

    procedure AnnotationCommandMenuPopup(Sender: TObject);

    procedure BoldMenuItemClick(Sender: TObject);
    procedure ItalicMenuItemClick(Sender: TObject);
    procedure UnderlineMenuItemClick(Sender: TObject);
    procedure StrikeoutMenuItemClick(Sender: TObject);

    procedure BlackMenuItemClick(Sender: TObject);
    procedure RedMenuItemClick(Sender: TObject);
    procedure GreenMenuItemClick(Sender: TObject);
    procedure BlueMenuItemClick(Sender: TObject);
    procedure GrayMenuItemClick(Sender: TObject);
    procedure HighlightMenuItemClick(Sender: TObject);

    procedure SmallMenuItemClick(Sender: TObject);
    procedure MediumMenuItemClick(Sender: TObject);
    procedure LargeMenuItemClick(Sender: TObject);
    procedure SubtitleMenuItemClick(Sender: TObject);
    procedure TitleMenuItemClick(Sender: TObject);

    procedure RemoveMenuItemClick(Sender: TObject);
    procedure OpenMenuItemClick(Sender: TObject);
    procedure QuitMenuItemClick(Sender: TObject);
  private
    { private declarations }
    startup: Boolean;
    lPage, rPage, nPages, homeOffset, endOffset: Integer;

    mouseDownXY: TPoint;
    mouseImage: TImage;
    mouseAnnot: TAnnotation;
    mouseAnnotXY: TPoint;
    mouseControl: TControl;

    editAnnot: TAnnotation;
    scratchMemo: TMemo;

    lAnnots, rAnnots: TAnnotationSet;

    procedure loadScore( selected: TIndexEntry );
    procedure loadPage( n, max: Integer );
    procedure paintFiller( filler: TImage );
    procedure toggleAnnotationStyle( annot: TAnnotation; style: TFontStyle );
    procedure setAnnotationColor( annot: TAnnotation; code: Integer );
    procedure setAnnotationSize( annot: TAnnotation; code: Integer );
    procedure saveAnnotations( annot: TAnnotation );
    function whichImage( xy: TPoint ): TImage;
  public
    { public declarations }
    procedure AnnotationMouseDown(Sender: TObject; Button: TMouseButton;
			     Shift: TShiftState; X, Y: Integer);
    procedure EditAnnotation( annot: TAnnotation );
    procedure FinishEditing;

    procedure UpdateProgress( msg: String );
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses LCLType, GPIO, Score;

const
  BINDING = 1;
  //FILLER_COLOR = $7F7F7F;
  FILLER_COLOR = $837B65;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin Sender := Sender;
  startup := TRUE;
  nPages := 0;

  mouseDownXY := Point(-1,-1);
  mouseImage := NIL;
  mouseAnnot := NIL;
  mouseAnnotXY := Point(-1,-1);
  mouseControl := NIL;

  editAnnot := NIL;
  EditMemo.Visible := FALSE;

  Color := $E3F6FD;

  scratchMemo := TMemo.Create(MainForm);
  lAnnots := TAnnotationSet.Create;
  rAnnots := TAnnotationSet.Create;

  GPIOScanTimer.Enabled := GPIO.Open
end;

procedure TMainForm.FormResize(Sender: TObject);
var
  page: Integer;
begin Sender := Sender;
  LeftFiller.Top := 0;
  LeftFiller.Left := 0;
  LeftFiller.Height := ClientHeight;
  LeftFiller.Width := 0;

  LeftImage.Top := 0;
  LeftImage.Left := 0;
  LeftImage.Height := ClientHeight;
  LeftImage.Width := ClientWidth div 2;

  MiddleFiller.Top := 0;
  MiddleFiller.Left := LeftImage.Left + LeftImage.Width;
  MiddleFiller.Height := ClientHeight;
  MiddleFiller.Width := 1;

  RightImage.Top := 0;
  RightImage.Left := MiddleFiller.Left + MiddleFiller.Width;
  RightImage.Height := ClientHeight;
  RightImage.Width := ClientWidth - RightImage.Left;

  RightFiller.Top := 0;
  RightFiller.Left := RightImage.Left + RightImage.Width;
  RightFiller.Height := ClientHeight;
  RightFiller.Width := 0;

  ProgressLabel.AutoSize := FALSE;
  ProgressLabel.Left := ClientWidth div 20;
  ProgressLabel.Top := (ClientHeight - ProgressLabel.Height) div 2;
  ProgressLabel.Width := (9 * ClientWidth) div 10;
  ProgressLabel.Height := ProgressLabel.Font.Size * 2;
  ProgressLabel.Color := FILLER_COLOR;
  ProgressLabel.Font.Color := clWhite;

  if not startup then begin
    page := lPage; lPage := 0; rPage := 0;
    loadPage(page,nPages)
  end
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin Sender := Sender;
  if startup then begin
    loadScore(OpenForm.GetMostRecent);
    startup := FALSE
  end
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin Sender := Sender;
  GPIO.Close
end;

procedure TMainForm.GPIOScanTimerTimer(Sender: TObject);
var
  key: Word;
  shift: TShiftState;
begin
  key := Word(GPIO.Scan);
  if key <> 0 then begin
    if (Ord('a') <= key) and (key <= Ord('z')) then
      DEC(key, Ord('a') - Ord('A'));
    if key = Ord('Q') then
      shift := [ssCtrl]
    else
      shift := [];
    FormKeyDown(Sender,key,shift)
  end;
  GPIOScanTimer.Enabled := TRUE
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word;
				Shift: TShiftState);
begin Shift := Shift;
  case Key of
  Ord('L'), VK_BACK, VK_LEFT:
    loadPage(lPage-1,nPages);
  Ord('R'), VK_TAB, VK_SPACE, VK_RIGHT:
    loadPage(lPage+1,nPages);
  Ord('U'), VK_UP, VK_PRIOR:
    loadPage(lPage-2,nPages);
  Ord('D'), VK_DOWN, VK_NEXT:
    loadPage(lPage+2,nPages);
  Ord('H'), VK_HOME:
    loadPage(1 + homeOffset,nPages);
  Ord('E'), VK_END:
    loadPage(nPages - endOffset - 1,nPages);
  Ord('1') .. Ord('9'):
    loadPage(Key - Ord('0') + homeOffset,nPages);
  Ord('0'):
    loadPage(10 + homeOffset,nPages);
  VK_F1 .. VK_F24:
    loadPage(Key - VK_F1 + 1 + homeOffset,nPages);
  Ord('O'):
    OpenMenuItemClick(Sender);
  Ord('Q'):
    if ssCtrl in Shift then
      QuitMenuItemClick(Sender);
  end
end;

procedure TMainForm.FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin Sender := Sender; Shift := Shift; MousePos := MousePos;
  loadPage(lPage+2,nPages); Handled := TRUE
end;

procedure TMainForm.FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin Sender := Sender; Shift := Shift; MousePos := MousePos;
  loadPage(lPage-2,nPages); Handled := TRUE
end;

procedure TMainForm.FormMouseDown(Sender: TObject; Button: TMouseButton;
				  Shift: TShiftState; X, Y: Integer);
begin Button := Button; Shift := Shift;
  if Sender is TControl then begin
    mouseControl := TControl(Sender);
    mouseDownXY := Point(X,Y)
  end
end;

procedure TMainForm.FormMouseUp(Sender: TObject; Button: TMouseButton;
				Shift: TShiftState; X, Y: Integer);
const
  CLICK_THRESHOLD = 5;
  DRAG_THRESHOLD = 100;
var
  xy: TPoint; 
  dx, dy, ax, ay: Integer;
begin Button := Button; Shift := Shift;
  if Sender = mouseControl then begin
    dx := X - mouseDownXY.X; ax := abs(dx);
    dy := Y - mouseDownXY.Y; ay := abs(dy);
    if (ax > DRAG_THRESHOLD) and (ax > 2 * ay) then
      if dx > 0 then
	loadPage(lPage-1,nPages)
      else
	loadPage(lPage+1,nPages)
    else if (ay > DRAG_THRESHOLD) and (ay > 2 * ax) then
      if dy > 0 then begin
	loadPage(lPage-2,nPages)
      end
      else begin
	loadPage(lPage+2,nPages)
      end
    else if (ax < CLICK_THRESHOLD) and (ay < CLICK_THRESHOLD) then begin
      xy := TControl(Sender).ClientToScreen(Point(X,Y));
      if Sender = LeftImage then
	lAnnots.Add(xy.X,xy.Y)
      else if Sender = RightImage then
	rAnnots.Add(xy.X,xy.Y)
    end;
    mouseDownXY := Point(-1,-1)
  end
end;

procedure TMainForm.AnnotationMouseDown(Sender: TObject; Button: TMouseButton;
				        Shift: TShiftState; X, Y: Integer);
var
  xy: TPoint;
begin
  if (Button = mbLeft) and (Shift * [ssShift,ssCtrl,ssAlt] = []) then begin
    xy := Point(x,y);
    xy := TAnnotation(Sender).ClientToScreen(xy);
    xy := MainForm.ScreenToClient(xy);

    mouseImage := whichImage(xy);
    if mouseImage <> NIL then begin
      mouseDownXY := xy;
      mouseAnnot := TAnnotation(Sender);
      mouseAnnotXY := Point(mouseAnnot.Left,mouseAnnot.Top);
      mouseAnnot.BeginDrag(TRUE)
    end
  end
end;

procedure TMainForm.ImageDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  xy: TPoint;
begin State := State;
  xy := Point(x,y);
  xy := TControl(Sender).ClientToScreen(xy);
  xy := MainForm.ScreenToClient(xy);

  Accept := (whichImage(xy) = mouseImage) and (Source = mouseAnnot);
  if Accept then begin
    mouseAnnot.Left := mouseAnnotXY.X + xy.X - mouseDownXY.X;
    if mouseAnnot.Left < mouseImage.Left then
      mouseAnnot.Left := mouseImage.Left
    else if mouseAnnot.Left + mouseAnnot.Width > mouseImage.Left + mouseImage.Width then
      mouseAnnot.Left := mouseImage.Left + mouseImage.Width - mouseAnnot.Width;
    mouseAnnot.Top := mouseAnnotXY.Y + xy.Y - mouseDownXY.Y;
    if mouseAnnot.Top < mouseImage.Top then
      mouseAnnot.Top := mouseImage.Top
    else if mouseAnnot.Top + mouseAnnot.Height > mouseImage.Top + mouseImage.Height - mouseAnnot.Height then
      mouseAnnot.Top := mouseImage.Top + mouseImage.Height - mouseAnnot.Height;
  end
  else begin
    mouseAnnot.Left := mouseAnnotXY.X;
    mouseAnnot.Top := mouseAnnotXY.Y
  end
end;

procedure TMainForm.ImageDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  xy: TPoint;
begin
  xy := Point(x,y);
  xy := TControl(Sender).ClientToScreen(xy);
  xy := MainForm.ScreenToClient(xy);

  if (xy.X = mouseDownXY.X) and (xy.Y = mouseDownXY.Y) then
    EditAnnotation(mouseAnnot)
  else if (whichImage(xy) = mouseImage) and (Source = mouseAnnot) then begin
    mouseAnnot.Left := mouseAnnotXY.X + xy.X - mouseDownXY.X;
    if mouseAnnot.Left < mouseImage.Left then
      mouseAnnot.Left := mouseImage.Left
    else if mouseAnnot.Left + mouseAnnot.Width > mouseImage.Left + mouseImage.Width then
      mouseAnnot.Left := mouseImage.Left + mouseImage.Width - mouseAnnot.Width;
    mouseAnnot.Top := mouseAnnotXY.Y + xy.Y - mouseDownXY.Y;
    if mouseAnnot.Top < mouseImage.Top then
      mouseAnnot.Top := mouseImage.Top
    else if mouseAnnot.Top + mouseAnnot.Height > mouseImage.Top + mouseImage.Height - mouseAnnot.Height then
      mouseAnnot.Top := mouseImage.Top + mouseImage.Height - mouseAnnot.Height;
    if mouseImage = LeftImage then
      lAnnots.Save(scratchMemo)
    else
      rAnnots.Save(scratchMemo)
  end
end;

procedure TMainForm.EditMemoKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin Sender := Sender;
  if ((key = VK_TAB) or (key = VK_ESCAPE))
  and (Shift * [ssShift,ssCtrl,ssAlt] = []) then
    FinishEditing
end;

procedure TMainForm.EditAnnotation( annot: TAnnotation );
begin
  FinishEditing;
  EditMemo.Top := annot.Top - 1;
  EditMemo.Left := annot.Left - 1;

  EditMemo.Width := annot.Width + 4;
  if EditMemo.Width < 240 then EditMemo.Width := 240;
  EditMemo.Height := annot.Height + 4;
  if EditMemo.Height < 40 then EditMemo.Height := 40;

  if EditMemo.Left < 0 then
    EditMemo.Left := 0
  else if EditMemo.Left + EditMemo.Width > ClientWidth then
    EditMemo.Left := ClientWidth - EditMemo.Width;
  if EditMemo.Top < 0 then
    EditMemo.Top := 0
  else if EditMemo.Top + EditMemo.Height > ClientHeight then
    EditMemo.Top := ClientHeight - EditMemo.Height;

  EditMemo.Font.Assign(annot.Font);
  if annot.ParentColor then
    EditMemo.Color := clDefault
  else
    EditMemo.Color := annot.Color;
  EditMemo.Text := annot.Caption;
  EditMemo.Visible := TRUE;

  editAnnot := annot;

  FocusControl(editMemo)
end;

procedure TMainForm.FinishEditing;
var
  img: TImage;
  s: String;
begin
  if EditMemo.Visible and (editAnnot <> NIL) then begin
    img := whichImage(Point(editAnnot.Left,editAnnot.Top));
    s := TrimRight(EditMemo.Text);
    if s = '' then
      if img = LeftImage then
        lAnnots.Remove(editAnnot)
      else
        rAnnots.Remove(editAnnot)
    else begin
      editAnnot.Caption := s;
      if img <> NIL then begin
	if editAnnot.Left + editAnnot.Width > img.Left + img.Width then
	  editAnnot.Left := img.Left + img.Width - editAnnot.Width;
	if editAnnot.Top + editAnnot.Height > img.Top + img.Height - editAnnot.Height then
	  editAnnot.Top := img.Top + img.Height - editAnnot.Height;
	if img = LeftImage then
	  lAnnots.Save(scratchMemo)
	else
	  rAnnots.Save(scratchMemo)
      end
    end;
    editAnnot := NIL;
    EditMemo.Visible := FALSE
  end
end;

procedure TMainForm.AnnotationCommandMenuPopup(Sender: TObject);
var
  annot: TAnnotation;
  code: Integer;
begin Sender := Sender;
  if AnnotationCommandMenu.PopupComponent is TAnnotation then begin
    annot := TAnnotation(AnnotationCommandMenu.PopupComponent);
    BoldMenuItem.Checked := fsBold in annot.Font.Style;
    ItalicMenuItem.Checked := fsItalic in annot.Font.Style;
    UnderlineMenuItem.Checked := fsUnderline in annot.Font.Style;
    StrikeoutMenuItem.Checked := fsStrikeout in annot.Font.Style;

    code := annot.ColorCode;
    BlackMenuItem.Checked := code = ANNOTATION_BLACK;
    RedMenuItem.Checked := code = ANNOTATION_RED;
    GreenMenuItem.Checked := code = ANNOTATION_GREEN;
    BlueMenuItem.Checked := code = ANNOTATION_BLUE;
    GrayMenuItem.Checked := code = ANNOTATION_GRAY;
    HighlightMenuItem.Checked := code = ANNOTATION_HIGHLIGHT;

    code := annot.SizeCode;
    SmallMenuItem.Checked := code = ANNOTATION_SMALL;
    MediumMenuItem.Checked := code = ANNOTATION_MEDIUM;
    LargeMenuItem.Checked := code = ANNOTATION_LARGE;
    SubtitleMenuItem.Checked := code = ANNOTATION_SUBTITLE;
    TitleMenuItem.Checked := code = ANNOTATION_TITLE
  end;
end;

procedure TMainForm.BoldMenuItemClick(Sender: TObject);
begin Sender := Sender;
  if AnnotationCommandMenu.PopupComponent is TAnnotation then
    toggleAnnotationStyle(TAnnotation(AnnotationCommandMenu.PopupComponent),
                          fsBold);
end;

procedure TMainForm.ItalicMenuItemClick(Sender: TObject);
begin Sender := Sender;
  if AnnotationCommandMenu.PopupComponent is TAnnotation then
    toggleAnnotationStyle(TAnnotation(AnnotationCommandMenu.PopupComponent),
                          fsItalic);
end;

procedure TMainForm.UnderlineMenuItemClick(Sender: TObject);
begin Sender := Sender;
  if AnnotationCommandMenu.PopupComponent is TAnnotation then
    toggleAnnotationStyle(TAnnotation(AnnotationCommandMenu.PopupComponent),
                          fsUnderline);
end;

procedure TMainForm.StrikeoutMenuItemClick(Sender: TObject);
begin Sender := Sender;
  if AnnotationCommandMenu.PopupComponent is TAnnotation then
    toggleAnnotationStyle(TAnnotation(AnnotationCommandMenu.PopupComponent),
                          fsStrikeout);
end;

procedure TMainForm.BlackMenuItemClick(Sender: TObject);
begin Sender := Sender;
  if AnnotationCommandMenu.PopupComponent is TAnnotation then
    setAnnotationColor(TAnnotation(AnnotationCommandMenu.PopupComponent),
                       ANNOTATION_BLACK);
end;

procedure TMainForm.RedMenuItemClick(Sender: TObject);
begin Sender := Sender;
  if AnnotationCommandMenu.PopupComponent is TAnnotation then
    setAnnotationColor(TAnnotation(AnnotationCommandMenu.PopupComponent),
                       ANNOTATION_RED);
end;

procedure TMainForm.GreenMenuItemClick(Sender: TObject);
begin Sender := Sender;
  if AnnotationCommandMenu.PopupComponent is TAnnotation then
    setAnnotationColor(TAnnotation(AnnotationCommandMenu.PopupComponent),
                       ANNOTATION_GREEN);
end;

procedure TMainForm.BlueMenuItemClick(Sender: TObject);
begin Sender := Sender;
  if AnnotationCommandMenu.PopupComponent is TAnnotation then
    setAnnotationColor(TAnnotation(AnnotationCommandMenu.PopupComponent),
                       ANNOTATION_BLUE);
end;

procedure TMainForm.GrayMenuItemClick(Sender: TObject);
begin Sender := Sender;
  if AnnotationCommandMenu.PopupComponent is TAnnotation then
    setAnnotationColor(TAnnotation(AnnotationCommandMenu.PopupComponent),
                       ANNOTATION_GRAY);
end;

procedure TMainForm.HighlightMenuItemClick(Sender: TObject);
begin Sender := Sender;
  if AnnotationCommandMenu.PopupComponent is TAnnotation then
    setAnnotationColor(TAnnotation(AnnotationCommandMenu.PopupComponent),
                       ANNOTATION_HIGHLIGHT);
end;

procedure TMainForm.SmallMenuItemClick(Sender: TObject);
begin Sender := Sender;
  if AnnotationCommandMenu.PopupComponent is TAnnotation then
    setAnnotationSize(TAnnotation(AnnotationCommandMenu.PopupComponent),
                      ANNOTATION_SMALL);
end;

procedure TMainForm.MediumMenuItemClick(Sender: TObject);
begin Sender := Sender;
  if AnnotationCommandMenu.PopupComponent is TAnnotation then
    setAnnotationSize(TAnnotation(AnnotationCommandMenu.PopupComponent),
                      ANNOTATION_MEDIUM);
end;

procedure TMainForm.LargeMenuItemClick(Sender: TObject);
begin Sender := Sender;
  if AnnotationCommandMenu.PopupComponent is TAnnotation then
    setAnnotationSize(TAnnotation(AnnotationCommandMenu.PopupComponent),
                      ANNOTATION_LARGE);
end;

procedure TMainForm.SubtitleMenuItemClick(Sender: TObject);
begin Sender := Sender;
  if AnnotationCommandMenu.PopupComponent is TAnnotation then
    setAnnotationSize(TAnnotation(AnnotationCommandMenu.PopupComponent),
                      ANNOTATION_SUBTITLE);
end;

procedure TMainForm.TitleMenuItemClick(Sender: TObject);
begin Sender := Sender;
  if AnnotationCommandMenu.PopupComponent is TAnnotation then
    setAnnotationSize(TAnnotation(AnnotationCommandMenu.PopupComponent),
                      ANNOTATION_TITLE);
end;

procedure TMainForm.RemoveMenuItemClick(Sender: TObject);
var
  img: TImage;
  annot: TAnnotation;
begin Sender := Sender;
  if AnnotationCommandMenu.PopupComponent is TAnnotation then begin
    annot := TAnnotation(AnnotationCommandMenu.PopupComponent);
    img := whichImage(Point(annot.Left,annot.Top));
    if img = LeftImage then begin
      lAnnots.Remove(annot);
      lAnnots.Save(scratchMemo)
    end
    else begin
      rAnnots.Remove(annot);
      rAnnots.Save(scratchMemo)
    end
  end
end;

procedure TMainForm.OpenMenuItemClick(Sender: TObject);
begin Sender := Sender;
  if OpenForm.ShowModal = mrOk then begin
    FinishEditing;
    loadScore(OpenForm.GetSelectedEntry)
  end
end;

procedure TMainForm.QuitMenuItemClick(Sender: TObject);
begin Sender := Sender;
  FinishEditing;
  Close
end;

procedure TMainForm.UpdateProgress( msg: String );
begin
  ProgressLabel.Caption := msg;
  if not ProgressLabel.Visible and (msg <> '') then begin
    LeftFiller.Visible := TRUE;
    LeftFiller.Left := 0;
    LeftFiller.Width := ClientWidth;
    paintFiller(LeftFiller);
    MiddleFiller.Visible := FALSE;
    RightFiller.Visible := FALSE
  end;
  ProgressLabel.Visible := msg <> '';
  Application.ProcessMessages
end;

procedure TMainForm.loadScore( selected: TIndexEntry );
var
  err: String;
begin
  if selected <> NIL then begin
    Screen.Cursor := crHourGlass;
    LeftImage.Visible := FALSE; RightImage.Visible := FALSE;
    lAnnots.Clear; rAnnots.Clear;
    Application.ProcessMessages;

    nPages := Score.Prepare(selected,ClientWidth div 2,ClientHeight,
    			    homeOffset,endOffset,err);
    UpdateProgress('');

    if nPages > 0 then begin
      lPage := 0; rPage := 0;
      loadPage(1 + homeOffset,nPages);
      OpenForm.UpdateMostRecent(selected)
    end
    else
      MessageDlg(err,mtError,[mbOk],0);

    Screen.Cursor := crDefault
  end
end;

procedure TMainForm.loadPage( n, max: Integer );
var
  lFName, rFName: String;
  wl, wr: Integer;
begin
  if (n < 1) or (n = 1) then
    n := 1
  else if (n >= max) and (max >= 2) then
    n := max - 1;

  if (1 <= n) and (n < lPage)
  or (lPage < n) and (n <= max) and (rPage < max) then begin
    Screen.Cursor := crHourGlass;
    Application.ProcessMessages;

    LeftFiller.Visible := FALSE; LeftImage.Visible := FALSE;
    MiddleFiller.Visible := FALSE;
    RightImage.Visible := FALSE; RightFiller.Visible := FALSE;
    lFName := Score.GetPagePath(n);
    rFName := Score.GetPagePath(n+1);
    if lFName <> '' then begin
      if n = rPage then begin
	(* The image we want for the left is already on the right. *)
	LeftImage.Picture.Assign(RightImage.Picture);
	LeftImage.Visible := TRUE;
	lPage := n
      end
      else if (n + 1 = lPage) and (rPage > lPage) then begin
	(* The image we want for the right is already on the left. *)
	RightImage.Picture.Assign(LeftImage.Picture);
	RightImage.Visible := TRUE;
	rPage := n + 1
      end;
      if not LeftImage.Visible then begin
	(* Load left image from file. *)
	LeftImage.Picture.LoadFromFile(lFName);
	LeftImage.Visible := TRUE;
	lPage := n
      end;
      if n + 1 > max then
	(* There is no page to display on the right side. *)
        rPage := 0
      else if not RightImage.Visible then begin
	RightImage.Picture.LoadFromFile(rFName);
	RightImage.Visible := TRUE;
	rPage := n + 1
      end;
      (* Compute left-right alignment of page images. *)
      if LeftImage.Visible then begin
        if RightImage.Visible then begin
	  (* Apportion space between the left and right pages based on the
	     relative sizes of the two images. *)
	  LeftImage.Width := LeftImage.Picture.Width;
	  RightImage.Width := RightImage.Picture.Width;
          LeftImage.Left := (ClientWidth - LeftImage.Width - RightImage.Width - BINDING) div 2;
          RightImage.Left := LeftImage.Left + LeftImage.Width + BINDING;
	  (* Size fillers appropriately. *)
	  wl := LeftImage.Left;
	  if wl > 0 then begin
	    LeftFiller.Width := wl;
            paintFiller(LeftFiller);
	    LeftFiller.Visible := TRUE
	  end;
	  wl := LeftImage.Left + LeftImage.Width;
	  wr := RightImage.Left;
	  if wr > wl then begin
	    MiddleFiller.Left := wl;
	    MiddleFiller.Width := wr - wl;
            paintFiller(MiddleFiller);
	    MiddleFiller.Visible := TRUE
	  end;
	  wr := ClientWidth - (RightImage.Left + RightImage.Width);
	  if wr > 0 then begin
	    RightFiller.Left := ClientWidth - wr;
	    RightFiller.Width := wr;
            paintFiller(RightFiller);
	    RightFiller.Visible := TRUE
	  end;
	  rAnnots.Load(rFName,ClientHeight,RightImage.Left)
	end
	else begin
	  (* Center left page on screen if there is no right page. *)
	  LeftImage.Width := LeftImage.Picture.Width;
	  LeftImage.Left := (ClientWidth - LeftImage.Width) div 2;
	  wl := LeftImage.Left;
	  if wl > 0 then begin
	    LeftFiller.Width := wl;
            paintFiller(LeftFiller);
	    LeftFiller.Visible := TRUE
	  end;
	  wr := ClientWidth - (LeftImage.Left + LeftImage.Width);
	  if wr > 0 then begin
	    RightFiller.Left := ClientWidth - wr;
	    RightFiller.Width := wr;
            paintFiller(RightFiller);
	    RightFiller.Visible := TRUE
	  end;
	  rAnnots.Clear
	end;
	lAnnots.Load(lFName,ClientHeight,LeftImage.Left)
      end
    end;

    Screen.Cursor := crDefault
  end
end;

procedure TMainForm.paintFiller( filler: TImage );
begin
  filler.Picture.Bitmap.SetSize(filler.Width,filler.Height);
  filler.Picture.Bitmap.Canvas.Brush.Color := FILLER_COLOR;
  filler.Picture.Bitmap.Canvas.FillRect(0,0,filler.Width,filler.Height);
  (* Draw a dark line along the left edge of the filler. *)
  if filler <> LeftFiller then begin
    if filler = MiddleFiller then
      filler.Picture.Bitmap.Canvas.Brush.Color := FILLER_COLOR
    else
      filler.Picture.Bitmap.Canvas.Brush.Color := (FILLER_COLOR and $FEFEFE) shr 1;
    filler.Picture.Bitmap.Canvas.FillRect(0,0,1,filler.Height)
  end
end;

// TODO - Have these accept TObject and do the check here, instead of each call.
procedure TMainForm.toggleAnnotationStyle( annot: TAnnotation; style: TFontStyle );
begin
  if style in annot.Font.Style then
    annot.Font.Style := annot.Font.Style - [style]
  else
    annot.Font.Style := annot.Font.Style + [style];
  saveAnnotations(annot)
end;

procedure TMainForm.setAnnotationColor( annot: TAnnotation; code: Integer );
begin
  annot.SetColorCode(code);
  saveAnnotations(annot)
end;

procedure TMainForm.setAnnotationSize( annot: TAnnotation; code: Integer );
begin
  annot.SetSizeCode(code);
  saveAnnotations(annot)
end;

procedure TMainForm.saveAnnotations( annot: TAnnotation );
begin
  if whichImage(Point(annot.Left,annot.Top)) = LeftImage then
    lAnnots.Save(scratchMemo)
  else
    rAnnots.Save(scratchMemo)
end;

function TMainForm.whichImage( xy: TPoint ): TImage;
begin
  if LeftImage.Visible and (LeftImage.Left <= xy.X)
  and (xy.X < LeftImage.Left + LeftImage.Width) then
    whichImage := LeftImage
  else if RightImage.Visible and (RightImage.Left <= xy.X)
  and (xy.X < RightImage.Left + RightImage.Width) then
    whichImage := RightImage
  else
    whichImage := NIL;
end;

end.
