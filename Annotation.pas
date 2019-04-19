unit Annotation; // vim:sw=2:

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

(* Classes for annotations and annotation sets. The latter is a container for
   all the annotations on a particular page of a score. Typically two such sets
   exist for the two visible pages.
   
   After an annotation set is created, it must be initialized by calling the
   Load method, which specifies the image file of the score page it is for, and
   partial geometry of the image the page is rendered on. An annotation set is
   reusable by just calling Load again for a new score page; it is not
   necessary to re-Create it first. *)

interface

uses Classes, Contnrs, Controls, StdCtrls;

const
  ANNOTATION_BLACK = 0;
  ANNOTATION_RED = 1;
  ANNOTATION_GREEN = 2;
  ANNOTATION_BLUE = 3;
  ANNOTATION_GRAY = 4;
  ANNOTATION_HIGHLIGHT = 5;

  ANNOTATION_SMALL = -1;
  ANNOTATION_MEDIUM = 0;
  ANNOTATION_LARGE = 1;
  ANNOTATION_SUBTITLE = 2;
  ANNOTATION_TITLE = 3;

type
  TAnnotation = class(TLabel)
    procedure SetColorCode( code: Integer );
    function ColorCode: Integer;
    procedure SetSizeCode( code: Integer );
    function SizeCode: Integer;
  end;

  TAnnotationSet = class(TObject)
  public
    constructor Create;
    procedure Clear;
    procedure Load( scorefile: String; scale, left: Integer );
    procedure Add( x, y: Integer );
    procedure Remove( annot: TAnnotation );
    procedure Save( scratch: TMemo );
  private
    annots: TObjectList;
    (* These are all set when Load is called (even if the file doesn't exist
       yet). Annotations cannot be added or saved until then. *)
    filename: String;	(* Backing store for the annotation set. *)
    scale: Integer;	(* Window height the annotations are rendered in. *)	
    left: Integer;	(* Window x-offset the annotations are rendered in. *)
    function createAnnotation( x, y: Integer ): TAnnotation;
  end;

implementation

uses Graphics, Math, SysUtils, Main, Utility;

const
  ANNOTATION_HIGH_FG = ANNOTATION_HIGHLIGHT;
  ANNOTATION_HIGH_BG = ANNOTATION_HIGH_FG + 1;

  COLOR_MAP: array [ANNOTATION_BLACK..ANNOTATION_HIGH_BG] of Integer = (
    $333333,	// Black
    $3333CC,	// Red
    $009922,	// Green
    $CC6633,	// Blue
    $888888,	// Grey
    $010101,	// Highlight foreground (almost black)
    $00FFFF );	// Hightlight background (yellow)

  SMALLER_SIZES = -ANNOTATION_SMALL;
  LARGER_SIZES = ANNOTATION_TITLE;
  NUM_SIZES = SMALLER_SIZES + 1 + LARGER_SIZES;

var
  sizeMap: array [0..NUM_SIZES-1] of Integer;

constructor TAnnotationSet.Create;
begin
  annots := TObjectList.Create;
  annots.OwnsObjects := TRUE;
  Clear
end;

procedure TAnnotationSet.Clear;
begin
  annots.Clear;
  filename := ''; scale := 0; left := 0
end;

function TAnnotationSet.createAnnotation( x, y: Integer ): TAnnotation;
var
  i: Integer;
  annot: TAnnotation;
begin
  inherited Create;
  if sizeMap[0] < 0 then
    for i := -SMALLER_SIZES to LARGER_SIZES do
      sizeMap[i + SMALLER_SIZES] := Round(Power(2,i/3) * scale / 90);
  annot := TAnnotation.Create(MainForm);
  annot.Parent := MainForm;
  annot.Left := x;
  annot.Top := y;
  annot.ParentColor := TRUE;
  annot.Font.Color := COLOR_MAP[ANNOTATION_BLUE];
  annot.Font.Height := sizeMap[SMALLER_SIZES];
  annot.AutoSize := TRUE;
  annot.Caption := '';
  annot.PopupMenu := MainForm.AnnotationCommandMenu;
  annot.OnMouseDown := @MainForm.AnnotationMouseDown;
  annot.OnDragOver := @MainForm.ImageDragOver;
  annot.OnDragDrop := @MainForm.ImageDragDrop;
  createAnnotation := annot
end;

procedure TAnnotationSet.Load( scorefile: String; scale, left: Integer );
var
  x, y: Double;
  dir, name, s: String;
  ix, iy, style: Integer;
  f: TextFile;
  annot: TAnnotation;
begin
  Clear;
  Self.scale := scale;
  Self.left := left;
  dir := CombinePath(ExtractFileDir(ExtractFileDir(scorefile)),'.annotations');
  if not DirectoryExists(dir) then CreateDir(dir);
  name := ExtractFileName(scorefile);
  if (Length(name) > 4) and (RightStr(name,4) = '.png') then begin
    name := LeftStr(name,Length(name)-4) + '.txt';
    filename := CombinePath(dir,name);
    AssignFile(f,filename);
    {$I-}
    Reset(f);
    {$I+}
    if IOResult = 0 then begin
      annot := NIL;
      while not EOF(f) do begin
	(* See the comment in the Save function for a description of the
	   annotation file format. *)
	ReadLn(f,style,x,y,s);
	if (Length(s) > 1) and (s[1] = ' ') then
	  s := RightStr(s,Length(s)-1);
	if style >= 0 then begin
	  ix := Round(x * scale);
	  iy := Round(y * scale);
	  annot := createAnnotation(ix+left,iy);
	  annot.Caption := s;
	  annot.Font.Style := TFontStyles(Int32(style mod 100));
	  annot.SetColorCode((style div 1000) mod 100);
	  annot.SetSizeCode(style div 1000000 - 10);
	  annots.Add(annot)
	end
	else if annot <> NIL then
	  annot.Caption := annot.Caption + LineEnding + s
      end;
      Close(f)
    end
  end
end;
  
procedure TAnnotationSet.Add( x, y: Integer );
var
  annot: TAnnotation;
begin
  if scale > 0 then begin
    annot := createAnnotation(x,y);
    annots.Add(annot);
    MainForm.EditAnnotation(annot)
  end
end;

procedure TAnnotationSet.Remove( annot: TAnnotation );
begin
  annots.Remove(annot)
end;

procedure TAnnotationSet.Save( scratch: TMemo );
var
  i, j, style: Integer;
  x, y: Double;
  f: TextFile;
  annot: TAnnotation;
begin
  if filename <> '' then begin
    if (annots.Count > 0) and (scale > 0) then begin
      AssignFile(f,filename);
      {$I-}
      Rewrite(f);
      {$I+}
      if IOResult = 0 then begin
	(* Write each label in the form:
	
	     style x y First line of text
	     -1 x y Second line of text
	     ...
	     -1 x y Last line of text

	   where style is the size code times 1,000,000 plus the color code
	   times 1,000 plus the font style set cast to an integer, x is
	   relative to the edge of the page, not the main form, and both x and
	   y are expressed as a fraction of the form height. *)
	for i := 0 to annots.Count-1 do begin
	  annot := TAnnotation(annots[i]);
	  style := Integer(annot.Font.Style) + (annot.ColorCode * 1000)
			   + ((annot.SizeCode + 10) * 1000000);
	  x := (annot.Left - left) / scale;
	  y := annot.Top / scale;
	  scratch.Text := annot.Caption;
	  for j := 0 to scratch.Lines.Count-1 do begin
	    WriteLn(f,style,' ',x:8:6,' ',y:8:6,' ',scratch.Lines[j]);
	    style := -1
	  end
	end;
	Close(f)
      end
    end
    else if FileExists(filename) then
      DeleteFile(filename)
  end
end;

procedure TAnnotation.SetColorCode( code: Integer );
begin
  if (0 <= code) and (code <= ANNOTATION_HIGHLIGHT) then begin
    Font.Color := COLOR_MAP[code];
    ParentColor := code < ANNOTATION_HIGHLIGHT;
    if not ParentColor then
      Color := COLOR_MAP[ANNOTATION_HIGH_BG]
  end
end;

function TAnnotation.ColorCode: Integer;
var
  code: Integer;
begin
  ColorCode := ANNOTATION_BLACK;
  for code := ANNOTATION_BLACK to ANNOTATION_HIGH_FG do
    if COLOR_MAP[code] = Font.Color then begin
      ColorCode := code;
      break
    end
end;

procedure TAnnotation.SetSizeCode( code: Integer );
begin
  if (-SMALLER_SIZES <= code) and (code <= LARGER_SIZES) then
    Font.Height := sizeMap[code+SMALLER_SIZES]
end;

function TAnnotation.SizeCode: Integer;
var
  code: Integer;
begin
  sizeCode := 0;
  for code := -SMALLER_SIZES to LARGER_SIZES do
    if sizeMap[code+SMALLER_SIZES] = Font.Height then begin
      SizeCode := code;
      break
    end
end;

begin
  sizeMap[0] := -1
end.
