unit Score; // vim:sw=2:

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

uses Index;

(* Convert a PDF document, or a series of BMP, GIF, JPEG, PNG, or TIFF image,
   into multiple ready-to-display PNG files, one per page. Returns the number
   of page files produced, or zero on failure, in which case an error message
   will be returned via the err parameter. *)
function Prepare( selected: TIndexEntry; w, h: Integer;
		  out head, tail: Integer; out err: String ): Integer;

(* Get the full path to the n-th page of the most recently opened score. *)
function GetPagePath( n: Integer ): String;

implementation

uses Classes, Controls, Dialogs, FileUtil, Forms, Main, Process, StrUtils,
  SysUtils, Utility;

const
  NUM_WIDTH = 3;	(* Width of page numbers in generated PNG files. *)

var
  gsNumFmt: String;	(* Number format for GhostScript output template. *)

  (* These variables record the state of the currently loaded score. Score is
     effectively a singleton, so there's no point even making it an object. *)
  sDir: String;		(* Full path of directory containing current score. *)
  sName: String;	(* Name of current score file without -digits suffix. *)
  sExt: String;		(* Extension of current score file, without the dot. *)
  sGeom: String;	(* Width and height as a string of the form WxH. *)
  sTmpgeom: String;	(* Geometry of intermediate files for PDF to PNG. *)
  sCache: String;	(* Full path of cache for display files. *)

function nthPage( tag, num: String ): String;
begin
  nthPage := CombinePath(sCache, sName + '-' + tag + '-' + num + '.png')
end;

function nthPage( tag: String; num: Integer ): String;
begin
  nthPage := nthPage(tag,AddChar('0',IntToStr(num),NUM_WIDTH))
end;

function resizeToPNG( src, dst: String; out err: String ): Boolean;
var
  exe, outstr: String;
  argv: array of String;
begin
  if FileExists(dst) and (FileTime(dst) >= FileTime(src)) then
    resizeToPNG := TRUE

  else begin
    resizeToPNG := FALSE;

    MainForm.UpdateProgress('Preparing ' + dst + ' ...');

    exe := CombinePath(ExtractFileDir(Application.ExeName),'muprep');
    argv := argv.Create('-v',src,sGeom,dst);
    outstr := '';
    RunCommand(exe,argv,outstr);

    if FileExists(dst) then
      resizeToPNG := TRUE
    else
      err := 'Failed to create ' + dst + ':' + LineEnding + outStr
  end
end;

(* Numerically compare the -digits suffixes of two strings. *)
function compareSuffixes( list: TStringList; a, b: Integer ): Integer;
var
  sa, sb: String;
  la, lb, ia, ib: Integer;
begin
  compareSuffixes := 0;
  sa := list[a]; la := Length(sa); ia := RPos('-',sa);
  sb := list[b]; lb := Length(sb); ib := RPos('-',sb);
  if (ia = 0) or (ia = la) or not (sa[ia+1] in ['0'..'9'])
  or (ib = 0) or (ib = lb) or not (sb[ib+1] in ['0'..'9']) then
    if sa < sb then compareSuffixes := -1 else compareSuffixes := 1
  else begin
    ia := StrToIntDef(Copy(sa,ia+1,la-ia),-1);
    ib := StrToIntDef(Copy(sb,ib+1,lb-ib),-1);
    if (ia < 0) or (ib < 0) then
      if sa < sb then compareSuffixes := -1 else compareSuffixes := 1
    else
      if ia < ib then compareSuffixes := -1 else compareSuffixes := 1
  end
end;

function prepareImages( n: Integer; out err: String ): Integer;
var
  path, fname, suffix: String;
  i, j, len: Integer;
  found: TSearchRec;
  files: TStringList;
begin
  prepareImages := 0;
  
  path := CombinePath(sDir, sName + '*.' + sExt);
  if FindFirst(path,faReadOnly,found) = 0 then begin
    (* Make a list of the input images comprising this score. *)
    files := TStringList.Create;
    i := 0;
    repeat
      fname := found.Name;
      suffix := Copy(fname,Length(sName)+1,Length(fname)-Length(sName)-1-Length(sExt));

      (* If there is a single file, ignore files with any suffix. *)
      if (n = 1) and (suffix <> '') then continue;

      (* If there are multiple files, ignore files without a -digits suffix. *)
      if n > 1 then begin
	len := Length(suffix);
        if (len < 2) or (suffix[1] <> '-') then continue;
	j := 2;
	while (j <= len) and (suffix[j] in ['0'..'9']) do
	  INC(j);
	if j <= len then continue
      end;

      files.Add(CombinePath(sDir,fname));

      INC(i)
    until (i = n) or (FindNext(found) <> 0);
    FindClose(found);

    (* Sort list by file name so we process them in the right order. *)
    if i > 1 then files.CustomSort(@compareSuffixes);

    (* Convert each file into display format. *)
    i := 0;
    for fname in files do begin
      INC(i);
      if not resizeToPNG(fname,nthPage(sExt,i),err) then
	break
    end;

    if err = '' then
      prepareImages := i
  end
end;

function splitPDF( out err: String ): Integer;
var
  pdf, png, tmp, outstr, exe: String;
  i: Integer;
  argv: array of String;
begin
  splitPDF := 0;

  pdf := CombinePath(sDir, sName + '.pdf');
  tmp := nthPage('tmp',1);
  png := nthPage('pdf',1);

  (* Use Ghostscript to split PDF file into one temporary greyscale PNG file
     per page, at about 4 times the desired final resolution. *)
  if not FileExists(png) or (FileTime(pdf) > FileTime(png)) then begin
    MainForm.UpdateProgress('Splitting ' + pdf + ' into PNG files ...');
    exe := FindDefaultExecutablePath('gs');
    argv := argv.Create('-sDEVICE=pnggray', '-o', nthPage('tmp',gsNumFmt),
    			'-g' + sTmpgeom, '-dPDFFitPage',
			CombinePath(sDir, sName + '.pdf'));
    outstr := '';
    RunCommand(exe,argv,outstr);
    if not FileExists(tmp) then begin
      err := 'Failed to split ' + pdf + ' into PNG files:' + LineEnding + outstr;
      MainForm.UpdateProgress('');
      exit
    end;

    (* Resize and trim temporary PNG files from GhostScript into final ones. *)
    i := 1;
    while FileExists(tmp) and (FileTime(tmp) > FileTime(pdf)) do begin
      if not FileExists(png) or (FileTime(tmp) > FileTime(png)) then begin
	if not resizeToPNG(tmp,png,err) then begin
	  while FileExists(tmp) do begin
	    DeleteFile(tmp);
	    INC(i);
	    tmp := nthPage('tmp',i)
	  end;
	  MainForm.UpdateProgress('');
	  exit
	end;
	DeleteFile(tmp)
      end;
      INC(i);
      png := nthPage('pdf',i);
      tmp := nthPage('tmp',i)
    end
  end;

  (* Count how many PNG files were generated, or already existed if they were
     generated previously. *)
  i := 0;
  repeat
    INC(i);
    png := nthPage('pdf',i)
  until not FileExists(png);
  splitPDF := i - 1;

  MainForm.UpdateProgress('')
end;

function Prepare( selected: TIndexEntry; w, h: Integer;
		  out head, tail: Integer; out err: String ): Integer;
var
  n: Integer;
begin
  sDir := selected.Dir;
  sName := LeftStr(selected.Name,selected.Dash-1);
  sExt := selected.Ext;

  sGeom := IntToStr(w) + 'x' + IntToStr(h);
  sTmpgeom := IntToStr(w*4) + 'x' + IntToStr(h*4);
  sCache := CombinePath(sDir, '.' + sGeom);

  if not DirectoryExists(sCache) then CreateDir(sCache);

  if sExt = 'pdf' then
    n := splitPDF(err)
  else
    n := prepareImages(selected.Files,err);

  head := selected.Head;
  tail := selected.Tail;
  Prepare := n
end;

function GetPagePath( n: Integer ): String;
begin
  GetPagePath := nthPage(sExt,n)
end;

begin
  sDir := ''; sName := ''; sExt := '';
  sGeom := ''; sTmpgeom := ''; sCache := '';
  gsNumFmt := '%0' + IntToStr(NUM_WIDTH) + 'd'
end.
