unit Index; // vim:sw=2:

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

(* This unit implements the primary and most-recently-used (MRU) indexes of
   scores that MusicRack knows about. These are constructed shortly after
   application startup time by calling Build, which traverses ~/SheetMusic or
   the directories specified on the command line. All access to the indexes
   should be done through OpenForm, as it "owns" the index. *)

interface

uses
  Classes, SysUtils;

type
  TIndexEntry = class(TObject)
  public
    Dir: String;	(* Full path of the directory containing this score. *)
    Name: String;	(* Name of the score file, without the extension. *)
    Ext: String;	(* Extension of the score file, without leading dot. *)
    Dash: Integer;	(* Location of '-' separating file number in Name, or
    			   one more than Length(Name) if there isn't one. *)
    Files: Integer;	(* Number of files comprising the original score. *)
    Head: Integer;	(* Number of pages to ignore at each end of score. *)
    Tail: Integer;
  end;

(* Build and return the index. *)
function Build: TStringList;

function GetMRU: TStringList;

procedure UpdateMRU( entry: TIndexEntry );

implementation

uses Dialogs, StrUtils, Utility;

const
  MAX_MRU = 12;

var
  scores, mruList: TStringList;
  home, mruFile: String;
  common: String;	(* Prefix common to all specified directories. *)

(* Recursively traverse a directory, recording all PDF, BMP, JPEG, PNG, and
   TIFF files found. Don't descend into sub-directories whose names begin with
   a dot, as converted files and annotations are stored in such directories. *)
procedure traverse( dir: String );
var
  path, name, ext: String;
  found: TSearchRec;
  entry: TIndexEntry;
begin
  path := CombinePath(dir,'*');
  if FindFirst(path,faReadOnly + faDirectory,found) = 0 then begin
    entry := NIL;
    repeat
      if (Found.attr and faDirectory) = 0 then begin
	name := found.Name;
	ext := ExtractFileExt(name);
	name := LeftStr(name,Length(name)-Length(ext));
	if (Length(ext) > 1) and (ext[1] = '.') then
	  ext := RightStr(ext,Length(ext)-1);

	if (ext = 'bmp') or (ext = 'jpg') or (ext = 'jpeg') or (ext = 'png')
	or (ext = 'tif') or (ext = 'tiff') or (ext = 'pdf') then begin
	  entry := TIndexEntry.Create;
	  entry.Dir := dir;
	  entry.Name := name;
	  entry.Ext := ext;
	  entry.Dash := 0;
	  entry.Files := 1;
	  entry.Head := 0;
	  entry.Tail := 0;
	  scores.AddObject(name,entry)
	end
      end
      else if found.Name[1] <> '.' then
        traverse(CombinePath(dir,found.Name))
    until FindNext(found) <> 0;
    FindClose(found)
  end
end;

(* Traverse the list of scores, finding and collapsing consecutive non-PDF
   entries that differ only by a -digits suffix. Also strip trailing +n-m from
   PDF score titles and set the head and tail fields appropriately. *)
procedure mergePages;
var
  i, j, k, len, dash: Integer;
  entry, prev: TIndexEntry;
  s: String;
  plus, minus, dir, ext: Boolean;
begin
  i := 0; prev := NIL;
  while i < scores.Count do begin
    entry := TIndexEntry(scores.Objects[i]);
    len := Length(entry.Name);
    dash := 0;

    if entry.Ext = 'pdf' then begin
      (* Look for trailing +n-m indicating number of leading pages to skip
         when opening the score or when Home is pressed, and number of trailing
	 pages to ignore when End is pressed. *)
      j := len; plus := FALSE; minus := FALSE;
      while not plus or not minus do begin
	while (1 < j) and (entry.Name[j] in ['0'..'9']) do
	  DEC(j);
	if (1 < j) and (j < len) then begin
	  if (entry.Name[j] = '+') and not plus then begin
	    entry.Head := StrToInt(Copy(entry.Name,j+1,len-j));
	    plus := TRUE
	  end
	  else if (entry.Name[j] = '-') and not minus then begin
	    entry.Tail := StrToInt(Copy(entry.Name,j+1,len-j));
	    minus := TRUE
	  end
	  else
	    break;
          DEC(j);
	  len := j;
	  scores.Strings[i] := LeftStr(entry.Name,len)
	end
        else
          break
      end
    end
    else begin
      if (prev <> NIL) and (prev.Dash > 0) and (len > prev.Dash)
      and (entry.Name[prev.Dash] = '-')
      and (LeftStr(entry.Name,prev.Dash) = LeftStr(prev.Name,prev.Dash)) then
      begin
	(* Name is the same as the previous entry up to the dash. Check that
	   only digits follow it. *)
	j := prev.Dash + 1;
	while (j <= len) and (entry.Name[j] in ['0'..'9']) do
	  INC(j);
	if j > len then begin
	  (* Names the same except for -digits suffix. Part of same score. *)
	  INC(prev.Files);
	  entry.Destroy;
	  scores.Delete(i);
	  continue
	end
      end;

      (* Name is new. See if it ends in -digits. *)
      dash := RPos('-',entry.Name);
      if dash > 0 then begin
	j := dash + 1;
	while (j <= len) and (entry.Name[j] in ['0'..'9']) do
	  INC(j);
	if j > len then begin
	  entry.Dash := dash;
	  scores.Strings[i] := LeftStr(entry.Name,dash-1)
	end
      end
    end;

    prev := entry;
    INC(i)
  end;

  (* Set Dash field to length + 1 for all entries that didn't have a dash, and
     replace underscores with spaces in score display names. *)
  i := 0;
  while i < scores.Count do begin
    entry := TIndexEntry(scores.Objects[i]);
    if entry.Dash = 0 then
      entry.Dash := Length(entry.Name) + 1;
    scores.Strings[i] := Trim(DelSpace1(ReplaceStr(scores.Strings[i],'_',' ')));
    INC(i)
  end;

  (* Differentiate duplicate titles by directory and/or extension. *)
  i := 0; j := 0;
  while i < scores.Count do begin
    if (i > 0) and (scores.Strings[i] <> scores.Strings[i-1]) then begin
      (* Determine if there are directory, extension, or both differences. *)
      dir := FALSE; ext := FALSE;
      for k := j+1 to i-1 do begin
        if TIndexEntry(scores.Objects[k]).Dir <> TIndexEntry(scores.Objects[j]).Dir then
	  dir := TRUE;
        if TIndexEntry(scores.Objects[k]).Ext <> TIndexEntry(scores.Objects[j]).Ext then
	  ext := TRUE
      end;
      (* Construct a suffix for each duplicate title showing the differences. *)
      while j < i do begin
	s := '';
	if dir then begin
	  s := TIndexEntry(scores.Objects[j]).Dir;
	  if s = common then
	    s := ''
	  else if (common <> '') and (Length(s) > Length(common))
	  and (LeftStr(s,Length(common)) = common)
	  and (s[Length(common)+1] in AllowDirectorySeparators) then
	    s := RightStr(s,Length(s)-Length(common)-1);
	  if (s = home) or (Length(s) > Length(home))
			and (LeftStr(s,Length(home)) = home)
			and (s[Length(home)+1] in AllowDirectorySeparators) then
	    s := '~' + RightStr(s,Length(s)-Length(home))
	end;
	if ext then
	  if s <> '' then
	    s := CombinePath(s,'_.'+TIndexEntry(scores.Objects[j]).Ext)
	  else
	    s := TIndexEntry(scores.Objects[j]).Ext;
	if s <> '' then
	  scores.Strings[j] := scores.Strings[j] + ' (' + s + ')';
        INC(j)
      end
    end;
    INC(i)
  end
end;

procedure loadMRU;
var
  s: String;
  f: TextFile;
begin
  mruList := TStringList.Create;
  mruList.CaseSensitive := TRUE;

  AssignFile(f,mruFile);
  {$I-}
  Reset(f);
  {$I+}
  if IOResult = 0 then begin
    while (mruList.Count < MAX_MRU) and not EOF(f) do begin
      ReadLn(f,s);
      if scores.IndexOf(s) >= 0 then
	mruList.Add(s)
    end;
    CloseFile(f);
  end
end;

function GetMRU: TStringList;
begin
  GetMRU := mruList
end;

procedure UpdateMRU( entry: TIndexEntry );
var
  i: Integer;
  s: String;
  f: TextFile;
begin
  i := scores.IndexOfObject(entry);
  if i >= 0 then begin
    s := scores.Strings[i];
    i := mruList.IndexOf(s);
    (* Add new entry to the end of the MRU list if it's not already present. *)
    if i < 0 then begin
      i := mruList.Count;
      mruList.Add(s)
    end;
    (* Percolate entry from wherever it is to the beginning of the list. *)
    while i > 0 do begin
      mruList.Exchange(i,i-1);
      DEC(i)
    end;
    while mruList.Count > MAX_MRU do
      mruList.Delete(mruList.Count-1)
  end;

  AssignFile(f,mruFile);
  {$I-}
  Rewrite(f);
  {$I+}
  if IOResult = 0 then begin
    for s in mruList do
      WriteLn(f,s);
    CloseFile(f)
  end
end;

(* Case-insensitive string compare ignoring leading "a", "an", or "the". *)
function compareTitles( list: TStringList; a, b: Integer ): Integer;
var
  sa, sb: String;
  ia, la, ib, lb: Integer;

  (* Find beginning of first word after leading "a", "an", or "the". *)
  function skipArticle( s: String ): Integer;
  var
    l: Integer;
  begin
    skipArticle := 1;
    l := Length(s);
    if (l > 2) and (s[1] = 'a') and (s[2] = ' ') then
      skipArticle := 3
    else if (l > 3) and (s[1] = 'a') and (s[2] = 'n') and (s[3] = ' ') then
      skipArticle := 4
    else if (l > 4) and (s[1] = 't') and (s[2] = 'h') and (s[3] = 'e')
    and (s[4] = ' ') then
      skipArticle := 5
  end;

begin
  sa := LowerCase(list[a]); la := Length(sa); ia := skipArticle(sa);
  sb := LowerCase(list[b]); lb := Length(sb); ib := skipArticle(sb);
  while (ia <= la) and (ib <= lb) and (sa[ia] = sb[ib]) do begin
    INC(ia);
    INC(ib)
  end;
  if ia > la then
    if ib > lb then
      (* Identical when ignoring articles. Sort based on entire string. *)
      if sa > sb then
        compareTitles := 1
      else if sa < sb then
        compareTitles := -1
      else
        compareTitles := 0
    else
      compareTitles := -1
  else if (ib > lb) or (sa[ia] > sb[ib])  then
    compareTitles := 1
  else
    compareTitles := -1
end;

function Build: TStringList;
var
  root: String;
  i, j: Integer;
begin
  scores := TStringList.Create;
  scores.CaseSensitive := TRUE;

  (* Search specified directories, or ~/SheetMusic if none were specified. *)
  if ParamCount > 0 then begin
    common := ParamStr(1);
    for i := 1 to ParamCount do begin
      root := ParamStr(i);
      if i > 1 then begin
	j := 1;
	while (j <= Length(root)) and (j <= Length(common))
	and (root[j] = common[j]) do
	  INC(j);
	common := LeftStr(common,j-1)
      end;
      if DirectoryExists(root) then traverse(root)
    end;
    i := Length(common);
    while (i > 0) and not (common[i] in AllowDirectorySeparators) do DEC(i);
    if i = 0 then common := '' else common := LeftStr(common,i-1)
  end
  else begin
    root := CombinePath(home,'SheetMusic');
    if DirectoryExists(root) then traverse(root);
    common := root
  end;

  scores.CustomSort(@compareTitles);
  mergePages;
  scores.CustomSort(@compareTitles);

  loadMRU;

  Build := scores
end;

var
  config: String;
begin
  home := GetUserDir;
  if (home <> '') and (home[Length(home)] in AllowDirectorySeparators) then
    home := LeftStr(home,Length(home)-1);

  config := GetAppConfigDir(FALSE);
  if not DirectoryExists(config) then CreateDir(config);
  mruFile := CombinePath(config,'mru');
  mruList := NIL;

  common := ''
end.
