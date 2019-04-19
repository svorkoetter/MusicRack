unit Utility; // vim:sw=2:

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

(* Combine two components of a file path, fixing up directory separators,
   and ensuring that exactly one separator ends up between the two parts. *)
function CombinePath( a, b: String ): String;

(* Return the last modified time of a file given its name. This is just a
   renaming of the misleadingly named FileAge function of Delphi/FreePascal. *)
function FileTime( fn: String ): LongInt;

implementation

uses SysUtils;

function CombinePath( a, b: String ): String;
begin
  DoDirSeparators(a);
  DoDirSeparators(b);
  if (Length(a) > 0) and (a[Length(a)] = DirectorySeparator) then
    a := LeftStr(a,Length(a)-1);
  if (Length(b) > 0) and (b[1] = DirectorySeparator) then
    b := RightStr(b,Length(b)-1);
  CombinePath := a + DirectorySeparator + b
end;

function FileTime( fn: String ): LongInt;
begin
  fileTime := FileAge(fn)
end;

end.
