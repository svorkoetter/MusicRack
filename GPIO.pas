unit GPIO; // vim:sw=2:

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

function Open: Boolean;
function Scan: Char;
procedure Close;

implementation

uses BaseUnix, FileUtil, Process, SysUtils, Utility;

const
  MAX_GPIO = 30;

type
  GPIOEntry = record
    bcm: Integer;	{ BCM GPIO port number }
    pos: Boolean;	{ Positive logic, i.e. '1' when pressed }
    res: Integer;	{ Internal pull up/down resistors: -1, 0, or 1 }
    key: Char;		{ Keyboard shortcut to invoke (ASCII character) }
    pressed: Boolean;	{ Current state, pressed or not }
  end;
  
var
  gpioFile: String;
  gpioCount: Integer;
  gpioMap: array [1..MAX_GPIO] of GPIOEntry;
  gpioPath: String[50];
  gpioNumIndex: Integer;

function Open: Boolean;
var
  s, exe, outstr, mode: String;
  i, j, n: Integer;
  argv: array of String;
  f: TextFile;
begin
  (* Read the GPIO configuration file if present. *)
  AssignFile(f,gpioFile);
  {$I-}
  Reset(f);
  {$I+}
  if IOResult = 0 then begin
    gpioCount := 0;
    while not EOF(f) do begin
      {$I-}
      ReadLn(f,s);
      {$I+}
      if IOResult <> 0 then break;
      (* Process only lines beginning with a number. Line format is:

         gpioNum [~] [+|-] [shortcutChar]

	 Defaults for the optional parts are blank, '+', and ' '. *)
      i := 1;
      while (i <= Length(s)) and (s[i] in ['0'..'9']) do INC(i);
      if i > 1 then begin
	n := StrToInt(LeftStr(s,i-1));
	(* In case of a duplicate port number, replace the existing entry. *)
	j := 1;
	while (j <= gpioCount) and (gpioMap[j].bcm <> n) do INC(j);
	if j > gpioCount then
	  INC(gpioCount);
	(* Set BCM port number. *)
	gpioMap[j].bcm := n;
	while (i <= Length(s)) and (s[i] in [' ',#9]) do INC(i);
	(* Check for ~ indicating negative logic. *)
	if (i <= Length(s)) and (s[i] = '~') then begin
	  gpioMap[j].pos := FALSE;
	  INC(i);
	  while (i <= Length(s)) and (s[i] in [' ',#9]) do INC(i)
	end
	else
	  gpioMap[j].pos := TRUE;
	(* Check for + or - indicating pull-up or pull-down requested. *)
	if (i <= Length(s)) and (s[i] in ['+','-']) then begin
	  if s[i] = '+' then
	    gpioMap[j].res := 1
	  else
	    gpioMap[j].res := -1;
	  INC(i);
	  while (i <= Length(s)) and (s[i] in [' ',#9]) do INC(i)
	end
	else
	  gpioMap[j].res := 0;
	(* Get the character that this GPIO input should map to. *)
	if i <= Length(s) then
	  gpioMap[j].key := s[i]
	else
	  gpioMap[j].key := ' ';
	gpioMap[j].pressed := FALSE
      end
    end;
    CloseFile(f);
  end;

  (* Initialize the GPIO ports using the external 'gpio' command, so as not to
     require root privileges to run this program. *)
  if gpioCount > 0 then begin
    exe := FindDefaultExecutablePath('gpio');
    if exe <> '' then begin
      for i := 1 to gpioCount do begin
	(* Make the pin available via /sys/class/gpio. *)
	argv := argv.Create('export',IntToStr(gpioMap[i].bcm),'in');
	outstr := '';
	RunCommand(exe,argv,outstr);
	(* Set pull-up, pull-down, or none. *)
	if gpioMap[i].res > 0 then
	  mode := 'up'
	else if gpioMap[i].res < 0 then
	  mode := 'down'
	else
	  mode := 'tri';
	argv := argv.Create('-g','mode',IntToStr(gpioMap[i].bcm),mode);
	RunCommand(exe,argv,outstr)
      end
    end
  end;

  Open := gpioCount > 0
end;

function Scan: Char;
var
  i, j, n, fd: Integer;
  buf: String[1];
  pressed: Boolean;
begin
  Scan := #0;
  for i := 1 to gpioCount do begin
    (* Construct path to /sys/class/gpio in-place to avoid memory allocation. *)
    n := gpioMap[i].bcm;
    j := gpioNumIndex;
    if n >= 10 then begin
      gpioPath[j] := Chr(Ord('0') + n div 10);
      INC(j)
    end;
    gpioPath[j] := Chr(Ord('0') + n mod 10); INC(j);
    gpioPath[j] := '/'; INC(j);
    gpioPath[j] := 'v'; INC(j);
    gpioPath[j] := 'a'; INC(j);
    gpioPath[j] := 'l'; INC(j);
    gpioPath[j] := 'u'; INC(j);
    gpioPath[j] := 'e';
    SetLength(gpioPath,j);
    fd := fpopen(gpioPath,O_RDONLY);
    if fd >= 0 then begin
      buf := '0';
      if fpread(fd,buf[1],1) = 1 then begin
	pressed := (buf[1] = '1') xor not gpioMap[i].pos;
	if pressed and not gpioMap[i].pressed then begin
	  gpioMap[i].pressed := TRUE;
	  Scan := gpioMap[i].key;
	  break
	end
	else if not pressed and gpioMap[i].pressed then
	  gpioMap[i].pressed := FALSE
      end;
      fpclose(fd)
    end
  end
end;

procedure Close;
var
  exe, outstr: String;
  i: Integer;
  argv: array of String;
begin
  if gpioCount > 0 then begin
    exe := FindDefaultExecutablePath('gpio');
    if exe <> '' then begin
      for i := 1 to gpioCount do begin
	(* Remove from /sys/class/gpio. *)
	argv := argv.Create('unexport',IntToStr(gpioMap[i].bcm));
	outstr := '';
	RunCommand(exe,argv,outstr);
	(* Disconnect pull-up/down resistors. *)
	argv := argv.Create('-g','mode',IntToStr(gpioMap[i].bcm),'tri');
	RunCommand(exe,argv,outstr)
      end
    end;
    gpioCount := 0
  end
end;

begin
  gpioFile := CombinePath(GetAppConfigDir(FALSE),'gpio');
  gpioCount := 0;
  gpioPath := '/sys/class/gpio/gpio';
  gpioNumIndex := Length(gpioPath) + 1
end.
