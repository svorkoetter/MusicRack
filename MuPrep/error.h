/* MusicRack/muprep - Error Handling

   Copyright (c) 2019 by Stefan Vorkoetter

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
   MusicRack. If not, see <http://www.gnu.org/licenses/>. */

#ifndef __MUSIC_RACK_MUPREP_ERROR_H__
#define __MUSIC_RACK_MUPREP_ERROR_H__

/* Print error message and exit with exit code 1. */
extern void Error( const char *msg );

/* Print error message with a string parameter and exit with exit code 1. */
extern void ErrorS( const char *msg, const char *s );

/* Print error message with an integer parameter and exit with exit code 1. */
extern void ErrorI( const char *msg, int i );

#endif
