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

#include <stdio.h>
#include <stdlib.h>

#include "error.h"

void Error( const char *msg )
{
    fputs("muprep: ",stderr);
    fprintf(stderr,"%s\n",msg);
    exit(1);
}

void ErrorS( const char *msg, const char *s )
{
    fputs("muprep: ",stderr);
    fprintf(stderr,msg,s);
    fputc('\n',stderr);
    exit(1);
}

void ErrorI( const char *msg, int i )
{
    fputs("muprep: ",stderr);
    fprintf(stderr,msg,i);
    fputc('\n',stderr);
    exit(1);
}
