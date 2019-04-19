/* MusicRack/muprep - Image Convolution

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

#ifndef __MUSIC_RACK_MUPREP_CONVOLUTION_H__
#define __MUSIC_RACK_MUPREP_CONVOLUTION_H__

/* Apply a 3x3 convolution mask to a greyscale image, returning a new image. */
extern uint16_t *Convolve3x3( uint16_t *data, int wData, int hData, int *mask );

#endif
