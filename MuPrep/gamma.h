/* MusicRack/muprep - Image Gamma Conversion

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

#ifndef __MUSIC_RACK_MUPREP_GAMMA_H__
#define __MUSIC_RACK_MUPREP_GAMMA_H__

/* Transform image from 8-bit gamma-corrected to 16-bit linear intensity. The
   result will be in newly allocated storage since the 8-bit buffer is only
   half the size needed. */
extern uint16_t *ToLinear( uint8_t *img, int w, int h );

/* Transform image from 16-bit linear to 8-bit gamma-corrected in-place (since
   there will already be twice as much space as needed). */
extern uint8_t *ToGamma( uint16_t *img, int w, int h );

#endif
