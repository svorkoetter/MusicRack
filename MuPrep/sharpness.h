/* MusicRack/muprep - Image Sharpness Determination

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

#ifndef __MUSIC_RACK_MUPREP_SHARPNESS_H__
#define __MUSIC_RACK_MUPREP_SHARPNESS_H__

/* Estimate the sharpness of an image by computing the average horizontal and
   vertical delta between pixels. */
extern double EstimateSharpness( uint8_t *img, int w, int h );

#endif
