/* MusicRack/muprep - Image Skew Determination and Rotation

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

#ifndef __MUSIC_RACK_MUPREP_ROTATION_H__
#define __MUSIC_RACK_MUPREP_ROTATION_H__

/* Compute a vector with one entry for the median of each row in rectangle. */
extern uint8_t *RowMedians( uint8_t *img, int w, int h,
			    int x1, int y1, int x2, int y2 );

/* Given two vectors calculated by the function above, determine the shift, up
   to a specified maximum, that minimizes the Hamming distance between them. */
extern int EstimateSkew( uint8_t *med1, uint8_t *med2, int h, int max );

/* Given an image and a slope, pseudo-rotate (by horizontal and vertical
   skewing) the image to level that slope. */
extern uint16_t *Rotate( uint16_t *img, int w, int h, int dy, int dx );

#endif
