/* MusicRack/muprep - Image Trimming, Cropping, and Padding

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

#ifndef __MUSIC_RACK_MUPREP_CROPPING_H__
#define __MUSIC_RACK_MUPREP_CROPPING_H__

/* Find and remove the white margins of a greyscale image. Return the trimmed
   image in (a prefix of) the original image's memory. */
extern uint8_t *Trim( uint8_t *img, int w, int h, int *wout, int *hout );

/* Keep the the part of an image specified by the rectangle (c1,r1)-(c2,r2).
   Return the cropped image in (a prefix of) the original image's memory. */
extern uint8_t *Crop( uint8_t *img, int w, int h, int c1, int r1, int c2, int r2,
		      int *wout, int *hout );

/* Pad the image with extra whitespace on the left, top, right, and bottom,
   returning a new image. */
extern uint8_t *Pad( uint8_t *img, int w, int h, int lm, int tm, int rm, int bm,
		     int *wout, int *hout );

#endif
