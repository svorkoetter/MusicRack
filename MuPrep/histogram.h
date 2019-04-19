/* MusicRack/muprep - Image Histogram Operations

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

#ifndef __MUSIC_RACK_MUPREP_HISTOGRAM_H__
#define __MUSIC_RACK_MUPREP_HISTOGRAM_H__

struct Histogram {
    int buckets[256];
    int total;
};

/* Compute a 256-bucket histogram of an image. A 257th bucket is used to store
   to total pixel count. */
extern void ComputeHistogram( uint8_t *img, int w, int h,
			      struct Histogram *hist );

/* Find the bucket (0 to 255) corresponding to a percentile (0 to 1). */
extern int FindPercentile( struct Histogram *hist, double p );

/* Find the longest contiguous sequence of buckets containing at least the
   specified fraction (0 to 1) of pixels. */
extern void FindHistogramSpan( struct Histogram *hist, double p,
			       int *p1, int *p2 );

/* Expand the range of levels i1 to i2 in an image to use the full 0 to 255
   range and update the histogram accordingly. */
extern void ExpandHistogram( uint8_t *img, int w, int h,
			     struct Histogram *hist, int i1, int i2 );

#endif
