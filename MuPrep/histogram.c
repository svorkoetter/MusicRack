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

#include <stdbool.h>
#include <stdint.h>
#include <math.h>

#include "histogram.h"

void ComputeHistogram( uint8_t *img, int w, int h, struct Histogram *hist )
{
    /* Clear the histogram. */
    for( int i = 0; i < 256; ++i )
        hist->buckets[i] = 0;
    hist->total = 0;

    /* Fill in the buckets. */
    for( int i = w * h; i > 0; --i )
        ++hist->buckets[*img++];

    /* Record the total. */
    for( int i = 0; i < 256; ++i )
        hist->total += hist->buckets[i];
}

int FindPercentile( struct Histogram *hist, double p )
{
    int ip = (int) (p * hist->total);

    for( int i = 0; ip > 0 && i < 256; ++i ) {
	if( (ip -= hist->buckets[i]) <= 0 ) {
	    /* Return either the current or previous bucket depending on how
	       far the current bucket went past the target. */
	    if( i > 0 && ip + hist->buckets[i] < -ip )
	        return( i - 1 );
	    return( i );
	}
    }
    return( 255 );
}

void FindHistogramSpan( struct Histogram *hist, double p, int *i1, int *i2 )
{
    int ip = (int) (p * hist->total);

    int len = 0;
    *i1 = 0, *i2 = 255;

    for( int i = 0; i < 255; ++i ) {
        int s = 0;
	for( int j = i; j < 256; ++j ) {
	    if( s += hist->buckets[j] > ip ) {
	        if( j - i > len ) {
		    *i1 = i; *i2 = j;
		    len = j - i;
		}
		break;
	    }
	}
    }
}

void ExpandHistogram( uint8_t *img, int w, int h,
		      struct Histogram *hist, int i1, int i2 )
{
    /* ExpandHistogram pixels in i1..i2 to 0..255 */
    if( i1 > 0 || i2 < 255 ) {
	for( int i = w * h; i > 0; --i ) {
	    uint8_t v = *img;
	    if( v < i1 )
		v = 0;
	    else if( v > i2 )
		v = 255;
	    else
		v = ((v - i1) * 255) / (i2 - i1);
	    *img++ = v;
	}
    }

    /* Fix up histogram (to avoid having to recalculate it from the image). */
    int newHist[256];
    for( int i = 0; i < 256; ++i )
        newHist[i] = 0;
    for( int i = 0; i < 256; ++i ) {
        if( i < i1 )
	    newHist[0] += hist->buckets[i];
	else if( i > i2 )
	    newHist[255] += hist->buckets[i];
	else
	    newHist[((i - i1) * 255) / (i2 - i1)] += hist->buckets[i];
    }
    for( int i = 0; i < 256; ++i )
        hist->buckets[i] = newHist[i];
}
