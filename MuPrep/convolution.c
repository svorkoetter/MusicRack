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

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <math.h>

#include "convolution.h"
#include "error.h"

#ifdef SHARPEN
uint16_t *Sharpen( uint16_t *data, int wData, int hData )
#else
uint16_t *Convolve3( uint16_t *data, int wData, int hData,
		     int centre, int edge, int corner )
#endif
{
#ifndef SHARPEN
    /* Compute weight of convolution mask. */
    int maskWeight = centre + 4 * (edge + corner);
    if( maskWeight == 0 )
        maskWeight = 1;
#endif

    uint16_t *res = (uint16_t *) malloc(wData * hData * sizeof(uint16_t));
    if( res == NULL )
        Error("failed to allocate storage for convolution output");

    /* Edge pixels are untouched, so copy them into the output. First the left
       and right edges ... */
    uint16_t *ip = data, *op = res;
    for( int r = 0; r < hData; ++r ) {
        op[0] = ip[0];
	op[wData-1] = ip[wData-1];
	op += wData;
	ip += wData;
    }
    /* ... then the top and bottom edges. The corners get copied again. */
    ip = data + (hData - 1) * wData;
    op = res + (hData - 1) * wData;
    for( int c = 0; c < wData; ++c ) {
	res[c] = data[c];
	op[c] = ip[c];
    }

    /* Now perform the convolution on the interior pixels. */
    uint16_t *ip0 = data, *ip1 = ip0 + wData, *ip2 = ip1 + wData;
    op = res + wData;
    for( int r = 2; r < hData; ++r ) {
        for( int c = 2; c < wData; ++c ) {
#ifdef SHARPEN
	    /* Mask weight is 32, so we can normalize by shifting right 5. */
	    int f = 44 * ip1[1]
	          - (ip0[1] + ip1[0] + ip1[2] + ip2[1])
	          - 2 * (ip0[0] + ip0[2] + ip2[0] + ip2[2]);
#else
	    int f = corner * (ip0[0] + ip0[2] + ip2[0] + ip2[2])
	          + edge   * (ip0[1] + ip1[0] + ip1[2] + ip2[1])
		  + centre * ip1[1];
#endif
	    ++ip0; ++ip1; ++ip2;
	    if( f < 0 )
		*++op = 0;
#ifdef SHARPEN
	    else if( (f >>= 5) <= 65535 )
#else
	    else if( (f /= maskWeight) <= 65535 )
#endif
		*++op = f;
	    else
		*++op = 65535;
	}
	/* Skip edges. */
	ip0 += 2; ip1 += 2; ip2 += 2;
	op += 2;
    }

    return( res );
}

/* Include self to generate a sharpening-specific version of Convolve without
   having to duplicate all the code. */
#ifndef SHARPEN
# define SHARPEN
# include "convolution.c"
#endif
