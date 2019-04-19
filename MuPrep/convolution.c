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

uint16_t *Convolve3x3( uint16_t *data, int wData, int hData, int *mask )
{
    /* Compute weight of convolution mask. */
    int maskWeight = 0;
    for( int i = 0; i < 9; ++i )
	maskWeight += mask[i];
    if( maskWeight == 0 )
        maskWeight = 1;

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
    /* ... then the top and bottom edges. */
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
	    int f = ip0[0] * mask[0] + ip0[1] * mask[1] + ip0[2] * mask[2]
	          + ip1[0] * mask[3] + ip1[1] * mask[4] + ip1[2] * mask[5]
	          + ip2[0] * mask[6] + ip2[1] * mask[7] + ip2[2] * mask[8];
	    ++ip0; ++ip1; ++ip2;
	    if( f < 0 )
		*++op = 0;
	    else if( (f /= maskWeight) <= 65535 )
		*++op = f;
	    else
		*++op = 65535;
	}
	ip0 += 2; ip1 += 2; ip2 += 2;
	op += 2;
    }

    return( res );
}
