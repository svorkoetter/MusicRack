/* MusicRack/muprep - Image Scaling

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
#include "scaling.h"
#include "error.h"

/* Perform weighted box scaling. This has the advantage over bilinear or
   bicubic of not requiring multiple scaling passes when scaling down by more
   than a factor of 2, and preserving small detail such as thin lines. */

static int generateWeights( int lIn, int lOut,
			    double **pWeights, int **pIndices )
{
    double scale = (double) lIn / (double) lOut;
    int nWeights = ceil(scale) + 1;
    double *weights = (double *) malloc(lOut * nWeights * sizeof(double));
    int *indices = (int *) malloc(lOut * nWeights * sizeof(int));

    if( weights == NULL || indices == NULL )
        Error("failed to allocate storage for scaling weights");

    double invScale = 1.0 / scale;

    double i1L = 0.0;
    int ii1L = 0, offset = 0;
    for( int iOut = 0; iOut < lOut; ++iOut ) {
	double i1R = i1L + scale;
	if( i1R > lIn - 1e-6 ) {
	    /* Guard against accumulated error going beyond the array. */
	    i1R = lIn - 1e-6;
	}
	int ii1R = floor(i1R);

	/* Partial left pixel. */
	int w = 0;
	indices[offset + w] = ii1L;
	weights[offset + w] = (ii1L + 1 - i1L) * invScale;
	++w;

	/* Full intermediate pixels. */
	for( int c = ii1L + 1; c < ii1R; ++c ) {
	    indices[offset + w] = c;
	    weights[offset + w] = invScale;
	    ++w;
	}

	/* Partial right pixel. */
	indices[offset + w] = ii1R;
	weights[offset + w] = (i1R - ii1R) * invScale;
	++w;

	/* Unused weights and indices. Use last valid index and zero weight. */
	while( w < nWeights ) {
	    indices[offset + w] = ii1R;
	    weights[offset + w] = 0.0;
	    ++w;
	}

	i1L = i1R;
	ii1L = ii1R;
	offset += nWeights;
    }

    /* Return values. */
    *pWeights = weights;
    *pIndices = indices;
    return( nWeights );
}

uint16_t *Scale( uint16_t *img, int w, int h, int w2, int h2 )
{
    /* Don't do anything if there's no difference in size. */
    if( h2 == h && w2 == w )
        return( img );

    /* Precalculate column and row weights. */
    double *cWeights = NULL;
    int *cIndices = NULL;
    int nCWeights = generateWeights(w,w2,&cWeights,&cIndices);

    double *rWeights = NULL;
    int *rIndices = NULL;
    int nRWeights = generateWeights(h,h2,&rWeights,&rIndices);

    uint16_t *res = (uint16_t *) malloc(h2 * w2 * sizeof(uint16_t));
    if( res == NULL )
        Error("failed to allocate storage for scaled image");

    /* Walk through output image, accumulating weighted input pixels. */
    uint16_t *rp = res;
    int r2Offset = 0;
    for( int r2 = 0; r2 < h2; ++r2 ) {
	int c2Offset = 0;
        for( int c2 = 0; c2 < w2; ++c2 ) {
	    double f = 0.5;
	    for( int rw = 0; rw < nRWeights; ++rw ) {
		double rWeight = rWeights[r2Offset + rw];
		int rOffset = rIndices[r2Offset + rw] * w;
	        for( int cw = 0; cw < nCWeights; ++cw ) {
		    f += img[rOffset + cIndices[c2Offset + cw]]
		       * rWeight * cWeights[c2Offset + cw];
		}
	    }
	    *rp++ = f;
	    c2Offset += nCWeights;
	}
	r2Offset += nRWeights;
    }

    free(cWeights);
    free(cIndices);
    free(rWeights);
    free(rIndices);

    return( res );
}
