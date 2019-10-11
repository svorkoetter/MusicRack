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

/* Highly optimized implementation of bicubic B-spline interpolation described
   in the paper http://paulbourke.net/miscellaneous/imageprocess (last article
   on page). */

static double R( double x )
{
    double a;
    if( (a = x + 2) <= 0 ) return( 0.0 );

    double rx = 0.25 * a * a * a;
    if( (a = x + 1) > 0 ) {
	rx -= a * a * a;
	if( x > 0 ) {
	    rx += 1.5 * x * x * x;
	    if( (a = x - 1) > 0 )
		rx -= a * a * a;
	}
    }

    return( rx * 0.666666666666667 );
}

static void scale( int w, int h, int w2, int h2, uint16_t *dp, uint16_t *dpout )
{
    /* Compute the scaling factors so that only source pixels actually within
       the source image are ever looked at. That removes a lot of special-case
       code from the loops. It's not strictly correct, because the _content_ of
       the image will be rescaled a tiny bit smaller than asked for, but the
       difference is negligible in this application (scaling sheet music). */
    double epsilon = 1e-3;
    double scaleX = (w2 - 1.0) / (w - 4.0 - 2 * epsilon);
    double scaleY = (h2 - 1.0) / (h - 4.0 - 2 * epsilon);

    /* Precompute R(dc-dx) values, as they are the same on every row, and thus
       will be used over and over again. */
    double *RX = (double *) malloc(w2 * 4 * sizeof(double));
    if( RX == NULL )
        Error("failed to allocate storage for row coefficients");
    for( int c2 = 0; c2 < w2; ++c2 ) {
	double x = c2 / scaleX + 1;
	int c = (int) x;
	double dx = x - c;
	for( int dc = -1; dc <= 2; ++dc )
	    RX[c2*4+1+dc] = R(dc-dx);
    }
	
    for( int r2 = 0; r2 < h2; ++r2 ) {
	double y = r2 / scaleY + 1;
	int r = (int) y;

	double dy = y - r;
	double RY0 = R(-1 - dy), RY1 = R(0 - dy);
	double RY2 = R(1 - dy),  RY3 = R(2 - dy);

	int i = (r - 1) * w - 1;
	double *rxp = RX;
	for( int c2 = 0; c2 < w2; ++c2 ) {
	    double x = c2 / scaleX + 1;
	    int c = (int) x;

	    uint16_t *dp0 = dp + i + c, *dp1 = dp0 + w;
	    uint16_t *dp2 = dp1 + w,    *dp3 = dp2 + w;

	    double f = 0.5;
	    f += *rxp++
	       * (*dp0++ * RY0 + *dp1++ * RY1 + *dp2++ * RY2 + *dp3++ * RY3);
	    f += *rxp++
	       * (*dp0++ * RY0 + *dp1++ * RY1 + *dp2++ * RY2 + *dp3++ * RY3);
	    f += *rxp++
	       * (*dp0++ * RY0 + *dp1++ * RY1 + *dp2++ * RY2 + *dp3++ * RY3);
	    f += *rxp++
	       * (*dp0   * RY0 + *dp1   * RY1 + *dp2   * RY2 + *dp3   * RY3);

	    if( f < 0 )
		*dpout++ = 0;
	    else if( f > 65535 )
		*dpout++ = 65535;
	    else
		*dpout++ = (uint16_t) f;
	}
    }

    free(RX);
}

/* Maximum downsampling factor to do in a single step. A larger factor means
   fewer steps for large scale changes, but will result in a poorer image. Too
   small a factor results in many steps, and a fuzzy image. */
#define MIN_STEP 2.2

/* The fudge factor is to prevent recursive downscaling from generating an
   extra step due to rounding error in cases where the downscaling amount is
   close to an integer power of MIN_STEP. */
#define MIN_STEP_FUDGE 0.1

static uint16_t *scaleRecursive( uint16_t *img, int w, int h, int w2, int h2,
				 double minStep )
{
    /* Don't do anything if there's no difference in size. */
    if( h2 == h && w2 == w )
        return( img );

    double scaleX = (double) w2 / (double) w;
    double scaleY = (double) h2 / (double) h;
    double scaleXY = sqrt(scaleX * scaleY);

    uint16_t *origImg = img;

    /* Break the operation down into a number of intermediate steps of
       equal scaling. For example, to scale by 1/3, scale by (1/3)^(1/2)
       twice; to scale by (1/7), scale by (1/7)^(1/3) three times. */
    int steps = 1;
    for( double s = scaleXY; s < (1/minStep); s *= minStep )
	++steps;

    if( steps > 1 ) {
	/* Compute scale factors for each step. */
	double stepY = pow(scaleY,(steps-1.0)/steps);
	double stepX = pow(scaleX,(steps-1.0)/steps);

	/* Do a recursive call to scale far enough that we can scale the
	   rest of the way in a single step. */
	int wt = w * stepX, ht = h * stepY;
	img = scaleRecursive(img,w,h,wt,ht,minStep+MIN_STEP_FUDGE);
	w = wt; h = ht;
    }

    uint16_t *res = (uint16_t *) malloc(h2 * w2 * sizeof(uint16_t));
    if( res == NULL )
        Error("failed to allocate storage for scaled image");

    /* The scaling operation is in a separate function to let the compiler do a
       better job optimizing on architectures with few registers. */
    scale(w,h,w2,h2,img,res);

    /* Delete any temporary image created for blurring. */
    if( img != origImg )
        free(img);

    return( res );
}

uint16_t *Scale( uint16_t *img, int w, int h, int w2, int h2 )
{
    return( scaleRecursive(img,w,h,w2,h2,MIN_STEP) );
}
