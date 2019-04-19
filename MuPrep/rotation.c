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

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>
#include <string.h>
#include <math.h>

#include "cropping.h"
#include "error.h"
#include "rotation.h"

static int comparePixels( const void *p1, const void *p2 )
{
    uint8_t i1 = *(const uint8_t *)p1;
    uint8_t i2 = *(const uint8_t *)p2;
    if( i1 < i2 ) return( -1 );
    if( i1 > i2 ) return( 1 );
    return( 0 );
}

uint8_t *RowMedians( uint8_t *img, int w, int h,
		     int c1, int r1, int c2, int r2 )
{
    uint8_t *buf = (uint8_t *) malloc(c2 - c1 + 1);
    if( buf == NULL )
        Error("failed to allocate storage for median buffer");

    uint8_t *med = (uint8_t *) malloc(r2 - r1 + 1);
    if( med == NULL )
        Error("failed to allocate storage for median vector");

    uint8_t *mp = med;
    uint8_t *ip = img + r1 * w;
    for( int r = r1; r <= r2; ++r ) {
        uint8_t *p = buf;
	for( int c = c1; c <= c2; ++c )
	    *p++ = ip[c];
	qsort(buf,p-buf,1,comparePixels);
	*mp++ = (buf[(c2 - c1) / 2] + buf[(c2 - c1 + 1) /2]) / 2;
	ip += w;
    }
    free(buf);

    return( med );
}

int EstimateSkew( uint8_t *med1, uint8_t *med2, int h, int max )
{
    /* Initialize best Hamming distance with that of zero shift. */
    int s = 0;
    for( int j = 0; j < h; ++j ) {
	int d = med1[j] - med2[j];
	s += d * d;
    }
    int bestShift = 0, bestDist = s / h;

    /* Try shifts up to the specified maximum in each direction, trying to find
       the one resulting in the smallest Hamming distance. */
    for( int i = 1; i < max; ++i ) {
	s = 0;
	for( int j = i; j < h; ++j ) {
	    int d = med2[j-i] - med1[j];
	    s += d * d;
	}
	if( s / (h - i) < bestDist ) {
	    bestDist = s / (h - i);
	    bestShift = i;
	}
	s = 0;
	for( int j = i; j < h; ++j ) {
	    int d = med2[j] - med1[j-i];
	    s += d * d;
	}
	if( s / (h - i) < bestDist ) {
	    bestDist = s / (h - i);
	    bestShift = -i;
	}
    }

    return( bestShift );
}

static void horizontalShear( uint16_t *img, int w, int h, double slope )
{
    int offset = (int) ceil(fabs(h * slope)) + 2;

    size_t rowBufLen = w + 2 * offset;
    uint16_t *rowBuf = (uint16_t *) malloc(rowBufLen * sizeof(uint16_t));
    if( rowBuf == NULL )
        Error("failed to allocate storage for horizontal shear buffer");
    uint16_t *rbp = rowBuf + offset;

    memset(rowBuf,255,rowBufLen*sizeof(uint16_t));

    double dx = slope < 0 ? offset / 2.0 : -offset / 2.0;
    for( int r = 0; r < h; ++r ) {
	int wt1 = (int) (256 * (ceil(dx) - dx));
	int wt2 = 256 - wt1;

	memcpy(rbp,img,w*sizeof(uint16_t));

	uint16_t *ip = rbp + (int) dx;
	for( int c = 0; c < w; ++c ) {
	    *img++ = (ip[0] * wt1 + ip[1] * wt2) / 256;
	    ++ip;
	}
	dx += slope;
    }

    free(rowBuf);
}

static void verticalShear( uint16_t *img, int w, int h, double slope )
{
    int offset = (int) ceil(fabs(w * slope)) + 2;

    size_t colBufLen = h + 2 * offset;

    uint16_t *colBuf = (uint16_t *) malloc(colBufLen * sizeof(uint16_t));
    if( colBuf == NULL )
        Error("failed to allocate storage for vertical shear buffer");
    uint16_t *cbp = colBuf + offset;

    memset(colBuf,255,colBufLen*sizeof(uint16_t));

    double dy = slope < 0 ? offset / 2.0 : -offset / 2.0;
    for( int c = 0; c < w; ++c ) {
	int wt1 = (int) (256 * (ceil(dy) - dy));
	int wt2 = 256 - wt1;

	uint16_t *cp = img + c;
	for( int r = 0; r < h; ++r ) {
	    cbp[r] = *cp;
	    cp += w;
	}

	uint16_t *ip = cbp + (int) dy;
	cp = img + c;
	for( int r = 0; r < h; ++r ) {
	    *cp = (ip[0] * wt1 + ip[1] * wt2) / 256;
	    ++ip;
	    cp += w;
	}
	dy += slope;
    }

    free(colBuf);
}

uint16_t *Rotate( uint16_t *img, int w, int h, int dy, int dx )
{
    double slope = (1.0 * dy) / dx;

    if( fabs(slope) < 0.02 ) {
	/* For angles less than about 1.15 degrees, do two shear operations to
	   perform an approximate rotation. */
	horizontalShear(img,w,h,slope);
	verticalShear(img,w,h,-slope);
    }
    else {
	/* For larger angles, do three shears to perform an actual rotation. */
	double theta = atan2(dy,dx);
	double hshear = tan(theta/2);
	double vshear = -sin(theta);
	horizontalShear(img,w,h,hshear);
	verticalShear(img,w,h,vshear);
	horizontalShear(img,w,h,hshear);
    }

    return( img );
}
