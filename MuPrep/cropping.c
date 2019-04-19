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

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <math.h>

#include "cropping.h"
#include "error.h"

/* Average pixel values below which a row or column is considered non-blank. */
#define TOP_THRESHOLD 251
#define BOT_THRESHOLD 253
#define COL_THRESHOLD 253

/* To try to remove page numbers, place more weight on the middle of each row
   and column when determining the average darkness These constants control
   what fraction of the total row or column is considered the middle. */
#define TOP_MIDDLE 0.80
#define BOT_MIDDLE 0.90
#define COL_MIDDLE 0.90

/* Calculate sum of row r from columns c1 to c2 and again from mc1 to mc2. */
static int rowSum( uint8_t *img, int w, int r, int c1, int c2, int mc1, int mc2 )
{
    int s = 0;
    uint8_t *rp = img + r * w;
    for( int c = c1; c <= c2; ++c )
	s += rp[c];
    for( int c = mc1; c <= mc2; ++c )
	s += rp[c];
    return( s );
}

/* Calculate sum of column c from rows r1 to r2 and again from mr1 to mr2. */
static int colSum( uint8_t *img, int w, int c, int r1, int r2, int mr1, int mr2 )
{
    int s = 0;
    uint8_t *cp = img + r1 * w + c;
    for( int r = r1; r <= r2; ++r ) {
	s += *cp;
	cp += w;
    }
    cp = img + mr1 * w + c;
    for( int r = mr1; r <= mr2; ++r ) {
	s += *cp;
	cp += w;
    }
    return( s );
}

/* For an image of width w and height h, keep the part from (c1,r1) to (c2,r2),
   and pad it with lm, tm, rm, and bm spaces on the respective sides. Perform
   this operation in-place if possible. */
static uint8_t *cropAndPad( uint8_t *img, int w, int h,
			    int c1, int r1, int c2, int r2,
			    int lm, int tm, int rm, int bm,
			    int *wout, int *hout )
{
    int wt = lm + c2 - c1 + 1 + rm;
    int ht = tm + r2 - r1 + 1 + bm;

    uint8_t *res = img;

    /* Allocate new storage for result if trying to work in-place would cause
       any part of the input to be overwritten before it was read. */
    if( wt * ht > w * h
     || r1 * w + c1 < tm * wt
     || r2 * w + c2 < (tm + (r2 - r1 + 1)) * wt )
    {
	res = (uint8_t *) malloc(wt * ht);
	if( res == NULL )
	    Error("failed to allocate storage to pad image");
    }

    uint8_t *ip = img + r1 * w + c1;
    uint8_t *op = res;
    if( tm > 0 ) {
	memset(op,255,tm * wt);
	op += tm * wt;
    }
    for( int r = r1; r <= r2; ++r ) {
	if( lm > 0 ) {
	    memset(op,255,lm);
	    op += lm;
	}
	memmove(op,ip,c2 - c1 + 1);
	ip += w;
	op += c2 - c1 + 1;
	if( rm > 0 ) {
	    memset(op,255,rm);
	    op += rm;
	}
    }
    if( bm > 0 )
	memset(op,255,bm * wt);
    *wout = wt; *hout = ht;
    return( res );
}

uint8_t *Trim( uint8_t *img, int w, int h, int *wout, int *hout )
{
    int c1 = 0, r1 = 0, c2 = w - 1, r2 = h - 1;

    /* Find top margin. */
    int mc1 = (int) (c1 + (c2 - c1) * (1 - TOP_MIDDLE) / 2);
    int mc2 = (int) (c2 - (c2 - c1) * (1 - TOP_MIDDLE) / 2);

    int topThreshold = (int) (TOP_THRESHOLD * (c2 - c1 + 1) * (1 + TOP_MIDDLE));
    for( int r = r1; r <= r2; ++r ) {
	if( rowSum(img,w,r,c1,c2,mc1,mc2) < topThreshold ) {
	    r1 = r;
	    break;
	}
    }

    /* Find bottom margin. */
    mc1 = (int) (c1 + (c2 - c1) * (1 - BOT_MIDDLE) / 2);
    mc2 = (int) (c2 - (c2 - c1) * (1 - BOT_MIDDLE) / 2);

    int botThreshold = (int) (BOT_THRESHOLD * (c2 - c1 + 1) * (1 + BOT_MIDDLE));
    for( int r = r2; r >= r1; --r ) {
	if( rowSum(img,w,r,c1,c2,mc1,mc2) < botThreshold ) {
	    r2 = r;
	    break;
	}
    }

    /* Find left margin. */
    int mr1 = (int) (r1 + (r2 - r1) * (1 - COL_MIDDLE) / 2);
    int mr2 = (int) (r2 - (r2 - r1) * (1 - COL_MIDDLE) / 2);

    int colThreshold = (int) (COL_THRESHOLD * (r2 - r1 + 1) * (1 + COL_MIDDLE));
    for( int c = c1; c <= c2; ++c ) {
	if( colSum(img,w,c,r1,r2,mr1,mr2) < colThreshold ) {
	    c1 = c;
	    break;
	}
    }

    /* Find right margin. */
    for( int c = c2; c >= c1; --c ) {
	if( colSum(img,w,c,r1,r2,mr1,mr2) < colThreshold ) {
	    c2 = c;
	    break;
	}
    }

    /* Back off by a few pixels in each direction in case something small got
       cut off, and also to leave a margin. */
    int tm = 0, bm = 0;
    int vm = (r2 - r1) / 100;
    r1 -= vm; if( r1 < 0 ) { tm = -r1; r1 = 0; }
    r2 += vm; if( r2 > h - 1 ) { bm = r2 - (h - 1); r2 = h - 1; }

    int lm = 0, rm = 0;
    int hm = (c2 - c1) / 50;
    c1 -= hm; if( c1 < 0 ) { lm = -c1; c1 = 0; }
    c2 += hm; if( c2 > w - 1 ) { rm = c2 - (w - 1); c2 = w - 1; }

    return( cropAndPad(img,w,h,c1,r1,c2,r2,lm,tm,rm,bm,wout,hout) );
}

uint8_t *Crop( uint8_t *img, int w, int h, int c1, int r1, int c2, int r2,
	       int *wout, int *hout )
{
    return( cropAndPad(img,w,h,c1,r1,c2,r2,0,0,0,0,wout,hout) );
}

uint8_t *Pad( uint8_t *img, int w, int h, int lm, int tm, int rm, int bm,
	      int *wout, int *hout )
{
    return( cropAndPad(img,w,h,0,0,w-1,h-1,lm,tm,rm,bm,wout,hout) );
}
