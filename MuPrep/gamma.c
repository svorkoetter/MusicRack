/* MusicRack/muprep - Image Gamma Conversion

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

#include "gamma.h"
#include "error.h"

/* Don't use the proper sRGB gamma correction (nor simple x^2.2 either), as it
   makes lines to light in the output when scaling music. Uncomment the line
   below to use the proper formula. */
// #define USE_SRGB_GAMMA

uint16_t *ToLinear( uint8_t *img, int w, int h )
{
    static bool first = true;
    static uint16_t map[256];

    if( first ) {
	for( int i = 0; i < 256; ++i ) {
	    double signal = i / 255.0;
#ifdef USE_SRGB_GAMMA
	    double power = signal < 0.04045
			 ? signal / 12.92
			 : pow((signal + 0.055)/(1.055),2.4);
#else
	    double power = pow(signal,1.6);
#endif
	    map[i] = power * 65535 + 0.5;
	}
	first = false;
    }

    uint16_t *res = (uint16_t *) malloc(w * h * sizeof(uint16_t));
    if( res == NULL )
        Error("failed to allocate storage for linear greyscale image");

    uint16_t *rp = res;
    for( int i = w * h; i > 0; --i )
	*rp++ = map[*img++];

    return( res );
}

uint8_t *ToGamma( uint16_t *img, int w, int h )
{
    static bool first = true;
    static uint8_t map[65536];

    if( first ) {
	for( int i = 0; i < 65536; ++i ) {
	    double power = i / 65535.0;
#ifdef USE_SRGB_GAMMA
	    double signal = power < 0.0031308
			  ? 12.92 * power
			  : -0.055 + 1.055 * pow(power,5./12);
#else
	    double signal = pow(power,0.625);
#endif
	    map[i] = signal * 255 + 0.5;
	}
    }

    uint8_t *res = (uint8_t *) img;

    uint8_t *rp = res;
    for( int i = w * h; i > 0; --i )
	*rp++ = map[*img++];

    return( res );
}
