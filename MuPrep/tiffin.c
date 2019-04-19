/* MusicRack/muprep - TIFF Input - Read a monochrome, greyscale, indexed
				    colour, or RGB TIFF file and return an
				    8-bit greyscale image in memory.

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
#include <ctype.h>
#include <tiffio.h>

#include "error.h"
#include "imageio.h"

#define ERR_BUF_LEN 1024

static void handleTIFFError( const char *module, const char *fmt, va_list ap )
{
    char bigBuf[1024], *p = bigBuf; 
    size_t bufSize = sizeof(bigBuf), space = bufSize;

    if( module ) {
	snprintf(p,space,"[%s] ",module);
	p += strlen(bigBuf);
	space -= (p - bigBuf);
    }
    if( space > 0 )
	vsnprintf(p,space,fmt,ap);
    bigBuf[bufSize-1] = '\0';
    ErrorS("error while reading TIFF file: %s",bigBuf);
}

static void handleTIFFWarning( const char *module, const char *fmt, va_list ap )
{
    if( module )
        fprintf(stderr,"[%s] ",module);
    vfprintf(stderr,fmt,ap);
}

const char *GetTIFFVersion( void )
{
    const char *p = TIFFGetVersion();
    static char buf[15];
    while( *p != '\n' && *p != '\0' && !isdigit(*p) )
        ++p;
    int i = 0;
    while( i < sizeof(buf) - 1 && (isdigit(*p) || *p == '.') )
        buf[i++] = *p++;
    buf[i] = '\0';
    return( buf );
}

uint8_t *ReadTIFF( FILE *file, const char *name, int *wout, int *hout )
{
    /* Connect to the file and read the meta information. */
    TIFFSetErrorHandler(handleTIFFError);
    TIFFSetWarningHandler(handleTIFFWarning);
    TIFF *tif = TIFFOpen(name,"rm");

    uint32_t w, h;
    TIFFGetField(tif,TIFFTAG_IMAGEWIDTH,&w);
    TIFFGetField(tif,TIFFTAG_IMAGELENGTH,&h);

    /* Read the image using TIFFReadRGBAImage, because that takes care of a lot
       of things automatically, like converting different formats into one
       common format, but unfortunately not the desired greyscale. That means
       an intermediate buffer into which to read the raw image is needed. */
    uint32_t *buffer = (uint32_t *) malloc(w * h * sizeof(uint32_t));
    if( buffer == NULL )
        Error("failed to allocate buffer for TIFF image");
    TIFFReadRGBAImage(tif,w,h,buffer,0);

    /* Allocate a buffer for the final image. */
    uint8_t *img = (uint8_t *) malloc(w * h * sizeof(uint8_t));
    if( img == NULL )
        Error("failed to allocate storage for TIFF image");

    /* Convert image to 8-bit greyscale and put it the right way up. */
    uint32_t *bp = buffer + (h - 1) * w;
    uint8_t *ip = img;
    for( int r = 0; r < h; ++r ) {
	for( int c = 0; c < w; ++c ) {
	    uint32_t rgba = bp[c];
	    uint8_t red = rgba & 0xFF;
	    uint8_t green = (rgba >> 8) & 0xFF;
	    uint8_t blue = (rgba >> 16) & 0xFF;
	    uint8_t alpha = (rgba >> 24) & 0xFF;
	    *ip++ = RGBAToGrey(red,green,blue,alpha);
	}
	bp -= w;
    }

    TIFFClose(tif);

    /* Delete the temporary buffer we created. */
    free(buffer);

    *wout = w; *hout = h;
    return( img );
}
