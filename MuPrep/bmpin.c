/* MusicRack/muprep - BMP Input - Read a monochrome, greyscale, indexed
				   colour, or RGB BMP file and return an 8-bit
				   greyscale image in memory.

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

#include "error.h"
#include "imageio.h"

static int readByte( FILE *fp )
{
    int c = fgetc(fp);
    if( c == EOF )
	Error("unexpected end of BMP file");
    return( c & 0xFF );
}

static uint16_t readLittleEndianWORD( FILE *fp )
{
    uint16_t r = 0;

    r = readByte(fp);
    r |= readByte(fp) << 8;

    return( r );
}

static uint32_t readLittleEndianDWORD( FILE *fp )
{
    uint32_t r = 0;

    r = readByte(fp);
    r |= readByte(fp) << 8;
    r |= readByte(fp) << 16;
    r |= readByte(fp) << 24;

    return( r );
}

uint8_t *ReadBMP( FILE *fp, const char *name, int *wout, int *hout )
{
    /* Bitmap File Header - 14 bytes */
    if( readLittleEndianWORD(fp) != ('B' + ('M' << 8)) )
	ErrorS("\"%s\" does not appear to be a BMP file",name);

    readLittleEndianDWORD(fp);	/* Skip file size. */
    readLittleEndianWORD(fp);	/* Skip two reserved 16-bit fields. */
    readLittleEndianWORD(fp);
    uint32_t offsetToImage = readLittleEndianDWORD(fp);

    /* DIB Header - headerSize bytes */
    uint32_t headerSize = readLittleEndianDWORD(fp);
    if( headerSize < 40 )
	ErrorI("BMP header size %d is not supported",(int) headerSize); 

    uint32_t w = readLittleEndianDWORD(fp);
    uint32_t h = readLittleEndianDWORD(fp);

    readLittleEndianWORD(fp);	/* Skip number of colour panes (always 1). */

    uint16_t bitsPerPixel = readLittleEndianWORD(fp);
    if( bitsPerPixel != 1 && bitsPerPixel != 4 && bitsPerPixel != 8
     && bitsPerPixel != 16 && bitsPerPixel != 24 && bitsPerPixel != 32 )
	ErrorI("bits per pixel must be 1, 4, 8, 16, 24, or 32, but file has %d",
	       bitsPerPixel);
    uint16_t extraBytesPerRow = (4 - ((w * bitsPerPixel + 7) / 8) % 4) % 4;

    uint32_t compression = readLittleEndianDWORD(fp);
    if( compression != 0 && compression != 3 )
        ErrorI("compressed (type %d) BMP files are not supported",compression);

    readLittleEndianDWORD(fp);	/* Skip image size. */
    readLittleEndianDWORD(fp);	/* Skip horizontal resolution (pixels/m). */
    readLittleEndianDWORD(fp);	/* Skip vertical resolution. */

    uint32_t numColours = readLittleEndianDWORD(fp);
    if( bitsPerPixel <= 8 && numColours == 0 )
        numColours = 1 << bitsPerPixel;

    readLittleEndianDWORD(fp);	/* Skip number of important colours. */

    size_t pos = 40;  /* Current position in header. */

    /* Determine channel masks and shifts for 16-bit and 32-bit formats. */
    uint32_t amask = 0xFF000000, rmask = 0xFF0000, gmask = 0xFF00, bmask = 0xFF;
    if( compression == 3 ) {
	if( bitsPerPixel != 16 && bitsPerPixel != 32 )
	    ErrorI("compressed (type 3) BMP files are not supported for %d-bit images",bitsPerPixel);
        rmask = readLittleEndianDWORD(fp);
        gmask = readLittleEndianDWORD(fp);
	bmask = readLittleEndianDWORD(fp);
	amask = ~(rmask|gmask|bmask);
	pos += 12;
    }
    else if( bitsPerPixel == 16 ) {
        rmask = 0x1F << 10;
	gmask = 0x1F << 5;
	bmask = 0x1F;
    }
    int ashift = 0, rshift = 0, gshift = 0, bshift = 0;
    if( amask > 0 )
        while( ((amask >> ashift) & 0x01) == 0 )
	     ++ashift;
    if( rmask > 0 )
        while( ((rmask >> rshift) & 0x01) == 0 )
	     ++rshift;
    if( gmask > 0 )
        while( ((gmask >> gshift) & 0x01) == 0 )
	     ++gshift;
    if( bmask > 0 )
        while( ((bmask >> bshift) & 0x01) == 0 )
	     ++bshift;
    uint32_t amax = amask >> ashift, rmax = rmask >> rshift,
    	     gmax = gmask >> gshift, bmax = bmask >> bshift;

    /* Skip remainder of header. */
    while( pos++ < headerSize )
        readByte(fp);

    /* Read the colour map and convert the entries directly to grey scale. */
    uint8_t red, green, blue, alpha, *colourMap = NULL;
    int grey;
    if( numColours > 0 ) {
	if( bitsPerPixel > 8 ) {
	    /* Skip (advisory) colour table for non-indexed-colour images. */
	    for( uint32_t i = 0; i < numColours; ++i )
	        readLittleEndianDWORD(fp);
	}
	else if( numColours > (1 << bitsPerPixel) )
	    ErrorI("BMP colour map contains too many entries (%d)",numColours);
	else {
	    colourMap = (uint8_t *) malloc(numColours * sizeof(uint8_t));
	    if( colourMap == NULL )
		Error("failed to allocate storage for BMP colour map");
	    for( uint32_t i = 0; i < numColours; ++i ) {
		blue = readByte(fp);
		green = readByte(fp);
		red = readByte(fp);
		colourMap[i] = RGBAToGrey(red,green,blue,255);
		readByte(fp);  /* Skip reserved byte (should be 0). */
	    }
	}
    }

    /* Skip to where the image is. Most of the time, we'll already be there. */
    uint32_t offset = 14 + headerSize + numColours * 4;
    if( offset > offsetToImage )
        Error("BMP header information is larger than offset to image");
    while( offset++ < offsetToImage )
	readByte(fp);

    /* Allocate space for the image. */
    uint8_t *img = (uint8_t *) malloc(w * h * sizeof(uint8_t));
    if( img == NULL )
        Error("failed to allocate storage for BMP image");

    /* Read the image data, one row at a time, from bottom to top. */
    for( int r = h-1; r >= 0; --r ) {
	int rw = r * w;
	uint8_t b = 0, index; 
	uint32_t rgba;
        for( int c = 0; c < w; ++c ) {
	    switch( bitsPerPixel ) {
	    case 1:
		if( c & 0x07 == 0 )
		    b = readByte(fp);
		index = (b >> (~c & 0x07)) & 0x01;
		grey = colourMap[index];
		break;
	    case 4:
		if( c & 0x01 == 0 ) {
		    b = readByte(fp);
		    index = (b >> 4) & 0x0F;
		}
		else
		    index = b & 0x0F;
		grey = colourMap[index];
		break;
	    case 8:
		index = readByte(fp);
		grey = colourMap[index];
		break;
	    case 16:
		rgba = readLittleEndianWORD(fp);
		blue = (((rgba & bmask) >> bshift) * 255) / bmax;
		green = (((rgba & gmask) >> gshift) * 255) / gmax;
		red = (((rgba & rmask) >> rshift) * 255) / rmax;
		grey = RGBAToGrey(red,green,blue,255);
		break;
	    case 24:
		blue = readByte(fp);
		green = readByte(fp);
		red = readByte(fp);
		grey = RGBAToGrey(red,green,blue,255);
		break;
	    case 32:
		rgba = readLittleEndianDWORD(fp);
		blue = (((rgba & bmask) >> bshift) * 255) / bmax;
		green = (((rgba & gmask) >> gshift) * 255) / gmax;
		red = (((rgba & rmask) >> rshift) * 255) / rmax;
		alpha = (((rgba & amask) >> ashift) * 255) / amax;
		grey = RGBAToGrey(red,green,blue,alpha);
		break;
	    default:
		grey = 0;
		ErrorI("internal error while reading BMP file (%d)",bitsPerPixel);
	    }

	    img[rw + c] = grey;
	}
	for( int c = 0; c < extraBytesPerRow; ++c )
	    readByte(fp);
    }

    if( colourMap != NULL )
        free(colourMap);

    *wout = w; *hout = h;
    return( img );
}
