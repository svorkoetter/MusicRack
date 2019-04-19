/* MusicRack/muprep - PNG I/O - Read a monochrome, greyscale, indexed colour,
				 or RGB PNG file and return an 8-bit greyscale
				 image in memory. Write an indexed colour image
				 to a PNG file.

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
#include <png.h>

#include "error.h"
#include "imageio.h"

static void handlePNGReadError( png_structp png, png_const_charp msg )
{
    ErrorS("error while reading PNG file: %s\n",msg);
}

static void handlePNGReadWarning( png_structp png, png_const_charp msg )
{
    fprintf(stderr,"warning while reading PNG file: %s\n",msg);
}

static void readPNGData( png_structp png, png_bytep data,
				 png_size_t count )
{
    FILE *fp = (FILE *) png_get_io_ptr(png);
    if( fread(data,sizeof(png_byte),count,fp) != count )
	handlePNGReadError(png,"read error or end of file");
}

const char *GetPNGVersion( void )
{
    static char buf[15];
    uint32_t ver = png_access_version_number();
    sprintf(buf,"%d.%d.%d", ver/10000, (ver/100)%100, ver%100);
    return( buf );
}

uint8_t *ReadPNG( FILE *file, const char *name, int *wout, int *hout )
{
    /* Create the PNG read and information structures. */
    png_structp png = png_create_read_struct(PNG_LIBPNG_VER_STRING,
    					     (void *) file,
    					     handlePNGReadError,
					     handlePNGReadWarning);
    if( png == NULL )
        Error("unable to create PNG read structure");

    png_infop info = png_create_info_struct(png);
    if( info == NULL ) {
        png_destroy_read_struct(&png,NULL,NULL);
	Error("unable to create PNG information structure");
    }

    png_infop endInfo = png_create_info_struct(png);
    if( endInfo == NULL ) {
        png_destroy_read_struct(&png,&info,NULL);
	Error("unable to create PNG end information structure");
    }

    /* Since we're lazy and just abort on error, this setjmp call should never
       actually return non-zero. */
    if( setjmp(png_jmpbuf(png)) != 0 ) {
        png_destroy_read_struct(&png,&info,&endInfo);
	exit(1);
    }

    /* Initialize I/O. */
    png_set_read_fn(png,(void *) file,readPNGData);
    png_set_sig_bytes(png,0);
    png_set_keep_unknown_chunks(png,PNG_HANDLE_CHUNK_NEVER,0,0);

    /* Read the PNG information (dimensions, depth, etc.). */
    png_read_info(png,info);
    int w = png_get_image_width(png,info);
    int h = png_get_image_height(png,info);

    /* Downsample to 8-bits per channel. */
    png_byte bitDepth = png_get_bit_depth(png,info);
    if( bitDepth > 8 )
        png_set_strip_16(png);

    /* Transform paletted images to RGB. */
    png_byte colorType = png_get_color_type(png,info);
    if( colorType == PNG_COLOR_TYPE_PALETTE )
        png_set_palette_to_rgb(png);

    /* Convert to 8-bits for 1-, 2-, and 4-bit greyscale. */
    if( colorType == PNG_COLOR_TYPE_GRAY && bitDepth < 8 )
        png_set_expand_gray_1_2_4_to_8(png);

    /* Update information that may have changed due to the transformations. */
    png_read_update_info(png,info);
    colorType = png_get_color_type(png,info);
    size_t rowBytes = png_get_rowbytes(png,info);

    /* Allocate a block of memory into which to read the image. */
    png_bytep img = (png_bytep) malloc(h * rowBytes);
    if( img == NULL ) {
        png_destroy_read_struct(&png,&info,&endInfo);
        Error("failed to allocate storage for PNG image");
    }

    /* Allocate another block of memory to hold pointers to the rows in the
       block above. */
    png_bytepp rowPtrs = (png_bytepp) malloc(h * sizeof(png_bytep));
    if( rowPtrs == NULL ) {
	free(img);
        png_destroy_read_struct(&png,&info,&endInfo);
        Error("failed to allocate row pointers for PNG image");
    }

    /* Set up the row pointers to point into the image rows. */
    for( int r = 0; r < h; ++r )
        rowPtrs[r] = (png_bytep) img + rowBytes * r;

    /* Read the image into memory, automatically deinterlacing if necessary. */
    png_read_image(png,rowPtrs);

    png_read_end(png,endInfo);
    png_destroy_read_struct(&png,&info,&endInfo);

    free(rowPtrs);

    /* Convert to greyscale in-place. */
    uint8_t *in = img, *out = img;
    switch( colorType ) {
    case PNG_COLOR_TYPE_RGB_ALPHA:
        for( int r = 0; r < h; ++r ) {
	    for( int c = 0; c < w; ++c ) {
	        uint8_t red = *in++;
		uint8_t green = *in++;
		uint8_t blue = *in++;
		uint8_t alpha = *in++;
		*out++ = RGBAToGrey(red,green,blue,alpha);
	    }
	}
	break;

    case PNG_COLOR_TYPE_RGB:
        for( int r = 0; r < h; ++r ) {
	    for( int c = 0; c < w; ++c ) {
	        uint8_t red = *in++;
		uint8_t green = *in++;
		uint8_t blue = *in++;
		*out++ = RGBAToGrey(red,green,blue,255);
	    }
	}
	break;

    case PNG_COLOR_TYPE_GRAY_ALPHA:
        for( int r = 0; r < h; ++r ) {
	    for( int c = 0; c < w; ++c ) {
	        uint8_t grey = *in++;
		uint8_t alpha = *in++;
		*out++ = RGBAToGrey(grey,grey,grey,alpha);
	    }
	}
	break;

    case PNG_COLOR_TYPE_GRAY:
        break;

    default:
        ErrorI("unexpected PNG color type %d after transformation",colorType);
    }

    *wout = w; *hout = h;
    return( img );
}

static void handlePNGWriteError( png_structp png, png_const_charp msg )
{
    ErrorS("error while writing PNG file: %s\n",msg);
}

static void handlePNGWriteWarning( png_structp png, png_const_charp msg )
{
    fprintf(stderr,"warning while writing PNG file: %s\n",msg);
}

static void writePNGData( png_structp png, png_bytep data,
				  png_size_t count )
{
    FILE *fp = (FILE *) png_get_io_ptr(png);
    if( fwrite(data,sizeof(png_byte),count,fp) != count )
	handlePNGWriteError(png,"write error");
}

static void flushPNGData( png_structp png )
{
    (void) png;
}

void WritePNG( FILE *file, const char *name, uint8_t *img, int w, int h,
	       int32_t bg )
{
    /* Create the PNG write and information structures. */
    png_structp png = png_create_write_struct(PNG_LIBPNG_VER_STRING,
    					      (void *) file,
    					      handlePNGWriteError,
					      handlePNGWriteWarning);
    if( png == NULL )
        Error("unable to create PNG write structure");

    png_infop info = png_create_info_struct(png);
    if( info == NULL ) {
        png_destroy_write_struct(&png,NULL);
	Error("unable to create PNG information structure");
    }

    /* Since we're lazy and just abort on error, this setjmp call should never
       actually return non-zero. */
    if( setjmp(png_jmpbuf(png)) != 0 ) {
        png_destroy_write_struct(&png,&info);
	exit(1);
    }

    png_set_write_fn(png,(void *) file,writePNGData,flushPNGData);

    /* Set PNG information. */
    png_set_IHDR(png, info, (png_uint_32) w, (png_uint_32) h, 8,
		 bg < 0 ? PNG_COLOR_TYPE_GRAY : PNG_COLOR_TYPE_PALETTE,
		 PNG_INTERLACE_NONE,
		 PNG_COMPRESSION_TYPE_DEFAULT, PNG_FILTER_TYPE_DEFAULT);

    /* Create a colour palette if a background colour was specified. */
    png_colorp palette = NULL;
    if( bg >= 0 ) {
	uint8_t red = (uint8_t) (bg >> 16),
		green = (uint8_t)(bg >> 8),
		blue = (uint8_t) bg;
	palette = (png_colorp) png_malloc(png, 256 * sizeof(png_color));
	for( int i = 0; i < 256; ++i ) {
	    palette[i].red = (red * i) / 255;
	    palette[i].green = (green * i) / 255;
	    palette[i].blue = (blue * i) / 255;
	}
	png_set_PLTE(png,info,palette,256);
    }

    /* Write out all pre-image data. */
    png_write_info(png,info);

    /* Write out the image. */
    for( int r = 0; r < h; ++r ) {
	png_write_row(png,img);
	img += w;
    }

    /* Finish up. */
    png_write_end(png,NULL);
    if( palette != NULL ) free(palette);
    png_destroy_write_struct(&png,&info);
}
