/* MusicRack/muprep - JPEG Input - Read a greyscale or RGB JPEG file and
				    return an 8-bit greyscale image in memory.

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
#include <jpeglib.h>

#include "error.h"

static void handleJPEGReadError( j_common_ptr dinfo )
{
    char jpegErrorMsg[JMSG_LENGTH_MAX+1];

    dinfo->err->format_message(dinfo,jpegErrorMsg);
    jpeg_destroy_decompress((struct jpeg_decompress_struct *) dinfo);
    ErrorS("error while reading JPEG file: %s",jpegErrorMsg);
}

const char *GetJPEGVersion( void )
{
    static char buf[15];
    sprintf(buf,"%d.%d", JPEG_LIB_VERSION / 10, JPEG_LIB_VERSION % 10);
    return( buf );
}

uint8_t *ReadJPEG( FILE *file, const char *name, int *wout, int *hout )
{

    /* Initialize the JPEG decompression (i.e. reading) object with an error
       handler that will print a message and quit. */
    struct jpeg_decompress_struct dinfo;
    struct jpeg_error_mgr derr;
    dinfo.err = jpeg_std_error(&derr);
    derr.error_exit = handleJPEGReadError;

    jpeg_create_decompress(&dinfo);

    /* Connect to the file and read the meta information. */
    jpeg_stdio_src(&dinfo,file);
    jpeg_read_header(&dinfo,TRUE);

    int w = dinfo.image_width;
    int h = dinfo.image_height;

    /* Read all images as greyscale. */
    dinfo.out_color_space = JCS_GRAYSCALE;
    jpeg_calc_output_dimensions(&dinfo);

    /* Allocate space for the image. */
    JOCTET *img = (JOCTET *) malloc(w * h * sizeof(JOCTET));
    if( img == NULL )
        Error("failed to allocate storage for JPEG image");

    jpeg_start_decompress(&dinfo);

    for( int r = 0; r < h; ++r ) {
	JOCTET *ptr = img + r * w;
	jpeg_read_scanlines(&dinfo,&ptr,1);
    }

    jpeg_finish_decompress(&dinfo);

    /* Clean up after the read. */
    jpeg_destroy_decompress(&dinfo);

    *wout = w; *hout = h;
    return( (uint8_t *) img );
}
