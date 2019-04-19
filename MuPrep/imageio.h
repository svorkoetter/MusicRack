/* MusicRack/muprep - BMP, JPEG, PNG, and TIFF Image I/O

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

#ifndef __MUSIC_RACK_MUPREP_IMAGEIO_H__
#define __MUSIC_RACK_MUPREP_IMAGEIO_H__

/* Read an uncompressed BMP file and return a greyscale image. */
extern uint8_t *ReadBMP( FILE *fp, const char *name, int *wout, int *hout );

/* Return JPEG library version number. */
extern const char *GetJPEGVersion( void );

/* Read any JPEG file and return a greyscale image. */
extern uint8_t *ReadJPEG( FILE *file, const char *name, int *wout, int *hout );

/* Return PNG library version number. */
extern const char *GetPNGVersion( void );

/* Read any PNG file and return a greyscale image. */
extern uint8_t *ReadPNG( FILE *file, const char *name, int *wout, int *hout );

/* Write a greyscale image to a PNG file. */
extern void WritePNG( FILE *file, const char *name, uint8_t *img, int w, int h,
		      int32_t bg );

/* Return TIFF library version number. */
extern const char *GetTIFFVersion( void );

/* Read any TIFF file and return a greyscale image. */
extern uint8_t *ReadTIFF( FILE *file, const char *name, int *wout, int *hout );

/* Convert RGBA to greyscale assuming a white background. Uses weights based on
   modern phosphors, not NTSC phosphors from 1953. */
inline uint8_t RGBAToGrey( uint8_t r, uint8_t g, uint8_t b, uint8_t a )
{
    uint32_t grey = (a * (r * 2125 + g * 7154 + b * 721) + (255 - a) * 2550000)
		  / 2550000;
    return( (uint8_t) grey );
}

#endif
