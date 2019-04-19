/* MusicRack/muprep - Main Program

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
#include <time.h>

#include <zlib.h>

#include "convolution.h"
#include "cropping.h"
#include "error.h"
#include "gamma.h"
#include "histogram.h"
#include "imageio.h"
#include "rotation.h"
#include "scaling.h"
#include "sharpness.h"

int main( int argc, const char **argv )
{
    int i = 1;
    bool verbose = false;

    while( i < argc && *argv[i] == '-' ) {
	if( strcmp(argv[i],"-v") == 0 )
	    verbose = true;
	else
	    i = argc;
	++i;
    }
    
    if( i + 2 >= argc )
        Error("Usage: muprep [-v] infile geometry outfile\n"
	      "-v\tverbose; show progress and timing information");

    const char *inFile = argv[i];
    const char *geometry = argv[i+1];
    const char *outFile = argv[i+2];

    int w2 = 0, h2 = 0;
    if( sscanf(geometry,"%dx%d",&w2,&h2) != 2 )
        Error("geometry should be of the form WxH");

    const char *p = inFile, *q = "";
    while( *p != '\0' )
        if( *p++ == '.' ) q = p;

    clock_t start = clock(), prev = start, curr;

    FILE *fp = fopen(inFile,"r");
    if( fp == NULL )
        ErrorS("unable to open input file \"%s\" for reading",inFile);

    int w = 0, h = 0;
    uint8_t *img = NULL;

    if( strcmp(q,"bmp") == 0 )
        img = ReadBMP(fp,inFile,&w,&h);
    else if( strcmp(q,"jpg") == 0 || strcmp(q,"jpeg") == 0 )
        img = ReadJPEG(fp,inFile,&w,&h);
    else if( strcmp(q,"png") == 0 )
        img = ReadPNG(fp,inFile,&w,&h);
    else if( strcmp(q,"tif") == 0 || strcmp(q,"tiff") == 0 )
        img = ReadTIFF(fp,inFile,&w,&h);

    fclose(fp);
    if( img == NULL )
        ErrorS("unable to determine type of input file \"%s\"",inFile);

    if( verbose ) {
	printf("libjpeg %s, ",GetJPEGVersion());
	printf("libpng %s, ",GetPNGVersion());
	printf("libtiff %s, ",GetTIFFVersion());
	printf("zlib %s\n",ZLIB_VERSION);
	curr = clock();
        printf("%1.3fs read %s %d x %d\n",
	       ((double) (curr - prev)) / CLOCKS_PER_SEC, inFile, w, h);
	prev = curr;
    }

    /* Compute an image histogram, find the levels corresponding to the 1st and
       75th percentile of the pixels, and then expand the values between that
       range to use the full 0-255 range. This is a pre-expansion so that later
       heuristics will work properly. */
    struct Histogram hist;
    ComputeHistogram(img,w,h,&hist);
    int i1 = FindPercentile(&hist,0.01);
    int i2 = FindPercentile(&hist,0.75);

    if( verbose ) {
        curr = clock();
	printf("%1.3fs histogram %d - %d\n",
	       ((double) (curr - prev)) / CLOCKS_PER_SEC, i1, i2);
	prev = curr;
    }

    ExpandHistogram(img,w,h,&hist,i1,i2);

    if( verbose ) {
        curr = clock();
	printf("%1.3fs normalize\n", ((double) (curr - prev)) / CLOCKS_PER_SEC);
	prev = curr;
    }

    /* Find the longest contiguous sequence of buckets containing a small
       fraction of all the pixels. The sharper the image, the fewer pixels
       wanted. The goal is to find the unused range of levels between the
       notation (black) and the paper (white) while preserving most of the
       intermediate levels used for edges:

           Notation                          Paper

           #                                   ###
           ##                                  ###
           ##                                  ###
           ##                                 ####
           ###.                             .#####
           #######################################
             |--------- span to find --------|

       After finding the span, expand the histogram so that everything outside
       of it is black or white. */
    double sh = EstimateSharpness(img,w,h);

    if( verbose ) {
        curr = clock();
	printf("%1.3fs sharpness %1.3f\n",
	       ((double) (curr - prev)) / CLOCKS_PER_SEC, sh);
	prev = curr;
    }

    FindHistogramSpan(&hist,0.025/sh,&i1,&i2);

    if( verbose ) {
        curr = clock();
	printf("%1.3fs span %d - %d\n",
	       ((double) (curr - prev)) / CLOCKS_PER_SEC, i1, i2);
	prev = curr;
    }

    ExpandHistogram(img,w,h,&hist,i1,i2);

    if( verbose ) {
        curr = clock();
	printf("%1.3fs expand\n", ((double) (curr - prev)) / CLOCKS_PER_SEC);
	prev = curr;
    }

    /* Trim whitespace off all four sides and add consistent margins. */
    uint8_t *img2 = Trim(img,w,h,&w,&h);
    if( img2 != img ) {
	free(img);
        img = img2;
    }

    if( verbose ) {
	curr = clock();
        printf("%1.3fs trim %d x %d\n",
	       ((double) (curr - prev)) / CLOCKS_PER_SEC, w, h);
	prev = curr;
    }

    /* Calculate how far the image is rotated by measuring skew in the staff
       lines. This will work for a maximum of about 3 degrees. */
    int m1l = (25*w)/100, m1r = (35*w)/100;
    int m2l = (80*w)/100, m2r = (90*w)/100;
    int mt = (12*h)/100, mb = (85*h)/100;
    uint8_t *med1 = RowMedians(img,w,h,m1l,mt,m1r,mb);
    uint8_t *med2 = RowMedians(img,w,h,m2l,mt,m2r,mb);
    int skew = EstimateSkew(med1,med2,mb-mt+1,(m2l-m1l)/19);

    if( verbose ) {
	curr = clock();
        printf("%1.3fs skew %1.3f deg (%d/%d)\n",
	       ((double) (curr - prev)) / CLOCKS_PER_SEC,
	       atan2(skew,m2l-m1l) * 57.29577951, skew, m2l - m1l);
	prev = curr;
    }

    /* Convert from gamma corrected (linear perceived intensity) to linear
       intensity before scaling. See http://ericbrasseur.org/gamma.html */
    uint16_t *img16 = ToLinear(img,w,h);
    free(img);

    if( verbose ) {
	curr = clock();
	printf("%1.3fs linear\n",
	       ((double) (curr - prev)) / CLOCKS_PER_SEC);
	prev = curr;
    }

    /* If the image would be skewed by more than one pixel in its final size,
       fix that now before scaling. Don't deskew tall narrow images (lyrics),
       since this algorithm only works reliably on music. */
    uint16_t *img2_16;
    if( h < 2 * w && (abs(skew) * h2) / (m2l - m1l) > 1 ) {
        img2_16 = Rotate(img16,w,h,skew,m2l - m1l);
	if( img2_16 != img16 ) {
	    free(img16);
	    img16 = img2_16;
	}

	if( verbose ) {
	    curr = clock();
	    printf("%1.3fs rotate %d x %d\n",
		   ((double) (curr - prev)) / CLOCKS_PER_SEC, w, h);
	    prev = curr;
	}

	/* Redo trimming and padding after rotation. This requires temporarily
	   converting back to 8-bit gamma corrected form. */
	img = ToGamma(img16,w,h);

	uint8_t *img2 = Trim(img,w,h,&w,&h);
	if( img2 != img ) {
	    free(img);
	    img = img2;
	}

	img16 = ToLinear(img,w,h);
	free(img);

	if( verbose ) {
	    curr = clock();
	    printf("%1.3fs retrim %d x %d\n",
		   ((double) (curr - prev)) / CLOCKS_PER_SEC, w, h);
	    prev = curr;
	}
    }

    /* Determine which dimension is more constrained by the target size and
       adjust the other one accordingly. */
    if( (h * w2) / w > h2 )
        w2 = (w * h2) / h;
    else
        h2 = (h * w2) / w;
    double sf = sqrt((1.0 * h * w) / (h2 * w2));

    /* Scale the image to the target size. */
    img2_16 = Scale(img16,w,h,w2,h2);
    if( img2_16 != img16 ) {
	free(img16);
        img16 = img2_16;
    }
    w = w2; h = h2;

    if( verbose ) {
	curr = clock();
        printf("%1.3fs scale %d x %d (1/%1.3f)\n",
	       ((double) (curr - prev)) / CLOCKS_PER_SEC, w, h, sf);
	prev = curr;
    }

    /* Apply a little bit of sharpening after scaling. */
    int sharpen[9] = { -1, -2, -1, -2, 40, -2, -1, -2, -1 };
    img2_16 = Convolve3x3(img16,w,h,sharpen);
    free(img16);
    img16 = img2_16;

    if( verbose ) {
        curr = clock();
	printf("%1.3fs sharpen\n", ((double) (curr - prev)) / CLOCKS_PER_SEC);
	prev = curr;
    }

    /* Convert back to gamma corrected intensity for the final image. */
    img = ToGamma(img16,w,h);

    if( verbose ) {
	curr = clock();
	printf("%1.3fs gamma\n",
	       ((double) (curr - prev)) / CLOCKS_PER_SEC);
	prev = curr;
    }

    /* Write the result to the specified 8-bit colour mapped PNG output file
       with a soft-white background. */
    if( (fp = fopen(outFile,"w")) == NULL )
        ErrorS("unable to open output file \"%s\" for writing",outFile);
    WritePNG(fp,outFile,img,w,h,0xFDF6E3);
    fclose(fp);

    if( verbose ) {
	curr = clock();
        printf("%1.3fs write %s\n",
	       ((double) (curr - prev)) / CLOCKS_PER_SEC, outFile);
        printf("%1.3fs total\n", ((double) (curr - start)) / CLOCKS_PER_SEC);
    }

    free(img);

    return( 0 );
}
