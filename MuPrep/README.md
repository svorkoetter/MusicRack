# muprep - A Musical Score Image Preparation Utility

The `muprep` command line utility is used by [MusicRack](../README.md) to
transform and fix up images of pages of musical scores, producing screen-sized
ready-to-diplay images. It's usage is,

    muprep [-v] inputFile geometry outputFile

where:

* The `-v` option produces verbose progress output.
* The input file is a BMP, JPEG, PNG or TIFF image file.
* Geometry is of the form `WxH`, where _W_ is the desired maximum width, and
  _H_ the desired maximum height, both in pixels.
* The output file is a PNG image file.

## System Requirements

On Linux and OS/X systems, the following libraries, or newer versions thereof,
need are used by `muprep`:

* libjpeg
* libpng12-0
* libtiff5
* zlib1g

To rebuild `muprep` from source, you will need the following tools and
libraries, or newer versions thereof:

* GCC 4.9.2
* libjpeg-dev
* libpng12-dev
* libtiff5-dev
* zlib1g-dev

## What it Does

The `muprep` utility performs the following operations when transforming
images:

* Reads the image from the `inputFile`. This can be a BMP (Windows Bitmap),
  JPEG, PNG, or TIFF file.
* Converts the image to 256-level greyscale.
* Computes a histogram of the greyscale levels.
* Normalizes the image, considering any levels within the first percentile to
  be black, and any levels in the last quartile to be white.
* Estimates the sharpness of the image.
* Computes a second histogram, and then finds the largest span of levels that
  contains a small fraction of the pixels. The size of that fraction depends on
  the sharpness computed previously.
* Re-normalizes the image to that span. At this point, all pixels that are
  supposed to be black or white really will be.
* Does an initial trimming of the image, removing all surrounding white space,
  and skipping over small blotches like dirt specks and page numbers. The image
  is then also padded to leave consistent margins around the edge.
* Determines how badly the image is skewed. Rotations of up to about 2 degrees
  can be reliably determined and corrected.
* Converts from 8-bit gamma corrected (linear perceived intensity) to 16-bit
  linear intensity before performing any spatial transformations (see
  <http://ericbrasseur.org/gamma.html> for the reasons).
* If the image was skewed by more than one pixel in its final intended size,
  rotates it appropriately. After rotation the image was rotated in the
  previous step, it is retrimmed and/or padded.
* Scales the image to fit within the specified geometry, and applies a bit of
  sharpening.
* Converts from 16-bit linear back to 8-bit gamma corrected intensity.
* Writes out the image in colour mapped PNG form, with a light off-white
  ([Solarized](https://ethanschoonover.com/solarized/) Base3) background colour
  for reduced eye strain.

All of this is done completely automatically, with no user interaction.
