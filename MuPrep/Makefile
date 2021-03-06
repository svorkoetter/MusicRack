TARGET = muprep
CC = gcc
CCFLAGS = -c -I$$HOME/include -std=c99 -O3 -Wall -Wno-parentheses -Wno-char-subscripts
LD = gcc
LDFLAGS =
LIBS = -lm -ljpeg -lpng -ltiff

$(TARGET): bmpin.o convolution.o cropping.o error.o gamma.o histogram.o \
jpegin.o main.o pngio.o rotation.o scaling.o sharpness.o tiffin.o
	$(LD) $(LDFLAGS) -o $(TARGET) *.o $(LIBS)
	strip $(TARGET)

bmpin.o: bmpin.c error.h imageio.h
	$(CC) $(CCFLAGS) bmpin.c

convolution.o: convolution.c convolution.h error.h
	$(CC) $(CCFLAGS) convolution.c

cropping.o: cropping.c cropping.h error.h
	$(CC) $(CCFLAGS) cropping.c

error.o: error.c error.h
	$(CC) $(CCFLAGS) error.c

gamma.o: gamma.c error.h gamma.h
	$(CC) $(CCFLAGS) gamma.c

histogram.o: histogram.c histogram.h
	$(CC) $(CCFLAGS) histogram.c

jpegin.o: jpegin.c error.h
	$(CC) $(CCFLAGS) jpegin.c

main.o: main.c convolution.h cropping.h error.h gamma.h histogram.h imageio.h \
rotation.h scaling.h sharpness.h
	$(CC) $(CCFLAGS) main.c

pngio.o: pngio.c error.h imageio.h
	$(CC) $(CCFLAGS) pngio.c

rotation.o: rotation.c cropping.h error.h rotation.h
	$(CC) $(CCFLAGS) rotation.c

scaling.o: scaling.c convolution.h error.h scaling.h
	$(CC) $(CCFLAGS) scaling.c

sharpness.o: sharpness.c sharpness.h
	$(CC) $(CCFLAGS) sharpness.c

tiffin.o: tiffin.c error.h imageio.h
	$(CC) $(CCFLAGS) tiffin.c

clean:
	rm -f bmpin.o
	rm -f convolution.o
	rm -f cropping.o
	rm -f error.o
	rm -f gamma.o
	rm -f histogram.o
	rm -f jpegin.o
	rm -f main.o
	rm -f pngio.o
	rm -f rotation.o
	rm -f scaling.o
	rm -f sharpness.o
	rm -f tiffin.o
