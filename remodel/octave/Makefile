#
# Makefile for octave module
# Pierre-Alexandre Fournier
# fournier@carretechnologies.com
# Copyright 2007-2008
#

GSC=gsc
GSCFLAGS=-warnings
VERSION=0.6

all: octave.o1

octave.o1: octave.scm
	${GSC} ${GSCFLAGS} octave.scm

tarball:
	(cd ..;									\
	tar cvvf scheme-octave-${VERSION}.tar octave/octave.scm			\
	octave/Makefile octave/octave.html octave/screenshots.html		\
	octave/bar.png octave/mesh.png octave/rose.png octave/simple.png	\
	octave/README octave/octave.license;					\
	gzip --verbose scheme-octave-${VERSION}.tar;				\
	mv -v scheme-octave-${VERSION}.tar.gz octave/ ;)

clean: 
	rm -vf *.o*
