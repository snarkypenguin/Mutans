# Makefile for 
# Copyright (C) 2016 Randall Gray Oct
# $Header$
# $Log$

CC = gcc
CXX = g++

GSC = gambitc

#CPPFLAGS =
FLAGS = 
CFLAGS = -g
CXXFLAGS = -g

#LDFLAGS =
#LDLIBES =
#LDLIBS = 
SFS = utils.scm units.scm sort.scm maths.scm integrate.scm matrix.scm postscript.scm sort.scm basic-population.scm
SF = $(SFS:=.c)
SFC = $(SFS:.o1.c=.scm)
SFO = $(SFS:.o=.c)

#TEX = latex
#TEXI2DVI = texi2dvi
%.c:	%.scm
	$(gsc) $(FLAGS) -c $<


clean:
	rm -f sclos.o utils.c units.c sort.c maths.c integrate.c matrix.c postscript.c basic-population.c support.o1.c \
		utils.o units.o sort.o maths.o integrate.o matrix.o postscript.o basic-population.o support.o1.o support.o1

stuff:
	gambitc -link -flat -o support.o1.c sclos.scm utils.scm units.scm sort.scm maths.scm integrate.scm matrix.scm postscript.scm basic-population.scm
	gambitc -cc-options -D___DYNAMIC -obj sclos.c utils.c units.c sort.c maths.c integrate.c matrix.c postscript.c basic-population.c support.o1.c
	gcc -shared sclos.o utils.o units.o sort.o maths.o integrate.o matrix.o postscript.o basic-population.o support.o1.o -o support.o1




support-lib.o1.c: $SF
	$(GSC) -link -flat -o $@ $(SF)
	$(GSC) -cc-options -D___DYNAMIC -obj 






