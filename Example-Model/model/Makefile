# Makefile for 
# Copyright (C) 2016 Randall Gray Oct
# $Header$
# $Log$

DEBUGGING = -track-scheme -debug-location -debug-source -debug-environments
GSC = gambitc -f  $(DEBUGGING)


CC = gcc
CXX = g++


#CPPFLAGS =
FLAGS = 
CFLAGS = -g
CXXFLAGS = -g

#LDFLAGS =
#LDLIBES =
#LDLIBS = 


# These *must not* be put into the rewrite group else they get overwritten.  YOU HAVE BEEN WARNED!
SCLOS = preamble.scm sort.scm sclos.scm 
KERNEL = utils.scm kernel.scm

# BEGIN REWRITE GROUP
SUPPORT = units.scm maths.scm integrate.scm matrix.scm postscript.scm basic-population.scm
CLASSDECS = framework.scm framework-declarations.scm declarations.scm 
CHASSIS = framework-wrappers.scm framework-methods.scm
LOG = log-methods.scm
LANDSCAPE =  landscape-methods.scm
PLANT = plant-methods.scm
ANIMAL = animal-methods.scm
# END REWRITE GROUP

all:	sclos.o1 support.o1 classdecs.o1 chassis.o1 log.o1 landscape.o1 plant.o1 animal.o1 kernel.o1
	sed -ne '/# BEGIN REWRITE GROUP/,/# END REWRITE GROUP/p' -e '/# END REWRITE GROUP/q' < Makefile | egrep -v '^#' | tr '[A-Z]' '[a-z]' > rewrite
	gsi rebuild-load-sequence.scm
	rm rewrite


checks:
	./run-checks

sclos.o1.o:	$(SCLOS)
	$(GSC) -link -flat -o sclos.o1.c $(SCLOS) > /dev/null
	$(GSC) -cc-options -D___DYNAMIC -obj $(SCLOS:.scm=.c) sclos.o1.c > /dev/null
#	$(CC) -shared sclos.o1.o $(SCLOS:.scm=.o) -o sclos.o1 > /dev/null

support.o1.o: $(SUPPORT)
	$(GSC) -link -flat -o support.o1.c $(SUPPORT)
	$(GSC) -cc-options -D___DYNAMIC -obj $(SUPPORT:.scm=.c) support.o1.c
#	$(CC) -shared support.o1.o $(SUPPORT:.scm=.o) -o support.o1

classdecs.o1.o: $(CLASSDECS)
	$(GSC) -link -flat -o classdecs.o1.c $(CLASSDECS)
	$(GSC) -cc-options -D___DYNAMIC -obj $(CLASSDECS:.scm=.c) classdecs.o1.c
#	$(CC) -shared $(CLASSDECS:.scm=.o) classdecs.o1.o -o classdecs.o1

chassis.o1.o: $(CHASSIS)
	$(GSC) -link -flat -o chassis.o1.c $(CHASSIS)
	$(GSC) -cc-options -D___DYNAMIC -obj $(CHASSIS:.scm=.c) chassis.o1.c
#	$(CC) -shared $(CHASSIS:.scm=.o) chassis.o1.o -o chassis.o1

log.o1.o:
	$(GSC) -link -flat -o log.o1.c $(LOG)
	$(GSC) -cc-options -D___DYNAMIC -obj $(LOG:.scm=.c) log.o1.c
#	$(CC) -shared $(LOG:.scm=.o) log.o1.o -o log.o1

landscape.o1.o:
	$(GSC) -link -flat -o landscape.o1.c $(LANDSCAPE)
	$(GSC) -cc-options -D___DYNAMIC -obj $(LANDSCAPE:.scm=.c) landscape.o1.c
#	$(CC) -shared $(LANDSCAPE:.scm=.o) landscape.o1.o -o landscape.o1


plant.o1.o:
	$(GSC) -link -flat -o plant.o1.c $(PLANT)
	$(GSC) -cc-options -D___DYNAMIC -obj $(PLANT:.scm=.c) plant.o1.c
#	$(CC) -shared $(PLANT:.scm=.o) plant.o1.o -o plant.o1

animal.o1.o:
	$(GSC) -link -flat -o animal.o1.c $(ANIMAL)
	$(GSC) -cc-options -D___DYNAMIC -obj $(ANIMAL:.scm=.c) animal.o1.c
#	$(CC) -shared $(ANIMAL:.scm=.o) animal.o1.o -o animal.o1

kernel.o1.o:
	$(GSC) -link -flat -o kernel.o1.c $(KERNEL)
	$(GSC) -cc-options -D___DYNAMIC -obj $(KERNEL:.scm=.c) kernel.o1.c
#	$(CC) -shared $(KERNEL:.scm=.o) kernel.o1.o -o kernel.o1

framework.o1: sclos.o1.o support.o1.o chassis.o1.o classdecs.o1.o log.o1.o landscap1.o1.o plant.o1.o animal.o1.o kernel.o1.o
	$(CC) -shared $(KERNEL:.scm=.o) 

clean:
	rm *.o

Clean:
	rm -v *.o *.o1

done:
	say The compilation has finished.
