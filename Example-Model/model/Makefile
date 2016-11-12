# Makefile for 
# Copyright (C) 2016 Randall Gray Oct
# $Header$
# $Log$

#DEBUGGING = -track-scheme -debug-location -debug-source -debug-environments
DEBUGGING = -track-scheme -debug
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
KERNEL = utils.scm kdnl.scm kernel.scm

# BEGIN REWRITE GROUP
SUPPORT = units.scm constants.scm maths.scm integrate.scm matrix.scm postscript.scm basic-population.scm
CLASSDECS = framework.scm framework-classes.scm framework-declarations.scm declarations.scm
CHASSIS = log-classes.scm landscape-classes.scm plant-classes.scm animal-classes.scm framework-wrappers.scm framework-methods.scm 
LOG = log-methods.scm
LANDSCAPE =  landscape-methods.scm
PLANT = plant-methods.scm
ANIMAL = animal-methods.scm
# END REWRITE GROUP

ALLFILES = $(SCLOS) $(KERNEL) $(SUPPORT) $(CLASSDECS) $(CHASSIS) $(LOG) $(LANDSCAPE) $(PLANT) $(ANIMAL)

all: checks model

model:	sclos.o1 support.o1 classdecs.o1 chassis.o1 log.o1 landscape.o1 plant.o1 animal.o1 kernel.o1
	sed -ne '/# BEGIN REWRITE GROUP/,/# END REWRITE GROUP/p' -e '/# END REWRITE GROUP/q' < Makefile | egrep -v '^#' | tr '[A-Z]' '[a-z]' > rewrite
	rm rewrite

checks:
	./run-checks

#make-load-wrappers:
#	gsi rebuild-load-sequence.scm

## This is *really* slow, fails to compile and is useful for getting rid of multiple declarations
one-big-hunk:
	./make-bolus

sclos.o1.o:	$(SCLOS)
	$(GSC) -link -flat -o sclos.o1.c $(SCLOS) > /dev/null
	$(GSC) -cc-options -D___DYNAMIC -obj $(SCLOS:.scm=.c) sclos.o1.c > /dev/null

sclos.o1:	sclos.o1.o
	$(CC) -shared sclos.o1.o $(SCLOS:.scm=.o) -o sclos.o1 > /dev/null

support.o1.o: $(SUPPORT)
	$(GSC) -link -flat -o support.o1.c $(SUPPORT)
	$(GSC) -cc-options -D___DYNAMIC -obj $(SUPPORT:.scm=.c) support.o1.c

support.o1: support.o1.o
	$(CC) -shared support.o1.o $(SUPPORT:.scm=.o) -o support.o1

classdecs.o1.o: $(CLASSDECS)
	$(GSC) -link -flat -o classdecs.o1.c $(CLASSDECS)
	$(GSC) -cc-options -D___DYNAMIC -obj $(CLASSDECS:.scm=.c) classdecs.o1.c

classdecs.o1: classdecs.o1.o
	$(CC) -shared $(CLASSDECS:.scm=.o) classdecs.o1.o -o classdecs.o1

chassis.o1.o: $(CHASSIS)
	$(GSC) -link -flat -o chassis.o1.c $(CHASSIS)
	$(GSC) -cc-options -D___DYNAMIC -obj $(CHASSIS:.scm=.c) chassis.o1.c

chassis.o1: chassis.o1.o
	$(CC) -shared $(CHASSIS:.scm=.o) chassis.o1.o -o chassis.o1

log.o1.o: $(LOG)
	$(GSC) -link -flat -o log.o1.c $(LOG)
	$(GSC) -cc-options -D___DYNAMIC -obj $(LOG:.scm=.c) log.o1.c

log.o1: log.o1.o
	$(CC) -shared $(LOG:.scm=.o) log.o1.o -o log.o1


landscape.o1.o: $(LANDSCAPE)
	$(GSC) -link -flat -o landscape.o1.c $(LANDSCAPE)
	$(GSC) -cc-options -D___DYNAMIC -obj $(LANDSCAPE:.scm=.c) landscape.o1.c

landscape.o1: landscape.o1.o 
	$(CC) -shared $(LANDSCAPE:.scm=.o) landscape.o1.o -o landscape.o1



plant.o1.o: $(PLANT)
	$(GSC) -link -flat -o plant.o1.c $(PLANT)
	$(GSC) -cc-options -D___DYNAMIC -obj $(PLANT:.scm=.c) plant.o1.c

plant.o1: plant.o1.o
	$(CC) -shared $(PLANT:.scm=.o) plant.o1.o -o plant.o1


animal.o1.o: $(ANIMAL)
	$(GSC) -link -flat -o animal.o1.c $(ANIMAL)
	$(GSC) -cc-options -D___DYNAMIC -obj $(ANIMAL:.scm=.c) animal.o1.c

animal.o1: animal.o1.o
	$(CC) -shared $(ANIMAL:.scm=.o) animal.o1.o -o animal.o1


kernel.o1.o: $(KERNEL)
	$(GSC) -link -flat -o kernel.o1.c $(KERNEL)
	$(GSC) -cc-options -D___DYNAMIC -obj $(KERNEL:.scm=.c) kernel.o1.c

kernel.o1: kernel.o1.o
	$(CC) -shared $(KERNEL:.scm=.o) kernel.o1.o -o kernel.o1


framework.o1: sclos.o1.o support.o1.o chassis.o1.o classdecs.o1.o log.o1.o landscap1.o1.o plant.o1.o animal.o1.o kernel.o1.o
	$(CC) -shared $(KERNEL:.scm=.o) 

clean:
	rm -fv *.o $(SCLOS:.scm=.c) model-bolus.scm


veryclean: clean
	rm -fv *.o1 

done:
	say The compilation has finished.
