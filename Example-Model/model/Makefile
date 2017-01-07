# Makefile for 
# Copyright (C) 2016 Randall Gray Oct
# $Header$
# $Log$

#DEBUGGING = -track-scheme -debug-location -debug-source -debug-environments
#DEBUGGING = -track-scheme -debug
DEBUGGING = 
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

SCLOS = preamble.scm sort.scm sclos.scm utils.scm sclos-extn.scm framework-declarations.scm framework.scm 
KERNEL = kdnl.scm kernel.scm

DECLARATIONS = landscape-declarations.scm diffeq-declarations.scm animal-declarations.scm plant-declarations.scm monitor-declarations.scm

DECS = declarations.scm $(DECLARATIONS)

# BEGIN REWRITE GROUP
SUPPORT = units.scm constants.scm maths.scm integrate.scm matrix.scm postscript.scm 
CLASSDECS = framework-classes.scm introspection-classes.scm monitor-classes.scm log-classes.scm diffeq-classes.scm landscape-classes.scm plant-classes.scm animal-classes.scm 
CHASSIS = framework-wrappers.scm framework-methods.scm declarations.scm
INTROSPECTION =  introspection-methods.scm monitor-methods.scm log-methods.scm
DIFFEQ = basic-population.scm diffeq-methods.scm
LANDSCAPE =  landscape-methods.scm
PLANT = plant-methods.scm
ANIMAL = animal-methods.scm
RING = tree-ring.scm
# END REWRITE GROUP

ALLFILES = $(SCLOS) $(KERNEL) $(SUPPORT) $(CLASSDECS) $(DECS) $(CHASSIS) $(INTROSPECTION) $(DIFFEQ) $(LANDSCAPE) $(PLANT) $(ANIMAL) $(RING)

all: checks model
	say Done

model:	sclos.o1 support.o1 kernel.o1 classdecs.o1 chassis.o1 mon-log.o1 diffeq.o1 landscape.o1 plant.o1 animal.o1
	sed -ne '/# BEGIN REWRITE GROUP/,/# END REWRITE GROUP/p' -e '/# END REWRITE GROUP/q' < Makefile | egrep -v '^#' | tr '[A-Z]' '[a-z]' > rewrite
	rm rewrite

check: checks linecount-nc

# This is to minimise the risk of multiple generics
.PHONY: declarations.scm
declarations.scm: $(DECLARATIONS) 
	touch declarations.scm 
	mv declarations.scm .declarations.scm;
	touch declarations.scm 
	cat $(DECS) .declarations.scm | sort -k2 | uniq | egrep -v -e '^;'  > declarations.scm
	@echo ';;; Local Variables:' >> declarations.scm 
	@echo ';;; mode: scheme' >> declarations.scm
	@echo ';;; outline-regexp: ";-+"' >> declarations.scm
	@echo ';;; comment-column:0' >> declarations.scm
	@echo ';;; comment-start: ";;; "' >> declarations.scm
	@echo ';;; comment-end:"" ' >> declarations.scm
	@echo ';;; End:' >> declarations.scm



checks: linecount-nc
	@./run-checks

verbose-checks: linecount-nc
	@./run-verbose-checks 

linecount:
	@echo -n 'The number of lines of code is approximately: '
	@cat $(ALLFILES) | wc -l

linecount-nc:
	@echo -n 'The number of lines of code is approximately: '
	@grep '' $(ALLFILES) | fgrep -v ':;' | wc -l


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

kernel.o1.o: $(KERNEL)
	$(GSC) -link -flat -o kernel.o1.c $(KERNEL)
	$(GSC) -cc-options -D___DYNAMIC -obj $(KERNEL:.scm=.c) kernel.o1.c

kernel.o1: kernel.o1.o
	$(CC) -shared $(KERNEL:.scm=.o) kernel.o1.o -o kernel.o1

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

mon-log.o1.o: $(INTROSPECTION)
	$(GSC) -link -flat -o mon-log.o1.c $(INTROSPECTION)
	$(GSC) -cc-options -D___DYNAMIC -obj $(INTROSPECTION:.scm=.c) mon-log.o1.c

mon-log.o1: mon-log.o1.o
	$(CC) -shared $(INTROSPECTION:.scm=.o) mon-log.o1.o -o mon-log.o1

diffeq.o1.o: $(DIFFEQ)
	$(GSC) -link -flat -o diffeq.o1.c $(DIFFEQ)
	$(GSC) -cc-options -D___DYNAMIC -obj $(DIFFEQ:.scm=.c) diffeq.o1.c

diffeq.o1: diffeq.o1.o 
	$(CC) -shared $(DIFFEQ:.scm=.o) diffeq.o1.o -o diffeq.o1

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


framework.o1: sclos.o1.o support.o1.o chassis.o1.o classdecs.o1.o mon-log.o1.o diffeq.o1.o landscape.o1.o plant.o1.o animal.o1.o kernel.o1.o
	$(CC) -shared $(KERNEL:.scm=.o) 

clean:
	rm -fv *.o $(SCLOS:.scm=.c) *.c model-bolus.scm


veryclean: clean
	rm -fv *.o1 

done:
	say The compilation has finished.
