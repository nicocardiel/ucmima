#!/bin/csh
#------------------------------------------------------------------------------
# Version 11-February-2002
#------------------------------------------------------------------------------
# To install ucmima properly, you must follow these steps:
# make include
# make ucmima
# make clean
#------------------------------------------------------------------------------
# define here maximum image dimensions
# NXMAX must be the image size in the X direction
# NYMAX must be the image size in the Y direction
NXMAX   = 2200
NYMAX   = 2200
# verify that the required libraries are properly set 
#---------------------------------------------------------------(Linux version)
#PGPDIR  = /usr/local/pgplot
#X11DIR  = /usr/X11R6/lib
#FIODIR  = /usr/local/cfitsio
#FCOMPIL = g77 -O3 -g -Wall
#CCOMPIL = gcc -O3 -g -Wall
#------------------------------------------------------------(Mac OS X version)
PGPDIR  = /opt/local/var/macports/software/pgplot/5.2.2_5/opt/local/lib
X11DIR  = /opt/local/var/macports/software/xorg-libX11/1.3.3_0/opt/local/lib
FIODIR  = /opt/local/var/macports/software/cfitsio/3.240_0/opt/local/lib
FCOMPIL = g95 -O3 -g -Wall
CCOMPIL = gcc -O3 -g -Wall
#------------------------------------------------------------------------------
# Nothing SHOULD be modified below this comment line
#------------------------------------------------------------------------------
# macro definitions
FSOURCE = button.f buttqbr.f buttqcf.f buttqch.f buttqex.f \
          buttqit.f buttqpr.f buttqxb.f buttqyb.f buttqytext.f buttsbr.f \
          buttscf.f buttsch.f buttsex.f buttsit.f buttspr.f buttsxb.f \
          buttsyb.f buttsytext.f ifbutton.f rpgband.f rpgbegin.f rpgbegok.f \
          rpgenv.f rpgeras.f rpgerasb.f rpgerasw.f \
          iofunctions.f \
          fmean0.f \
          fmedian1.f \
          ordena1f.f \
          ucmima.f
FOBJECT = button.o buttqbr.o buttqcf.o buttqch.o buttqex.o \
          buttqit.o buttqpr.o buttqxb.o buttqyb.o buttqytext.o buttsbr.o \
          buttscf.o buttsch.o buttsex.o buttsit.o buttspr.o buttsxb.o \
          buttsyb.o buttsytext.o ifbutton.o rpgband.o rpgbegin.o rpgbegok.o \
          rpgenv.o rpgeras.o rpgerasb.o rpgerasw.o \
          iofunctions.o \
          fmean0.o \
          fmedian1.o \
          ordena1f.o \
          ucmima.o
CSOURCE = systemfunction_.c
COBJECT = systemfunction_.o
# Default rule to create program
ucmima:  $(FOBJECT) $(COBJECT)
#	$(FCOMPIL) -o $@ $(FOBJECT) $(COBJECT) -L$(PGPDIR) -L$(FIODIR) -L$(X11DIR) -lpgplot -lcfitsio -lX11 -lnsl -lsocket
	$(FCOMPIL) -o $@ $(FOBJECT) $(COBJECT) -L$(PGPDIR) -L$(FIODIR) -L$(X11DIR) -lpgplot -lcfitsio -lX11
# Target to clean object modules
clean:    $(FOBJECT) $(COBJECT)
	rm -f $(FOBJECT) $(COBJECT)
# Target to touch source modules
touch:
	touch $(FSOURCE) $(CSOURCE)
# Target to create the file dimensions.inc
include:
	rm -f dimensions.inc
	echo "        INTEGER NXMAX" > dimensions.inc
	echo "        PARAMETER (NXMAX=$(NXMAX))" >> dimensions.inc
	echo "        INTEGER NYMAX" >> dimensions.inc
	echo "        PARAMETER (NYMAX=$(NYMAX))" >> dimensions.inc
	touch $(FSOURCE) $(CSOURCE)
# second level dependencies
.f.o: $(FSOURCE)
	$(FCOMPIL) -c $?
.c.o: $(CSOURCE)
	$(CCOMPIL) -c $?
# definitions
.PRECIOUS: ucmima
