############################################################################
#
# Typing the command below   => results in the following being created
#      make               => Fortran and C libraries for the generator,
#                            Timing executibles.
#      make mlfg	  => Library for the Multiplicative LFG
#
# Object files created during the compilation process can be deleted finally
# by typing
#       make clean
############################################################################

SHELL = /bin/sh

include ../../make.CHOICES

LIBDIR = ../../$(LIB_REL_DIR)
SRCDIR = ..
CHKDIR = ../..

include $(SRCDIR)/make.$(PLAT)

all : mlfg.o 

mlfg.o : $(SRCDIR)/interface.h mlfg.c mlfg.h int64.h  $(SRCDIR)/memory.h $(SRCDIR)/store.h $(SRCDIR)/fwrap_.h
	$(CC) -c $(CFLAGS)  $(FFXN) $(INLINEOPT) mlfg.c -I$(SRCDIR)
#---------------------------------------------------------------------------
.F.f : 
	@if [ -f $*.i ] ; then \
	rm $*.i ;\
	fi
	$(CPP) $*.F
	@if [ -f $*.i ] ; then \
	mv $*.i  $*.f ;\
	fi


#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
clean :
	rm -f *.o *.i

realclean :
	rm -f *.o *.f *~ *.i core a.out

.SUFFIXES : 

.SUFFIXES : .f .F
