############################################################################
#
# Typing the command below   => results in the following being created
#      make               => Fortran and C libraries for the generator,
#                            Timing executibles.
#      make sprng		  => Library for the Linear Congruential Generator
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
GENLIST = lfg lcg lcg64 cmrg mlfg pmlcg

include $(SRCDIR)/make.$(PLAT)

#all : sprng timesprng checksprng
all : sprng 


#---------------------------------------------------------------------------
# Construct Library
#---------------------------------------------------------------------------
SPRNG_COMMON_DEPEND = $(SRCDIR)/primes_32.o $(SRCDIR)/primes_64.o \
			  $(SRCDIR)/fwrap_mpi.o  $(SRCDIR)/cputime.o \
		      $(SRCDIR)/makeseed.o  $(SRCDIR)/simple_mpi.o \
		      $(SRCDIR)/memory.o  $(SRCDIR)/communicate.o  \
			  $(SRCDIR)/checkid.o $(SRCDIR)/store.o

sprng :
	(cd ..; $(MAKE) sprng_common)
	@for g in $(GENLIST) ; do \
		(cd ../$$g; $(MAKE) ) ; \
	done
	rm -f $(LIBDIR)/libsprng.a
	$(MAKE) $(LIBDIR)/libsprng.a

$(LIBDIR)/libsprng.a : sprng.o   \
		$(SRCDIR)/lcg/lcg.o $(SRCDIR)/lfg/lfg.o \
		$(SRCDIR)/lcg64/lcg64.o \
		$(SRCDIR)/mlfg/mlfg.o $(SRCDIR)/cmrg/cmrg.o \
		$(SRCDIR)/pmlcg/pmlcg.o \
		$(SPRNG_COMMON_DEPEND)
	$(AR) $(ARFLAGS) $(LIBDIR)/libsprng.a sprng.o \
		$(SRCDIR)/lcg/lcg.o $(SRCDIR)/lfg/lfg.o \
		$(SRCDIR)/lcg64/lcg64.o \
		$(SRCDIR)/mlfg/mlfg.o $(SRCDIR)/cmrg/cmrg.o \
		$(SRCDIR)/pmlcg/pmlcg.o \
		$(SPRNG_COMMON_DEPEND)
	$(RANLIB) $(LIBDIR)/libsprng.a

sprng.o : $(SRCDIR)/interface.h sprng.c   $(SRCDIR)/memory.h  \
	$(SRCDIR)/multiply.h $(SRCDIR)/store.h $(SRCDIR)/fwrap_.h
	$(CC) -c $(CFLAGS)  $(FFXN) $(INLINEOPT) sprng.c -I$(SRCDIR)


#---------------------------------------------------------------------------
# Time the generator
#---------------------------------------------------------------------------
#timesprng :
#	(cd ..; $(MAKE) timesprng_exec TIMESPRNG_LIB=sprng)


#---------------------------------------------------------------------------
# Check portability of generator
#---------------------------------------------------------------------------
#checksprng :
#	(cd ..; $(MAKE) checksprng_exec CHECKSPRNG_LIB=sprng)


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
