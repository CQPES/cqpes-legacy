############################################################################
#
# Then typing the command below   => results in the following being created
#      make               => libsprng.a
#                            Timing and checking executibles.
#
# Object files created during the compilation process can be deleted finally
# by typing
#       make clean
############################################################################

SHELL = /bin/sh
include ../make.CHOICES
LIBDIR = ../$(LIB_REL_DIR)
CHKDIR = ..

##############################################################################
####### The following is the only line in this file that you may modify ######
# The default generator
#RNGDEFAULT = lfg 
LIBLIST = lfg lcg lcg64 cmrg mlfg pmlcg 
##############################################################################


include make.$(PLAT)


#---------------------------------------------------------------------------
# Jump point to sprng
#---------------------------------------------------------------------------
all : 
	(cd sprng; $(MAKE) )
	(p=0 ; \
	for l in $(LIBLIST) ; do \
	./insertmenu "printf(\042" $$l $$p "\134\156\042);" ../EXAMPLES/gen_types_header ../EXAMPLES/gen_types_menu.h ; \
	./insertmenu "print *, \047" $$l $$p "\047" ../EXAMPLES/genf_types_header ../EXAMPLES/genf_types_menu.h ; \
	p=$$(($$p+1)) ; \
	done )

#---------------------------------------------------------------------------
# Create files that all generators need
#---------------------------------------------------------------------------
SPRNG_COMMON_DEPEND = primes_32.o primes_64.o \
			fwrap_mpi.o  cputime.o makeseed.o  store.o\
                    simple_mpi.o memory.o  communicate.o  checkid.o

sprng_common : $(SPRNG_COMMON_DEPEND)

primes_32.o : primes_32.h primes_32.c
	$(CC) -c $(CFLAGS) primes_32.c 

primes_64.o : primes_64.h primes_64.c
	$(CC) -c $(CFLAGS) primes_64.c 

fwrap_mpi.o : fwrap.h fwrap_mpi.c
	$(CC) -c $(CFLAGS) $(FFXN) fwrap_mpi.c

cputime.o : cputime.c fwrap.h
	$(CC) -c $(CFLAGS) $(FFXN) cputime.c

makeseed.o : makeseed.c
	$(CC) -c $(CFLAGS) makeseed.c

simple_mpi.o : simple_mpi.c memory.h interface.h
	$(CC) -c $(CFLAGS) simple_mpi.c

memory.o : memory.c
	$(CC) -c $(CFLAGS) memory.c

communicate.o : communicate.c memory.h
	$(CC) -c $(CFLAGS) communicate.c

checkid.o : checkid.c memory.h
	$(CC) -c $(CFLAGS) checkid.c

store.o : store.c store.h
	$(CC) -c $(CFLAGS) store.c



.SUFFIXES : 

.SUFFIXES : .f .F

#---------------------------------------------------------------------------
.F.f : 
	@if [ -f $*.i ] ; then \
	rm $*.i ;\
	fi
	$(CPP) $(SRCDIR) $*.F
	@if [ -f $*.i ] ; then \
	mv $*.i  $*.f ;\
	fi

#---------------------------------------------------------------------------
clean :
	rm -f *.o *.i
	@for l in $(LIBLIST) sprng;  do \
	  cd $$l ; \
	  $(MAKE) PLAT=$(PLAT) clean ; \
	  cd .. ; \
        done

realclean :
	rm -f *.o *.f *~ *.i core a.out
	rm -f ../EXAMPLES/gen_types_menu.h ../EXAMPLES/genf_types_menu.h
	@for l in $(LIBLIST) sprng; do \
	  cd $$l ; \
	  $(MAKE) PLAT=$(PLAT) realclean ; \
	  cd .. ; \
	  rm -f $(CHKDIR)/$$l.data; \
        done
