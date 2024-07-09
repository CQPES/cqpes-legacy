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


#---------------------------------------------------------------------------
# Create executables to time the generator
#---------------------------------------------------------------------------
timesprng_exec : timesprng_c_exec timesprng_f_exec
	@chmod u+x $(CHKDIR)/timesprng


timesprng_c_exec : $(CHKDIR)/timingc$(TIMESPRNG_LIB)
	(p=0 ; \
	for l in $(LIBLIST) ; do \
	./inserttimecheck $$l $(CHKDIR)/timesprng "Timing C interface:" timingc$(TIMESPRNG_LIB) $(CHKDIR)/timingc$(TIMESPRNG_LIB) $$p ; \
	p=$$(($$p+1)) ; \
	done; )

timesprng_f_exec : $(CHKDIR)/timingf$(TIMESPRNG_LIB)
	(p=0 ; \
	for l in $(LIBLIST) ; do \
	./inserttimecheck $$l $(CHKDIR)/timesprng "Timing FORTRAN interface:" timingf$(TIMESPRNG_LIB) $(CHKDIR)/timingf$(TIMESPRNG_LIB) $$p ; \
	p=$$(($$p+1)) ; \
	done; )

$(CHKDIR)/timingc$(TIMESPRNG_LIB) : timing.o $(LIBDIR)/lib$(TIMESPRNG_LIB).a
	$(CLD) $(CLDFLAGS) -o $(CHKDIR)/timingc$(TIMESPRNG_LIB) timing.o -L$(LIBDIR) -l$(TIMESPRNG_LIB) $(GMPLIB)


$(CHKDIR)/timingf$(TIMESPRNG_LIB) : timingf.o  $(LIBDIR)/lib$(TIMESPRNG_LIB).a
	$(F77LD) $(F77LDFLAGS) -o $(CHKDIR)/timingf$(TIMESPRNG_LIB) timingf.o -L$(LIBDIR) -l$(TIMESPRNG_LIB) $(GMPLIB)


timing.o : timing.c cputime.o
	$(CC) -c $(CFLAGS) timing.c

drand : drand.c cputime.h cputime.o
	$(CC) $(CFLAGS) -o drand drand.c cputime.o

timingf.o : timingf.$(FSUFFIX)
	$(F77) -c $(FFLAGS) timingf.$(FSUFFIX)

#$(LIBDIR)/lib$(TIMESPRNG_LIB).a : $(TIMESPRNG_LIB)/$(TIMESPRNG_LIB).c
#		(cd $(TIMESPRNG_LIB); $(MAKE) $(TIMESPRNG_LIB))


#---------------------------------------------------------------------------
# Create executables to check whether the generator is ported correctly
#---------------------------------------------------------------------------
checksprng_exec : checksprng_c_exec checksprng_f_exec
	@chmod u+x $(CHKDIR)/checksprng


checksprng_c_exec : $(CHKDIR)/check.c$(CHECKSPRNG_LIB) \
		    $(CHKDIR)/check_sim.c$(CHECKSPRNG_LIB) \
		    $(CHKDIR)/check_ptr.c$(CHECKSPRNG_LIB)
	(p=0 ; \
	for l in $(LIBLIST) ; do \
	./insertcheck  $$l $(CHKDIR)/checksprng "Checking Default C interface:" check.c$(CHECKSPRNG_LIB) $(CHKDIR)/check.c$(CHECKSPRNG_LIB) $$p ; \
	./insertcheck  $$l $(CHKDIR)/checksprng "Checking Simple C interface:" check_sim.c$(CHECKSPRNG_LIB) $(CHKDIR)/check_sim.c$(CHECKSPRNG_LIB) $$p ; \
	./insertcheck  $$l $(CHKDIR)/checksprng "Checking C interface with pointer checking:" check_ptr.c$(CHECKSPRNG_LIB) $(CHKDIR)/check_ptr.c$(CHECKSPRNG_LIB) $$p ; \
	p=$$(($$p+1)) ; \
	done; )

checksprng_f_exec : $(CHKDIR)/check.f$(CHECKSPRNG_LIB) \
		    $(CHKDIR)/check_sim.f$(CHECKSPRNG_LIB) \
		    $(CHKDIR)/check_ptr.f$(CHECKSPRNG_LIB)
	(p=0 ; \
	for l in $(LIBLIST) ; do \
	./insertcheck  $$l $(CHKDIR)/checksprng "Checking Default FORTRAN interface:" check.f$(CHECKSPRNG_LIB) $(CHKDIR)/check.f$(CHECKSPRNG_LIB) $$p ; \
	./insertcheck  $$l $(CHKDIR)/checksprng "Checking Simple FORTRAN interface:" check_sim.f$(CHECKSPRNG_LIB) $(CHKDIR)/check_sim.f$(CHECKSPRNG_LIB) $$p ; \
	./insertcheck  $$l $(CHKDIR)/checksprng "Checking FORTRAN interface with pointer checking:" check_ptr.f$(CHECKSPRNG_LIB) $(CHKDIR)/check_ptr.f$(CHECKSPRNG_LIB) $$p ; \
	p=$$(($$p+1)) ; \
	done; )

$(CHKDIR)/check.c$(CHECKSPRNG_LIB) : check_gen.o $(LIBDIR)/lib$(CHECKSPRNG_LIB).a
	$(CLD) $(CLDFLAGS) -o $(CHKDIR)/check.c$(CHECKSPRNG_LIB) check_gen.o \
		-L$(LIBDIR) -l$(CHECKSPRNG_LIB) $(GMPLIB)

$(CHKDIR)/check_sim.c$(CHECKSPRNG_LIB) : check_gen_simple.o $(LIBDIR)/lib$(CHECKSPRNG_LIB).a
	$(CLD) $(CLDFLAGS) -o $(CHKDIR)/check_sim.c$(CHECKSPRNG_LIB) check_gen_simple.o \
		-L$(LIBDIR) -l$(CHECKSPRNG_LIB) $(GMPLIB)

$(CHKDIR)/check_ptr.c$(CHECKSPRNG_LIB) : check_gen_ptr.o $(LIBDIR)/lib$(CHECKSPRNG_LIB).a
	$(CLD) $(CLDFLAGS) -o $(CHKDIR)/check_ptr.c$(CHECKSPRNG_LIB) check_gen_ptr.o \
		-L$(LIBDIR) -l$(CHECKSPRNG_LIB) $(GMPLIB)

$(CHKDIR)/check.f$(CHECKSPRNG_LIB) : check_genf.o $(LIBDIR)/lib$(CHECKSPRNG_LIB).a
	$(F77LD) $(F77LDFLAGS) -o $(CHKDIR)/check.f$(CHECKSPRNG_LIB) check_genf.o \
		-L$(LIBDIR) -l$(CHECKSPRNG_LIB) $(GMPLIB)

$(CHKDIR)/check_sim.f$(CHECKSPRNG_LIB) : check_genf_simple.o $(LIBDIR)/lib$(CHECKSPRNG_LIB).a
	$(F77LD) $(F77LDFLAGS) -o $(CHKDIR)/check_sim.f$(CHECKSPRNG_LIB) check_genf_simple.o \
		-L$(LIBDIR) -l$(CHECKSPRNG_LIB) $(GMPLIB)

$(CHKDIR)/check_ptr.f$(CHECKSPRNG_LIB) : check_genf_ptr.o $(LIBDIR)/lib$(CHECKSPRNG_LIB).a
	$(F77LD) $(F77LDFLAGS) -o $(CHKDIR)/check_ptr.f$(CHECKSPRNG_LIB) check_genf_ptr.o \
		-L$(LIBDIR) -l$(CHECKSPRNG_LIB) $(GMPLIB)

#$(LIBDIR)/lib$(CHECKSPRNG_LIB).a : $(CHECKSPRNG_LIB)/$(CHECKSPRNG_LIB).c
		(cd $(CHECKSPRNG_LIB); $(MAKE) $(CHECKSPRNG_LIB))

check_gen.o : check_gen.c sprng.h
	$(CC) -c $(CFLAGS) check_gen.c

check_gen_simple.o : check_gen_simple.c sprng.h
	$(CC) -c $(CFLAGS) check_gen_simple.c

check_gen_ptr.o : check_gen_ptr.c sprng.h
	$(CC) -c $(CFLAGS) check_gen_ptr.c

check_genf.o : check_genf.$(FSUFFIX) sprng_f.h
	$(F77) -c $(FFLAGS) check_genf.$(FSUFFIX)

check_genf_simple.o : check_genf_simple.$(FSUFFIX) sprng_f.h
	$(F77) -c $(FFLAGS) check_genf_simple.$(FSUFFIX)

check_genf_ptr.o : check_genf_ptr.$(FSUFFIX) sprng_f.h
	$(F77) -c $(FFLAGS) check_genf_ptr.$(FSUFFIX)



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
