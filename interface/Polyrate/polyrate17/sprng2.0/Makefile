############################################################################
#
# Then typing the command below   => results in the following being created
#      make               => all
#      make src		  => SPRNG library, libsprng, and checksprng, timesprng
#      make examples	  => SPRNG examples
#      make tests	  => Tests of quality of random streams
#
# Object files created during the compilation process can be deleted finally
# by typing
#       make clean
#
# Object files, executables, and the libraries can be deleted by typing
#       make realclean
############################################################################

SHELL = /bin/sh

include make.CHOICES

LIBDIR = $(LIB_REL_DIR)
SRCDIR = SRC
#DIRS = SRC EXAMPLES TESTS lib
DIRS = SRC lib

include $(SRCDIR)/make.$(PLAT)

#all : src examples tests
all : src 

src :
	(cd SRC; $(MAKE) ; cd ..)

#examples : 
#	(cd EXAMPLES; $(MAKE); cd ..) 

#tests : 
#	(cd TESTS; $(MAKE) ; cd ..)

#---------------------------------------------------------------------------
clean :
	@for l in $(DIRS) ; do \
	  cd $$l ; \
	  $(MAKE) PLAT=$(PLAT) clean ; \
	  cd .. ; \
        done

realclean :
	@for l in $(DIRS) ; do \
	  cd $$l ; \
	  $(MAKE) PLAT=$(PLAT) realclean ; \
	  cd .. ; \
        done
	@rm -f core *~ check* tim* *.data gen*


