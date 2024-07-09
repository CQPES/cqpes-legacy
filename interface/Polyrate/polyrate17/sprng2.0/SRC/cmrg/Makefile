############################################################################
#
# Typing the command below   => results in the following being created
#      make              => cmrg.o object
#      make cmrg.o	 => cmrg.o object
#
# Object files created during the compilation process can be deleted finally
# by typing
#       make clean
#       make realclean
############################################################################

SHELL = /bin/sh

include ../../make.CHOICES

LIBDIR = ../../$(LIB_REL_DIR)
SRCDIR = ..
CHKDIR = ../..

include $(SRCDIR)/make.$(PLAT)

all : cmrg.o


cmrg.o : $(SRCDIR)/interface.h cmrg.c  cmrg.h $(SRCDIR)/memory.h  \
		$(SRCDIR)/primes_64.h  $(SRCDIR)/multiply.h \
		$(SRCDIR)/store.h $(SRCDIR)/fwrap_.h
	$(CC) -c $(CFLAGS) $(FFXN) $(INLINEOPT)  cmrg.c -I$(SRCDIR)

clean :
	rm -f *.o *.i

realclean :
	rm -f *.o *.f *~ *.i core a.out

