############################################################################
#
# Typing the command below   => results in the following being created
#      make              => lcg64.o object
#      make lcg64.o	 => lcg64.o object
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

all : lcg64.o


lcg64.o : $(SRCDIR)/interface.h lcg64.c  lcg64.h $(SRCDIR)/memory.h  \
		$(SRCDIR)/primes_64.h  $(SRCDIR)/multiply.h \
		$(SRCDIR)/store.h $(SRCDIR)/fwrap_.h
	$(CC) -c $(CFLAGS) $(FFXN) $(INLINEOPT) -I$(SRCDIR) lcg64.c 

clean :
	rm -f *.o *.i

realclean :
	rm -f *.o *.f *~ *.i core a.out

