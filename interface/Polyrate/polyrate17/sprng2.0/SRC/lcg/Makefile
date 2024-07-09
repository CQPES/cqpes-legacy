############################################################################
#
# Typing the command below   => results in the following being created
#      make              => lcg.o object
#      make lcg.o	 => lcg.o object
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

all : lcg.o 


lcg.o : $(SRCDIR)/interface.h lcg.c  lcg.h $(SRCDIR)/memory.h  \
		$(SRCDIR)/primes_32.h  $(SRCDIR)/multiply.h \
		$(SRCDIR)/store.h $(SRCDIR)/fwrap_.h
	$(CC) -c $(CFLAGS)  $(FFXN) $(INLINEOPT) lcg.c -I$(SRCDIR)
clean :
	rm -f *.o *.i

realclean :
	rm -f *.o *.f *~ *.i core a.out

