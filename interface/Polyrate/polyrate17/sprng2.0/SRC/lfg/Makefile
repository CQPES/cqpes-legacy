############################################################################
#
# Typing the command below   => results in the following being created
#      make              => lfg.o object
#      make lfg.o	 => lfg.o object
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

all : lfg.o 

lfg.o : $(SRCDIR)/interface.h lfg.c  lfg.h $(SRCDIR)/multiply.h  $(SRCDIR)/memory.h $(SRCDIR)/store.h $(SRCDIR)/fwrap_.h
	$(CC) -c $(CFLAGS)  $(FFXN) $(INLINEOPT) lfg.c -I$(SRCDIR)

clean :
	rm -f *.o *.i

realclean :
	rm -f *.o *.f *~ *.i core a.out

