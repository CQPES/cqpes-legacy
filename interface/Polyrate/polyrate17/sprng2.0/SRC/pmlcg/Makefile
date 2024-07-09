############################################################################
#
# Typing the command below   => results in the following being created
#      make              => pmlcg.o object
#      make pmlcg.o	 => pmlcg.o object
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

all : pmlcg.o


pmlcg.o : $(SRCDIR)/interface.h pmlcg.c  pmlcg.h $(SRCDIR)/memory.h  basic.h info.h gmp.h longlong.h $(SRCDIR)/store.h $(SRCDIR)/fwrap_.h
	$(CC) -c $(CFLAGS)  $(FFXN) $(INLINEOPT) pmlcg.c -I$(SRCDIR)

clean :
	rm -f *.o *.i

realclean :
	rm -f *.o *.f *~ *.i core a.out

