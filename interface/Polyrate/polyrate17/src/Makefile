F77C    = /opt/intel/oneapi/compiler/2022.2.0/linux/bin/intel64/ifort -O -save 
CC      = /opt/intel/oneapi/compiler/2022.2.0/linux/bin/intel64/icc
VERSION =17
VPATH = ../poten    
# executables that can be built from this makefile

DUMSEXE   = ../exe/polyrate$(VERSION).dum.serial.exe

CH5SEXE   = ../exe/polyrate$(VERSION).ch5.serial.exe  
CLHBRSEXE = ../exe/polyrate$(VERSION).clhbr.serial.exe
CH4OSEXE  = ../exe/polyrate$(VERSION).ch4o.serial.exe 
CMCSEXE   = ../exe/polyrate$(VERSION).cmc.serial.exe  
CWMCSEXE  = ../exe/polyrate$(VERSION).cwmc.serial.exe 
NH3SEXE   = ../exe/polyrate$(VERSION).nh3.serial.exe  
H2NISEXE  = ../exe/polyrate$(VERSION).h2ni.serial.exe 
H3SEXE    = ../exe/polyrate$(VERSION).h3.serial.exe   
HNISEXE   = ../exe/polyrate$(VERSION).hni.serial.exe  
OH3SEXE   = ../exe/polyrate$(VERSION).oh3.serial.exe 
OHCLSEXE  = ../exe/polyrate$(VERSION).ohcl.serial.exe
HO2SEXE   = ../exe/polyrate$(VERSION).ho2.serial.exe
#User may add new executables here
CH4SEXE   = ../exe/polyrate$(VERSION).ch4.serial.exe

%.o: %.F
	$(F77C) -c $<
%.o: %.f90
	$(F77C) -c $<
%.o: %.c
	$(CC) -c $<


OBJ = aamod.o acespoly.o alloc.o dattim.o givtst.o intbsv3.o poly40.o \
polyrr.o headr.o interface.o polysz.o ef.o hooks.o ivtstm.o polyag.o rtpjac.o \
energetics.o intbsv1.o main.o polyhl.o fromblas.o intbsv2.o poly31.o \
polymq.o util.o torsion.o fromlapack.o

poly_mpi.o: poly_mpi.F
	$(MPIF77C) -c poly_mpi.F
ch3.o: ../poten/ch3.F
	$(MPIF77C) -c ../poten/ch3.F
ch4o.o: ../poten/ch4o_module.f90 ../poten/ch4o.F
	$(F77C) -c ../poten/ch4o_module.f90 ../poten/ch4o.F
ch5.o: ../poten/ch5.F
	$(F77C) -c ../poten/ch5.F
chain3.o: ../poten/chain3.F
	$(F77C) -c ../poten/chain3.F
chain4.o: ../poten/chain4.F
	$(F77C) -c ../poten/chain4.F
clhbr.o: ../poten/clhbr.F
	$(F77C) -c ../poten/clhbr.F
cmc.o: ../poten/cmc.F
	$(F77C) -c ../poten/cmc.F
coord3.o: ../poten/coord3.F
	$(F77C) -c ../poten/coord3.F
coord4.o: ../poten/coord4.F
	$(F77C) -c ../poten/coord4.F
cwmc.o: ../poten/cwmc.F
	$(F77C) -c ../poten/cwmc.F
dumpot.o: ../poten/dumpot.F
	$(F77C) -c ../poten/dumpot.F
h2ni.o: ../poten/h2ni.F
	$(F77C) -c ../poten/h2ni.F
h3.o: ../poten/h3.F
	$(F77C) -c ../poten/h3.F
hni.o: ../poten/hni.F
	$(F77C) -c ../poten/hni.F
ho2.o: ../poten/ho2.F
	$(F77C) -c ../poten/ho2.F
nh3.o: ../poten/nh3.F
	$(F77C) -c ../poten/nh3.F
oh3.o: ../poten/oh3.F
	$(F77C) -c ../poten/oh3.F
ohcl.o: ../poten/ohcl.F
	$(F77C) -c ../poten/ohcl.F
setup3.o: ../poten/setup3.F
	$(F77C) -c ../poten/setup3.F
setup4.o: ../poten/setup4.F
	$(F77C) -c ../poten/setup4.F
surf3.o: ../poten/surf3.F
	$(F77C) -c ../poten/surf3.F
surf4.o: ../poten/surf4.F
	$(F77C) -c ../poten/surf4.F
#User may add line here to compile potential file.  Start second line with a tab
ch4.o: ../poten/basis.f90 ../poten/ch4_pipnn.f90
	$(F77C) -c ../poten/basis.f90 ../poten/ch4_pipnn.f90


#User may add lines here to make the executable.  Example:
#SURFACE:
#	gmake $(SURFACEEXE)
#$(SURFACEEXE):$(OBJ) dummy_mpi.o surface.o
#	$(F77C) -o $(SURFACEEXE) $(OBJ) dummy_mpi.o surface.o

DUMS:
	gmake -s $(DUMSEXE)
$(DUMSEXE):$(OBJ) dummy_mpi.o dumpot.o
	$(F77C) -o $(DUMSEXE) $(OBJ) dummy_mpi.o dumpot.o

CH4OS:
	gmake -s $(CH4OSEXE)
$(CH4OSEXE):$(OBJ) dummy_mpi.o ch4o.o
	$(F77C) -o $(CH4OSEXE) $(OBJ) dummy_mpi.o ch4o.o ch4o_module.o

CH5S:
	gmake -s $(CH5SEXE)
$(CH5SEXE):$(OBJ) dummy_mpi.o ch5.o
	$(F77C) -o $(CH5SEXE) $(OBJ) dummy_mpi.o ch5.o

CLHBRS:
	gmake -s $(CLHBRSEXE)
$(CLHBRSEXE):$(OBJ) dummy_mpi.o clhbr.o setup3.o surf3.o coord3.o chain3.o
	$(F77C) -o $(CLHBRSEXE) $(OBJ) dummy_mpi.o clhbr.o setup3.o surf3.o coord3.o chain3.o

CMCS:
	gmake -s $(CMCSEXE)
$(CMCSEXE):$(OBJ) dummy_mpi.o cmc.o
	$(F77C) -o $(CMCSEXE) $(OBJ) dummy_mpi.o cmc.o

CWMCS:
	gmake -s $(CWMCSEXE)
$(CWMCSEXE):$(OBJ) dummy_mpi.o cwmc.o
	$(F77C) -o $(CWMCSEXE) $(OBJ) dummy_mpi.o cwmc.o

H2NIS:
	gmake -s $(H2NISEXE)
$(H2NISEXE):$(OBJ) dummy_mpi.o h2ni.o
	$(F77C) -o $(H2NISEXE) $(OBJ) dummy_mpi.o h2ni.o

H3S:
	gmake -s $(H3SEXE)
$(H3SEXE):$(OBJ) dummy_mpi.o h3.o setup3.o surf3.o coord3.o chain3.o
	$(F77C) -o $(H3SEXE) $(OBJ) dummy_mpi.o h3.o setup3.o surf3.o coord3.o chain3.o

HNIS:
	gmake -s $(HNISEXE)
$(HNISEXE):$(OBJ) dummy_mpi.o hni.o
	$(F77C) -o $(HNISEXE) $(OBJ) dummy_mpi.o hni.o

HO2S:
	gmake -s $(HO2SEXE)
$(HO2SEXE):$(OBJ) dummy_mpi.o ho2.o setup3.o surf3.o coord3.o chain3.o
	$(F77C) -o $(HO2SEXE) $(OBJ) dummy_mpi.o ho2.o setup3.o surf3.o coord3.o chain3.o

NH3S:
	gmake -s $(NH3SEXE)
$(NH3SEXE):$(OBJ) dummy_mpi.o nh3.o
	$(F77C) -o $(NH3SEXE) $(OBJ) dummy_mpi.o nh3.o

OH3S:
	gmake -s $(OH3SEXE)
$(OH3SEXE):$(OBJ) dummy_mpi.o oh3.o setup4.o surf4.o coord4.o chain4.o
	$(F77C) -o $(OH3SEXE) $(OBJ) dummy_mpi.o oh3.o setup4.o surf4.o coord4.o chain4.o

OHCLS:
	gmake -s $(OHCLSEXE)
$(OHCLSEXE):$(OBJ) dummy_mpi.o ohcl.o setup3.o surf3.o coord3.o chain3.o
	$(F77C) -o $(OHCLSEXE) $(OBJ) dummy_mpi.o ohcl.o setup3.o surf3.o coord3.o chain3.o

CH3:
	gmake -s $(CH3PEXE)
$(CH3PEXE):$(OBJ) dummy_mpi.o vrctst_mpi.o ch3.o
	$(MPIF77C) -o $(CH3PEXE) $(OBJ) dummy_mpi.o vrctst_mpi.o ch3.o $(SPRNGLIB)

CH5P:
	gmake -s $(CH5PEXE)
$(CH5PEXE):$(OBJ) poly_mpi.o ch5.o
	$(MPIF77C) -o $(CH5PEXE) $(OBJ) poly_mpi.o ch5.o

CH4S:
	gmake -s $(CH4SEXE)
$(CH4SEXE):$(OBJ) dummy_mpi.o basis.o ch4_pipnn.o ch4_interface.o
	$(F77C) -o $(CH4SEXE) $(OBJ) dummy_mpi.o basis.o ch4_pipnn.o ch4_interface.o -L$(MKLROOT)/lib/intel64 -lmkl_rt


clean:
	rm -f *.o
