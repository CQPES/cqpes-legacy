FC = ifort
FFLAGS = -Ofast -march=native

MKL_FLAGS = -L$(MKLROOT)/lib/intel64 -lmkl_rt

F2PY = f2py
F2PYFLAGS = --fcompiler=intelem

all: LIBCH4

ch4_pipnn.o: basis.o ch4_pipnn.f90
	$(FC) $(FFLAGS) -c ch4_pipnn.f90

basis.o: basis.f90
	$(FC) $(FFLAGS) -c basis.f90

LIBCH4: basis.o ch4_pipnn.o libch4.f90
	$(F2PY) libch4.f90 -m libch4 -h libch4.pyf
	$(F2PY) $(F2PYFLAGS) $(MKL_FLAGS) -c libch4.f90 basis.f90 ch4_pipnn.f90 libch4.pyf

.PHONY: clean
clean:
	rm -rf *.o *.mod *.pyf *.so
