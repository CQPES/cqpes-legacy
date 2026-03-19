FC = ifort
FFLAGS = -Ofast -static-intel -march=native -qmkl=sequential

.PHONY: all clean

all: venus96_ch4.x

venus96_ch4.x: basis.f90 ch4_pipnn.f90 ch4_interface.f90 venus96_ch4.f
	$(FC) $(FFLAGS) basis.f90 ch4_pipnn.f90 ch4_interface.f90 venus96_ch4.f -o venus96_ch4.x

clean:
	rm -rf *.o *.mod *.x
