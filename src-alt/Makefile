


F90 := gfortran
FFLAGS := -g -O0 -pedantic -fcheck=all


source := $(wildcard m_*.f90)
source.o := $(source:%.f90=%.o) 

default: lib test.x

test.x: $(source.o) test.f90
	$(F90) $(FFLAGS) -o $@ $^

lib:

%.o: %.f90
	$(F90) $(FFLAGS) -c $^ -o $@


clean:
	rm -f *.x *.o *.mod


m_dict.o: m_pair.o
m_pair.o: m_objects.o





