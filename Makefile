
TARGETS = dynaglace_fix u.fill_levels u.re_tag_date

FFLAGS = -O 2

all:	$(TARGETS)

dynaglace_fix:	dynaglace_fix.F90
	s.f90 $(FFLAGS) -o $@ $^ -lrmn_015.3

u.fill_levels:	u.fill_levels.F90
	s.f90 $(FFLAGS) -o $@ $^ -lrmn_015.3

u.re_tag_date:	u.re_tag_date.F90
	s.f90 $(FFLAGS) -o $@ $^ -lrmn_015.3

clean:
	rm -f $(TARGETS) *.o
