
TARGETS = dynaglace_fix u.fill_levels u.re_tag_date

SCRIPTS = post_sps.sh pre_sps.sh run_sps.sh functions_sps.dot

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

to_bin:	$(TARGETS)
	mkdir -p $(HOME)/bin/$(BASE_ARCH)
	cp $(TARGETS) $(HOME)/bin/$(BASE_ARCH)/.
	cp $(SCRIPTS) $(HOME)/bin/.

clean_bin:
	(cd $(HOME)/bin/$(BASE_ARCH) && rm -f $(TARGETS))
	(cd $(HOME)/bin/ && rm -f $(SCRIPTS))

to_ovbin:	$(TARGETS)
	mkdir -p $(HOME)/ovbin/$(BASE_ARCH)
	cp $(TARGETS) $(HOME)/ovbin/$(BASE_ARCH)/.
	cp $(SCRIPTS) $(HOME)/ovbin/.

clean_ovbin:
	(cd $(HOME)/ovbin/$(BASE_ARCH) && rm -f $(TARGETS))
	(cd $(HOME)/ovbin/ && rm -f $(SCRIPTS))
