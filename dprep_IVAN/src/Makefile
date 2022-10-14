
include ../../eta/src/configure/make.inc

RN         = ./
CP         = cp -f
RM         = rm -f
FFLAGST  = $(FFLAGS) $(FFext)
EXECS      = allinc grib2model_new grib2surface_new grib2model_new_grib2 grib2surface_new_grib2
OTHERLIB   = $(root)/libraries/w3lib.a
OTHERLIB2   = $(OTHERLIB) ../g2lib/libg2.a -I../g2lib -L../grib2/lib -ljasper -lpng -lz

# Default target

all	:
	@for name in $(EXECS); do\
	(make ../exe/$$name); \
	done

../exe/allinc	: allinc.f
	$(CP) $(root)/eta/bin/ETAIN .
	$(CP) $(root)/eta/src/include/ecommons.h .
	$(FC) $(FFLAGST) allinc.f -I$(root)/eta/src/include $(root)/eta/lib/lib*.a -o $@
	$(RN)../exe/allinc
	$(RM) ETAIN ecommons.h cdate11 $@ allinc.o

../exe/grib2model_new	: grib2model_new.f
	$(FC) $(FFLAGST) grib2model_new.f thermolib3d.F $(OTHERLIB) -o $@ ; $(RM) grib2model_new.o thermolib3d.o

../exe/grib2surface_new	: grib2surface_new.f
	$(FC) $(FFLAGST) grib2surface_new.f thermolib3d.F dutil.f $(OTHERLIB) -o $@ ; $(RM) grib2surface_new.o dutil.o thermolib3d.o

../exe/grib2model_new_grib2	: grib2model_new_grib2.f
	$(FC) $(FFLAGST) grib2model_new_grib2.f $(OTHERLIB2) -o $@ ; $(RM) grib2model_new_grib2.o

../exe/grib2surface_new_grib2	: grib2surface_new_grib2.f
	$(FC) $(FFLAGST) grib2surface_new_grib2.f dutil.f $(OTHERLIB2) -o $@ ; $(RM) grib2surface_new_grib2.o dutil.o

clean	:
	@for name in $(EXECS); do\
	($(RM) ../exe/$$name $$name.o ); \
	done ;\
	$(RM) ../include/all*.inc ../exe/param* 
