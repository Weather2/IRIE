
include ../../../eta/src/configure/make.inc

RN         = ./
CP         = cp -f
RM         = rm -f
FFLAGST  = $(FFLAGS) $(FFext)
EXECS      = vegUSGS soilSTAS
OTHERLIB   = $(root)/libraries/w3lib.a

# Default target

all	:
	@for name in $(EXECS); do\
	(make ../exe/$$name); \
	done

../exe/vegUSGS	: vegUSGS.F
	$(FC) $(FFLAGST) vegUSGS.F dutil.f $(OTHERLIB) -o $@ ; $(RM) vegUSGS.o dutil.o

../exe/soilSTAS	: soilSTAS.F
	$(FC) $(FFLAGST) soilSTAS.F dutil.f $(OTHERLIB) -o $@ ; $(RM) soilSTAS.o dutil.o

clean	:
	@for name in $(EXECS); do\
	($(RM) ../exe/$$name $$name.o ); \
	done ;\
