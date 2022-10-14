SHELL=	/bin/sh
include	../src/configure/make.inc
EXE=	stalst.x
OBJS=	staids_prep.o
################################################################################
exe:	$(EXE)
$(EXE):	$(OBJS)
	$(FC) $(FFLAGS) -o $(EXE) $(OBJS)
$(OBJS):	$(INCS)
staids_prep.o:	staids_prep.f
	$(FC) $(FFLAGS) -c $(FFLAGS) $*.f
