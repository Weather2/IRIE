SHELL=	/bin/sh
include ../eta/src/configure/make.inc
CMD=	../eta/exe/copygb.x
LOPTS=	
INCS=	
OBJS=	copygb.o  
LIBA= ../libraries/w3lib.a
LIBC= ../libraries/iplib.a
LIBE=
LIBS= $(LIBE) $(LIBA) $(LIBC) 
################################################################################
$(CMD):	$(OBJS)
	$(FC) $(LDFLAGS) -o $(CMD) $(OBJS) $(LIBS)
$(OBJS):	$(INCS)
copygb.o:	copygb.f
	$(FC) -c $(FFLAGS) $*.f
clean:	
	-rm -f $(OBJS)
