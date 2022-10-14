#
#

SHELL    = /bin/sh
INCLUDE  = ../include/
CPP      = /lib/cpp -P -traditional -I$(INCLUDE)
RM       = rm -f
MNTINC   = $(INCLUDE)all.inc 
EXECS    = gfdlco2


#
default :
	@echo " "
	@echo "Type :"
	@echo "make sgi       to make for SGI workstation      "
	@echo "make ibm       to make for IBM workstation      "
	@echo "make convex    to make for CONVEX with IEEE     "
	@echo "make HP        to make for HP  workstation      "
	@echo "make Sun       to make for Sun BDS os           "
	@echo "make linux     to make for linux (Red Hat)      "
	@echo "make clean     to remove all files except surces and input data"
	@echo " "

all	:
	@for name in $(EXECS); do\
	(make ./exe/$$name); \
	done

./exe/gfdlco2	: ./src/gfdlco2.F $(MNTINC)
	$(CPP) ./src/gfdlco2.F > ./src/gfdlco2.f ; \
	$(F77) ./src/gfdlco2.f -o ./exe/gfdlco2;\
	$(RM)  ./src/gfdlco2.f


clean	:
	@for name in $(EXECS); do\
	($(RM) ./exe/$$name ./src/$$name.f ./src/$$name.o ); \
	done ;\
	$(RM) ./tmp/*


sgi     :
	@make all \
	"F77    = f77 -O3"

linux     :
	@make all \
	"F77    = f77"

Sun     :
	@make all \
	"F77    = f77 -O"

ibm     :
	@make all  \
	"F77    = xlf -O3"

convex  :
	@make all \
	"F77    = fc"

HP      :
	@make all \
	"F77=f77 "
