#! /bin/csh -f
#

rm -f ../bacio
rm -f ../bacio.a

####################################################
#	C Part					   #
####################################################

#
#	Uncomment (remove the leading # sign)
#	the appropriate cc line from the following
#	three options


echo " "
echo "did you uncomment a C-compilation line??"
echo " "

# DEC option
# cc -std -c bacio.v1.3.c

# HP option
# cc -Aa -Ae -c bacio.v1.3.c

# SGI,IBM,LINUX option (and probably most others)
cc -c bacio.v1.3.c

ar rv ../bacio bacio.v1.3.o
ar rv ../bacio.a bacio.v1.3.o

####################################################
# 	FORTRAN part				   #
####################################################

set fc=`head -n1 ../../dprep/src/configure/make.inc | awk '{print $3}'`

echo $fc

$fc -c baciof.f
ar rv ../bacio baciof.o
ar rv ../bacio.a baciof.o

rm -f bacio.v1.3.o baciof.o
