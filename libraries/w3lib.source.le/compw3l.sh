#
# Start of script compw3l.sh         M. Farley 02-24-1998 
#
# This script is used to compile F90 subroutines for the W3LIB.
#    Note the F90 library is a "portable archive" -vs- the previous
#    "build" library.  Compiler option "-O nofastint" has been included
#    to default to 64-bit integers (CDIR$ INTEGER=64 no longer allowed
#    in F90 codes as well as "-i 64" at compilation time).
#
# Example:  Compile subroutine w3fq02.f and put it into library w3lib
#           compw3l.sh w3fq02
#
#mp  rm    $1.o               # delete subroutine old object

fc=`head -n1 ../../dprep/src/configure/make.inc | awk '{print $3}'`

if $fc -O -c $1.f # compile subroutine
then
ar r ../w3lib $1.o
rm    $1.o               # delete subroutine new object
# rm    $1.l               # delete listing file
#
echo Subroutine $1 was compiled and placed in library w3lib
else
echo Compile error, subroutine $1 was not placed in library w3lib
fi
#
# End of script compw3l.sh
