#
# Start of script compc.sh          Micki Farley  97-02-01
#
echo Compile C program and place in library w3lib subroutine $1.c
#
# Example:  Compile subroutine xyz.c and put it into library w3lib
#
#           compc.sh xyz
#
rm    $1.o               # delete subroutine old object
if cc -c -DCAPS  -o $1.o $1.c # compile subroutine
then
ar r ../w3lib $1.o
rm    $1.o               # delete subroutine new object
rm    $1.l               # delete listing file
#
echo Subroutine $1 was compiled and placed in library w3lib
else
echo Compile error, subroutine $1 was not placed in library w3lib
fi
#
# End of script compc.sh
