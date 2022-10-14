#
# Start of script asmw3l.sh   Boi Vuong  98-03-12
# 
echo Compile assembler and place in library w3lib subroutine $1.s with script asmw3l.sh
#
#  Example: Compile assembler subroutine gbytes.s
#
#           asmw3l.sh gbytes 
#
set -x
rm  $1.o
as  $1.s 
ar r ../w3lib $1.o
# rm   $1.o
# echo Subroutine $1.s was compiled and placed in library w3lib
#
# End of script asmw3l.sh
