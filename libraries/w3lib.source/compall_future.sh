#
# script to make w3lib_export
#
# compile all the subroutines in the w3lib.a library R.E.JONES  91-05-11
# changes made for f90 w3lib M.E. Pyle 98-10-01
# Now only build the library needed for workstation Eta 99-07-26
#
date
#
rm ../w3lib

compw3l.sh gbyte
compw3l.sh gbytes
compw3l.sh sbyte
compw3l.sh sbytes
compw3l.sh getgb
compw3l.sh getgb1
compw3l.sh getgb1r
compw3l.sh getgb1s
compw3l.sh getgbm
compw3l.sh getgbmh

compw3l.sh getgi
compw3l.sh getgir
compw3l.sh getbit
compw3l.sh iw3jdn
compw3l.sh ixgb
compw3l.sh skgb
compw3l.sh pdseup
compw3l.sh putgb
compw3l.sh lengds
compw3l.sh putgbn

compw3l.sh r63w72
compw3l.sh instrument
compw3l.sh errmsg
compw3l.sh errexit
compw3l.sh fparser
compw3l.sh fparsei
# compw3l.sh w3addate
compw3l.sh w3difdat
compw3l.sh w3utcdat
compw3l.sh w3doxdat

compw3l.sh w3locdat
for num in 01 58 59 63 68 71 72 73 74 75 76 82 83
do
compw3l.sh w3fi$num
done
compw3l.sh w3fs26
compw3l.sh w3reddat
compw3l.sh w3tagb
## (in bacio?) compw3l.sh wryte
compw3l.sh xmovex
compw3l.sh xstore

date
# End of compile of all subroutines into w3lib.a
