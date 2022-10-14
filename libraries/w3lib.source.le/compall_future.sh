#
# script to make w3lib_export
#
# compile all the subroutines in the w3lib.a library R.E.JONES  91-05-11
# changes made for f90 w3lib M.E. Pyle 98-10-01
#
date
#
rm ../w3lib
# asmw3l.sh gbytes
# asmw3l.sh sbyte

# compc.sh cdate
# compc.sh cdate2

# 2/15/99 uncommented next 4
compw3l.sh gbyte
compw3l.sh gbytes
compw3l.sh sbyte
compw3l.sh sbytes

# below added 6/10/99 for LINUX version
compw3l.sh gbytec
compw3l.sh gbytesc
compw3l.sh sbytec
compw3l.sh sbytesc


# compw3l.sh aea
# BAOPEN AND BAREAD are taken care of in bacio library
# compw3l.sh baopen
# compw3l.sh baopenr
# compw3l.sh baread
#
# uncommented datimx 6/10/99
compw3l.sh datimx
compw3l.sh errexit
compw3l.sh errmsg
compw3l.sh fparsei
compw3l.sh fparser
compw3l.sh getbit
compw3l.sh getgb
compw3l.sh getgb1
compw3l.sh getgb1r
# compw3l.sh getgb1re
compw3l.sh getgb1s
# compw3l.sh getgbe
# compw3l.sh getgbeh
# compw3l.sh getgbem
# compw3l.sh getgbemh
# compw3l.sh getgbemp
# compw3l.sh getgbens
# compw3l.sh getgbep
# compw3l.sh getgbex
# compw3l.sh getgbexm
# compw3l.sh getgbh
compw3l.sh getgbm
compw3l.sh getgbmh
# compw3l.sh getgbmp
# compw3l.sh getgbp
compw3l.sh getgi
compw3l.sh getgir
compw3l.sh gtbits
# compw3l.sh hostname
# compw3l.sh idsdef
compw3l.sh instrument
# compw3l.sh iw3dat
compw3l.sh iw3jdn
# compw3l.sh iw3mat
# compw3l.sh iw3pds
# compw3l.sh iw3unp29
compw3l.sh ixgb
compw3l.sh lengds
# compw3l.sh mvdate
# compw3l.sh nfhour
# compw3l.sh pdsens
compw3l.sh pdseup
compw3l.sh putgb
# compw3l.sh putgbe
# compw3l.sh putgben
# compw3l.sh putgbens
# compw3l.sh putgbex
compw3l.sh putgbn
# all the q9s had problems
# compw3l.sh q9c32y
# compw3l.sh q9c64y
# compw3l.sh q9e32y
# compw3l.sh q9e64y
# compw3l.sh q9yc32
# compw3l.sh q9yc64
# compw3l.sh q9ye32
# compw3l.sh q9ye64
compw3l.sh r63w72
compw3l.sh skgb
# compw3l.sh swap16
# undeleted swap32 990607
compw3l.sh swap32
# compw3l.sh swap64
# compw3l.sh w3addate
# compw3l.sh w3ai00
# compw3l.sh w3ai01
# compw3l.sh w3ai08
# compw3l.sh w3ai15
# compw3l.sh w3ai18
# compw3l.sh w3ai19
# compw3l.sh w3ai24
# compw3l.sh w3ai38
# compw3l.sh w3ai39
# compw3l.sh w3ai40
# compw3l.sh w3ai41
# compw3l.sh w3aq03
# compw3l.sh w3aq05
# compw3l.sh w3aq06
# compw3l.sh w3aq07
# compw3l.sh w3aq15
# compw3l.sh w3as00
# compw3l.sh w3ctzdat
compw3l.sh w3difdat
compw3l.sh w3doxdat
# compw3l.sh w3fa01
# compw3l.sh w3fa03
# compw3l.sh w3fa03v
# compw3l.sh w3fa04
# compw3l.sh w3fa06
# compw3l.sh w3fa07
# compw3l.sh w3fa09
# compw3l.sh w3fa11
# compw3l.sh w3fa12
# compw3l.sh w3fa13
# compw3l.sh w3fb00
# compw3l.sh w3fb01
# compw3l.sh w3fb02
# compw3l.sh w3fb03
# compw3l.sh w3fb04
# compw3l.sh w3fb05
# compw3l.sh w3fb06
# compw3l.sh w3fb07
# compw3l.sh w3fb08
# compw3l.sh w3fb09
# compw3l.sh w3fb10
# compw3l.sh w3fb11
# compw3l.sh w3fb12
# compw3l.sh w3fc02
# compw3l.sh w3fc05
# compw3l.sh w3fc06
# compw3l.sh w3fc07
# compw3l.sh w3fc08

# deleted from below list 32 33 34 
# for num in 01 02 03 04 18 19 20 43 47 48 52 58 59 61 62 63 64 65 66 67
for num in 01 58 59 63 68 71 72 73 74 75 76 82 83
do
compw3l.sh w3fi$num
done

# deleted from below 81 
# for numb in 68 69 70 71 72 73 74 75 76 77 78 82 83 85 88 92 
# do
# compw3l.sh w3fi$numb
# done

# compw3l.sh w3fm07
# compw3l.sh w3fm08
# compw3l.sh w3fp04
# compw3l.sh w3fp05
# compw3l.sh w3fp06
# compw3l.sh w3fp10
# compw3l.sh w3fp11
# compw3l.sh w3fp12
# compw3l.sh w3fp13
# compw3l.sh w3fq02
# compw3l.sh w3fq06
# compw3l.sh w3fq07
# compw3l.sh w3fs03
# compw3l.sh w3fs04
# compw3l.sh w3fs05
# compw3l.sh w3fs06
# compw3l.sh w3fs11
# compw3l.sh w3fs12
# compw3l.sh w3fs13
# compw3l.sh w3fs15
# compw3l.sh w3fs17
# compw3l.sh w3fs19
# compw3l.sh w3fs21
# compw3l.sh w3fs22
compw3l.sh w3fs26

# for num in 00 01 02 03 05 05v 06 06v 07 08 09 10 11 12 16 17 201 202 203
# do
# compw3l.sh w3ft$num
# done

# for numb in 204 205 206 207 208 209 21 210 211 212 213 214 26 32 33 38 39 40 41 43v
# do 
# compw3l.sh w3ft$numb
# done

# compw3l.sh w3getunt
compw3l.sh w3locdat
# compw3l.sh w3miscan
compw3l.sh w3movdat
# compw3l.sh w3msg
# compw3l.sh w3nogds
# compw3l.sh w3pradat
# compw3l.sh w3prrdat
compw3l.sh w3reddat
compw3l.sh w3tagb
# compw3l.sh w3toveds
# compw3l.sh w3tovunp
# compw3l.sh w3trnarg
# compw3l.sh w3unpk77
compw3l.sh w3utcdat
# compw3l.sh w3valdat
# compw3l.sh w3ymdh4
# wryte added back 2/15/99
# redeleted 2/4/2000 compw3l.sh wryte
# compw3l.sh xdopen
# xmovex added back 2/15/99
compw3l.sh xmovex
compw3l.sh xstore
# will include the FFT part in w3lib
# compw3l.sh FFT99M
#
date
# End of compile of all subroutines into w3lib.a
