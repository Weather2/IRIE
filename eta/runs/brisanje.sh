#!/bin/bash
###############################################
echo 'Cleaning old model data '
rm -f namelist.* post.* quilt.*
rm -f run_eur.* EGRD* fcstdone* latlon* lmbc*
for((i=0;i<=180;i+=1)); do
    if [ $i -lt 10 ]; then
        echo '00'$i
        hh='00'$i
    elif [ $i -lt 100 ]; then
        echo '0'$i
        hh='0'$i
    else
        echo $i
        hh=$i
    fi
rm -f restrt"$hh"*
done
###############################################
