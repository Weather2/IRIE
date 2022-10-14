#!/bin/sh

if [ $# -ne 5 ] ; then
echo "need five arguments "
echo "year month day start_hour forecast_length"
exit
fi

cd ..
ROOT=`pwd`
echo "ROOT:$ROOT"

yr=$1
month=$2
day=$3
termin=$4
hour=$5

rm -f $ROOT/eta/runs/bndy.*
rm -f $ROOT/eta/runs/cnst.file
rm -f $ROOT/eta/runs/preproc.bc.*
rm -f $ROOT/eta/runs/preproc.init
rm -f $ROOT/eta/runs/init.file
rm -f $ROOT/data/prep/cdate11
rm -f $ROOT/data/prep/gdsinfo.ETA_avn
rm -f $ROOT/data/prep/ua.*
rm -f $ROOT/data/prep/cpcp.*

for((i=0;i<=$hour;i+=6)); do

    hh=`printf "%03d" $i`

    if [ $i -lt 100 ] ; then
      ofset=`printf "%02d" $i`
    else
      ofset=$i
    fi

if [ $ofset -eq 000 ] ; then
cd $ROOT/dprep_IVAN/exe
./grib2surface_new_grib2 $yr$month$day$termin
cp $ROOT/data/prep/cdate11 $ROOT/eta/bin/.
fi

cd $ROOT/dprep_IVAN/exe

./grib2model_new_grib2 $yr$month$day$termin $hh $hh 6

cd $ROOT/eta/bin

../exe/initbc_new.exe $hh

done

exit
