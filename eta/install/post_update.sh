#! /bin/csh -f

cd ../bin
echo "20091010 0" > cdate11
./new_prep.sh
cd ../../dprep_IVAN/exe
echo "../../data/grib/data_GFS." > paramavn
./grib2model_new 09101000 0 0 6
./grib2surface_new 09101000 1
cd ../soil/exe
./soilSTAS
./vegUSGS
cd ../../../eta/bin
./new_prep.sh
rm -f ../../dprep_IVAN/exe/paramavn

exit
