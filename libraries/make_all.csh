#! /bin/csh -f

cd ei_bufrlib.source
sh makebufrlib.sh

cd ../w3lib-1.4
make

cd ../iplib.source.le
sh compallip.sh

cd ../dummyMPI
make

cd ..

exit
