#! /bin/csh -f

rm -f bufrlib.a
rm -f iplib.a

cd w3lib-1.4
make clean

cd ../dummyMPI
make clean

cd ..

exit
