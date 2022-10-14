#!/bin/sh

# head is used for symbolic links.  
# head="ftn" for HP workstations
# head="fort." for most (all?) others.

head="fort."
rm ${head}*

echo Starting eta topography creation...

#### get the proper sea mask data uncompressed

pgf90 -fast -mcmodel=medium -o select.x smask_select.f
./select.x

first=`head -n1 tmp.smask`
last=`tail -n1 tmp.smask`

tiles="01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 \
	 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36"

for tl in $tiles
do

if [ $tl -ge $first ]
then
if [ $tl -le $last ]
then
if [ ! -s smask.30s.$tl ]
then
echo "processing strip number " $tl
cp ./smasks_30s/smask.30s.${tl}.Z .
gzip -d smask.30s.${tl}.Z
fi
fi
fi

done


# ***** T O P O ************

if [ ! -e "ZEFF" ]
then
../exe/etatopo.exe > topo.out
err=$?
cp ZEFF ../runs
exit
else
err=0
fi

# *****************

if [ $err -eq 0 ]
then
echo " "
echo Starting eta initialization and bc file creation...

#if [ -s snowdepth.grb ]
#then
#echo "appear to have surface files"
#else
#echo "didnt find snow file...running get_sfcfields.com"
#./get_sfcfields.com
#fi


##################################
#
# SOIL,VEG,SST and SNOW links
#
rm ${head}*

ln -s -f deta  ${head}16
ln -s -f maxsnoalb.bin  ${head}20
ln -s -f alb1_ieee      ${head}21
ln -s -f alb2_ieee      ${head}22
ln -s -f alb3_ieee      ${head}23
ln -s -f alb4_ieee      ${head}24
ln -s -f ivgtyp_1d_ieee ${head}30
ln -s -f isltyp_1d_ieee ${head}31
ln -s -f islope_1d_ieee ${head}32
ln -s -f sfcanl		${head}33
ln -s -f sstgrb ${head}39
ln -s -f sstgrb.index ${head}40
ln -s -f rfusaflw_ieee ${head}42
ln -sf imsmask.ascii  ${head}43
#
####################################

../exe/initbc_new.exe 000 > initbc.out
err=$?
echo value of err is $err
fi

if [ $err -ne 0 ]
then
echo "BAILING OUT BECAUSE SOMETHING FAILED!!!!!!!!!"
fi

cp ETAIN ../runs/

#######################

cat ETAIN | grep 'SIGMA' | grep -i true

if [ $? -eq 0 ]
then
cp ../runs/sigma.co2.dat ../runs/co2.dat
else
cp ../runs/eta.co2.dat ../runs/co2.dat
fi

exit 0
