#! /bin/csh -f

set pword=`whoami`"@"

set today=`date +%Y%m%d`

set dget=`./yesterday_4 $today`

echo "grabbing hires SST for " $dget

# first get the snow data

ftp -in ftpprd.ncep.noaa.gov << endftp1
user anonymous $pword
binary
cd /pub/data/nccf/com/fnl/prod/sst.$dget/
get sst2dvar_grb_0.5 
cd /pub/data/nccf/com/fnl/prod/fnl.$dget/
get gdas1.t18z.sstgrb sstgrb
cd /pub/emc/mmb/wrkstn_eta/cursnow/
mget *.grb
bye

endftp1

# mv rtg_sst_grb_0.5 sstgrb_hires
mv sst2dvar_grb_0.5 hires_sst
# mv gdas1.T00Z.sstgrb sstgrb
# mv gdas1.T00Z.sstgrb.index sstgrb.index
