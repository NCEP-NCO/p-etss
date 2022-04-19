#!/bin/bash
# 
# Huiqing.Liu/MDL--- 08/2018 -------Convert grib2 file to Shp file ---------
#
# "HISTORY: March,    2016 - First Version" 
# "         Aug,      2018 - Second Version"
# " ---------------------------------------------------------"
# 
# Author: Huiqing.Liu (Huiqing.Liu@noaa.gov) 
# Abstract: 
# Converting grib2 file to Shp file by using degrib
#  Inputs:  625m grib2 file
#           
#  Outputs: Shp file
#############################################################################################

if test "$4" == ''
then
  echo "Usage $0 <'start mes #'> <end mes #> "
  exit
fi
if test "$5" != ''
then
  echo "Usage $0 <'start mes #'> <end mes #> "
  exit
fi


num1=$1
num2=$2
prop=$3
datumm=$4

prodc=stormtide

if [[ "$datumm" = 'agl' ]]; then
   datum="agl"
   intv="6hr"
else
   datum="dat"
   intv="1hr"
fi
propf=${prop}
etsurge_grib=${COMIN}/petss.t${cyc}z.stormtide_${propf}_${intv}_inc_${datum}.con625m.grib2
export pgm="degrib"
for i in `seq ${num1} ${num2}`;
do
    shpfile=${SHPpetss}/con_${datumm}_${cyc}_${intv}_msg_${i}_${prop}_625m.shp
    startmsg
    ${EXECpetss}/degrib ${etsurge_grib} -out ${shpfile} -C -msg $i -Shp -poly 2 -nMissing >> $pgmout 2> errfile
  err=$?;export err; err_chk
done    
