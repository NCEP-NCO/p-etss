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
#  Inputs:  2.5km grib2 file 
#           
#  Outputs: Shp file
#############################################################################################

if test "$2" == ''
then
  echo "Usage $0 <'con'> <10p> <.agl> "
  exit
fi

area=$1
prop=$2
datumm=$3

if [[ "$datumm" = 'agl' ]]; then
   datumLabel="Ground Level"
   datum=".agl"
fi

echo "area= $area"
echo "prop= $prop"
echo "datum= $datumm"


propf=${prop}


if [[ "$area" = 'con' ]]; then
   etsurge_grib=${COMIN}/petss.t${cyc}z.stormtide_${propf}_6hr_inc_${datumm}.${area}2p5km.grib2
   
   box=(-130.00 15.0 -63.00 53)
elif [[ "$area" = 'ala' ]]; then
   etsurge_grib=${COMIN}/petss.t${cyc}z.stormtide_${propf}_6hr_inc_${datumm}.${area}3km.grib2
   
   box=(195.00 47.70 250.00 71.20)
fi

export pgm="degrib"
for i in `seq 1 17`; do
   
  shpfile=${SHPpetss}/${area}_${datumm}_${cyc}_6hr_msg_${i}_${prop}.shp
  startmsg
  ${EXECpetss}/degrib ${etsurge_grib} -out ${shpfile} -C -msg $i -Shp -poly 2 -nMissing >> $pgmout 2> errfile
  err=$?;export err; err_chk

done    
