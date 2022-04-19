#!/bin/bash
# 
# Huiqing.Liu/MDL--- 08/2018 -------Convert shp file to img file ---------
#
# "HISTORY: March,    2016 - First Version" 
# "         Aug,      2018 - Second Version"
# " ---------------------------------------------------------"
#  
# Author: Huiqing.Liu (Huiqing.Liu@noaa.gov) 
# Abstract:  
# Converting Shp file to img file by using drawshp
#  Inputs:  Shp file 
#           
#  Outputs: Img file
#############################################################################################

if test "$2" == ''
then
  echo "Usage $0 <'con'> <2.0> "
  exit
fi
if test "$3" != ''
then
  echo "Usage $0 <'con'> <2.0> "
  exit
fi

prop=$1
datumm=$2

if [[ "$datumm" = 'agl' ]]; then
   datumLabel="Ground Level"
   numhrs=17
   intv=6hr
else
   datumLabel="NAVD88"
   numhrs=102
   intv=1hr
fi

prodc="stormtide"
res="625m"

area=con 
inifn=petss_sample_height_1km_estglf.ini
propf=${prop}
if [[ "$prop" = 'e10' || "$prop" = 'e20' || "$prop" = 'e30' || "$prop" = 'e40' || "$prop" = 'e50' || "$prop" = 'e90' ]]; then
   if [[ "$prop" = 'e10' ]]; then
     fieldname=SURGE10
   elif [[ "$prop" = 'e20' ]]; then
     fieldname=TCSRG20
   elif [[ "$prop" = 'e30' ]]; then
     fieldname=TCSRG30
   elif [[ "$prop" = 'e40' ]]; then
     fieldname=TCSRG40
   elif [[ "$prop" = 'e50' ]]; then
     fieldname=TCSRG50
   elif [[ "$prop" = 'e90' ]]; then
     fieldname=TCSRG90
   fi

elif [[ "$prop" = 'max' || "$prop" = 'mean' || "$prop" = 'min' ]]; then
   fieldname=ETCWL
else
   propf=${prop}
   inifn=petss_sample_proba_1km_estglf.ini
   if [[ "$datumm" = 'agl' ]]; then
      fieldname=PETCWL06
   else
      fieldname=PETCWL01
   fi
fi


box=(-98.0 23.61 -64.0 46.08) #good small est & gulf of MX
export pgm="drawshp"    
for i in `seq 1 ${numhrs}`;
do
    shpfile=${SHPpetss}/con_${datumm}_${cyc}_${intv}_msg_${i}_${prop}_625m.shp

    outfile=${IMGpetss}/con_${datumm}_06_${intv}_msg_${i}_${prop}_1km_glfest

  sed \
    -e s#LAT1#${box[1]}#g \
    -e s#LAT2#${box[3]}#g \
    -e s#LON1#${box[0]}#g \
    -e s#LON2#${box[2]}#g \
    -e s#FILENAME#${outfile}#g \
    -e s#INFILE#${shpfile}#g \
    -e s#FIELDNAME#${fieldname}#g \
    ${PARMpetss}/${inifn} > ${DATA}/petss_1km_estglf_${prop}_${datumm}_tmp.ini
      startmsg
    ${EXECpetss}/drawshp ${DATA}/petss_1km_estglf_${prop}_${datumm}_tmp.ini >> $pgmout 2> errfile
    err=$?;export err; err_chk
    convert ${IMGpetss}/con_${datumm}_06_${intv}_msg_${i}_${prop}_1km_glfest.png -fuzz 10% -transparent white ${IMGpetss}/con_${datumm}_06_${intv}_msg_${i}_${prop}_1km_glfest_trans.png
  done    



