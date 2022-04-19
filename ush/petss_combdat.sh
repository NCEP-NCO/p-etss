#!/bin/sh

# ####################################################################################
# exetsurge_combdat.sh
# ####################################################################################
# Author: Ryan Schuster (ryan.schuster@noaa.gov)
# Abstract:
# 1) Calls the combAll executable to combine tideGrid, obsGrid, and
#    surgeGrid (see exetsurge_griddat.sh)
# 2) combAll uses the grids to create output SHEF-encoded files which
#    stored in ${COMIN}, along with specially-formatted .csv files
#
# Parameters:
#  Inputs:  Various envrionmental variables declared upstream in ecflow
#           and in the j-job jobs/JETSURGE_COMBDAT
#  Outputs: ${COMOUT}/xxxxxxx.csv - specially-formatted .csv files containing tide,
#                                   obs, and surge data where the 'xxxxxxx' is the 
#                                   station's COOPS ID number
#           ${COMOUT}/shef.etss.t${HH}z.totalwater.${label}  - 
#             output shef files where HH is the latest forecast cycle and 
#             label is the relevant geographical basin
# Revised by Huiqing Liu 
#  Format of xxxxxxx.csv
#
# Time, Tide, Obs (stations with NWSLI), Surge, Diff between obs and tide, bias corrected total water
#
######################################################################################
######################################################################################
# Updated by: Huiqing.Liu 04/2016
#             1) Extended forecast hour from 96 to 102 hr
#             2) Move from WCOSS to CRAY
# Updated by: Huiqing.Liu 07/2020
#             1) Modify to handle P-ETSS GEFS and NAEFS output
######################################################################################

set -xa
msg="Starting job $job"
postmsg "$jlogfile" "$msg"

export RANK=$1

# Get latest forecast hour
declare -a  hrs=('00' '06' '12' '18')
ind=$((${cyc}/6))
HH=${hrs[${ind}]}

# ################################
# Fortran file unit variables
# ################################
export pgm=combAll
. prep_step
# Any file's path length can change (assuming max possible path length of 255 characters)

# General I/O used by everyone
export FORT10=${HOMEpetss}/parm/model/mllw.csv
export FORT11=${HOMEpetss}/parm/master.csv
export FORT12=${DATA}/datelist

# I/O used by sorc/combAll.fd/combineAll.f
if [[ $RANK == 0 ]]; then
   export FORT13=${DATA}/surgeGrid
   echo 'naef' > prod_stn_naef
   export FORT18=prod_stn_naef 
# ###############################################
# Copy all necessary input files from $COMIN
# ###############################################
   cpreq $COMIN/tideGrid ./
   cpreq $COMIN/surgeGrid ./
   cpreq $COMIN/HSBY ./
   echo 'finished copying files' > fn1_cp
else
   export FORT13=${DATA}/surgeGrid_gefs
   echo 'gefs' > prod_stn_gefs
   export FORT18=prod_stn_gefs 
# ###############################################
# Copy all necessary input files from $COMIN
# ###############################################
   cpreq $COMIN/surgeGrid_gefs ./
   cpreq $COMIN/obsGrid ./
   cpreq $COMIN/datelist ./
   echo 'finished copying files' > fn2_cp
fi
export FORT14=${DATA}/tideGrid
export FORT15=${DATA}/obsGrid

declare -a shef=(FORT51 FORT52 FORT53 FORT54 FORT55)
declare -a shef90=(FORT61 FORT62 FORT63 FORT64 FORT65)
declare -a shef10=(FORT71 FORT72 FORT73 FORT74 FORT75)
declare -a area=('US' 'US' 'US' 'AK' 'AK')
declare -a awips=('TWE' 'TWG' 'TWP' 'TWC' 'TWB')
declare -a label=('est' 'gom' 'wst' 'goa' 'ber')
declare -a member=('srus70_est' 'srus70_gom' 'srus70_wst' 'srak70_goa' 'srak70_ber')
for i in `seq 0 4` ; do
  export ${shef[${i}]}=${COMOUT}/shef.petss.t${HH}z.mean.totalwater.${label[$i]}
  export ${shef90[${i}]}=${COMOUT}/shef.petss.t${HH}z.e90.totalwater.${label[$i]}
  export ${shef10[${i}]}=${COMOUT}/shef.petss.t${HH}z.e10.totalwater.${label[$i]}
done

while [[ ! -f fn2_cp || ! -f fn1_cp ]] ; do
  echo "`date`: Rank ${RANK} waiting for copying files." >> timing.txt
  sleep 1
done

# This is a placeholder in sorc/combAll.fd/combineAll.f where '0000000' is replaced with
# station COOPS ID if necessary
if [[ $RANK == 0 ]]; then
   mkdir -p ${COMOUT}/t${HH}z_csv
   export FORT57=${COMOUT}/t${HH}z_csv/0000000.csv
# Execute...
  startmsg
  ${EXECpetss}/petss_post_combAll >> $pgmout 2> errfile
  err=$?; export err; err_chk

# ################################
# Create DBnet alert
# ################################

  if [[ $SENDCOM == "YES" ]] ; then

    cd ${COMOUT}/../
    tar -zcvf  ${COMOUT}/petss.t${HH}z.csv_tar ${RUN}.${PDY}/t${HH}z_csv/*.csv    
    tar -cvf ${COMOUT}/petss.t${HH}z.shef_tar ${RUN}.${PDY}/shef.petss.t${HH}z.*

  fi

  if [ $SENDDBN = "YES" ] ; then
    $DBNROOT/bin/dbn_alert MDLFCST PETSSCSV $job ${COMOUT}/petss.t${HH}z.csv_tar
    $DBNROOT/bin/dbn_alert MDLFCST PETSSBULL $job ${COMOUT}/petss.t${HH}z.shef_tar
    if [ $SENDDBN_NTC = "YES" ] ; then
       for i in `seq 0 4` ; do
           for ffn in mean e90 e10; do
               file=shef.petss.t${HH}z.${ffn}.totalwater.${label[$i]}
               $USHutil/form_ntc.pl -d ${member[$i]} -f ${COMOUT}/$file -j $job -m $NET -p $COMOUTwmo -s $SENDDBN_NTC -o $file -n
               export err=$?; err_chk
           done
       done
    fi
  fi
  rm ${COMOUT}/../shef*
else
   mkdir -p ${COMOUT}/t${HH}z_gefs_csv
   export FORT57=${COMOUT}/t${HH}z_gefs_csv/0000000.csv
# Execute...
  startmsg
  ${EXECpetss}/petss_post_combAll >> $pgmout 2> errfile
  err=$?; export err; err_chk

  if [[ $SENDCOM == "YES" ]] ; then
     cd ${COMOUT}/../
     tar -zcvf  ${COMOUT}/petss.t${HH}z.gefs_csv_tar ${RUN}.${PDY}/t${HH}z_gefs_csv/*.csv
  fi
  if [ $SENDDBN = "YES" ] ; then
     $DBNROOT/bin/dbn_alert MDLFCST PETSSCSV $job ${COMOUT}/petss.t${HH}z.gefs_csv_tar
  fi
fi

if [[ ${RUN_ENVIR} == "MDLTEST" ]] ; then
   echo $msg > ${MDLTEST_DIR}/tmpnwprd1/post_finish.txt
fi

msg="$job completed normally"
postmsg "$jlogfile" "$msg"

