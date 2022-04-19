#!/bin/sh

# ###########################################################################################
# exetsurge_parsedat.sh
# ###########################################################################################
# Author: Ryan Schuster (ryan.schuster@noaa.gov)
# Abstract: 
# 1) Figure out the current date and
#    a) use it to get the latest forecast cycle (00Z, 06Z, 12Z, or 18Z)
#    b) use it to get a list of dates in six-minute intervals
#       spanning five days into the past (5-day hindcast)
#    c) use it to find the number of hours from the beginning of the year
#       to the beginning of the 5-day hindcast (for tide code)
# 2) Acquire BUFR files of observed water levels at COOPS tide gauges for the
#    appropriate dates
# 3) Acquire ETSS text product output - 96 hour forecast of surge at relevant stations
#    a) Concatenate text products from all geographical basins into one
#       file and rename based on date and forecast cycle time
# 4) Create a poescript of debufr commands to be run in parallel. These will
#    parse the binary BUFR files into readable obs for later source code to use.
# 5) Run the poescript and copy all relevant outputs and lists of date-based
#    filenames to use down the line.
# Parameters:
#  Inputs:  Various environmental variables defined either in ecflow or in the
#           j-job jobs/JETSURGE_PARSEDAT
#  Outputs: poescript_obsraw - list of executables for mpirun.lsf
#           De-BUFRed water-level obs - ${DATA}/out/YYYYmmdd.shef12
#           ETSS text products - ${DATA}/YYYYmmdd_HH.ss
#           fns_ss.txt / fns_obs.txt - lists of those filenames
#           datelist - YYYYmmddHHMM format of dates encompasing the 5-day
#                      hindcast to the 102-hour forecast in six-minute intervals
#           HSBY - number of hours since the beginning of the year
#############################################################################################
# Updated by: Huiqing.Liu 04/2016
#             1) Extended forecast hour from 96 to 102 hr
#             2) Move from WCOSS to CRAY
# Updated by: Huiqing.Liu 03/2018
#             Don't use POE at all.
# Updated by: Huiqing.Liu 07/2020
#             1) Modify to handle P-ETSS GEFS and NAEFS output
#############################################################################################

set -xa
msg="Starting job $job"
postmsg "$jlogfile" "$msg"

export RANK=$1

. prep_step 

# ###############################
# Fortran file unit variables
# ###############################
# Any file's path length can change (assuming max possible path length of 255 characters)

# General I/O used by everyone
export FORT11=${HOMEpetss}/parm/master.csv
export FORT12=datelist

# I/O used by lib/sorc/mybufr/fdebufr.f
export FORT13=/dev/null

# Set today's date
if [[ ${RANK} == 0 ]] ; then
# ###############################
# Create timestamp list
# ###############################
# Get last forecast cycle

   declare -a hrs=('00' '06' '12' '18')
   ind=$((${cyc}/6))
   HHfc=${hrs[${ind}]}


   touch ${DATA}/datelist
   IDATE="${PDYm5}${HHfc}00"
   #for MM in `seq 0 6 12960` ; do #5 days + 96 hours forecast
   for MM in `seq 0 6 13320` ; do #5 days + 102 hours forecast
       echo `${MDATE} ${MM} ${IDATE}` >> datelist
   done

# ###############################
# Hours since beginning of year
# ###############################
   PDY2="${IDATE:0:4}-${IDATE:4:2}-${IDATE:6:2} ${IDATE:8:2}:00:00"

   PDY1="${IDATE:0:4}-01-01 00:00:00"

   HHsby=$((($(date -ud "${PDY2}" +%s)-$(date -ud "${PDY1}" +%s))/3600))
   touch ${DATA}/HSBY
   echo $HHsby >> HSBY
   echo ${IDATE:0:4} >> HSBY

   echo 'time setup is finished' > msg_Done_timesetup.txt

fi

while [[ ! -f msg_Done_timesetup.txt ]] ; do
  echo "`date`: Rank ${RANK} is waiting Rank 1 to setpdy.sh." >> timing.txt
  sleep 1
done


if [[ ${RANK} == 1 ]] ; then
# Grab BUFR files from data tanks and rename
   for i in `seq 5 -1 0` ; do
       if [[ $i -eq 0 ]] ; then
          PDYmx=$PDY
       else
          PDYmx=$(finddate.sh $PDY d-${i})
       fi
       
       bufrTank="${DCOMbase}/${PDYmx}/b001"
       if [ -s ${bufrTank}/xx012 ] ; then
          cpreq ${bufrTank}/xx012 ${DATA}
          mv ${DATA}/xx012 ${DATA}/${PDYmx}.shef12
       else
         msg="$job Warning: The bufr tank file ${bufrTank}/xx012 is missing or 0-bytes"
         postmsg "$jlogfile" "$msg"
       fi
    done
    echo "Bufr Tank data is copied" > msg_Done_bufr1.txt

elif [[ ${RANK} == 2 ]] ; then

   for i in `seq 5 -1 0` ; do
       if [[ $i -eq 0 ]] ; then
          PDYmx=$PDY
       else
          PDYmx=$(finddate.sh $PDY d-${i})
       fi

       bufrTank="${DCOMbase}/${PDYmx}/b001"
       if [ -s ${bufrTank}/xx005 ] ; then
          cpreq ${bufrTank}/xx005 ${DATA}
          mv ${DATA}/xx005 ${DATA}/${PDYmx}.shef05
       else
          msg="$job Warning: The bufr tank file ${bufrTank}/xx005 is missing or 0-bytes"
          postmsg "$jlogfile" "$msg"
       fi
    done
    echo "Bufr Tank data is copied" > msg_Done_bufr2.txt

elif [[ ${RANK} == 3 ]] ; then

   for i in `seq 5 -1 0` ; do
       if [[ $i -eq 0 ]] ; then
          PDYmx=$PDY
       else
          PDYmx=$(finddate.sh $PDY d-${i})
       fi

##---------------------------------------------------------
## Using DCOMROOT1 for b255 bufrtank (restricked data)
##---------------------------------------------------------
       bufrTank="${DCOMbase1}/${PDYmx}/b255"

       if [ -s ${bufrTank}/xx102 ] ; then
          cpreq ${bufrTank}/xx102 ${DATA}
          mv ${DATA}/xx102 ${DATA}/${PDYmx}.shef13
       else
          msg="$job Warning: The bufr tank file ${bufrTank}/xx102 is missing or 0-bytes"
          postmsg "$jlogfile" "$msg"
       fi
    done
    echo "Bufr Tank data is copied" > msg_Done_bufr3.txt

elif [[ ${RANK} == 0 ]] ; then

# ###################################
# Get surge forecast text products
# ###################################
# declare basins
declare -a bsns=('ber' 'est' 'gom' 'goa' 'wst')

# delete old parm files
touch fns_ss.txt

# We need a 5-day hindcast of surge, so grab 5 days' worth of files
for i in `seq 5 -1 0` ; do
  # Files are stored in prod directory by date and fcst cycle
  if [[ $i -eq 0 ]] ; then
    PDYmx=$PDY
  else
#   PDYmx=$(${USHutil}/finddate.sh $PDY d-${i})
    PDYmx=$(finddate.sh $PDY d-${i})
  fi

  # If this is from 5 days ago...
  if [[ $i -eq 5 ]] ; then
    # Grab files from fcst cycle to 18Z on that day
    for HH in "${hrs[@]:${ind}:4}" ; do
      # Tell Fortran what files to look for
      echo "${PDYmx}_$HH" >> fns_ss.txt
      for bsn in "${bsns[@]}" ; do
          # Put files in your data dir
          if [ -s ${COMIN}/petss.${PDYmx}/petss.t${HH}z.mean.stormsurge.${bsn}.txt ]; then
             cat ${COMIN}/petss.${PDYmx}/petss.t${HH}z.mean.stormsurge.${bsn}.txt >> ${PDYmx}_${HH}.ss
             cat ${COMIN}/petss.${PDYmx}/petss.t${HH}z.mean_gefs.stormsurge.${bsn}.txt >> ${PDYmx}_${HH}_gefs.ss
          else
             msg="$job Warning: surge file ${COMIN}/etss.${PDYmx}/petss.t${HH}z.mean.stormsurge.${bsn}.txt is missing or 0-bytes"
             postmsg "$jlogfile" "$msg"
          fi
          if [ -s ${COMIN}/petss.${PDYmx}/petss.t${HH}z.e90.stormsurge.${bsn}.txt ]; then
             cat ${COMIN}/petss.${PDYmx}/petss.t${HH}z.e90.stormsurge.${bsn}.txt >> ${PDYmx}_${HH}_90p.ss
             cat ${COMIN}/petss.${PDYmx}/petss.t${HH}z.e90_gefs.stormsurge.${bsn}.txt >> ${PDYmx}_${HH}_90p_gefs.ss
          else
             msg="$job Warning: surge file ${COMIN}/etss.${PDYmx}/petss.t${HH}z.e90.stormsurge.${bsn}.txt is missing or 0-bytes"
             postmsg "$jlogfile" "$msg"
          fi
          if [ -s ${COMIN}/petss.${PDYmx}/petss.t${HH}z.e10.stormsurge.${bsn}.txt ] ; then
             cat ${COMIN}/petss.${PDYmx}/petss.t${HH}z.e10.stormsurge.${bsn}.txt >> ${PDYmx}_${HH}_10p.ss
             cat ${COMIN}/petss.${PDYmx}/petss.t${HH}z.e10_gefs.stormsurge.${bsn}.txt >> ${PDYmx}_${HH}_10p_gefs.ss
          else
             msg="$job Warning: surge file ${COMIN}/etss.${PDYmx}/petss.t${HH}z.e10.stormsurge.${bsn}.txt is missing or 0-bytes"
             postmsg "$jlogfile" "$msg"
          fi
      done
    done
  # Else if this is today...
  elif [[ $i -eq 0 ]] ; then
    # Grab files from 00Z to the current fcst cycle
    for HH in "${hrs[@]:0:$((${ind}+1))}" ; do
      echo "${PDYmx}_$HH" >> fns_ss.txt
      for bsn in "${bsns[@]}" ; do
          if [ -s ${COMIN}/petss.${PDYmx}/petss.t${HH}z.mean.stormsurge.${bsn}.txt ]; then
             cat ${COMIN}/petss.${PDYmx}/petss.t${HH}z.mean.stormsurge.${bsn}.txt >> ${PDYmx}_${HH}.ss
             cat ${COMIN}/petss.${PDYmx}/petss.t${HH}z.mean_gefs.stormsurge.${bsn}.txt >> ${PDYmx}_${HH}_gefs.ss
          else
             msg="$job Warning: surge file ${COMIN}/etss.${PDYmx}/etss.t${HH}z.mean.stormsurge.${bsn}.txt is missing or 0-bytes"
             postmsg "$jlogfile" "$msg"
          fi
          if [ -s ${COMIN}/petss.${PDYmx}/petss.t${HH}z.e90.stormsurge.${bsn}.txt ]; then
             cat ${COMIN}/petss.${PDYmx}/petss.t${HH}z.e90.stormsurge.${bsn}.txt >> ${PDYmx}_${HH}_90p.ss
             cat ${COMIN}/petss.${PDYmx}/petss.t${HH}z.e90_gefs.stormsurge.${bsn}.txt >> ${PDYmx}_${HH}_90p_gefs.ss
          else
             msg="$job Warning: surge file ${COMIN}/etss.${PDYmx}/etss.t${HH}z.e90.stormsurge.${bsn}.txt is missing or 0-bytes"
             postmsg "$jlogfile" "$msg"
          fi
          if [ -s ${COMIN}/petss.${PDYmx}/petss.t${HH}z.e10.stormsurge.${bsn}.txt ]; then
             cat ${COMIN}/petss.${PDYmx}/petss.t${HH}z.e10.stormsurge.${bsn}.txt >> ${PDYmx}_${HH}_10p.ss
             cat ${COMIN}/petss.${PDYmx}/petss.t${HH}z.e10_gefs.stormsurge.${bsn}.txt >> ${PDYmx}_${HH}_10p_gefs.ss
          else
             msg="$job Warning: surge file ${COMIN}/etss.${PDYmx}/etss.t${HH}z.e10.stormsurge.${bsn}.txt is missing or 0-bytes"
             postmsg "$jlogfile" "$msg"
          fi
      done
    done
  # Else if this file is in between...
  else
    # Grab files for all 4 forecast cycles
    for HH in "${hrs[@]}" ; do
      echo "${PDYmx}_$HH" >> fns_ss.txt
      for bsn in "${bsns[@]}" ; do
            if [ -s ${COMIN}/petss.${PDYmx}/petss.t${HH}z.mean.stormsurge.${bsn}.txt ]; then
               cat ${COMIN}/petss.${PDYmx}/petss.t${HH}z.mean.stormsurge.${bsn}.txt >> ${PDYmx}_${HH}.ss
               cat ${COMIN}/petss.${PDYmx}/petss.t${HH}z.mean_gefs.stormsurge.${bsn}.txt >> ${PDYmx}_${HH}_gefs.ss
            else
               msg="$job Warning: surge file ${COMIN}/etss.${PDYmx}/petss.t${HH}z.mean.stormsurge.${bsn}.txt is missing or 0-bytes"
               postmsg "$jlogfile" "$msg" 
            fi
            if [ -s ${COMIN}/petss.${PDYmx}/petss.t${HH}z.e90.stormsurge.${bsn}.txt ]; then
               cat ${COMIN}/petss.${PDYmx}/petss.t${HH}z.e90.stormsurge.${bsn}.txt >> ${PDYmx}_${HH}_90p.ss
               cat ${COMIN}/petss.${PDYmx}/petss.t${HH}z.e90_gefs.stormsurge.${bsn}.txt >> ${PDYmx}_${HH}_90p_gefs.ss
            else
               msg="$job Warning: surge file ${COMIN}/etss.${PDYmx}/petss.t${HH}z.e90.stormsurge.${bsn}.txt is missing or 0-bytes"
               postmsg "$jlogfile" "$msg" 
            fi
            if [ -s ${COMIN}/petss.${PDYmx}/petss.t${HH}z.e10.stormsurge.${bsn}.txt ]; then
               cat ${COMIN}/petss.${PDYmx}/petss.t${HH}z.e10.stormsurge.${bsn}.txt >> ${PDYmx}_${HH}_10p.ss
               cat ${COMIN}/petss.${PDYmx}/petss.t${HH}z.e10_gefs.stormsurge.${bsn}.txt >> ${PDYmx}_${HH}_10p_gefs.ss
            else
               msg="$job Warning: surge file ${COMIN}/etss.${PDYmx}/petss.t${HH}z.e10.stormsurge.${bsn}.txt is missing or 0-bytes"
               postmsg "$jlogfile" "$msg" 
            fi
      done
    done
  fi
done

# ################################
# Store obs date-based file names 
# ################################
touch ${DATA}/fns_obs.txt
touch ${DATA}/poescript_obsraw

# Loop through date-based shef BUFR filenames and write to poescript
for i in `seq 5 -1 0` ; do
  if [[ $i -eq 0 ]] ; then
    PDYmx=$PDY
  else
#   PDYmx=$(${USHutil}/finddate.sh $PDY d-${i})
    PDYmx=$(finddate.sh $PDY d-${i})
  fi
  echo "$PDYmx" >> ${DATA}/fns_obs.txt
done

cp *.ss $COMOUT
echo "Storm Surge model data is copied" > msg_Done_surge.txt

fi

while [[ ! -f msg_Done_bufr1.txt || ! -f msg_Done_bufr2.txt || ! -f msg_Done_bufr3.txt || ! -f msg_Done_surge.txt ]] ; do
  echo "`date`: Rank ${RANK} Sleeping." >> timing.txt
  sleep 1
done

export pgm=petss_post_debufr
if [[ ${RANK} == 0 ]] ; then
   if [ -s ${PDY}.shef12 ]; then
      startmsg
      ${EXECpetss}/petss_post_debufr ${PDY}.shef12 >> $pgmout 2> errfile_${RANK}
      err=$?;export err; err_chk
      ${EXECpetss}/petss_post_debufr ${PDY}.shef13 >> $pgmout 2> errfile_${RANK}
      err=$?;export err; err_chk
      cp out/${PDY}.shef12 $COMOUT
      cp out/${PDY}.shef13 $COMOUT
      echo "`date`: Rank ${RANK} finished." > msg_Done_shef1.txt
   else
      echo "`date`: Rank ${RANK} skipped." > msg_Done_shef1.txt
   fi
elif [[ ${RANK} == 1 ]] ; then
   if [ -s ${PDYm1}.shef12 ]; then
      startmsg
      ${EXECpetss}/petss_post_debufr ${PDYm1}.shef12 >> $pgmout 2> errfile_${RANK}
      err=$?;export err; err_chk
      ${EXECpetss}/petss_post_debufr ${PDYm1}.shef13 >> $pgmout 2> errfile_${RANK}
      err=$?;export err; err_chk 

      cp out/${PDYm1}.shef12 $COMOUT
      cp out/${PDYm1}.shef13 $COMOUT
      echo "`date`: Rank ${RANK} finished." > msg_Done_shef2.txt
   else
      echo "`date`: Rank ${RANK} skipped." > msg_Done_shef2.txt
   fi
elif [[ ${RANK} == 2 ]] ; then
   if [ -s ${PDYm2}.shef12 ]; then
      startmsg
      ${EXECpetss}/petss_post_debufr ${PDYm2}.shef12 >> $pgmout 2> errfile_${RANK}
      err=$?;export err; err_chk
      ${EXECpetss}/petss_post_debufr ${PDYm2}.shef13 >> $pgmout 2> errfile_${RANK}
      err=$?;export err; err_chk 

      cp out/${PDYm2}.shef12 $COMOUT
      cp out/${PDYm2}.shef13 $COMOUT
      echo "`date`: Rank ${RANK} finished." > msg_Done_shef3.txt
   else
      echo "`date`: Rank ${RANK} skipped." > msg_Done_shef3.txt
   fi
elif [[ ${RANK} == 3 ]] ; then
   if [ -s ${PDYm3}.shef12 ]; then
      startmsg
      ${EXECpetss}/petss_post_debufr ${PDYm3}.shef12 >> $pgmout 2> errfile_${RANK}
      err=$?;export err; err_chk
      ${EXECpetss}/petss_post_debufr ${PDYm3}.shef13 >> $pgmout 2> errfile_${RANK}
      err=$?;export err; err_chk

      cp out/${PDYm3}.shef12 $COMOUT
      cp out/${PDYm3}.shef13 $COMOUT
      echo "`date`: Rank ${RANK} finished." > msg_Done_shef4.txt
   else
      echo "`date`: Rank ${RANK} skipped." > msg_Done_shef4.txt
   fi
elif [[ ${RANK} == 4 ]] ; then
   if [ -s ${PDYm4}.shef12 ]; then
      startmsg
      ${EXECpetss}/petss_post_debufr ${PDYm4}.shef12 >> $pgmout 2> errfile_${RANK}
      err=$?;export err; err_chk
      ${EXECpetss}/petss_post_debufr ${PDYm4}.shef13 >> $pgmout 2> errfile_${RANK}
      err=$?;export err; err_chk
      cp out/${PDYm4}.shef12 $COMOUT
      cp out/${PDYm4}.shef13 $COMOUT
      echo "`date`: Rank ${RANK} finished." > msg_Done_shef5.txt
   else
      echo "`date`: Rank ${RANK} skipped." > msg_Done_shef5.txt
   fi
elif [[ ${RANK} == 5 ]] ; then
   if [ -s ${PDYm5}.shef12 ]; then
      startmsg
      ${EXECpetss}/petss_post_debufr ${PDYm5}.shef12 >> $pgmout 2> errfile_${RANK}
      err=$?;export err; err_chk
      ${EXECpetss}/petss_post_debufr ${PDYm5}.shef13 >> $pgmout 2> errfile_${RANK}
      err=$?;export err; err_chk
      cp out/${PDYm5}.shef12 $COMOUT
      cp out/${PDYm5}.shef13 $COMOUT
      echo "`date`: Rank ${RANK} finished." > msg_Done_shef6.txt
   else
      echo "`date`: Rank ${RANK} skipped." > msg_Done_shef6.txt
   fi
  touch filler.ss
elif [[ ${RANK} == 6 ]] ; then
   if [ -s ${PDY}.shef05 ]; then
      startmsg
      ${EXECpetss}/petss_post_debufr ${PDY}.shef05 >> $pgmout 2> errfile_${RANK}
      err=$?;export err; err_chk
      echo "`date`: Rank ${RANK} finished." > msg_Done_shef7.txt
      cp out/${PDY}.shef05 $COMOUT
   else
      echo "`date`: Rank ${RANK} skipped." > msg_Done_shef7.txt
   fi
elif [[ ${RANK} == 7 ]] ; then
   if [ -s ${PDYm1}.shef05 ]; then
      startmsg
      ${EXECpetss}/petss_post_debufr ${PDYm1}.shef05 >> $pgmout 2> errfile_${RANK}
      err=$?;export err; err_chk
      echo "`date`: Rank ${RANK} finished." > msg_Done_shef8.txt
      cp out/${PDYm1}.shef05 $COMOUT
   else
      echo "`date`: Rank ${RANK} skipped." > msg_Done_shef8.txt
   fi
elif [[ ${RANK} == 8 ]] ; then
   if [ -s ${PDYm2}.shef05 ]; then
      startmsg
      ${EXECpetss}/petss_post_debufr ${PDYm2}.shef05 >> $pgmout 2> errfile_${RANK}
      err=$?;export err; err_chk
      echo "`date`: Rank ${RANK} finished." > msg_Done_shef9.txt
      cp out/${PDYm2}.shef05 $COMOUT
   else
      echo "`date`: Rank ${RANK} skipped." > msg_Done_shef9.txt
   fi
elif [[ ${RANK} == 9 ]] ; then
   if [ -s ${PDYm3}.shef05 ]; then
      startmsg
      ${EXECpetss}/petss_post_debufr ${PDYm3}.shef05 >> $pgmout 2> errfile_${RANK}
      err=$?;export err; err_chk
      echo "`date`: Rank ${RANK} finished." > msg_Done_shef10.txt
      cp out/${PDYm3}.shef05 $COMOUT
   else
      echo "`date`: Rank ${RANK} skipped." > msg_Done_shef10.txt
   fi
elif [[ ${RANK} == 10 ]] ; then
   if [ -s ${PDYm4}.shef05 ]; then
      startmsg
      ${EXECpetss}/petss_post_debufr ${PDYm4}.shef05 >> $pgmout 2> errfile_${RANK}
      err=$?;export err; err_chk
      echo "`date`: Rank ${RANK} finished." > msg_Done_shef11.txt
      cp out/${PDYm4}.shef05 $COMOUT
   else
      echo "`date`: Rank ${RANK} skipped." > msg_Done_shef11.txt
   fi
elif [[ ${RANK} == 11 ]] ; then
   if [ -s ${PDYm5}.shef05 ]; then
      startmsg
      ${EXECpetss}/petss_post_debufr ${PDYm5}.shef05 >> $pgmout 2> errfile_${RANK}
      err=$?;export err; err_chk
      echo "`date`: Rank ${RANK} finished." > msg_Done_shef12.txt
      cp out/${PDYm5}.shef05 $COMOUT
   else
      echo "`date`: Rank ${RANK} skipped." > msg_Done_shef12.txt
   fi
fi

while [[ ! -f msg_Done_shef1.txt || ! -f msg_Done_shef2.txt || ! -f msg_Done_shef3.txt || ! -f msg_Done_shef4.txt || ! -f msg_Done_shef5.txt || ! -f msg_Done_shef6.txt || ! -f msg_Done_shef7.txt || ! -f msg_Done_shef8.txt || ! -f msg_Done_shef9.txt || ! -f msg_Done_shef10.txt || ! -f msg_Done_shef11.txt || ! -f msg_Done_shef12.txt ]] ; do
  echo "`date`: Rank ${RANK} Sleeping." >> timing.txt
  sleep 1
done

if [ $SENDCOM = "YES" ] ; then
   if [[ ${RANK} == 0 ]] ; then

      cpreq fns_ss.txt $COMOUT
      cpreq fns_obs.txt $COMOUT
      cpreq datelist $COMOUT
      cpreq HSBY $COMOUT
   fi
fi

msg="$job completed normally"
postmsg "$jlogfile" "$msg"
