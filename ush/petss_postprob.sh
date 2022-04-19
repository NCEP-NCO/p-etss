#!/bin/bash
# ###########################################################################################
# petss_modelprob.sh
# ###########################################################################################
# Author: Huiqing Liu (Huiqing.Liu@noaa.gov)                                      10-01-2021
# 1) Prepare probabilistic storm surge data above the threshold on the native grid 
#    after model runs, which will be used by merging stage.
# 2) Generate the probabilistic station products
#
#  Inputs:  1) model output at native grid and station locations. 
#
#  Outputs: text and native resolution grid probabilistic products 

#############################################################################################
# 10-01-2021: Created by Huiqing Liu
#############################################################################################

###################################################################
#  Post process the basin (34 basins) runs for native grids.
####################################################################

set -x
export RANKO=$1

echo "`date`: Start Rank ${RANKO}" >> timing.txt

msg="Begin job for $job"
postmsg "$jlogfile" "$msg"

if [ $RANKO -lt 136 ] ; then

   echo "`date`: Rank ${RANKO} Generate Probabilistic Grid products above model datum" >> timing.txt
   if [[ -s ${COMOUT}/PETSS_GEFS_${cyc} ]]; then
      if [[ ${RUN_NewGEFS} == YES ]] ; then
         ${USHpetss}/petss_prob_gefs_new.sh $RANKO DATUM
      else
         ${USHpetss}/petss_prob_gefs.sh $RANKO DATUM
      fi
   else
      if [[ ${WIND} == GEFS ]] ; then
         if [[ ${RUN_NewGEFS} == YES ]] ; then
            ${USHpetss}/petss_prob_gefs_new.sh $RANKO DATUM
         else
            ${USHpetss}/petss_prob_gefs.sh $RANKO DATUM
         fi
      else
         if [[ ${RUN_NewGEFS} == YES ]] ; then
            ${USHpetss}/petss_prob_naefs_new.sh $RANKO DATUM
         else
            ${USHpetss}/petss_prob_naefs.sh $RANKO DATUM
         fi
      fi
   fi
   echo "`date`: RANK ${RANKO} Finished generating grid probabilistic products above model datum " >> timing.txt

   echo "RANK ${RANKO} above model datum " >> msn_grid_datum.txt

elif [ $RANKO -lt 272 ] ; then

   RK=$((RANKO-136))

   echo "`date`: Rank ${RANKO} Generate Probabilistic Grid products Above Ground Level" >> timing.txt
   if [[ -s ${COMOUT}/PETSS_GEFS_${cyc} ]]; then
      if [[ ${RUN_NewGEFS} == YES ]] ; then
         ${USHpetss}/petss_prob_gefs_new.sh $RK AGL
      else
         ${USHpetss}/petss_prob_gefs.sh $RK AGL
      fi
   else
      if [[ ${WIND} == GEFS ]] ; then
         if [[ ${RUN_NewGEFS} == YES ]] ; then
            ${USHpetss}/petss_prob_gefs_new.sh $RK AGL
         else
            ${USHpetss}/petss_prob_gefs.sh $RK AGL
         fi
      else
         if [[ ${RUN_NewGEFS} == YES ]] ; then
            ${USHpetss}/petss_prob_naefs_new.sh $RK AGL
         else
            ${USHpetss}/petss_prob_naefs.sh $RK AGL
         fi
      fi
   fi

   echo "`date`: RANK ${RANKO} Finished generating grid probabilistic products Above Ground Level " >> timing.txt

   echo "RANK ${RANKO} above Ground Level " >> msn_grid_agl.txt

fi
#Generating station products
if [[ ${RANKO} == 272 ]] ; then
#Generating NAEFS station products

   if [[ -s ${COMOUT}/PETSS_GEFS_${cyc} ]]; then
      if [[ ${RUN_NewGEFS} == YES ]] ; then
         ${USHpetss}/petss_prob_stn_gefs_new.sh
      else
         ${USHpetss}/petss_prob_stn_gefs.sh
      fi
   else
      if [ ${cpuNUM} -eq 588 ] ; then
         ${USHpetss}/petss_prob_stn_naefs.sh
      elif [ ${cpuNUM} -eq 728 ] ; then
         ${USHpetss}/petss_prob_stn_naefs_new.sh
      elif [ ${cpuNUM} -eq 294 ] ; then
         ${USHpetss}/petss_prob_stn_gefs.sh
      elif [ ${cpuNUM} -eq 434 ] ; then
         ${USHpetss}/petss_prob_stn_gefs_new.sh
      fi
   fi

elif [[ ${RANKO} == 273 ]] ; then
#Generating GEFS Only station products
   if [[ ! -s ${COMOUT}/PETSS_GEFS_${cyc} ]]; then

      if [[ ${RUN_NewGEFS} == YES ]] ; then
         ${USHpetss}/petss_prob_stn_gefs_new.sh
      else
         ${USHpetss}/petss_prob_stn_gefs.sh
      fi
   fi
fi

