#!/bin/bash
# ###########################################################################################
# expetss_prewnd.sh.ecf
# ###########################################################################################
# Author: Huiqing Liu (Huiqing.Liu@noaa.gov)                                         09/30/21
# Abstract:
#   Creat poe-script and submit the jobs to prepare wind stage
#
# Parameters:
#  Inputs:  Various environmental variables defined either in ecflow or in the
#           j-job jobs/JPETSS_PREWND
#  Outputs: poe-script - list of running scripts for mpiexec
#           WIND files for each of basins - ${COMOUT}/gfspuv.${cyc}* ${COMOUT}/cylf10${cyc}*
#
#############################################################################################
# 09-30-2021: Created by Huiqing Liu
#############################################################################################
set -x
#==============================================================================
# Create poe-script by iterating over the RANK
#------------------------------------------------------------------------------
rm -f poePreWnd
RANKO=-1
for (( c1=0; c1<52; c1++ )) ; do
  if [ ${c1} -lt 31 ] ; then
    if [ ${c1} -eq 0 ]; then	 
      WND=gec
    else
      WND=gep
    fi      
    if [ ${c1} -lt 10 ] ; then
      PEP=0${c1}
    else
      PEP=${c1}
    fi  
  else
    if [ ${c1} -eq 31 ]; then
      WND=cmc_gec	    
    else
      WND=cmc_gep	    
    fi
    if [ ${c1} -lt 41 ] ; then
      temp=$((c1-31))	    
      PEP=0${temp}
    else    
      PEP=$((c1-31))
    fi  
  fi	  
  for (( c2=0; c2<2; c2++ )) ; do
    RANKO=$((RANKO+1))
    echo "${USHpetss:?}/petss_prewnd.sh ${RANKO} ${c2} ${WND} ${PEP}" >> poePreWnd
  done  
done

chmod 755 poePreWnd
mpiexec -n 104 -ppn 104 --cpu-bind core cfp poePreWnd
export err=$?; err_chk

#####################################################################
# GOOD RUN
set +x
echo "**************JOB P-ETSS_PRE_WND COMPLETED NORMALLY"
echo "**************JOB P-ETSS_PRE_WND COMPLETED NORMALLY"
echo "**************JOB P-ETSS_PRE_WND COMPLETED NORMALLY"
set -x
#####################################################################

