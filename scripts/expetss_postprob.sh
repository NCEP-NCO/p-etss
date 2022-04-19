#!/bin/bash
#
set +x
echo " ---------------------------------------------------------"
echo "  "
echo "              PROBABILISTIC EXTRA-TROPICAL STORM SURGE"
echo "  "
echo "  "
echo "                   JOB P-ETSS "
echo "  "
echo "          ANALYSIS CYCLE TIME IS .. $CYCLE"
echo "  "
echo "  "
echo " ---------------------------------------------------------"
echo " ---------------------------------------------------------"
# Author: Huiqing Liu (Huiqing.Liu@noaa.gov)
# Abstract:
#   Creat poe-script and submit the jobs to run the P-ETSS model stage
#
# " 09/30/21 - The First Version of P-ETSS1.2 for WCOSS 2
# " ---------------------------------------------------------"
# Parameters:
#  Inputs:  Various environmental variables defined either in ecflow or in the
#           j-job jobs/JPETSS
#  Outputs: poe-script - list of running scripts for mpiexec
#           P-ETSS text products - ${COMOUT}/petss.t${cyc}z.storm*.txt
#           P-ETSS native resolution gridded data - ${DATA}/fle*
set -x
#==============================================================================
# Create poe-script by iterating over the RANK
#------------------------------------------------------------------------------
rm -f poePostprob
RANKO=-1
for (( c1=0; c1<274; c1++ )) ; do
  echo "${USHpetss:?}/petss_postprob.sh ${c1}" >> poePostprob
done

chmod 755 poePostprob
mpiexec -n 274 -ppn 128 --cpu-bind core cfp poePostprob
export err=$?; err_chk

#####################################################################
# GOOD RUN
set +x
echo "**************JOB P-ETSS_POSTPROB COMPLETED NORMALLY"
echo "**************JOB P-ETSS_POSTPROB COMPLETED NORMALLY"
echo "**************JOB P-ETSS_POSTPROB COMPLETED NORMALLY"
set -x
#####################################################################

