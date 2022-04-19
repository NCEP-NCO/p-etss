#!/bin/bash
# ###########################################################################################
# Author: Huiqing Liu (Huiqing.Liu@noaa.gov)                                      09-24-2021
#
# Abstract:
#   Creat poe-script and submit the jobs to collect bufr, and station surgeOnly data in
#   post-processing stage
#
# Parameters:
#  Inputs:  Various environmental variables defined either in ecflow or in the
#           j-job jobs/JETSS
#  Outputs: poe-script - list of running scripts for mpiexec
# ###########################################################################################

#############################################################################################
# 09-24-2021: Created by Huiqing Liu
#############################################################################################

set -x
#==============================================================================
# Create poe-script by iterating over the RANK
#------------------------------------------------------------------------------
rm -f poeParsedat
for (( c=0; c<12; c++ )) ; do
    echo "${USHpetss:?}/petss_parsedat.sh ${c}" >> poeParsedat
done

chmod 755 poeParsedat
mpiexec -n 12 -ppn 12 --cpu-bind core cfp poeParsedat
export err=$?; err_chk

#####################################################################
# GOOD RUN
set +x
echo "**************JOB P-ETSS_PARSEDAT COMPLETED NORMALLY"
echo "**************JOB P-ETSS_PARSEDAT COMPLETED NORMALLY"
echo "**************JOB P-ETSS_PARSEDAT COMPLETED NORMALLY"
set -x
#####################################################################

