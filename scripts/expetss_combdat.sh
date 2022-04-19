#!/bin/sh

# ###########################################################################################
# Author: Huiqing Liu (Huiqing.Liu@noaa.gov)                                      09-24-2021
#
# Abstract:
#   Creat poe-script and submit the jobs to create obs, surge, and tide grid data in
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

set -xa
#==============================================================================
# Create poe-script by iterating over the RANK
#------------------------------------------------------------------------------
rm -f poeCombdat
for (( c=0; c<2; c++ )) ; do
    echo "${USHpetss:?}/petss_combdat.sh ${c}" >> poeCombdat
done

chmod 755 poeCombdat
mpiexec -n 2 -ppn 2 --cpu-bind core cfp poeCombdat
export err=$?; err_chk

#####################################################################
# GOOD RUN
set +x
echo "**************JOB P-ETSS_COMBDAT COMPLETED NORMALLY"
echo "**************JOB P-ETSS_COMBDAT COMPLETED NORMALLY"
echo "**************JOB P-ETSS_COMBDAT COMPLETED NORMALLY"
set -x
#####################################################################

