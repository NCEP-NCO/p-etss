#!/bin/bash
# ###########################################################################################
# Author: Huiqing Liu (Huiqing.Liu@noaa.gov)                                      09-24-2021
#
# Abstract:
#   Creat poe-script and submit the jobs to draw gridded products image
#
# Parameters:
#  Inputs:  Various environmental variables defined either in ecflow or in the
#           j-job jobs/JETSS
#  Outputs: poe-script - list of running scripts for mpiexec
#           - ${COMOUT}/petss.t00z.img.tar.gz
# ###########################################################################################

#############################################################################################
# 09-24-2021: Created by Huiqing Liu
#############################################################################################

set -x
#==============================================================================
# Create poe-script by iterating over the RANK
#------------------------------------------------------------------------------
rm -f poeDrawImage
for (( c=0; c<294; c++ )) ; do
    echo "${USHpetss:?}/petss_drawimages.sh ${c}" >> poeDrawImage
done

chmod 755 poeDrawImage
mpiexec -n 294 -ppn 128 --cpu-bind core cfp poeDrawImage
export err=$?; err_chk

#####################################################################
# GOOD RUN
set +x
echo "**************JOB P-ETSS_DRAWIMAGE COMPLETED NORMALLY"
echo "**************JOB P-ETSS_DRAWIMAGE COMPLETED NORMALLY"
echo "**************JOB P-ETSS_DRAWIMAGE COMPLETED NORMALLY"
set -x
#####################################################################

