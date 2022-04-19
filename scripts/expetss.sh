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
rm -f poeSLOSH
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
  echo 'Y' > sds.${WND}${PEP}.${cyc}
  for (( c2=0; c2<14; c2++ )) ; do
    RANKO=$((RANKO+1))
    echo "${USHpetss:?}/petss_model.sh ${RANKO} ${c2} ${WND} ${PEP}" >> poeSLOSH
  done
done

cpreq $PARMpetss/model/grid_depth_nep.txt grid_depth_nep.txt
cpreq $PARMpetss/model/water-levels.dat ./

if [[ ${cyc} == 00 ]]; then
  oldcyc=18
  cp ${COMINSS}/${RUN}.${PDYm1}/t${oldcyc}z_csv/*.csv ./
  cp ${COMINSS}/${RUN}.${PDYm1}/petss.t${oldcyc}z.init_wl.txt ./
else
  if [[ ${cyc} == 06 ]]; then
    oldcyc=00
  elif [[ ${cyc} == 12 ]]; then
    oldcyc=06
  else
    oldcyc=12
  fi
  cp ${COMINSS}/${RUN}.${PDY}/t${oldcyc}z_csv/*.csv ./
  cp ${COMINSS}/${RUN}.${PDY}/petss.t${oldcyc}z.init_wl.txt ./
fi

#cpreq ${COMINSS}/${RUN}.${PDY}/gfspuv.* ./
#cpreq ${COMINSS}/${RUN}.${PDY}/cylf10.* ./
#rm ${COMINSS}/${RUN}.${PDY}/gfspuv.*
#rm ${COMINSS}/${RUN}.${PDY}/cylf10.*

if [[ -s petss.t${oldcyc}z.init_wl.txt ]] ; then
  { read
    while IFS=, read -r f c d ; do
      myfn=`echo $c`
      for fnid in $(seq -f "%02g" 1 31); do
        echo $f > wl.cmc_gep$fnid.surge.$myfn
        echo $f > wl.cmc_gep$fnid.surge_tide.$myfn
        echo 0 > wl.cmc_gep$fnid.tide.$myfn
        echo $f > wl.gep$fnid.surge.$myfn
        echo $f > wl.gep$fnid.surge_tide.$myfn
        echo 0 > wl.gep$fnid.tide.$myfn
        if [[ ${myfn} == hmi3 ]]; then
          echo $f > wl.cmc_gep$fnid.surge.pb3
          echo $f > wl.cmc_gep$fnid.surge_tide.pb3
          echo $f > wl.gep$fnid.surge.pb3
          echo $f > wl.gep$fnid.surge_tide.pb3
        fi
      done
      echo $f > wl.cmc_gec00.surge.$myfn
      echo $f > wl.cmc_gec00.surge_tide.$myfn
      echo 0 > wl.cmc_gec00.tide.$myfn
      echo $f > wl.gec00.surge.$myfn
      echo $f > wl.gec00.surge_tide.$myfn
      echo 0 > wl.gec00.tide.$myfn
      if [[ ${myfn} == hmi3 ]]; then
        echo $f > wl.cmc_gec00.surge.pb3
        echo $f > wl.cmc_gec00.surge_tide.pb3
        echo $f > wl.gec00.surge.pb3
        echo $f > wl.gec00.surge_tide.pb3
      fi
    done
  }  < petss.t${oldcyc}z.init_wl.txt
else
  msg="$job Warning:  No previous initial water condition file found, use zero as the initial water condition"
  postmsg "$jlogfile" "$msg"
fi

chmod 755 poeSLOSH
mpiexec -n 728 -ppn 128 --cpu-bind core cfp poeSLOSH
export err=$?; err_chk

#####################################################################
# GOOD RUN
set +x
echo "**************JOB P-ETSS_MODEL COMPLETED NORMALLY"
echo "**************JOB P-ETSS_MODEL COMPLETED NORMALLY"
echo "**************JOB P-ETSS_MODEL COMPLETED NORMALLY"
set -x
#####################################################################

