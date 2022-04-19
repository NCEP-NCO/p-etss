#!/bin/bash
# ###########################################################################################
# petss_prewnd.sh
# ###########################################################################################
# Author: Huiqing Liu (Huiqing.Liu@noaa.gov)                                      09-30-2021
# 1) Prepare data before running program to extract current and
#    forecast (future) ensemble wind products surface pressure, and U and V 
#    WIND vector fields.
# 2) Prepare data before running program to extract Past (historic)
#    ensemble wind products surface pressure, and U and V wind vector fields.
#    Needs 60 hours back...
#    (ie cycle 0Z day 0 needs back to cycle 12Z day -3)
#    Use PDYm1,PDYm2.... for -1 day, -2 day ...
# ###########################################################################################

#############################################################################################
# 09-30-2021: Created by Huiqing Liu
#############################################################################################
set -x
export RANKO=$1
export RANK=$2
export WNDS=$3
export PEP=$4

echo "`date`: Start Rank ${RANKO} ${RANK} ${WNDS} ${PEP}" >> timing.txt

msg="Begin job for $job"
postmsg "$jlogfile" "$msg"


if [[ ${RANKO} == 0 ]] ; then

  echo "DATE=${PDY}_${cyc}" > ${COMOUT}/petss.t${cyc}z.meta.txt
  echo "STATUS=GOOD" >> ${COMOUT}/petss.t${cyc}z.meta.txt
  echo "FORCING=NAEFS" >> ${COMOUT}/petss.t${cyc}z.meta.txt
  echo "COMMENT=P-ETSS results are from NAEFS forcing without any warning" >> ${COMOUT}/petss.t${cyc}z.meta.txt

fi

if [[ ${cyc} == 06 || ${cyc} == 18 ]] ;then
  if [[ ${WNDS} == gep || ${WNDS} == gec ]] ;then # GEFS
    wndshr=102
  else
    wndshr=108
  fi
  if [[ ${cyc} == 06 ]];then
    cyc_cmc=00
    cyc_cmc_old=12
  else
    cyc_cmc=12
    cyc_cmc_old=00
  fi
else
  wndshr=102
  cyc_cmc=${cyc}
  if [[ ${cyc} == 00 ]];then
    cyc_cmc_old=12
  else
    cyc_cmc_old=00
  fi
fi
cycle_cmc=t${cyc_cmc}z

if [[ ${RUN_NewGEFS} == YES ]] ; then
  GEFS_Res='0p25'
  cycatmos=${cyc}/atmos/pgrb2sp25
  GEFS_Fn='pgrb2s'
else
  GEFS_Res='0p50'
  cycatmos=${cyc}/pgrb2ap5
  GEFS_Fn='pgrb2a'
fi

if [[ ${RANK} == 0 ]] ; then

  for i in $(seq -f "%03g" 0 3 $wndshr); do
    if [[ ${WNDS} == gep || ${WNDS} == gec ]] ;then # GEFS
      Start=$SECONDS
      while [[ ! -s ${COMINgefs}/${cycatmos}/${WNDS}${PEP}.${cycle}.${GEFS_Fn}.${GEFS_Res}.f$i ]]; do
        Dura=$((SECONDS-Start))
        if [ $Dura -ge 600 ]; then
          msg="$job Error: Wind file ${COMINgefs}/${cycatmos}/${WNDS}${PEP}.${cycle}.${GEFS_Fn}.${GEFS_Res}.f$i is missed after 10 mins wait"
          postmsg "$jlogfile" "$msg"
          err_exit
          break
        fi  
        sleep 1
      done
      cpreq  ${COMINgefs}/${cycatmos}/${WNDS}${PEP}.${cycle}.${GEFS_Fn}.${GEFS_Res}.f$i  ${WNDS}${PEP}fore$i
    else
      if [[ ${RUN_ENVIR} == "MDLTEST" ]] ; then
        cpreq  ${COMINcmce}/${cyc_cmc}/pgrb2ap5/${WNDS}${PEP}.${cycle_cmc}.pgrb2a.0p50.f$i  ${WNDS}${PEP}fore$i
      else
        if [[ ! -s ${COMINcmce}/${PDY}${cyc_cmc}_CMC_naefs_hr_latlon0p5x0p5_P${i}_0${PEP}.grib2 ]]; then
          for iold in $(seq -f "%03g" $(expr $i + 12) $(expr $i + 12)); do
            if [[ ${cyc} == 00 || ${cyc} == 06 ]] ;then
              if [[ ! -s ${COMINcmcem1}/${PDYm1}${cyc_cmc_old}_CMC_naefs_hr_latlon0p5x0p5_P${iold}_0${PEP}.grib2 ]]; then
                msg="Warning: Two consecutive CMC cycles of forecast wind data are missing."
                msg="Warning:    Missing file: ${COMINcmce}/${PDY}${cyc_cmc}_CMC_naefs_hr_latlon0p5x0p5_P${i}_0${PEP}.grib2"
                msg="Warning:    Missing file: ${COMINcmcem1}/${PDYm1}${cyc_cmc_old}_CMC_naefs_hr_latlon0p5x0p5_P${iold}_0${PEP}.grib2"
                msg="Warning: P-ETSS is running with GEFS only forcing."
                postmsg "$jlogfile" "$msg"
                echo "$job Warning: P-ETSS results are from GEFS only due to two CMC cycles of missing forecast wind data." > ${COMOUT}/PETSS_GEFS_${cyc}
              else
                cpreq  ${COMINcmcem1}/${PDYm1}${cyc_cmc_old}_CMC_naefs_hr_latlon0p5x0p5_P${iold}_0${PEP}.grib2 ${WNDS}${PEP}fore$i
                msg="Warning: One CMC cycle of forecast wind data is missing."
                msg="Warning:    Missing file: ${COMINcmce}/${PDY}${cyc_cmc}_CMC_naefs_hr_latlon0p5x0p5_P${i}_0${PEP}.grib2"
                msg="Warning: Using ${COMINcmcem1}/${PDYm1}${cyc_cmc_old}_CMC_naefs_hr_latlon0p5x0p5_P${iold}_0${PEP}.grib2 to fill in the gap." 
                postmsg "$jlogfile" "$msg"
                echo "$job Warning: P-ETSS results are from NAEFS, but used the previous CMC cycle forecast wind data." > ${COMOUT}/PETSS_NAEFS1_${cyc}
              fi
            else
              if [[ ! -s ${COMINcmce}/${PDY}${cyc_cmc_old}_CMC_naefs_hr_latlon0p5x0p5_P${iold}_0${PEP}.grib2 ]]; then
                msg="Warning: Two consecutive CMC cycles of forecast wind data are missing."
                msg="Warning:    Missing file: ${COMINcmce}/${PDY}${cyc_cmc}_CMC_naefs_hr_latlon0p5x0p5_P${i}_0${PEP}.grib2"
                msg="Warning:    Missing file: ${COMINcmce}/${PDY}${cyc_cmc_old}_CMC_naefs_hr_latlon0p5x0p5_P${iold}_0${PEP}.grib2"
                msg="Warning: P-ETSS is running with GEFS only forcing."
                postmsg "$jlogfile" "$msg"
                echo "$job Warning: P-ETSS results are from GEFS only due to two CMC cycles of missing forecast wind data." > ${COMOUT}/PETSS_GEFS_${cyc}
              else
                cpreq  ${COMINcmce}/${PDY}${cyc_cmc_old}_CMC_naefs_hr_latlon0p5x0p5_P${iold}_0${PEP}.grib2 ${WNDS}${PEP}fore$i
                msg="Warning: One CMC cycle of forecast wind data is missing."
                msg="Warning:    Missing file: ${COMINcmce}/${PDY}${cyc_cmc}_CMC_naefs_hr_latlon0p5x0p5_P${i}_0${PEP}.grib2"
                msg="Warning: Using ${COMINcmce}/${PDY}${cyc_cmc_old}_CMC_naefs_hr_latlon0p5x0p5_P${iold}_0${PEP}.grib2 to fill in the gap."
                postmsg "$jlogfile" "$msg"
                echo "$job Warning: P-ETSS results are from NAEFS, but used the previous CMC cycle forecast wind data." > ${COMOUT}/PETSS_NAEFS1_${cyc}
              fi
            fi
          done
        else
          cpreq  ${COMINcmce}/${PDY}${cyc_cmc}_CMC_naefs_hr_latlon0p5x0p5_P${i}_0${PEP}.grib2 ${WNDS}${PEP}fore$i
        fi
      fi
    fi
  done

  echo "`date`: Finished first file copies" >> timing.txt
  echo " finished first file copies" > msg_Done_first_cp_${WNDS}${PEP}.txt

elif [[ ${RANK} == 1 ]] ; then

######################################################################
#  Prepare data before running program to extract Past (historic)
#    global surface pressure, and U and V wind vector fields.
#  Needs 60 hours back...
#    (ie cycle 0Z day 0 needs back to cycle 12Z day -3)
#  Use PDYm1,PDYm2.... for -1 day, -2 day ...
#  The reason this is so long is to keep it simple.
######################################################################

  if [[ ${RUN_NewGEFS} == YES && ${WNDS} == gep || ${RUN_NewGEFS} == YES && ${WNDS} == gec ]] ; then
    CYC1='00/atmos/pgrb2sp25'
    CYC2='06/atmos/pgrb2sp25'
    CYC3='12/atmos/pgrb2sp25'
    CYC4='18/atmos/pgrb2sp25'
  elif [[ ${WNDS} == gep || ${WNDS} == gec ]] ; then
    CYC1='00/pgrb2ap5'
    CYC2='06/pgrb2ap5'
    CYC3='12/pgrb2ap5'
    CYC4='18/pgrb2ap5'
  else
    CYC1='00'
    CYC2='06'
    CYC3='12'
    CYC4='18'
  fi
  WNDRES='.0p50.'
  CYCLE1='t00z'
  CYCLE2='t06z'
  CYCLE3='t12z'
  CYCLE4='t18z'

# Files needed by all cycles (0,6,12,18)...
#     6Z of -2 day to 18Z of -1 day
  if [[ ${WNDS} == gep || ${WNDS} == gec ]] ;then
    if [[ ${RUN_RETRO} == YES ]] ; then
      cpreq  $COMINgefsm2/${CYC3}/${WNDS}${PEP}.$CYCLE3.${GEFS_Fn}.${GEFS_Res}.f000  ${WNDS}${PEP}hind212
      cpreq  $COMINgefsm1/${CYC1}/${WNDS}${PEP}.$CYCLE1.${GEFS_Fn}.${GEFS_Res}.f000  ${WNDS}${PEP}hind100
      cpreq  $COMINgefsm1/${CYC3}/${WNDS}${PEP}.$CYCLE3.${GEFS_Fn}.${GEFS_Res}.f000  ${WNDS}${PEP}hind112
    else
      Start=$SECONDS
      while [[ ! -s $COMINgefsm2/${CYC2}/${WNDS}${PEP}.$CYCLE2.${GEFS_Fn}.${GEFS_Res}.f000 || ! -s $COMINgefsm2/${CYC3}/${WNDS}${PEP}.$CYCLE3.${GEFS_Fn}.${GEFS_Res}.f000 || ! -s $COMINgefsm2/${CYC4}/${WNDS}${PEP}.$CYCLE4.${GEFS_Fn}.${GEFS_Res}.f000 || ! -s $COMINgefsm1/${CYC1}/${WNDS}${PEP}.$CYCLE1.${GEFS_Fn}.${GEFS_Res}.f000 || ! -s $COMINgefsm1/${CYC2}/${WNDS}${PEP}.$CYCLE2.${GEFS_Fn}.${GEFS_Res}.f000 || ! -s $COMINgefsm1/${CYC3}/${WNDS}${PEP}.$CYCLE3.${GEFS_Fn}.${GEFS_Res}.f000 || ! -s $COMINgefsm1/${CYC4}/${WNDS}${PEP}.$CYCLE4.${GEFS_Fn}.${GEFS_Res}.f000 ]]; do
        Dura=$((SECONDS-Start))
        if [ $Dura -ge 600 ]; then
          msg="$job Error: Hindcast GEFS Wind file is missed after 10 mins wait in all cycles"
          echo "Missed one of the following wind files."
          echo "$COMINgefsm2/${CYC2}/${WNDS}${PEP}.$CYCLE2.${GEFS_Fn}.${GEFS_Res}.f000"
          echo "$COMINgefsm2/${CYC3}/${WNDS}${PEP}.$CYCLE3.${GEFS_Fn}.${GEFS_Res}.f000"
          echo "$COMINgefsm2/${CYC4}/${WNDS}${PEP}.$CYCLE4.${GEFS_Fn}.${GEFS_Res}.f000"
          echo "$COMINgefsm1/${CYC1}/${WNDS}${PEP}.$CYCLE1.${GEFS_Fn}.${GEFS_Res}.f000"
          echo "$COMINgefsm1/${CYC2}/${WNDS}${PEP}.$CYCLE2.${GEFS_Fn}.${GEFS_Res}.f000"
          echo "$COMINgefsm1/${CYC3}/${WNDS}${PEP}.$CYCLE3.${GEFS_Fn}.${GEFS_Res}.f000"
          echo "$COMINgefsm1/${CYC4}/${WNDS}${PEP}.$CYCLE4.${GEFS_Fn}.${GEFS_Res}.f000"   
          postmsg "$jlogfile" "$msg"
          err_exit
        fi  
        sleep 1
      done
      cpreq  $COMINgefsm2/${CYC2}/${WNDS}${PEP}.$CYCLE2.${GEFS_Fn}.${GEFS_Res}.f000  ${WNDS}${PEP}hind206
      cpreq  $COMINgefsm2/${CYC3}/${WNDS}${PEP}.$CYCLE3.${GEFS_Fn}.${GEFS_Res}.f000  ${WNDS}${PEP}hind212
      cpreq  $COMINgefsm2/${CYC4}/${WNDS}${PEP}.$CYCLE4.${GEFS_Fn}.${GEFS_Res}.f000  ${WNDS}${PEP}hind218
      cpreq  $COMINgefsm1/${CYC1}/${WNDS}${PEP}.$CYCLE1.${GEFS_Fn}.${GEFS_Res}.f000  ${WNDS}${PEP}hind100
      cpreq  $COMINgefsm1/${CYC2}/${WNDS}${PEP}.$CYCLE2.${GEFS_Fn}.${GEFS_Res}.f000  ${WNDS}${PEP}hind106
      cpreq  $COMINgefsm1/${CYC3}/${WNDS}${PEP}.$CYCLE3.${GEFS_Fn}.${GEFS_Res}.f000  ${WNDS}${PEP}hind112
      cpreq  $COMINgefsm1/${CYC4}/${WNDS}${PEP}.$CYCLE4.${GEFS_Fn}.${GEFS_Res}.f000  ${WNDS}${PEP}hind118
    fi
  else # CMC
    if [[ ${RUN_ENVIR} == "MDLTEST" ]] ; then
      cpreq  $COMINcmcem2/${CYC3}/pgrb2ap5/${WNDS}${PEP}.$CYCLE3.pgrb2a${WNDRES}f000  ${WNDS}${PEP}hind212
      cpreq  $COMINcmcem1/${CYC1}/pgrb2ap5/${WNDS}${PEP}.$CYCLE1.pgrb2a${WNDRES}f000  ${WNDS}${PEP}hind100
      cpreq  $COMINcmcem1/${CYC3}/pgrb2ap5/${WNDS}${PEP}.$CYCLE3.pgrb2a${WNDRES}f000  ${WNDS}${PEP}hind112
    else
      if [[ ! -s $COMINcmcem2/${PDYm2}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2 ]]; then
        if [[ ! -s $COMINcmcem2/${PDYm2}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2 ]]; then
          if [[ ! -s $COMINcmcem3/${PDYm3}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P024_0${PEP}.grib2 ]]; then
            if [[ ! -s $COMINcmcem3/${PDYm3}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P036_0${PEP}.grib2 ]]; then
              if [[ ! -s $COMINcmcem4/${PDYm4}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P048_0${PEP}.grib2 ]]; then
                msg="Warning: Five consecutive CMC cycles hindacast wind data are missing."
                msg="Warning:    Missing file: $COMINcmcem2/${PDYm2}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2"
                msg="Warning:    Missing file: $COMINcmcem2/${PDYm2}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2"
                msg="Warning:    Missing file: $COMINcmcem3/${PDYm3}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P024_0${PEP}.grib2"
                msg="Warning:    Missing file: $COMINcmcem3/${PDYm3}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P036_0${PEP}.grib2"
                msg="Warning:    Missing file: $COMINcmcem4/${PDYm4}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P048_0${PEP}.grib2"
                msg="Warning: P-ETSS is running with GEFS only forcing."
                postmsg "$jlogfile" "$msg"
                echo "$job Warning: P-ETSS results are from GEFS only due to five CMC cycles of missing hindacast wind data." > ${COMOUT}/PETSS_GEFS_${cyc}
              else
                msg="Warning: Four consecutive CMC cycle hindcast wind data are missing."
                msg="Warning:    Missing file: $COMINcmcem2/${PDYm2}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2"
                msg="Warning:    Missing file: $COMINcmcem2/${PDYm2}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2"
                msg="Warning:    Missing file: $COMINcmcem3/${PDYm3}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P024_0${PEP}.grib2"
                msg="Warning:    Missing file: $COMINcmcem3/${PDYm3}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P036_0${PEP}.grib2"
                msg="Warning: Using $COMINcmcem4/${PDYm4}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P048_0${PEP}.grib2 to fill in the gap."
                postmsg "$jlogfile" "$msg"
                echo "$job Warning: P-ETSS results are from NAEFS, but used the previous 4 CMC cycle wind data in spinup part." > ${COMOUT}/PETSS_NAEFS_${cyc}
                cpreq  $COMINcmcem4/${PDYm4}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P048_0${PEP}.grib2 ${WNDS}${PEP}hind212
              fi
            else
              msg="Warning: Three consecutive CMC cycle hindcast wind data are missing."
              msg="Warning:    Missing file: $COMINcmcem2/${PDYm2}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2"
              msg="Warning:    Missing file: $COMINcmcem2/${PDYm2}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2"
              msg="Warning:    Missing file: $COMINcmcem3/${PDYm3}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P024_0${PEP}.grib2"
              msg="Warning: Using $COMINcmcem3/${PDYm3}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P036_0${PEP}.grib2 to fill in the gap."
              postmsg "$jlogfile" "$msg"
              echo "$job Warning: P-ETSS results are from NAEFS, but used the previous 3 CMC cycle wind data in spinup part." > ${COMOUT}/PETSS_NAEFS_${cyc}
              cpreq  $COMINcmcem3/${PDYm3}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P036_0${PEP}.grib2 ${WNDS}${PEP}hind212
            fi
          else
            msg="Warning: Two consecutive CMC cycle hindcast wind data are missing."
            msg="Warning:    Missing file: $COMINcmcem2/${PDYm2}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2"
            msg="Warning:    Missing file: $COMINcmcem2/${PDYm2}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2"
            msg="Warning: Using $COMINcmcem3/${PDYm3}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P024_0${PEP}.grib2 to fill in the gap."
            postmsg "$jlogfile" "$msg"
            echo "$job Warning: P-ETSS results are from NAEFS, but used the previous 2 CMC cycle wind data in spinup part." > ${COMOUT}/PETSS_NAEFS_${cyc}
            cpreq  $COMINcmcem3/${PDYm3}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P024_0${PEP}.grib2 ${WNDS}${PEP}hind212
          fi
        else
          msg="Warning: One CMC cycle hindcast wind data is missing."
          msg="Warning:    Mising file: $COMINcmcem2/${PDYm2}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2"
          msg="Warning: Using $COMINcmcem2/${PDYm2}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2 to fill in the gap."
          postmsg "$jlogfile" "$msg"
          echo "$job Warning: P-ETSS results are from NAEFS, but used the previous CMC cycle wind data in spinup part." > ${COMOUT}/PETSS_NAEFS_${cyc}
          cpreq  $COMINcmcem2/${PDYm2}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2 ${WNDS}${PEP}hind212
        fi
      else
        cpreq  $COMINcmcem2/${PDYm2}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2 ${WNDS}${PEP}hind212
      fi
      if [[ ! -s $COMINcmcem1/${PDYm1}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2 ]]; then
        if [[ ! -s $COMINcmcem2/${PDYm2}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2 ]]; then
          if [[ ! -s $COMINcmcem2/${PDYm2}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P024_0${PEP}.grib2 ]]; then
            if [[ ! -s $COMINcmcem3/${PDYm3}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P036_0${PEP}.grib2 ]]; then
              if [[ ! -s $COMINcmcem3/${PDYm3}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P048_0${PEP}.grib2 ]]; then
                msg="Warning: Five consecutive CMC cycle hindcast wind data are missing."
                msg="Warning:    Missing file: $COMINcmcem1/${PDYm1}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2"
                msg="Warning:    Missing file: $COMINcmcem2/${PDYm2}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2"
                msg="Warning:    Missing file: $COMINcmcem2/${PDYm2}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P024_0${PEP}.grib2"
                msg="Warning:    Missing file: $COMINcmcem3/${PDYm3}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P036_0${PEP}.grib2"
                msg="Warning:    Missing file: $COMINcmcem3/${PDYm3}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P048_0${PEP}.grib2"
                msg="Warning: P-ETSS is running with GEFS only forcing."
                postmsg "$jlogfile" "$msg"
                echo "$job Warning: P-ETSS results are from GEFS only due to five CMC cycles of missing hindacast wind data." > ${COMOUT}/PETSS_GEFS_${cyc}
              else
                msg="Warning: Four consecutive CMC cycle hindcast wind data are missing."
                msg="Warning:    Missing file: $COMINcmcem1/${PDYm1}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2"
                msg="Warning:    Missing file: $COMINcmcem2/${PDYm2}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2"
                msg="Warning:    Missing file: $COMINcmcem2/${PDYm2}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P024_0${PEP}.grib2"
                msg="Warning:    Missing file: $COMINcmcem3/${PDYm3}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P036_0${PEP}.grib2"
                msg="Warning: Using $COMINcmcem3/${PDYm3}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P048_0${PEP}.grib2 to fill in the gap."
                postmsg "$jlogfile" "$msg"
                echo "$job Warning: P-ETSS results are from NAEFS, but used the previous 4 CMC cycle wind data in spinup part." > ${COMOUT}/PETSS_NAEFS_${cyc}
                cpreq  $COMINcmcem3/${PDYm3}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P048_0${PEP}.grib2 ${WNDS}${PEP}hind100
              fi
            else
              msg="Warning: Three consecutive CMC cycle hindcast wind data are missing."
              msg="Warning:    Missing file: $COMINcmcem1/${PDYm1}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2"
              msg="Warning:    Missing file: $COMINcmcem2/${PDYm2}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2"
              msg="Warning:    Missing file: $COMINcmcem2/${PDYm2}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P024_0${PEP}.grib2"
              msg="Warning: Using $COMINcmcem3/${PDYm3}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P036_0${PEP}.grib2 to fill in the gap."
              postmsg "$jlogfile" "$msg"
              echo "$job Warning: P-ETSS results are from NAEFS, but used the previous 3 CMC cycle wind data in spinup part." > ${COMOUT}/PETSS_NAEFS_${cyc}
              cpreq  $COMINcmcem3/${PDYm3}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P036_0${PEP}.grib2 ${WNDS}${PEP}hind100
            fi
          else
            msg="Warning: Two consecutive CMC cycle hindcast wind data are missing."
            msg="Warning:    Missing file: $COMINcmcem1/${PDYm1}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2"
            msg="Warning:    Missing file: $COMINcmcem2/${PDYm2}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2"
            msg="Warning: Using $COMINcmcem2/${PDYm2}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P024_0${PEP}.grib2 to fill in the gap."
            postmsg "$jlogfile" "$msg"
            echo "$job Warning: P-ETSS results are from NAEFS, but used the previous 2 CMC cycle wind data in spinup part." > ${COMOUT}/PETSS_NAEFS_${cyc}
            cpreq  $COMINcmcem2/${PDYm2}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P024_0${PEP}.grib2 ${WNDS}${PEP}hind100
          fi
        else
          msg="Warning: One CMC cycle hindcast wind data is missing."
          msg="Warning:    Missing file: $COMINcmcem1/${PDYm1}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2"
          msg="Warning: Using $COMINcmcem2/${PDYm2}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2 to fill in the gap."
          postmsg "$jlogfile" "$msg"
          echo "$job Warning: P-ETSS results are from NAEFS, but used the previous CMC cycle wind data in spinup part." > ${COMOUT}/PETSS_NAEFS_${cyc}
          cpreq  $COMINcmcem2/${PDYm2}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2 ${WNDS}${PEP}hind100
        fi
      else
        cpreq  $COMINcmcem1/${PDYm1}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2 ${WNDS}${PEP}hind100
      fi
      if [[ ! -s $COMINcmcem1/${PDYm1}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2 ]]; then
        if [[ ! -s $COMINcmcem1/${PDYm1}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2 ]]; then
          if [[ ! -s $COMINcmcem2/${PDYm2}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P024_0${PEP}.grib2 ]]; then
            if [[ ! -s $COMINcmcem2/${PDYm2}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P036_0${PEP}.grib2 ]]; then
              if [[ ! -s $COMINcmcem3/${PDYm3}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P048_0${PEP}.grib2 ]]; then
                msg="Warning: Five consecutive CMC cycles hindacast wind data are missing."
                msg="Warning:    Missing file: $COMINcmcem1/${PDYm1}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2"
                msg="Warning:    Missing file: $COMINcmcem1/${PDYm1}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2"
                msg="Warning:    Missing file: $COMINcmcem2/${PDYm2}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P024_0${PEP}.grib2"
                msg="Warning:    Missing file: $COMINcmcem2/${PDYm2}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P036_0${PEP}.grib2"
                msg="Warning:    Missing file: $COMINcmcem3/${PDYm3}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P048_0${PEP}.grib2"
                msg="Warning: P-ETSS is running with GEFS only forcing."
                postmsg "$jlogfile" "$msg"
                echo "$job Warning: P-ETSS results are from GEFS only due to five CMC cycles of missing hindacast wind data." > ${COMOUT}/PETSS_GEFS_${cyc}
              else
                msg="Warning: Four consecutive CMC cycle hindcast wind data are missing."
                msg="Warning:    Missing file: $COMINcmcem1/${PDYm1}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2"
                msg="Warning:    Missing file: $COMINcmcem1/${PDYm1}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2"
                msg="Warning:    Missing file: $COMINcmcem2/${PDYm2}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P024_0${PEP}.grib2"
                msg="Warning:    Missing file: $COMINcmcem2/${PDYm2}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P036_0${PEP}.grib2"
                msg="Warning: Using $COMINcmcem3/${PDYm3}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P048_0${PEP}.grib2 to fill in the gap."
                postmsg "$jlogfile" "$msg"
                echo "$job Warning: P-ETSS results are from NAEFS, but used the previous 4 CMC cycle wind data in spinup part." > ${COMOUT}/PETSS_NAEFS_${cyc}
                cpreq  $COMINcmcem3/${PDYm3}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P048_0${PEP}.grib2 ${WNDS}${PEP}hind112
              fi
            else
              msg="Warning: Three consecutive CMC cycle hindcast wind data are missing."
              msg="Warning:    Missing file: $COMINcmcem1/${PDYm1}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2"
              msg="Warning:    Missing file: $COMINcmcem1/${PDYm1}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2"
              msg="Warning:    Missing file: $COMINcmcem2/${PDYm2}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P024_0${PEP}.grib2"
              msg="Warning: Using $COMINcmcem2/${PDYm2}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P036_0${PEP}.grib2 fill in the gap."              
              postmsg "$jlogfile" "$msg"
              echo "$job Warning: P-ETSS results are from NAEFS, but used the previous 3 CMC cycle wind data in spinup part." > ${COMOUT}/PETSS_NAEFS_${cyc}
              cpreq  $COMINcmcem2/${PDYm2}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P036_0${PEP}.grib2 ${WNDS}${PEP}hind112
            fi
          else
            msg="Warning: Two consecutive CMC cycle hindcast wind data are missing."
            msg="Warning:    Missing file $COMINcmcem1/${PDYm1}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2"
            msg="Warning:    Missing file $COMINcmcem1/${PDYm1}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2"
            msg="Warning: Using $COMINcmcem2/${PDYm2}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P024_0${PEP}.grib2 to fill in the gap"
            postmsg "$jlogfile" "$msg"
            echo "$job Warning: P-ETSS results are from NAEFS, but used the previous 2 CMC cycle wind data in spinup part." > ${COMOUT}/PETSS_NAEFS_${cyc}
            cpreq  $COMINcmcem2/${PDYm2}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P024_0${PEP}.grib2 ${WNDS}${PEP}hind112
          fi
        else
          msg="Warning: One CMC cycle hindcast wind data is missing."
          msg="Warning:    Missing file: $COMINcmcem1/${PDYm1}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2"
          msg="Warning: Using $COMINcmcem1/${PDYm1}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2 to fill in the gap"
          postmsg "$jlogfile" "$msg"
          echo "$job Warning: P-ETSS results are from NAEFS, but used the previous CMC cycle wind data in spinup part." > ${COMOUT}/PETSS_NAEFS_${cyc}
          cpreq  $COMINcmcem1/${PDYm1}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2 ${WNDS}${PEP}hind112
        fi
      else
        cpreq  $COMINcmcem1/${PDYm1}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2 ${WNDS}${PEP}hind112
      fi
    fi
  fi
  case "$cyc" in
  00)
    # Cycle 0 needs 12Z 18Z of -3 day and 0Z of -2 day
    if [[ ${WNDS} == gep || ${WNDS} == gec ]] ;then
      if [[ ${RUN_RETRO} == YES ]] ; then # NewGEFS Retrospective Run
        cpreq  $COMINgefsm3/${CYC3}/${WNDS}${PEP}.$CYCLE3.${GEFS_Fn}.${GEFS_Res}.f000  ${WNDS}${PEP}hind312
        cpreq  $COMINgefsm2/${CYC1}/${WNDS}${PEP}.$CYCLE1.${GEFS_Fn}.${GEFS_Res}.f000  ${WNDS}${PEP}hind200
      else
        Start=$SECONDS
        while [[ ! -s $COMINgefsm3/${CYC3}/${WNDS}${PEP}.$CYCLE3.${GEFS_Fn}.${GEFS_Res}.f000 || ! -s $COMINgefsm3/${CYC4}/${WNDS}${PEP}.$CYCLE4.${GEFS_Fn}.${GEFS_Res}.f000 || ! -s $COMINgefsm2/${CYC1}/${WNDS}${PEP}.$CYCLE1.${GEFS_Fn}.${GEFS_Res}.f000 ]]; do
          Dura=$((SECONDS-Start))
          if [ $Dura -ge 600 ]; then
            msg="$job Error: Hindcast GEFS Wind file is missed after 10 mins wait in Cycle 00"
            echo "Missed one of the following wind files."
            echo "$COMINgefsm3/${CYC3}/${WNDS}${PEP}.$CYCLE3.${GEFS_Fn}.${GEFS_Res}.f000"
            echo "$COMINgefsm3/${CYC4}/${WNDS}${PEP}.$CYCLE4.${GEFS_Fn}.${GEFS_Res}.f000"
            echo "$COMINgefsm2/${CYC1}/${WNDS}${PEP}.$CYCLE1.${GEFS_Fn}.${GEFS_Res}.f000"
            postmsg "$jlogfile" "$msg"
            err_exit
          fi
          sleep 1
        done
        cpreq  $COMINgefsm3/${CYC3}/${WNDS}${PEP}.$CYCLE3.${GEFS_Fn}.${GEFS_Res}.f000  ${WNDS}${PEP}hind312
        cpreq  $COMINgefsm3/${CYC4}/${WNDS}${PEP}.$CYCLE4.${GEFS_Fn}.${GEFS_Res}.f000  ${WNDS}${PEP}hind318
        cpreq  $COMINgefsm2/${CYC1}/${WNDS}${PEP}.$CYCLE1.${GEFS_Fn}.${GEFS_Res}.f000  ${WNDS}${PEP}hind200
      fi
    else # CMC
      if [[ ${RUN_ENVIR} == "MDLTEST" ]] ; then
        cpreq  $COMINcmcem3/${CYC3}/pgrb2ap5/${WNDS}${PEP}.$CYCLE3.pgrb2a${WNDRES}f000  ${WNDS}${PEP}hind312
        cpreq  $COMINcmcem2/${CYC1}/pgrb2ap5/${WNDS}${PEP}.$CYCLE1.pgrb2a${WNDRES}f000  ${WNDS}${PEP}hind200
      else
        if [[ ! -s $COMINcmcem3/${PDYm3}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2 ]]; then
          if [[ ! -s $COMINcmcem3/${PDYm3}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2 ]]; then 
            if [[ ! -s $COMINcmcem4/${PDYm4}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P024_0${PEP}.grib2 ]]; then 
              if [[ ! -s $COMINcmcem4/${PDYm4}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P036_0${PEP}.grib2 ]]; then 
                if [[ ! -s $COMINcmcem5/${PDYm5}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P048_0${PEP}.grib2 ]]; then 
                  msg="Warning: Five consecutive CMC cycles hindacast wind data are missing."
                  msg="Warning:    Missing file: $COMINcmcem3/${PDYm3}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2"
                  msg="Warning:    Missing file: $COMINcmcem3/${PDYm3}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2"
                  msg="Warning:    Missing file: $COMINcmcem4/${PDYm4}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P024_0${PEP}.grib2"
                  msg="Warning:    Missing file: $COMINcmcem4/${PDYm4}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P036_0${PEP}.grib2"
                  msg="Warning:    Missing file: $COMINcmcem5/${PDYm5}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P048_0${PEP}.grib2"
                  msg="Warning: P-ETSS is running with GEFS only forcing."
                  postmsg "$jlogfile" "$msg"
                  echo "$job Warning: P-ETSS results are from GEFS only due to five CMC cycles of missing hindacast wind data." > ${COMOUT}/PETSS_GEFS_${cyc}
                else
                  msg="Warning: Four consecutive CMC cycle hindcast wind data are missing."
                  msg="Warning:    Missing file: $COMINcmcem3/${PDYm3}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2"
                  msg="Warning:    Missing file: $COMINcmcem3/${PDYm3}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2"
                  msg="Warning:    Missing file: $COMINcmcem4/${PDYm4}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P024_0${PEP}.grib2"
                  msg="Warning:    Missing file: $COMINcmcem4/${PDYm4}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P036_0${PEP}.grib2"
                  msg="Warning: Using $COMINcmcem5/${PDYm5}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P048_0${PEP}.grib2 to fill in the gap."
                  postmsg "$jlogfile" "$msg"
                  echo "$job Warning: P-ETSS results are from NAEFS, but used the previous 4 CMC cycle wind data in spinup part." > ${COMOUT}/PETSS_NAEFS_${cyc}
                  cpreq  $COMINcmcem5/${PDYm5}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P048_0${PEP}.grib2  ${WNDS}${PEP}hind312
                fi
              else
                msg="Warning: Three consecutive CMC cycle hindcast wind data are missing."
                msg="Warning:    Missing file: $COMINcmcem3/${PDYm3}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2"
                msg="Warning:    Missing file: $COMINcmcem3/${PDYm3}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2"
                msg="Warning:    Missing file: $COMINcmcem4/${PDYm4}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P024_0${PEP}.grib2"
                msg="Warning: Using $COMINcmcem4/${PDYm4}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P036_0${PEP}.grib2 to fill in the gap."
                postmsg "$jlogfile" "$msg"
                echo "$job Warning: P-ETSS results are from NAEFS, but used the previous 3 CMC cycle wind data in spinup part." > ${COMOUT}/PETSS_NAEFS_${cyc}
                cpreq  $COMINcmcem4/${PDYm4}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P036_0${PEP}.grib2  ${WNDS}${PEP}hind312
              fi
            else
              msg="Warning: Two consecutive CMC cycle hindcast wind data are missing."
              msg="Warning:    Missing file: $COMINcmcem3/${PDYm3}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2"
              msg="Warning:    Missing file: $COMINcmcem3/${PDYm3}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2"
              msg="Warning: Using $COMINcmcem4/${PDYm4}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P024_0${PEP}.grib2 to fill in the gap."
              postmsg "$jlogfile" "$msg"
              echo "$job Warning: P-ETSS results are from NAEFS, but used the previous 2 CMC cycle wind data in spinup part." > ${COMOUT}/PETSS_NAEFS_${cyc}
              cpreq  $COMINcmcem4/${PDYm4}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P024_0${PEP}.grib2  ${WNDS}${PEP}hind312
            fi
          else
            msg="Warning: one CMC cycle hindcast wind data is missing."
            msg="Warning:    Missing file: $COMINcmcem3/${PDYm3}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2"
            msg="Warning: Using $COMINcmcem3/${PDYm3}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2 to fill in the gap."
            postmsg "$jlogfile" "$msg"
            echo "$job Warning: P-ETSS results are from NAEFS, but used the previous CMC cycle wind data in spinup part." > ${COMOUT}/PETSS_NAEFS_${cyc}
            cpreq  $COMINcmcem3/${PDYm3}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2  ${WNDS}${PEP}hind312
          fi
        else
          cpreq  $COMINcmcem3/${PDYm3}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2  ${WNDS}${PEP}hind312
        fi
        if [[ ! -s $COMINcmcem2/${PDYm2}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2 ]]; then
          if [[ ! -s $COMINcmcem3/${PDYm3}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2 ]]; then
            if [[ ! -s $COMINcmcem3/${PDYm3}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P024_0${PEP}.grib2 ]]; then
              if [[ ! -s $COMINcmcem4/${PDYm4}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P036_0${PEP}.grib2 ]]; then
                if [[ ! -s $COMINcmcem4/${PDYm4}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P048_0${PEP}.grib2 ]]; then
                  msg="Warning: Five consecutive CMC cycles hindacast wind data are missing."
                  msg="Warning:    Missing file: $COMINcmcem2/${PDYm2}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2"
                  msg="Warning:    Missing file: $COMINcmcem3/${PDYm3}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2"
                  msg="Warning:    Missing file: $COMINcmcem3/${PDYm3}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P024_0${PEP}.grib2"
                  msg="Warning:    Missing file: $COMINcmcem4/${PDYm4}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P036_0${PEP}.grib2"
                  msg="Warning:    Missing file: $COMINcmcem4/${PDYm4}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P048_0${PEP}.grib2"
                  msg="Warning: P-ETSS is running with GEFS only forcing."
                  postmsg "$jlogfile" "$msg"
                  echo "$job Warning: P-ETSS results are from GEFS only due to five CMC cycles of missing hindacast wind data." > ${COMOUT}/PETSS_GEFS_${cyc}
                else
                  msg="Warning: Four consecutive CMC cycle hindcast wind data are missing."
                  msg="Warning:    Missing file: $COMINcmcem2/${PDYm2}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2"
                  msg="Warning:    Missing file: $COMINcmcem3/${PDYm3}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2"
                  msg="Warning:    Missing file: $COMINcmcem3/${PDYm3}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P024_0${PEP}.grib2"
                  msg="Warning:    Missing file: $COMINcmcem4/${PDYm4}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P036_0${PEP}.grib2"
                  msg="Warning: Using  $COMINcmcem4/${PDYm4}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P048_0${PEP}.grib2 to fill in the gap."
                  postmsg "$jlogfile" "$msg"
                  echo "$job Warning: P-ETSS results are from NAEFS, but used the previous 4 CMC cycle wind data in spinup part." > ${COMOUT}/PETSS_NAEFS_${cyc}
                  cpreq  $COMINcmcem4/${PDYm4}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P048_0${PEP}.grib2 ${WNDS}${PEP}hind200
                fi
                else
                  msg="Warning: Three consecutive CMC cycle hindcast wind data are missing."
                  msg="Warning:    Missing file: $COMINcmcem2/${PDYm2}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2"
                  msg="Warning:    Missing file: $COMINcmcem3/${PDYm3}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2"
                  msg="Warning:    Missing file: $COMINcmcem3/${PDYm3}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P024_0${PEP}.grib2"
                  msg="Warning: Using $COMINcmcem4/${PDYm4}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P036_0${PEP}.grib2 to fill in the gap."
                  postmsg "$jlogfile" "$msg"
                  echo "$job Warning: P-ETSS results are from NAEFS, but used the previous 3 CMC cycle wind data in spinup part." > ${COMOUT}/PETSS_NAEFS_${cyc}
                  cpreq  $COMINcmcem4/${PDYm4}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P036_0${PEP}.grib2 ${WNDS}${PEP}hind200
                fi
              else
                msg="Warning: Two consecutive CMC cycle hindcast wind data are missing."
                msg="Warning:    Missing file: $COMINcmcem2/${PDYm2}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2"
                msg="Warning:    Missing file: $COMINcmcem3/${PDYm3}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2"
                msg="Warning: Using $COMINcmcem3/${PDYm3}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P024_0${PEP}.grib2 to fill in the gap."
                postmsg "$jlogfile" "$msg"
                echo "$job Warning: P-ETSS results are from NAEFS, but used the previous 2 CMC cycle wind data in spinup part." > ${COMOUT}/PETSS_NAEFS_${cyc}
                cpreq  $COMINcmcem3/${PDYm3}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P024_0${PEP}.grib2 ${WNDS}${PEP}hind200
              fi
            else   
              msg="Warning: One CMC cycle hindcast wind data is missing."
              msg="Warning:    Missing file: $COMINcmcem2/${PDYm2}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2"
              msg="Warning: Using $COMINcmcem3/${PDYm3}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2 to fill in the gap."
              postmsg "$jlogfile" "$msg"
              echo "$job Warning: P-ETSS results are from NAEFS, but used the previous CMC cycle wind data in spinup part." > ${COMOUT}/PETSS_NAEFS_${cyc}
              cpreq  $COMINcmcem3/${PDYm3}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2  ${WNDS}${PEP}hind200
            fi
          else
            cpreq  $COMINcmcem2/${PDYm2}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2  ${WNDS}${PEP}hind200
          fi
        fi
      fi
    ;;
  06)
    # Cycle 6 needs 18Z of -3 day and 0Z of -2 day
    if [[ ${WNDS} == gep || ${WNDS} == gec ]] ;then # GEFS 6 hours
      Start=$SECONDS
      while [[ ! -s $COMINgefsm3/${CYC4}/${WNDS}${PEP}.$CYCLE4.${GEFS_Fn}.${GEFS_Res}.f000 || ! -s $COMINgefsm2/${CYC1}/${WNDS}${PEP}.$CYCLE1.${GEFS_Fn}.${GEFS_Res}.f000 || ! -s $COMINgefs/${CYC1}/${WNDS}${PEP}.$CYCLE1.${GEFS_Fn}.${GEFS_Res}.f000 ]]; do
        Dura=$((SECONDS-Start))
        if [ $Dura -ge 600 ]; then
          msg="$job Error: Hindcast Wind file is missed after 10 mins wait in cyc06"
          echo "Missed one of the following wind files."
          echo "$COMINgefsm3/${CYC4}/${WNDS}${PEP}.$CYCLE4.${GEFS_Fn}.${GEFS_Res}.f000"
          echo "$COMINgefsm2/${CYC1}/${WNDS}${PEP}.$CYCLE1.${GEFS_Fn}.${GEFS_Res}.f000"
          echo "$COMINgefs/${CYC1}/${WNDS}${PEP}.$CYCLE1.${GEFS_Fn}.${GEFS_Res}.f000"
          postmsg "$jlogfile" "$msg"
          err_exit
        fi
        sleep 1
      done
      cpreq  $COMINgefsm3/${CYC4}/${WNDS}${PEP}.$CYCLE4.${GEFS_Fn}.${GEFS_Res}.f000  ${WNDS}${PEP}hind318
      cpreq  $COMINgefsm2/${CYC1}/${WNDS}${PEP}.$CYCLE1.${GEFS_Fn}.${GEFS_Res}.f000  ${WNDS}${PEP}hind200
      # Cycle 6 needs 0Z of current day
      cpreq  $COMINgefs/${CYC1}/${WNDS}${PEP}.$CYCLE1.${GEFS_Fn}.${GEFS_Res}.f000  ${WNDS}${PEP}hind000
    else # CMC
      if [[ ${RUN_ENVIR} == "MDLTEST" ]] ; then
        cpreq  $COMINcmcem3/${CYC3}/pgrb2ap5/${WNDS}${PEP}.$CYCLE3.pgrb2a${WNDRES}f000  ${WNDS}${PEP}hind312
        cpreq  $COMINcmcem2/${CYC1}/pgrb2ap5/${WNDS}${PEP}.$CYCLE1.pgrb2a${WNDRES}f000  ${WNDS}${PEP}hind200
      else
        if [[ ! -s $COMINcmcem3/${PDYm3}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2 ]]; then
          if [[ ! -s $COMINcmcem3/${PDYm3}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2 ]]; then 
            if [[ ! -s $COMINcmcem4/${PDYm4}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P024_0${PEP}.grib2 ]]; then 
              if [[ ! -s $COMINcmcem4/${PDYm4}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P036_0${PEP}.grib2 ]]; then 
                if [[ ! -s $COMINcmcem5/${PDYm5}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P048_0${PEP}.grib2 ]]; then 
                  msg="Warning: Five consecutive CMC cycles hindacast wind data are missing."
                  msg="Warning:    Missing file: $COMINcmcem3/${PDYm3}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2"
                  msg="Warning:    Missing file: $COMINcmcem3/${PDYm3}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2"
                  msg="Warning:    Missing file: $COMINcmcem4/${PDYm4}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P024_0${PEP}.grib2"
                  msg="Warning:    Missing file: $COMINcmcem4/${PDYm4}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P036_0${PEP}.grib2"
                  msg="Warning:    Missing file: $COMINcmcem5/${PDYm5}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P048_0${PEP}.grib2"
                  msg="Warning: P-ETSS is running with GEFS only forcing."
                  postmsg "$jlogfile" "$msg"
                  echo "$job Warning: P-ETSS results are from GEFS only due to five CMC cycles of missing hindacast wind data." > ${COMOUT}/PETSS_GEFS_${cyc}
                else
                  msg="Warning: Four consecutive CMC cycle hindcast wind data are missing."
                  msg="Warning:    Missing file: $COMINcmcem3/${PDYm3}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2"
                  msg="Warning:    Missing file: $COMINcmcem3/${PDYm3}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2"
                  msg="Warning:    Missing file: $COMINcmcem4/${PDYm4}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P024_0${PEP}.grib2"
                  msg="Warning:    Missing file: $COMINcmcem4/${PDYm4}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P036_0${PEP}.grib2"
                  msg="Warning: Using  $COMINcmcem5/${PDYm5}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P048_0${PEP}.grib2 to fill in the gap."
                  postmsg "$jlogfile" "$msg"
                  echo "$job Warning: P-ETSS results are from NAEFS, but used the previous 4 CMC cycle wind data in spinup part." > ${COMOUT}/PETSS_NAEFS_${cyc}
                  cpreq  $COMINcmcem5/${PDYm5}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P048_0${PEP}.grib2  ${WNDS}${PEP}hind312
                fi
              else
                msg="Warning: Three consecutive CMC cycle hindcast wind data are missing."
                msg="Warning:    Missing file: $COMINcmcem3/${PDYm3}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2"
                msg="Warning:    Missing file: $COMINcmcem3/${PDYm3}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2"
                msg="Warning:    Missing file: $COMINcmcem4/${PDYm4}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P024_0${PEP}.grib2"
                msg="Warning: Using  $COMINcmcem4/${PDYm4}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P036_0${PEP}.grib2 to fill in the gap."
                postmsg "$jlogfile" "$msg"
                echo "$job Warning: P-ETSS results are from NAEFS, but used the previous 3 CMC cycle wind data in spinup part." > ${COMOUT}/PETSS_NAEFS_${cyc}
                cpreq  $COMINcmcem4/${PDYm4}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P036_0${PEP}.grib2  ${WNDS}${PEP}hind312
              fi
            else
              msg="Warning: Two consecutive CMC cycle hindcast wind data are missing."
              msg="Warning:    Missing file: $COMINcmcem3/${PDYm3}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2"
              msg="Warning:    Missing file: $COMINcmcem3/${PDYm3}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2"
              msg="Warning: Using $COMINcmcem4/${PDYm4}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P024_0${PEP}.grib2 to fill in the gap."
              postmsg "$jlogfile" "$msg"
              echo "$job Warning: P-ETSS results are from NAEFS, but used the previous 2 CMC cycle wind data in spinup part." > ${COMOUT}/PETSS_NAEFS_${cyc}
              cpreq  $COMINcmcem4/${PDYm4}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P024_0${PEP}.grib2  ${WNDS}${PEP}hind312
            fi
          else
            msg="Warning: One CMC cycle hindcast wind data is missing."
            msg="Warning:    Missing file: $COMINcmcem3/${PDYm3}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2"
            msg="Warning: Using $COMINcmcem3/${PDYm3}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2 to fill in the gap."
            postmsg "$jlogfile" "$msg"
            echo "$job Warning: P-ETSS results are from NAEFS, but used the previous CMC cycle wind data in spinup part." > ${COMOUT}/PETSS_NAEFS_${cyc}
            cpreq  $COMINcmcem3/${PDYm3}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2  ${WNDS}${PEP}hind312
          fi
        else
          cpreq  $COMINcmcem3/${PDYm3}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2  ${WNDS}${PEP}hind312
        fi
        if [[ ! -s $COMINcmcem2/${PDYm2}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2 ]]; then
          if [[ ! -s $COMINcmcem3/${PDYm3}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2 ]]; then
            if [[ ! -s $COMINcmcem3/${PDYm3}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P024_0${PEP}.grib2 ]]; then
              if [[ ! -s $COMINcmcem4/${PDYm4}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P036_0${PEP}.grib2 ]]; then
                if [[ ! -s $COMINcmcem4/${PDYm4}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P048_0${PEP}.grib2 ]]; then
                  msg="Warning: Five consecutive CMC cycles hindacast wind data are missing."
                  msg="Warning:    Missing file: $COMINcmcem2/${PDYm2}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2"
                  msg="Warning:    Missing file: $COMINcmcem3/${PDYm3}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2"
                  msg="Warning:    Missing file: $COMINcmcem3/${PDYm3}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P024_0${PEP}.grib2"
                  msg="Warning:    Missing file: $COMINcmcem4/${PDYm4}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P036_0${PEP}.grib2"
                  msg="Warning:    Missing file: $COMINcmcem4/${PDYm4}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P048_0${PEP}.grib2"
                  msg="Warning: P-ETSS is running with GEFS only forcing."
                  postmsg "$jlogfile" "$msg"
                  echo "$job Warning: P-ETSS results are from GEFS only due to five CMC cycles of missing hindacast wind data." > ${COMOUT}/PETSS_GEFS_${cyc}
                else
                  msg="Warning: Four consecutive CMC cycles hindacast wind data are missing."
                  msg="Warning:    Missing file: $COMINcmcem2/${PDYm2}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2"
                  msg="Warning:    Missing file: $COMINcmcem3/${PDYm3}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2"
                  msg="Warning:    Missing file: $COMINcmcem3/${PDYm3}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P024_0${PEP}.grib2"
                  msg="Warning:    Missing file: $COMINcmcem4/${PDYm4}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P036_0${PEP}.grib2"
                  msg="Warning: Using $COMINcmcem4/${PDYm4}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P048_0${PEP}.grib2 to fill in the gap."
                  postmsg "$jlogfile" "$msg"
                  echo "$job Warning: P-ETSS results are from NAEFS, but used the previous 4 CMC cycle wind data in spinup part." > ${COMOUT}/PETSS_NAEFS_${cyc}
                  cpreq  $COMINcmcem4/${PDYm4}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P048_0${PEP}.grib2 ${WNDS}${PEP}hind200
                fi
              else
                msg="Warning: Three consecutive CMC cycles hindacast wind data are missing."
                msg="Warning:    Missing file: $COMINcmcem2/${PDYm2}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2"
                msg="Warning:    Missing file: $COMINcmcem3/${PDYm3}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2"
                msg="Warning:    Missing file: $COMINcmcem3/${PDYm3}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P024_0${PEP}.grib2"
                msg="Warning: Using $COMINcmcem4/${PDYm4}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P036_0${PEP}.grib2 to fill in the gap."
                postmsg "$jlogfile" "$msg"
                echo "$job Warning: P-ETSS results are from NAEFS, but used the previous 3 CMC cycle wind data in spinup part." > ${COMOUT}/PETSS_NAEFS_${cyc}
                cpreq  $COMINcmcem4/${PDYm4}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P036_0${PEP}.grib2 ${WNDS}${PEP}hind200
              fi
            else
              msg="Warning: Two consecutive CMC cycles hindacast wind data are missing."
              msg="Warning:    Missing file: $COMINcmcem2/${PDYm2}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2"
              msg="Warning:    Missing file: $COMINcmcem3/${PDYm3}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2"
              msg="Warning: Using $COMINcmcem3/${PDYm3}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P024_0${PEP}.grib2 to fill in the gap."
              postmsg "$jlogfile" "$msg"
              echo "$job Warning: P-ETSS results are from NAEFS, but used the previous 2 CMC cycle wind data in spinup part." > ${COMOUT}/PETSS_NAEFS_${cyc}
              cpreq  $COMINcmcem3/${PDYm3}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P024_0${PEP}.grib2 ${WNDS}${PEP}hind200
            fi
          else   
            msg="Warning: One CMC cycles hindacast wind data is missing."
            msg="Warning:    Missing file: $COMINcmcem2/${PDYm2}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2"
            msg="Warning: Using $COMINcmcem3/${PDYm3}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2 to fill in the gap."
            postmsg "$jlogfile" "$msg"
            echo "$job Warning: P-ETSS results are from NAEFS, but used the previous CMC cycle wind data in spinup part." > ${COMOUT}/PETSS_NAEFS_${cyc}
            cpreq  $COMINcmcem3/${PDYm3}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2  ${WNDS}${PEP}hind200
          fi
        else
          cpreq  $COMINcmcem2/${PDYm2}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2  ${WNDS}${PEP}hind200
        fi
      fi    
    fi
    ;;
  12)
    # Cycle 12 needs 0Z of -2 day
    if [[ ${WNDS} == gep || ${WNDS} == gec ]] ;then # GEFS
      if [[ ${RUN_RETRO} == YES ]] ; then
        cpreq  $COMINgefsm2/${CYC1}/${WNDS}${PEP}.$CYCLE1.${GEFS_Fn}.${GEFS_Res}.f000  ${WNDS}${PEP}hind200
        cpreq  $COMINgefs/${CYC1}/${WNDS}${PEP}.$CYCLE1.${GEFS_Fn}.${GEFS_Res}.f000  ${WNDS}${PEP}hind000
      else
        Start=$SECONDS
        while [[ ! -s $COMINgefsm2/${CYC1}/${WNDS}${PEP}.$CYCLE1.${GEFS_Fn}.${GEFS_Res}.f000 || ! -s $COMINgefs/${CYC1}/${WNDS}${PEP}.$CYCLE1.${GEFS_Fn}.${GEFS_Res}.f000 || ! -s $COMINgefs/${CYC2}/${WNDS}${PEP}.$CYCLE2.${GEFS_Fn}.${GEFS_Res}.f000 ]]; do
          Dura=$((SECONDS-Start))
          if [ $Dura -ge 600 ]; then
            msg="$job Error: Hindcast Wind file is missed after 10 mins wait in cyc12" 
            echo "Missed one of the following wind files."
            echo "$COMINgefsm2/${CYC1}/${WNDS}${PEP}.$CYCLE1.${GEFS_Fn}.${GEFS_Res}.f000"
            echo "$COMINgefs/${CYC1}/${WNDS}${PEP}.$CYCLE1.${GEFS_Fn}.${GEFS_Res}.f000"
            echo "$COMINgefs/${CYC2}/${WNDS}${PEP}.$CYCLE2.${GEFS_Fn}.${GEFS_Res}.f000"
            postmsg "$jlogfile" "$msg"
            err_exit
          fi
          sleep 1
        done
        cpreq  $COMINgefsm2/${CYC1}/${WNDS}${PEP}.$CYCLE1.${GEFS_Fn}.${GEFS_Res}.f000  ${WNDS}${PEP}hind200
       # Cycle 12 needs 0Z 6Z of current day
        cpreq  $COMINgefs/${CYC1}/${WNDS}${PEP}.$CYCLE1.${GEFS_Fn}.${GEFS_Res}.f000  ${WNDS}${PEP}hind000
        cpreq  $COMINgefs/${CYC2}/${WNDS}${PEP}.$CYCLE2.${GEFS_Fn}.${GEFS_Res}.f000  ${WNDS}${PEP}hind006
      fi
    else  # CMC
      if [[ ${RUN_ENVIR} == "MDLTEST" ]] ; then
        cpreq  $COMINcmcem2/${CYC1}/pgrb2ap5/${WNDS}${PEP}.$CYCLE1.pgrb2a${WNDRES}f000  ${WNDS}${PEP}hind200
       # Cycle 12 needs 0Z 6Z of current day
        cpreq  $COMINcmce/${CYC1}/pgrb2ap5/${WNDS}${PEP}.$CYCLE1.pgrb2a${WNDRES}f000  ${WNDS}${PEP}hind000       
      else
        if [[ ! -s $COMINcmcem2/${PDYm2}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2 ]]; then
          if [[ ! -s $COMINcmcem3/${PDYm3}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2 ]]; then
            if [[ ! -s $COMINcmcem3/${PDYm3}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P024_0${PEP}.grib2 ]]; then
              if [[ ! -s $COMINcmcem4/${PDYm4}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P036_0${PEP}.grib2 ]]; then
                if [[ ! -s $COMINcmcem4/${PDYm4}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P048_0${PEP}.grib2 ]]; then
                  msg="Warning: Five consecutive CMC cycles hindacast wind data are missing."
                  msg="Warning:    Missing file: $COMINcmcem2/${PDYm2}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2" 
                  msg="Warning:    Missing file: $COMINcmcem3/${PDYm3}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2"
                  msg="Warning:    Missing file: $COMINcmcem3/${PDYm3}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P024_0${PEP}.grib2"
                  msg="Warning:    Missing file: $COMINcmcem4/${PDYm4}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P036_0${PEP}.grib2"
                  msg="Warning:    Missing file: $COMINcmcem4/${PDYm4}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P048_0${PEP}.grib2"
                  msg="Warning: P-ETSS is running with GEFS only forcing."
                  postmsg "$jlogfile" "$msg"
                  echo "$job Warning: P-ETSS results are from GEFS only due to five CMC cycles of missing hindacast wind data." > ${COMOUT}/PETSS_GEFS_${cyc}
                else
                  msg="Warning: Four consecutive CMC cycles hindacast wind data are missing."
                  msg="Warning:    Missing file: $COMINcmcem2/${PDYm2}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2" 
                  msg="Warning:    Missing file: $COMINcmcem3/${PDYm3}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2"
                  msg="Warning:    Missing file: $COMINcmcem3/${PDYm3}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P024_0${PEP}.grib2"
                  msg="Warning:    Missing file: $COMINcmcem4/${PDYm4}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P036_0${PEP}.grib2"
                  msg="Warning: Using $COMINcmcem4/${PDYm4}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P048_0${PEP}.grib2 to fill in the gap."
                  postmsg "$jlogfile" "$msg"
                  echo "$job Warning: P-ETSS results are from NAEFS, but used the previous 4 CMC cycle wind data in spinup part." > ${COMOUT}/PETSS_NAEFS_${cyc}
                  cpreq  $COMINcmcem4/${PDYm4}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P048_0${PEP}.grib2 ${WNDS}${PEP}hind200
                fi
              else
                msg="Warning: Three consecutive CMC cycles hindacast wind data are missing."
                msg="Warning:    Missing file: $COMINcmcem2/${PDYm2}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2" 
                msg="Warning:    Missing file: $COMINcmcem3/${PDYm3}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2"
                msg="Warning:    Missing file: $COMINcmcem3/${PDYm3}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P024_0${PEP}.grib2"
                msg="Warning: Using $COMINcmcem4/${PDYm4}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P036_0${PEP}.grib2 to fill in the gap."
                postmsg "$jlogfile" "$msg"
                echo "$job Warning: P-ETSS results are from NAEFS, but used the previous 3 CMC cycle wind data in spinup part." > ${COMOUT}/PETSS_NAEFS_${cyc}
                cpreq  $COMINcmcem4/${PDYm4}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P036_0${PEP}.grib2 ${WNDS}${PEP}hind200
              fi
            else
              msg="Warning: Two consecutive CMC cycles hindacast wind data are missing."
              msg="Warning:    Missing file: $COMINcmcem2/${PDYm2}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2" 
              msg="Warning:    Missing file: $COMINcmcem3/${PDYm3}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2"
              msg="Warning: Using $COMINcmcem3/${PDYm3}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P024_0${PEP}.grib2 to fill in the gap."
              postmsg "$jlogfile" "$msg"
              echo "$job Warning: P-ETSS results are from NAEFS, but used the previous 2 CMC cycle wind data in spinup part." > ${COMOUT}/PETSS_NAEFS_${cyc}
              cpreq  $COMINcmcem3/${PDYm3}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P024_0${PEP}.grib2 ${WNDS}${PEP}hind200
            fi
          else
            msg="Warning: One CMC cycles hindacast wind data is missing."
            msg="Warning:    Missing file: $COMINcmcem2/${PDYm2}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2" 
            msg="Warning: Using $COMINcmcem3/${PDYm3}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2 to fill in the gap."
            postmsg "$jlogfile" "$msg"
            echo "$job Warning: P-ETSS results are from NAEFS, but used the previous CMC cycle wind data in spinup part." > ${COMOUT}/PETSS_NAEFS_${cyc}
            cpreq  $COMINcmcem3/${PDYm3}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2 ${WNDS}${PEP}hind200
          fi
        else
          cpreq  $COMINcmcem2/${PDYm2}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2 ${WNDS}${PEP}hind200
        fi
        if [[ ! -s $COMINcmce/${PDY}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2 ]]; then
          if [[ ! -s $COMINcmcem1/${PDYm1}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2 ]]; then
            if [[ ! -s $COMINcmcem1/${PDYm1}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P024_0${PEP}.grib2 ]]; then
              if [[ ! -s $COMINcmcem2/${PDYm2}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P036_0${PEP}.grib2 ]]; then
                if [[ ! -s $COMINcmcem2/${PDYm2}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P048_0${PEP}.grib2 ]]; then
                  msg="Warning: Five consecutive CMC cycles hindacast wind data are missing."
                  msg="Warning:    Missing file: $COMINcmce/${PDY}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2"
                  msg="Warning:    Missing file: $COMINcmcem1/${PDYm1}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2"
                  msg="Warning:    Missing file: $COMINcmcem1/${PDYm1}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P024_0${PEP}.grib2"
                  msg="Warning:    Missing file: $COMINcmcem2/${PDYm2}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P036_0${PEP}.grib2"
                  msg="Warning:    Missing file: $COMINcmcem2/${PDYm2}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P048_0${PEP}.grib2"
                  msg="Warning: P-ETSS is running with GEFS only forcing."
                  postmsg "$jlogfile" "$msg"
                  echo "$job Warning: P-ETSS results are from GEFS only due to five CMC cycles of missing hindacast wind data." > ${COMOUT}/PETSS_GEFS_${cyc}
                else
                  msg="Warning: Four consecutive CMC cycles hindacast wind data are missing."
                  msg="Warning:    Missing file: $COMINcmce/${PDY}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2"
                  msg="Warning:    Missing file: $COMINcmcem1/${PDYm1}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2"
                  msg="Warning:    Missing file: $COMINcmcem1/${PDYm1}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P024_0${PEP}.grib2"
                  msg="Warning:    Missing file: $COMINcmcem2/${PDYm2}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P036_0${PEP}.grib2"
                  msg="Warning: Using $COMINcmcem2/${PDYm2}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P048_0${PEP}.grib2 to fill in the gap."
                  postmsg "$jlogfile" "$msg"
                  echo "$job Warning: P-ETSS results are from NAEFS, but used the previous 4 CMC cycle wind data in spinup part." > ${COMOUT}/PETSS_NAEFS_${cyc}
                  cpreq  $COMINcmcem2/${PDYm2}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P048_0${PEP}.grib2 ${WNDS}${PEP}hind000
                fi
              else
                msg="Warning: Three consecutive CMC cycles hindacast wind data are missing."
                msg="Warning:    Missing file: $COMINcmce/${PDY}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2"
                msg="Warning:    Missing file: $COMINcmcem1/${PDYm1}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2"
                msg="Warning:    Missing file: $COMINcmcem1/${PDYm1}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P024_0${PEP}.grib2"
                msg="Warning: Using $COMINcmcem2/${PDYm2}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P036_0${PEP}.grib2 to fill in the gap."
                postmsg "$jlogfile" "$msg"
                echo "$job Warning: P-ETSS results are from NAEFS, but used the previous 3 CMC cycle wind data in spinup part." > ${COMOUT}/PETSS_NAEFS_${cyc}
                cpreq  $COMINcmcem2/${PDYm2}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P036_0${PEP}.grib2 ${WNDS}${PEP}hind000
              fi
            else
              msg="Warning: Two consecutive CMC cycles hindacast wind data are missing."
              msg="Warning:    Missing file: $COMINcmce/${PDY}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2"
              msg="Warning:    Missing file: $COMINcmcem1/${PDYm1}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2"
              msg="Warning: Using $COMINcmcem1/${PDYm1}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P024_0${PEP}.grib2 to fill in the gap."
              postmsg "$jlogfile" "$msg"
              echo "$job Warning: P-ETSS results are from NAEFS, but used the previous 2 CMC cycle wind data in spinup part." > ${COMOUT}/PETSS_NAEFS_${cyc}
              cpreq  $COMINcmcem1/${PDYm1}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P024_0${PEP}.grib2 ${WNDS}${PEP}hind000
            fi
          else
            msg="Warning: One CMC cycles hindacast wind data is missing."
            msg="Warning:    Missing file: $COMINcmce/${PDY}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2"
            msg="Warning: Using $COMINcmcem1/${PDYm1}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2 to fill in the gap."
            postmsg "$jlogfile" "$msg"
            echo "$job Warning: P-ETSS results are from NAEFS, but used the previous CMC cycle wind data in spinup part." > ${COMOUT}/PETSS_NAEFS_${cyc}
            cpreq  $COMINcmcem1/${PDYm1}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2 ${WNDS}${PEP}hind000
          fi
        else
          cpreq  $COMINcmce/${PDY}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2 ${WNDS}${PEP}hind000
        fi
      fi
    fi
    ;;
  18)
    # Cycle 18 needs 0Z 6Z 12Z of current day
    if [[ ${WNDS} == gep || ${WNDS} == gec ]] ;then # GEFS 6 hours
      Start=$SECONDS 
      while [[ ! -s $COMINgefs/${CYC1}/${WNDS}${PEP}.$CYCLE1.${GEFS_Fn}.${GEFS_Res}.f000 || ! -s $COMINgefs/${CYC2}/${WNDS}${PEP}.$CYCLE2.${GEFS_Fn}.${GEFS_Res}.f000 || ! -s $COMINgefs/${CYC3}/${WNDS}${PEP}.$CYCLE3.${GEFS_Fn}.${GEFS_Res}.f000 ]]; do
        Dura=$((SECONDS-Start))
        if [ $Dura -ge 600 ]; then
          msg="$job Error: Hindcast Wind file is missed after 10 mins wait in cyc18"
          echo "Missed one of the following wind files."
          echo "$COMINgefs/${CYC1}/${WNDS}${PEP}.$CYCLE1.${GEFS_Fn}.${GEFS_Res}.f000"
          echo "$COMINgefs/${CYC2}/${WNDS}${PEP}.$CYCLE2.${GEFS_Fn}.${GEFS_Res}.f000"
          echo "$COMINgefs/${CYC3}/${WNDS}${PEP}.$CYCLE3.${GEFS_Fn}.${GEFS_Res}.f000"
          postmsg "$jlogfile" "$msg"
          err_exit 
        fi 
        sleep 1
      done
      cpreq  $COMINgefs/${CYC1}/${WNDS}${PEP}.$CYCLE1.${GEFS_Fn}.${GEFS_Res}.f000  ${WNDS}${PEP}hind000
      cpreq  $COMINgefs/${CYC2}/${WNDS}${PEP}.$CYCLE2.${GEFS_Fn}.${GEFS_Res}.f000  ${WNDS}${PEP}hind006
      cpreq  $COMINgefs/${CYC3}/${WNDS}${PEP}.$CYCLE3.${GEFS_Fn}.${GEFS_Res}.f000  ${WNDS}${PEP}hind012
    else  # CMC
      if [[ ${RUN_ENVIR} == "MDLTEST" ]] ; then
        cpreq  $COMINcmcem2/${CYC1}/pgrb2ap5/${WNDS}${PEP}.$CYCLE1.pgrb2a${WNDRES}f000  ${WNDS}${PEP}hind200
         # Cycle 12 needs 0Z 6Z of current day
        cpreq  $COMINcmce/${CYC1}/pgrb2ap5/${WNDS}${PEP}.$CYCLE1.pgrb2a${WNDRES}f000  ${WNDS}${PEP}hind000
      else
        if [[ ! -s $COMINcmcem2/${PDYm2}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2 ]]; then
          if [[ ! -s $COMINcmcem3/${PDYm3}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2 ]]; then
            if [[ ! -s $COMINcmcem3/${PDYm3}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P024_0${PEP}.grib2 ]]; then
              if [[ ! -s $COMINcmcem4/${PDYm4}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P036_0${PEP}.grib2 ]]; then
                if [[ ! -s $COMINcmcem4/${PDYm4}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P048_0${PEP}.grib2 ]]; then
                  msg="Warning: Five consecutive CMC cycles hindacast wind data are missing."
                  msg="Warning:    Missing file: $COMINcmcem2/${PDYm2}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2" 
                  msg="Warning:    Missing file: $COMINcmcem3/${PDYm3}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2"
                  msg="Warning:    Missing file: $COMINcmcem3/${PDYm3}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P024_0${PEP}.grib2"
                  msg="Warning:    Missing file: $COMINcmcem4/${PDYm4}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P036_0${PEP}.grib2"
                  msg="Warning:    Missing file: $COMINcmcem4/${PDYm4}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P048_0${PEP}.grib2"
                  msg="Warning: P-ETSS is running with GEFS only forcing."
                  postmsg "$jlogfile" "$msg"
                  echo "$job Warning: P-ETSS results are from GEFS only due to five CMC cycles of missing hindacast wind data." > ${COMOUT}/PETSS_GEFS_${cyc}
                else
                  msg="Warning: Four consecutive CMC cycles hindacast wind data are missing."
                  msg="Warning:    Missing file: $COMINcmcem2/${PDYm2}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2" 
                  msg="Warning:    Missing file: $COMINcmcem3/${PDYm3}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2"
                  msg="Warning:    Missing file: $COMINcmcem3/${PDYm3}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P024_0${PEP}.grib2"
                  msg="Warning:    Missing file: $COMINcmcem4/${PDYm4}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P036_0${PEP}.grib2"
                  msg="Warning: Using $COMINcmcem4/${PDYm4}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P048_0${PEP}.grib2 to fill in the gap."
                  postmsg "$jlogfile" "$msg"
                  echo "$job Warning: P-ETSS results are from NAEFS, but used the previous 4 CMC cycle wind data in spinup part." > ${COMOUT}/PETSS_NAEFS_${cyc}
                  cpreq  $COMINcmcem4/${PDYm4}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P048_0${PEP}.grib2 ${WNDS}${PEP}hind200
                fi
              else
                msg="Warning: Three consecutive CMC cycles hindacast wind data are missing."
                msg="Warning:    Missing file: $COMINcmcem2/${PDYm2}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2" 
                msg="Warning:    Missing file: $COMINcmcem3/${PDYm3}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2"
                msg="Warning:    Missing file: $COMINcmcem3/${PDYm3}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P024_0${PEP}.grib2"
                msg="Warning: Using $COMINcmcem4/${PDYm4}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P036_0${PEP}.grib2 to fill in the gap."
                postmsg "$jlogfile" "$msg"
                echo "$job Warning: P-ETSS results are from NAEFS, but used the previous 3 CMC cycle wind data in spinup part." > ${COMOUT}/PETSS_NAEFS_${cyc}
                cpreq  $COMINcmcem4/${PDYm4}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P036_0${PEP}.grib2 ${WNDS}${PEP}hind200
              fi
            else
              msg="Warning: Two consecutive CMC cycles hindacast wind data are missing."
              msg="Warning:    Missing file: $COMINcmcem2/${PDYm2}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2" 
              msg="Warning:    Missing file: $COMINcmcem3/${PDYm3}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2"
              msg="Warning: Using $COMINcmcem3/${PDYm3}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P024_0${PEP}.grib2 to fill in the gap."
              postmsg "$jlogfile" "$msg"
              echo "$job Warning: P-ETSS results are from NAEFS, but used the previous 2 CMC cycle wind data in spinup part." > ${COMOUT}/PETSS_NAEFS_${cyc}
              cpreq  $COMINcmcem3/${PDYm3}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P024_0${PEP}.grib2 ${WNDS}${PEP}hind200
            fi
          else
            msg="Warning: One CMC cycles hindacast wind data is missing."
            msg="Warning:    Missing file: $COMINcmcem2/${PDYm2}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2" 
            msg="Warning: Using $COMINcmcem3/${PDYm3}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2 to fill in the gap."
            postmsg "$jlogfile" "$msg"
            echo "$job Warning: P-ETSS results are from NAEFS, but used the previous CMC cycle wind data in spinup part." > ${COMOUT}/PETSS_NAEFS_${cyc}
            cpreq  $COMINcmcem3/${PDYm3}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2 ${WNDS}${PEP}hind200
          fi
        else
          cpreq  $COMINcmcem2/${PDYm2}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2 ${WNDS}${PEP}hind200
        fi
        if [[ ! -s $COMINcmce/${PDY}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2 ]]; then
          if [[ ! -s $COMINcmcem1/${PDYm1}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2 ]]; then
            if [[ ! -s $COMINcmcem1/${PDYm1}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P024_0${PEP}.grib2 ]]; then
              if [[ ! -s $COMINcmcem2/${PDYm2}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P036_0${PEP}.grib2 ]]; then
                if [[ ! -s $COMINcmcem2/${PDYm2}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P048_0${PEP}.grib2 ]]; then
                  msg="Warning: Five consecutive CMC cycles hindacast wind data are missing."
                  msg="Warning:    Missing file: $COMINcmce/${PDY}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2"
                  msg="Warning:    Missing file: $COMINcmcem1/${PDYm1}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2"
                  msg="Warning:    Missing file: $COMINcmcem1/${PDYm1}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P024_0${PEP}.grib2"
                  msg="Warning:    Missing file: $COMINcmcem2/${PDYm2}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P036_0${PEP}.grib2"
                  msg="Warning:    Missing file: $COMINcmcem2/${PDYm2}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P048_0${PEP}.grib2"
                  msg="Warning: P-ETSS is running with GEFS only forcing."
                  postmsg "$jlogfile" "$msg"
                  echo "$job Warning: P-ETSS results are from GEFS only due to five CMC cycles of missing hindacast wind data." > ${COMOUT}/PETSS_GEFS_${cyc}
                else
                  msg="Warning: Four consecutive CMC cycles hindacast wind data are missing."
                  msg="Warning:    Missing file: $COMINcmce/${PDY}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2"
                  msg="Warning:    Missing file: $COMINcmcem1/${PDYm1}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2"
                  msg="Warning:    Missing file: $COMINcmcem1/${PDYm1}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P024_0${PEP}.grib2"
                  msg="Warning:    Missing file: $COMINcmcem2/${PDYm2}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P036_0${PEP}.grib2"
                  msg="Warning: Using $COMINcmcem2/${PDYm2}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P048_0${PEP}.grib2 to fill in the gap."
                  postmsg "$jlogfile" "$msg"
                  echo "$job Warning: P-ETSS results are from NAEFS, but used the previous 4 CMC cycle wind data in spinup part." > ${COMOUT}/PETSS_NAEFS_${cyc}
                  cpreq  $COMINcmcem2/${PDYm2}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P048_0${PEP}.grib2 ${WNDS}${PEP}hind000
                fi
              else
                msg="Warning: Three consecutive CMC cycles hindacast wind data are missing."
                msg="Warning:    Missing file: $COMINcmce/${PDY}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2"
                msg="Warning:    Missing file: $COMINcmcem1/${PDYm1}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2"
                msg="Warning:    Missing file: $COMINcmcem1/${PDYm1}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P024_0${PEP}.grib2"
                msg="Warning: Using $COMINcmcem2/${PDYm2}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P036_0${PEP}.grib2 to fill in the gap."
                postmsg "$jlogfile" "$msg"
                echo "$job Warning: P-ETSS results are from NAEFS, but used the previous 3 CMC cycle wind data in spinup part." > ${COMOUT}/PETSS_NAEFS_${cyc}
                cpreq  $COMINcmcem2/${PDYm2}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P036_0${PEP}.grib2 ${WNDS}${PEP}hind000
              fi
            else
              msg="Warning: Two consecutive CMC cycles hindacast wind data are missing."
              msg="Warning:    Missing file: $COMINcmce/${PDY}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2"
              msg="Warning:    Missing file: $COMINcmcem1/${PDYm1}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2"
              msg="Warning: Using $COMINcmcem1/${PDYm1}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P024_0${PEP}.grib2 to fill in the gap."
              postmsg "$jlogfile" "$msg"
              echo "$job Warning: P-ETSS results are from NAEFS, but used the previous 2 CMC cycle wind data in spinup part." > ${COMOUT}/PETSS_NAEFS_${cyc}
              cpreq  $COMINcmcem1/${PDYm1}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P024_0${PEP}.grib2 ${WNDS}${PEP}hind000
            fi
          else
            msg="Warning: One CMC cycles hindacast wind data are missing."
            msg="Warning:    Missing file: $COMINcmce/${PDY}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2"
            msg="Warning: Using $COMINcmcem1/${PDYm1}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2 to fill in the gap."
            postmsg "$jlogfile" "$msg"
            echo "$job Warning: P-ETSS results are from NAEFS, but used the previous CMC cycle wind data in spinup part." > ${COMOUT}/PETSS_NAEFS_${cyc}
            cpreq  $COMINcmcem1/${PDYm1}${CYC3}_CMC_naefs_hr_latlon0p5x0p5_P012_0${PEP}.grib2 ${WNDS}${PEP}hind000
          fi
        else
          cpreq  $COMINcmce/${PDY}${CYC1}_CMC_naefs_hr_latlon0p5x0p5_P000_0${PEP}.grib2 ${WNDS}${PEP}hind000
        fi
      fi
    fi
    ;;
  esac

echo "`date`: Finished Second copy of files" >> timing.txt
echo "Finished second copy of files" > msg_Done_second_cp_${WNDS}${PEP}.txt

fi

while [[ ! -f msg_Done_second_cp_${WNDS}${PEP}.txt || ! -f msg_Done_first_cp_${WNDS}${PEP}.txt ]] ; do
  echo "`date`: Rank ${RANK} waiting for copying files." >> timing.txt
  sleep 1
done

echo "RANK ${RANKO} is ready for wind pre-processing." >> wind_ready.txt

num_line1=$(grep "RANK" wind_ready.txt | wc -l)

while [[ $num_line1 -lt ${cpuNUM} ]] ; do
  echo "`date`: Rank ${RANKO} Sleeping nums of lines." >> timing.txt
  num_line1=$(grep "RANK" wind_ready.txt | wc -l)
  sleep 1
done

if [[ ${RANKO} == 0 ]] ; then
  if [[ -s ${COMOUT}/PETSS_GEFS_${cyc} ]] ; then
    echo "DATE=${PDY}_${cyc}" > ${COMOUT}/petss.t${cyc}z.meta.txt
    echo "Warning: P-ETSS is resorting to using GEFS only due to problems accessing the CMC" >> ${COMOUT}/petss.t${cyc}z.meta.txt
    echo "Warning: P-ETSS total number of members: 31" >> ${COMOUT}/petss.t${cyc}z.meta.txt
    echo "..." >> ${COMOUT}/petss.t${cyc}z.meta.txt
    echo "We're missing the current and previous CMC model runs, so need to run in GEFS only mode." >> ${COMOUT}/petss.t${cyc}z.meta.txt
    echo "No action for the SPA other than to work to get CMC model data ready for the next run." >> ${COMOUT}/petss.t${cyc}z.meta.txt
    echo "..." >> ${COMOUT}/petss.t${cyc}z.meta.txt
    echo "For more information on the missing files, please do a 'grep Warning $jlogfile'" >> ${COMOUT}/petss.t${cyc}z.meta.txt
    echo "" >> ${COMOUT}/petss.t${cyc}z.meta.txt
  elif [[ -s ${COMOUT}/PETSS_NAEFS_${cyc} ]] ; then
    echo "DATE=${PDY}_${cyc}" > ${COMOUT}/petss.t${cyc}z.meta.txt
    echo "Warning: P-ETSS is using the previous CMC model run for the spin-up portion." >> ${COMOUT}/petss.t${cyc}z.meta.txt
    echo "Warning: P-ETSS total number of members: 52" >> ${COMOUT}/petss.t${cyc}z.meta.txt
    echo "..." >> ${COMOUT}/petss.t${cyc}z.meta.txt
    echo "We had problems reading the most recent hind-cast files from the CMC for P-ETSS spin-up," >> ${COMOUT}/petss.t${cyc}z.meta.txt
    echo "so we're using previous CMC cycles to fill in the gaps." >> ${COMOUT}/petss.t${cyc}z.meta.txt
    echo "..." >> ${COMOUT}/petss.t${cyc}z.meta.txt
    echo "No action for the SPA other than to let CMC and or DataFlow know there were problems," >> ${COMOUT}/petss.t${cyc}z.meta.txt
    echo "with the most recent CMC files.  They likely haven't finished recovery from a CMC outage." >> ${COMOUT}/petss.t${cyc}z.meta.txt
    echo "..." >> ${COMOUT}/petss.t${cyc}z.meta.txt
    echo "For more information on the missing files, please do a 'grep Warning $jlogfile'" >> ${COMOUT}/petss.t${cyc}z.meta.txt
  elif [[ -s ${COMOUT}/PETSS_NAEFS1_${cyc} ]] ; then
    echo "DATE=${PDY}_${cyc}" > ${COMOUT}/petss.t${cyc}z.meta.txt
    echo "Warning: P-ETSS is using the previous CMC model run for the forecast portion." >> ${COMOUT}/petss.t${cyc}z.meta.txt
    echo "Warning: P-ETSS total number of members: 52" >> ${COMOUT}/petss.t${cyc}z.meta.txt
    echo "..." >> ${COMOUT}/petss.t${cyc}z.meta.txt
    echo "We had problems reading the most recent forecast files from the CMC," >> ${COMOUT}/petss.t${cyc}z.meta.txt
    echo "so we're using previous CMC cycle's forecast." >> ${COMOUT}/petss.t${cyc}z.meta.txt
    echo "..." >> ${COMOUT}/petss.t${cyc}z.meta.txt
    echo "No action for the SPA other than to let CMC and or DataFlow know there were problems," >> ${COMOUT}/petss.t${cyc}z.meta.txt
    echo "with the most recent CMC files.  They likely haven't finished recovery from a CMC outage." >> ${COMOUT}/petss.t${cyc}z.meta.txt
    echo "..." >> ${COMOUT}/petss.t${cyc}z.meta.txt
    echo "For more information on the missing files, please do a 'grep Warning $jlogfile'" >> ${COMOUT}/petss.t${cyc}z.meta.txt
  fi
fi

if [[ ${WNDS} == gep || ${WNDS} == gec || ${WNDS} == cmc_gec && ! -s ${COMOUT}/PETSS_GEFS_${cyc} || ${WNDS} == cmc_gep && ! -s ${COMOUT}/PETSS_GEFS_${cyc} ]] ; then

if [[ ${RANK} == 0 ]] ; then
######################################################################
#  Run program to extract current and forecast (future) global
#    surface pressure, and U and V wind vector fields.
######################################################################
  export pgm="petss_in_wind_fcst"
  . prep_step

  if [[ ${WNDS} == gep || ${WNDS} == gec ]] ; then
    if [[ ${RUN_NewGEFS} == YES ]] ; then
      echo 'gfnw' ${cyc}> gefs.cmce.fcst.${RANK}.${WNDS}${PEP}.${cyc}
    else
      echo 'gefs' ${cyc}> gefs.cmce.fcst.${RANK}.${WNDS}${PEP}.${cyc}
    fi
  else
    echo 'cmce' ${cyc}> gefs.cmce.fcst.${RANK}.${WNDS}${PEP}.${cyc}
  fi
  export FORT10=gefs.cmce.fcst.${RANK}.${WNDS}${PEP}.${cyc}

  export FORT11=${WNDS}${PEP}fore000
  export FORT12=${WNDS}${PEP}fore003
  export FORT13=${WNDS}${PEP}fore006
  export FORT14=${WNDS}${PEP}fore009
  export FORT15=${WNDS}${PEP}fore012
  export FORT16=${WNDS}${PEP}fore015
  export FORT17=${WNDS}${PEP}fore018
  export FORT18=${WNDS}${PEP}fore021
  export FORT19=${WNDS}${PEP}fore024
  export FORT20=${WNDS}${PEP}fore027
  export FORT21=${WNDS}${PEP}fore030
  export FORT22=${WNDS}${PEP}fore033
  export FORT23=${WNDS}${PEP}fore036
  export FORT24=${WNDS}${PEP}fore039
  export FORT25=${WNDS}${PEP}fore042
  export FORT26=${WNDS}${PEP}fore045
  export FORT27=${WNDS}${PEP}fore048
  export FORT28=${WNDS}${PEP}fore051
  export FORT29=${WNDS}${PEP}fore054
  export FORT30=${WNDS}${PEP}fore057
  export FORT31=${WNDS}${PEP}fore060
  export FORT32=${WNDS}${PEP}fore063
  export FORT33=${WNDS}${PEP}fore066
  export FORT34=${WNDS}${PEP}fore069
  export FORT35=${WNDS}${PEP}fore072
  export FORT36=${WNDS}${PEP}fore075
  export FORT37=${WNDS}${PEP}fore078
  export FORT38=${WNDS}${PEP}fore081
  export FORT39=${WNDS}${PEP}fore084
  export FORT40=${WNDS}${PEP}fore087
  export FORT41=${WNDS}${PEP}fore090
  export FORT42=${WNDS}${PEP}fore093
  export FORT43=${WNDS}${PEP}fore096
  export FORT44=${WNDS}${PEP}fore099
  export FORT45=${WNDS}${PEP}fore102
  export FORT46=${WNDS}${PEP}fore105
  export FORT47=${WNDS}${PEP}fore108

  export pgm="petss_in_wind_fcst"

  export FORT49=$PARMpetss/model/mdl_ft11.egnkmw
  export FORT51=gfspuv.${WNDS}${PEP}.${cyc}e
  export FORT52=gfspuv.${WNDS}${PEP}.${cyc}g
  export FORT53=gfspuv.${WNDS}${PEP}.${cyc}n
  export FORT54=gfspuv.${WNDS}${PEP}.${cyc}k
  export FORT55=gfspuv.${WNDS}${PEP}.${cyc}m
  export FORT56=gfspuv.${WNDS}${PEP}.${cyc}w

  export FORT96=sds.${WNDS}${PEP}.${cyc}
       
  startmsg
  $EXECpetss/petss_in_wind_fcst >> $pgmout 2> errfile_${RANKO}
  err=$?;export err; err_chk
  #cpreq gfspuv.${WNDS}${PEP}.${cyc}* $COMOUT/
  echo "Finished creating ForeCast Winds1" >> msg_FcstWinds1_${WNDS}${PEP}.txt
  echo "`date`: Rank ${RANKO} Finished with Forecast winds" >> timing.txt

else

######################################################################
#  Run program to extract Past (historic) global surface pressure,
#    and U and V wind vector fields.
######################################################################
  export pgm="petss_in_wind_hindcst"
  . prep_step

  if [[ ${WNDS} == gep || ${WNDS} == gec ]] ; then
    if [[ ${RUN_NewGEFS} == YES ]] ; then
      echo 'gfnw' > gefs.cmce.${RANK}.${WNDS}${PEP}.${cyc}
    else
      echo 'gefs' > gefs.cmce.${RANK}.${WNDS}${PEP}.${cyc}
    fi
    export FORT31=gefs.cmce.${RANK}.${WNDS}${PEP}.${cyc}
  case "$cyc" in
  00)  # 00Z cycle
    export FORT11=${WNDS}${PEP}hind312
    export FORT12=${WNDS}${PEP}hind318
    export FORT13=${WNDS}${PEP}hind200
    export FORT14=${WNDS}${PEP}hind206
    export FORT15=${WNDS}${PEP}hind212
    export FORT16=${WNDS}${PEP}hind218
    export FORT17=${WNDS}${PEP}hind100
    export FORT18=${WNDS}${PEP}hind106
    export FORT19=${WNDS}${PEP}hind112
    export FORT20=${WNDS}${PEP}hind118
    ;;
  06)  # 06Z cycle
    export FORT11=${WNDS}${PEP}hind318
    export FORT12=${WNDS}${PEP}hind200
    export FORT13=${WNDS}${PEP}hind206
    export FORT14=${WNDS}${PEP}hind212
    export FORT15=${WNDS}${PEP}hind218
    export FORT16=${WNDS}${PEP}hind100
    export FORT17=${WNDS}${PEP}hind106
    export FORT18=${WNDS}${PEP}hind112
    export FORT19=${WNDS}${PEP}hind118
    export FORT20=${WNDS}${PEP}hind000
    ;;
  12)  # 12Z cycle
    export FORT11=${WNDS}${PEP}hind200
    export FORT12=${WNDS}${PEP}hind206
    export FORT13=${WNDS}${PEP}hind212
    export FORT14=${WNDS}${PEP}hind218
    export FORT15=${WNDS}${PEP}hind100
    export FORT16=${WNDS}${PEP}hind106
    export FORT17=${WNDS}${PEP}hind112
    export FORT18=${WNDS}${PEP}hind118
    export FORT19=${WNDS}${PEP}hind000
    export FORT20=${WNDS}${PEP}hind006
    ;;
  18)  # 18z cycle
    export FORT11=${WNDS}${PEP}hind206
    export FORT12=${WNDS}${PEP}hind212
    export FORT13=${WNDS}${PEP}hind218
    export FORT14=${WNDS}${PEP}hind100
    export FORT15=${WNDS}${PEP}hind106
    export FORT16=${WNDS}${PEP}hind112
    export FORT17=${WNDS}${PEP}hind118
    export FORT18=${WNDS}${PEP}hind000
    export FORT19=${WNDS}${PEP}hind006
    export FORT20=${WNDS}${PEP}hind012
    ;;
  esac

  else
    echo 'cmce' > gefs.cmce.${RANK}.${WNDS}${PEP}.${cyc}
    export FORT31=gefs.cmce.${RANK}.${WNDS}${PEP}.${cyc}

  case "$cyc" in
  00)  # 00Z cycle
    export FORT11=${WNDS}${PEP}hind312
    export FORT12=${WNDS}${PEP}hind200
    export FORT13=${WNDS}${PEP}hind212
    export FORT14=${WNDS}${PEP}hind100
    export FORT15=${WNDS}${PEP}hind112
    ;;
  06)  # 00Z cycle
    export FORT11=${WNDS}${PEP}hind312
    export FORT12=${WNDS}${PEP}hind200
    export FORT13=${WNDS}${PEP}hind212
    export FORT14=${WNDS}${PEP}hind100
    export FORT15=${WNDS}${PEP}hind112 
    ;;
  12)  # 12Z cycle
    export FORT11=${WNDS}${PEP}hind200
    export FORT12=${WNDS}${PEP}hind212
    export FORT13=${WNDS}${PEP}hind100
    export FORT14=${WNDS}${PEP}hind112
    export FORT15=${WNDS}${PEP}hind000
    ;;
  18)  # 12Z cycle
    export FORT11=${WNDS}${PEP}hind200
    export FORT12=${WNDS}${PEP}hind212
    export FORT13=${WNDS}${PEP}hind100
    export FORT14=${WNDS}${PEP}hind112
    export FORT15=${WNDS}${PEP}hind000
    ;;
  esac
  
  fi
  export pgm="petss_in_wind_hindcst"

  export FORT30=$PARMpetss/model/mdl_ft11.egnkmw
  export FORT51=cylf10.${WNDS}${PEP}.${cyc}e
  export FORT52=cylf10.${WNDS}${PEP}.${cyc}g
  export FORT53=cylf10.${WNDS}${PEP}.${cyc}n
  export FORT54=cylf10.${WNDS}${PEP}.${cyc}k
  export FORT55=cylf10.${WNDS}${PEP}.${cyc}m
  export FORT56=cylf10.${WNDS}${PEP}.${cyc}w

  export FORT96=sds2a.${WNDS}${PEP}.${cyc}
  export FORT81=cylp1.${WNDS}${PEP}.tmp
  export FORT82=cylu1.${WNDS}${PEP}.tmp
  export FORT83=cylv1.${WNDS}${PEP}.tmp

  startmsg
  $EXECpetss/petss_in_wind_hindcst >> $pgmout 2> errfile_${RANKO}
  err=$?;export err; err_chk
  #cpreq cylf10.${WNDS}${PEP}.${cyc}* $COMOUT/
  echo "Finished creating Hind Cast Winds for Extra Tropical Basins" >> msg_HindWinds1_${WNDS}${PEP}.txt
  echo "`date`: Rank ${RANKO} Finished with Hind Cast winds." >> timing.txt

fi

fi
