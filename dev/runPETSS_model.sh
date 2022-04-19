#!/bin/bash -l

module load prod_util

if [ $# -ne 4 ] ; then
  now=$(date +"%Y%m%d")
  echo "You are trying to run the PETSS model but you missed something--"
  echo ""
  echo "Usage $0 <date ${now}> <cycle (00, 06, 12, 18)> <copy data (copy/no-copy)>"
  echo ""
  exit
fi

src=$(cd $(dirname $0) && pwd)
root=${src}


set -x

export PDY=$1
export cyc=$2
export cycle=t${cyc}z

# Paramter to determin whether model need copy data from DATA TANK
export f_GetData=$3
export web=$4

export WIND=NAEFS #Use NAEFS
export MDLFORCEDATE=yes
export RUN_NewGEFS=YES #New GEFS (31 ensemble members)
export GEFS_para=NO
export RUN_RETRO=NO #NewGEFS Retrospective run's data
# Add some debugs...
set -xa


###############################################
#  Indicate to the J script that this is an 
#  "MDLTEST" and don't delete the running data
###############################################
export RUN_ENVIR=nco
#export RUN_ENVIR=MDLTEST
export KEEPDATA=NO
export HIGHRES=NO #Copy native resolution rex files to /com
export MDLTEST_HOME=${root}

export envir=prod
export MDLTEST_DIR=${root}/petss.v1.1.0

cnt=0
while [ -e ${MDLTEST_DIR}/tmpnwprd1/take${cnt} && ! "$(ls -A ${MDLTEST_DIR}/tmpnwprd1/take${cnt})" ] ; do
   cnt=$[$cnt+1]
done

mkdir -p ${MDLTEST_DIR}
mkdir -p ${MDLTEST_DIR}/tmpnwprd1/take${cnt}
mkdir -p ${MDLTEST_DIR}/$envir/com/logs

export COMROOT=${MDLTEST_DIR}/$envir/com
export DATAROOT=${MDLTEST_DIR}/tmpnwprd1/take${cnt}
export DBNROOT=${MDLTEST_HOME}/dbnet
export HOMEpetss=${MDLTEST_HOME}/..
export SENDDBN_NTC=YES

export SENDCOM=${SENDCOM:-YES}
export SENDDBN=${SENDDBN:-YES}
export SENDECF=${SENDECF:-YES}

export DCOMROOT="/lfs/h1/ops/canned/dcom"

export COMROOT1="/lfs/h1/mdl/marine/noscrub/Huiqing.Liu/petss.v1.2.0/prod/com"
export DCOMROOT1="/lfs/h1/ops/canned/dcom"

cd ${MDLTEST_DIR}/tmpnwprd1

setpdy.sh
. PDY

export jlogfile=${COMROOT}/logs/jlogfile.${PDY}.${cyc}

###############################################
#  COPY GEFS data from DATA TANK
###############################################
if [[ ${GEFS_para} == "YES" ]] ; then
   export GEFS_V=/gpfs/dell4/nco/ops/com/gefs/para/gefs.${PDY}
else
   export GEFS_V=$(compath.py gefs/prod/gefs.$PDY)
fi
if [[ ${RUN_NewGEFS} == YES ]] ; then
   GEFS_Res='0p25'
   cycatmos=${cyc}/atmos/pgrb2sp25
   GEFS_Fn='pgrb2s'
else
   GEFS_Res='0p50'
   cycatmos=${cyc}/pgrb2ap5
   GEFS_Fn='pgrb2a'
fi

export CMCE_V=${DCOMROOT}/${PDY}/wgrbbul/cmcens_gb2

if [[ $f_GetData == "copy" && ${RUN_ENVIR} == "MDLTEST" ]] ; then
   if [[ ${cyc} == 00 || ${cyc} == 12 ]]; then
       Start=$SECONDS
       while [[ ! -s ${CMCE_V}/${PDY}${cyc}_CMC_naefs_hr_latlon0p5x0p5_P102_020.grib2 ]]; do
             Dura=$((SECONDS-Start))
             if [ $Dura -ge 1800 ]; then
                msg="$job Error: Wind file ${COMINgefs}/${cycatmos}/${WNDS}${PEP}.${cycle}.${GEFS_Fn}.${GEFS_Res}.f$i is missed after 10 mins wait"
                postmsg "$jlogfile" "$msg"
                err_exit
                break
             fi
             sleep 1
       done

       Start=$SECONDS
       while [[ ! -s ${GEFS_V}/${cycatmos}/gec00.${cycle}.${GEFS_Fn}.${GEFS_Res}.f102 ]]; do
             Dura=$((SECONDS-Start))
             if [ $Dura -ge 1800 ]; then
                msg="$job Error: Wind file ${COMINgefs}/${cycatmos}/${WNDS}${PEP}.${cycle}.${GEFS_Fn}.${GEFS_Res}.f$i is missed after 10 mins wait"
                postmsg "$jlogfile" "$msg"
                err_exit
                break
             fi
             sleep 1
       done
       ${root}/setup/setup_naefs.sh
   else
       Start=$SECONDS
       while [[ ! -s ${GEFS_V}/${cycatmos}/gec00.${cycle}.${GEFS_Fn}.${GEFS_Res}.f102 ]]; do
             Dura=$((SECONDS-Start))
             if [ $Dura -ge 1800 ]; then
                msg="$job Error: Wind file ${COMINgefs}/${cycatmos}/${WNDS}${PEP}.${cycle}.${GEFS_Fn}.${GEFS_Res}.f$i is missed after 10 mins wait"
                postmsg "$jlogfile" "$msg"
                err_exit
                break
             fi
             sleep 1
       done
       ${root}/setup/setup_naefs.sh
   fi
fi
export COMPATH=${COMROOT}

export COMINSS=${COMROOT}/petss/v1.2

#if [[ ${RUN_ENVIR} == "MDLTEST" ]] ; then
#   export COMINcmce=${COMROOT}/naefs/prod/naefs.${PDY}
#   export COMINcmcem1=${COMROOT}/naefs/prod/naefs.${PDYm1}
#   export COMINcmcem2=${COMROOT}/naefs/prod/naefs.${PDYm2}
#   export COMINcmcem3=${COMROOT}/naefs/prod/naefs.${PDYm3}

#   export COMINgefs=${COMROOT}/naefs/prod/naefs.${PDY}
#   export COMINgefsm1=${COMROOT}/naefs/prod/naefs.${PDYm1}
#   export COMINgefsm2=${COMROOT}/naefs/prod/naefs.${PDYm2}
#   export COMINgefsm3=${COMROOT}/naefs/prod/naefs.${PDYm3}
export CANNEDROOT=/lfs/h1/ops/canned 
export COMINgefs=${CANNEDROOT}/com/gefs/v12.2/gefs.${PDY}
export COMINgefsm1=${CANNEDROOT}/com/gefs/v12.2/gefs.${PDYm1}
export COMINgefsm2=${CANNEDROOT}/com/gefs/v12.2/gefs.${PDYm2}
export COMINgefsm3=${CANNEDROOT}/com/gefs/v12.2/gefs.${PDYm3}

export COMINcmce=${CANNEDROOT}/dcom/${PDY}/wgrbbul/cmcens_gb2
export COMINcmcem1=${CANNEDROOT}/dcom/${PDYm1}/wgrbbul/cmcens_gb2
export COMINcmcem2=${CANNEDROOT}/dcom/${PDYm2}/wgrbbul/cmcens_gb2
export COMINcmcem3=${CANNEDROOT}/dcom/${PDYm3}/wgrbbul/cmcens_gb2
export COMINcmcem4=${CANNEDROOT}/dcom/${PDYm4}/wgrbbul/cmcens_gb2
export COMINcmcem5=${CANNEDROOT}/dcom/${PDYm5}/wgrbbul/cmcens_gb2

#else
#   export COMINgefs=/gpfs/dell4/nco/ops/com/gefs/para/gefs.${PDY}
#   export COMINgefsm1=/gpfs/dell4/nco/ops/com/gefs/para/gefs.${PDYm1}
#   export COMINgefsm2=/gpfs/dell4/nco/ops/com/gefs/para/gefs.${PDYm2}
#   export COMINgefsm3=/gpfs/dell4/nco/ops/com/gefs/para/gefs.${PDYm3}
#fi

###############################################
# Kick off the LSF script
###############################################
if [[ ${WIND} == NAEFS ]]; then
   if [[ ${RUN_NewGEFS} == YES ]] ; then
     export nodesNUM=31
   else
     export nodesNUM=25
   fi
elif [[ ${WIND} == GEFS ]]; then
    if [[ ${RUN_NewGEFS} == YES ]] ; then
       export nodesNUM=19
    else
       export nodesNUM=13
    fi
fi

export job=petss_${PDY}_${cyc}
export outid="LL$job"
export jobid="${outid}.o${pid}"

export petss_prewnd_id=$(${root}/myEcf/jpetss_prewnd.ecf)

export petss_id=$(${root}/myEcf/jpetss.ecf)

export petss_postprob_id=$(${root}/myEcf/jpetss_postprob.ecf)

export petss_merg_id=$(${root}/myEcf/jpetss_merg.ecf)

############################################################
${root}/runPost_PETSS.sh ${PDY} ${cyc} N          #submit PETSS post processing job
if [[ ${web} == "Y" ]]; then
   ${root}/run_gen_png_prod.sh ${PDY} ${cyc} N       #submit generating images job
fi

exit
############## END OF SCRIPT #######################
