#!/bin/bash
# 
# Huiqing.Liu/MDL--- 03/08/2016 -------PETSS model nesting run script ---------
#
# "HISTORY: March 08, 2016 - First Version of PETSS1.0" 
# "         Nov,      2018 - PETSS1.1"
# " ---------------------------------------------------------"
# 
# Author: Huiqing.Liu (huiqing.liu@noaa.gov) 
# Abstract: 
# Running SLOSH model in different basins driven by different ensemble wind memebers
#  Inputs:  Basin, cycle, parm locaion, exe location, tide version, wind members 
#           
#  Outputs: Station/Grid output
#############################################################################################

src=$(cd $(dirname $0) && pwd)
root=${src}/..

if test "$8" != ''
then
  echo "Usage $0 <'g' or 'e' for all other basins> <cycle> <PARMpetss> <EXECpetss> <Tide> <GEC/GEP><00,01..>"
  echo "Example: g 00 $root/parm $root/exec 0 GEC 00" 
  exit
fi
if test "$7" == ''
then
  echo "Usage $0 <'g' or 'e' for all other basins> <cycle> <PARMpetss> <EXECpetss><Tide> <GEC/GEP><00,01..>"
  echo "Example: g 00 $root/parm $root/exec 0 GEC 00" 
  exit
fi

f_gulf=$1
cyc=$2
PARMpetss=$3
EXECpetss=$4
tide_v=$5
WNDS=$6
PEP=$7

PARMpetssmodel=${PARMpetss}/model

if [[ ${tide_v} == 0 ]] ; then
   fle_ext="surge"
else
   fle_ext="surge_tide"
fi

######################################################################

. prep_step

export FORT21=${PARMpetssmodel}/mdl_ft01.ega

if [[ ${WNDS} == gep || ${WNDS} == gec ]] ; then
   if [[ ${RUN_NewGEFS} == YES ]] ; then
      if [[ RUN_RETRO == YES ]] ; then
# new GEFS retrspective run 0.25 deg 3-hr forecast wind, 12-hr analysis wind         
         echo '5' > wndsmod.${RANKO}.${WNDS}${PEP}.${cyc}
      else
# new GEFS realtime run 0.25 deg 3-hr forecast wind, 6-hr analysis wind         
         echo '3' > wndsmod.${RANKO}.${WNDS}${PEP}.${cyc}
      fi
   else
      echo '1' > wndsmod.${RANKO}.${WNDS}${PEP}.${cyc}
   fi
else 
   echo '2' > wndsmod.${RANKO}.${WNDS}${PEP}.${cyc}
   if [[ ${cyc} == 06 || ${cyc} == 18 ]] ;then
      export FORT21=${PARMpetssmodel}/mdl_ft01.ega.cmc
   fi 
fi 
export FORT12=wndsmod.${RANKO}.${WNDS}${PEP}.${cyc}

export FORT22=water-levels.dat

export iopt=2

if [[ ${f_gulf} == cp5 ]] ; then

      bsn=cp5

      export pgm="petss_model"
#      . prep_step
      echo e ${iopt} > fle20.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT19=fle20.${WNDS}${PEP}.${fle_ext}.${bsn}
 
      export FORT81=fle10.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT84=fle40.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT87=fle70.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT88=fle40.agl.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT96=sds.${WNDS}${PEP}.${cyc}

      export FORT11=${PARMpetssmodel}/mdl_ft11.${bsn}
      export FORT14=${PARMpetssmodel}/mdl_ettgp.${bsn}
      export FORT15=${PARMpetssmodel}/mdl_ettgp.${bsn}_2nd
      export FORT33=cylf10.${WNDS}${PEP}.${cyc}e
      export FORT34=gfspuv.${WNDS}${PEP}.${cyc}e
      export FORT52=sshistory.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
      export FORT55=sshistory.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}_2nd
      export FORT53=ssgrid.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
      export FORT54=ssgrid.agl.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
      export FORT50=wl.${WNDS}${PEP}.${fle_ext}.${bsn}

      export FORT20=out_nesting5.${WNDS}${PEP}.bin

      echo "`date`: Starting Run for ${bsn} in tide ${tide_v}" >> timing.txt
      if [[ $pgmout == '' ]] ; then
         pgmout=OUTPUT.0000
      fi
      startmsg
         ${EXECpetss}/petss_model -basin ${bsn} -rootDir ${PARMpetss} -trk ${PARMpetss}/trkfiles/tideOnly_14.trk -rex ${bsn}.${WNDS}${PEP}.${fle_ext}.etss2.0.rex -env ${bsn}.${fle_ext}.etss2.0.env -f_tide ${tide_v} -TideDatabase 2014 -spinUp 0 -rexSave 6 -nest 21 -verbose 1 >> $pgmout 2> errfile_${RANKO}
      err=$?;export err; err_chk
      echo "`date`: Finished Run for ${bsn} in tide ${tide_v}" >> timing.txt

elif [[ ${f_gulf} == emo2 ]] ; then
 
      bsn=emo2
 
      export pgm="petss_model"
#      . prep_step
      echo g ${iopt} > fle20.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT19=fle20.${WNDS}${PEP}.${fle_ext}.${bsn}

      export FORT81=fle10.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT84=fle40.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT87=fle70.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT88=fle40.agl.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT96=sds.${WNDS}${PEP}.${cyc}

      export FORT11=${PARMpetssmodel}/mdl_ft11.${bsn}
      export FORT14=${PARMpetssmodel}/mdl_ettgp.${bsn}
      export FORT15=${PARMpetssmodel}/mdl_ettgp.${bsn}_2nd
      export FORT33=cylf10.${WNDS}${PEP}.${cyc}g
      export FORT34=gfspuv.${WNDS}${PEP}.${cyc}g
      export FORT52=sshistory.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
      export FORT55=sshistory.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}_2nd
      export FORT53=ssgrid.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
      export FORT54=ssgrid.agl.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
      export FORT50=wl.${WNDS}${PEP}.${fle_ext}.${bsn}


      export FORT20=out_nesting22.${WNDS}${PEP}.bin

      echo "`date`: Starting Run for ${bsn} in tide ${tide_v}" >> timing.txt
      if [[ $pgmout == '' ]] ; then
         pgmout=OUTPUT.0000
      fi
      startmsg
         ${EXECpetss}/petss_model -basin ${bsn} -rootDir ${PARMpetss} -trk ${PARMpetss}/trkfiles/tideOnly_14.trk -rex ${bsn}.${WNDS}${PEP}.${fle_ext}.etss2.0.rex -env ${bsn}.${fle_ext}.etss2.0.env -f_tide ${tide_v} -TideDatabase 2014 -spinUp 0 -rexSave 6 -nest 21 -verbose 1 >> $pgmout 2> errfile_${RANKO}
      err=$?;export err; err_chk
      echo "`date`: Finished Run for ${bsn} in tide ${tide_v}" >> timing.txt

elif [[ ${f_gulf} == hor3 ]] ; then

      bsn=hor3

      export pgm="petss_model"
#      . prep_step
      echo e ${iopt} > fle20.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT19=fle20.${WNDS}${PEP}.${fle_ext}.${bsn}

      export FORT81=fle10.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT84=fle40.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT87=fle70.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT88=fle40.agl.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT96=sds.${WNDS}${PEP}.${cyc}
 
      export FORT11=${PARMpetssmodel}/mdl_ft11.${bsn}
      export FORT14=${PARMpetssmodel}/mdl_ettgp.${bsn}
      export FORT15=${PARMpetssmodel}/mdl_ettgp.${bsn}_2nd
      export FORT33=cylf10.${WNDS}${PEP}.${cyc}e
      export FORT34=gfspuv.${WNDS}${PEP}.${cyc}e
      export FORT52=sshistory.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
      export FORT55=sshistory.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}_2nd
      export FORT53=ssgrid.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
      export FORT54=ssgrid.agl.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
      export FORT50=wl.${WNDS}${PEP}.${fle_ext}.${bsn}
 

      export FORT20=out_nesting6.${WNDS}${PEP}.bin

      echo "`date`: Starting Run for ${bsn} in tide ${tide_v}" >> timing.txt
      if [[ $pgmout == '' ]] ; then
         pgmout=OUTPUT.0000
      fi
      startmsg
         ${EXECpetss}/petss_model -basin ${bsn} -rootDir ${PARMpetss} -trk ${PARMpetss}/trkfiles/tideOnly_14.trk -rex ${bsn}.${WNDS}${PEP}.${fle_ext}.etss2.0.rex -env ${bsn}.${fle_ext}.etss2.0.env -f_tide ${tide_v} -TideDatabase 2014 -spinUp 0 -rexSave 6 -nest 21 -verbose 1 >> $pgmout 2> errfile_${RANKO}
      err=$?;export err; err_chk
      echo "`date`: Finished Run for ${bsn} in tide ${tide_v}" >> timing.txt

elif [[ ${f_gulf} == epn3 ]] ; then

   bsn=epn3

   export pgm="petss_model"
#   . prep_step
   echo g ${iopt} > fle20.${WNDS}${PEP}.${fle_ext}.${bsn}
   export FORT19=fle20.${WNDS}${PEP}.${fle_ext}.${bsn}

   export FORT81=fle10.${WNDS}${PEP}.${fle_ext}.${bsn}
   export FORT84=fle40.${WNDS}${PEP}.${fle_ext}.${bsn}
   export FORT87=fle70.${WNDS}${PEP}.${fle_ext}.${bsn}
   export FORT88=fle40.agl.${WNDS}${PEP}.${fle_ext}.${bsn}
   export FORT96=sds.${WNDS}${PEP}.${cyc}

   export FORT11=${PARMpetssmodel}/mdl_ft11.${bsn}
   export FORT14=${PARMpetssmodel}/mdl_ettgp.${bsn}
   export FORT15=${PARMpetssmodel}/mdl_ettgp.${bsn}_2nd
   export FORT33=cylf10.${WNDS}${PEP}.${cyc}g
   export FORT34=gfspuv.${WNDS}${PEP}.${cyc}g
   export FORT52=sshistory.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
   export FORT55=sshistory.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}_2nd
   export FORT53=ssgrid.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
   export FORT54=ssgrid.agl.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
   export FORT50=wl.${WNDS}${PEP}.${fle_ext}.${bsn}

   export FORT20=out_nesting21.${WNDS}${PEP}.bin

   echo "`date`: Starting Run for ${bsn} in tide ${tide_v}" >> timing.txt
   if [[ $pgmout == '' ]] ; then
      pgmout=OUTPUT.0000
   fi
   startmsg
   ${EXECpetss}/petss_model -basin ${bsn} -rootDir ${PARMpetss} -trk ${PARMpetss}/trkfiles/tideOnly_14.trk -rex ${bsn}.${WNDS}${PEP}.${fle_ext}.etss2.0.rex -env ${bsn}.${fle_ext}.etss2.0.env -f_tide ${tide_v} -TideDatabase 2014 -spinUp 0 -rexSave 6 -nest 21 -verbose 1 >> $pgmout 2> errfile_${RANKO}
   err=$?;export err; err_chk
   echo "`date`: Finished Run for ${bsn} in tide ${tide_v}" >> timing.txt

elif [[ ${f_gulf} == ejx3 ]] ; then

   for bsn in ejx3
   do
      export pgm="petss_model"
#      . prep_step
      echo e ${iopt} > fle20.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT19=fle20.${WNDS}${PEP}.${fle_ext}.${bsn}

      export FORT81=fle10.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT84=fle40.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT87=fle70.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT88=fle40.agl.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT96=sds.${WNDS}${PEP}.${cyc}

      export FORT11=${PARMpetssmodel}/mdl_ft11.${bsn}
      export FORT14=${PARMpetssmodel}/mdl_ettgp.${bsn}
      export FORT15=${PARMpetssmodel}/mdl_ettgp.${bsn}_2nd
      export FORT33=cylf10.${WNDS}${PEP}.${cyc}e
      export FORT34=gfspuv.${WNDS}${PEP}.${cyc}e
      export FORT52=sshistory.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
      export FORT55=sshistory.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}_2nd
      export FORT53=ssgrid.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
      export FORT54=ssgrid.agl.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
      export FORT50=wl.${WNDS}${PEP}.${fle_ext}.${bsn}

      case ${bsn} in

      ejx3)
         export FORT20=out_nesting11.${WNDS}${PEP}.bin ;;
      esac

      echo "`date`: Starting Run for ${bsn} in tide ${tide_v}" >> timing.txt
      if [[ $pgmout == '' ]] ; then
         pgmout=OUTPUT.0000
      fi
      startmsg
      ${EXECpetss}/petss_model -basin ${bsn} -rootDir ${PARMpetss} -trk ${PARMpetss}/trkfiles/tideOnly_14.trk -rex ${bsn}.${WNDS}${PEP}.${fle_ext}.etss2.0.rex -env ${bsn}.${fle_ext}.etss2.0.env -f_tide ${tide_v} -TideDatabase 2014 -spinUp 0 -rexSave 6 -nest 21 -verbose 1 >> $pgmout 2> errfile_${RANKO}
      err=$?;export err; err_chk
      echo "`date`: Finished Run for ${bsn} in tide ${tide_v}" >> timing.txt
   done

elif [[ ${f_gulf} == ebp3 ]] ; then

   for bsn in ebp3
   do
      export pgm="petss_model"
#      . prep_step
      echo g ${iopt} > fle20.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT19=fle20.${WNDS}${PEP}.${fle_ext}.${bsn}

      export FORT81=fle10.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT84=fle40.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT87=fle70.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT88=fle40.agl.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT96=sds.${WNDS}${PEP}.${cyc}

      export FORT11=${PARMpetssmodel}/mdl_ft11.${bsn}
      export FORT14=${PARMpetssmodel}/mdl_ettgp.${bsn}
      export FORT15=${PARMpetssmodel}/mdl_ettgp.${bsn}_2nd
      export FORT33=cylf10.${WNDS}${PEP}.${cyc}g
      export FORT34=gfspuv.${WNDS}${PEP}.${cyc}g
      export FORT52=sshistory.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
      export FORT55=sshistory.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}_2nd
      export FORT53=ssgrid.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
      export FORT54=ssgrid.agl.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
      export FORT50=wl.${WNDS}${PEP}.${fle_ext}.${bsn}

      case ${bsn} in

      ebp3)
         export FORT20=out_nesting25.${WNDS}${PEP}.bin ;;
      esac

      echo "`date`: Starting Run for ${bsn} in tide ${tide_v}" >> timing.txt
      if [[ $pgmout == '' ]] ; then
         pgmout=OUTPUT.0000
      fi
      startmsg
      ${EXECpetss}/petss_model -basin ${bsn} -rootDir ${PARMpetss} -trk ${PARMpetss}/trkfiles/tideOnly_14.trk -rex ${bsn}.${WNDS}${PEP}.${fle_ext}.etss2.0.rex -env ${bsn}.${fle_ext}.etss2.0.env -f_tide ${tide_v} -TideDatabase 2014 -spinUp 0 -rexSave 6 -nest 21 -verbose 1 >> $pgmout 2> errfile_${RANKO}
      err=$?;export err; err_chk
      echo "`date`: Finished Run for ${bsn} in tide ${tide_v}" >> timing.txt
   done

elif [[ ${f_gulf} == ht3 ]] ; then

   for bsn in ht3
   do
      export pgm="petss_model"
#      . prep_step
      echo e ${iopt} > fle20.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT19=fle20.${WNDS}${PEP}.${fle_ext}.${bsn}

      export FORT81=fle10.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT84=fle40.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT87=fle70.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT88=fle40.agl.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT96=sds.${WNDS}${PEP}.${cyc}

      export FORT11=${PARMpetssmodel}/mdl_ft11.${bsn}
      export FORT14=${PARMpetssmodel}/mdl_ettgp.${bsn}
      export FORT15=${PARMpetssmodel}/mdl_ettgp.${bsn}_2nd
      export FORT33=cylf10.${WNDS}${PEP}.${cyc}e
      export FORT34=gfspuv.${WNDS}${PEP}.${cyc}e
      export FORT52=sshistory.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
      export FORT55=sshistory.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}_2nd
      export FORT53=ssgrid.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
      export FORT54=ssgrid.agl.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
      export FORT50=wl.${WNDS}${PEP}.${fle_ext}.${bsn}

      case ${bsn} in

      ht3)
         export FORT20=out_nesting7.${WNDS}${PEP}.bin ;;
      esac

      echo "`date`: Starting Run for ${bsn} in tide ${tide_v}" >> timing.txt
      if [[ $pgmout == '' ]] ; then
         pgmout=OUTPUT.0000
      fi
      startmsg
      ${EXECpetss}/petss_model -basin ${bsn} -rootDir ${PARMpetss} -trk ${PARMpetss}/trkfiles/tideOnly_14.trk -rex ${bsn}.${WNDS}${PEP}.${fle_ext}.etss2.0.rex -env ${bsn}.${fle_ext}.etss2.0.env -f_tide ${tide_v} -TideDatabase 2014 -spinUp 0 -rexSave 6 -nest 21 -verbose 1 >> $pgmout 2> errfile_${RANKO}
      err=$?;export err; err_chk
      echo "`date`: Finished Run for ${bsn} in tide ${tide_v}" >> timing.txt
   done

elif [[ ${f_gulf} == ps2 ]] ; then
      bsn=ps2
      export pgm="petss_model"
#      . prep_step
      echo g ${iopt} > fle20.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT19=fle20.${WNDS}${PEP}.${fle_ext}.${bsn}

      export FORT81=fle10.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT84=fle40.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT87=fle70.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT88=fle40.agl.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT96=sds.${WNDS}${PEP}.${cyc}

      export FORT11=${PARMpetssmodel}/mdl_ft11.${bsn}
      export FORT14=${PARMpetssmodel}/mdl_ettgp.${bsn}
      export FORT15=${PARMpetssmodel}/mdl_ettgp.${bsn}_2nd
      export FORT33=cylf10.${WNDS}${PEP}.${cyc}g
      export FORT34=gfspuv.${WNDS}${PEP}.${cyc}g
      export FORT52=sshistory.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
      export FORT55=sshistory.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}_2nd
      export FORT53=ssgrid.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
      export FORT54=ssgrid.agl.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
      export FORT50=wl.${WNDS}${PEP}.${fle_ext}.${bsn}

      export FORT20=out_nesting27.${WNDS}${PEP}.bin
      echo "`date`: Starting Run for ${bsn} in tide ${tide_v}" >> timing.txt

      if [[ $pgmout == '' ]] ; then
         pgmout=OUTPUT.0000
      fi
      startmsg

      ${EXECpetss}/petss_model -basin ${bsn} -rootDir ${PARMpetss} -trk ${PARMpetss}/trkfiles/tideOnly_14.trk -rex ${bsn}.${WNDS}${PEP}.${fle_ext}.etss2.0.rex -env ${bsn}.${fle_ext}.etss2.0.env -f_tide ${tide_v} -TideDatabase 2014 -spinUp 0 -rexSave 6 -nest 21 -verbose 1 >> $pgmout 2> errfile_${RANKO}

      err=$?;export err; err_chk
      echo "`date`: Finished Run for ${bsn} in tide ${tide_v}" >> timing.txt


elif [[ ${f_gulf} == ap3 ]] ; then
      bsn=ap3
      export pgm="petss_model"
#      . prep_step
      echo g ${iopt} > fle20.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT19=fle20.${WNDS}${PEP}.${fle_ext}.${bsn}

      export FORT81=fle10.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT84=fle40.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT87=fle70.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT88=fle40.agl.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT96=sds.${WNDS}${PEP}.${cyc}

      export FORT11=${PARMpetssmodel}/mdl_ft11.${bsn}
      export FORT14=${PARMpetssmodel}/mdl_ettgp.${bsn}
      export FORT15=${PARMpetssmodel}/mdl_ettgp.${bsn}_2nd
      export FORT33=cylf10.${WNDS}${PEP}.${cyc}g
      export FORT34=gfspuv.${WNDS}${PEP}.${cyc}g
      export FORT52=sshistory.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
      export FORT55=sshistory.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}_2nd
      export FORT53=ssgrid.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
      export FORT54=ssgrid.agl.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
      export FORT50=wl.${WNDS}${PEP}.${fle_ext}.${bsn}

      export FORT20=out_nesting19.${WNDS}${PEP}.bin

      echo "`date`: Starting Run for ${bsn} in tide ${tide_v}" >> timing.txt
      if [[ $pgmout == '' ]] ; then
         pgmout=OUTPUT.0000
      fi
      startmsg

      ${EXECpetss}/petss_model -basin ${bsn} -rootDir ${PARMpetss} -trk ${PARMpetss}/trkfiles/tideOnly_14.trk -rex ${bsn}.${WNDS}${PEP}.${fle_ext}.etss2.0.rex -env ${bsn}.${fle_ext}.etss2.0.env -f_tide ${tide_v} -TideDatabase 2014 -spinUp 0 -rexSave 6 -nest 21 -verbose 1 >> $pgmout 2> errfile_${RANKO}

      err=$?;export err; err_chk
      echo "`date`: Finished Run for ${bsn} in tide ${tide_v}" >> timing.txt

elif [[ ${f_gulf} == g1 ]] ; then
  ########################################################################################
  #  Nesting run extratropical storm surge model for following tropical basins then exit.
  #  in which the nesting boundary are generated from egm3 (gulf of mexico basin) run
  ########################################################################################
   for bsn in efm2 etp3 cd2 emo2 ms7 lf2 ps2
   do
      export pgm="petss_model"
#      . prep_step
      echo g ${iopt} > fle20.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT19=fle20.${WNDS}${PEP}.${fle_ext}.${bsn}

      export FORT81=fle10.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT84=fle40.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT87=fle70.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT88=fle40.agl.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT96=sds.${WNDS}${PEP}.${cyc}

      export FORT11=${PARMpetssmodel}/mdl_ft11.${bsn}
      export FORT14=${PARMpetssmodel}/mdl_ettgp.${bsn}
      export FORT15=${PARMpetssmodel}/mdl_ettgp.${bsn}_2nd
      export FORT33=cylf10.${WNDS}${PEP}.${cyc}g
      export FORT34=gfspuv.${WNDS}${PEP}.${cyc}g
      export FORT52=sshistory.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
      export FORT55=sshistory.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}_2nd
      export FORT53=ssgrid.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
      export FORT54=ssgrid.agl.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
      export FORT50=wl.${WNDS}${PEP}.${fle_ext}.${bsn}

      case ${bsn} in
      
      efm2)
          export FORT20=out_nesting16.${WNDS}${PEP}.bin ;;
      etp3)
          export FORT20=out_nesting17.${WNDS}${PEP}.bin ;;
      cd2)
          export FORT20=out_nesting18.${WNDS}${PEP}.bin ;;
      emo2)
          export FORT20=out_nesting22.${WNDS}${PEP}.bin ;;
      ms7)
          export FORT20=out_nesting23.${WNDS}${PEP}.bin ;;
      lf2)
          export FORT20=out_nesting24.${WNDS}${PEP}.bin ;;
      eok3)
          export FORT20=out_nesting30.${WNDS}${PEP}.bin ;;
      ps2)
         export FORT20=out_nesting27.${WNDS}${PEP}.bin ;;
      esac

      echo "`date`: Starting Run for ${bsn} in tide ${tide_v}" >> timing.txt
      if [[ $pgmout == '' ]] ; then
         pgmout=OUTPUT.0000
      fi
      startmsg
      if [ ${bsn} == eok3 ] ; then

         ${EXECpetss}/petss_model -basin ${bsn} -rootDir ${PARMpetss} -trk ${PARMpetss}/trkfiles/tideOnly_14.trk -rex ${bsn}.${WNDS}${PEP}.${fle_ext}.etss2.0.rex -env ${bsn}.${fle_ext}.etss2.0.env -f_tide ${tide_v} -TideDatabase 2014 -spinUp 0 -rexSave 6 -nest 0 -verbose 1 >> $pgmout 2> errfile_${RANKO}
      else
         ${EXECpetss}/petss_model -basin ${bsn} -rootDir ${PARMpetss} -trk ${PARMpetss}/trkfiles/tideOnly_14.trk -rex ${bsn}.${WNDS}${PEP}.${fle_ext}.etss2.0.rex -env ${bsn}.${fle_ext}.etss2.0.env -f_tide ${tide_v} -TideDatabase 2014 -spinUp 0 -rexSave 6 -nest 21 -verbose 1 >> $pgmout 2> errfile_${RANKO}
      fi
      err=$?;export err; err_chk
      echo "`date`: Finished Run for ${bsn} in tide ${tide_v}" >> timing.txt
   done
elif [[ ${f_gulf} == g1a ]] ; then
  ########################################################################################
  #  Nesting run extratropical storm surge model for following tropical basins then exit.
  #  in which the nesting boundary are generated from egm3 (gulf of mexico basin) run
  ########################################################################################
   for bsn in etp3 lf2
   do
      export pgm="petss_model"
#      . prep_step 
      echo g ${iopt} > fle20.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT19=fle20.${WNDS}${PEP}.${fle_ext}.${bsn}

      export FORT81=fle10.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT84=fle40.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT87=fle70.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT88=fle40.agl.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT96=sds.${WNDS}${PEP}.${cyc}

      export FORT11=${PARMpetssmodel}/mdl_ft11.${bsn}
      export FORT14=${PARMpetssmodel}/mdl_ettgp.${bsn}
      export FORT15=${PARMpetssmodel}/mdl_ettgp.${bsn}_2nd
      export FORT33=cylf10.${WNDS}${PEP}.${cyc}g
      export FORT34=gfspuv.${WNDS}${PEP}.${cyc}g
      export FORT52=sshistory.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
      export FORT55=sshistory.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}_2nd
      export FORT53=ssgrid.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
      export FORT54=ssgrid.agl.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
      export FORT50=wl.${WNDS}${PEP}.${fle_ext}.${bsn}

      case ${bsn} in

      etp3)
          export FORT20=out_nesting17.${WNDS}${PEP}.bin ;;
      lf2)
          export FORT20=out_nesting24.${WNDS}${PEP}.bin ;;
      esac

      echo "`date`: Starting Run for ${bsn} in tide ${tide_v}" >> timing.txt
      if [[ $pgmout == '' ]] ; then
         pgmout=OUTPUT.0000
      fi
      startmsg
      ${EXECpetss}/petss_model -basin ${bsn} -rootDir ${PARMpetss} -trk ${PARMpetss}/trkfiles/tideOnly_14.trk -rex ${bsn}.${WNDS}${PEP}.${fle_ext}.etss2.0.rex -env ${bsn}.${fle_ext}.etss2.0.env -f_tide ${tide_v} -TideDatabase 2014 -spinUp 0 -rexSave 6 -nest 21 -verbose 1 >> $pgmout 2> errfile_${RANKO}
      err=$?;export err; err_chk
      echo "`date`: Finished Run for ${bsn} in tide ${tide_v}" >> timing.txt
   done

elif [[ ${f_gulf} == g1b ]] ; then
  ########################################################################################
  #  Nesting run extratropical storm surge model for following tropical basins then exit.
  #  in which the nesting boundary are generated from egm3 (gulf of mexico basin) run
  ########################################################################################
   for bsn in efm2 cd2 ms7
   do
      export pgm="petss_model"
#      . prep_step 
      echo g ${iopt} > fle20.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT19=fle20.${WNDS}${PEP}.${fle_ext}.${bsn}

      export FORT81=fle10.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT84=fle40.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT87=fle70.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT88=fle40.agl.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT96=sds.${WNDS}${PEP}.${cyc}

      export FORT11=${PARMpetssmodel}/mdl_ft11.${bsn}
      export FORT14=${PARMpetssmodel}/mdl_ettgp.${bsn}
      export FORT15=${PARMpetssmodel}/mdl_ettgp.${bsn}_2nd
      export FORT33=cylf10.${WNDS}${PEP}.${cyc}g
      export FORT34=gfspuv.${WNDS}${PEP}.${cyc}g
      export FORT52=sshistory.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
      export FORT55=sshistory.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}_2nd
      export FORT53=ssgrid.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
      export FORT54=ssgrid.agl.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
      export FORT50=wl.${WNDS}${PEP}.${fle_ext}.${bsn}

      case ${bsn} in

      efm2)
          export FORT20=out_nesting16.${WNDS}${PEP}.bin ;;
      cd2)
          export FORT20=out_nesting18.${WNDS}${PEP}.bin ;;
      ms7)
          export FORT20=out_nesting23.${WNDS}${PEP}.bin ;;
      esac
      echo "`date`: Starting Run for ${bsn} in tide ${tide_v}" >> timing.txt
      if [[ $pgmout == '' ]] ; then
         pgmout=OUTPUT.0000
      fi
      startmsg

      ${EXECpetss}/petss_model -basin ${bsn} -rootDir ${PARMpetss} -trk ${PARMpetss}/trkfiles/tideOnly_14.trk -rex ${bsn}.${WNDS}${PEP}.${fle_ext}.etss2.0.rex -env ${bsn}.${fle_ext}.etss2.0.env -f_tide ${tide_v} -TideDatabase 2014 -spinUp 0 -rexSave 6 -nest 21 -verbose 1 >> $pgmout 2> errfile_${RANKO}
      err=$?;export err; err_chk
      echo "`date`: Finished Run for ${bsn} in tide ${tide_v}" >> timing.txt
   done

elif [[ ${f_gulf} == g1c ]] ; then
  ########################################################################################
  #  Nesting run extratropical storm surge model for following tropical basins then exit.
  #  in which the nesting boundary are generated from egm3 (gulf of mexico basin) run
  ########################################################################################
   for bsn in ps2 ap3
   do
      export pgm="petss_model"
#      . prep_step
      echo g ${iopt} > fle20.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT19=fle20.${WNDS}${PEP}.${fle_ext}.${bsn}

      export FORT81=fle10.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT84=fle40.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT87=fle70.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT88=fle40.agl.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT96=sds.${WNDS}${PEP}.${cyc}

      export FORT11=${PARMpetssmodel}/mdl_ft11.${bsn}
      export FORT14=${PARMpetssmodel}/mdl_ettgp.${bsn}
      export FORT15=${PARMpetssmodel}/mdl_ettgp.${bsn}_2nd
      export FORT33=cylf10.${WNDS}${PEP}.${cyc}g
      export FORT34=gfspuv.${WNDS}${PEP}.${cyc}g
      export FORT52=sshistory.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
      export FORT55=sshistory.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}_2nd
      export FORT53=ssgrid.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
      export FORT54=ssgrid.agl.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
      export FORT50=wl.${WNDS}${PEP}.${fle_ext}.${bsn}

      case ${bsn} in

      ps2)
          export FORT20=out_nesting27.${WNDS}${PEP}.bin ;;
      ap3)
          export FORT20=out_nesting19.${WNDS}${PEP}.bin ;;
      esac
      echo "`date`: Starting Run for ${bsn} in tide ${tide_v}" >> timing.txt
      if [[ $pgmout == '' ]] ; then
         pgmout=OUTPUT.0000
      fi
      startmsg

      ${EXECpetss}/petss_model -basin ${bsn} -rootDir ${PARMpetss} -trk ${PARMpetss}/trkfiles/tideOnly_14.trk -rex ${bsn}.${WNDS}${PEP}.${fle_ext}.etss2.0.rex -env ${bsn}.${fle_ext}.etss2.0.env -f_tide ${tide_v} -TideDatabase 2014 -spinUp 0 -rexSave 6 -nest 21 -verbose 1 >> $pgmout 2> errfile_${RANKO}
      err=$?;export err; err_chk
      echo "`date`: Finished Run for ${bsn} in tide ${tide_v}" >> timing.txt
   done


elif [[ ${f_gulf} == hpa2 ]] ; then
  ########################################################################################
  #  Nesting run extratropical storm surge model for following tropical basins then exit.
  #  in which the nesting boundary are generated from egm3 (gulf of mexico basin) run
  ########################################################################################
      bsn=hpa2
      export pgm="petss_model"
#      . prep_step
      echo g ${iopt} > fle20.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT19=fle20.${WNDS}${PEP}.${fle_ext}.${bsn}

      export FORT81=fle10.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT84=fle40.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT87=fle70.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT88=fle40.agl.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT96=sds.${WNDS}${PEP}.${cyc}

      export FORT11=${PARMpetssmodel}/mdl_ft11.${bsn}
      export FORT14=${PARMpetssmodel}/mdl_ettgp.${bsn}
      export FORT15=${PARMpetssmodel}/mdl_ettgp.${bsn}_2nd
      export FORT33=cylf10.${WNDS}${PEP}.${cyc}g
      export FORT34=gfspuv.${WNDS}${PEP}.${cyc}g
      export FORT52=sshistory.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
      export FORT55=sshistory.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}_2nd
      export FORT53=ssgrid.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
      export FORT54=ssgrid.agl.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
      export FORT50=wl.${WNDS}${PEP}.${fle_ext}.${bsn}

      export FORT20=out_nesting20.${WNDS}${PEP}.bin

      echo "`date`: Starting Run for ${bsn} in tide ${tide_v}" >> timing.txt
      if [[ $pgmout == '' ]] ; then
         pgmout=OUTPUT.0000
      fi
      startmsg
      ${EXECpetss}/petss_model -basin ${bsn} -rootDir ${PARMpetss} -trk ${PARMpetss}/trkfiles/tideOnly_14.trk -rex ${bsn}.${WNDS}${PEP}.${fle_ext}.etss2.0.rex -env ${bsn}.${fle_ext}.etss2.0.env -f_tide ${tide_v} -TideDatabase 2014 -spinUp 0 -rexSave 6 -nest 21 -verbose 1 >> $pgmout 2> errfile_${RANKO}
      err=$?;export err; err_chk
      echo "`date`: Finished Run for ${bsn} in tide ${tide_v}" >> timing.txt

elif [[ ${f_gulf} == ny3 ]] ; then

      export pgm="petss_model"
#      . prep_step
      bsn=ny3
      echo e ${iopt} > fle20.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT19=fle20.${WNDS}${PEP}.${fle_ext}.${bsn}

      export FORT81=fle10.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT84=fle40.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT87=fle70.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT88=fle40.agl.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT96=sds.${WNDS}${PEP}.${cyc}

      export FORT11=$PARMpetssmodel/mdl_ft11.${bsn}
      export FORT14=$PARMpetssmodel/mdl_ettgp.${bsn}
      export FORT15=$PARMpetssmodel/mdl_ettgp.${bsn}_2nd
      export FORT33=cylf10.${WNDS}${PEP}.${cyc}e
      export FORT34=gfspuv.${WNDS}${PEP}.${cyc}e
      export FORT52=sshistory.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
      export FORT55=sshistory.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}_2nd
      export FORT53=ssgrid.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
      export FORT54=ssgrid.agl.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
      export FORT50=wl.${WNDS}${PEP}.${fle_ext}.${bsn}

      export FORT20=out_nesting3.${WNDS}${PEP}.bin

      echo "`date`: Starting Run for ${bsn} in tide ${tide_v}" >> timing.txt
      startmsg

      ${EXECpetss}/petss_model -basin ${bsn} -rootDir ${PARMpetss} -trk ${PARMpetss}/trkfiles/tideOnly_14.trk -rex ${bsn}.${WNDS}${PEP}.${fle_ext}.etss2.0.rex -env ${bsn}.${fle_ext}.etss2.0.env -f_tide ${tide_v} -TideDatabase 2014 -spinUp 0 -rexSave 6 -nest 21 -verbose 1 >> $pgmout 2> errfile_${RANKO}
      err=$?;export err; err_chk

      echo "`date`: Finished Run for ${bsn} in tide ${tide_v}" >> timing.txt

     
elif [[ ${f_gulf} == e1 ]] ; then

   for bsn in pn2 pv2 de3 eke2 hmi3 
   do
      export pgm="petss_model"
#      . prep_step
      if [[ ${bsn} == eke2 ]] ; then
         echo g ${iopt} > fle20.${WNDS}${PEP}.${fle_ext}.${bsn}
      else
         echo e ${iopt} > fle20.${WNDS}${PEP}.${fle_ext}.${bsn}
      fi
      export FORT19=fle20.${WNDS}${PEP}.${fle_ext}.${bsn}

      export FORT81=fle10.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT84=fle40.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT87=fle70.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT88=fle40.agl.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT96=sds.${WNDS}${PEP}.${cyc}

      export FORT11=$PARMpetssmodel/mdl_ft11.${bsn}
      export FORT14=$PARMpetssmodel/mdl_ettgp.${bsn}
      export FORT15=$PARMpetssmodel/mdl_ettgp.${bsn}_2nd
      if [[ ${bsn} == eke2 ]] ; then
         export FORT33=cylf10.${WNDS}${PEP}.${cyc}g
         export FORT34=gfspuv.${WNDS}${PEP}.${cyc}g
      else
         export FORT33=cylf10.${WNDS}${PEP}.${cyc}e
         export FORT34=gfspuv.${WNDS}${PEP}.${cyc}e
      fi
      export FORT52=sshistory.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
      export FORT55=sshistory.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}_2nd
      export FORT53=ssgrid.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
      export FORT54=ssgrid.agl.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
      export FORT50=wl.${WNDS}${PEP}.${fle_ext}.${bsn}

      echo "`date`: Starting Run for ${bsn} in tide ${tide_v}" >> timing.txt
      case ${bsn} in

      pn2)
         export FORT20=out_nesting1.${WNDS}${PEP}.bin ;;
      pv2)
         export FORT20=out_nesting2.${WNDS}${PEP}.bin ;;
      de3)
         export FORT20=out_nesting4.${WNDS}${PEP}.bin ;;
      eke2)
         export FORT20=out_nesting15.${WNDS}${PEP}.bin ;;
      hmi3)
         export FORT20=out_nesting14.${WNDS}${PEP}.bin ;;
      esac 
      startmsg
      if [[ ${bsn} == pn2 ]] && [[ ${tide_v} == V3 ]] ; then

         ${EXECpetss}/petss_model -basin ${bsn} -rootDir ${PARMpetss} -trk ${PARMpetss}/trkfiles/tideOnly_14.trk -rex ${bsn}.${WNDS}${PEP}.${fle_ext}.etss2.0.rex -env ${bsn}.${fle_ext}.etss2.0.env -f_tide VDEF -TideDatabase 2014 -spinUp 0 -rexSave 6 -nest 21 -verbose 1 >> $pgmout 2> errfile_${RANKO}
      err=$?;export err; err_chk
      else

        ${EXECpetss}/petss_model -basin ${bsn} -rootDir ${PARMpetss} -trk ${PARMpetss}/trkfiles/tideOnly_14.trk -rex ${bsn}.${WNDS}${PEP}.${fle_ext}.etss2.0.rex -env ${bsn}.${fle_ext}.etss2.0.env -f_tide ${tide_v} -TideDatabase 2014 -spinUp 0 -rexSave 6 -nest 21 -verbose 1 >> $pgmout 2> errfile_${RANKO}

      fi
      err=$?;export err; err_chk
      echo "`date`: Finished Run for ${bsn} in tide ${tide_v}" >> timing.txt


   done

  ####################################################################################
  #  Nesting run extratropical storm surge model in the following tropical basins,
  #  in which nesting boundary conditions are generated from eex2 (East Coastal) basin
  ####################################################################################
elif [[ ${f_gulf} == egl3 ]] ; then

      bsn=egl3
      export pgm="petss_model"
#      . prep_step
      echo g ${iopt} > fle20.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT19=fle20.${WNDS}${PEP}.${fle_ext}.${bsn}

      export FORT81=fle10.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT84=fle40.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT87=fle70.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT88=fle40.agl.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT96=sds.${WNDS}${PEP}.${cyc}

      export FORT11=$PARMpetssmodel/mdl_ft11.${bsn}
      export FORT14=$PARMpetssmodel/mdl_ettgp.${bsn}
      export FORT15=$PARMpetssmodel/mdl_ettgp.${bsn}_2nd
      export FORT33=cylf10.${WNDS}${PEP}.${cyc}g
      export FORT34=gfspuv.${WNDS}${PEP}.${cyc}g
      export FORT52=sshistory.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
      export FORT55=sshistory.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}_2nd
      export FORT53=ssgrid.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
      export FORT54=ssgrid.agl.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
      export FORT50=wl.${WNDS}${PEP}.${fle_ext}.${bsn}

      echo "`date`: Starting Run for ${bsn} in tide ${tide_v}" >> timing.txt

      export FORT20=out_nesting26.${WNDS}${PEP}.bin

      startmsg

      ${EXECpetss}/petss_model -basin ${bsn} -rootDir ${PARMpetss} -trk ${PARMpetss}/trkfiles/tideOnly_14.trk -rex ${bsn}.${WNDS}${PEP}.${fle_ext}.etss2.0.rex -env ${bsn}.${fle_ext}.etss2.0.env -f_tide ${tide_v} -TideDatabase 2014 -spinUp 0 -rexSave 6 -nest 21 -verbose 1 >> $pgmout 2> errfile_${RANKO}
      err=$?;export err; err_chk
      echo "`date`: Finished Run for ${bsn} in tide ${tide_v}" >> timing.txt

elif [[ ${f_gulf} == e2a ]] ; then

   for bsn in hch2 ebr3
   do
      export pgm="petss_model"
#      . prep_step
      if [[ ${bsn} == hch2 ]] ; then
         echo e ${iopt} > fle20.${WNDS}${PEP}.${fle_ext}.${bsn}
      else
         echo g ${iopt} > fle20.${WNDS}${PEP}.${fle_ext}.${bsn}
      fi
      export FORT19=fle20.${WNDS}${PEP}.${fle_ext}.${bsn}

      export FORT81=fle10.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT84=fle40.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT87=fle70.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT88=fle40.agl.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT96=sds.${WNDS}${PEP}.${cyc}

      export FORT11=$PARMpetssmodel/mdl_ft11.${bsn}
      export FORT14=$PARMpetssmodel/mdl_ettgp.${bsn}
      export FORT15=$PARMpetssmodel/mdl_ettgp.${bsn}_2nd
      if [[ ${bsn} == hch2 ]] ; then
         export FORT33=cylf10.${WNDS}${PEP}.${cyc}e
         export FORT34=gfspuv.${WNDS}${PEP}.${cyc}e
      else
         export FORT33=cylf10.${WNDS}${PEP}.${cyc}g
         export FORT34=gfspuv.${WNDS}${PEP}.${cyc}g
      fi
      export FORT52=sshistory.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
      export FORT55=sshistory.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}_2nd
      export FORT53=ssgrid.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
      export FORT54=ssgrid.agl.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
      export FORT50=wl.${WNDS}${PEP}.${fle_ext}.${bsn}

      echo "`date`: Starting Run for ${bsn} in tide ${tide_v}" >> timing.txt
      case ${bsn} in

      hch2)
         export FORT20=out_nesting9.${WNDS}${PEP}.bin ;;
      ebr3)
         export FORT20=out_nesting29.${WNDS}${PEP}.bin ;;
      esac

      startmsg

      ${EXECpetss}/petss_model -basin ${bsn} -rootDir ${PARMpetss} -trk ${PARMpetss}/trkfiles/tideOnly_14.trk -rex ${bsn}.${WNDS}${PEP}.${fle_ext}.etss2.0.rex -env ${bsn}.${fle_ext}.etss2.0.env -f_tide ${tide_v} -TideDatabase 2014 -spinUp 0 -rexSave 6 -nest 21 -verbose 1 >> $pgmout 2> errfile_${RANKO}
      err=$?;export err; err_chk
      echo "`date`: Finished Run for ${bsn} in tide ${tide_v}" >> timing.txt
   done      #  Basin loop

elif [[ ${f_gulf} == e2b ]] ; then

   for bsn in il3 esv4 co2 pb3 cr3
   do
      export pgm="petss_model"
#      . prep_step
      if [[ ${bsn} == cr3 ]] ; then
         echo g ${iopt} > fle20.${WNDS}${PEP}.${fle_ext}.${bsn}
      else
         echo e ${iopt} > fle20.${WNDS}${PEP}.${fle_ext}.${bsn}
      fi
      export FORT19=fle20.${WNDS}${PEP}.${fle_ext}.${bsn}

      export FORT81=fle10.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT84=fle40.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT87=fle70.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT88=fle40.agl.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT96=sds.${WNDS}${PEP}.${cyc}

      export FORT11=$PARMpetssmodel/mdl_ft11.${bsn}
      export FORT14=$PARMpetssmodel/mdl_ettgp.${bsn}
      export FORT15=$PARMpetssmodel/mdl_ettgp.${bsn}_2nd
      if [[ ${bsn} == cr3 ]] ; then
         export FORT33=cylf10.${WNDS}${PEP}.${cyc}g
         export FORT34=gfspuv.${WNDS}${PEP}.${cyc}g
      else
         export FORT33=cylf10.${WNDS}${PEP}.${cyc}e
         export FORT34=gfspuv.${WNDS}${PEP}.${cyc}e
      fi
      export FORT52=sshistory.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
      export FORT55=sshistory.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}_2nd
      export FORT53=ssgrid.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
      export FORT54=ssgrid.agl.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
      export FORT50=wl.${WNDS}${PEP}.${fle_ext}.${bsn}

      echo "`date`: Starting Run for ${bsn} in tide ${tide_v}" >> timing.txt
      case ${bsn} in

      il3)
         export FORT20=out_nesting8.${WNDS}${PEP}.bin ;;
      esv4)
         export FORT20=out_nesting10.${WNDS}${PEP}.bin ;;
      co2)
         export FORT20=out_nesting12.${WNDS}${PEP}.bin ;;
      pb3)
         export FORT20=out_nesting13.${WNDS}${PEP}.bin ;;
      cr3)
         export FORT20=out_nesting28.${WNDS}${PEP}.bin ;;
      esac

      startmsg

      ${EXECpetss}/petss_model -basin ${bsn} -rootDir ${PARMpetss} -trk ${PARMpetss}/trkfiles/tideOnly_14.trk -rex ${bsn}.${WNDS}${PEP}.${fle_ext}.etss2.0.rex -env ${bsn}.${fle_ext}.etss2.0.env -f_tide ${tide_v} -TideDatabase 2014 -spinUp 0 -rexSave 6 -nest 21 -verbose 1 >> $pgmout 2> errfile_${RANKO}
      err=$?;export err; err_chk
      echo "`date`: Finished Run for ${bsn} in tide ${tide_v}" >> timing.txt
   done      #  Basin loop

elif [[ ${f_gulf} == e2 ]] ; then

   for bsn in il3 hch2 esv4 co2 pb3 ebr3 cr3
   do
      export pgm="petss_model"
#      . prep_step
      if [[ ${bsn} == ebr3 || ${bsn} == cr3 ]] ; then
         echo g ${iopt} > fle20.${WNDS}${PEP}.${fle_ext}.${bsn}
      else
         echo e ${iopt} > fle20.${WNDS}${PEP}.${fle_ext}.${bsn}
      fi
      export FORT19=fle20.${WNDS}${PEP}.${fle_ext}.${bsn}
  
      export FORT81=fle10.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT84=fle40.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT87=fle70.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT88=fle40.agl.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT96=sds.${WNDS}${PEP}.${cyc}

      export FORT11=$PARMpetssmodel/mdl_ft11.${bsn}
      export FORT14=$PARMpetssmodel/mdl_ettgp.${bsn}
      export FORT15=$PARMpetssmodel/mdl_ettgp.${bsn}_2nd
      if [[ ${bsn} == ebr3 || ${bsn} == cr3 ]] ; then
         export FORT33=cylf10.${WNDS}${PEP}.${cyc}g
         export FORT34=gfspuv.${WNDS}${PEP}.${cyc}g
      else
         export FORT33=cylf10.${WNDS}${PEP}.${cyc}e
         export FORT34=gfspuv.${WNDS}${PEP}.${cyc}e
      fi
      export FORT52=sshistory.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
      export FORT55=sshistory.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}_2nd
      export FORT53=ssgrid.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
      export FORT54=ssgrid.agl.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
      export FORT50=wl.${WNDS}${PEP}.${fle_ext}.${bsn}

      echo "`date`: Starting Run for ${bsn} in tide ${tide_v}" >> timing.txt
      case ${bsn} in

      il3)
         export FORT20=out_nesting8.${WNDS}${PEP}.bin ;;
      hch2)
         export FORT20=out_nesting9.${WNDS}${PEP}.bin ;;
      esv4)
         export FORT20=out_nesting10.${WNDS}${PEP}.bin ;;
      co2)
         export FORT20=out_nesting12.${WNDS}${PEP}.bin ;;
      pb3)
         export FORT20=out_nesting13.${WNDS}${PEP}.bin ;;
      ebr3)
         export FORT20=out_nesting29.${WNDS}${PEP}.bin ;;
      cr3)
         export FORT20=out_nesting28.${WNDS}${PEP}.bin ;;
      esac

      startmsg

      ${EXECpetss}/petss_model -basin ${bsn} -rootDir ${PARMpetss} -trk ${PARMpetss}/trkfiles/tideOnly_14.trk -rex ${bsn}.${WNDS}${PEP}.${fle_ext}.etss2.0.rex -env ${bsn}.${fle_ext}.etss2.0.env -f_tide ${tide_v} -TideDatabase 2014 -spinUp 0 -rexSave 6 -nest 21 -verbose 1 >> $pgmout 2> errfile_${RANKO}
      err=$?;export err; err_chk
      echo "`date`: Finished Run for ${bsn} in tide ${tide_v}" >> timing.txt
   done      #  Basin loop
fi
