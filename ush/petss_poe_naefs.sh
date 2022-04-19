#!/bin/bash
#
# 
# Huiqing.Liu/MDL--- 03/08/2016 -------PETSS model run script ---------
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
  echo "Usage $0 <'g' or 'other' for all other basins> <cycle> <PARMpetss> <EXECpetss><Tide> <GEC/GEP><00,01..>"
  echo "Example: g 00 $root/parm $root/exec V1 GEC 00" 
  exit
fi
if test "$7" == ''
then
  echo "Usage $0 <'g' or 'other' for all other basins> <cycle> <PARMpetss> <EXECpetss><Tide> <GEC/GEP><00,01..>"
  echo "Example: g 00 $root/parm $root/exec V1 GEC 00" 
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
elif [[ ${tide_v} == T1 ]] ; then
   fle_ext="tide"
else
   fle_ext="surge_tide"
fi

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

if [[ ${tide_v} == T1 ]] ; then
   export iopt=0
fi

if [[ ${f_gulf} == m ]] ; then

   bsn=m
   export pgm="petss_model_13consti"
   echo ${bsn} ${iopt} > fle20.${WNDS}${PEP}.${fle_ext}.${bsn}
   export FORT19=fle20.${WNDS}${PEP}.${fle_ext}.${bsn}
   export FORT81=fle10.${WNDS}${PEP}.${fle_ext}.${bsn}
   export FORT84=fle40.${WNDS}${PEP}.${fle_ext}.${bsn}
   export FORT87=fle70.${WNDS}${PEP}.${fle_ext}.${bsn}
   export FORT88=fle40.agl.${WNDS}${PEP}.${fle_ext}.${bsn}
   export FORT96=sds.${WNDS}${PEP}.${cyc}

   export FORT11=${PARMpetssmodel}/mdl_ft11.${bsn}
   export FORT14=${PARMpetssmodel}/mdl_ettgp.${bsn}
   export FORT15=${PARMpetssmodel}/mdl_ettgp.${bsn}_2nd
   export FORT33=cylf10.${WNDS}${PEP}.${cyc}${bsn}
   export FORT34=gfspuv.${WNDS}${PEP}.${cyc}${bsn}
   export FORT52=sshistory.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
   export FORT53=ssgrid.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
   export FORT54=ssgrid.agl.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
   export FORT55=sshistory.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}_2nd
   export FORT50=wl.${WNDS}${PEP}.${fle_ext}.${bsn}

   echo "`date`: Starting Run for ${bsn}" >> timing.txt
   startmsg
   if [[ $pgmout == '' ]] ; then
      pgmout=OUTPUT.0000
   fi
   ${EXECpetss}/petss_model_13consti -basin ebbc -rootDir ${PARMpetss} -trk ${PARMpetss}/trkfiles/tideOnly_14.trk -rex ebbc.${WNDS}${PEP}.${fle_ext}.etss2.0.rex -env ebbc.${WNDS}${PEP}.${fle_ext}.etss2.0.env -f_tide ${tide_v} -TideDatabase 2014 -spinUp 0 -rexSave 6 -nest 0 -verbose 1 >> $pgmout 2> errfile_${RANKO}
   err=$?;export err; err_chk
   echo "`date`: Finished Run for ${bsn}" >> timing.txt

elif [[ ${f_gulf} == g ]] ; then

  ####################################################################
  #  Run extratropical storm surge model for gulf of mexico then exit.
  ####################################################################
   export iopt=0
   bsn='g'
   bsnss="eglc"
   export pgm="petss_model"
   echo ${bsn} ${iopt} > fle20.${WNDS}${PEP}.${fle_ext}.${bsn}
   export FORT19=fle20.${WNDS}${PEP}.${fle_ext}.${bsn}

   export FORT81=fle10.${WNDS}${PEP}.${fle_ext}.${bsn}
   export FORT84=fle40.${WNDS}${PEP}.${fle_ext}.${bsn}
   export FORT87=fle70.${WNDS}${PEP}.${fle_ext}.${bsn}
   export FORT88=fle40.agl.${WNDS}${PEP}.${fle_ext}.${bsn}
   export FORT96=sds.${WNDS}${PEP}.${cyc}

   export FORT11=$PARMpetssmodel/mdl_ft11.${bsn}
   export FORT14=$PARMpetssmodel/mdl_ettgp.${bsn}
   export FORT15=${PARMpetssmodel}/mdl_ettgp.${bsn}_2nd
   export FORT33=cylf10.${WNDS}${PEP}.${cyc}${bsn}
   export FORT34=gfspuv.${WNDS}${PEP}.${cyc}${bsn}
   export FORT52=sshistory.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
   export FORT55=sshistory.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}_2nd
   export FORT53=ssgrid.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
   export FORT54=ssgrid.agl.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
   export FORT50=wl.${WNDS}${PEP}.${fle_ext}.${bsn}
  
   if [[ ${tide_v} == 0 ]] ; then

      export FORT35=$PARMpetss/nesting/outer_ij_inner_boun_slosh16.txt
      export FORT36=$PARMpetss/nesting/outer_ij_inner_boun_slosh17.txt
      export FORT37=$PARMpetss/nesting/outer_ij_inner_boun_slosh18.txt
      export FORT38=$PARMpetss/nesting/outer_ij_inner_boun_slosh19.txt
      export FORT39=$PARMpetss/nesting/outer_ij_inner_boun_slosh20.txt
      export FORT40=$PARMpetss/nesting/outer_ij_inner_boun_slosh21.txt
      export FORT41=$PARMpetss/nesting/outer_ij_inner_boun_slosh22.txt
      export FORT42=$PARMpetss/nesting/outer_ij_inner_boun_slosh23.txt
      export FORT43=$PARMpetss/nesting/outer_ij_inner_boun_slosh24.txt
      export FORT44=$PARMpetss/nesting/outer_ij_inner_boun_slosh25.txt
      export FORT45=$PARMpetss/nesting/outer_ij_inner_boun_slosh26.txt
      export FORT46=$PARMpetss/nesting/outer_ij_inner_boun_slosh27.txt
      export FORT47=$PARMpetss/nesting/outer_ij_inner_boun_slosh28.txt
      export FORT48=$PARMpetss/nesting/outer_ij_inner_boun_slosh29.txt
      export FORT49=$PARMpetss/nesting/outer_ij_inner_boun_slosh15.txt

      export FORT61=out_nesting16.${WNDS}${PEP}.bin
      export FORT62=out_nesting17.${WNDS}${PEP}.bin
      export FORT63=out_nesting18.${WNDS}${PEP}.bin
      export FORT64=out_nesting19.${WNDS}${PEP}.bin
      export FORT65=out_nesting20.${WNDS}${PEP}.bin
      export FORT66=out_nesting21.${WNDS}${PEP}.bin
      export FORT67=out_nesting22.${WNDS}${PEP}.bin
      export FORT68=out_nesting23.${WNDS}${PEP}.bin
      export FORT69=out_nesting24.${WNDS}${PEP}.bin
      export FORT70=out_nesting25.${WNDS}${PEP}.bin
      export FORT71=out_nesting26.${WNDS}${PEP}.bin
      export FORT72=out_nesting27.${WNDS}${PEP}.bin
      export FORT73=out_nesting28.${WNDS}${PEP}.bin
      export FORT74=out_nesting29.${WNDS}${PEP}.bin
      export FORT75=out_nesting30.${WNDS}${PEP}.bin

   fi

   startmsg
   if [[ $pgmout == '' ]] ; then
     pgmout=OUTPUT.0000
   fi
  
   if [[ ${tide_v} == 0 ]] ; then
     echo "`date`: Starting Run for ${bsn} in surge only" >> timing.txt
     $EXECpetss/petss_model -basin ${bsnss} -rootDir $PARMpetss -trk $PARMpetss/trkfiles/tideOnly_14.trk -rex egm3.${WNDS}${PEP}.${fle_ext}.etss2.0.rex -env egm3.${WNDS}${PEP}.${fle_ext}.etss2.0.env -f_tide ${tide_v} -TideDatabase 2014 -spinUp 0 -rexSave 6 -nest 1 -verbose 1 >> $pgmout 2> errfile_${RANKO}
     echo "`date`: Finished Run for ${bsn} in surge only" >> timing.txt
   else
     echo "`date`: Starting Run for ${bsn} in tide only" >> timing.txt
     $EXECpetss/petss_model -basin ${bsnss} -rootDir $PARMpetss -trk $PARMpetss/trkfiles/tideOnly_14.trk -rex egm3.${WNDS}${PEP}.${fle_ext}.etss2.0.rex -env egm3.${WNDS}${PEP}.${fle_ext}.etss2.0.env -f_tide ${tide_v} -TideDatabase 2014 -spinUp 0 -rexSave 6 -nest 0 -verbose 1 >> $pgmout 2> errfile_${RANKO}
     echo "`date`: Finished Run for ${bsn} in tide only" >> timing.txt
   fi

   err=$?;export err; err_chk

elif [[ ${f_gulf} == e ]] ; then
   export iopt=0
   bsn=e
   export pgm="petss_model"
   echo ${bsn} ${iopt} > fle20.${WNDS}${PEP}.${fle_ext}.${bsn}
   export FORT19=fle20.${WNDS}${PEP}.${fle_ext}.${bsn}

   export FORT81=fle10.${WNDS}${PEP}.${fle_ext}.${bsn}
   export FORT84=fle40.${WNDS}${PEP}.${fle_ext}.${bsn}
   export FORT87=fle70.${WNDS}${PEP}.${fle_ext}.${bsn}
   export FORT88=fle40.agl.${WNDS}${PEP}.${fle_ext}.${bsn}
   export FORT96=sds.${WNDS}${PEP}.${cyc}

   export FORT11=$PARMpetssmodel/mdl_ft11.${bsn}
   export FORT14=$PARMpetssmodel/mdl_ettgp.${bsn}
   export FORT15=$PARMpetssmodel/mdl_ettgp.${bsn}_2nd
   export FORT33=cylf10.${WNDS}${PEP}.${cyc}${bsn}
   export FORT34=gfspuv.${WNDS}${PEP}.${cyc}${bsn}
   export FORT52=sshistory.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
   export FORT55=sshistory.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}_2nd
   export FORT53=ssgrid.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
   export FORT54=ssgrid.agl.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
   export FORT50=wl.${WNDS}${PEP}.${fle_ext}.${bsn}


   bsnss="exm"

   if [[ ${tide_v} == 0 ]] ; then

  export FORT35=$PARMpetss/nesting/outer_ij_inner_boun_slosh1.txt
  export FORT36=$PARMpetss/nesting/outer_ij_inner_boun_slosh2.txt
  export FORT37=$PARMpetss/nesting/outer_ij_inner_boun_slosh3.txt
  export FORT38=$PARMpetss/nesting/outer_ij_inner_boun_slosh4.txt
  export FORT39=$PARMpetss/nesting/outer_ij_inner_boun_slosh5.txt
  export FORT40=$PARMpetss/nesting/outer_ij_inner_boun_slosh6.txt
  export FORT41=$PARMpetss/nesting/outer_ij_inner_boun_slosh7.txt
  export FORT42=$PARMpetss/nesting/outer_ij_inner_boun_slosh8.txt
  export FORT43=$PARMpetss/nesting/outer_ij_inner_boun_slosh9.txt
  export FORT44=$PARMpetss/nesting/outer_ij_inner_boun_slosh10.txt
  export FORT45=$PARMpetss/nesting/outer_ij_inner_boun_slosh11.txt
  export FORT46=$PARMpetss/nesting/outer_ij_inner_boun_slosh12.txt
  export FORT47=$PARMpetss/nesting/outer_ij_inner_boun_slosh13.txt
  export FORT48=$PARMpetss/nesting/outer_ij_inner_boun_slosh14.txt
  export FORT49=$PARMpetss/nesting/outer_ij_inner_boun_slosh15.txt

  export FORT61=out_nesting1.${WNDS}${PEP}.bin
  export FORT62=out_nesting2.${WNDS}${PEP}.bin
  export FORT63=out_nesting3.${WNDS}${PEP}.bin
  export FORT64=out_nesting4.${WNDS}${PEP}.bin
  export FORT65=out_nesting5.${WNDS}${PEP}.bin
  export FORT66=out_nesting6.${WNDS}${PEP}.bin
  export FORT67=out_nesting7.${WNDS}${PEP}.bin
  export FORT68=out_nesting8.${WNDS}${PEP}.bin
  export FORT69=out_nesting9.${WNDS}${PEP}.bin
  export FORT70=out_nesting10.${WNDS}${PEP}.bin
  export FORT71=out_nesting11.${WNDS}${PEP}.bin
  export FORT72=out_nesting12.${WNDS}${PEP}.bin
  export FORT73=out_nesting13.${WNDS}${PEP}.bin
  export FORT74=out_nesting14.${WNDS}${PEP}.bin
  export FORT75=out_nesting15.${WNDS}${PEP}.bin


   fi

   startmsg
   if [[ $pgmout == '' ]] ; then
     pgmout=OUTPUT.0000
   fi
   
   if [[ ${tide_v} == 0 ]] ; then
      echo "`date`: Starting Run for ${bsn} surge only" >> timing.txt
      $EXECpetss/petss_model -basin $bsnss -rootDir $PARMpetss -trk $PARMpetss/trkfiles/tideOnly_14.trk -rex $bsnss.${WNDS}${PEP}.${fle_ext}.etss2.0.rex -env $bsnss.${WNDS}${PEP}.${fle_ext}.etss2.0.env -f_tide ${tide_v} -TideDatabase 2014 -spinUp 0 -rexSave 6 -nest 1 -verbose 1 >> $pgmout 2> errfile_${RANKO}
      echo "`date`: Finished Run for ${bsn} surge only" >> timing.txt
   else
      echo "`date`: Starting Run for ${bsn} tide only" >> timing.txt
      $EXECpetss/petss_model -basin $bsnss -rootDir $PARMpetss -trk $PARMpetss/trkfiles/tideOnly_14.trk -rex $bsnss.${WNDS}${PEP}.${fle_ext}.etss2.0.rex -env $bsnss.${WNDS}${PEP}.${fle_ext}.etss2.0.env -f_tide ${tide_v} -TideDatabase 2014 -spinUp 0 -rexSave 6 -nest 0 -verbose 1 >> $pgmout 2> errfile_${RANKO}
      echo "`date`: Finished Run for ${bsn} tide only" >> timing.txt
   fi

   err=$?;export err; err_chk
   echo "`date`: Finished Run for ${bsn}" >> timing.txt
  
elif [[ ${f_gulf} == nep ]] ; then
  
   bsn=n
   bsnss=nep
   export pgm="petss_model_13consti"
   echo ${bsn} ${iopt} > fle20.${WNDS}${PEP}.${fle_ext}.${bsn}
   export FORT19=fle20.${WNDS}${PEP}.${fle_ext}.${bsn}

   export FORT81=fle10.${WNDS}${PEP}.${fle_ext}.${bsn}
   export FORT84=fle40.${WNDS}${PEP}.${fle_ext}.${bsn}
   export FORT87=fle70.${WNDS}${PEP}.${fle_ext}.${bsn}
   export FORT88=fle40.agl.${WNDS}${PEP}.${fle_ext}.${bsn}
   export FORT96=sds.${WNDS}${PEP}.${cyc}

   export FORT11=$PARMpetssmodel/mdl_ft11.${bsn}
   export FORT14=$PARMpetssmodel/mdl_ettgp.${bsn}
   export FORT15=$PARMpetssmodel/mdl_ettgp.${bsn}_2nd
   export FORT33=cylf10.${WNDS}${PEP}.${cyc}${bsn}
   export FORT34=gfspuv.${WNDS}${PEP}.${cyc}${bsn}
   export FORT52=sshistory.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
   export FORT53=ssgrid.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
   export FORT54=ssgrid.agl.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
   export FORT55=sshistory.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}_2nd
   export FORT50=wl.${WNDS}${PEP}.${fle_ext}.${bsn}

   echo "`date`: Starting Run for ${bsn} tide mode ${tide_v}" >> timing.txt
   startmsg
   if [[ $pgmout == '' ]] ; then
     pgmout=OUTPUT.0000
   fi

   $EXECpetss/petss_model_13consti -basin $bsnss -rootDir $PARMpetss -trk $PARMpetss/trkfiles/tideOnly_14.trk -rex $bsnss.${WNDS}${PEP}.${fle_ext}.etss2.0.rex -env $bsnss.${WNDS}${PEP}.${fle_ext}.etss2.0.env -f_tide ${tide_v} -TideDatabase 2014 -spinUp 0 -rexSave 6 -nest 0 -verbose 1 >> $pgmout 2> errfile_${RANKO}
   err=$?;export err; err_chk

   echo "`date`: Finished Run for ${bsn} tide mode ${tide_v}" >> timing.txt
elif [[ ${f_gulf} == ala ]] ; then
   for bsn in k
   do
  ####################################################################
  #  Run extratropical storm surge model.
  ####################################################################
      export pgm="petss_model_13consti"
      echo ${bsn} ${iopt} > fle20.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT19=fle20.${WNDS}${PEP}.${fle_ext}.${bsn}
  
      export FORT81=fle10.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT84=fle40.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT87=fle70.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT88=fle40.agl.${WNDS}${PEP}.${fle_ext}.${bsn}
      export FORT96=sds.${WNDS}${PEP}.${cyc}

      export FORT11=$PARMpetssmodel/mdl_ft11.${bsn}
      export FORT14=$PARMpetssmodel/mdl_ettgp.${bsn}
      export FORT15=$PARMpetssmodel/mdl_ettgp.${bsn}_2nd
      export FORT33=cylf10.${WNDS}${PEP}.${cyc}${bsn}
      export FORT34=gfspuv.${WNDS}${PEP}.${cyc}${bsn}
      export FORT52=sshistory.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
      export FORT55=sshistory.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}_2nd
      export FORT53=ssgrid.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
      export FORT54=ssgrid.agl.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
      export FORT50=wl.${WNDS}${PEP}.${fle_ext}.${bsn}

      if test "$bsn" = 'a'
      then
         bsnss="enom"
      fi
      if test "$bsn" = 'z'
      then
         bsnss="eotz"
      fi
      if test "$bsn" = 'k'
      then
         bsnss="egoa"
      fi

      echo "`date`: Starting Run for ${bsn} in tide ${tide_v}" >> timing.txt

      startmsg
      if [[ $pgmout == '' ]] ; then
         pgmout=OUTPUT.0000
      fi

      $EXECpetss/petss_model_13consti -basin $bsnss -rootDir $PARMpetss -trk $PARMpetss/trkfiles/tideOnly_14.trk -rex $bsnss.${WNDS}${PEP}.${fle_ext}.etss2.0.rex -env $bsnss.${WNDS}${PEP}.${fle_ext}.etss2.0.env -f_tide ${tide_v} -TideDatabase 2014 -spinUp 0 -rexSave 6 -nest 0 -verbose 1 >> $pgmout 2> errfile_${RANKO}
      err=$?;export err; err_chk

      if [[ ${bsn} == k ]] ; then
         
         tide_v="V2.2.10"
         fle_ext="surge_tide"
         export pgm="petss_model_13consti"

        export FORT81=fle10.${WNDS}${PEP}.${fle_ext}.${bsn}
        export FORT84=fle40.${WNDS}${PEP}.${fle_ext}.${bsn}
        export FORT87=fle70.${WNDS}${PEP}.${fle_ext}.${bsn}
        export FORT88=fle40.agl.${WNDS}${PEP}.${fle_ext}.${bsn}
        export FORT96=sds.${WNDS}${PEP}.${cyc}

        export FORT11=$PARMpetssmodel/mdl_ft11.${bsn}
        export FORT14=$PARMpetssmodel/mdl_ettgp.${bsn}
        export FORT15=$PARMpetssmodel/mdl_ettgp.${bsn}_2nd
        export FORT33=cylf10.${WNDS}${PEP}.${cyc}${bsn}
        export FORT34=gfspuv.${WNDS}${PEP}.${cyc}${bsn}
        export FORT52=sshistory.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
        export FORT55=sshistory.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}_2nd
        export FORT53=ssgrid.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
        export FORT54=ssgrid.agl.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
        export FORT50=wl.${WNDS}${PEP}.${fle_ext}.${bsn}

        $EXECpetss/petss_model_13consti -basin $bsnss -rootDir $PARMpetss -trk $PARMpetss/trkfiles/tideOnly_14.trk -rex $bsnss.${WNDS}${PEP}.${fle_ext}.etss2.0.rex -env $bsnss.${WNDS}${PEP}.${fle_ext}.etss2.0.env -f_tide ${tide_v} -TideDatabase 2014 -spinUp 0 -rexSave 6 -nest 0 -verbose 1 >> $pgmout 2> errfile_${RANKO}          
      err=$?;export err; err_chk

      echo "`date`: Finished Run for ${bsn} in tide ${tide_v}" >> timing.txt


       fi

      echo "`date`: Finished Run for ${bsn} in tide ${tide_v}" >> timing.txt
   done      #  Basin loop
  #########################################################
  # The following for Tide only run in 30 Tropical basins #
  #########################################################
elif [[ ${f_gulf} == cp5 ]] ; then

   for bsn in cp5 hor3
   do

      export pgm="petss_model"
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


      if [[ $pgmout == '' ]] ; then
         pgmout=OUTPUT.0000
      fi

      echo "`date`: Starting Run for ${bsn} in tide ${tide_v}" >> timing.txt

      startmsg
         ${EXECpetss}/petss_model -basin ${bsn} -rootDir ${PARMpetss} -trk ${PARMpetss}/trkfiles/tideOnly_14.trk -rex ${bsn}.${WNDS}${PEP}.${fle_ext}.etss2.0.rex -env ${bsn}.${fle_ext}.etss2.0.env -f_tide ${tide_v} -TideDatabase 2014 -spinUp 0 -rexSave 6 -nest 0 -verbose 1 >> $pgmout 2> errfile_${RANKO}
      err=$?;export err; err_chk
      echo "`date`: Finished Run for ${bsn} in tide ${tide_v}" >> timing.txt
   done

elif [[ ${f_gulf} == epn3 ]] ; then

   bsn=epn3

   export pgm="petss_model"
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

   echo "`date`: Starting Run for ${bsn} in tide ${tide_v}" >> timing.txt
   if [[ $pgmout == '' ]] ; then
      pgmout=OUTPUT.0000
   fi
   startmsg
   ${EXECpetss}/petss_model -basin ${bsn} -rootDir ${PARMpetss} -trk ${PARMpetss}/trkfiles/tideOnly_14.trk -rex ${bsn}.${WNDS}${PEP}.${fle_ext}.etss2.0.rex -env ${bsn}.${fle_ext}.etss2.0.env -f_tide ${tide_v} -TideDatabase 2014 -spinUp 0 -rexSave 6 -nest 0 -verbose 1 >> $pgmout 2> errfile_${RANKO}
      err=$?;export err; err_chk
      echo "`date`: Finished Run for ${bsn} in tide ${tide_v}" >> timing.txt

elif [[ ${f_gulf} == ejx3 ]] ; then

   for bsn in ejx3 ht3 ebp3
   do
      export pgm="petss_model"
#      . prep_step
      if [[ ${bsn} == ebp3 ]] ; then
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

      echo "`date`: Starting Run for ${bsn} in tide ${tide_v}" >> timing.txt
      if [[ $pgmout == '' ]] ; then
         pgmout=OUTPUT.0000
      fi
      startmsg
      ${EXECpetss}/petss_model -basin ${bsn} -rootDir ${PARMpetss} -trk ${PARMpetss}/trkfiles/tideOnly_14.trk -rex ${bsn}.${WNDS}${PEP}.${fle_ext}.etss2.0.rex -env ${bsn}.${fle_ext}.etss2.0.env -f_tide ${tide_v} -TideDatabase 2014 -spinUp 0 -rexSave 6 -nest 0 -verbose 1 >> $pgmout 2> errfile_${RANKO}
      err=$?;export err; err_chk
      echo "`date`: Finished Run for ${bsn} in tide ${tide_v}" >> timing.txt
   done

elif [[ ${f_gulf} == g1 ]] ; then
  ########################################################################################
  #  Nesting run extratropical storm surge model for following tropical basins then exit.
  #  in which the nesting boundary are generated from egm3 (gulf of mexico basin) run
  ########################################################################################
   for bsn in efm2 etp3 cd2 ap3 emo2 ms7 lf2 ps2
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

      echo "`date`: Starting Run for ${bsn} in tide ${tide_v}" >> timing.txt

      if [[ $pgmout == '' ]] ; then
         pgmout=OUTPUT.0000
      fi
      startmsg

         ${EXECpetss}/petss_model -basin ${bsn} -rootDir ${PARMpetss} -trk ${PARMpetss}/trkfiles/tideOnly_14.trk -rex ${bsn}.${WNDS}${PEP}.${fle_ext}.etss2.0.rex -env ${bsn}.${fle_ext}.etss2.0.env -f_tide ${tide_v} -TideDatabase 2014 -spinUp 0 -rexSave 6 -nest 0 -verbose 1 >> $pgmout 2> errfile_${RANKO}
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

      echo "`date`: Starting Run for ${bsn} in tide ${tide_v}" >> timing.txt

      if [[ $pgmout == '' ]] ; then
         pgmout=OUTPUT.0000
      fi
      startmsg

         ${EXECpetss}/petss_model -basin ${bsn} -rootDir ${PARMpetss} -trk ${PARMpetss}/trkfiles/tideOnly_14.trk -rex ${bsn}.${WNDS}${PEP}.${fle_ext}.etss2.0.rex -env ${bsn}.${fle_ext}.etss2.0.env -f_tide ${tide_v} -TideDatabase 2014 -spinUp 0 -rexSave 6 -nest 0 -verbose 1 >> $pgmout 2> errfile_${RANKO}
      err=$?;export err; err_chk
      echo "`date`: Finished Run for ${bsn} in tide ${tide_v}" >> timing.txt


elif [[ ${f_gulf} == e1 ]] ; then

   for bsn in pn2 pv2 ny3 de3 eke2 hmi3
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

      echo "`date`: Starting Run for ${bsn} in tide ${tide_v}" >> timing.txt

      startmsg
      ${EXECpetss}/petss_model -basin ${bsn} -rootDir ${PARMpetss} -trk ${PARMpetss}/trkfiles/tideOnly_14.trk -rex ${bsn}.${WNDS}${PEP}.${fle_ext}.etss2.0.rex -env ${bsn}.${fle_ext}.etss2.0.env -f_tide ${tide_v} -TideDatabase 2014 -spinUp 0 -rexSave 6 -nest 0 -verbose 1 >> $pgmout 2> errfile_${RANKO}
      err=$?;export err; err_chk
      echo "`date`: Finished Run for ${bsn} in tide ${tide_v}" >> timing.txt

   done

elif [[ ${f_gulf} == e2 ]] ; then

   for bsn in il3 hch2 esv4 co2 pb3 ebr3 egl3 cr3
   do
      export pgm="petss_model"
#      . prep_step
      case ${bsn} in

      il3)
          echo e ${iopt} > fle20.${WNDS}${PEP}.${fle_ext}.${bsn} ;;
      hch2)
          echo e ${iopt} > fle20.${WNDS}${PEP}.${fle_ext}.${bsn} ;;
      esv4)
          echo e ${iopt} > fle20.${WNDS}${PEP}.${fle_ext}.${bsn} ;;
      co2)
          echo e ${iopt} > fle20.${WNDS}${PEP}.${fle_ext}.${bsn} ;;
      pb3)
          echo e ${iopt} > fle20.${WNDS}${PEP}.${fle_ext}.${bsn} ;;
      ebr3)
          echo g ${iopt} > fle20.${WNDS}${PEP}.${fle_ext}.${bsn} ;;
      egl3)
          echo g ${iopt} > fle20.${WNDS}${PEP}.${fle_ext}.${bsn} ;;
      cr3)
          echo g ${iopt} > fle20.${WNDS}${PEP}.${fle_ext}.${bsn} ;;
      esac
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

      echo "`date`: Starting Run for ${bsn} in tide ${tide_v}" >> timing.txt
      startmsg

      ${EXECpetss}/petss_model -basin ${bsn} -rootDir ${PARMpetss} -trk ${PARMpetss}/trkfiles/tideOnly_14.trk -rex ${bsn}.${WNDS}${PEP}.${fle_ext}.etss2.0.rex -env ${bsn}.${fle_ext}.etss2.0.env -f_tide ${tide_v} -TideDatabase 2014 -spinUp 0 -rexSave 6 -nest 0 -verbose 1 >> $pgmout 2> errfile_${RANKO}
      err=$?;export err; err_chk
      echo "`date`: Finished Run for ${bsn} in tide ${tide_v}" >> timing.txt
   done      #  Basin loop

fi

