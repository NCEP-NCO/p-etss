#!/bin/bash
#
#
# Huiqing.Liu/MDL--- 03/08/2016 -------PETSS EX-SCRIPT ---------
#
# "HISTORY: 03/08/2016 - First Version of PETSS1.0, which use 21 GEFS ensemble"
# "                      members to run ETSS2.1. H.Liu/MDL
# "         12/06/2017 - PETSS1.0 is implemented. H.Liu/MDL
# "         03/06/2018 - First Version of PETSS1.1, which uses 42 NAEFS
# "                      ensemble members at 00/12z to run ETSS2.2 and operationlize
# "                      station based bias-corrected post-processing" H.Liu/MDL
# "         03/08/2019 - Second Version of PETSS1.1, which uses previous cycle wind
# "                      at 06z and 18z to fill the gap for CMC wind only providing
# "                      products at 00 and 12z. H.Liu/MDL
# "         02/10/2020 - Adding option to use new 31 GEFS members. H.Liu/MDL
# "         07/07/2020 - Adding GEFS only station text products. H.Liu/MDL
# "         10/01/2021 - Converting to use cfp poe methodology and break one big job into 4 jobs.
#                        PREWND, MODEL, Statistic, and MERGING. H.Liu/MDL
# " ---------------------------------------------------------"
# " ---------------------------------------------------------"
#
# Author: Huiqing.Liu (huiqing.liu@noaa.gov)
# Abstract:
# Borrow the main ideas from ETSS ex-script
#  Inputs:  1) Last cycle's water initital wate condition file and csv files
#           2) Wind ensemble members files generated by PETSS_PREWND job
#           3) sds.${cyc} to make sure wind files are good
#           4) mask file for NEP basin - parm file
#
#  Outputs: text and native resolution grid products for each of wind ensemble member
#############################################################################################
set -x
export RANKO=$1
export RANK=$2
export WNDS=$3
export PEP=$4

echo "`date`: Start Rank ${RANKO} ${RANK} ${WNDS} ${PEP}" >> timing.txt
#if [[ ${RANK} == 0 ]] ; then
#   echo 'Y' > sds.${WNDS}${PEP}.${cyc}
#   echo "Finished copy" > msg_Done_WND_cp_${WNDS}${PEP}.txt
#fi
#while [[ ! -f msg_Done_WND_cp_${WNDS}${PEP}.txt ]] ; do
#  echo "`date`: Rank ${RANKO} waiting for msg_Done_WND_cp_${WNDS}${PEP}.txt." >> timing.txt
#  sleep 1
#done

if [[ ${WNDS} == gep || ${WNDS} == gec || ${WNDS} == cmc_gec && ! -s ${COMOUT}/PETSS_GEFS_${cyc} || ${WNDS} == cmc_gep && ! -s ${COMOUT}/PETSS_GEFS_${cyc} ]] ; then


if [[ ${RANK} == 0 ]] ; then

  echo "`date`: Rank ${RANK} Starting model runs east coast extra-tropical basins surge only." >> timing.txt
  ${USHpetss}/petss_poe_naefs.sh e ${cyc} ${PARMpetss} ${EXECpetss} 0 ${WNDS} ${PEP}
  echo "`date`: Rank ${RANK} finished east coast extra-tropical basins surge only run." >> timing.txt
  echo "Model in East Coast Surge only is completed" > msg_EastDone_${WNDS}${PEP}.txt

elif [[ ${RANK} == 1 ]] ; then

  echo "`date`: Rank ${RANK} Starting model runs ALA extra-tropical basins surge only." >> timing.txt
     ${USHpetss}/petss_poe_naefs.sh ala ${cyc} ${PARMpetss} ${EXECpetss} 0 ${WNDS} ${PEP}
  echo "`date`: Rank ${RANK} finished ALA extra-tropical basins surge only run." >> timing.txt
###################################################################
#  Post process station output for Alaska regions
####################################################################
  bsn=k
  ####################################################################
  #  Run program to extract storm surge output.
  ####################################################################
    . prep_step

    fle_ext="surge"
    fle_extf="stormsurge"
    export pgm="petss_out_stn"

    echo $bsn > control_station.${WNDS}${PEP}.${fle_ext}.${bsn}.txt
    export FORT10=control_station.${WNDS}${PEP}.${fle_ext}.${bsn}.txt

    export FORT11=$PARMpetss/model/mdl_ft11_${fle_ext}.${bsn}
    if [[ ${WNDS} == gep || ${WNDS} == gec ]] ; then
       export FORT14=$PARMpetss/model/mdl_ft01.ega
    else
       if [[ ${cyc} == 06 || ${cyc} == 18 ]] ;then
          export FORT14=$PARMpetss/model/mdl_ft01.ega.stn
       else
          export FORT14=$PARMpetss/model/mdl_ft01.ega
       fi
    fi
    export FORT67=$PARMpetss/model/mdl_ettgp.${bsn}_2nd

    export FORT16=gfspuv.${WNDS}${PEP}.${cyc}${bsn}
    export FORT96=sds.${WNDS}${PEP}.${cyc}
    export FORT17=sshistory.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
    export FORT18=fle10.${WNDS}${PEP}.${fle_ext}.${bsn}
    export FORT34=sshistory.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}_2nd
    export FORT35=fle70.${WNDS}${PEP}.${fle_ext}.${bsn}

    bsnf="goa"

    export FORT58=mdlsurge.${WNDS}${PEP}.${cyc}${bsn}
    export FORT59=etss.${WNDS}${PEP}.${cycle}.${fle_extf}.${bsnf}.txt

    startmsg
    $EXECpetss/petss_out_stn >> $pgmout 2> errfile_${RANKO}
    export err=$?; err_chk
    echo "`date`: Finished extracting text answers for ${bsn} surge only" >> timing.txt
    . prep_step

    fle_ext="surge_tide"
    fle_extf="stormtide"
    export pgm="petss_out_stn"

    echo $bsn > control_station.${WNDS}${PEP}.${fle_ext}.${bsn}.txt
    export FORT10=control_station.${WNDS}${PEP}.${fle_ext}.${bsn}.txt

    export FORT11=$PARMpetss/model/mdl_ft11_${fle_ext}.${bsn}
    export FORT12=$PARMpetss/model/mdl_ft11_${fle_ext}.a
    export FORT13=$PARMpetss/model/mdl_ft11_${fle_ext}.z
    if [[ ${WNDS} == gep || ${WNDS} == gec ]] ; then
       export FORT14=$PARMpetss/model/mdl_ft01.ega
    else
       if [[ ${cyc} == 06 || ${cyc} == 18 ]] ;then
          export FORT14=$PARMpetss/model/mdl_ft01.ega.stn
       else
          export FORT14=$PARMpetss/model/mdl_ft01.ega
       fi
    fi
    export FORT67=$PARMpetss/model/mdl_ettgp.${bsn}_2nd

    export FORT16=gfspuv.${WNDS}${PEP}.${cyc}${bsn}

    export FORT57=tmp.mdlsurge.${WNDS}${PEP}.${fle_extf}.${cyc}${bsn}
    export FORT58=tmp.mdlsurge.${WNDS}${PEP}.${fle_extf}.${cyc}${bsn}
    export FORT59=etss.${WNDS}${PEP}.${cycle}.${fle_extf}.${bsnf}.txt

    export FORT96=sds.${WNDS}${PEP}.${cyc}

    export FORT17=sshistory.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
    export FORT18=fle10.${WNDS}${PEP}.${fle_ext}.${bsn}
    export FORT34=sshistory.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}_2nd
    export FORT35=fle70.${WNDS}${PEP}.${fle_ext}.${bsn}
    startmsg
    $EXECpetss/petss_out_stn >> $pgmout 2> errfile_${RANKO}
    export err=$?; err_chk

    echo "`date`: Finished extracting text answers for ${bsn} surge_tide" >> timing.txt

  echo "Model in Ak Surge only is completed" > msg_AKDone_${WNDS}${PEP}.txt

elif [[ ${RANK} == 2 ]] ; then

  echo "`date`: Rank ${RANK} Starting model runs Gulf of Mexico extra-tropical basins surge only." >> timing.txt
  ${USHpetss}/petss_poe_naefs.sh g ${cyc} ${PARMpetss} ${EXECpetss} 0 ${WNDS} ${PEP}
  echo "`date`: Rank ${RANK} finished Gulf of Mexico extra-tropical basins surge only run." >> timing.txt
  echo "Model in Gulf Surge only is completed" > msg_GulfDone_${WNDS}${PEP}.txt


elif [[ ${RANK} == 3 ]] ; then

  echo "`date`: Rank ${RANK} Starting model runs New Bering Sea extra-tropical basins surge_tide." >> timing.txt
  ${USHpetss}/petss_poe_naefs.sh m ${cyc} ${PARMpetss} ${EXECpetss} V2.2.100 ${WNDS} ${PEP}
  echo "`date`: Rank ${RANK} finished New Bering Sea extra-tropical basins surge_tide run." >> timing.txt
  echo "Model in New Ala Bering Surge_tide is completed" > msg_NewBeringDone_surge_tide_${WNDS}${PEP}.txt

  while [[ ! -f msg_NewBeringDone_surge_${WNDS}${PEP}.txt ]] ; do
        echo "`date`: Rank ${RANK} Sleeping." >> timing.txt
        sleep 1
  done

###################################################################
#  Post process station output for Alaska regions
####################################################################
  for bsn in m ; do
  
  ####################################################################
  #  Run program to extract storm surge output.
  ####################################################################
    . prep_step

    fle_ext="surge"
    fle_extf="stormsurge"
    export pgm="petss_out_stn"

    echo $bsn > control_station.${WNDS}${PEP}.${fle_ext}.${bsn}.txt
    export FORT10=control_station.${WNDS}${PEP}.${fle_ext}.${bsn}.txt
 
    export FORT11=$PARMpetss/model/mdl_ft11_${fle_ext}.${bsn}
    if [[ ${WNDS} == gep || ${WNDS} == gec ]] ; then
       export FORT14=$PARMpetss/model/mdl_ft01.ega 
    else
       if [[ ${cyc} == 06 || ${cyc} == 18 ]] ;then
          export FORT14=$PARMpetss/model/mdl_ft01.ega.stn
       else
          export FORT14=$PARMpetss/model/mdl_ft01.ega 
       fi
    fi
    export FORT67=$PARMpetss/model/mdl_ettgp.${bsn}_2nd 

    export FORT16=gfspuv.${WNDS}${PEP}.${cyc}${bsn}
    export FORT96=sds.${WNDS}${PEP}.${cyc}
    export FORT17=sshistory.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
    export FORT18=fle10.${WNDS}${PEP}.${fle_ext}.${bsn} 
    export FORT34=sshistory.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}_2nd
    export FORT35=fle70.${WNDS}${PEP}.${fle_ext}.${bsn} 

    bsnf="ber"

    export FORT12=$PARMpetss/model/mdl_ft11_${fle_ext}.a
    export FORT13=$PARMpetss/model/mdl_ft11_${fle_ext}.z

    export FORT57=mdlsurge.${WNDS}${PEP}.${cyc}a
    export FORT58=mdlsurge.${WNDS}${PEP}.${cyc}z
    export FORT59=etss.${WNDS}${PEP}.${cycle}.${fle_extf}.${bsnf}.txt


    startmsg
    $EXECpetss/petss_out_stn >> $pgmout 2> errfile_${RANKO}
    export err=$?; err_chk

    echo "`date`: Finished extracting text answers for ${bsn} surge only" >> timing.txt

    . prep_step

    fle_ext="surge_tide"
    fle_extf="stormtide"
    export pgm="petss_out_stn"

    echo $bsn > control_station.${WNDS}${PEP}.${fle_ext}.${bsn}.txt
    export FORT10=control_station.${WNDS}${PEP}.${fle_ext}.${bsn}.txt

    export FORT11=$PARMpetss/model/mdl_ft11_${fle_ext}.${bsn}
    export FORT12=$PARMpetss/model/mdl_ft11_${fle_ext}.a
    export FORT13=$PARMpetss/model/mdl_ft11_${fle_ext}.z
    if [[ ${WNDS} == gep || ${WNDS} == gec ]] ; then 
       export FORT14=$PARMpetss/model/mdl_ft01.ega
    else
       if [[ ${cyc} == 06 || ${cyc} == 18 ]] ;then
          export FORT14=$PARMpetss/model/mdl_ft01.ega.stn
       else
          export FORT14=$PARMpetss/model/mdl_ft01.ega 
       fi
    fi 
    export FORT67=$PARMpetss/model/mdl_ettgp.${bsn}_2nd
    
    export FORT16=gfspuv.${WNDS}${PEP}.${cyc}${bsn}

    export FORT57=tmp.mdlsurge.${WNDS}${PEP}.${fle_extf}.${cyc}${bsn}
    export FORT58=tmp.mdlsurge.${WNDS}${PEP}.${fle_extf}.${cyc}${bsn}
    export FORT59=etss.${WNDS}${PEP}.${cycle}.${fle_extf}.${bsnf}.txt

    export FORT96=sds.${WNDS}${PEP}.${cyc}

    export FORT17=sshistory.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
    export FORT18=fle10.${WNDS}${PEP}.${fle_ext}.${bsn}
    export FORT34=sshistory.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}_2nd
    export FORT35=fle70.${WNDS}${PEP}.${fle_ext}.${bsn}
    startmsg 
    $EXECpetss/petss_out_stn >> $pgmout 2> errfile_${RANKO}
    export err=$?; err_chk

    echo "`date`: Finished extracting text answers for ${bsn} surge_tide" >> timing.txt

  done      #  Basin loop

elif [[ ${RANK} == 4 ]] ; then

  echo "`date`: Rank ${RANK} Starting model runs cp5 tide only." >> timing.txt
  ${USHpetss}/petss_poe_naefs.sh cp5 ${cyc} ${PARMpetss} ${EXECpetss} T1 ${WNDS} ${PEP}
  echo "`date`: Rank ${RANK} finished cp5 tropical basins tide only run." >> timing.txt
  echo "Model cp5 tide only is completed" > msg_Done_tropical1_tide_${WNDS}${PEP}.txt

elif [[ ${RANK} == 5 ]] ; then

  echo "`date`: Rank ${RANK} Starting model runs west coast extra-tropical basins surge only." >> timing.txt
  ${USHpetss}/petss_poe_naefs.sh nep ${cyc} ${PARMpetss} ${EXECpetss} 0 ${WNDS} ${PEP}
  echo "`date`: Rank ${RANK} finished west coast extra-tropical basins surge only run." >> timing.txt
  echo "Model in West Coast Surge only is completed" > msg_WestDone_surge_${WNDS}${PEP}.txt


  ####################################################################
  #  Run program to extract storm surge output for western coastal grids.
  ####################################################################

  . prep_step

  bsn=n
  bsn1=w
  bsnf=wst
  fle_ext=surge

  export pgm="petss_out_stn"

  echo $bsn > control_station.${WNDS}${PEP}.${fle_ext}.${bsn}.txt
  export FORT10=control_station.${WNDS}${PEP}.${fle_ext}.${bsn}.txt

  if [[ ${fle_ext} == surge ]] ; then
     fle_extf="stormsurge"
     export FORT58=mdlsurge.${WNDS}${PEP}.${cyc}${bsn1}
  else
     fle_extf="stormtide"
     export FORT58=tmp.mdlsurge.${WNDS}${PEP}.stormtide.${cyc}${bsn1}
  fi

  export FORT11=$PARMpetss/model/mdl_ft11_${fle_ext}.${bsn}
  if [[ ${WNDS} == gep || ${WNDS} == gec ]] ; then 
     export FORT14=$PARMpetss/model/mdl_ft01.ega
  else
     if [[ ${cyc} == 06 || ${cyc} == 18 ]] ;then
        export FORT14=$PARMpetss/model/mdl_ft01.ega.stn
       else
        export FORT14=$PARMpetss/model/mdl_ft01.ega 
     fi
  fi 

  export FORT67=$PARMpetss/model/mdl_ettgp.${bsn}_2nd
  export FORT16=gfspuv.${WNDS}${PEP}.${cyc}${bsn}
  export FORT59=etss.${WNDS}${PEP}.${cycle}.${fle_extf}.${bsnf}.txt
  export FORT96=sds.${WNDS}${PEP}.${cyc}
  export FORT17=sshistory.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
  export FORT18=fle10.${WNDS}${PEP}.${fle_ext}.${bsn}
  export FORT34=sshistory.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}_2nd
  export FORT35=fle70.${WNDS}${PEP}.${fle_ext}.${bsn}
  startmsg
  $EXECpetss/petss_out_stn >> $pgmout 2> errfile_${RANKO}
  export err=$?; err_chk

  echo "`date`: Finished extracting text answers for ${bsn}" >> timing.txt

elif [[ ${RANK} == 6 ]] ; then

  echo "`date`: Rank ${RANK} Starting model runs east coast tide only." >> timing.txt
  ${USHpetss}/petss_poe_naefs.sh e ${cyc} ${PARMpetss} ${EXECpetss} T1 ${WNDS} ${PEP}
  echo "Model in East Coast Tide only is completed" > msg_EastDone_tide_${WNDS}${PEP}.txt
  echo "`date`: Rank ${RANK} finished eas coast tide only run." >> timing.txt

elif [[ ${RANK} == 8 ]] ; then

  echo "`date`: Rank ${RANK} Starting model runs west coast surge+tide." >> timing.txt
  ${USHpetss}/petss_poe_naefs.sh nep ${cyc} ${PARMpetss} ${EXECpetss} V2.4.30 ${WNDS} ${PEP}
  echo "Model in West Coast Surge only is completed" > msg_WestDone_surge_tide_${WNDS}${PEP}.txt
  echo "`date`: Rank ${RANK} finished west coast extra-tropical basins surge-tide." >> timing.txt

  . prep_step

  bsn=n
  bsn1=w
  bsnf=wst
  fle_ext=surge_tide

  export pgm="petss_out_stn"

  echo $bsn > control_station.${WNDS}${PEP}.${fle_ext}.${bsn}.txt
  export FORT10=control_station.${WNDS}${PEP}.${fle_ext}.${bsn}.txt

  if [[ ${fle_ext} == surge ]] ; then
     fle_extf="stormsurge"
     export FORT58=mdlsurge.${WNDS}${PEP}.${cyc}${bsn1}
  else
     fle_extf="stormtide"
     export FORT58=tmp.mdlsurge.${WNDS}${PEP}.stormtide.${cyc}${bsn1}
  fi

  export FORT11=$PARMpetss/model/mdl_ft11_${fle_ext}.${bsn}
  if [[ ${WNDS} == gep || ${WNDS} == gec ]] ; then 
     export FORT14=$PARMpetss/model/mdl_ft01.ega
  else
     if [[ ${cyc} == 06 || ${cyc} == 18 ]] ;then
        export FORT14=$PARMpetss/model/mdl_ft01.ega.stn
       else
        export FORT14=$PARMpetss/model/mdl_ft01.ega 
     fi
  fi 

  export FORT67=$PARMpetss/model/mdl_ettgp.${bsn}_2nd
  export FORT16=gfspuv.${WNDS}${PEP}.${cyc}${bsn}
  export FORT59=etss.${WNDS}${PEP}.${cycle}.${fle_extf}.${bsnf}.txt
  export FORT96=sds.${WNDS}${PEP}.${cyc}
  export FORT17=sshistory.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}
  export FORT18=fle10.${WNDS}${PEP}.${fle_ext}.${bsn}
  export FORT34=sshistory.${WNDS}${PEP}.${fle_ext}.${cyc}${bsn}_2nd
  export FORT35=fle70.${WNDS}${PEP}.${fle_ext}.${bsn}
  startmsg
  $EXECpetss/petss_out_stn >> $pgmout 2> errfile_${RANKO}
  export err=$?; err_chk
 
  echo "`date`: Finished extracting text answers for ${bsn}" >> timing.txt


elif [[ ${RANK} == 9 ]] ; then

  echo "`date`: Rank ${RANK} Starting model runs east coast I tide only." >> timing.txt
  ${USHpetss}/petss_poe_naefs.sh e1 ${cyc} ${PARMpetss} ${EXECpetss} T1 ${WNDS} ${PEP}
  echo "Model in east coast I Tide only is completed" > msg_Done_tropical2_tide_${WNDS}${PEP}.txt
  echo "`date`: Rank ${RANK} finished east coast I tide only run." >> timing.txt


  echo "`date`: Rank ${RANK} Starting model runs Gulf of Mexico tropical tide only." >> timing.txt
  ${USHpetss}/petss_poe_naefs.sh g1 ${cyc} ${PARMpetss} ${EXECpetss} T1 ${WNDS} ${PEP}
  echo "Model nesting Gulf of mexico complete" > msg_Done_tropical3_tide_${WNDS}${PEP}.txt
  echo "`date`: Rank ${RANK} finished Gulf of Mexico tropical tide only run." >> timing.txt



  echo "`date`: Rank ${RANK} Starting model runs Gulf of Mexico tide only." >> timing.txt
  ${USHpetss}/petss_poe_naefs.sh g ${cyc} ${PARMpetss} ${EXECpetss} T1 ${WNDS} ${PEP}
  echo "Model in Gulf Tide only is completed" > msg_GulfDone_tide_${WNDS}${PEP}.txt
  echo "`date`: Rank ${RANK} finished Gulf of Mexico tide only run." >> timing.txt

elif [[ ${RANK} == 11 ]] ; then

  echo "`date`: Rank ${RANK} Starting model runs hpa2 tide only." >> timing.txt
  ${USHpetss}/petss_poe_naefs.sh hpa2 ${cyc} ${PARMpetss} ${EXECpetss} T1 ${WNDS} ${PEP}
  echo "Model in hpa2 Tide only is completed" > msg_Done_tropical7_tide_${WNDS}${PEP}.txt
  echo "`date`: Rank ${RANK} finished hpa2 tide only run." >> timing.txt


elif [[ ${RANK} == 13 ]] ; then

  echo "`date`: Rank ${RANK} Starting model runs New Bering Sea extra-tropical basins surgeonly." >> timing.txt
  ${USHpetss}/petss_poe_naefs.sh m ${cyc} ${PARMpetss} ${EXECpetss} 0 ${WNDS} ${PEP}
  echo "`date`: Rank ${RANK} finished New Bering Sea extra-tropical basins surge only run." >> timing.txt
  echo "Model in New Ala Bering Surge only is completed" > msg_NewBeringDone_surge_${WNDS}${PEP}.txt

  echo "`date`: Rank ${RANK} Starting model runs epn3 tide only." >> timing.txt
  ${USHpetss}/petss_poe_naefs.sh epn3 ${cyc} ${PARMpetss} ${EXECpetss} T1 ${WNDS} ${PEP}
  echo "Model in epn3 Tide only is completed" > msg_Done_tropical5_tide_${WNDS}${PEP}.txt
  echo "`date`: Rank ${RANK} finished epn3 tide only run." >> timing.txt

fi


while [[ ! -f msg_GulfDone_${WNDS}${PEP}.txt ]] ; do
#  if [[ ${RANK} != 0 && ${RANK} != 1 && ${RANK} != 4 && ${RANK} != 5 ]] ; then
  if [[ ${RANK} == 2 || ${RANK} == 13 || ${RANK} == 8 || ${RANK} == 9 ]] ; then
    echo "`date`: Rank ${RANK} Sleeping." >> timing.txt
    sleep 1
  else
    break
  fi
done

while [[ ! -f msg_EastDone_${WNDS}${PEP}.txt ]] ; do
  echo "`date`: Rank ${RANK} Sleeping." >> timing.txt
  sleep 1
done


echo "`date`: Rank ${RANK} Starting model nesting runs at tropical basins." >> timing.txt

if [[ ${RANK} == 0 ]] ; then

  echo "`date`: Rank ${RANK} Starting model runs cp5 surge_tide." >> timing.txt
  ${USHpetss}/petss_nest_poe_naefs.sh cp5 ${cyc} ${PARMpetss} ${EXECpetss} VDEF ${WNDS} ${PEP}
  echo "Model nesting cp5 complete" > msg_Done_${WNDS}${PEP}_nest1_tide.txt
  echo "`date`: Rank ${RANK} finished cp5 surge_tide run." >> timing.txt

  echo "`date`: Rank ${RANK} Starting model runs ap3 and ps2 surge only" >> timing.txt
  ${USHpetss}/petss_nest_poe_naefs.sh g1c ${cyc} ${PARMpetss} ${EXECpetss} 0 ${WNDS} ${PEP}
  echo "Model nesting ap3 and ps2 complete" > msg_Done_${WNDS}${PEP}_nest11_surge.txt
  echo "`date`: Rank ${RANK} finished ap3 and ps2 surge only run." >> timing.txt


elif [[ ${RANK} == 1 ]] ; then


  echo "`date`: Rank ${RANK} Starting model runs east coast II tropical surge_tide." >> timing.txt
  ${USHpetss}/petss_nest_poe_naefs.sh e2 ${cyc} ${PARMpetss} ${EXECpetss} VDEF ${WNDS} ${PEP}
  echo "`date`: Rank ${RANK} finished east coast II tropical surge_tide run." >> timing.txt
  echo "Model nesting Gulf of mexico complete" > msg_Done_${WNDS}${PEP}_nest6_tide.txt

elif [[ ${RANK} == 2 ]] ; then

  echo "`date`: Rank ${RANK} Starting model runs Gulf of Mexico tropical surge_tide." >> timing.txt
  ${USHpetss}/petss_nest_poe_naefs.sh g1 ${cyc} ${PARMpetss} ${EXECpetss} VDEF ${WNDS} ${PEP}
  echo "Model nesting Gulf of mexico complete" > msg_Done_${WNDS}${PEP}_nest3_tide.txt
  echo "`date`: Rank ${RANK} finished Gulf of Mexico tropical surge_tide run." >> timing.txt


elif [[ ${RANK} == 3 ]] ; then

  echo "`date`: Rank ${RANK} Starting model runs ht3 surge_tide." >> timing.txt
  ${USHpetss}/petss_nest_poe_naefs.sh ht3 ${cyc} ${PARMpetss} ${EXECpetss} VDEF ${WNDS} ${PEP}
  echo "Model nesting ht3 complete" > msg_Done_${WNDS}${PEP}_nest12_tide.txt
  echo "`date`: Rank ${RANK} finished ht3 surge_tide only." >> timing.txt

  echo "`date`: Rank ${RANK} Starting model runs ebp3 surge_tide." >> timing.txt
  ${USHpetss}/petss_nest_poe_naefs.sh ebp3 ${cyc} ${PARMpetss} ${EXECpetss} VDEF ${WNDS} ${PEP}
  echo "Model nesting ebp3 complete" > msg_Done_${WNDS}${PEP}_nest13_tide.txt

  echo "`date`: Rank ${RANK} finished ebp3 surge_tide only." >> timing.txt

  echo "`date`: Rank ${RANK} Starting model runs hor3 surge only." >> timing.txt
  ${USHpetss}/petss_nest_poe_naefs.sh hor3 ${cyc} ${PARMpetss} ${EXECpetss} 0 ${WNDS} ${PEP}
  echo "Model nesting hor3 complete" > msg_Done_${WNDS}${PEP}_nest8_surge.txt
  echo "`date`: Rank ${RANK} finished hor3 surge only run." >> timing.txt

  echo "`date`: Rank ${RANK} Starting model runs hor3 surge tide." >> timing.txt
  ${USHpetss}/petss_nest_poe_naefs.sh hor3 ${cyc} ${PARMpetss} ${EXECpetss} VDEF ${WNDS} ${PEP}
  echo "Model nesting Gulf of mexico complete" > msg_Done_${WNDS}${PEP}_nest8_tide.txt
  echo "`date`: Rank ${RANK} finished hor3 surge tide run." >> timing.txt

  echo "`date`: Rank ${RANK} Starting model runs hpa2 tropical surge only." >> timing.txt
  ${USHpetss}/petss_nest_poe_naefs.sh hpa2 ${cyc} ${PARMpetss} ${EXECpetss} 0 ${WNDS} ${PEP}
  echo "Model nesting hpa2 complete" > msg_Done_${WNDS}${PEP}_nest7_surge.txt
  echo "`date`: Rank ${RANK} finished hpa2 tropical surge only run." >> timing.txt


elif [[ ${RANK} == 4 ]] ; then 

  echo "`date`: Rank ${RANK} Starting model runs ejx3 surge only." >> timing.txt
  ${USHpetss}/petss_nest_poe_naefs.sh ejx3 ${cyc} ${PARMpetss} ${EXECpetss} 0 ${WNDS} ${PEP}
  echo "Model ejx3 complete" > msg_Done_${WNDS}${PEP}_nest5_surge.txt
  echo "`date`: Rank ${RANK} finished ejx3 surge only run." >> timing.txt

  echo "`date`: Rank ${RANK} Starting model runs egl3 tropical surge_tide." >> timing.txt
  ${USHpetss}/petss_nest_poe_naefs.sh egl3 ${cyc} ${PARMpetss} ${EXECpetss} VDEF ${WNDS} ${PEP}
  echo "Model nesting egl3 complete" > msg_Done_${WNDS}${PEP}_nest9_tide.txt
  echo "`date`: Rank ${RANK} finished egl3 tropical surge_tide run." >> timing.txt


elif [[ ${RANK} == 5 ]] ; then


  echo "`date`: Rank ${RANK} Starting model runs Gulf of Mexico tropical surge only." >> timing.txt
  ${USHpetss}/petss_nest_poe_naefs.sh g1b ${cyc} ${PARMpetss} ${EXECpetss} 0 ${WNDS} ${PEP}
  echo "`date`: Rank ${RANK} finished Gulf of Mexico tropical surge only run." >> timing.txt

  echo "`date`: Rank ${RANK} Starting model runs egl3 tropical surge only." >> timing.txt
  ${USHpetss}/petss_nest_poe_naefs.sh egl3 ${cyc} ${PARMpetss} ${EXECpetss} 0 ${WNDS} ${PEP}
  echo "Model nesting egl3 complete" > msg_Done_${WNDS}${PEP}_nest9_surge.txt
  echo "`date`: Rank ${RANK} finished egl3 tropical surge only run." >> timing.txt

  echo "`date`: Rank ${RANK} Starting model runs ebp3 surge only" >> timing.txt
  ${USHpetss}/petss_nest_poe_naefs.sh ebp3 ${cyc} ${PARMpetss} ${EXECpetss} 0 ${WNDS} ${PEP}
  echo "Model nesting ebp3 complete" > msg_Done_${WNDS}${PEP}_nest13_surge.txt
  echo "`date`: Rank ${RANK} finished ebp3 surge only." >> timing.txt


  echo "`date`: Rank ${RANK} Starting model runs east coast II tide only." >> timing.txt
  ${USHpetss}/petss_poe_naefs.sh e2 ${cyc} ${PARMpetss} ${EXECpetss} T1 ${WNDS} ${PEP} 
  echo "Model in east coast II Tide only is completed" > msg_Done_tropical6_tide_${WNDS}${PEP}.txt
  echo "`date`: Rank ${RANK} finished eas coast II tide only run." >> timing.txt
 

  echo "`date`: Rank ${RANK} Starting model runs ejx3 tide only." >> timing.txt
  ${USHpetss}/petss_poe_naefs.sh ejx3 ${cyc} ${PARMpetss} ${EXECpetss} T1 ${WNDS} ${PEP}
  echo "Model in ejx3 Tide only is completed" > msg_Done_tropical4_tide_${WNDS}${PEP}.txt
  echo "`date`: Rank ${RANK} finished ejx3 tide only run." >> timing.txt

elif [[ ${RANK} == 6 ]] ; then

  echo "`date`: Rank ${RANK} Starting model runs ejx3 surge_tide." >> timing.txt
  ${USHpetss}/petss_nest_poe_naefs.sh ejx3 ${cyc} ${PARMpetss} ${EXECpetss} VDEF ${WNDS} ${PEP}
  echo "Model ejx3 complete" > msg_Done_${WNDS}${PEP}_nest5_tide.txt
  echo "`date`: Rank ${RANK} finished ejx3 surge_tide run." >> timing.txt

  echo "`date`: Rank ${RANK} Starting model runs ht3 surge only" >> timing.txt
  ${USHpetss}/petss_nest_poe_naefs.sh ht3 ${cyc} ${PARMpetss} ${EXECpetss} 0 ${WNDS} ${PEP}
  echo "Model nesting ht3 complete" > msg_Done_${WNDS}${PEP}_nest12_surge.txt
  echo "`date`: Rank ${RANK} finished ht3 surge only." >> timing.txt

  echo "`date`: Rank ${RANK} Starting model runs ny3 surge only." >> timing.txt
  ${USHpetss}/petss_nest_poe_naefs.sh ny3 ${cyc} ${PARMpetss} ${EXECpetss} 0 ${WNDS} ${PEP}
  echo "Model nesting ny3 complete" > msg_Done_${WNDS}${PEP}_nest10_surge.txt
  echo "`date`: Rank ${RANK} finished ny3 surge only run." >> timing.txt

elif [[ ${RANK} == 7 ]] ; then

  echo "`date`: Rank ${RANK} Starting model runs east coast tropical surge only." >> timing.txt
  ${USHpetss}/petss_nest_poe_naefs.sh e1 ${cyc} ${PARMpetss} ${EXECpetss} 0 ${WNDS} ${PEP}
  echo "Model nesting e1 complete" > msg_Done_${WNDS}${PEP}_nest2_surge.txt
  echo "`date`: Rank ${RANK} finished east coast tropical surge only run." >> timing.txt

elif [[ ${RANK} == 8 ]] ; then

  echo "`date`: Rank ${RANK} Starting model runs Gulf of Mexico tropical surge only." >> timing.txt
  ${USHpetss}/petss_nest_poe_naefs.sh g1a ${cyc} ${PARMpetss} ${EXECpetss} 0 ${WNDS} ${PEP}
  echo "Model nesting Gulf of mexico complete" > msg_Done_${WNDS}${PEP}_nest3_surge.txt
  echo "`date`: Rank ${RANK} finished Gulf of Mexico tropical surge only run." >> timing.txt

  echo "`date`: Rank ${RANK} Starting model runs emo2 surge only." >> timing.txt
  ${USHpetss}/petss_nest_poe_naefs.sh emo2 ${cyc} ${PARMpetss} ${EXECpetss} 0 ${WNDS} ${PEP}
  echo "Model nesting emo2 complete" > msg_Done_${WNDS}${PEP}_nest44_surge.txt
  echo "`date`: Rank ${RANK} finished emo2 surge only." >> timing.txt


elif [[ ${RANK} == 9 ]] ; then

  echo "`date`: Rank ${RANK} Starting model runs epn3 tropical surge only." >> timing.txt
  ${USHpetss}/petss_nest_poe_naefs.sh epn3 ${cyc} ${PARMpetss} ${EXECpetss} 0 ${WNDS} ${PEP}
  echo "Model nesting epn3 complete" > msg_Done_${WNDS}${PEP}_nest4_surge.txt
  echo "`date`: Rank ${RANK} finished epn3 tropical surge only run." >> timing.txt

elif [[ ${RANK} == 10 ]] ; then

  echo "`date`: Rank ${RANK} Starting model runs cp5 tropical surge only." >> timing.txt
  ${USHpetss}/petss_nest_poe_naefs.sh cp5 ${cyc} ${PARMpetss} ${EXECpetss} 0 ${WNDS} ${PEP}
  echo "Model nesting cp5 complete" > msg_Done_${WNDS}${PEP}_nest1_surge.txt
  echo "`date`: Rank ${RANK} finished cp5 tropical surge run." >> timing.txt

  echo "`date`: Rank ${RANK} Starting model runs ny3 surge_tide." >> timing.txt
  ${USHpetss}/petss_nest_poe_naefs.sh ny3 ${cyc} ${PARMpetss} ${EXECpetss} VDEF ${WNDS} ${PEP}
  echo "Model nesting ny3 complete" > msg_Done_${WNDS}${PEP}_nest10_tide.txt
  echo "`date`: Rank ${RANK} finished ny3 surge_tide run." >> timing.txt

  echo "`date`: Rank ${RANK} Starting model runs ap3 surge_tide." >> timing.txt
  ${USHpetss}/petss_nest_poe_naefs.sh ap3 ${cyc} ${PARMpetss} ${EXECpetss} VDEF ${WNDS} ${PEP}
  echo "Model nesting ap3 complete" > msg_Done_${WNDS}${PEP}_nest11_tide.txt
  echo "`date`: Rank ${RANK} finished ap3 surge_tide run." >> timing.txt

elif [[ ${RANK} == 11 ]] ; then

  echo "`date`: Rank ${RANK} Starting model runs east coast II tropical surge only." >> timing.txt
  ${USHpetss}/petss_nest_poe_naefs.sh e2 ${cyc} ${PARMpetss} ${EXECpetss} 0 ${WNDS} ${PEP}
  echo "Model nesting east coast II complete" > msg_Done_${WNDS}${PEP}_nest6_surge.txt
  echo "`date`: Rank ${RANK} finished east coast II tropical surge only run." >> timing.txt

  echo "`date`: Rank ${RANK} Starting model runs hpa2 tropical surge_tide." >> timing.txt
  ${USHpetss}/petss_nest_poe_naefs.sh hpa2 ${cyc} ${PARMpetss} ${EXECpetss} VDEF ${WNDS} ${PEP}
  echo "Model nesting hpa2 complete" > msg_Done_${WNDS}${PEP}_nest7_tide.txt
  echo "`date`: Rank ${RANK} finished hpa2 tropical surge_tide run." >> timing.txt

elif [[ ${RANK} == 12 ]] ; then

  echo "`date`: Rank ${RANK} Starting model runs east coast I surge_tide." >> timing.txt
  ${USHpetss}/petss_nest_poe_naefs.sh e1 ${cyc} ${PARMpetss} ${EXECpetss} VDEF ${WNDS} ${PEP}
  echo "Model nesting e1 complete" > msg_Done_${WNDS}${PEP}_nest2_tide.txt
  echo "`date`: Rank ${RANK} finished east coast I surge_tide run." >> timing.txt

elif [[ ${RANK} == 13 ]] ; then

  echo "`date`: Rank ${RANK} Starting model runs epn3 surge_tide." >> timing.txt
  ${USHpetss}/petss_nest_poe_naefs.sh epn3 ${cyc} ${PARMpetss} ${EXECpetss} VDEF ${WNDS} ${PEP}
  echo "Model nesting epn3 complete" > msg_Done_${WNDS}${PEP}_nest4_tide.txt
  echo "`date`: Rank ${RANK} finished epn3 surge_tide only." >> timing.txt

fi


while [[ ! -f msg_Done_${WNDS}${PEP}_nest1_tide.txt || ! -f msg_Done_${WNDS}${PEP}_nest2_tide.txt || ! -f msg_Done_${WNDS}${PEP}_nest3_tide.txt || ! -f msg_Done_${WNDS}${PEP}_nest4_tide.txt || ! -f msg_Done_${WNDS}${PEP}_nest5_tide.txt || ! -f msg_Done_${WNDS}${PEP}_nest6_tide.txt || ! -f msg_Done_${WNDS}${PEP}_nest7_tide.txt || ! -f msg_Done_${WNDS}${PEP}_nest8_tide.txt || ! -f msg_Done_${WNDS}${PEP}_nest9_tide.txt || ! -f msg_Done_${WNDS}${PEP}_nest10_tide.txt || ! -f msg_Done_${WNDS}${PEP}_nest11_tide.txt || ! -f msg_Done_${WNDS}${PEP}_nest12_tide.txt || ! -f msg_Done_${WNDS}${PEP}_nest13_tide.txt || ! -f msg_Done_${WNDS}${PEP}_nest1_surge.txt || ! -f msg_Done_${WNDS}${PEP}_nest2_surge.txt || ! -f msg_Done_${WNDS}${PEP}_nest3_surge.txt || ! -f msg_Done_${WNDS}${PEP}_nest4_surge.txt || ! -f msg_Done_${WNDS}${PEP}_nest5_surge.txt || ! -f msg_Done_${WNDS}${PEP}_nest6_surge.txt || ! -f msg_Done_${WNDS}${PEP}_nest7_surge.txt || ! -f msg_Done_${WNDS}${PEP}_nest8_surge.txt || ! -f msg_Done_${WNDS}${PEP}_nest9_surge.txt || ! -f msg_Done_${WNDS}${PEP}_nest10_surge.txt || ! -f msg_Done_${WNDS}${PEP}_nest11_surge.txt || ! -f msg_Done_${WNDS}${PEP}_nest12_surge.txt || ! -f msg_Done_${WNDS}${PEP}_nest13_surge.txt || ! -f msg_Done_${WNDS}${PEP}_nest44_surge.txt || ! -f msg_Done_tropical4_tide_${WNDS}${PEP}.txt ]] ; do
  echo "`date`: Rank ${RANK} Sleeping." >> timing.txt
  sleep 1
done

if [[ ${RANK} == 10 ]] ; then

echo "`date`: Rank ${RANK} preparing extracting text product for e and g" >> timing.txt

for bsn in e g ; do
  ####################################################################
  #  Run program to extract storm surge output.
  ####################################################################
 for fle_ext in surge_tide surge ; do

  . prep_step

  if [[ ${fle_ext} == surge ]] ; then
     fle_extf="stormsurge"
     export FORT58=mdlsurge.${WNDS}${PEP}.${cyc}${bsn}
  else
     fle_extf="stormtide"
     export FORT58=tmp.mdlsurge.${WNDS}${PEP}.stormtide.${cyc}${bsn}
  fi

  echo $bsn > ${WNDS}${PEP}.${fle_ext}.control_station.${bsn}.txt
  export FORT10=${WNDS}${PEP}.${fle_ext}.control_station.${bsn}.txt

  export FORT11=$PARMpetss/model/mdl_ft11_${fle_ext}.${bsn}
  if [[ ${WNDS} == gep || ${WNDS} == gec ]] ; then 
     export FORT14=$PARMpetss/model/mdl_ft01.ega
  else
     if [[ ${cyc} == 06 || ${cyc} == 18 ]] ;then
        export FORT14=$PARMpetss/model/mdl_ft01.ega.stn
       else
        export FORT14=$PARMpetss/model/mdl_ft01.ega 
     fi
  fi 

  export FORT16=gfspuv.${WNDS}${PEP}.${cyc}${bsn}
  export FORT96=sds.${WNDS}${PEP}.${cyc}

  export FORT12=sshistory.${WNDS}${PEP}.surge.${cyc}${bsn}
  export FORT13=sshistory.${WNDS}${PEP}.surge.${cyc}${bsn}_2nd
 

  if [[ ${bsn} == e ]] ; then

     bsnf=est

     export pgm="petss_out_stn"

     export FORT17=fle10.${WNDS}${PEP}.${fle_ext}.pn2
     export FORT18=fle10.${WNDS}${PEP}.${fle_ext}.pv2
     export FORT19=fle10.${WNDS}${PEP}.${fle_ext}.ny3
     export FORT20=fle10.${WNDS}${PEP}.${fle_ext}.de3
     export FORT21=fle10.${WNDS}${PEP}.${fle_ext}.cp5
     export FORT22=fle10.${WNDS}${PEP}.${fle_ext}.hor3
     export FORT23=fle10.${WNDS}${PEP}.${fle_ext}.ht3
     export FORT24=fle10.${WNDS}${PEP}.${fle_ext}.il3
     export FORT25=fle10.${WNDS}${PEP}.${fle_ext}.hch2
     export FORT26=fle10.${WNDS}${PEP}.${fle_ext}.esv4
     export FORT27=fle10.${WNDS}${PEP}.${fle_ext}.ejx3
     export FORT28=fle10.${WNDS}${PEP}.${fle_ext}.co2
     export FORT29=fle10.${WNDS}${PEP}.${fle_ext}.hmi3

     export FORT34=fle70.${WNDS}${PEP}.${fle_ext}.pn2
     export FORT35=fle70.${WNDS}${PEP}.${fle_ext}.pv2
     export FORT36=fle70.${WNDS}${PEP}.${fle_ext}.ny3
     export FORT37=fle70.${WNDS}${PEP}.${fle_ext}.de3
     export FORT38=fle70.${WNDS}${PEP}.${fle_ext}.cp5
     export FORT39=fle70.${WNDS}${PEP}.${fle_ext}.hor3
     export FORT40=fle70.${WNDS}${PEP}.${fle_ext}.ht3
     export FORT41=fle70.${WNDS}${PEP}.${fle_ext}.il3
     export FORT42=fle70.${WNDS}${PEP}.${fle_ext}.hch2
     export FORT43=fle70.${WNDS}${PEP}.${fle_ext}.esv4
     export FORT44=fle70.${WNDS}${PEP}.${fle_ext}.ejx3
     export FORT45=fle70.${WNDS}${PEP}.${fle_ext}.co2
     export FORT46=fle70.${WNDS}${PEP}.${fle_ext}.hmi3

     export FORT59=etss.${WNDS}${PEP}.${cycle}.${fle_extf}.${bsnf}.txt

     export FORT67=$PARMpetss/model/mdl_ettgp.pn2_2nd
     export FORT68=$PARMpetss/model/mdl_ettgp.pv2_2nd
     export FORT69=$PARMpetss/model/mdl_ettgp.ny3_2nd
     export FORT70=$PARMpetss/model/mdl_ettgp.de3_2nd
     export FORT71=$PARMpetss/model/mdl_ettgp.cp5_2nd
     export FORT72=$PARMpetss/model/mdl_ettgp.hor3_2nd
     export FORT73=$PARMpetss/model/mdl_ettgp.ht3_2nd
     export FORT74=$PARMpetss/model/mdl_ettgp.il3_2nd
     export FORT75=$PARMpetss/model/mdl_ettgp.hch2_2nd
     export FORT76=$PARMpetss/model/mdl_ettgp.esv4_2nd
     export FORT77=$PARMpetss/model/mdl_ettgp.ejx3_2nd
     export FORT78=$PARMpetss/model/mdl_ettgp.co2_2nd
     export FORT79=$PARMpetss/model/mdl_ettgp.hmi3_2nd

     startmsg
     $EXECpetss/petss_out_stn >> $pgmout 2> errfile_${RANKO}
     export err=$?; err_chk
  elif [[ ${bsn} == g ]]; then

     bsnf=gom

     export pgm="petss_out_stn"
     export FORT17=fle10.${WNDS}${PEP}.${fle_ext}.efm2
     export FORT18=fle10.${WNDS}${PEP}.${fle_ext}.etp3
     export FORT19=fle10.${WNDS}${PEP}.${fle_ext}.cd2
     export FORT20=fle10.${WNDS}${PEP}.${fle_ext}.ap3
     export FORT21=fle10.${WNDS}${PEP}.${fle_ext}.hpa2
     export FORT22=fle10.${WNDS}${PEP}.${fle_ext}.epn3
     export FORT23=fle10.${WNDS}${PEP}.${fle_ext}.emo2
     export FORT24=fle10.${WNDS}${PEP}.${fle_ext}.ms7
     export FORT25=fle10.${WNDS}${PEP}.${fle_ext}.lf2
     export FORT26=fle10.${WNDS}${PEP}.${fle_ext}.ebp3
     export FORT27=fle10.${WNDS}${PEP}.${fle_ext}.egl3
     export FORT28=fle10.${WNDS}${PEP}.${fle_ext}.ps2
     export FORT29=fle10.${WNDS}${PEP}.${fle_ext}.cr3
     export FORT30=fle10.${WNDS}${PEP}.${fle_ext}.ebr3
     export FORT31=fle10.${WNDS}${PEP}.${fle_ext}.eke2

     export FORT34=fle70.${WNDS}${PEP}.${fle_ext}.efm2
     export FORT35=fle70.${WNDS}${PEP}.${fle_ext}.etp3
     export FORT36=fle70.${WNDS}${PEP}.${fle_ext}.cd2
     export FORT37=fle70.${WNDS}${PEP}.${fle_ext}.ap3
     export FORT38=fle70.${WNDS}${PEP}.${fle_ext}.hpa2
     export FORT39=fle70.${WNDS}${PEP}.${fle_ext}.epn3
     export FORT40=fle70.${WNDS}${PEP}.${fle_ext}.emo2
     export FORT41=fle70.${WNDS}${PEP}.${fle_ext}.ms7
     export FORT42=fle70.${WNDS}${PEP}.${fle_ext}.lf2
     export FORT43=fle70.${WNDS}${PEP}.${fle_ext}.ebp3
     export FORT44=fle70.${WNDS}${PEP}.${fle_ext}.egl3
     export FORT45=fle70.${WNDS}${PEP}.${fle_ext}.ps2
     export FORT46=fle70.${WNDS}${PEP}.${fle_ext}.cr3
     export FORT47=fle70.${WNDS}${PEP}.${fle_ext}.ebr3
     export FORT48=fle70.${WNDS}${PEP}.${fle_ext}.eke2

     export FORT59=etss.${WNDS}${PEP}.${cycle}.${fle_extf}.${bsnf}.txt

     export FORT67=$PARMpetss/model/mdl_ettgp.efm2_2nd
     export FORT68=$PARMpetss/model/mdl_ettgp.etp3_2nd
     export FORT69=$PARMpetss/model/mdl_ettgp.cd2_2nd
     export FORT70=$PARMpetss/model/mdl_ettgp.ap3_2nd
     export FORT71=$PARMpetss/model/mdl_ettgp.hpa2_2nd
     export FORT72=$PARMpetss/model/mdl_ettgp.epn3_2nd
     export FORT73=$PARMpetss/model/mdl_ettgp.emo2_2nd
     export FORT74=$PARMpetss/model/mdl_ettgp.ms7_2nd
     export FORT75=$PARMpetss/model/mdl_ettgp.lf2_2nd
     export FORT76=$PARMpetss/model/mdl_ettgp.ebp3_2nd
     export FORT77=$PARMpetss/model/mdl_ettgp.egl3_2nd
     export FORT78=$PARMpetss/model/mdl_ettgp.ps2_2nd
     export FORT79=$PARMpetss/model/mdl_ettgp.cr3_2nd
     export FORT80=$PARMpetss/model/mdl_ettgp.ebr3_2nd
     export FORT81=$PARMpetss/model/mdl_ettgp.eke2_2nd

     startmsg
     $EXECpetss/petss_out_stn >> $pgmout 2> errfile_${RANKO}
     export err=$?; err_chk
  fi
  echo "`date`:  Rank ${RANK} Finished extracting text answers for ${bsn}" >> timing.txt
 done # surge and surge+tide loop
  echo "`date`: Finished transmitting text answers for ${bsn}" >> timing.txt

done      #  Basin loop


fi

fi
echo "`date`: Rank ${RANKO} Finished." >> timing.txt

############## END OF SCRIPT #######################

