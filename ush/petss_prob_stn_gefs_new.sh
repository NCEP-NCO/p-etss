#!/bin/bash
#
# 
# Huiqing.Liu/MDL--- 04/23/2019 -------PETSS Post_SCRIPT for station--------
#
# "HISTORY: April 23, 2019 - First Version of PETSS1.3"
# " ---------------------------------------------------------"
#
# Author: Huiqing.Liu (huiqing.liu@noaa.gov)
# Abstract: 
#  Post process station results (statistic post processing).
# Parameters:
#  Inputs:  CPUs number and Datum
#          
#  Outputs: Max, min, mean,90%,10% station output in est,gom,wst,ber,goa
#############################################################################################
set -x
    for bsn in ber goa wst gom est; do
    for fle_extf in stormsurge stormtide; do

     . prep_step

     export pgm="petss_post_sort"
     
     export FORT110=etss.gec00.t${cyc}z.${fle_extf}.${bsn}.txt
     export FORT111=etss.gep01.t${cyc}z.${fle_extf}.${bsn}.txt
     export FORT112=etss.gep02.t${cyc}z.${fle_extf}.${bsn}.txt
     export FORT113=etss.gep03.t${cyc}z.${fle_extf}.${bsn}.txt
     export FORT114=etss.gep04.t${cyc}z.${fle_extf}.${bsn}.txt
     export FORT115=etss.gep05.t${cyc}z.${fle_extf}.${bsn}.txt
     export FORT116=etss.gep06.t${cyc}z.${fle_extf}.${bsn}.txt
     export FORT117=etss.gep07.t${cyc}z.${fle_extf}.${bsn}.txt
     export FORT118=etss.gep08.t${cyc}z.${fle_extf}.${bsn}.txt
     export FORT119=etss.gep09.t${cyc}z.${fle_extf}.${bsn}.txt
     export FORT120=etss.gep10.t${cyc}z.${fle_extf}.${bsn}.txt
     export FORT121=etss.gep11.t${cyc}z.${fle_extf}.${bsn}.txt
     export FORT122=etss.gep12.t${cyc}z.${fle_extf}.${bsn}.txt
     export FORT123=etss.gep13.t${cyc}z.${fle_extf}.${bsn}.txt
     export FORT124=etss.gep14.t${cyc}z.${fle_extf}.${bsn}.txt
     export FORT125=etss.gep15.t${cyc}z.${fle_extf}.${bsn}.txt
     export FORT126=etss.gep16.t${cyc}z.${fle_extf}.${bsn}.txt
     export FORT127=etss.gep17.t${cyc}z.${fle_extf}.${bsn}.txt
     export FORT128=etss.gep18.t${cyc}z.${fle_extf}.${bsn}.txt
     export FORT129=etss.gep19.t${cyc}z.${fle_extf}.${bsn}.txt
     export FORT130=etss.gep20.t${cyc}z.${fle_extf}.${bsn}.txt
     export FORT131=etss.gep21.t${cyc}z.${fle_extf}.${bsn}.txt
     export FORT132=etss.gep22.t${cyc}z.${fle_extf}.${bsn}.txt
     export FORT133=etss.gep23.t${cyc}z.${fle_extf}.${bsn}.txt
     export FORT134=etss.gep24.t${cyc}z.${fle_extf}.${bsn}.txt
     export FORT135=etss.gep25.t${cyc}z.${fle_extf}.${bsn}.txt
     export FORT136=etss.gep26.t${cyc}z.${fle_extf}.${bsn}.txt
     export FORT137=etss.gep27.t${cyc}z.${fle_extf}.${bsn}.txt
     export FORT138=etss.gep28.t${cyc}z.${fle_extf}.${bsn}.txt
     export FORT139=etss.gep29.t${cyc}z.${fle_extf}.${bsn}.txt
     export FORT140=etss.gep30.t${cyc}z.${fle_extf}.${bsn}.txt

     export FORT31=${PARMpetss}/model/n_sta_${bsn}

     echo stn_gefsn ${cyc} > post_sort_stn_gefs
     export FORT49=post_sort_stn_gefs

     export FORT61=petss.t${cyc}z.max_gefs.${fle_extf}.${bsn}.txt
     export FORT62=petss.t${cyc}z.min_gefs.${fle_extf}.${bsn}.txt
     export FORT63=petss.t${cyc}z.mean_gefs.${fle_extf}.${bsn}.txt
     export FORT64=petss.t${cyc}z.e10_gefs.${fle_extf}.${bsn}.txt
     export FORT65=petss.t${cyc}z.e90_gefs.${fle_extf}.${bsn}.txt
     export FORT66=petss.t${cyc}z.e20_gefs.${fle_extf}.${bsn}.txt
     export FORT67=petss.t${cyc}z.e30_gefs.${fle_extf}.${bsn}.txt
     export FORT68=petss.t${cyc}z.e40_gefs.${fle_extf}.${bsn}.txt
     export FORT69=petss.t${cyc}z.e50_gefs.${fle_extf}.${bsn}.txt
     export FORT70=petss.t${cyc}z.e60_gefs.${fle_extf}.${bsn}.txt
     export FORT71=petss.t${cyc}z.e70_gefs.${fle_extf}.${bsn}.txt
     export FORT72=petss.t${cyc}z.e80_gefs.${fle_extf}.${bsn}.txt

     startmsg
     $EXECpetss/petss_post_sort >> $pgmout 2> errfile_${RANKO}
     export err=$?; err_chk

    done
    done
   
    exit
