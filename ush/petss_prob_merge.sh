#!/bin/bash
#
# 
# Huiqing.Liu/MDL--- 03/08/2016 -------PETSS Post_SCRIPT ---------
#
# "HISTORY: March 08, 2016 - First Version of PETSS1.0"
# " ---------------------------------------------------------"
#
# Author: Huiqing.Liu (huiqing.liu@noaa.gov)
# Abstract: 
# Generating Probabilistic CONUS/ALA grid products  
#  Inputs:  CPUs number, Datum and Area (con/ala)
#          
#  Outputs: Exceedance Height and Probability of each basin grid
#############################################################################################
set -x

if [ $# -ne 4 ] ; then
  echo "This exec's the PETSS model on Tide."
  echo ""
  echo "Usage $0 <RANK><Prob,10p/max/mean/min><Area, con/ala>"
  echo ""
  exit
fi


   RK=$1
   probp=$2
   area=$3
   sect=$4
if [[ ${area} == 'con' ]]; then

   . prep_step

   export pgm="petss_out_grid"
   fle_ext=surge_tide
   fle_extf=stormtide

   if [[ ${RK} == 0 ]] ; then
      area1=con1
      NESTYN=Y
      gridhighres=2p5km
      DTM=''
      DTMM='DAT'
      
      echo $area $fle_extf $NESTYN ${probp} ${DTMM} 1 103 > $DATA/${probp}.control.${area1}.txt
      export FORT11=$DATA/${probp}.control.${area1}.txt
      if [[ ${sect} == '1' ]]; then
         export FORT53=petss${DTM}.1hr.inc.${probp}.${cycle}.${fle_extf}.${area}${gridhighres}
         export FORT54=petss${DTM}.6hr.cum.${probp}.${cycle}.${fle_extf}.${area}${gridhighres}
         echo "Test" ${sect} >> timing.tst.txt
      elif [[ ${sect} == '2' ]]; then
         echo "Test" ${sect} >> timing.tst.txt
         export FORT53=petss2${DTM}.1hr.inc.${probp}.${cycle}.${fle_extf}.${area}${gridhighres}
         export FORT54=petss2${DTM}.6hr.cum.${probp}.${cycle}.${fle_extf}.${area}${gridhighres}
      fi
      export FORT49=$PARMpetss/mergemask/mdl_etconus_etss2.1.bin
   elif [[ ${RK} == 1 ]] ; then
      area1=con2
      NESTYN=Y
      gridhighres=2p5km
      DTM='.agl'
      DTMM='AGL'
      echo $area $fle_extf $NESTYN ${probp} ${DTMM} 1 103 > $DATA/${probp}.control.${area1}.txt
      export FORT11=$DATA/${probp}.control.${area1}.txt
      if [[ ${sect} == '1' ]]; then
         export FORT53=petss${DTM}.6hr.inc.${probp}.${cycle}.${fle_extf}.${area}${gridhighres}
         export FORT54=petss${DTM}.6hr.cum.${probp}.${cycle}.${fle_extf}.${area}${gridhighres}
      elif [[ ${sect} == '2' ]]; then
         export FORT53=petss2${DTM}.6hr.inc.${probp}.${cycle}.${fle_extf}.${area}${gridhighres}
         export FORT54=petss2${DTM}.6hr.cum.${probp}.${cycle}.${fle_extf}.${area}${gridhighres}
      fi
      export FORT49=$PARMpetss/mergemask/mdl_etconus_etss2.1.bin
   elif [[ ${RK} == 2 ]] ; then
      NESTYN=H
      gridhighres=625m
      DTM=''
      DTMM='DAT'
      if [[ ${sect} == '1' ]]; then
         area1=con3
         echo $area $fle_extf $NESTYN ${probp} ${DTMM} 1 103 > $DATA/${probp}.control.${area1}.txt
         export FORT11=$DATA/${probp}.control.${area1}.txt

         export FORT53=petss${DTM}.1hr.inc.${probp}.${cycle}.${fle_extf}.${area}${gridhighres}
         export FORT54=petss${DTM}.6hr.cum.${probp}.${cycle}.${fle_extf}.${area}${gridhighres}
      elif [[ ${sect} == '2' ]]; then
         area1=con5
         echo $area $fle_extf $NESTYN ${probp} ${DTMM} 100 103 > $DATA/${probp}.control.${area1}.txt
         export FORT11=$DATA/${probp}.control.${area1}.txt

         export FORT53=petss${DTM}2.1hr.inc.${probp}.${cycle}.${fle_extf}.${area}${gridhighres}
         export FORT54=petss${DTM}2.6hr.cum.${probp}.${cycle}.${fle_extf}.${area}${gridhighres}
      fi
      export FORT49=$PARMpetss/mergemask/mdl_etconus_etss2.1_625m.bin
   elif [[ ${RK} == 3 ]] ; then
      NESTYN=H
      gridhighres=625m
      DTM='.agl'
      DTMM='AGL'
      if [[ ${sect} == '1' ]]; then
         area1=con4
         echo $area $fle_extf $NESTYN ${probp} ${DTMM} 1 103 > $DATA/${probp}.control.${area1}.txt
         export FORT11=$DATA/${probp}.control.${area1}.txt

         export FORT53=petss${DTM}.6hr.inc.${probp}.${cycle}.${fle_extf}.${area}${gridhighres}
         export FORT54=petss${DTM}.6hr.cum.${probp}.${cycle}.${fle_extf}.${area}${gridhighres}
      elif [[ ${sect} == '2' ]]; then
         area1=con6
         echo $area $fle_extf $NESTYN ${probp} ${DTMM} 100 103 > $DATA/${probp}.control.${area1}.txt
         export FORT11=$DATA/${probp}.control.${area1}.txt

         export FORT53=petss${DTM}2.6hr.inc.${probp}.${cycle}.${fle_extf}.${area}${gridhighres}
         export FORT54=petss${DTM}2.6hr.cum.${probp}.${cycle}.${fle_extf}.${area}${gridhighres}
      fi
      export FORT49=$PARMpetss/mergemask/mdl_etconus_etss2.1_625m.bin
   fi

   export FORT10=$PARMpetss/mergemask/mask_f.txt

   export FORT13=ssgrid${DTM}.${probp}.${fle_ext}.${cyc}e
   export FORT14=ssgrid${DTM}.${probp}.${fle_ext}.${cyc}g
   echo "`date`: Rank ${RANKO} gridmerge CONUS ${gridhighres} using nesting tropical basins for ${probp} ${fle_extf} run" >> timing.txt

   ##########set up date info... PDY is today, PDYm1 one day ago... etc.
   echo $PDY $cyc > $DATA/${probp}.datetime.${area1}.txt
   export FORT48=$DATA/${probp}.datetime.${area1}.txt
   ##########write area and cycle into control file

   export FORT12=$PARMpetss/mergemask/mdl_etgrids.${area}

   export FORT15=ssgrid${DTM}.${probp}.${fle_ext}.${cyc}n
   export FORT16=ssgrid${DTM}.${probp}.${fle_ext}.${cyc}pn2
   export FORT17=ssgrid${DTM}.${probp}.${fle_ext}.${cyc}pv2
   export FORT18=ssgrid${DTM}.${probp}.${fle_ext}.${cyc}ny3
   export FORT19=ssgrid${DTM}.${probp}.${fle_ext}.${cyc}de3
   export FORT20=ssgrid${DTM}.${probp}.${fle_ext}.${cyc}cp5
   export FORT21=ssgrid${DTM}.${probp}.${fle_ext}.${cyc}hor3
   export FORT22=ssgrid${DTM}.${probp}.${fle_ext}.${cyc}ht3
   export FORT23=ssgrid${DTM}.${probp}.${fle_ext}.${cyc}il3
   export FORT24=ssgrid${DTM}.${probp}.${fle_ext}.${cyc}hch2
   export FORT25=ssgrid${DTM}.${probp}.${fle_ext}.${cyc}esv4
   export FORT26=ssgrid${DTM}.${probp}.${fle_ext}.${cyc}ejx3
   export FORT27=ssgrid${DTM}.${probp}.${fle_ext}.${cyc}co2
   export FORT28=ssgrid${DTM}.${probp}.${fle_ext}.${cyc}pb3
   export FORT29=ssgrid${DTM}.${probp}.${fle_ext}.${cyc}hmi3
   export FORT30=ssgrid${DTM}.${probp}.${fle_ext}.${cyc}eke2
   export FORT31=ssgrid${DTM}.${probp}.${fle_ext}.${cyc}efm2
   export FORT32=ssgrid${DTM}.${probp}.${fle_ext}.${cyc}etp3
   export FORT33=ssgrid${DTM}.${probp}.${fle_ext}.${cyc}cd2
   export FORT34=ssgrid${DTM}.${probp}.${fle_ext}.${cyc}ap3
   export FORT35=ssgrid${DTM}.${probp}.${fle_ext}.${cyc}hpa2
   export FORT36=ssgrid${DTM}.${probp}.${fle_ext}.${cyc}epn3
   export FORT37=ssgrid${DTM}.${probp}.${fle_ext}.${cyc}emo2
   export FORT38=ssgrid${DTM}.${probp}.${fle_ext}.${cyc}ms7
   export FORT39=ssgrid${DTM}.${probp}.${fle_ext}.${cyc}lf2
   export FORT40=ssgrid${DTM}.${probp}.${fle_ext}.${cyc}ebp3
   export FORT41=ssgrid${DTM}.${probp}.${fle_ext}.${cyc}egl3
   export FORT42=ssgrid${DTM}.${probp}.${fle_ext}.${cyc}ps2
   export FORT43=ssgrid${DTM}.${probp}.${fle_ext}.${cyc}cr3
   export FORT44=ssgrid${DTM}.${probp}.${fle_ext}.${cyc}ebr3
   if [[ ${sect} == '1' ]]; then
      startmsg
      $EXECpetss/petss_out_grid >> $pgmout 2> errfile_${RANKO}
      err=$?;export err; err_chk
   fi
   echo "`date`: ${RANKO} Finished Creating GRIB message for ${DTM} ${probp} ${gridhighres} ${area} run" >> timing.txt
   echo "RANK Merge CONUS is complete" > msg_Done_newCONUS${DTM}_${probp}_${gridhighres}.txt

elif [[ ${area} == 'ala' ]]; then
  
   . prep_step

   export pgm="petss_out_grid"
   NESTYN=N
   fle_ext=surge_tide
   fle_extf=stormtide
   gridhighres=3km

   if [[ ${RK} == 0 ]] ; then
   
      area1=aladat
      DTM=''
      DTMM='DAT'
      export FORT53=petss${DTM}.1hr.inc.${probp}.${cycle}.${fle_extf}.${area}${gridhighres}

   elif [[ ${RK} == 1 ]] ; then

      area1=alaagl
      DTM='.agl'
      DTMM='AGL'
      export FORT53=petss${DTM}.6hr.inc.${probp}.${cycle}.${fle_extf}.${area}${gridhighres}

   fi

   ##########set up date info... PDY is today, PDYm1 one day ago... etc.
   echo $PDY $cyc > $DATA/${probp}${DTM}.datetime.${area1}.txt
   export FORT17=$DATA/${probp}${DTM}.datetime.${area1}.txt
   ##########write area and cycle into control file
   echo $area $fle_extf $NESTYN ${probp} ${DTMM} 1 103 > $DATA/${probp}${DTM}.control.${area1}.txt
   export FORT11=$DATA/${probp}${DTM}.control.${area1}.txt

   export FORT12=$PARMpetss/mergemask/mdl_etgrids.${area}
   export FORT13=ssgrid${DTM}.${probp}.${fle_ext}.${cyc}m
   export FORT14=ssgrid${DTM}.${probp}.${fle_ext}.${cyc}k
   export FORT16=$PARMpetss/mergemask/mdl_etalaska_etss2.1.bin

   export FORT54=petss${DTM}.6hr.cum.${probp}.${cycle}.${fle_extf}.${area}${gridhighres}
   startmsg
   $EXECpetss/petss_out_grid >> $pgmout 2> errfile_${RANKO}
   err=$?;export err; err_chk
   echo "`date`: ${RANKO} Finished Creating GRIB message for ${probp} ${gridhighres} ${area} ${fle_ext} run" >> timing.txt
   echo "Merge new ALA is complete" > msg_Done_newALA${DTM}_${probp}_${gridhighres}.txt
fi
