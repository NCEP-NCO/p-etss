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

#--------------------------------------------------------------
# Generating PETSS grid products for CONUS (2p5km and 625m)
#--------------------------------------------------------------
#  ppb=('10p' 'max' 'mean' 'min' '1ft.chance' '2ft.chance' '3ft.chance' '6ft.chance' '10ft.chance')

if [ ${RANKO} -lt 4 ] ; then

   ${USHpetss}/petss_prob_merge.sh $RANKO 10p con 1
   echo "RANK ${RANKO} finished " >> msn_merge_prod.txt

elif [ ${RANKO} -lt 8 ]; then

   RK=$((RANKO-4))
   ${USHpetss}/petss_prob_merge.sh $RK 20p con 1
   echo "RANK ${RANKO} finished " >> msn_merge_prod.txt   

elif [ ${RANKO} -lt 10 ]; then

   RK=$((RANKO-8))
   ${USHpetss}/petss_prob_merge.sh $RK 10p ala 1

   echo "RANK ${RANKO} finished " >> msn_merge_num2.txt
   m_num2=$(grep "RANK" msn_merge_num2.txt | wc -l)
   while [[ ${m_num2} -ne 2 ]] ; do
       echo "`date`: Rank ${RANKO} Sleeping nums msn_merge_num2.txt=${m_num2}" >> timing.txt
       m_num2=$(grep "RANK" msn_merge_num2.txt | wc -l)
       sleep 1
   done

   ${USHpetss}/petss_prob_merge.sh $RK 20p ala 1
   echo "RANK ${RANKO} finished " >> msn_merge_prod.txt

elif [ ${RANKO} -lt 12 ]; then

   RK=$((RANKO-10))
   ${USHpetss}/petss_prob_merge.sh $RK 30p ala 1

   echo "RANK ${RANKO} finished " >> msn_merge_num1.txt
   m_num1=$(grep "RANK" msn_merge_num1.txt | wc -l)
   while [[ ${m_num1} -ne 2 ]] ; do
       echo "`date`: Rank ${RANKO} Sleeping nums msn_merge_num1.txt=${m_num1}" >> timing.txt
       m_num1=$(grep "RANK" msn_merge_num1.txt | wc -l)
       sleep 1 
   done

   ${USHpetss}/petss_prob_merge.sh $RK 40p ala 1
   echo "RANK ${RANKO} finished " >> msn_merge_prod.txt

elif [ ${RANKO} -lt 24 ]; then

   rm *ge*fore*

elif [ ${RANKO} -lt 28 ]; then

   RK=$((RANKO-24))
   ${USHpetss}/petss_prob_merge.sh $RK 30p con 1
   echo "RANK ${RANKO} finished " >> msn_merge_prod.txt

elif [ ${RANKO} -lt 32 ]; then

   RK=$((RANKO-28))
   ${USHpetss}/petss_prob_merge.sh $RK 40p con 1
   echo "RANK ${RANKO} finished " >> msn_merge_prod.txt

elif [ ${RANKO} -lt 34 ]; then

   RK=$((RANKO-32))
   ${USHpetss}/petss_prob_merge.sh $RK 50p ala 1

   echo "RANK ${RANKO} finished " >> msn_merge_num3.txt
   m_num3=$(grep "RANK" msn_merge_num3.txt | wc -l)
   while [[ ${m_num3} -ne 2 ]] ; do
       echo "`date`: Rank ${RANKO} Sleeping nums msn_merge_num3.txt=${m_num3}" >> timing.txt
       m_num3=$(grep "RANK" msn_merge_num3.txt | wc -l)
       sleep 1
   done

   ${USHpetss}/petss_prob_merge.sh $RK 90p ala 1
   echo "RANK ${RANKO} finished " >> msn_merge_prod.txt

elif [ ${RANKO} -lt 36 ]; then

   RK=$((RANKO-34))
   ${USHpetss}/petss_prob_merge.sh $RK 0ft.chance ala 1

   echo "RANK ${RANKO} finished " >> msn_merge_num4.txt
   m_num4=$(grep "RANK" msn_merge_num4.txt | wc -l)
   while [[ ${m_num4} -ne 2 ]] ; do
       echo "`date`: Rank ${RANKO} Sleeping nums msn_merge_num4.txt=${m_num4}" >> timing.txt
       m_num4=$(grep "RANK" msn_merge_num4.txt | wc -l)
       sleep 1
   done

   ${USHpetss}/petss_prob_merge.sh $RK 1ft.chance ala 1
   echo "RANK ${RANKO} finished " >> msn_merge_prod.txt

elif [ ${RANKO} -lt 48 ]; then

   rm *.env *ge*hind*

elif [ ${RANKO} -lt 52 ]; then

   RK=$((RANKO-48))
   ${USHpetss}/petss_prob_merge.sh $RK 50p con 1
   echo "RANK ${RANKO} finished " >> msn_merge_prod.txt

elif [ ${RANKO} -lt 56 ]; then

   RK=$((RANKO-52))
   ${USHpetss}/petss_prob_merge.sh $RK 90p con 1
   echo "RANK ${RANKO} finished " >> msn_merge_prod.txt

elif [ ${RANKO} -lt 58 ]; then

   RK=$((RANKO-56))
   ${USHpetss}/petss_prob_merge.sh $RK 2ft.chance ala 1

   echo "RANK ${RANKO} finished " >> msn_merge_num5.txt
   m_num5=$(grep "RANK" msn_merge_num5.txt | wc -l)
   while [[ ${m_num5} -ne 2 ]] ; do
       echo "`date`: Rank ${RANKO} Sleeping nums msn_merge_num5.txt=${m_num5}" >> timing.txt
       m_num5=$(grep "RANK" msn_merge_num5.txt | wc -l)
       sleep 1
   done

   ${USHpetss}/petss_prob_merge.sh $RK 3ft.chance ala 1
   echo "RANK ${RANKO} finished " >> msn_merge_prod.txt

elif [ ${RANKO} -lt 60 ]; then
 
   RK=$((RANKO-58))
   ${USHpetss}/petss_prob_merge.sh $RK 4ft.chance ala 1
 
   echo "RANK ${RANKO} finished " >> msn_merge_num6.txt
   m_num6=$(grep "RANK" msn_merge_num6.txt | wc -l)
   while [[ ${m_num6} -ne 2 ]] ; do
       echo "`date`: Rank ${RANKO} Sleeping nums msn_merge_num6.txt=${m_num6}" >> timing.txt
       m_num6=$(grep "RANK" msn_merge_num6.txt | wc -l)
       sleep 1
   done

   ${USHpetss}/petss_prob_merge.sh $RK 5ft.chance ala 1
   echo "RANK ${RANKO} finished " >> msn_merge_prod.txt

elif [ ${RANKO} -lt 72 ]; then

   rm *.ge*.rex wndsmod*

elif [ ${RANKO} -lt 76 ]; then

   RK=$((RANKO-72))
   ${USHpetss}/petss_prob_merge.sh $RK 0ft.chance con 1

   echo "RANK ${RANKO} finished " >> msn_merge_prod.txt

elif [ ${RANKO} -lt 80 ]; then

   RK=$((RANKO-76))
   ${USHpetss}/petss_prob_merge.sh $RK 1ft.chance con 1
   echo "RANK ${RANKO} finished " >> msn_merge_prod.txt

elif [ ${RANKO} -lt 82 ]; then
 
   RK=$((RANKO-80))
   ${USHpetss}/petss_prob_merge.sh $RK 6ft.chance ala 1
 
   echo "RANK ${RANKO} finished " >> msn_merge_num7.txt
   m_num7=$(grep "RANK" msn_merge_num7.txt | wc -l)
   while [[ ${m_num7} -ne 2 ]] ; do
       echo "`date`: Rank ${RANKO} Sleeping nums msn_merge_num7.txt=${m_num7}" >> timing.txt
       m_num7=$(grep "RANK" msn_merge_num7.txt | wc -l)
       sleep 1
   done

   ${USHpetss}/petss_prob_merge.sh $RK 7ft.chance ala 1
   echo "RANK ${RANKO} finished " >> msn_merge_prod.txt

elif [ ${RANKO} -lt 84 ]; then

   RK=$((RANKO-82))
   ${USHpetss}/petss_prob_merge.sh $RK 8ft.chance ala 1

   echo "RANK ${RANKO} finished " >> msn_merge_num8.txt
   m_num8=$(grep "RANK" msn_merge_num8.txt | wc -l)
   while [[ ${m_num8} -ne 2 ]] ; do
       echo "`date`: Rank ${RANKO} Sleeping nums msn_merge_num8.txt=${m_num8}" >> timing.txt
       m_num8=$(grep "RANK" msn_merge_num8.txt | wc -l)
       sleep 1
   done

   ${USHpetss}/petss_prob_merge.sh $RK 9ft.chance ala 1
   echo "RANK ${RANKO} finished " >> msn_merge_prod.txt

elif [ ${RANKO} -lt 96 ]; then

    rm *cmc*.rex *mdlsurge.*ge*

elif [ ${RANKO} -lt 100 ]; then

   RK=$((RANKO-96))
   ${USHpetss}/petss_prob_merge.sh $RK 2ft.chance con 1
   echo "RANK ${RANKO} finished " >> msn_merge_prod.txt

elif [ ${RANKO} -lt 104 ]; then

   RK=$((RANKO-100))
   ${USHpetss}/petss_prob_merge.sh $RK 3ft.chance con 1
   echo "RANK ${RANKO} finished " >> msn_merge_prod.txt

elif [ ${RANKO} -lt 106 ]; then

   RK=$((RANKO-104))
   ${USHpetss}/petss_prob_merge.sh $RK 10ft.chance ala 1

   echo "RANK ${RANKO} finished " >> msn_merge_num9.txt
   m_num9=$(grep "RANK" msn_merge_num9.txt | wc -l)
   while [[ ${m_num9} -ne 2 ]] ; do
       echo "`date`: Rank ${RANKO} Sleeping nums msn_merge_num9.txt=${m_num9}" >> timing.txt
       m_num9=$(grep "RANK" msn_merge_num9.txt | wc -l)
       sleep 1
   done

   ${USHpetss}/petss_prob_merge.sh $RK 13ft.chance ala 1
   echo "RANK ${RANKO} finished " >> msn_merge_prod.txt

elif [ ${RANKO} -lt 108 ]; then

   RK=$((RANKO-106))
   ${USHpetss}/petss_prob_merge.sh $RK 15ft.chance ala 1

   echo "RANK ${RANKO} finished " >> msn_merge_num10.txt
   m_num10=$(grep "RANK" msn_merge_num10.txt | wc -l)
   while [[ ${m_num10} -ne 2 ]] ; do
       echo "`date`: Rank ${RANKO} Sleeping nums msn_merge_num10.txt=${m_num10}" >> timing.txt
       m_num10=$(grep "RANK" msn_merge_num10.txt | wc -l)
       sleep 1
   done

   ${USHpetss}/petss_prob_merge.sh $RK 16ft.chance ala 1
   echo "RANK ${RANKO} finished " >> msn_merge_prod.txt

elif [ ${RANKO} -lt 120 ]; then

   rm ssgrid.ge* out_nesting* ssgrid.agl.ge*

elif [ ${RANKO} -lt 124 ]; then

   RK=$((RANKO-120))
   ${USHpetss}/petss_prob_merge.sh $RK 4ft.chance con 1
   echo "RANK ${RANKO} finished " >> msn_merge_prod.txt

elif [ ${RANKO} -lt 128 ]; then

    RK=$((RANKO-124))
    ${USHpetss}/petss_prob_merge.sh $RK 5ft.chance con 1 
   echo "RANK ${RANKO} finished " >> msn_merge_prod.txt

elif [ ${RANKO} -lt 130 ]; then

   RK=$((RANKO-128))
   ${USHpetss}/petss_prob_merge.sh $RK max ala 1

   echo "RANK ${RANKO} finished " >> msn_merge_num11.txt
   m_num11=$(grep "RANK" msn_merge_num11.txt | wc -l)
   while [[ ${m_num11} -ne 2 ]] ; do
       echo "`date`: Rank ${RANKO} Sleeping nums msn_merge_num11.txt=${m_num11}" >> timing.txt
       m_num11=$(grep "RANK" msn_merge_num11.txt | wc -l)
       sleep 1
   done

   ${USHpetss}/petss_prob_merge.sh $RK mean ala 1
   echo "RANK ${RANKO} finished " >> msn_merge_prod.txt

elif [ ${RANKO} -lt 132 ]; then

   RK=$((RANKO-130))
   ${USHpetss}/petss_prob_merge.sh $RK min ala 1
   echo "RANK ${RANKO} finished " >> msn_merge_prod.txt

elif [ ${RANKO} -lt 144 ]; then

   rm ssgrid.cmc* ssgrid.agl.cmc*

elif [ ${RANKO} -lt 148 ]; then

   RK=$((RANKO-144))
   ${USHpetss}/petss_prob_merge.sh $RK 6ft.chance con 1
   echo "RANK ${RANKO} finished " >> msn_merge_prod.txt

elif [ ${RANKO} -lt 152 ]; then

   RK=$((RANKO-148))
   ${USHpetss}/petss_prob_merge.sh $RK 7ft.chance con 1
   echo "RANK ${RANKO} finished " >> msn_merge_prod.txt

elif [ ${RANKO} -lt 168 ]; then

   rm fle20* fle10* fle70* sds*

elif [ ${RANKO} -lt 172 ]; then

   RK=$((RANKO-168))
   ${USHpetss}/petss_prob_merge.sh $RK 8ft.chance con 1
   echo "RANK ${RANKO} finished " >> msn_merge_prod.txt

elif [ ${RANKO} -lt 176 ]; then

   RK=$((RANKO-172))
   ${USHpetss}/petss_prob_merge.sh $RK 9ft.chance con 1
   echo "RANK ${RANKO} finished " >> msn_merge_prod.txt

elif [ ${RANKO} -lt 192 ]; then

     echo rm gfspuv*

elif [ ${RANKO} -lt 196 ]; then

   RK=$((RANKO-192))
   ${USHpetss}/petss_prob_merge.sh $RK 10ft.chance con 1
   echo "RANK ${RANKO} finished " >> msn_merge_prod.txt

elif [ ${RANKO} -lt 200 ]; then

   RK=$((RANKO-196))
   ${USHpetss}/petss_prob_merge.sh $RK 13ft.chance con 1
   echo "RANK ${RANKO} finished " >> msn_merge_prod.txt

elif [ ${RANKO} -lt 216 ]; then
   
   echo #rm cylf10*

elif [ ${RANKO} -lt 220 ]; then

   RK=$((RANKO-216))
   ${USHpetss}/petss_prob_merge.sh $RK 15ft.chance con 1
   echo "RANK ${RANKO} finished " >> msn_merge_prod.txt


elif [ ${RANKO} -lt 224 ]; then
 
   RK=$((RANKO-220))
   ${USHpetss}/petss_prob_merge.sh $RK 16ft.chance con 1
   echo "RANK ${RANKO} finished " >> msn_merge_prod.txt

elif [ ${RANKO} -lt 240 ]; then

   rm fle40* 

elif [ ${RANKO} -lt 244 ]; then

   RK=$((RANKO-240))
   ${USHpetss}/petss_prob_merge.sh $RK max con 1
   echo "RANK ${RANKO} finished " >> msn_merge_prod.txt

elif [ ${RANKO} -lt 248 ]; then

   RK=$((RANKO-244))
   ${USHpetss}/petss_prob_merge.sh $RK mean con 1
   echo "RANK ${RANKO} finished " >> msn_merge_prod.txt

elif [ ${RANKO} -lt 264 ]; then

  rm sshistory*

elif [ ${RANKO} -lt 268 ]; then

   RK=$((RANKO-264))
   ${USHpetss}/petss_prob_merge.sh $RK min con 1
   echo "RANK ${RANKO} finished " >> msn_merge_prod.txt

fi



num_line3=$(grep "RANK" msn_merge_prod.txt | wc -l)

while [[ ${num_line3} -ne 116 ]] ; do
  echo "`date`: Rank ${RANKO} Sleeping nums line3=${num_line3}" >> timing.txt
  num_line3=$(grep "RANK" msn_merge_prod.txt | wc -l)
  sleep 1
done


if [[ ${RANKO} == 14 ]]; then   
   if [[ $SENDCOM == 'YES' ]]; then
      ppold=('0ft.chance' '4ft.chance' '5ft.chance' '7ft.chance' '8ft.chance' '9ft.chance' '13ft.chance' '16ft.chance')
      ppnew=('gt0' 'gt4' 'gt5' 'gt7' 'gt8' 'gt9' 'gt13' 'gt16')
      for i in $(seq 0 7); do
          for areares in con2p5km con625m ala3km; do
              cpreq petss.1hr.inc.${ppold[$i]}.t${cyc}z.stormtide.${areares} ${COMOUT}/petss.t${cyc}z.stormtide_${ppnew[$i]}_1hr_inc_dat.${areares}.grib2
              cpreq petss.6hr.cum.${ppold[$i]}.t${cyc}z.stormtide.${areares} ${COMOUT}/petss.t${cyc}z.stormtide_${ppnew[$i]}_6hr_cum_dat.${areares}.grib2

              cpreq petss.agl.6hr.inc.${ppold[$i]}.t${cyc}z.stormtide.${areares} $COMOUT/petss.t${cyc}z.stormtide_${ppnew[$i]}_6hr_inc_agl.${areares}.grib2
              cpreq petss.agl.6hr.cum.${ppold[$i]}.t${cyc}z.stormtide.${areares} $COMOUT/petss.t${cyc}z.stormtide_${ppnew[$i]}_6hr_cum_agl.${areares}.grib2

              if [ $SENDDBN = YES ]; then
                $DBNROOT/bin/dbn_alert MDLFCST PETSS_GB2 $job ${COMOUT}/petss.t${cyc}z.stormtide_${ppnew[$i]}_1hr_inc_dat.${areares}.grib2
                $DBNROOT/bin/dbn_alert MDLFCST PETSS_GB2 $job ${COMOUT}/petss.t${cyc}z.stormtide_${ppnew[$i]}_6hr_cum_dat.${areares}.grib2

                $DBNROOT/bin/dbn_alert MDLFCST PETSS_GB2 $job $COMOUT/petss.t${cyc}z.stormtide_${ppnew[$i]}_6hr_inc_agl.${areares}.grib2
                $DBNROOT/bin/dbn_alert MDLFCST PETSS_GB2 $job $COMOUT/petss.t${cyc}z.stormtide_${ppnew[$i]}_6hr_cum_agl.${areares}.grib2
              fi

          done
      done
    
  #   txtnew=('e10' 'e90' 'max' 'mean' 'min','e20','e30','e40','e50','e60','e70','e80')
      for prodtxt in e10 e90 max mean min; do
          for area in ber est goa gom wst; do
              for fle in stormsurge stormtide; do
                  if [[ -s ${COMOUT}/PETSS_GEFS_${cyc} || ${WIND} == GEFS ]]; then
                     cpreq petss.t${cyc}z.${prodtxt}_gefs.${fle}.${area}.txt $COMOUT/petss.t${cyc}z.${prodtxt}.${fle}.${area}.txt
                  else
                     cpreq petss.t${cyc}z.${prodtxt}_naefs.${fle}.${area}.txt $COMOUT/petss.t${cyc}z.${prodtxt}.${fle}.${area}.txt
                  fi
                  cpreq petss.t${cyc}z.${prodtxt}_gefs.${fle}.${area}.txt $COMOUT/petss.t${cyc}z.${prodtxt}_gefs.${fle}.${area}.txt

                  if [ $SENDDBN = YES ]; then
                     $DBNROOT/bin/dbn_alert MDLFCST PETSS_TXT $job $COMOUT/petss.t${cyc}z.${prodtxt}.${fle}.${area}.txt
                  fi

              done
          done
      done
#   Data of opportunity warning emails
      if [[ -s ${COMOUT}/PETSS_GEFS_${cyc} ||  -s ${COMOUT}/PETSS_NAEFS_${cyc} || -s ${COMOUT}/PETSS_NAEFS1_${cyc} ]]; then
       cat ${COMOUT}/petss.t${cyc}z.meta.txt | mail.py -s P-ETSS-data-of-opportunity-warning -c Huiqing.Liu@noaa.gov
#       rm  ${COMOUT}/PETSS_GEFS_${cyc} ${COMOUT}/PETSS_NAEFS_${cyc} ${COMOUT}/PETSS_NAEFS1_${cyc}
       echo "sending warning email...." > ${COMOUT}/PETSS_warning_${cyc}
      fi

      echo "RANK ${RANKO} finished copying files to /com." >> timing.txt
      echo "RANK ${RANKO} finished copying files to /com." > Done_all_com.txt
   fi
elif [[ ${RANKO} == 15 ]]; then
   if [[ ${HIGHRES} == "YES" ]] ; then
      rm ${COMOUT}/highres/*
      mv ssgrid*max* ${COMOUT}/highres
      mv ssgrid*mean* ${COMOUT}/highres
#      mv ssgrid*min* ${COMOUT}/highres
      mv ssgrid*10p* ${COMOUT}/highres
      mv ssgrid*20p* ${COMOUT}/highres
      mv ssgrid*30p* ${COMOUT}/highres
#      mv ssgrid*40p* ${COMOUT}/highres
      mv ssgrid*50p* ${COMOUT}/highres
#      mv ssgrid*90p* ${COMOUT}/highres
#      mv ssgrid*ft.chance* ${COMOUT}/highres
      mv ssgrid.gec00.tide.* ${COMOUT}/highres
      tar -zcf ${COMOUT}/petss.t${cyc}z.highres.tar.gz ${COMOUT}/highres/*
      rm -rf ${COMOUT}/highres
   fi

   echo 'Initial-Water-Level (ft), Abbrev, Basin' > petss.t${cyc}z.init_wl.txt
   bsn_f=('pn2' 'pv2' 'ny3' 'de3' 'cp5' 'hor3' 'ht3' 'il3' 'hch2' 'esv4' 'ejx3' 'co2' 'pb3' 'hmi3' 'eke2' 'efm2' 'etp3' 'cd2' 'ap3' 'hpa2' 'epn3' 'emo2' 'ms7' 'lf2' 'ebp3' 'egl3' 'ps2' 'cr3' 'ebr3' 'e' 'g' 'n' 'k' 'm')
   basin_f=('Penobscot Bay' 'Providence/Boston' 'New York' 'Delaware Bay' 'Chesapeake Bay' 'Norfolk' 'Pamlico Sound' 'Wilmington/Myrtle Beach' 'Charleston Harbor' 'Savannah/Hilton Head' 'Jacksonville' 'Cape Canaveral' 'Palm Beach' 'Biscayne Bay' 'Florida Bay' 'Fort Myers' 'Tampa Bay' 'Cedar Key' 'Apalachicola Bay' 'Panama City' 'Pensacola Bay' 'Mobile Bay' 'New Orleans' 'Vermilion Bay' 'Sabine lake' 'Galveston Bay' 'Matagorda Bay' 'Corpus Christi Bay' 'Laguna Madre' 'Eastern Coast' 'Gulf of Mexico' 'Western Coast' 'Gulf of Alaska' 'Bering Sea and Arctic')
   for i in $(seq -f "%1g" 0 33); do
       printf -v anom "%7.2f" $(cat wl.gec00.surge.${bsn_f[${i}]})
       echo ${anom}, ${bsn_f[${i}]}, ${basin_f[${i}]} >> petss.t${cyc}z.init_wl.txt
   done
   if [[ $SENDCOM == 'YES' ]]; then
      cp petss.t${cyc}z.init_wl.txt $COMOUT/

      if [ $SENDDBN = YES ]; then
         $DBNROOT/bin/dbn_alert MDLFCST PETSS_TXT $job ${COMOUT}/petss.t${cyc}z.init_wl.txt
         $DBNROOT/bin/dbn_alert MDLFCST PETSS_TXT $job ${COMOUT}/petss.t${cyc}z.meta.txt
      fi

   fi

elif [ $RANKO -lt 14 ]; then
     ppold=('10p' '20p' '30p' '40p' '50p' '90p' '1ft.chance' '2ft.chance' '3ft.chance' '6ft.chance' '10ft.chance' 'max' 'mean' 'min')
     ppnew=('e10' 'e20' 'e30' 'e40' 'e50' 'e90' 'gt1' 'gt2' 'gt3' 'gt6' 'gt10' 'max' 'mean' 'min')
  
     #for i in $(seq 0 13); do
     i=${RANKO}
        for areares in con2p5km con625m ala3km; do

            . prep_step

            export pgm=tocgrib2

            if [ ${areares} != con625m ] && [ $i -ne 1 ] && [ $i -ne 2 ] && [ $i -ne 3 ]
            then

              export FORT11=petss.1hr.inc.${ppold[$i]}.t${cyc}z.stormtide.${areares}
              export FORT31=""
              export FORT51=petss.t${cyc}z.stormtide_${ppnew[$i]}_1hr_inc_dat.${areares}.grib2
           
              startmsg
              ${TOCGRIB2} < $PARMpetss/wmoheader/grib2.t${cyc}z.stormtide_${ppnew[$i]}_1hr_inc_dat.${areares}  >> $pgmout 2> errfile_${RANKO}
              export err=$?; err_chk

              if [[ $SENDCOM == 'YES' ]]; then
                cpreq petss.1hr.inc.${ppold[$i]}.t${cyc}z.stormtide.${areares} ${COMOUT}/petss.t${cyc}z.stormtide_${ppnew[$i]}_1hr_inc_dat.${areares}.grib2
              fi
              cpreq petss.t${cyc}z.stormtide_${ppnew[$i]}_1hr_inc_dat.${areares}.grib2 ${COMOUTwmo}/grib2.petss.t${cyc}z.stormtide_${ppnew[$i]}_1hr_inc_dat.${areares}
              if [ $SENDDBN_NTC = YES ]; then
                $DBNROOT/bin/dbn_alert NTC_LOW ${RUN} $job $COMOUTwmo/grib2.petss.t${cyc}z.stormtide_${ppnew[$i]}_1hr_inc_dat.${areares}
              fi
          
            else

              if [[ $SENDCOM == 'YES' ]]; then
                cpreq petss.1hr.inc.${ppold[$i]}.t${cyc}z.stormtide.${areares} ${COMOUT}/petss.t${cyc}z.stormtide_${ppnew[$i]}_1hr_inc_dat.${areares}.grib2
              fi

            fi

            if [ $SENDDBN = YES ]; then
              $DBNROOT/bin/dbn_alert MDLFCST PETSS_GB2 $job ${COMOUT}/petss.t${cyc}z.stormtide_${ppnew[$i]}_1hr_inc_dat.${areares}.grib2
            fi


            if [ $i -lt 11 ]
            then

              . prep_step

              export FORT11=petss.6hr.cum.${ppold[$i]}.t${cyc}z.stormtide.${areares}
              export FORT31="" 
              export FORT51=petss.t${cyc}z.stormtide_${ppnew[$i]}_6hr_cum_dat.${areares}.grib2
 
              startmsg 
              ${TOCGRIB2} < $PARMpetss/wmoheader/grib2.t${cyc}z.stormtide_${ppnew[$i]}_6hr_cum_dat.${areares}  >> $pgmout 2> errfile_${RANKO}
              export err=$?; err_chk

              . prep_step

              export FORT11=petss.agl.6hr.cum.${ppold[$i]}.t${cyc}z.stormtide.${areares}
              export FORT31=""
              export FORT51=petss.t${cyc}z.stormtide_${ppnew[$i]}_6hr_cum_agl.${areares}.grib2
   
              startmsg   
              ${TOCGRIB2} < $PARMpetss/wmoheader/grib2.t${cyc}z.stormtide_${ppnew[$i]}_6hr_cum_agl.${areares}  >> $pgmout 2> errfile_${RANKO}
              export err=$?; err_chk

              if [[ $SENDCOM == 'YES' ]]; then
                cpreq petss.6hr.cum.${ppold[$i]}.t${cyc}z.stormtide.${areares} ${COMOUT}/petss.t${cyc}z.stormtide_${ppnew[$i]}_6hr_cum_dat.${areares}.grib2
                cpreq petss.agl.6hr.cum.${ppold[$i]}.t${cyc}z.stormtide.${areares} ${COMOUT}/petss.t${cyc}z.stormtide_${ppnew[$i]}_6hr_cum_agl.${areares}.grib2
              fi
              cpreq petss.t${cyc}z.stormtide_${ppnew[$i]}_6hr_cum_dat.${areares}.grib2 ${COMOUTwmo}/grib2.petss.t${cyc}z.stormtide_${ppnew[$i]}_6hr_cum_dat.${areares}
              cpreq petss.t${cyc}z.stormtide_${ppnew[$i]}_6hr_cum_agl.${areares}.grib2 ${COMOUTwmo}/grib2.petss.t${cyc}z.stormtide_${ppnew[$i]}_6hr_cum_agl.${areares}
              if [ $SENDDBN_NTC = YES ]; then
                $DBNROOT/bin/dbn_alert NTC_LOW ${RUN} $job $COMOUTwmo/grib2.petss.t${cyc}z.stormtide_${ppnew[$i]}_6hr_cum_dat.${areares}
                $DBNROOT/bin/dbn_alert NTC_LOW ${RUN} $job $COMOUTwmo/grib2.petss.t${cyc}z.stormtide_${ppnew[$i]}_6hr_cum_agl.${areares}
              fi

            else

              if [[ $SENDCOM == 'YES' ]]; then
                cpreq petss.6hr.cum.${ppold[$i]}.t${cyc}z.stormtide.${areares} ${COMOUT}/petss.t${cyc}z.stormtide_${ppnew[$i]}_6hr_cum_dat.${areares}.grib2
                cpreq petss.agl.6hr.cum.${ppold[$i]}.t${cyc}z.stormtide.${areares} $COMOUT/petss.t${cyc}z.stormtide_${ppnew[$i]}_6hr_cum_agl.${areares}.grib2
              fi
              
            fi           
            if [ $SENDDBN = YES ]; then
               $DBNROOT/bin/dbn_alert MDLFCST PETSS_GB2 $job ${COMOUT}/petss.t${cyc}z.stormtide_${ppnew[$i]}_6hr_cum_dat.${areares}.grib2
               $DBNROOT/bin/dbn_alert MDLFCST PETSS_GB2 $job ${COMOUT}/petss.t${cyc}z.stormtide_${ppnew[$i]}_6hr_cum_agl.${areares}.grib2
            fi

 
            . prep_step

            export FORT11=petss.agl.6hr.inc.${ppold[$i]}.t${cyc}z.stormtide.${areares}
            export FORT31=""  
            export FORT51=petss.t${cyc}z.stormtide_${ppnew[$i]}_6hr_inc_agl.${areares}.grib2
  
            startmsg  
            ${TOCGRIB2} < $PARMpetss/wmoheader/grib2.t${cyc}z.stormtide_${ppnew[$i]}_6hr_inc_agl.${areares}  >> $pgmout 2> errfile_${RANKO}
            export err=$?; err_chk 

            if [[ $SENDCOM == 'YES' ]]; then
              cpreq petss.agl.6hr.inc.${ppold[$i]}.t${cyc}z.stormtide.${areares} ${COMOUT}/petss.t${cyc}z.stormtide_${ppnew[$i]}_6hr_inc_agl.${areares}.grib2
            fi
            if [ $SENDDBN = YES ]; then
              $DBNROOT/bin/dbn_alert MDLFCST PETSS_GB2 $job $COMOUT/petss.t${cyc}z.stormtide_${ppnew[$i]}_6hr_inc_agl.${areares}.grib2
            fi
            cpreq petss.t${cyc}z.stormtide_${ppnew[$i]}_6hr_inc_agl.${areares}.grib2 $COMOUTwmo/grib2.petss.t${cyc}z.stormtide_${ppnew[$i]}_6hr_inc_agl.${areares}
            if [ $SENDDBN_NTC = YES ]; then
              $DBNROOT/bin/dbn_alert NTC_LOW ${RUN} $job $COMOUTwmo/grib2.petss.t${cyc}z.stormtide_${ppnew[$i]}_6hr_inc_agl.${areares}
            fi
 
        done
    #done
    echo "RANK ${RANKO} finished copying files to /pcom." > Done_all_pcom.txt
fi

while [[ ! -f Done_all_pcom.txt || ! -f Done_all_com.txt ]] ; do
  echo "RANK ${RANKO} is waiting for copying files to /com and /pcom" >> timing.txt
  sleep 1
done

echo "`date`: Rank ${RANKO} Finished." >> timing.txt

############## END OF SCRIPT #######################
