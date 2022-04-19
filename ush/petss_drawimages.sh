#!/bin/bash

# ###########################################################################################
# Author: Huiqing Liu (Huiqing.Liu@noaa.gov)                                      09-24-2021
#
# Abstract:
#   Creat poe-script and submit the jobs to draw gridded products image
#
# Parameters:
#  Inputs:  Various environmental variables defined either in ecflow or in the
#           j-job jobs/JPETSS_DRAWIMAGES
#           girdded stormtide grib2 files - ${COMIN}/petss.t${cyc}z.stormtide*grib2
#  Outputs: poe-script - list of running scripts for mpiexec
#           - ${COMOUT}/petss.t00z.img.tar.gz
# ###########################################################################################

#############################################################################################
# 09-24-2021: Created by Huiqing Liu
#############################################################################################

msg="Starting job $job"
postmsg "$jlogfile" "$msg"

export RANKO=$1

export shpimg=1

if [[ ${RANKO} == 0 ]] ; then

   ${USHpetss}/petss_shp_img.sh con e10 datum
   ${USHpetss}/petss_shp_img_web_wst.sh e10 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png.txt

elif [[ ${RANKO} == 1 ]] ; then

   ${USHpetss}/petss_shp_img.sh ala e10 datum
   ${USHpetss}/petss_shp_img_web_ala.sh e10 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png.txt

elif [[ ${RANKO} == 2 ]] ; then

   ${USHpetss}/petss_shp_img_625m_web.sh 1 9 e10 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_10pd.txt

elif [[ ${RANKO} == 3 ]] ; then
 
   ${USHpetss}/petss_shp_img_625m_web.sh 10 19 e10 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_10pd.txt
elif [[ ${RANKO} == 4 ]] ; then
 
   ${USHpetss}/petss_shp_img_625m_web.sh 20 29 e10 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_10pd.txt
elif [[ ${RANKO} == 5 ]] ; then
 
   ${USHpetss}/petss_shp_img_625m_web.sh 30 39 e10 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_10pd.txt
elif [[ ${RANKO} == 6 ]] ; then
 
   ${USHpetss}/petss_shp_img_625m_web.sh 40 49 e10 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_10pd.txt
elif [[ ${RANKO} == 7 ]] ; then
 
   ${USHpetss}/petss_shp_img_625m_web.sh 50 59 e10 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_10pd.txt
elif [[ ${RANKO} == 8 ]] ; then
  
   ${USHpetss}/petss_shp_img_625m_web.sh 60 69 e10 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_10pd.txt
elif [[ ${RANKO} == 9 ]] ; then
  
   ${USHpetss}/petss_shp_img_625m_web.sh 70 79 e10 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_10pd.txt
elif [[ ${RANKO} == 10 ]] ; then
  
   ${USHpetss}/petss_shp_img_625m_web.sh 80 89 e10 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_10pd.txt
elif [[ ${RANKO} == 11 ]] ; then
  
   ${USHpetss}/petss_shp_img_625m_web.sh 90 102 e10 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_10pd.txt

   num_line1=$(grep "RANK" png_10pd.txt | wc -l)

   while [[ $num_line1 -ne 10 ]] ; do
         echo "`date`: Rank ${RANKO} Sleeping nums of lines from png_10pd." >> timing.txt
         num_line1=$(grep "RANK" png_10pd.txt | wc -l)
         sleep 1
   done
   ${USHpetss}/petss_shp_img_web_estglf.sh e10 datum 
   echo "`date`: RANK ${RANKO} finished images generating" >> png.txt
elif [[ ${RANKO} == 12 ]] ; then
  
   ${USHpetss}/petss_shp_img_625m_web.sh 1 9 e10 agl
   echo "`date`: RANK ${RANKO} finished images generating" >> png_10pa.txt
elif [[ ${RANKO} == 13 ]] ; then
   
   ${USHpetss}/petss_shp_img_625m_web.sh 10 17 e10 agl
   echo "`date`: RANK ${RANKO} finished images generating" >> png_10pa.txt

   num_line1=$(grep "RANK" png_10pa.txt | wc -l)

   while [[ $num_line1 -ne 2 ]] ; do
         echo "`date`: Rank ${RANKO} Sleeping nums of lines from png_10pd." >> timing.txt
         num_line1=$(grep "RANK" png_10pa.txt | wc -l)
         sleep 1
   done
   ${USHpetss}/petss_shp_img_web_estglf.sh e10 agl 
   echo "`date`: RANK ${RANKO} finished images generating" >> png.txt

elif [[ ${RANKO} == 14 ]] ; then
 
   ${USHpetss}/petss_shp_img_625m_web.sh 1 9 e20 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_20pd.txt
 
elif [[ ${RANKO} == 15 ]] ; then

   ${USHpetss}/petss_shp_img_625m_web.sh 10 19 e20 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_20pd.txt
elif [[ ${RANKO} == 16 ]] ; then
 
   ${USHpetss}/petss_shp_img_625m_web.sh 20 29 e20 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_20pd.txt
elif [[ ${RANKO} == 17 ]] ; then
 
   ${USHpetss}/petss_shp_img_625m_web.sh 30 39 e20 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_20pd.txt
elif [[ ${RANKO} == 18 ]] ; then
  
   ${USHpetss}/petss_shp_img_625m_web.sh 40 49 e20 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_20pd.txt
elif [[ ${RANKO} == 19 ]] ; then
  
   ${USHpetss}/petss_shp_img_625m_web.sh 50 59 e20 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_20pd.txt
elif [[ ${RANKO} == 20 ]] ; then
 
   ${USHpetss}/petss_shp_img_625m_web.sh 60 69 e20 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_20pd.txt
elif [[ ${RANKO} == 21 ]] ; then
   
   ${USHpetss}/petss_shp_img_625m_web.sh 70 79 e20 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_20pd.txt
elif [[ ${RANKO} == 22 ]] ; then 
   
   ${USHpetss}/petss_shp_img_625m_web.sh 80 89 e20 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_20pd.txt
elif [[ ${RANKO} == 23 ]] ; then
   
   ${USHpetss}/petss_shp_img_625m_web.sh 90 102 e20 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_20pd.txt

   num_line1=$(grep "RANK" png_20pd.txt | wc -l)

   while [[ $num_line1 -ne 10 ]] ; do
         echo "`date`: Rank ${RANKO} Sleeping nums of lines from png_10pd." >> timing.txt
         num_line1=$(grep "RANK" png_20pd.txt | wc -l)
         sleep 1
   done
   ${USHpetss}/petss_shp_img_web_estglf.sh e20 datum 
   echo "`date`: RANK ${RANKO} finished images generating" >> png.txt

elif [[ ${RANKO} == 24 ]] ; then
   
   ${USHpetss}/petss_shp_img_625m_web.sh 1 9 e20 agl
   echo "`date`: RANK ${RANKO} finished images generating" >> png_20pa.txt 
elif [[ ${RANKO} == 25 ]] ; then
 
   ${USHpetss}/petss_shp_img_625m_web.sh 10 17 e20 agl
   echo "`date`: RANK ${RANKO} finished images generating" >> png_20pa.txt

   num_line1=$(grep "RANK" png_20pa.txt | wc -l)

   while [[ $num_line1 -ne 2 ]] ; do
         echo "`date`: Rank ${RANKO} Sleeping nums of lines from png_10pd." >> timing.txt
         num_line1=$(grep "RANK" png_20pa.txt | wc -l)
         sleep 1
   done
   ${USHpetss}/petss_shp_img_web_estglf.sh e20 agl 
   echo "`date`: RANK ${RANKO} finished images generating" >> png.txt

elif [[ ${RANKO} == 26 ]] ; then
 
   ${USHpetss}/petss_shp_img_625m_web.sh 1 9 e30 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_30pd.txt
 
elif [[ ${RANKO} == 27 ]] ; then

   ${USHpetss}/petss_shp_img_625m_web.sh 10 19 e30 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_30pd.txt
elif [[ ${RANKO} == 28 ]] ; then
 
   ${USHpetss}/petss_shp_img_625m_web.sh 20 29 e30 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_30pd.txt
elif [[ ${RANKO} == 29 ]] ; then
 
   ${USHpetss}/petss_shp_img_625m_web.sh 30 39 e30 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_30pd.txt
elif [[ ${RANKO} == 30 ]] ; then
  
   ${USHpetss}/petss_shp_img_625m_web.sh 40 49 e30 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_30pd.txt
elif [[ ${RANKO} == 31 ]] ; then
  
   ${USHpetss}/petss_shp_img_625m_web.sh 50 59 e30 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_30pd.txt
elif [[ ${RANKO} == 32 ]] ; then
 
   ${USHpetss}/petss_shp_img_625m_web.sh 60 69 e30 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_30pd.txt
elif [[ ${RANKO} == 33 ]] ; then
   
   ${USHpetss}/petss_shp_img_625m_web.sh 70 79 e30 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_30pd.txt
elif [[ ${RANKO} == 34 ]] ; then 
   
   ${USHpetss}/petss_shp_img_625m_web.sh 80 89 e30 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_30pd.txt
elif [[ ${RANKO} == 35 ]] ; then
   
   ${USHpetss}/petss_shp_img_625m_web.sh 90 102 e30 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_30pd.txt

   num_line1=$(grep "RANK" png_30pd.txt | wc -l)

   while [[ $num_line1 -ne 10 ]] ; do
         echo "`date`: Rank ${RANKO} Sleeping nums of lines from png_10pd." >> timing.txt
         num_line1=$(grep "RANK" png_30pd.txt | wc -l)
         sleep 1
   done
   ${USHpetss}/petss_shp_img_web_estglf.sh e30 datum 
   echo "`date`: RANK ${RANKO} finished images generating" >> png.txt

elif [[ ${RANKO} == 36 ]] ; then
   
   ${USHpetss}/petss_shp_img_625m_web.sh 1 9 e30 agl
   echo "`date`: RANK ${RANKO} finished images generating" >> png_30pa.txt 
elif [[ ${RANKO} == 37 ]] ; then
 
   ${USHpetss}/petss_shp_img_625m_web.sh 10 17 e30 agl
   echo "`date`: RANK ${RANKO} finished images generating" >> png_30pa.txt

   num_line1=$(grep "RANK" png_30pa.txt | wc -l)

   while [[ $num_line1 -ne 2 ]] ; do
         echo "`date`: Rank ${RANKO} Sleeping nums of lines from png_10pd." >> timing.txt
         num_line1=$(grep "RANK" png_30pa.txt | wc -l)
         sleep 1
   done
   ${USHpetss}/petss_shp_img_web_estglf.sh e30 agl
   echo "`date`: RANK ${RANKO} finished images generating" >> png.txt

elif [[ ${RANKO} == 38 ]] ; then
 
   ${USHpetss}/petss_shp_img_625m_web.sh 1 9 e40 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_40pd.txt
 
elif [[ ${RANKO} == 39 ]] ; then

   ${USHpetss}/petss_shp_img_625m_web.sh 10 19 e40 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_40pd.txt
elif [[ ${RANKO} == 40 ]] ; then
 
   ${USHpetss}/petss_shp_img_625m_web.sh 20 29 e40 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_40pd.txt
elif [[ ${RANKO} == 41 ]] ; then
 
   ${USHpetss}/petss_shp_img_625m_web.sh 30 39 e40 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_40pd.txt
elif [[ ${RANKO} == 42 ]] ; then
  
   ${USHpetss}/petss_shp_img_625m_web.sh 40 49 e40 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_40pd.txt
elif [[ ${RANKO} == 43 ]] ; then
  
   ${USHpetss}/petss_shp_img_625m_web.sh 50 59 e40 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_40pd.txt
elif [[ ${RANKO} == 44 ]] ; then
 
   ${USHpetss}/petss_shp_img_625m_web.sh 60 69 e40 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_40pd.txt
elif [[ ${RANKO} == 45 ]] ; then
   
   ${USHpetss}/petss_shp_img_625m_web.sh 70 79 e40 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_40pd.txt
elif [[ ${RANKO} == 46 ]] ; then 
   
   ${USHpetss}/petss_shp_img_625m_web.sh 80 89 e40 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_40pd.txt
elif [[ ${RANKO} == 47 ]] ; then
   
   ${USHpetss}/petss_shp_img_625m_web.sh 90 102 e40 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_40pd.txt

   num_line1=$(grep "RANK" png_40pd.txt | wc -l)

   while [[ $num_line1 -ne 10 ]] ; do
         echo "`date`: Rank ${RANKO} Sleeping nums of lines from png_10pd." >> timing.txt
         num_line1=$(grep "RANK" png_40pd.txt | wc -l)
         sleep 1
   done
   ${USHpetss}/petss_shp_img_web_estglf.sh e40 datum 
   echo "`date`: RANK ${RANKO} finished images generating" >> png.txt

elif [[ ${RANKO} == 48 ]] ; then
   
   ${USHpetss}/petss_shp_img_625m_web.sh 1 9 e40 agl
   echo "`date`: RANK ${RANKO} finished images generating" >> png_40pa.txt 
elif [[ ${RANKO} == 49 ]] ; then
 
   ${USHpetss}/petss_shp_img_625m_web.sh 10 17 e40 agl
   echo "`date`: RANK ${RANKO} finished images generating" >> png_40pa.txt

   num_line1=$(grep "RANK" png_40pa.txt | wc -l)

   while [[ $num_line1 -ne 2 ]] ; do
         echo "`date`: Rank ${RANKO} Sleeping nums of lines from png_10pd." >> timing.txt
         num_line1=$(grep "RANK" png_40pa.txt | wc -l)
         sleep 1
   done
   ${USHpetss}/petss_shp_img_web_estglf.sh e40 agl
   echo "`date`: RANK ${RANKO} finished images generating" >> png.txt

elif [[ ${RANKO} == 50 ]] ; then
 
   ${USHpetss}/petss_shp_img_625m_web.sh 1 9 e50 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_50pd.txt
 
elif [[ ${RANKO} == 51 ]] ; then

   ${USHpetss}/petss_shp_img_625m_web.sh 10 19 e50 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_50pd.txt
elif [[ ${RANKO} == 52 ]] ; then
 
   ${USHpetss}/petss_shp_img_625m_web.sh 20 29 e50 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_50pd.txt
elif [[ ${RANKO} == 53 ]] ; then
 
   ${USHpetss}/petss_shp_img_625m_web.sh 30 39 e50 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_50pd.txt
elif [[ ${RANKO} == 54 ]] ; then
  
   ${USHpetss}/petss_shp_img_625m_web.sh 40 49 e50 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_50pd.txt
elif [[ ${RANKO} == 55 ]] ; then
  
   ${USHpetss}/petss_shp_img_625m_web.sh 50 59 e50 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_50pd.txt
elif [[ ${RANKO} == 56 ]] ; then
 
   ${USHpetss}/petss_shp_img_625m_web.sh 60 69 e50 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_50pd.txt
elif [[ ${RANKO} == 57 ]] ; then
   
   ${USHpetss}/petss_shp_img_625m_web.sh 70 79 e50 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_50pd.txt
elif [[ ${RANKO} == 58 ]] ; then 
   
   ${USHpetss}/petss_shp_img_625m_web.sh 80 89 e50 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_50pd.txt
elif [[ ${RANKO} == 59 ]] ; then
   
   ${USHpetss}/petss_shp_img_625m_web.sh 90 102 e50 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_50pd.txt

   num_line1=$(grep "RANK" png_50pd.txt | wc -l)

   while [[ $num_line1 -ne 10 ]] ; do
         echo "`date`: Rank ${RANKO} Sleeping nums of lines from png_10pd." >> timing.txt
         num_line1=$(grep "RANK" png_50pd.txt | wc -l)
         sleep 1
   done
   ${USHpetss}/petss_shp_img_web_estglf.sh e50 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png.txt

elif [[ ${RANKO} == 60 ]] ; then
   
   ${USHpetss}/petss_shp_img_625m_web.sh 1 9 e50 agl
   echo "`date`: RANK ${RANKO} finished images generating" >> png_50pa.txt 
elif [[ ${RANKO} == 61 ]] ; then
 
   ${USHpetss}/petss_shp_img_625m_web.sh 10 17 e50 agl
   echo "`date`: RANK ${RANKO} finished images generating" >> png_50pa.txt

   num_line1=$(grep "RANK" png_50pa.txt | wc -l)

   while [[ $num_line1 -ne 2 ]] ; do
         echo "`date`: Rank ${RANKO} Sleeping nums of lines from png_10pd." >> timing.txt
         num_line1=$(grep "RANK" png_50pa.txt | wc -l)
         sleep 1
   done
   ${USHpetss}/petss_shp_img_web_estglf.sh e50 agl 
   echo "`date`: RANK ${RANKO} finished images generating" >> png.txt

elif [[ ${RANKO} == 62 ]] ; then
 
   ${USHpetss}/petss_shp_img_625m_web.sh 1 9 mean datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_mnd.txt
 
elif [[ ${RANKO} == 63 ]] ; then

   ${USHpetss}/petss_shp_img_625m_web.sh 10 19 mean datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_mnd.txt
elif [[ ${RANKO} == 64 ]] ; then
 
   ${USHpetss}/petss_shp_img_625m_web.sh 20 29 mean datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_mnd.txt
elif [[ ${RANKO} == 65 ]] ; then
 
   ${USHpetss}/petss_shp_img_625m_web.sh 30 39 mean datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_mnd.txt
elif [[ ${RANKO} == 66 ]] ; then
  
   ${USHpetss}/petss_shp_img_625m_web.sh 40 49 mean datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_mnd.txt
elif [[ ${RANKO} == 67 ]] ; then
  
   ${USHpetss}/petss_shp_img_625m_web.sh 50 59 mean datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_mnd.txt
elif [[ ${RANKO} == 68 ]] ; then
 
   ${USHpetss}/petss_shp_img_625m_web.sh 60 69 mean datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_mnd.txt
elif [[ ${RANKO} == 69 ]] ; then
   
   ${USHpetss}/petss_shp_img_625m_web.sh 70 79 mean datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_mnd.txt
elif [[ ${RANKO} == 70 ]] ; then 
   
   ${USHpetss}/petss_shp_img_625m_web.sh 80 89 mean datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_mnd.txt
elif [[ ${RANKO} == 71 ]] ; then
   
   ${USHpetss}/petss_shp_img_625m_web.sh 90 102 mean datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_mnd.txt

   num_line1=$(grep "RANK" png_mnd.txt | wc -l)

   while [[ $num_line1 -ne 10 ]] ; do
         echo "`date`: Rank ${RANKO} Sleeping nums of lines from png_10pd." >> timing.txt
         num_line1=$(grep "RANK" png_mnd.txt | wc -l)
         sleep 1
   done
   ${USHpetss}/petss_shp_img_web_estglf.sh mean datum 
   echo "`date`: RANK ${RANKO} finished images generating" >> png.txt

elif [[ ${RANKO} == 72 ]] ; then
   
   ${USHpetss}/petss_shp_img_625m_web.sh 1 9 mean agl
   echo "`date`: RANK ${RANKO} finished images generating" >> png_mna.txt 
elif [[ ${RANKO} == 73 ]] ; then
 
   ${USHpetss}/petss_shp_img_625m_web.sh 10 17 mean agl
   echo "`date`: RANK ${RANKO} finished images generating" >> png_mna.txt

   num_line1=$(grep "RANK" png_mna.txt | wc -l)

   while [[ $num_line1 -ne 2 ]] ; do
         echo "`date`: Rank ${RANKO} Sleeping nums of lines from png_10pd." >> timing.txt
         num_line1=$(grep "RANK" png_mna.txt | wc -l)
         sleep 1
   done
   ${USHpetss}/petss_shp_img_web_estglf.sh mean agl 
   echo "`date`: RANK ${RANKO} finished images generating" >> png.txt


elif [[ ${RANKO} == 74 ]] ; then

   ${USHpetss}/petss_shp_img.sh con mean datum
   ${USHpetss}/petss_shp_img_web_wst.sh mean datum

   echo "`date`: RANK ${RANKO} finished images generating" >> png.txt

elif [[ ${RANKO} == 75 ]] ; then

   ${USHpetss}/petss_shp_img.sh ala mean datum
   ${USHpetss}/petss_shp_img_web_ala.sh mean datum

   echo "`date`: RANK ${RANKO} finished images generating" >> png.txt

elif [[ ${RANKO} == 76 ]] ; then
 
   ${USHpetss}/petss_shp_img_625m_web.sh 1 9 max datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_maxd.txt
 
elif [[ ${RANKO} == 77 ]] ; then

   ${USHpetss}/petss_shp_img_625m_web.sh 10 19 max datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_maxd.txt
elif [[ ${RANKO} == 78 ]] ; then
 
   ${USHpetss}/petss_shp_img_625m_web.sh 20 29 max datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_maxd.txt
elif [[ ${RANKO} == 79 ]] ; then
 
   ${USHpetss}/petss_shp_img_625m_web.sh 30 39 max datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_maxd.txt
elif [[ ${RANKO} == 80 ]] ; then
  
   ${USHpetss}/petss_shp_img_625m_web.sh 40 49 max datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_maxd.txt
elif [[ ${RANKO} == 81 ]] ; then
  
   ${USHpetss}/petss_shp_img_625m_web.sh 50 59 max datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_maxd.txt
elif [[ ${RANKO} == 82 ]] ; then
 
   ${USHpetss}/petss_shp_img_625m_web.sh 60 69 max datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_maxd.txt
elif [[ ${RANKO} == 83 ]] ; then
   
   ${USHpetss}/petss_shp_img_625m_web.sh 70 79 max datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_maxd.txt
elif [[ ${RANKO} == 84 ]] ; then 
   
   ${USHpetss}/petss_shp_img_625m_web.sh 80 89 max datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_maxd.txt
elif [[ ${RANKO} == 85 ]] ; then
   
   ${USHpetss}/petss_shp_img_625m_web.sh 90 102 max datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_maxd.txt

   num_line1=$(grep "RANK" png_maxd.txt | wc -l)

   while [[ $num_line1 -ne 10 ]] ; do
         echo "`date`: Rank ${RANKO} Sleeping nums of lines from png_10pd." >> timing.txt
         num_line1=$(grep "RANK" png_maxd.txt | wc -l)
         sleep 1
   done
   ${USHpetss}/petss_shp_img_web_estglf.sh max datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png.txt

elif [[ ${RANKO} == 86 ]] ; then
   
   ${USHpetss}/petss_shp_img_625m_web.sh 1 9 max agl
   echo "`date`: RANK ${RANKO} finished images generating" >> png_maxa.txt 
elif [[ ${RANKO} == 87 ]] ; then
 
   ${USHpetss}/petss_shp_img_625m_web.sh 10 17 max agl
   echo "`date`: RANK ${RANKO} finished images generating" >> png_maxa.txt

   num_line1=$(grep "RANK" png_maxa.txt | wc -l)

   while [[ $num_line1 -ne 2 ]] ; do
         echo "`date`: Rank ${RANKO} Sleeping nums of lines from png_10pd." >> timing.txt
         num_line1=$(grep "RANK" png_maxa.txt | wc -l)
         sleep 1
   done
   ${USHpetss}/petss_shp_img_web_estglf.sh max agl
   echo "`date`: RANK ${RANKO} finished images generating" >> png.txt

elif [[ ${RANKO} == 88 ]] ; then

   ${USHpetss}/petss_shp_img_625m_web.sh 1 102 gt1 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_1ftd.txt
   ${USHpetss}/petss_shp_img_web_estglf.sh gt1 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png.txt

elif [[ ${RANKO} == 89 ]] ; then

   ${USHpetss}/petss_shp_img_625m_web.sh 1 102 gt2 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_2ftd.txt
   ${USHpetss}/petss_shp_img_web_estglf.sh gt2 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png.txt

elif [[ ${RANKO} == 90 ]] ; then

   ${USHpetss}/petss_shp_img_625m_web.sh 1 102 gt3 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_3ftd.txt
   ${USHpetss}/petss_shp_img_web_estglf.sh gt3 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png.txt

elif [[ ${RANKO} == 91 ]] ; then

   ${USHpetss}/petss_shp_img_625m_web.sh 1 102 gt6 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_6ftd.txt
   ${USHpetss}/petss_shp_img_web_estglf.sh gt6 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png.txt

elif [[ ${RANKO} == 92 ]] ; then

   ${USHpetss}/petss_shp_img_625m_web.sh 1 102 gt10 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_10ftd.txt
   ${USHpetss}/petss_shp_img_web_estglf.sh gt10 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png.txt

elif [[ ${RANKO} == 93 ]] ; then

   ${USHpetss}/petss_shp_img_625m_web.sh 1 102 gt13 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_13ftd.txt
   ${USHpetss}/petss_shp_img_web_estglf.sh gt13 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png.txt

elif [[ ${RANKO} == 94 ]] ; then

   ${USHpetss}/petss_shp_img_625m_web.sh 1 102 gt16 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_16ftd.txt
   ${USHpetss}/petss_shp_img_web_estglf.sh gt16 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png.txt
elif [[ ${RANKO} == 95 ]] ; then

   ${USHpetss}/petss_shp_img_625m_web.sh 1 17 gt1 agl
   echo "`date`: RANK ${RANKO} finished images generating" >> png_1fta.txt
   ${USHpetss}/petss_shp_img_web_estglf.sh gt1 agl

   ${USHpetss}/petss_shp_img_625m_web.sh 1 17 gt6 agl
   echo "`date`: RANK ${RANKO} finished images generating" >> png_6fta.txt
   ${USHpetss}/petss_shp_img_web_estglf.sh gt6 agl

   ${USHpetss}/petss_shp_img_625m_web.sh 1 17 gt10 agl
   echo "`date`: RANK ${RANKO} finished images generating" >> png_10fta.txt
   ${USHpetss}/petss_shp_img_web_estglf.sh gt10 agl

   echo "`date`: RANK ${RANKO} finished images generating" >> png.txt
elif [[ ${RANKO} == 96 ]] ; then

   ${USHpetss}/petss_shp_img_625m_web.sh 1 17 gt2 agl
   echo "`date`: RANK ${RANKO} finished images generating" >> png_2fta.txt
   ${USHpetss}/petss_shp_img_web_estglf.sh gt2 agl

   ${USHpetss}/petss_shp_img_625m_web.sh 1 17 gt3 agl
   echo "`date`: RANK ${RANKO} finished images generating" >> png_3fta.txt
   ${USHpetss}/petss_shp_img_web_estglf.sh gt3 agl

   ${USHpetss}/petss_shp_img_625m_web.sh 1 17 gt13 agl
   echo "`date`: RANK ${RANKO} finished images generating" >> png_13fta.txt
   ${USHpetss}/petss_shp_img_web_estglf.sh gt13 agl

   ${USHpetss}/petss_shp_img_625m_web.sh 1 17 gt16 agl
   echo "`date`: RANK ${RANKO} finished images generating" >> png_16fta.txt
   ${USHpetss}/petss_shp_img_web_estglf.sh gt16 agl

   echo "`date`: RANK ${RANKO} finished images generating" >> png.txt
elif [[ ${RANKO} == 97 ]] ; then

   ${USHpetss}/petss_shp_img.sh con e30 datum
   ${USHpetss}/petss_shp_img_web_wst.sh e30 datum

   echo "`date`: RANK ${RANKO} finished images generating" >> png.txt

elif [[ ${RANKO} == 98 ]] ; then

   ${USHpetss}/petss_shp_img.sh ala e30 datum
   ${USHpetss}/petss_shp_img_web_ala.sh e30 datum

   echo "`date`: RANK ${RANKO} finished images generating" >> png.txt

elif [[ ${RANKO} == 99 ]] ; then

   ${USHpetss}/petss_shp_img.sh con e20 datum
   ${USHpetss}/petss_shp_img_web_wst.sh e20 datum

   echo "`date`: RANK ${RANKO} finished images generating" >> png.txt

elif [[ ${RANKO} == 100 ]] ; then

   ${USHpetss}/petss_shp_img.sh ala e20 datum
   ${USHpetss}/petss_shp_img_web_ala.sh e20 datum

   echo "`date`: RANK ${RANKO} finished images generating" >> png.txt

elif [[ ${RANKO} == 120 ]] ; then

   ${USHpetss}/petss_shp_img.sh con max datum
   ${USHpetss}/petss_shp_img_web_wst.sh max datum

   echo "`date`: RANK ${RANKO} finished images generating" >> png.txt

elif [[ ${RANKO} == 121 ]] ; then

   ${USHpetss}/petss_shp_img.sh ala max datum
   ${USHpetss}/petss_shp_img_web_ala.sh max datum

   echo "`date`: RANK ${RANKO} finished images generating" >> png.txt

elif [[ ${RANKO} == 122 ]] ; then

   ${USHpetss}/petss_shp_img_625m_web.sh 1 9 e90 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_90pd.txt

elif [[ ${RANKO} == 123 ]] ; then

   ${USHpetss}/petss_shp_img_625m_web.sh 10 19 e90 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_90pd.txt
elif [[ ${RANKO} == 124 ]] ; then

   ${USHpetss}/petss_shp_img_625m_web.sh 20 29 e90 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_90pd.txt
elif [[ ${RANKO} == 125 ]] ; then

   ${USHpetss}/petss_shp_img_625m_web.sh 30 39 e90 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_90pd.txt
elif [[ ${RANKO} == 126 ]] ; then

   ${USHpetss}/petss_shp_img_625m_web.sh 40 49 e90 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_90pd.txt
elif [[ ${RANKO} == 127 ]] ; then

   ${USHpetss}/petss_shp_img_625m_web.sh 50 59 e90 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_90pd.txt
elif [[ ${RANKO} == 128 ]] ; then

   ${USHpetss}/petss_shp_img_625m_web.sh 60 69 e90 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_90pd.txt
elif [[ ${RANKO} == 129 ]] ; then

   ${USHpetss}/petss_shp_img_625m_web.sh 70 79 e90 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_90pd.txt
elif [[ ${RANKO} == 130 ]] ; then

   ${USHpetss}/petss_shp_img_625m_web.sh 80 89 e90 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_90pd.txt
elif [[ ${RANKO} == 131 ]] ; then

   ${USHpetss}/petss_shp_img_625m_web.sh 90 102 e90 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png_90pd.txt

   num_line1=$(grep "RANK" png_90pd.txt | wc -l)

   while [[ $num_line1 -ne 10 ]] ; do
         echo "`date`: Rank ${RANKO} Sleeping nums of lines from png_90pd." >> timing.txt
         num_line1=$(grep "RANK" png_90pd.txt | wc -l)
         sleep 1
   done
   ${USHpetss}/petss_shp_img_web_estglf.sh e90 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png.txt

elif [[ ${RANKO} == 132 ]] ; then

   ${USHpetss}/petss_shp_img.sh con e90 datum
   ${USHpetss}/petss_shp_img_web_wst.sh e90 datum

   echo "`date`: RANK ${RANKO} finished images generating" >> png.txt

elif [[ ${RANKO} == 133 ]] ; then

   ${USHpetss}/petss_shp_img.sh ala e90 datum
   ${USHpetss}/petss_shp_img_web_ala.sh e90 datum

   echo "`date`: RANK ${RANKO} finished images generating" >> png.txt

elif [[ ${RANKO} == 134 ]] ; then

   ${USHpetss}/petss_shp_img_625m_web.sh 1 9 e90 agl
   echo "`date`: RANK ${RANKO} finished images generating" >> png_90pa.txt
elif [[ ${RANKO} == 135 ]] ; then

   ${USHpetss}/petss_shp_img_625m_web.sh 10 17 e90 agl
   echo "`date`: RANK ${RANKO} finished images generating" >> png_90pa.txt

   num_line1=$(grep "RANK" png_90pa.txt | wc -l)

   while [[ $num_line1 -ne 2 ]] ; do
         echo "`date`: Rank ${RANKO} Sleeping nums of lines from png_90pd." >> timing.txt
         num_line1=$(grep "RANK" png_90pa.txt | wc -l)
         sleep 1
   done
   ${USHpetss}/petss_shp_img_web_estglf.sh e90 agl 
   echo "`date`: RANK ${RANKO} finished images generating" >> png.txt
elif [[ ${RANKO} == 136 ]] ; then

   ${USHpetss}/petss_shp_img_agl.sh con e90 agl
   ${USHpetss}/petss_shp_img_web_wst.sh e90 agl
   ${USHpetss}/petss_shp_img_agl.sh ala e90 agl
   ${USHpetss}/petss_shp_img_web_ala.sh e90 agl

   echo "`date`: RANK ${RANKO} finished images generating" >> png.txt

elif [[ ${RANKO} == 144 ]] ; then
 
   ${USHpetss}/petss_shp_img.sh con e40 datum 
   ${USHpetss}/petss_shp_img_web_wst.sh e40 datum

   echo "`date`: RANK ${RANKO} finished images generating" >> png.txt
 
elif [[ ${RANKO} == 145 ]] ; then

   ${USHpetss}/petss_shp_img.sh ala e40 datum
   ${USHpetss}/petss_shp_img_web_ala.sh e40 datum

   echo "`date`: RANK ${RANKO} finished images generating" >> png.txt

elif [[ ${RANKO} == 204 ]] ; then

   ${USHpetss}/petss_shp_img.sh con e50 datum
   ${USHpetss}/petss_shp_img_web_wst.sh e50 datum

   echo "`date`: RANK ${RANKO} finished images generating" >> png.txt

elif [[ ${RANKO} == 205 ]] ; then

   ${USHpetss}/petss_shp_img.sh ala e50 datum
   ${USHpetss}/petss_shp_img_web_ala.sh e50 datum

   echo "`date`: RANK ${RANKO} finished images generating" >> png.txt

elif [[ ${RANKO} == 228 ]] ; then
 
   ${USHpetss}/petss_shp_img.sh con gt1 datum
   ${USHpetss}/petss_shp_img_web_wst.sh gt1 datum

   ${USHpetss}/petss_shp_img.sh ala gt1 datum
   ${USHpetss}/petss_shp_img_web_ala.sh gt1 datum   

   echo "`date`: RANK ${RANKO} finished images generating" >> png.txt

elif [[ ${RANKO} == 229 ]] ; then

   ${USHpetss}/petss_shp_img.sh con gt10 datum
   ${USHpetss}/petss_shp_img_web_wst.sh gt10 datum
   ${USHpetss}/petss_shp_img.sh ala gt10 datum
   ${USHpetss}/petss_shp_img_web_ala.sh gt10 datum

   ${USHpetss}/petss_shp_img.sh con gt13 datum
   ${USHpetss}/petss_shp_img_web_wst.sh gt13 datum
   ${USHpetss}/petss_shp_img.sh ala gt13 datum
   ${USHpetss}/petss_shp_img_web_ala.sh gt13 datum

   ${USHpetss}/petss_shp_img.sh con gt16 datum
   ${USHpetss}/petss_shp_img_web_wst.sh gt16 datum
   ${USHpetss}/petss_shp_img.sh ala gt16 datum
   ${USHpetss}/petss_shp_img_web_ala.sh gt16 datum

   echo "`date`: RANK ${RANKO} finished images generating" >> png.txt

elif [[ ${RANKO} == 230 ]] ; then
 
   ${USHpetss}/petss_shp_img.sh con gt2 datum
   ${USHpetss}/petss_shp_img_web_wst.sh gt2 datum

   ${USHpetss}/petss_shp_img.sh ala gt2 datum
   ${USHpetss}/petss_shp_img_web_ala.sh gt2 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png.txt

elif [[ ${RANKO} == 231 ]] ; then
 
   ${USHpetss}/petss_shp_img.sh con gt3 datum
   ${USHpetss}/petss_shp_img_web_wst.sh gt3 datum
   ${USHpetss}/petss_shp_img.sh ala gt3 datum
   ${USHpetss}/petss_shp_img_web_ala.sh gt3 datum

   ${USHpetss}/petss_shp_img.sh con gt6 datum
   ${USHpetss}/petss_shp_img_web_wst.sh gt6 datum
   ${USHpetss}/petss_shp_img.sh ala gt6 datum
   ${USHpetss}/petss_shp_img_web_ala.sh gt6 datum
   echo "`date`: RANK ${RANKO} finished images generating" >> png.txt

elif [[ ${RANKO} == 232 ]] ; then 

   ${USHpetss}/petss_shp_img_agl.sh con gt1 agl
   ${USHpetss}/petss_shp_img_agl.sh con gt2 agl
   ${USHpetss}/petss_shp_img_agl.sh con gt3 agl
   ${USHpetss}/petss_shp_img_agl.sh con gt6 agl
   ${USHpetss}/petss_shp_img_agl.sh con gt10 agl
   ${USHpetss}/petss_shp_img_agl.sh con gt13 agl
   ${USHpetss}/petss_shp_img_agl.sh con gt16 agl
   ${USHpetss}/petss_shp_img_web_wst.sh gt1 agl
   ${USHpetss}/petss_shp_img_web_wst.sh gt2 agl
   ${USHpetss}/petss_shp_img_web_wst.sh gt3 agl
   ${USHpetss}/petss_shp_img_web_wst.sh gt6 agl
   ${USHpetss}/petss_shp_img_web_wst.sh gt10 agl
   ${USHpetss}/petss_shp_img_web_wst.sh gt13 agl
   ${USHpetss}/petss_shp_img_web_wst.sh gt16 agl
   echo "`date`: RANK ${RANKO} finished images generating" >> png.txt
 
elif [[ ${RANKO} == 233 ]] ; then 
 
   ${USHpetss}/petss_shp_img_agl.sh ala gt1 agl
   ${USHpetss}/petss_shp_img_agl.sh ala gt2 agl
   ${USHpetss}/petss_shp_img_agl.sh ala gt3 agl
   ${USHpetss}/petss_shp_img_agl.sh ala gt6 agl
   ${USHpetss}/petss_shp_img_agl.sh ala gt10 agl
   ${USHpetss}/petss_shp_img_agl.sh ala gt13 agl
   ${USHpetss}/petss_shp_img_agl.sh ala gt16 agl
   ${USHpetss}/petss_shp_img_web_ala.sh gt1 agl
   ${USHpetss}/petss_shp_img_web_ala.sh gt2 agl
   ${USHpetss}/petss_shp_img_web_ala.sh gt3 agl
   ${USHpetss}/petss_shp_img_web_ala.sh gt6 agl
   ${USHpetss}/petss_shp_img_web_ala.sh gt10 agl
   ${USHpetss}/petss_shp_img_web_ala.sh gt13 agl
   ${USHpetss}/petss_shp_img_web_ala.sh gt16 agl

   echo "`date`: RANK ${RANKO} finished images generating" >> png.txt

elif [[ ${RANKO} == 234 ]] ; then

   ${USHpetss}/petss_shp_img_agl.sh con e10 agl
   ${USHpetss}/petss_shp_img_agl.sh con e20 agl
   ${USHpetss}/petss_shp_img_agl.sh con e30 agl
   ${USHpetss}/petss_shp_img_agl.sh con e40 agl
   ${USHpetss}/petss_shp_img_web_wst.sh e10 agl
   ${USHpetss}/petss_shp_img_web_wst.sh e20 agl
   ${USHpetss}/petss_shp_img_web_wst.sh e30 agl
   ${USHpetss}/petss_shp_img_web_wst.sh e40 agl

   echo "`date`: RANK ${RANKO} finished images generating" >> png.txt

elif [[ ${RANKO} == 235 ]] ; then 

   ${USHpetss}/petss_shp_img_agl.sh ala e10 agl
   ${USHpetss}/petss_shp_img_agl.sh ala e20 agl
   ${USHpetss}/petss_shp_img_agl.sh ala e30 agl
   ${USHpetss}/petss_shp_img_agl.sh ala e40 agl
   ${USHpetss}/petss_shp_img_web_ala.sh e10 agl
   ${USHpetss}/petss_shp_img_web_ala.sh e20 agl
   ${USHpetss}/petss_shp_img_web_ala.sh e30 agl
   ${USHpetss}/petss_shp_img_web_ala.sh e40 agl
   echo "`date`: RANK ${RANKO} finished images generating" >> png.txt

elif [[ ${RANKO} == 240 ]] ; then 

   ${USHpetss}/petss_shp_img_agl.sh ala e50 agl
   ${USHpetss}/petss_shp_img_agl.sh con e50 agl
   ${USHpetss}/petss_shp_img_agl.sh con mean agl
   ${USHpetss}/petss_shp_img_web_ala.sh e50 agl
   ${USHpetss}/petss_shp_img_web_wst.sh e50 agl
   ${USHpetss}/petss_shp_img_web_wst.sh mean agl
   echo "`date`: RANK ${RANKO} finished images generating" >> png.txt
 
elif [[ ${RANKO} == 265 ]] ; then 
 
   ${USHpetss}/petss_shp_img_agl.sh ala max agl
   ${USHpetss}/petss_shp_img_agl.sh ala mean agl
   ${USHpetss}/petss_shp_img_agl.sh con max agl
   ${USHpetss}/petss_shp_img_web_ala.sh max agl
   ${USHpetss}/petss_shp_img_web_ala.sh mean agl
   ${USHpetss}/petss_shp_img_web_wst.sh max agl

   echo "`date`: RANK ${RANKO} finished images generating" >> png.txt

fi


if [[ ${RANKO} == 0 ]] ; then

   num_linef=$(grep "RANK" png.txt | wc -l)

   while [[ $num_linef -ne 52 ]] ; do
         echo "`date`: Rank ${RANKO} Sleeping nums of lines from png.txt." >> timing.txt
         num_linef=$(grep "RANK" png.txt | wc -l)
         sleep 1
   done
   echo "Done" > png_${cyc}.txt

   echo ${PDY}${cyc} > PDY.txt
   cp PDY.txt ${COMOUT}
   
   cd ${IMGpetss}/../   
   tar -czf ${COMOUT}/petss.t${cyc}z.img.tar.gz img/*

   if [[ ${RUN_ENVIR} == "MDLTEST" ]] ; then
      echo "Image generation is finished" > ${MDLTEST_DIR}/tmpnwprd1/image_finish.txt
   fi
   
fi
