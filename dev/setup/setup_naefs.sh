#!/bin/bash

NET=naefs
RUNgefs=gefs
RUNcmce=cmce
PER=00
cycle=t${cyc}z

COMIN=${MDLTEST_DIR}/com/${NET}/${envir}/${NET}.${PDY}
mkdir -p ${COMIN}
mkdir -p ${COMIN}/00/pgrb2ap5 ${COMIN}/00/atmos/pgrb2sp25
mkdir -p ${COMIN}/06/pgrb2ap5 ${COMIN}/06/atmos/pgrb2sp25
mkdir -p ${COMIN}/12/pgrb2ap5 ${COMIN}/12/atmos/pgrb2sp25
mkdir -p ${COMIN}/18/pgrb2ap5 ${COMIN}/18/atmos/pgrb2sp25

COMINm1=${MDLTEST_DIR}/com/naefs/${envir}/${NET}.${PDYm1}
mkdir -p ${COMINm1}
mkdir -p ${COMINm1}/00/pgrb2ap5 ${COMINm1}/00/atmos/pgrb2sp25
mkdir -p ${COMINm1}/06/pgrb2ap5 ${COMINm1}/06/atmos/pgrb2sp25
mkdir -p ${COMINm1}/12/pgrb2ap5 ${COMINm1}/12/atmos/pgrb2sp25
mkdir -p ${COMINm1}/18/pgrb2ap5 ${COMINm1}/18/atmos/pgrb2sp25
COMINm2=${MDLTEST_DIR}/com/naefs/${envir}/${NET}.${PDYm2}
mkdir -p ${COMINm2}
mkdir -p ${COMINm2}/00/pgrb2ap5 ${COMINm2}/00/atmos/pgrb2sp25
mkdir -p ${COMINm2}/06/pgrb2ap5 ${COMINm2}/06/atmos/pgrb2sp25
mkdir -p ${COMINm2}/12/pgrb2ap5 ${COMINm2}/12/atmos/pgrb2sp25
mkdir -p ${COMINm2}/18/pgrb2ap5 ${COMINm2}/18/atmos/pgrb2sp25
COMINm3=${MDLTEST_DIR}/com/naefs/${envir}/${NET}.${PDYm3}
mkdir -p ${COMINm3}
mkdir -p ${COMINm3}/00/pgrb2ap5 ${COMINm3}/00/atmos/pgrb2sp25
mkdir -p ${COMINm3}/06/pgrb2ap5 ${COMINm3}/06/atmos/pgrb2sp25
mkdir -p ${COMINm3}/12/pgrb2ap5 ${COMINm3}/12/atmos/pgrb2sp25
mkdir -p ${COMINm3}/18/pgrb2ap5 ${COMINm3}/18/atmos/pgrb2sp25


module load grib_util/1.0.5
export DCOMROOT="/gpfs/dell1/nco/ops/dcom/prod"

if [[ ${GEFS_para} == YES ]]; then
   export WCOSS_COMINgefs=/gpfs/dell4/nco/ops/com/gefs/para/gefs.${PDY}
   export WCOSS_COMINgefsm1=/gpfs/dell4/nco/ops/com/gefs/para/gefs.${PDYm1}
   export WCOSS_COMINgefsm2=/gpfs/dell4/nco/ops/com/gefs/para/gefs.${PDYm2}
   export WCOSS_COMINgefsm3=/gpfs/dell4/nco/ops/com/gefs/para/gefs.${PDYm3}
else
   export WCOSS_COMINgefs=$(compath.py gefs/prod/${RUNgefs}.$PDY)
   export WCOSS_COMINgefsm1=$(compath.py gefs/prod/${RUNgefs}.$PDYm1)
   export WCOSS_COMINgefsm2=$(compath.py gefs/prod/${RUNgefs}.$PDYm2)
   export WCOSS_COMINgefsm3=$(compath.py gefs/prod/${RUNgefs}.$PDYm3)
fi

export WCOSS_COMINcmce=${DCOMROOT}/${PDY}/wgrbbul/cmcens_gb2
export WCOSS_COMINcmcem1=${DCOMROOT}/${PDYm1}/wgrbbul/cmcens_gb2
export WCOSS_COMINcmcem2=${DCOMROOT}/${PDYm2}/wgrbbul/cmcens_gb2
export WCOSS_COMINcmcem3=${DCOMROOT}/${PDYm3}/wgrbbul/cmcens_gb2

if [[ ${RUN_NewGEFS} == YES ]] ; then
   GEFS_Res='0p25'
   cycatmos=${cyc}/atmos/pgrb2sp25
   GEFS_Fn='pgrb2s'
   ens_num=30
else
   GEFS_Res='0p50'
   cycatmos=${cyc}/pgrb2ap5
   GEFS_Fn='pgrb2a'
   ens_num=20
fi

cd ${COMIN}/${cycatmos}

for ZZ in $(seq -f "%03g" 3 3 102);do
# Copy GEFS 00
    ${WGRIB2} ${WCOSS_COMINgefs}/${cycatmos}/gec00.${cycle}.${GEFS_Fn}.${GEFS_Res}.f${ZZ} | \
    awk '{if($4 == "UGRD" && $5 == "10 m above ground" || $4 == "VGRD" && $5 == "10 m above ground" || $4 == "PRMSL") print $0}' FS=':' | \
    ${WGRIB2} ${WCOSS_COMINgefs}/${cycatmos}/gec00.${cycle}.${GEFS_Fn}.${GEFS_Res}.f${ZZ} -i -grib gec00.${cycle}.${GEFS_Fn}.${GEFS_Res}.f${ZZ}
    for per in $(seq -f "%02g" 1 1 ${ens_num});do
        ${WGRIB2} ${WCOSS_COMINgefs}/${cycatmos}/gep${per}.${cycle}.${GEFS_Fn}.${GEFS_Res}.f${ZZ} | \
        awk '{if($4 == "UGRD" && $5 == "10 m above ground" || $4 == "VGRD" && $5 == "10 m above ground" || $4 == "PRMSL") print $0}' FS=':' | \
        ${WGRIB2} ${WCOSS_COMINgefs}/${cycatmos}/gep${per}.${cycle}.${GEFS_Fn}.${GEFS_Res}.f${ZZ} -i -grib gep${per}.${cycle}.${GEFS_Fn}.${GEFS_Res}.f${ZZ}
    done

done

# Copy CMC 00 
if [[ ${cyc} == 00 || ${cyc} == 12 ]] ; then
cd ${COMIN}/${cyc}/pgrb2ap5

for ZZ in $(seq -f "%03g" 3 3 108);do
    ${WGRIB2} ${WCOSS_COMINcmce}/${PDY}${cyc}_CMC_naefs_hr_latlon0p5x0p5_P${ZZ}_000.grib2 | \
    awk '{if($4 == "UGRD" && $5 == "10 m above ground" || $4 == "VGRD" && $5 == "10 m above ground" || $4 == "PRMSL") print $0}' FS=':' | \
    ${WGRIB2} ${WCOSS_COMINcmce}/${PDY}${cyc}_CMC_naefs_hr_latlon0p5x0p5_P${ZZ}_000.grib2 -i -grib cmc_gec00.${cycle}.pgrb2a.0p50.f${ZZ}
    for per in $(seq -f "%02g" 1 1 20);do
         ${WGRIB2} ${WCOSS_COMINcmce}/${PDY}${cyc}_CMC_naefs_hr_latlon0p5x0p5_P${ZZ}_0${per}.grib2 | \
         awk '{if($4 == "UGRD" && $5 == "10 m above ground" || $4 == "VGRD" && $5 == "10 m above ground" || $4 == "PRMSL") print $0}' FS=':' | \
         ${WGRIB2} ${WCOSS_COMINcmce}/${PDY}${cyc}_CMC_naefs_hr_latlon0p5x0p5_P${ZZ}_0${per}.grib2 -i -grib cmc_gep${per}.${cycle}.pgrb2a.0p50.f${ZZ}
    done
done
fi

if [[ ${cyc} == 00 ]] ; then
   DDM=0
elif [[ ${cyc} == 06 ]] ; then
   DDM=6
elif [[ ${cyc} == 12 ]] ; then
   DDM=12
else
   DDM=18
fi
#Copy GEFS
for HH in $(seq -f "%02g" 0 6 ${DDM});do
    if [[ ${RUN_NewGEFS} == YES ]] ; then
       hhatmos=${HH}/atmos/pgrb2sp25
    else
       hhatmos=${HH}/pgrb2ap5
    fi

    cd ${COMIN}/${hhatmos}

    ${WGRIB2} ${WCOSS_COMINgefs}/${hhatmos}/gec00.t${HH}z.${GEFS_Fn}.${GEFS_Res}.f000 | \
    awk '{if($4 == "UGRD" && $5 == "10 m above ground" || $4 == "VGRD" && $5 == "10 m above ground" || $4 == "PRMSL") print $0}' FS=':' | \
    ${WGRIB2} ${WCOSS_COMINgefs}/${hhatmos}/gec00.t${HH}z.${GEFS_Fn}.${GEFS_Res}.f000 -i -grib gec00.t${HH}z.${GEFS_Fn}.${GEFS_Res}.f000
   for per in $(seq -f "%02g" 1 1 ${ens_num});do
    ${WGRIB2} ${WCOSS_COMINgefs}/${hhatmos}/gep${per}.t${HH}z.${GEFS_Fn}.${GEFS_Res}.f000 | \
    awk '{if($4 == "UGRD" && $5 == "10 m above ground" || $4 == "VGRD" && $5 == "10 m above ground" || $4 == "PRMSL") print $0}' FS=':' | \
    ${WGRIB2} ${WCOSS_COMINgefs}/${hhatmos}/gep${per}.t${HH}z.${GEFS_Fn}.${GEFS_Res}.f000 -i -grib gep${per}.t${HH}z.${GEFS_Fn}.${GEFS_Res}.f000
   done
done
   if [[ ${cyc} == 00 || ${cyc} == 12 ]] ; then
# Copy CMC  
      for HH in $(seq -f "%02g" 0 12 ${DDM});do

          cd ${COMIN}/${HH}/pgrb2ap5

          ${WGRIB2} ${WCOSS_COMINcmce}/${PDY}${HH}_CMC_naefs_hr_latlon0p5x0p5_P000_000.grib2 | \
          awk '{if($4 == "UGRD" && $5 == "10 m above ground" || $4 == "VGRD" && $5 == "10 m above ground" || $4 == "PRMSL") print $0}' FS=':' | \
          ${WGRIB2} ${WCOSS_COMINcmce}/${PDY}${HH}_CMC_naefs_hr_latlon0p5x0p5_P000_000.grib2 -i -grib cmc_gec00.t${HH}z.pgrb2a.0p50.f000
          for per in $(seq -f "%02g" 1 1 20);do
              ${WGRIB2} ${WCOSS_COMINcmce}/${PDY}${HH}_CMC_naefs_hr_latlon0p5x0p5_P000_0${per}.grib2 | \
              awk '{if($4 == "UGRD" && $5 == "10 m above ground" || $4 == "VGRD" && $5 == "10 m above ground" || $4 == "PRMSL") print $0}' FS=':' | \
              ${WGRIB2} ${WCOSS_COMINcmce}/${PDY}${HH}_CMC_naefs_hr_latlon0p5x0p5_P000_0${per}.grib2 -i -grib cmc_gep${per}.t${HH}z.pgrb2a.0p50.f000
          done
      done
   fi
# Copy GEFS ${COMINm1}
for HH in 00 06 12 18;do
    if [[ ${RUN_NewGEFS} == YES ]] ; then
       hhatmos=${HH}/atmos/pgrb2sp25
    else
       hhatmos=${HH}/pgrb2ap5
    fi

    cd ${COMINm1}/${hhatmos}

    ${WGRIB2} ${WCOSS_COMINgefsm1}/${hhatmos}/gec00.t${HH}z.${GEFS_Fn}.${GEFS_Res}.f000 | \
    awk '{if($4 == "UGRD" && $5 == "10 m above ground" || $4 == "VGRD" && $5 == "10 m above ground" || $4 == "PRMSL") print $0}' FS=':' | \
    ${WGRIB2} ${WCOSS_COMINgefsm1}/${hhatmos}/gec00.t${HH}z.${GEFS_Fn}.${GEFS_Res}.f000 -i -grib gec00.t${HH}z.${GEFS_Fn}.${GEFS_Res}.f000
    for per in $(seq -f "%02g" 1 1 ${ens_num});do
    ${WGRIB2} ${WCOSS_COMINgefsm1}/${hhatmos}/gep${per}.t${HH}z.${GEFS_Fn}.${GEFS_Res}.f000 | \
    awk '{if($4 == "UGRD" && $5 == "10 m above ground" || $4 == "VGRD" && $5 == "10 m above ground" || $4 == "PRMSL") print $0}' FS=':' | \
    ${WGRIB2} ${WCOSS_COMINgefsm1}/${hhatmos}/gep${per}.t${HH}z.${GEFS_Fn}.${GEFS_Res}.f000 -i -grib gep${per}.t${HH}z.${GEFS_Fn}.${GEFS_Res}.f000
    done
done
   if [[ ${cyc} == 00 || ${cyc} == 12 ]] ; then
# Copy CMC ${COMINm1}
      for HH in 00 12;do

         cd ${COMINm1}/${HH}/pgrb2ap5

         ${WGRIB2} ${WCOSS_COMINcmcem1}/${PDYm1}${HH}_CMC_naefs_hr_latlon0p5x0p5_P000_000.grib2 | \
         awk '{if($4 == "UGRD" && $5 == "10 m above ground" || $4 == "VGRD" && $5 == "10 m above ground" || $4 == "PRMSL") print $0}' FS=':' | \
         ${WGRIB2} ${WCOSS_COMINcmcem1}/${PDYm1}${HH}_CMC_naefs_hr_latlon0p5x0p5_P000_000.grib2 -i -grib cmc_gec00.t${HH}z.pgrb2a.0p50.f000
         for per in $(seq -f "%02g" 1 1 20);do
             ${WGRIB2} ${WCOSS_COMINcmcem1}/${PDYm1}${HH}_CMC_naefs_hr_latlon0p5x0p5_P000_0${per}.grib2 | \
             awk '{if($4 == "UGRD" && $5 == "10 m above ground" || $4 == "VGRD" && $5 == "10 m above ground" || $4 == "PRMSL") print $0}' FS=':' | \
             ${WGRIB2} ${WCOSS_COMINcmcem1}/${PDYm1}${HH}_CMC_naefs_hr_latlon0p5x0p5_P000_0${per}.grib2 -i -grib cmc_gep${per}.t${HH}z.pgrb2a.0p50.f000
         done
      done
   fi
# Copy GEFS ${COMINm2}
for HH in 00 06 12 18;do

    if [[ ${RUN_NewGEFS} == YES ]] ; then
       hhatmos=${HH}/atmos/pgrb2sp25
    else
       hhatmos=${HH}/pgrb2ap5
    fi

    cd ${COMINm2}/${hhatmos}

    ${WGRIB2} ${WCOSS_COMINgefsm2}/${hhatmos}/gec00.t${HH}z.${GEFS_Fn}.${GEFS_Res}.f000 | \
    awk '{if($4 == "UGRD" && $5 == "10 m above ground" || $4 == "VGRD" && $5 == "10 m above ground" || $4 == "PRMSL") print $0}' FS=':' | \
    ${WGRIB2} ${WCOSS_COMINgefsm2}/${hhatmos}/gec00.t${HH}z.${GEFS_Fn}.${GEFS_Res}.f000 -i -grib gec00.t${HH}z.${GEFS_Fn}.${GEFS_Res}.f000
    for per in $(seq -f "%02g" 1 1 ${ens_num});do
    ${WGRIB2} ${WCOSS_COMINgefsm2}/${hhatmos}/gep${per}.t${HH}z.${GEFS_Fn}.${GEFS_Res}.f000 | \
    awk '{if($4 == "UGRD" && $5 == "10 m above ground" || $4 == "VGRD" && $5 == "10 m above ground" || $4 == "PRMSL") print $0}' FS=':' | \
    ${WGRIB2} ${WCOSS_COMINgefsm2}/${hhatmos}/gep${per}.t${HH}z.${GEFS_Fn}.${GEFS_Res}.f000 -i -grib gep${per}.t${HH}z.${GEFS_Fn}.${GEFS_Res}.f000
    done
done
   if [[ ${cyc} == 00 || ${cyc} == 12 ]] ; then
# Copy CMC ${COMINm2}
      for HH in 00 12;do
 
          cd ${COMINm2}/${HH}/pgrb2ap5

          ${WGRIB2} ${WCOSS_COMINcmcem2}/${PDYm2}${HH}_CMC_naefs_hr_latlon0p5x0p5_P000_000.grib2 | \
          awk '{if($4 == "UGRD" && $5 == "10 m above ground" || $4 == "VGRD" && $5 == "10 m above ground" || $4 == "PRMSL") print $0}' FS=':' | \
          ${WGRIB2} ${WCOSS_COMINcmcem2}/${PDYm2}${HH}_CMC_naefs_hr_latlon0p5x0p5_P000_000.grib2 -i -grib cmc_gec00.t${HH}z.pgrb2a.0p50.f000
          for per in $(seq -f "%02g" 1 1 20);do
              ${WGRIB2} ${WCOSS_COMINcmcem2}/${PDYm2}${HH}_CMC_naefs_hr_latlon0p5x0p5_P000_0${per}.grib2 | \
              awk '{if($4 == "UGRD" && $5 == "10 m above ground" || $4 == "VGRD" && $5 == "10 m above ground" || $4 == "PRMSL") print $0}' FS=':' | \
             ${WGRIB2} ${WCOSS_COMINcmcem2}/${PDYm2}${HH}_CMC_naefs_hr_latlon0p5x0p5_P000_0${per}.grib2 -i -grib cmc_gep${per}.t${HH}z.pgrb2a.0p50.f000
          done
      done
   fi
# Copy GEFS ${COMINm3}
for HH in 00 06 12 18;do
    if [[ ${RUN_NewGEFS} == YES ]] ; then
       hhatmos=${HH}/atmos/pgrb2sp25
    else
       hhatmos=${HH}/pgrb2ap5
    fi

    cd ${COMINm3}/${hhatmos}

    ${WGRIB2} ${WCOSS_COMINgefsm3}/${hhatmos}/gec00.t${HH}z.${GEFS_Fn}.${GEFS_Res}.f000 | \
    awk '{if($4 == "UGRD" && $5 == "10 m above ground" || $4 == "VGRD" && $5 == "10 m above ground" || $4 == "PRMSL") print $0}' FS=':' | \
    ${WGRIB2} ${WCOSS_COMINgefsm3}/${hhatmos}/gec00.t${HH}z.${GEFS_Fn}.${GEFS_Res}.f000 -i -grib gec00.t${HH}z.${GEFS_Fn}.${GEFS_Res}.f000
    for per in $(seq -f "%02g" 1 1 ${ens_num});do
    ${WGRIB2} ${WCOSS_COMINgefsm3}/${hhatmos}/gep${per}.t${HH}z.${GEFS_Fn}.${GEFS_Res}.f000 | \
    awk '{if($4 == "UGRD" && $5 == "10 m above ground" || $4 == "VGRD" && $5 == "10 m above ground" || $4 == "PRMSL") print $0}' FS=':' | \
    ${WGRIB2} ${WCOSS_COMINgefsm3}/${hhatmos}/gep${per}.t${HH}z.${GEFS_Fn}.${GEFS_Res}.f000 -i -grib gep${per}.t${HH}z.${GEFS_Fn}.${GEFS_Res}.f000
    done
done
   if [[ ${cyc} == 00 || ${cyc} == 12 ]] ; then
# Copy CMC ${COMINm3}
      for HH in 00 12;do

          cd ${COMINm3}/${HH}/pgrb2ap5

          ${WGRIB2} ${WCOSS_COMINcmcem3}/${PDYm3}${HH}_CMC_naefs_hr_latlon0p5x0p5_P000_000.grib2 | \
          awk '{if($4 == "UGRD" && $5 == "10 m above ground" || $4 == "VGRD" && $5 == "10 m above ground" || $4 == "PRMSL") print $0}' FS=':' | \
          ${WGRIB2} ${WCOSS_COMINcmcem3}/${PDYm3}${HH}_CMC_naefs_hr_latlon0p5x0p5_P000_000.grib2 -i -grib cmc_gec00.t${HH}z.pgrb2a.0p50.f000
          for per in $(seq -f "%02g" 1 1 20);do
              ${WGRIB2} ${WCOSS_COMINcmcem3}/${PDYm3}${HH}_CMC_naefs_hr_latlon0p5x0p5_P000_0${per}.grib2 | \
              awk '{if($4 == "UGRD" && $5 == "10 m above ground" || $4 == "VGRD" && $5 == "10 m above ground" || $4 == "PRMSL") print $0}' FS=':' | \
              ${WGRIB2} ${WCOSS_COMINcmcem3}/${PDYm3}${HH}_CMC_naefs_hr_latlon0p5x0p5_P000_0${per}.grib2 -i -grib cmc_gep${per}.t${HH}z.pgrb2a.0p50.f000
          done
      done
   fi

