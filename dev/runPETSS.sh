#!/bin/bash -l

module load prod_util

if [ $# -eq 5 ] ; then
   YYYYMMDD=$1
   cycle=$2
   fgt_data=$3
   jump=$4
   web=$5
elif [ $# -eq 4 ] ; then
   cycle=$1
   fgt_data=$2
   jump=$3
   web=$4
   if [[ ${cycle} == 18 ]] ; then
     YYYYMMDD=$(date -d "yesterday" +%Y%m%d)
   else
     YYYYMMDD=$(date +%Y%m%d)
   fi
else
  now=$(date +"%Y%m%d")
  echo "You are trying to run the P-ETSS model but you missed something--"
  echo ""
  echo "[1]--You can run model using wind forcing from specifying date:"
  echo ""
#  echo "You can run model using wind forcing from specifying date:"
  echo "Usage $0 <date ${now}> <cycle (00, 06, 12, 18)> <copy/no-copy GEFS data> <skip p-etss model run (Y/N)> <generating web images (Y/N)>"
  echo ""
  echo "[2]--You can run model using today's wind forcing:"
#  echo "You can run model using today's wind forcing:"
  echo ""
  echo "Usage $0 <cycle (00, 06, 12, 18)> <copy/no-copy GEFS data> <skip p-etss model run (Y/N)> <generating web images (Y/N)>"
  echo ""
  exit
fi

export pid=$$

root=$(cd $(dirname $0) && pwd)
echo ${root}/runPETSS.sh $YYYYMMDD ${cycle} ${fgt_data} ${jump} ${web}

#########################################################
#  Directory for petss1.2 tmp and local copy of com, pcom
#########################################################
if [ ! -e ${root}/petss.v1.1.0 ]; then
   if [[ ${USER} == 'mdl.surge' ]]; then  # MDL dev env
      mkdir -p /gpfs/hps3/ptmp/mdl.surge/petss.v1.1.0
      ln -s /gpfs/hps3/ptmp/mdl.surge/petss.v1.1.0 petss.v1.1.0
   else
      echo 'Please create output directory "petss.v1.1.0" under' ${root}
      echo 'e.g. you can create a "petss.v1.1.0" directory under the "ptmp"'
      echo 'directory and then make a symbolic link to it under' ${root}
      exit
   fi
fi
export PETSS_PRIORITY=dev
export QUEUE=${PETSS_PRIORITY:-workq}
if [[ ${jump} == "N" ]] ; then
   ${root}/runPETSS_model.sh $YYYYMMDD ${cycle} ${fgt_data} ${web}  #submit PETSS job
else
   ${root}/runPost_PETSS.sh $YYYYMMDD ${cycle} ${jump}
   if [[ ${web} == "Y" ]] ; then
      ${root}/run_gen_png_prod.sh $YYYYMMDD ${cycle} ${jump}       #submit generating images job
   fi   
fi
