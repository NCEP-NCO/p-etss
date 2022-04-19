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
#  Post process the basin runs (statistic post processing).
# 1) Get the CPUs number and split to 4 groups (each has 34 CPUs---34 basins)
# 2) Assign each groups to parallel run exceedance height and probability of water level
#    for surgeOnly and Surge+tide above model datum and Ground Level
# Parameters:
#  Inputs:  CPUs number and Datum
#          
#  Outputs: Exceedance Height and Probability of each basin grid
#############################################################################################
set -x

if [ $# -ne 2 ] ; then
  echo "This exec's the PETSS model on Tide."
  echo ""
  echo "Usage $0 <RANK> <datum (AGL/DATUM)>"
  echo ""
  exit
fi


   RK=$1
   datum=$2

  bsn1=('e' 'g' 'n' 'k' 'm' 'pn2' 'pv2' 'ny3' 'de3' 'cp5' 'hor3' 'ht3' 'il3' 'hch2' 'esv4' 'ejx3' 'co2' 'pb3' 'hmi3' 'eke2' 'efm2' 'etp3' 'cd2' 'ap3' 'hpa2' 'epn3' 'emo2' 'ms7' 'lf2' 'ebp3' 'egl3' 'ps2' 'cr3' 'ebr3')
  imxb1=('472' '300' '763' '250' '246' '215' '183' '189' '223' '481' '319' '300' '171' '252' '152' '333' '69' '71' '125' '170' '111' '188' '157' '141' '105' '200' '229' '175' '218' '224' '242' '192' '145' '206')
  jmxb1=('440' '364' '886' '257' '745' '229' '280' '165' '241' '524' '429' '400' '251' '314' '200' '381' '89' '173' '190' '200' '100' '215' '169' '225' '118' '330' '135' '189' '301' '350' '191' '211' '149' '362')

   if [[ ${datum} == 'AGL' ]] ; then
      DTM='.agl'
   else
      DTM=''
   fi 
   tideYN=N
   basinNEP=N
   if [ $RK -lt 34 ] ; then
   
      . prep_step

      export pgm="petss_post_sort"

      ii=$RK
      fle_ext=surge_tide
      bsn=${bsn1[${ii}]}
      imxb=${imxb1[${ii}]}
      jmxb=${jmxb1[${ii}]}

      export FORT51=ssgrid${DTM}.max.${fle_ext}.${cyc}${bsn}
      export FORT52=ssgrid${DTM}.min.${fle_ext}.${cyc}${bsn}
      export FORT53=ssgrid${DTM}.mean.${fle_ext}.${cyc}${bsn}
      export FORT54=ssgrid${DTM}.10p.${fle_ext}.${cyc}${bsn}
      export FORT55=ssgrid${DTM}.90p.${fle_ext}.${cyc}${bsn}
      export FORT56=ssgrid${DTM}.20p.${fle_ext}.${cyc}${bsn}
      export FORT57=ssgrid${DTM}.30p.${fle_ext}.${cyc}${bsn}
      export FORT58=ssgrid${DTM}.40p.${fle_ext}.${cyc}${bsn}
      export FORT59=ssgrid${DTM}.50p.${fle_ext}.${cyc}${bsn}
      echo exc_naefsn ${cyc} > post${DTM}_sort_${fle_ext}.${bsn}
      export FORT49=post${DTM}_sort_${fle_ext}.${bsn}

      if [[ $ii == 0 || $ii == 1 ]] ; then #basin est coast and gulf of MX no surge_tide run (surge + tideOnly)
         fle_ext=surge
         tideYN=Y
         echo $imxb $jmxb $tideYN $basinNEP > dim${DTM}.surge_tide.${bsn}
         export FORT12=dim${DTM}.surge_tide.${bsn}
         export FORT34=fle40${DTM}.cmc_gec00.tide.${bsn}
      elif [[ $ii == 2 ]] ; then #Basin NEP
         basinNEP=Y
         echo $imxb $jmxb $tideYN $basinNEP > dim${DTM}.${fle_ext}.${bsn}
         export FORT12=dim${DTM}.${fle_ext}.${bsn}
      else
         echo $imxb $jmxb $tideYN $basinNEP > dim${DTM}.${fle_ext}.${bsn}
         export FORT12=dim${DTM}.${fle_ext}.${bsn}
      fi
   elif [ $RK -lt 68 ] ; then

      . prep_step

      export pgm="petss_post_sort"

      ii=$((RK-34))
      fle_ext=surge
      bsn=${bsn1[${ii}]}
      imxb=${imxb1[${ii}]}
      jmxb=${jmxb1[${ii}]}
      if [[ $ii == 2 ]] ; then #Basin NEP
         basinNEP=Y
      fi
      echo $imxb $jmxb $tideYN $basinNEP > dim${DTM}.${fle_ext}.${bsn}

      export FORT12=dim${DTM}.${fle_ext}.${bsn}
      export FORT51=ssgrid${DTM}.max.${fle_ext}.${cyc}${bsn}
      export FORT52=ssgrid${DTM}.min.${fle_ext}.${cyc}${bsn}
      export FORT53=ssgrid${DTM}.mean.${fle_ext}.${cyc}${bsn}
      export FORT54=ssgrid${DTM}.10p.${fle_ext}.${cyc}${bsn}
      export FORT55=ssgrid${DTM}.90p.${fle_ext}.${cyc}${bsn}
      export FORT56=ssgrid${DTM}.20p.${fle_ext}.${cyc}${bsn}
      export FORT57=ssgrid${DTM}.30p.${fle_ext}.${cyc}${bsn}
      export FORT58=ssgrid${DTM}.40p.${fle_ext}.${cyc}${bsn}
      export FORT59=ssgrid${DTM}.50p.${fle_ext}.${cyc}${bsn}
      echo exc_naefsn ${cyc} > post${DTM}_sort_${fle_ext}.${bsn}
      export FORT49=post${DTM}_sort_${fle_ext}.${bsn}

   elif [ $RK -lt 102 ] ; then

      . prep_step

      export pgm="petss_post_sort"

      ii=$((RK-68))
      fle_ext=surge_tide
      bsn=${bsn1[${ii}]}
      imxb=${imxb1[${ii}]}
      jmxb=${jmxb1[${ii}]}

      export FORT51=ssgrid${DTM}.1ft.chance.${fle_ext}.${cyc}${bsn}
      export FORT52=ssgrid${DTM}.2ft.chance.${fle_ext}.${cyc}${bsn}
      export FORT53=ssgrid${DTM}.3ft.chance.${fle_ext}.${cyc}${bsn}
      export FORT54=ssgrid${DTM}.6ft.chance.${fle_ext}.${cyc}${bsn}
      export FORT55=ssgrid${DTM}.10ft.chance.${fle_ext}.${cyc}${bsn}
      export FORT56=ssgrid${DTM}.13ft.chance.${fle_ext}.${cyc}${bsn}
      export FORT57=ssgrid${DTM}.16ft.chance.${fle_ext}.${cyc}${bsn}

      export FORT58=ssgrid${DTM}.0ft.chance.${fle_ext}.${cyc}${bsn}
      export FORT59=ssgrid${DTM}.4ft.chance.${fle_ext}.${cyc}${bsn}
      export FORT60=ssgrid${DTM}.5ft.chance.${fle_ext}.${cyc}${bsn}
      export FORT61=ssgrid${DTM}.7ft.chance.${fle_ext}.${cyc}${bsn}
      export FORT62=ssgrid${DTM}.8ft.chance.${fle_ext}.${cyc}${bsn}
      export FORT63=ssgrid${DTM}.9ft.chance.${fle_ext}.${cyc}${bsn}
      export FORT64=ssgrid${DTM}.15ft.chance.${fle_ext}.${cyc}${bsn}

      echo pro_naefsn ${cyc} > post1${DTM}_sort_${fle_ext}.${bsn}
      export FORT49=post1${DTM}_sort_${fle_ext}.${bsn}

      if [[ $ii == 0 || $ii == 1 ]] ; then #basin est coast and gulf of MX no surge_tide run (surge + tideOnly)
         fle_ext=surge
         tideYN=Y
         echo $imxb $jmxb $tideYN $basinNEP > dim1${DTM}.surge_tide.${bsn}
         export FORT12=dim1${DTM}.surge_tide.${bsn}
         export FORT34=fle40${DTM}.cmc_gec00.tide.${bsn}
      elif [[ $ii == 2 ]] ; then #Basin NEP
         basinNEP=Y
         echo $imxb $jmxb $tideYN $basinNEP > dim1${DTM}.${fle_ext}.${bsn}
         export FORT12=dim1${DTM}.${fle_ext}.${bsn}
      else
         echo $imxb $jmxb $tideYN $basinNEP > dim1${DTM}.${fle_ext}.${bsn}
         export FORT12=dim1${DTM}.${fle_ext}.${bsn}
      fi
   elif [ $RK -lt 136 ] ; then

      . prep_step

      export pgm="petss_post_sort"

      ii=$((RK-102))
      fle_ext=surge
      bsn=${bsn1[${ii}]}
      imxb=${imxb1[${ii}]}
      jmxb=${jmxb1[${ii}]}

      if [[ $ii == 2 ]] ; then #Basin NEP
         basinNEP=Y
      fi
      echo $imxb $jmxb $tideYN $basinNEP > dim1${DTM}.${fle_ext}.${bsn}

      export FORT12=dim1${DTM}.${fle_ext}.${bsn}
      export FORT51=ssgrid${DTM}.1ft.chance.${fle_ext}.${cyc}${bsn}
      export FORT52=ssgrid${DTM}.2ft.chance.${fle_ext}.${cyc}${bsn}
      export FORT53=ssgrid${DTM}.3ft.chance.${fle_ext}.${cyc}${bsn}
      export FORT54=ssgrid${DTM}.6ft.chance.${fle_ext}.${cyc}${bsn}
      export FORT55=ssgrid${DTM}.10ft.chance.${fle_ext}.${cyc}${bsn}
      export FORT56=ssgrid${DTM}.13ft.chance.${fle_ext}.${cyc}${bsn}
      export FORT57=ssgrid${DTM}.16ft.chance.${fle_ext}.${cyc}${bsn}

      export FORT58=ssgrid${DTM}.0ft.chance.${fle_ext}.${cyc}${bsn}
      export FORT59=ssgrid${DTM}.4ft.chance.${fle_ext}.${cyc}${bsn}
      export FORT60=ssgrid${DTM}.5ft.chance.${fle_ext}.${cyc}${bsn}
      export FORT61=ssgrid${DTM}.7ft.chance.${fle_ext}.${cyc}${bsn}
      export FORT62=ssgrid${DTM}.8ft.chance.${fle_ext}.${cyc}${bsn}
      export FORT63=ssgrid${DTM}.9ft.chance.${fle_ext}.${cyc}${bsn}
      export FORT64=ssgrid${DTM}.15ft.chance.${fle_ext}.${cyc}${bsn}

      echo pro_naefsn ${cyc} > post1${DTM}_sort_${fle_ext}.${bsn}
      export FORT49=post1${DTM}_sort_${fle_ext}.${bsn}

   fi
   export FORT11=ssgrid${DTM}.gec00.${fle_ext}.${cyc}${bsn}

   export FORT113=fle40${DTM}.gec00.${fle_ext}.${bsn}
   export FORT114=fle40${DTM}.gep01.${fle_ext}.${bsn}
   export FORT115=fle40${DTM}.gep02.${fle_ext}.${bsn}
   export FORT116=fle40${DTM}.gep03.${fle_ext}.${bsn}
   export FORT117=fle40${DTM}.gep04.${fle_ext}.${bsn}
   export FORT118=fle40${DTM}.gep05.${fle_ext}.${bsn}
   export FORT119=fle40${DTM}.gep06.${fle_ext}.${bsn}
   export FORT120=fle40${DTM}.gep07.${fle_ext}.${bsn}
   export FORT121=fle40${DTM}.gep08.${fle_ext}.${bsn}
   export FORT122=fle40${DTM}.gep09.${fle_ext}.${bsn}
   export FORT123=fle40${DTM}.gep10.${fle_ext}.${bsn}
   export FORT124=fle40${DTM}.gep11.${fle_ext}.${bsn}
   export FORT125=fle40${DTM}.gep12.${fle_ext}.${bsn}
   export FORT126=fle40${DTM}.gep13.${fle_ext}.${bsn}
   export FORT127=fle40${DTM}.gep14.${fle_ext}.${bsn}
   export FORT128=fle40${DTM}.gep15.${fle_ext}.${bsn}
   export FORT129=fle40${DTM}.gep16.${fle_ext}.${bsn}
   export FORT130=fle40${DTM}.gep17.${fle_ext}.${bsn}
   export FORT131=fle40${DTM}.gep18.${fle_ext}.${bsn}
   export FORT132=fle40${DTM}.gep19.${fle_ext}.${bsn}
   export FORT133=fle40${DTM}.gep20.${fle_ext}.${bsn}
   export FORT134=fle40${DTM}.gep21.${fle_ext}.${bsn}
   export FORT135=fle40${DTM}.gep22.${fle_ext}.${bsn}
   export FORT136=fle40${DTM}.gep23.${fle_ext}.${bsn}
   export FORT137=fle40${DTM}.gep24.${fle_ext}.${bsn}
   export FORT138=fle40${DTM}.gep25.${fle_ext}.${bsn}
   export FORT139=fle40${DTM}.gep26.${fle_ext}.${bsn}
   export FORT140=fle40${DTM}.gep27.${fle_ext}.${bsn}
   export FORT141=fle40${DTM}.gep28.${fle_ext}.${bsn}
   export FORT142=fle40${DTM}.gep29.${fle_ext}.${bsn}
   export FORT143=fle40${DTM}.gep30.${fle_ext}.${bsn}
   

   export FORT144=fle40${DTM}.cmc_gec00.${fle_ext}.${bsn}
   export FORT145=fle40${DTM}.cmc_gep01.${fle_ext}.${bsn}
   export FORT146=fle40${DTM}.cmc_gep02.${fle_ext}.${bsn}
   export FORT147=fle40${DTM}.cmc_gep03.${fle_ext}.${bsn}
   export FORT148=fle40${DTM}.cmc_gep04.${fle_ext}.${bsn}
   export FORT149=fle40${DTM}.cmc_gep05.${fle_ext}.${bsn}
   export FORT150=fle40${DTM}.cmc_gep06.${fle_ext}.${bsn}
   export FORT151=fle40${DTM}.cmc_gep07.${fle_ext}.${bsn}
   export FORT152=fle40${DTM}.cmc_gep08.${fle_ext}.${bsn}
   export FORT153=fle40${DTM}.cmc_gep09.${fle_ext}.${bsn}
   export FORT154=fle40${DTM}.cmc_gep10.${fle_ext}.${bsn}
   export FORT155=fle40${DTM}.cmc_gep11.${fle_ext}.${bsn}
   export FORT156=fle40${DTM}.cmc_gep12.${fle_ext}.${bsn}
   export FORT157=fle40${DTM}.cmc_gep13.${fle_ext}.${bsn}
   export FORT158=fle40${DTM}.cmc_gep14.${fle_ext}.${bsn}
   export FORT159=fle40${DTM}.cmc_gep15.${fle_ext}.${bsn}
   export FORT160=fle40${DTM}.cmc_gep16.${fle_ext}.${bsn}
   export FORT161=fle40${DTM}.cmc_gep17.${fle_ext}.${bsn}
   export FORT162=fle40${DTM}.cmc_gep18.${fle_ext}.${bsn}
   export FORT163=fle40${DTM}.cmc_gep19.${fle_ext}.${bsn}
   export FORT164=fle40${DTM}.cmc_gep20.${fle_ext}.${bsn}

   if [ ${RK} -lt 68 ] ; then #exceedence height
  
      startmsg
      $EXECpetss/petss_post_sort >> $pgmout 2> errfile_${RANKO}
      err=$?;export err; err_chk
      echo "Grid products exceed height in ${bsn} is completed" > msg_grid${DTM}_${bsn}_${fle_ext}_h.txt

   elif [ ${RK} -lt 136 ]; then #probability of height
   
      startmsg
      $EXECpetss/petss_post_sort >> $pgmout 2> errfile_${RANKO}
      err=$?;export err; err_chk
      echo "Grid products in proba of height ${bsn} is completed" > msg_grid${DTM}_${bsn}_${fle_ext}_p.txt

   fi

   exit
