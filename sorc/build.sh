#!/bin/bash

#------------------------------------------------------------------------------
# build.sh                                              Last Change: 2021-09-09
#                                                          Huiqing.Liu@noaa.gov
#                                                              NWS/OSTI/MDL/DSD
#------------------------------------------------------------------------------
if [[ $# -gt 2 ]] || [[ $1 == "help" ]] ; then
   echo -e "Build script for P-ETSS executables.\n"
   echo "Usage:"
   echo "  $0 help             - Display this message and quit"
   echo "  $0                  - Make all exec in each .cd and .fd subdir"
   echo "  $0 install          - Install all exec in each .cd and .fd subdir"
   echo "  $0 clean            - Clean all .cd and .fd sub-directory"
   echo "  $0 <dir w/o .cd> make    - Make <dir>.cd sub-directory"
   echo "  $0 <dir w/o .cd> install - Install <dir>.cd sub-directory"
   echo "  $0 <dir w/o .cd> clean   - Clean <dir>.cd sub-directory"  
   exit
fi

#================================================================== START =====

# Parse user input
if [[ $# -eq 2 ]] ; then
   DIR=$1
   CMD=$2   
elif [[ $# -eq 1 ]] ; then
   CMD=$1
elif [[ $# -eq 0 ]] ; then
   CMD=''
fi
if [[ ${CMD} != "" ]] && [[ ${CMD} != "clean" ]] && [[ ${CMD} != "install" ]]  &&
   [[ ${CMD} != "make" ]]; then
   echo "Unrecognized command '${CMD}'"
   $0 help
   exit
fi

srcDir=$(cd "$(dirname "$0")" && pwd)

#---------------------------------------
# Load the build module
#---------------------------------------
module reset
source $srcDir/../versions/build.ver
module use ./
module load build_petss

if [[ $# -eq 1 ]] || [[ $# -eq 0 ]]; then
   make -f Makefile.petss.wcoss2 ${CMD}
else
   if [[ ${CMD} == 'make' ]]; then
      CMD=""
   fi
   if [[ -d $1.cd ]] ; then
      cd $1.cd
   elif [[ -d $1.fd ]] ; then
      cd $1.fd
   elif [[ -d $1 ]] ; then
      cd $1
   else
      echo "Couldn't find $1.cd or $1.fd"
      exit
   fi
   make ${CMD}
fi	
