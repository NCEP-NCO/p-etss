README.txt            P-ETSS's Help Pages                 Last Change: 2020-07-21

This file is intended to describe how to build the P-ETSS executables.

================================================================================
BUILD                                                                    *build*
    $ cd /sorc
    
  Load the 'cray' module file to setup the compiling environment
    $ module load ./build_petss.module.cray

  Build the executables (takes 13 min on dev machines)
    $ make -f Makefile.petss.cray clean install
    Alternative:
    $ make -f Makefile.petss.cray clean install > log.$(date +%Y%m%d_%H%M) 2>&1

  Clean up afterwards
    $ make -f Makefile.petss.cray clean
    $ cd ../

================================================================================
vim:ft=help:norl:fdm=indent:sw=4
