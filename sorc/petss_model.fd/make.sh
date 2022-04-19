#!/bin/sh

if [[ $# -ne 1 ]] ; then
cat << EOF
usage $0 <dos|all> (which will build FOIA release)

Altneratively... 
  make -f makefile.dos.gcc342 clean install
    should build sloshDos.exe (rename from sloshDosF.exe)
  make -f makefile.win.gcc342 clean install
    should build sloshGui.exe and sloshDosN.exe (was known as sloshDos.exe)

Note...
  diff of makefile.dos and makefile.win should be very similar at top.

Note...
  Check alias for different MingCompilers.

EOF
exit
fi

make -f makefile.dos B=gcc461 DOUBLE=false clean install
make -f makefile.dos B=gcc450 DOUBLE=false clean install
make -f makefile.dos B=gcc453 DOUBLE=false MPI=true clean install
make -f makefile.dos B=gcc453 DOUBLE=true MPI=true clean install
make -f makefile.dos B=gcc342 DOUBLE=false clean install
# Last one is 450 double so it overwrites the default sloshDos
make -f makefile.dos B=gcc450 DOUBLE=true clean install

if [[ $1 == "dos" ]] ; then 
  exit
fi

make -f makefile.win B=gcc461 DOUBLE=false clean install
make -f makefile.win B=gcc450 DOUBLE=false clean install
make -f makefile.win B=gcc342 DOUBLE=false clean install
# Last one is 450 double so it overwrites the default sloshGui
make -f makefile.win B=gcc450 DOUBLE=true clean install

exit
