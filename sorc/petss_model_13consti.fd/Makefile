#!/bin/make
SHELL = /bin/bash

############################
# This makefile is intended to create the Linux executable.
#
# USAGE:
#    make B=gcc | B=intel (B is for build)
#
# Other options...
#    make MEM=true, DEBUG=true, BIT=32 | 64, DOUBLE=true, MPI=true, EXPR=true
#    EXPR=true means enable experimental options.
#       EXPR should be false for FOIA CD so it has well tested options.
############################
B=intel
MEM=false
DEBUG=false
BIT=64
DOUBLE=false
MPI=false
EXPR=true

############################
# Set up the binary files and version info.
############################
HEAD = /usr/bin/head
#CUT = /bin/cut
CP = /bin/cp
RM = /bin/rm
ifeq ($(B),intel)
  ifeq ($(MPI),true)
    CC = mpiicc
  else
    CC = ${COMPC}
  endif
  FC = ${COMP}
else
  ifeq ($(MPI),true)
    CC = mpicc
  else
    CC = gcc
  endif
  FC = gfortran
endif
STRIP = strip

############################
# Set up Targets
############################
ifeq ($(MPI),true)
 PRJ_NAME = petss_model_13consti_MPI
else
 PRJ_NAME = petss_model_13consti
endif
TARGETS = $(PRJ_NAME)
SUFFIX =

############################
# Set up Compiler flags...
#   CFLAGS (via STD_DEF and STD_INC) and FFLAGS
############################
# _LINUX_ ==> Indicates this is linux (vs aix)
STD_DEF = -D_LINUX_

# _GCC_ ==> Means use GCC wrappings for connecting to fortran.
STD_DEF += -D_GCC_

# SIZEOF_LONG_INT ==> long int is 8 vs 4 bytes (file I/O)
ifeq ($(BIT),64)
  STD_DEF += -DSIZEOF_LONG_INT=8
endif

# CC_VER ==> embeds the compiler version into exectuable.
#CC_VER := $(shell $(CC) --version | $(HEAD) -n1 | $(CUT) -d' ' -f2,3 --output-delimiter='-')
CC_VER :=
STD_DEF += -DCC_VER="\"$(CC_VER)\""

# Compile with double precision flags.
ifeq ($(DOUBLE),true)
  STD_DEF += -DDOUBLE_FORTRAN
endif

# Compile with MPI
ifeq ($(MPI),true)
  STD_DEF += -D_MPI_
endif

# Allow for MemWatch diagnostics.
ifeq ($(MEM),true)
  STD_DEF += -DMEMWATCH -DMEMWATCH_STDIO -DDEBUG
  MEMWATCH = memwatch.o
  STD_INC = -I./memwatch
else
  ifeq ($(DEBUG),true)
    STD_DEF += -DDEBUG
  endif
  MEMWATCH =
endif

# check to see if EXPR options should be enabled (not true in FOIA case)
ifeq ($(EXPR),true)
  STD_DEF += -D_EXPR_
endif

ifeq ($(MPI),true)
  STD_INC += $(MPI_INC)
endif
STD_INC += -I.

# Default compiler flags
ifeq ($(B),intel)
  CFLAGS = ${optFlag} -w 
  FFLAGS = ${optFlag} -w 
  #FFLAGS = ${optFlag} -w -xCORE-AVX2 -hfp3
  #FFLAGS = ${optFlag} -w -check bounds
else
  CFLAGS = -O3 -Wall -Wextra -pedantic -ansi
  FFLAGS = -O0
endif

# Enhance compiler flags
ifeq ($(BIT),32)
  CFLAGS += -m32
  FFLAGS += -m32
endif
CFLAGS += $(STD_DEF) $(STD_INC)

ifeq ($(DOUBLE),true)
  ifeq ($(FC),ifort)
    FFLAGS += -r8
  else
    FFLAGS += -fdefault-real-8
  endif
endif

############################
# Set up Linker flags...
#    STD_LIB and LD_FLAGS
############################
ifeq ($(B),intel)
  ifeq ($(BIT),32)
    # Following tells it where the runtime library is.
    LD_FLAGS = -Wl,-R$(INTEL_LIB)
    ifeq ($(MPI),true)
      LD_FLAGS += -Wl,$(MPI_RUNLIB)
    endif
  else
    LD_FLAGS =
  endif
  #STD_LIB = -lifcore -limf -lintlc
  STD_LIB = -L$(INTEL_LIB) -lm
else
  LD_FLAGS =
  STD_LIB = -lgfortran
  ifeq ($(BIT),32)
    STD_LIB += -L/usr/lib -lm
  else
    STD_LIB += -L/usr/lib64 -lm
  endif
endif

ifeq ($(MPI),true)
  STD_LIB += $(MPI_LIB)
endif

############################
# FILES
############################
SOURCE = amyutil.c \
         clock.c \
         complex.c \
         mpiutil.c \
         myassert.c \
         myutil.c \
         rex.c \
         pack.c \
         savellx.c \
         setup.c \
         slosh2.c \
         tendian.c \
         tideutil.c \
         tio3.c \
         usrparse.c \
         cstart.c

FSOURCE = parm_gfs.f \
          etsssub_naefs.f \
          nestbound.f \
          intrface_naefs.f \
          runslhg.f \
          spinup.f

OBJECTS = $(SOURCE:.c=.o) \
          $(FSOURCE:.f=.o) \
          $(MEMWATCH)

HEADERS = amyutil.h \
          clock.h \
          complex.h \
          halotype.h \
          mpiutil.h \
          myassert.h \
          myutil.h \
          pack.h \
          parm.for \
          parmmsy.for \
          rex.h \
          savellx.h \
          setup.h \
          slosh2.h \
          tendian.h \
          tideutil.h \
          tio3.h \
          type.h \
          usrparse.h

C_MAIN = 
#LIBS = -L/nwprod/lib -lw3nco_4
LIBS = ${W3NCO_LIB4}

############################
# TARGETS
############################
.PHONY: all
all: $(TARGETS)
	@echo ""

.PHONY: help
help:
	@echo "make B=(gcc | intel)"
	@echo "     other options: MEM=true, DEBUG=true, BIT=(32 | 64), DOUBLE=true, MPI=true, EXPR=true"

$(PRJ_NAME): $(OBJECTS) $(C_MAIN) $(HEADERS)
#	$(CC) $(C_MAIN) $(CFLAGS) $(LD_FLAGS) $(OBJECTS) $(STD_LIB) -o $(PRJ_NAME)
	$(FC) -nofor_main $(C_MAIN) $(CFLAGS) $(LD_FLAGS) $(OBJECTS) $(STD_LIB) -o $(PRJ_NAME) $(LIBS)
	$(STRIP) -s $(PRJ_NAME)$(SUFFIX)

.PHONY: install
install: $(TARGETS)
	mv $(PRJ_NAME)$(SUFFIX) ../../exec

.PHONY: clean
clean:
	$(RM) -f *.o *.bak *.BAK *.mod $(PRJ_NAME)

############################
# SUFFIXES
############################
.c.o : $(HEADERS)
	$(CC) -c $(CFLAGS) $<

.f.o :
	$(FC) -c $(FFLAGS) $<

############################
# Dependencies
############################
memwatch.o: ./memwatch/memwatch.c ./memwatch/memwatch.h
	$(CC) -c $(CFLAGS) ./memwatch/memwatch.c
