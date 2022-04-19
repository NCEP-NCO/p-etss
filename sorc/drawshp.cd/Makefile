#######################################################################
# Makefile                                    Arthur.Taylor@noaa.gov
#                              Decision Support Branch, MDL/OSTI/NWS
#                                                       Dec 12, 2017
# Makefile for drawshp
# Usage:
#   make         - Build the executable
#   make install - Move the built executable to the exec dir
#   make clean   - Start with a clean slate
#######################################################################
CC      = $(C_COMP)

# Various directories
BINDIR = ../../exec
LIBDIR = ../../lib
EMAPF_SRC = $(LIBDIR)/sorc/emapf-c
GD_SRC = /usr/include

# Compiler flags
CFLAGS = -O3 -Wall
# CFLAGS += -strict-ansi
# CFLAGS += -fsigned-char -Wextra -pedantic -ansi

# Program #defines
CFLAGS += -DSIZEOF_LONG_INT=8# # Compile on a 64 bit platform
CFLAGS += -D_LINUX_#           # Assume linux (rather than windows)

# Include paths
INC     = -I$(EMAPF_SRC)
INC    += -I$(GD_SRC)
CFLAGS += -I. $(INC)

# Libraries and linker flags
EMAPF_LIB = $(LIBDIR)/libemapf.a
GD_LIB = -L /usr/lib64/ -lgd
LIBS    = $(GD_LIB) $(PNG_LIB) $(Z_LIB) $(EMAPF_LIB)
LDFLAGS =

# Source, Headers and Objects
SOURCE=myassert.c
SOURCE+=myutil.c
SOURCE+=tendian.c
SOURCE+=mapini.c
SOURCE+=drawlib.c

HEADERS=myassert.h
HEADERS+=myutil.h
HEADERS+=tendian.h
HEADERS+=mapini.h
HEADERS+=drawlib.h

OBJS=$(SOURCE:.c=.o)

# File with 'main()'
MAIN=drawshp.c

CMD     = drawshp
# CMDFLAGS  = $(CFLAGS) -DPKGVERS=\"1.1\" -DPKGDATE=\"2016-03-16\"
# CMDFLAGS += $(CFLAGS) -DHOLD_CONSTANT#  # Allow stationary storms during cubic spline

#######################################################################
# Actual rules to build the program
#######################################################################
all: $(CMD)

$(CMD): $(OBJS) $(HEADERS) $(MAIN)
	$(CC) $(MAIN) $(CMDFLAGS) $(LDFLAGS) $(OBJS) $(LIBS) -o $(CMD)

install: $(CMD)
	mv $(CMD) ${BINDIR}/

clean:
	-rm -f $(OBJS) $(CMD)
#	(cd $(EMAPF_SRC) && make clean)
#	(cd $(GD_SRC) && make clean)
#	-rm -f $(EMAPF_LIB) $(GD_LIB)


.c.o: $(HEADERS)
	$(CC) -c $(CFLAGS) $<

$(EMAPF_LIB): $(EMAPF_SRC)/Makefile
	(cd $(EMAPF_SRC) && make clean install)

$(GD_LIB): $(GD_SRC)/Makefile
	(cd $(GD_SRC) && make clean)
	(cd $(GD_SRC) && make)
	cp $(GD_SRC)/libgd.a $(GD_LIB)

