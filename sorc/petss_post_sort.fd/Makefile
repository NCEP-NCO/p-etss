SHELL=/bin/sh
#
#Author: Huiqing.Liu/MDL Jan. 2016
#
# The parameters SRCS and OBJS should not need to be changed.  If, however,
# you need to add a new module add the name of the source module to the
# SRCS parameter and add the name of the resulting object file to the OBJS
# parameter.  
#
# Defining variables
SRCS= post_sort.f90 \
	grid_exceedance_all.f90 \
	grid_probabl_all.f90 \
	stn_sort_all.f90

OBJS=	$(SRCS:.f90=.o)
FC =	${COMP}
LIBS =
CMD =	petss_post_sort
PROFLIB =
#FFLAGS=-g -free -warn all
FFLAGS =	${optFlag}

all:	$(CMD)

$(CMD):	$(OBJS)
	$(FC) $(FFLAGS) -o $(@) $(OBJS) $(LIBS)

# Make the profiled version of the command and call it a.out.prof
#
$(CMD).prof:    $(OBJS)
	$(FC) $(FFLAGS) -o $(@) $(OBJS) $(PROFLIB) $(LIBS)

%.o: %.f90
	$(FC) -c $(FFLAGS) $<

install: $(CMD)
	mv $(CMD) ../../exec

clean:
	-rm -f $(OBJS) $(CMD) *.mod

clobber:        clean
	-rm -f $(CMD) $(CMD).prof

void:   clobber
	-rm -f $(SRCS) makefile


# End of the makefile

