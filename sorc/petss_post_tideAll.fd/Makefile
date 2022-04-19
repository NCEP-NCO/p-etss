SHELL = /bin/sh

EXEDIR = ../../exec
EXE = petss_post_tideAll

FC=${COMP}
FCFLAGS=-assume no2underscore -r8

SRC = gettide.f calctide.f sectide.f tidemaxmin.f numlines.f
OBJS = $(addsuffix .o,$(basename $(SRC)))

all: $(EXE)

$(EXE): $(OBJS)
	$(FC) $(OBJS) -o $(EXE)

install: $(EXE)
	mv $(EXE) $(EXEDIR)

%.o: %.f
	$(FC) $(FCFLAGS) -c $?

clean:
	rm -f *.o *.a *.bak *.mod $(EXE)
