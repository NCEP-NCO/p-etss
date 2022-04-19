SHELL = /bin/sh

EXEDIR = ../../exec

FC= ${COMP}
FCFLAGS=-assume no2underscores

SRC = defgridstns.f numlines.f makeSHEF.f
MAIN = combineAll.f
OBJS = $(addsuffix .o,$(basename $(SRC)))
EXE = petss_post_combAll

all:$(OBJS)
	$(FC) $(OBJS) $(MAIN) -o $(EXE)

install: all
	mv $(EXE) $(EXEDIR)
	rm -f $(EXE)

%.o: %.f
	$(FC) $(FCFLAGS) -c $(SRC)

clean:
	rm -f *.o *.a *.bak *.mod $(EXE)

