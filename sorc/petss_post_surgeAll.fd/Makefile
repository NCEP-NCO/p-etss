SHELL = /bin/sh

EXEDIR = ../../exec
EXE = petss_post_surgeAll

FC = ${COMP}
FCFLAGS = -assume no2underscore

SRC = numlines.f
MAIN = getsurge.f
OBJS = $(addsuffix .o,$(basename $(SRC)))

all: $(OBJS)
	$(FC) $(OBJS) $(MAIN) -o $(EXE)

install: all
	mv $(EXE) $(EXEDIR)
	rm -f $(EXE)

%.o: %.f
	$(FC) $(FCFLAGS) -c $(SRC)

clean:
	rm -f *.o *.a *.bak $(EXE)

