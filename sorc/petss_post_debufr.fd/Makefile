SHELL = /bin/sh

EXEDIR = ../../exec
EXE = petss_post_debufr

CC = ${COMPC}
FC = ${COMP}
CCFLAGS = -DUNDERSCORE

LDFLAGS = -lifcore $(BUFR_LIBd_DA)

INC     = -I$(BUFR_INCd_DA)
FFLAGS += -I. $(INC)

SOURCE = debufr.c

FSOURCE = defgridstns.f\
	griddata.f \
	numlines.f \
	myufdump.f \
	myfdebufr.f

OBJECTS = $(SOURCE:.c=.o) \
	$(FSOURCE:.f=.o)

#SRC = debufr.o

all: $(OBJECTS)
	$(CC) $(OBJECTS) -o ${EXE} $(LDFLAGS)

install: all
	mv $(EXE) $(EXEDIR)
	rm -f $(EXE)

%.o: %.c
	$(CC) -c $(CCFLAGS) $? 
%.o: %.f
	$(FC) -c $(FFLAGS) $?

clean:
	rm -f *.o *.a *.bak *.mod $(EXE)

