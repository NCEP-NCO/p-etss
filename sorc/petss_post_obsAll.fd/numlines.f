c-----------------------------------------------------------------------
c numlines.f
c-----------------------------------------------------------------------
c Author: Ryan Schuster (ryan.schuster@noaa.gov)
c History: Program created in 2014
c Abstract:
c Counts the number of lines in a given file
c Used primarly to find the length of parm/master.csv, i.e. the number
c of stations the code is running for
c
c Parameters:
c Inputs:  FN - filename to count lines of
C Outputs: NLINES - number of lines in the file
c-----------------------------------------------------------------------

      SUBROUTINE NUMLINES(FN,NLINES)

      CHARACTER*(*) FN
      INTEGER NLINES

C ---------------------------------------------------------------------
      NLINES = 0

      OPEN(1,FILE=FN,ACTION='READ',STATUS='OLD')
      DO 
        READ(1,*,END=10)
        NLINES = NLINES + 1
      ENDDO

 10   CLOSE(1)

      END
