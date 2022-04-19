C----------------------------------------------------------------------
C griddata.f
C----------------------------------------------------------------------
C Author: Ryan Schuster (ryan.schuster@noaa.gov)
C History: Program created in 2014
c Updated by Huiqing Liu in March 2016
c-----------------------------------------------------------------------
C Abstract:
C  Using a vector of stations encountered so far and a vector of 
C  all possible dates, place corresponding data value into a 2D 
C  array initialized with faux values.
C
C  Once you have a station, date, and data point, call this with a 
C  vector of all stations encountered so far and all possible dates.
C  Search the encountered station for your individual input
C  station. If your station has been encountered, go to that
C  column of the 2D array. If not, go to the next 'empty' column.
C
C  Once you're at the right column, match your individual input
C  date to one of all the possible dates. Get the date's index in
C  the array of all dates and go to that row. Figure out the precision
C  of your data point, multiply it by 10**precision, therefore turning
C  it into an integer and saving memory. Fill in the faux 2D array 
C  value with the data you have for this place and time.
C
C  When you're done filling out the 2D array, use it in the calling
C  program to make your calculations and write out to csv files
C  on a station-by-station, date-by-date basis.

C  Step 0: Initialize output grid with 9999's
C  Step 1: Choose a data point and its corresponding station and
C          time stamp. Turn it into an integer so it takes up less
C          memory
C  Step 2: Loop through station master list to find index of matching
C          station. This index = column number.
C  Step 3: Loop through date master list to find index of matching
C          date. This index = row number.
C  Step 4: Go to GRID(row,column) and replace initial 9999 value with 
C          the current data point
C 
C Parameters
C  INPUT: 
C        DATEIN (int) - Array of dates corresponding to input data
C        STNIN (char) - Array of stations corresponding to input data
C        DATAIN (float) - Input data
C  OUTPUT: 
C        GRID

      SUBROUTINE GRIDDATA(DATE,STN,DTPT,DATES)
C     Always pass in updated station and dates vectors

C     Read in COOPS to NDBC file to convert those stn names

      USE DEFGRIDSTNS
!------------------------------------------------------------------------
!H.Liu
!5 days + 102 hrs = 13320 mins and divide by 6 mins step + 1 equals 2221
! lines in datelist files
!-------------------------------------------------------------------------
!        CHARACTER(12) DATES(2161)
        CHARACTER(12) DATES(2221)

      CHARACTER(12) DATE
      CHARACTER(5) STN
      REAL*8 DTPT

      INTEGER DT

      INTEGER COL,ROW
C----------------------------------------------------------------------
C     Loop through input stations and dates to see where they match
C     the master lists of stations and dates
C     Fill in the grid with integer-ized data points in the appropriate
C     places
 
       DT = INT(DTPT*1000)

       COL = 0
       ROW = 0
       DO JJ = 1,SIZE(STNS)
          IF(STN.EQ.STNS(JJ)) THEN
             COL = JJ
             DO KK = 1,SIZE(DATES)
                IF(DATE.EQ.DATES(KK)) THEN
                   ROW = KK
                   GRID(ROW,COL) = DT
                   EXIT
                END IF
             END DO
             EXIT
          END IF
      END DO

      RETURN
      END

