c----------------------------------------------------------------------
c combineObs.f
c----------------------------------------------------------------------
c Author: Ryan Schuster (ryan.schuster@noaa.gov)
c History: Program created in 2014
c Updated by Huiqing Liu in March 2016
c-----------------------------------------------------------------------
c Abstract:
c  Reads in parsed BUFR water level obs, arranges them by time stamp
c  and location, and places them on a grid where rows represent time
c  and columns represent location. Obs are in MLLW
c 1) Sets up relevant filenames from envir variables; gets relevant
c    info from master file; gets list of dates from datelist;
c    initializes the grid.
c 2) Reads in parsed BUFR files from the last five days (5-day
c    hindcast) and arranges them on the grid. Filters for bad or
c    missing data. 
c 3) The BUFR parsing process has already placed the BUFR data onto
c    grids, so this program acts as more of a stitching-together
c    algorithm to combine all 6 days of BUFR files into one large
c    grid spanning the entire time period.
c Parameters:
c  Inputs: none
c  Outputs: obsGrid - Grid of water level obs for the last five days,
c                     as well as those from the current day
c----------------------------------------------------------------------
C  Combine the 12 obs grids into one by looping through
C  each one until you find a non-empty data point. Place
C  said data point in the appropriate place on the final grid

      PROGRAM COMBINEOBS

      USE DEFGRIDSTNS
!----------------------------------------------
! 08/2016 H.Liu increase forecast hour from 96 to 102
! 5 days + 102 hrs = 13320 mins and divide by 6 mins step + 1 equals 2221
! lines in datelist files
! 08/2018 H.Liu skip missed obs (missed bufr tank)
!----------------------------------------------
!      CHARACTER(12) DATES(2161)
!
      INTEGER :: NDATES
      CHARACTER(12) ,  ALLOCATABLE :: DATES(:)

!      CHARACTER(12) DATES(2221)
!----------------------------------------------
      CHARACTER(255) DATELIST

C     Input arrays
      INTEGER, ALLOCATABLE :: SARR12(:),SARR05(:),SARR102(:)

      CHARACTER*8 DAY
      CHARACTER(255) SHEF12,SHEF05,SHEF102

C     Indices
      INTEGER II,JJ,KK,LL,MM,NN,OO,PP,QQ
C     Data points
      INTEGER SDTPT12,SDTPT05,SDTPT102
C     IO statuses
      INTEGER STEST12,STEST05,STEST102

C     Number of stations
      INTEGER NLINES
      CHARACTER(255) MASTER
      CHARACTER(13) FMTSTR

C     Output files
      CHARACTER(255) OBSFNS
      CHARACTER(255) OBSGRID
!  Test file if exist
      LOGICAL THERE12, THERE05, THERE102
C  -----------------------------------------------------------
C  Get file names from environment
      CALL GET_ENVIRONMENT_VARIABLE('FORT11',MASTER)
      CALL GET_ENVIRONMENT_VARIABLE('FORT12',DATELIST)
      CALL GET_ENVIRONMENT_VARIABLE('FORT13',OBSFNS)
      CALL GET_ENVIRONMENT_VARIABLE('FORT51',OBSGRID)
      CALL GET_ENVIRONMENT_VARIABLE('FORT54',SHEF12)
      CALL GET_ENVIRONMENT_VARIABLE('FORT55',SHEF05)
      CALL GET_ENVIRONMENT_VARIABLE('FORT56',SHEF102)

C  Get number of stations
      CALL NUMLINES(TRIM(MASTER),NLINES)
      CALL NUMLINES(TRIM(DATELIST),NDATES)

      ALLOCATE(DATES(NDATES))

!      ALLOCATE(GRID(2161,NLINES))
!      ALLOCATE(GRID(2221,NLINES))
      ALLOCATE(GRID(NDATES,NLINES))

      ALLOCATE(SARR12(NLINES))
      ALLOCATE(SARR05(NLINES))
      ALLOCATE(SARR102(NLINES))

C     Write format string
      WRITE (FMTSTR,'("(",I4,"(I6,1X))")') NLINES

C  Get list of stations and dates
      OPEN(12,FILE=TRIM(DATELIST),ACTION='READ') 

C  Initialize the grid with 9999's
      DO JJ = 1,NDATES
         READ(12,'(A12)'), DATES(JJ)
         DO KK = 1,NLINES
            GRID(JJ,KK) = 9999
         ENDDO
      ENDDO
      CLOSE(12)

C  Read in list of file names to open
      OPEN(13,FILE=TRIM(OBSFNS), ACTION='READ')
      OPEN(51,FILE=TRIM(OBSGRID),ACTION='WRITE')

      DO LL = 1,6
        READ(13,'(A8)') DAY
        
        SHEF12((LEN(TRIM(SHEF12))-14):(LEN(TRIM(SHEF12))-7)) = DAY
        SHEF05((LEN(TRIM(SHEF05))-14):(LEN(TRIM(SHEF05))-7)) = DAY
        SHEF102((LEN(TRIM(SHEF102))-14):(LEN(TRIM(SHEF102))-7)) = DAY
! H.Liu AceInfo/MDL 08/2018
! Check file if exist: (1) file exist open it and read it
!                      (2) file no exist and then assign 9999 to SARR12 and SARR05        
        INQUIRE(FILE=TRIM(SHEF12),EXIST=THERE12)
        INQUIRE(FILE=TRIM(SHEF05),EXIST=THERE05)
        INQUIRE(FILE=TRIM(SHEF102),EXIST=THERE102)
        
        IF (THERE12) THEN
           OPEN(54,FILE=TRIM(SHEF12),ACTION='READ')
        ENDIF
        IF (THERE05) THEN
           OPEN(55,FILE=TRIM(SHEF05),ACTION='READ')
        ENDIF
        IF (THERE102) THEN
           OPEN(56,FILE=TRIM(SHEF102),ACTION='READ')
        ENDIF


        OUTER: DO MM = 1,SIZE(DATES)
          IF (THERE12) THEN
             READ(54,FMTSTR,IOSTAT=STEST12) (SARR12(NN),NN=1,NLINES) 
          ELSE
             SARR12 = 9999
          ENDIF

          IF (THERE05) THEN
             READ(55,FMTSTR,IOSTAT=STEST05) (SARR05(NN),NN=1,NLINES) 
          ELSE
             SARR05 = 9999
          ENDIF

          IF (THERE102) THEN
             READ(56,FMTSTR,IOSTAT=STEST102) (SARR102(NN),NN=1,NLINES)
          ELSE
             SARR102 = 9999 
          ENDIF 

! end H.Liu 08/2018
          DO OO = 1,NLINES
            SDTPT12 = SARR12(OO)
            SDTPT05 = SARR05(OO)
            SDTPT102 = SARR102(OO)
            IF (STEST12.LT.0) THEN
C             End of File
              CLOSE(54)
              CLOSE(55)
              close(56)
              EXIT OUTER
            ENDIF
C           Starts at beginning of row and reads each col,
C           then starts at the next row, etc.
            IF (SDTPT12.NE.9999 .AND. SDTPT12.LT.999999
     &          .AND. SDTPT12.GT.-99999) THEN
              IF ((SDTPT12.LT.1 .AND. SDTPT12.GT.0) .OR.
     &             (SDTPT12.GT.-1 .AND. SDTPT12.LT.0)) THEN
C               Catch really small numbers once we've eliminated
C               bad or missing data. Round #s less than abs(+/-0.001) to 0
                SDTPT12 = 0
              END IF
              GRID(MM,OO) = SDTPT12
            ELSEIF (SDTPT05.NE.9999 .AND. SDTPT05.LT.999999
     &          .AND. SDTPT05.GT.-99999) THEN
              IF ((SDTPT05.LT.1 .AND. SDTPT05.GT.0) .OR.
     &             (SDTPT05.GT.-1 .AND. SDTPT05.LT.0)) THEN
                 SDTPT05 = 0
              ENDIF
              GRID(MM,OO) = SDTPT05
            ELSEIF (SDTPT102.NE.9999 .AND. SDTPT102.LT.999999
     &          .AND. SDTPT102.GT.-99999) THEN
              IF ((SDTPT102.LT.1 .AND. SDTPT102.GT.0) .OR.
     &             (SDTPT102.GT.-1 .AND. SDTPT102.LT.0)) THEN
                 SDTPT102 = 0
              ENDIF
              GRID(MM,OO) = SDTPT102 

            ENDIF

          ENDDO
        ENDDO OUTER
        IF (THERE12) THEN
           CLOSE(54)
        ENDIF
        IF (THERE05) THEN
           CLOSE(55)
        ENDIF
      ENDDO

!      DO PP=1,2161
      DO PP=1,NDATES
        WRITE(51,FMTSTR) (GRID(PP,QQ), QQ=1,NLINES)
      ENDDO

      CLOSE(13)
      CLOSE(51)

      DEALLOCATE(DATES)
      DEALLOCATE(GRID)
      DEALLOCATE(SARR12)
      DEALLOCATE(SARR05)
      DEALLOCATE(SARR102)

      END
