c----------------------------------------------------------------------
c getsurge.f
c----------------------------------------------------------------------
c Author: Ryan Schuster (ryan.schuster@noaa.gov)
c History: Program created in 2014
c Updated by Huiqing Liu in March 2016
c-----------------------------------------------------------------------
c Abstract:
c Called by the gridData routines. This script reads and parses the
c ETSS output text products to partition surge by location and time.
c It then places those surge values onto a grid where each row
c represents a time stamp (YYYYmmddHHMM) and each column represents
c a location.
c 1) Sets up relevant filenames from envir variables; gets relevant
c    info from master file; gets list of dates from datelist;
c    initializes the grid
c 2) Loops through 21 ETSS surge text product files (each one for a 
c    different date/time (YYYYmmdd_HH.ss)). Every file represents a
c    6-hour interval of surge. The latest file is used as the 
c    current surge record plus the 96-hour forecast (97 values total)
c 3) For each 6-hour time period, determines if a file is missing
c    and fills in the gaps with missing data values. If the forecast
c    (latest) file is missing, the second latest file is used as the
c    forecast file. If all data is missing, the code returns a grid
c    full of missing i.e. '9999' values.
c    a) The grid time stamps range from five days ago to 96 hours 
c       from now (5-day hindcast plus 4-day forecast).
c    b) Each surge text product contains hundreds of locations for
c       which surge is predicted. Predictions begin at forecast
c       cycle times (00Z, 06Z, etc) and extend out 96 hours.
c    b) Surge values are selected in 6-hour intervals from the
c       beginnings of each text product file and stitched together 
c       to make the hindcast.
c    c) The latest ETSS text product file is used to make the
c       96 hour forecast.
c 4) Writes the grid file to the appropriate place so it can be used
c    by sorc/combAll.fd/combineAll.f later.
c
c Parameters:
c  Inputs:  none
c  Outputs: surgeGrid - Grid containing all of the surge values
c                       parsed from the ETSS text products
c----------------------------------------------------------------------
!-----------------------------------------------------------------------
!Huiqing.Liu/MDL March 2017
!
! Adding 10% and 90% surge line
!
!-----------------------------------------------------------------------

      PROGRAM GETSURGE

C     Input list of stations and dates / output grid
!      CHARACTER(12) DATES(2161)
      INTEGER :: NDATES
      CHARACTER(12) ,  ALLOCATABLE :: DATES(:)

!      CHARACTER(12) DATES(2221)
      CHARACTER(255) DATELIST

      CHARACTER(7), ALLOCATABLE :: STIDS(:)
      CHARACTER(7) STID1,STID2
      CHARACTER(5) NWSLI
      INTEGER, ALLOCATABLE :: GRID(:,:,:)

C     Get number of stations
      INTEGER NLINES
      CHARACTER(255) MASTER
      CHARACTER(13) FMTSTR

C     Indices and file io test
      INTEGER II,JJ,KK,LL,MM,NN,OO,PP,QQ,RR,TT,UU,VV,WW,ZZ
      INTEGER TEST,SSIND,IND,DUM,FNUM
      CHARACTER(250) HEAD,FLINE,OLINE,NOPE

C     Output strings/arrays
!      REAL SS(217) ! 5 days*24 + 97 hours 
      REAL SS(223)  ! 5 dyas*24 + 103 hours

C     Input filename
      CHARACTER*11 LN
      CHARACTER*14 PHOLD
!    Added by Huiqing.Liu/MDL March 2017 
!    for 90p and 10p stormtide files
      CHARACTER*255 FN90p,FN10P,FN
      CHARACTER*4 PROD
      INTEGER ISURGE
!
      LOGICAL THERE
! Added by Huiqing.Liu AceInfo/MDL in Sept. 2018 to make sure surge file is not empty
      INTEGER ss_size,stny
! H.Liu /MDL June 2020 stny is a flag for co-ops id found or not (1/0)
! in stormsurge output text files
      CHARACTER(255) SSFNS
      CHARACTER(255) SSGRID
      CHARACTER(255) PRODF

C --------------------------------------------------------------
C  Get file units from environmental variables
      CALL GET_ENVIRONMENT_VARIABLE('FORT11',MASTER)
      CALL GET_ENVIRONMENT_VARIABLE('FORT12',DATELIST)
      CALL GET_ENVIRONMENT_VARIABLE('FORT14',SSFNS)
      CALL GET_ENVIRONMENT_VARIABLE('FORT18',PRODF)
      CALL GET_ENVIRONMENT_VARIABLE('FORT52',SSGRID)

C  Number of stations
      CALL NUMLINES(TRIM(MASTER),NLINES)
      CALL NUMLINES(TRIM(DATELIST),NDATES)

C     Change MFILE to the new master
!      ALLOCATE(GRID(2161,NLINES))
!      ALLOCATE(GRID(2221,NLINES))

      ALLOCATE(DATES(NDATES))
      ALLOCATE(GRID(NDATES,NLINES,3))

      ALLOCATE(STIDS(NLINES))

C     Initialize placeholder for last opened surge file
      PHOLD="NULL"
C Read prod file to determin if this is for gefs only or 
C naefs storm surge products
      OPEN(18,FILE=TRIM(PRODF),ACTION='READ')
      READ(18,*) PROD
      CLOSE(18)
C Read relevant master file parameters in here so you can
C reference by each station later
      OPEN(11,FILE=TRIM(MASTER),ACTION='READ')
      II = 0
      DO
        II = II + 1
        READ(11,*,END=20), NOPE,STID1,NWSLI,NOPE,NOPE,NOPE,NOPE,
     &                    NOPE,NOPE,NOPE,NOPE,NOPE,NOPE,NOPE,NOPE
        
        ! Correct IDs like '0000007' to '7'
        IF (STID1(1:5).EQ.'00000') THEN
          IF (STID1(6:6).EQ.'0') THEN
            STID2 = STID1(7:7)
          ELSE
            STID2 = STID1(6:7)
          END IF
        ELSE
          STID2 = STID1
        END IF

        ! Get NWSLI's for stations with no COOPS ID
        IF (STID1.EQ.'0000000') THEN
          STIDS(II) = NWSLI
        ELSE
          STIDS(II) = STID2
        END IF
      END DO
 20   CLOSE(11)

C  Get list of time stamps (DATES)
      OPEN(12,FILE=TRIM(DATELIST),ACTION='READ')

C  Initialize output grid
      DO JJ = 1,NDATES
        READ(12,'(A12)'), DATES(JJ)
        DO KK = 1,NLINES
          GRID(JJ,KK,1) = 9999
          GRID(JJ,KK,2) = 9999
          GRID(JJ,KK,3) = 9999
        ENDDO
      ENDDO
      CLOSE(12)

C Initialize dummy integer
      DUM = 9999

C ***************************************
C  Loop through stations to get surge
C ***************************************
!    Added by Huiqing.Liu/MDL March 2017 
!    for 90p and 10p stormtide files
!    ISURGE = 1 => Mean of P-ETSS
!    ISURGE = 2 => 90% of P-ETSS
!    ISURGE = 3 => 10% of P-ETSS

      DO ISURGE = 1, 3
!
      OUTER: DO LL = 1,NLINES
        SSIND = 1

C       Initialize surge array for this station
!        DO VV = 1,216 ! 216 hours
        DO VV = 1,222  ! 222 hours
          SS(VV) = 9999
        END DO
        
C       Filenames like: 20141030_00, or YYYYmmdd_cycle
        OPEN(14,FILE=TRIM(SSFNS),ACTION="READ")

C Three options for data filenames:
C  1. We had a hiccup in the data and the file doesn't exist, so we
C     need to populate the grid with 'missing'
C  2. We had a hiccup in the data, the file doesn't exist, and we're
C     at the end of the list of filenames, so we need to use the
C     last file as our 96-hour forecast, even though it's not
C     representative of the most recent forecast cycle
C  3. No hiccup in the data; file is where and when it's supposed to
C     be.

C       Loop through 5 days, 6-hr chunks
        DO MM = 1,21
          READ(14,'(A11)') LN
          IF (PROD.EQ.'naef')THEN
             FN = LN // '.ss'
!    Added by Huiqing.Liu/MDL March 2017 
!    for 90p and 10p stormtide files
             FN90p = LN // '_90p.ss'
             FN10p = LN // '_10p.ss'
          ELSE
             FN = LN // '_gefs.ss'
             FN90p = LN // '_90p_gefs.ss'
             FN10p = LN // '_10p_gefs.ss'
          ENDIF
!
          INQUIRE(FILE=TRIM(FN),EXIST=THERE)
! Added by Huiqing.Liu AceInfo/MDL in Sept. 2018 to make sure surge file is not empty
          INQUIRE(FILE=TRIM(FN),SIZE=ss_size)
          IF (THERE.and.ss_size.gt.0) THEN ! Option 3
            PHOLD = TRIM(FN) ! Keep record of last existing file
            FNUM = 21-MM  ! Keep count of where PHOLD was

C           Once you get the date-based filename (FN), open
C           that file to get the data
!    Added by Huiqing.Liu/MDL March 2017 
!    for 90p and 10p stormtide files
            IF (ISURGE == 1) THEN
               OPEN(18,FILE=TRIM(FN),ACTION="READ")
            ELSE IF (ISURGE == 2) THEN
               OPEN(18,FILE=TRIM(FN90p),ACTION="READ")
            ELSE
               OPEN(18,FILE=TRIM(FN10p),ACTION="READ")
            ENDIF

            IF (MM.EQ.21) THEN
C           Last file: Get the 102-hour forecast
C             Read in the first line
              DO
!    Added by Huiqing.Liu/MDL March 2017 
!    for 90p and 10p stormtide files
                READ(18,'(A)',END=30) FLINE
!
                IF (ADJUSTL(TRIM(FLINE(2:9))).EQ.STIDS(LL)) THEN
C                 We've found the station we want
                  READ(FLINE(93:96),*) DUM
                  IF ((DUM.LT.500).AND.(DUM.GT.-500)) THEN
                    SS(SSIND) = DUM*100
                  END IF
                  DO TT = 1,4
                  READ(18,'(A)') OLINE
                    DO OO = 0,23
                      IND = 1 + (OO * 4)
                      READ(OLINE(IND:IND+3),*) DUM
                      IF ((DUM.LT.500).AND.(DUM.GT.-500)) THEN
                        SS(SSIND+OO+1) = DUM*100
                      END IF
                    END DO
                    SSIND = SSIND + 24
                  END DO
!-------------------------------
! Extra-6 hours forecast H.Liu
!-------------------------------
                  READ(18,'(A)') OLINE
                  DO OO = 0,5
                     IND = 1 + (OO * 4)
                     READ(OLINE(IND:IND+3),*) DUM
                     IF ((DUM.LT.500).AND.(DUM.GT.-500)) THEN
                       SS(SSIND+OO+1) = DUM*100
                     END IF
                  END DO
                  SSIND = SSIND + 6  
!--------------------------------------------               
C                 We've found the droids we're looking for, so 
C                 we can close out the file now and continue on
                  CLOSE(18)
                  EXIT  
                END IF
              END DO
 30           CLOSE(18)
            ELSE
C           Not the last file: only get the first 6-hours
              stny = 0
              DO
                READ(18,'(A)',END=40) FLINE

                IF (ADJUSTL(TRIM(FLINE(2:9))).EQ.STIDS(LL)) THEN
                  stny = 1
                  READ(FLINE(93:96),'(I6)') DUM
                  IF ((DUM.LT.500).AND.(DUM.GT.-500)) THEN
                    SS(SSIND) = DUM*100
                  END IF

                  READ(18,'(A)') OLINE
                  DO OO = 0,4
                    IND = 1 + (OO * 4) 
                    READ(OLINE(IND:IND+3),*) DUM
                    IF ((DUM.LT.500).AND.(DUM.GT.-500)) THEN
                      SS(SSIND+OO+1) = DUM*100
                    END IF
                  END DO
                  SSIND = SSIND + 6
                  CLOSE(18)
                  EXIT 
                END IF
              END DO
 40           CLOSE(18)
!------------------------------------------------------------------
! Added by H.Liu /MDL June 2020
! For new stations, there is no surge ouput for the last 5days, but
! are included in the current forecast
!------------------------------------------------------------------
              IF (stny .EQ. 0) THEN
                 SS(SSIND) = 9999.
                 DO OO = 0,4
                    SS(SSIND+OO+1) = 9999.
                 ENDDO
                 SSIND = SSIND + 6
              ENDIF
!------------------------------------------------------------------

            END IF
          ELSE ! The file doesn't exist...
            IF (MM.LT.21) THEN ! Option 1
C             Need to skip missing data 
              DO WW = 1,6
                SSIND = SSIND + 1
              END DO
            ELSE IF ((MM.EQ.21).AND.(PHOLD.NE."NULL")) THEN ! Option 2
C             Need to go back and find the last existing file to
C             get the forecast
              OPEN(18,FILE=PHOLD,ACTION="READ")
              SSIND = SSIND - (FNUM*6)
              DO ! Get 102-hr forecast from last existing file
                READ(18,'(A)',END=50) FLINE
                IF (ADJUSTL(TRIM(FLINE(2:9))).EQ.STIDS(LL)) THEN
                  READ(FLINE(93:96),*) DUM
                  IF ((DUM.LT.500).AND.(DUM.GT.-500)) THEN
                    SS(SSIND) = DUM*100
                  END IF
                  DO TT = 1,4
                  READ(18,'(A)') OLINE
                    DO OO = 0,23
                      IND = 1 + (OO * 4)
                      READ(OLINE(IND:IND+3),*) DUM
                      IF ((DUM.LT.500).AND.(DUM.GT.-500)) THEN
                        SS(SSIND+OO+1) = DUM*100
                      END IF
                    END DO
                    SSIND = SSIND + 24
                  END DO
!-------------------------------
! Extra-6 hours forecast H.Liu
!-------------------------------
                  READ(18,'(A)') OLINE
                  DO OO = 0,5
                     IND = 1 + (OO * 4)
                     READ(OLINE(IND:IND+3),*) DUM
                     IF ((DUM.LT.500).AND.(DUM.GT.-500)) THEN
                       SS(SSIND+OO+1) = DUM*100
                     END IF
                  END DO
                  SSIND = SSIND + 6
!-----------------------------------------
                  CLOSE(18)
                  EXIT  
                END IF
              END DO
 50           CLOSE(18)
            END IF
          END IF ! End of 'if file exists' statement
        END DO ! End of MM = 1,21 loop

        CLOSE(14)

C       Reset SSIND and use it to place surge into the grid
        SSIND = 1
        DO PP = 1,NDATES
          IF (MOD(PP,10).EQ.1) THEN
C           Print surge every 10th time stamp (once per hour)
            GRID(PP,LL,ISURGE) = SS(SSIND)
            SSIND = SSIND + 1
          END IF
        END DO

      END DO OUTER ! End of LL = 1,NLINES
      
      END DO ! End of ISURGE Huiqing.Liu/MLD March 2017
!
C     Write format mased on number of stations
      WRITE (FMTSTR,'("(",I4,"(I6,1X))")') NLINES

      OPEN(52,FILE=TRIM(SSGRID),ACTION="WRITE",STATUS="UNKNOWN")
      DO QQ = 1,SIZE(DATES)
        WRITE(52,FMTSTR) (GRID(QQ,RR,1), RR=1,NLINES)
        WRITE(52,FMTSTR) (GRID(QQ,RR,2), RR=1,NLINES)
        WRITE(52,FMTSTR) (GRID(QQ,RR,3), RR=1,NLINES)
      ENDDO
      CLOSE(52)

      DEALLOCATE(DATES)
      DEALLOCATE(GRID)
      DEALLOCATE(STIDS)
      
      STOP
      END

