C----------------------------------------------------------------------
C gettide.f
C----------------------------------------------------------------------
C Author: Ryan Schuster (ryan.schuster@noaa.gov)
C History: Program created in 2014
c Updated by Huiqing Liu in March 2016 to extend 102 hrs
c Updated by Huiqing Liu in April 2016 to reduce I/O
c Updated by Huiqing Liu in August 2017 to removed the secondary tide 
c hardwired with referenced stations
c-----------------------------------------------------------------------
C Abstract:
C  Reads in universal yearly constants from parm/tide/reordft03.csv and
C  loops through stations to determine if they're primary or secondary.
C  Based on that determination, calls calctide.f or sectide.f to load
C  primary or secondary constituents and calculate tides for the 5-day
C  hindcast to the 96 hour forecast. Outputs to grid.
C 1) Sets up relevant filenames from envir variables; gets relevant
C    info from master file; gets list of dates from datelist;
C    initializes the grid
C 2) Reads in the number of hours since the beginning of the year 
C    (HSBY) so it knows where to start calculating tides in the wider
C    cycle. Also reads in yearly constants (FANDVPU)
C 3) Starts looping through stations. If a station is primary, calls
C    calctide.f; if a station is secondary, calls sectide.f
C 4) Gets result back from tide calculation code and writes it into the
C    output grid 
C Useful Info:
C  Tide constit amplitudes are in feet, phases are in degrees and timed
C  to match GMT
C  Tide speeds in original ft07.dta file are deg/hr, amplitudes are in feet
C  COOPS, the source of these tide constituents, gives amplitude
C  relative to mean sea level (MSL), so Ho is MSL.
C  Tides are corrected to MLLW
C
C Parameters:
C  Inputs: none
C  Outputs: tideGrid - Grid of tide values at specific locations and
C  times
C-----------------------------------------------------------------------------
!Huiqing.Liu MDL/April. 2016 speeding the running speed by doing the following
! Read the tide constits one time in gettide.f instead of reading it every time
! step in calctide.f
!----------------------------------------------------------------------------

      module tide_constits

!----------------------------------------------------------------------------
! This module contains tide consititute related variable arrays, which will 
! be used in gettide.f and calctide.f
!----------------------------------------------------------------------------
      INTEGER, DIMENSION(37) :: CNUM
      CHARACTER (len=4) ::  NM(37)
      REAL, DIMENSION(37) :: AMP,PHASE,SPEED

      endmodule
!----------------------------------------------------------------------------
!
!      SUBROUTINE GETTIDE()
      PROGRAM GETTIDE

      use tide_constits !Huiqing.Liu MDL/April. 2016

C     Date/time values
      REAL YR
      REAL HSBY

C     Input list of stations and dates / output grid
!      CHARACTER(12) DATES(2161)
!      CHARACTER(12) DATES(2221) ! (5days*24+102 hours) *60/6 --every 6 mins
      INTEGER :: NDATES
      CHARACTER(12) ,  ALLOCATABLE :: DATES(:) 
      INTEGER, ALLOCATABLE :: GRID(:,:)
C     Station Names
      CHARACTER(7), ALLOCATABLE :: STNS2(:)
      CHARACTER(7) ST
C     Primary/secondary station
      CHARACTER(1) PS
      CHARACTER(1), ALLOCATABLE :: PSS(:)
C     Reference for secondary stations
      CHARACTER(255) REFFN
      CHARACTER(7) REF
      CHARACTER(7), ALLOCATABLE :: REFS(:)
C     True/false if station has tides
      CHARACTER(1) BOOL
      CHARACTER(1), ALLOCATABLE :: BOOLS(:)
C     Datums to adjust by
      REAL, ALLOCATABLE :: MSLS(:), MLLWS(:)
      REAL MSL, MLLW
C     Time zone offsetes
      INTEGER TZ
      INTEGER, ALLOCATABLE :: TZS(:)
C     Stuff I don't want
      CHARACTER(120) NOPE
C     Did we find the FandVPU data?
      INTEGER FOUNDYR

C     Indices and IO test param
      INTEGER II,JJ,KK,LL,MM,NN,OO,PP

C     Number of stations
      INTEGER NLINES
      CHARACTER(13) FMTSTR

C     Array containing f and vpu values for this year
      REAL, DIMENSION(75) :: FANDVPU
 
C     Calculated tide
      REAL TIDE
!      REAL, DIMENSION(2161) :: STIDE
!      REAL, DIMENSION(2221) :: STIDE
      REAL, ALLOCATABLE :: STIDE(:)

      INTEGER :: NLINES_FT03
C     Input/Output file names
      CHARACTER(255) MASTER,DATELIST,HSBYF,FT03,TIDEGRID,STN
!
!
      CHARACTER(120) HEAD ! Header for tide const files
      INTEGER :: TEST


C  --------------------------------------------------------------------
C  Get file units from environmental variables
      CALL GET_ENVIRONMENT_VARIABLE('FORT11',MASTER)
      CALL GET_ENVIRONMENT_VARIABLE('FORT12',DATELIST)
      CALL GET_ENVIRONMENT_VARIABLE('FORT15',HSBYF)
      CALL GET_ENVIRONMENT_VARIABLE('FORT16',FT03)
      CALL GET_ENVIRONMENT_VARIABLE('FORT53',TIDEGRID)
      CALL GET_ENVIRONMENT_VARIABLE('FORT17',STN)

C  Get number of stations and allocate arrays
      CALL NUMLINES(TRIM(MASTER), NLINES)
      CALL NUMLINES(TRIM(DATELIST),NDATES)

!      ALLOCATE(GRID(2161,NLINES))
      ALLOCATE(DATES(NDATES))
      ALLOCATE(GRID(NDATES,NLINES))
      ALLOCATE(STNS2(NLINES))
      ALLOCATE(PSS(NLINES))
      ALLOCATE(REFS(NLINES))
      ALLOCATE(BOOLS(NLINES))
      ALLOCATE(MSLS(NLINES))
      ALLOCATE(MLLWS(NLINES))
      ALLOCATE(TZS(NLINES))
      ALLOCATE(STIDE(NDATES))

C  Get relevant date/time info
      OPEN(15,FILE=TRIM(HSBYF),ACTION='READ',STATUS='OLD')
      READ(15,'(F6.0)') , HSBY
      READ(15,'(F5.0)'), YR
      CLOSE(15)

C  Get dates and stations, also extra time stamps for time zone offset
      OPEN(12,FILE=TRIM(DATELIST),ACTION='READ',STATUS='OLD')

C  Initialize THE GRID
      DO JJ = 1,NDATES
         READ(12,'(A12)'), DATES(JJ)
         DO KK = 1,NLINES
            GRID(JJ,KK) = 9999
         ENDDO
      ENDDO
      CLOSE(12)

C  Get info from master file
      OPEN(11,FILE=TRIM(MASTER),ACTION='READ',STATUS='OLD')
      II = 0
      DO
        II = II + 1

        READ(11,*,END=20), NOPE,ST,NOPE,NOPE,NOPE,NOPE,NOPE,
     &                    MSL,MLLW,NOPE,BOOL,PS,REF,NOPE,TZ
        BOOLS(II) = BOOL
        STNS2(II) = ST
        PSS(II) = PS
        REFS(II) = REF

        IF (TZ.EQ.0) THEN
         TZS(II) = TZ
        ELSE
         TZS(II) = TZ + 1 
        END IF

        MSLS(II) = MSL
        MLLWS(II) = MLLW
      END DO
 20   CLOSE(11)

!------------------------------------------------------------
!  Added by Huiqing.Liu /MDL (Ace Info.) Aug. 2017
!  Update the secondary tide station datum by the reference 
!  station datum, which will be used by sectide.f
!------------------------------------------------------------
      DO MM = 1,NLINES
         IF (BOOLS(MM) .EQ. '1' .AND. PSS(MM) .EQ. 'S') THEN
            DO II =1,NLINES
               IF (REFS(MM) .EQ. STNS2(II)) THEN
                  MSLS(MM) = MSLS(II)
                  MLLWS(MM) = MLLWS(II)
                  EXIT
               ENDIF
            ENDDO
         ENDIF
      ENDDO
C  Open parsed and reordered ft03 file
      CALL NUMLINES(FT03, NLINES_FT03)
      OPEN(16,FILE=TRIM(FT03),ACTION='READ',STATUS='OLD')

C  Find the right f and VPU for this year
C               226 years on record
      FOUNDYR = 0
!      DO LL = 1,226
      DO LL = 1,NLINES_FT03
        READ(16,*) FANDVPU(:)
        IF (FANDVPU(1).EQ.YR) THEN
          FOUNDYR = 1
          EXIT
        ENDIF
      ENDDO
      IF (FOUNDYR.EQ.0) THEN
        PRINT *, "ERROR: No FandVPU data for this year. Check ft03.dta
     &            file"
        STOP
      END IF

C  Loop through the stations
      DO MM = 1,NLINES
        IF (BOOLS(MM).EQ.'1') THEN
          STN((LEN(TRIM(STN))-10):LEN(TRIM(STN))) = STNS2(MM) // '.csv'
C         Check if this is primary or secondary station; act accordingly
          IF (PSS(MM).EQ.'P') THEN
!------------------------------------------------------------------------
! Huiqing.Liu /MDL Aprl. 2016
!      Open station file containing tide consititutes
!
            OPEN(19,FILE=TRIM(STN),ACTION='READ',STATUS='OLD')

            READ(19,*,IOSTAT=TEST) HEAD
            IF (TEST.LT.0) THEN
               CLOSE(19)
               PRINT *, "ERROR: No tide consititutes file for station-
     &         ", STN
               
            ELSE
               DO II = 1,37
                  READ(19,*,IOSTAT=TEST) CNUM(II),NM(II),AMP(II),
     &                                   PHASE(II),SPEED(II)

                  IF (TEST.LT.0) EXIT
C         File may have less than 37 constits, so we need
C         to exit the loop early
               ENDDO
               CLOSE(19)
            ENDIF
!------------------------------------------------------------------------
            DO NN = 1,NDATES
              CALL CALCTIDE(STN,((HSBY-TZS(MM))+((NN-1)*0.1)),FANDVPU,
     &                      TIDE)
              GRID(NN,MM) = INT(((TIDE+MSLS(MM))-MLLWS(MM))*1000)
            ENDDO
          ELSE IF (PSS(MM).EQ.'S') THEN
            REFFN = STN(1:(LEN(TRIM(STN))-11)) // REFS(MM) // '.csv'
!------------------------------------------------------------------------
! Huiqing.Liu /MDL July. 2017
!      Open reference station file containing tide consititutes
            OPEN(19,FILE=TRIM(REFFN),ACTION='READ',STATUS='OLD')

            READ(19,*,IOSTAT=TEST) HEAD
            IF (TEST.LT.0) THEN
               CLOSE(19)
               PRINT *, "ERROR: No tide consititutes file for station-
     &         ", STN

            ELSE
               DO II = 1,37
                  READ(19,*,IOSTAT=TEST) CNUM(II),NM(II),AMP(II),
     &                                   PHASE(II),SPEED(II)

                  IF (TEST.LT.0) EXIT
C         File may have less than 37 constits, so we need
C         to exit the loop early
               ENDDO
               CLOSE(19)
            ENDIF
            CALL SECTIDE(REFFN,STN,(HSBY-TZS(MM)),FANDVPU,STIDE,NDATES,
     &                   (MSLS(MM)-MLLWS(MM)))
            GRID(:,MM) = STIDE(:)
          ENDIF
        END IF
      ENDDO
      CLOSE(16)

C     Write format based on number of stations 
      WRITE (FMTSTR,'("(",I3,"(I6,1X))")') NLINES

      OPEN(53,FILE=TRIM(TIDEGRID),ACTION='WRITE',STATUS='UNKNOWN')
      DO OO=1,NDATES
        WRITE(53,FMTSTR) (GRID(OO,PP), PP=1,NLINES)
      ENDDO
      CLOSE(53)

      DEALLOCATE(DATES)
      DEALLOCATE(GRID)
      DEALLOCATE(STNS2)
      DEALLOCATE(PSS)
      DEALLOCATE(REFS)
      DEALLOCATE(BOOLS)
      DEALLOCATE(MLLWS)
      DEALLOCATE(MSLS)
      DEALLOCATE(TZS)
      DEALLOCATE(STIDE)

      END
