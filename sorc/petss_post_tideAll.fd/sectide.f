C----------------------------------------------------------------------
C sectide.f
C----------------------------------------------------------------------
C Author: Ryan Schuster (ryan.schuster@noaa.gov)
C History: Program created in 2014
c Updated by Huiqing Liu in March 2016
c Updated by Huiqing Liu in August 2017 to remove hardwired reference stn
c-----------------------------------------------------------------------
C Abstract:
C  Called by gettide.f to load special constituents for secondary tide
C  stations (i.e. stations without a long term water level obs record
C  or full constituents). It then calculates the tide for a nearby
C  reference station and uses tidemaxmin.f to correct that tide using
C  the secondary constituent values. The corrected tide is returned
C  to gettide.f. Tides are given in MLLW
C 1) Reads in secondary constituents (more like correction constants)
C 2) Uses algorithm adapted from xTide software to calculate primary
C    tide and correct it using the secondary constituents. Interpolates
C    between max/min values, which it gets from tidemaxmin.f
C 3) Filters out wonky or missing values.
C 4) Sends calculated tide back to gettide.f for gridding
C 
C Parameters:
C     Inputs
C     REFFN:   File name of reference station constituents
C     STN:     Filename of secondary tide station constituents
C     HSBY:    Hours since beginning of the year to reach the point in
C              time where we want to make the calculation
C     FANDVPU: List of F and VPU constants for this year
C     
C     Output   
C     STIDE:    Array of secondary tide 
C----------------------------------------------------------------------

      SUBROUTINE SECTIDE(REFFN,STN,HSBY,FANDVPU,STIDE,NDATES,DELTADATUM)

C     Input/output parameters
      INTEGER :: NDATES
      REAL    :: DELTADATUM ! REFENCE STATION DATUM DELTA (MSL-MLLW)
      CHARACTER(255) STN,REFFN
      REAL, DIMENSION(75) :: FANDVPU
!      REAL, DIMENSION(2161) :: STIDE
      REAL, DIMENSION(NDATES) :: STIDE ! 5days + 102 Hours not 96 hours

C     Indices
      INTEGER II,JJ

C     Initial/reference times
      REAL LOCT,REFT,HSBY,TMAX,TMIN

C     Initial/reference heights
      REAL HMAX,HMIN,REFZ,LOCZ

C     Adjustment parameters
      REAL MXADJ,MNADJ,MXINC,MNINC
      REAL MNTIME,MXTIME

C     Throwaway header
      CHARACTER(120) HEAD

C ---------------------------------------------------------------------
C  MNTIME, MXTIME given in minutes!! Here, we adjust them to hours
C     Open secondary station file in the tide/constits folder
      OPEN(17,FILE=TRIM(STN),ACTION="READ")

C     Get adjustment parameters for that station
      READ (17,*) HEAD
      READ (17,*) MNTIME,MXTIME,MNADJ,MXADJ,MNINC,MXINC
      CLOSE(17)

C     Loop through all the dates
!      DO II = 1,2160
!      DO II = 1,2221 ! every 6 mins for 5days + 102 hours
      DO II = 1,NDATES 
C       Get ref time (HSBY - avg time adj)
        LOCT = (HSBY+1) + (II-1)*0.1
        IF (II.EQ.1) THEN
          REFT = LOCT - ((MXTIME + MNTIME)/2)/60 
        ELSE
          REFT = REFT + 0.1
        END IF

C       Calculate tide at RefT and nearest min/max
        CALL CALCTIDE(REFFN,REFT,FANDVPU,REFZ)

        IF (II.EQ.1) THEN
          CALL TIDEMAXMIN(REFFN,REFT,FANDVPU,HMAX,HMIN,TMAX,TMIN)
        ELSE
          IF ((REFT.GT.TMAX).AND.(REFT.GT.TMIN)) THEN
            CALL TIDEMAXMIN(REFFN,REFT,FANDVPU,HMAX,
     &                      HMIN,TMAX,TMIN)
          END IF
        END IF

C       zero in on RefZ
        DO JJ = 1,5
          REFT = LOCT - (MNTIME*(HMAX-REFZ)
     &             +  MXTIME*(REFZ-HMIN))/((HMAX-HMIN)*60)
          CALL CALCTIDE(REFFN,REFT,FANDVPU,REFZ)
        ENDDO

C       Datum Corrections for reference stations (MSL-MLLW) Huiqing.Liu /MDL
        LOCZ = REFZ + DELTADATUM
!        IF (REFFN((LEN(TRIM(REFFN))-10):LEN(TRIM(REFFN)))
!     &      .EQ.'9465261.csv') THEN
!          LOCZ = REFZ + 10.35
!        ELSE IF (REFFN((LEN(TRIM(REFFN))-10):LEN(TRIM(REFFN)))
!     &           .EQ.'9468132.csv')
!     &  THEN
!          LOCZ = REFZ + 1.950
!        ELSE IF (REFFN((LEN(TRIM(REFFN))-10):LEN(TRIM(REFFN)))
!     &           .EQ.'9457292.csv')
!     &  THEN
!          LOCZ = REFZ + 4.450
!          LOCZ = REFZ + 30.05-25.56
!c Added by Huiqing.Liu /MDL
!        ELSE IF (REFFN((LEN(TRIM(REFFN))-10):LEN(TRIM(REFFN)))
!     &           .EQ.'8594900.csv')
!     &  THEN
!          LOCZ = REFZ + 1.560
!          LOCZ = REFZ + 5.87-4.31
!       ELSE IF (REFFN((LEN(TRIM(REFFN))-10):LEN(TRIM(REFFN)))
!     &           .EQ.'8574680.csv')
!     &  THEN 
!          LOCZ = REFZ + 0.820
!          LOCZ = REFZ + 4.69-3.87
!       ELSE IF (REFFN((LEN(TRIM(REFFN))-10):LEN(TRIM(REFFN)))
!     &           .EQ.'9439040.csv')
!     &  THEN
!          LOCZ = REFZ + 4.390
!          LOCZ = REFZ + 6.73-2.34
!       ELSE IF (REFFN((LEN(TRIM(REFFN))-10):LEN(TRIM(REFFN)))
!     &           .EQ.'9444900.csv')
!     &  THEN
!          LOCZ = REFZ + 4.980
!          LOCZ = REFZ + 8.17-3.19
!       ELSE IF (REFFN((LEN(TRIM(REFFN))-10):LEN(TRIM(REFFN)))
!     &           .EQ.'9447130.csv')
!     &  THEN
!          LOCZ = REFZ + 6.630
!          LOCZ = REFZ + 14.4-7.77
!        END IF

C       Apply height ratio/adj to get secondary tide at LocT
        LOCZ = LOCZ * (MNADJ*(HMAX-REFZ)
     &              +  MXADJ*(REFZ-HMIN))/(HMAX-HMIN)
  
        LOCZ = LOCZ + (MXINC*(HMAX-REFZ)
     &              +  MNINC*(REFZ-HMIN))/(HMAX-HMIN)

        IF ((LOCZ.GT.0 .AND. LOCZ.LT.0.001).OR.
     &      (LOCZ.LT.0 .AND. LOCZ.GT.-0.001))THEN
          STIDE(II) = 0
        ELSE IF (LOCZ.GT.50 .OR. LOCZ.LT.-50) THEN
          STIDE(II) = 9999
        ELSE
          STIDE(II) = INT(LOCZ*1000)
        END IF
      ENDDO

      END
