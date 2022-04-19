C----------------------------------------------------------------------
C tidemaxmin.f
C----------------------------------------------------------------------
C Author: Ryan Schuster (ryan.schuster@noaa.gov)
C History: Program created in 2014
C Abstract: 
C  Finds the nearest max/min (HMAX/HMIN) tide value to a given point on
C  the curve. Also finds the time of occurance of that point. These 
C  values are used to interpolate secondary tide values between 
C  themselves in sectide.f
C 
C Parameters:
C  Inputs
C  REF:      Reference station filename
C  REFT:     Calculated reference time
C  FANDVPU:  F and VPU yearly constants
C
C  Outputs
C  HMAX:     Max height nearest to REFZ
C  HMIN:     Min height nearest to REFZ
C  TMAX:     Time of max height
C  TMIN:     Time of min height
C----------------------------------------------------------------------

      SUBROUTINE TIDEMAXMIN(REFFN,REFT,FANDVPU,HMAX,HMIN,TMAX,TMIN)

C     Output/input parameters
      REAL HMIN,HMAX,REFT
      CHARACTER(255) REFFN
      REAL,DIMENSION(75) :: FANDVPU

C     Test heights and times
      REAL Z,Z1,Z2,T1,TMAX,TMIN

C     Time shift (constant)
      REAL MAX_RES
C ---------------------------------------------------------------------

      MAX_RES = 0.1

C     Get two heights right next to each other
      CALL CALCTIDE(REFFN,REFT,FANDVPU,Z)
      T1 = REFT - MAX_RES
      CALL CALCTIDE(REFFN,T1,FANDVPU,Z1)

C     Make sure they're not equal
      N = 1

      DO WHILE (Z1.EQ.Z)
        N = N + 1
C        T1 = T1 - MAX_RES
        T1 = REFT - N * MAX_RES

        CALL CALCTIDE(REFFN,T1,FANDVPU,Z1)
      ENDDO

      IF (Z1.LT.Z) THEN
C     The previous height is below the current height...
        Z2 = Z
        DO WHILE (Z1.LT.Z2)
C       Step through all the heights until you hit the min
          Z2 = Z1
          T1 = T1 - MAX_RES
          CALL CALCTIDE(REFFN,T1,FANDVPU,Z1)
        ENDDO
        HMIN = Z2
        TMIN = T1 + MAX_RES
        Z1 = Z
        T1 = REFT
        DO WHILE (Z1.GT.Z2)
          Z2 = Z1
          T1 = T1 + MAX_RES 
          CALL CALCTIDE(REFFN,T1,FANDVPU,Z1)
        ENDDO
        HMAX = Z2
        TMAX = T1 - MAX_RES
      ELSE
C     The previous height is above the current height...
        Z2 = Z
        DO WHILE (Z1.GT.Z2)
C       Step through all the heights until you hit the max
          Z2 = Z1
          T1 = T1 - MAX_RES
          CALL CALCTIDE(REFFN,T1,FANDVPU,Z1)
        ENDDO
        HMAX = Z2
        TMAX = T1 + MAX_RES
        Z1 = Z
        T1 = REFT
        DO WHILE (Z1.LT.Z2)
          Z2 = Z1
          T1 = T1 + MAX_RES
          CALL CALCTIDE(REFFN,T1,FANDVPU,Z1)
        ENDDO
        HMIN = Z2
        TMIN = T1 - MAX_RES
      ENDIF

      END
