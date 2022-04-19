C----------------------------------------------------------------------
C calctide.f
C----------------------------------------------------------------------
C Author: Ryan Schuster (ryan.schuster@noaa.gov)
C History: Program created in 2014
!----------------------------------------------------------------------
! Updated by Huiqing Liu in April 2016 by removing reading consititue files
! to gettide.f to avoid reading data from files every time step
!----------------------------------------------------------------------
C Abstract:
C  Calculates tide using equation from "Manual of Harmonic Analysis
C  and Prediction of Tides" from US DOC Coast and Geodetic Survey
C
C  Tide(t) = Ho + SUM(Fi*Hi*cos(VPUi - Ki + (Ai*t)))
C
C  Where t = time, Ho = avg height of datum, F = nodal factor,
C  H = amplitude, VPU = equilibrium argument, K = phase, and
C  A = speed
C
C  i denotes one of the 37 constituents which are unique to each
C  station and summed to calculate the tide level at time t.
C
C  F and VPU are unique to each tidal constituent but are constant
C  at each station. So F for constituent K1 at station A is the same
C  value as F for constituent K1 at station B and so on
C
C  H, K, and A change for each constituent and each station
C
C  Ho is unique to each station and is the height of this datum
C
C  Returns tide at a single location/time point
C
C  Constituents are from COOPS, in GMT/Feet/MSL
C
C Parameters:
C     Input
C     STN:      Station filename to get constits from
C     TIME:     Time to get tide at 
C     FANDVPU:  Yearly constants (F and VPU)
C     
C     Output
C     TIDE:     Tide at a single station(STN)/time(TIME) point
C----------------------------------------------------------------------

      SUBROUTINE CALCTIDE(STN,TIME,FANDVPU,TIDE)
     
      use tide_constits  !Huiqing.Liu MDL/April. 2016
!
      IMPLICIT NONE

     
 
C     Input station filename
      CHARACTER(255) STN
      REAL TIME

C     Array containing f and vpu values for this year
      REAL, DIMENSION(75) :: FANDVPU

C     Constituent-related values
!      INTEGER, DIMENSION(37) :: CNUM
!      CHARACTER*4  NM(37)
!      REAL, DIMENSION(37) :: AMP,PHASE,SPEED,F,VPU
      REAL, DIMENSION(37) :: F,VPU
      REAL T1,T2,T3,PI

C     Indices
      INTEGER II,TEST
!      CHARACTER(120) HEAD

C     Output tide
      REAL TIDE
C ---------------------------------------------------------------------

C     Define Pi and T1 (ht relative to MSL)
      PI = 4.D0*DATAN(1.D0)
      T1 = 0

C     Open station file
!      OPEN(19,FILE=TRIM(STN),ACTION='READ')

!      READ(19,*,IOSTAT=TEST) HEAD
!      IF (TEST.LT.0) THEN
!        CLOSE(19)
!      ELSE
C     The file contains constits
        T2 = 0
C                 37 tide constituents
        DO II = 1,37
C       Read in tide constits
!          READ(19,*,IOSTAT=TEST) CNUM(II),NM(II),AMP(II),
!     &                          PHASE(II),SPEED(II) 

!          IF (TEST.LT.0) THEN
C         File may have less than 37 constits, so we need
C         to exit the loop early
!            EXIT
!          END IF

C         Get F and VPU for this year
          F(II) = FANDVPU(((II*2)-1)+1)
          VPU(II) = FANDVPU((II*2)+1)

C         Sum constituents to calculate tide
          T3 = ((SPEED(II)*TIME) + VPU(II) - PHASE(II))*(PI/180)
          T2 = T2 + (F(II)*AMP(II)*COS(T3))
        ENDDO

        TIDE = T1+T2

C       Round small numbers down and catch wierd miscalcs
        IF (TIDE.GT.50 .OR. TIDE.LT.-50) THEN
          TIDE = 9999
        ELSE IF ((TIDE.LT.0.001 .AND. TIDE.GT.0).OR.
     &           (TIDE.GT.-0.001 .AND. TIDE.LT.0)) THEN
          TIDE = 0
        END IF
!      ENDIF

      CLOSE(19)

      RETURN

      END
