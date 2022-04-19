c-----------------------------------------------------------------------
c makeSHEF.f
c-----------------------------------------------------------------------
c Author: Ryan Schuster (ryan.schuster@noaa.gov) 
c History: Program created in 2014
c Updated by Huiqing Liu in March 2016
c Updated to handle 3 types of SHEF (10%,mean and 90%) by Huiqing Liu in Jan. 2020
c-----------------------------------------------------------------------
c Abstract:
c Called by combineAll.f to create a single output SHEF-encoded message.
c Note: SHEF messages make up larger SHEF bulletins, or files.
c Formats total water level or surge data to fit the SHEF standard.
c 1) Determines what data-type this SHEF output file will be (surge or
c    tide) and selects the correct code (HMIFV or HMIFW); uses the
c    inputs to make this determination
c 2) Calculates the total water level or predicted surge based on what
c    parameters are available
c 3) Formats the SHEF info string (containing the creation date of the
c    message, the data type, interval, etc)
c 4) Writes message containing TWL or surge data to the appropriate 
c    SHEF bulletin 
c
c Parameters:
c  Inputs:  SSARR - array of hourly surge values for this station
c           TIARR - array of hourly tide values for this station
c           ANARR - array of hourly anomaly values for this station
c           NWSLI - Stations NWSLI (NWS Location Identifier)
c           OBSB - Boolean indicating if the station has obs (1)
c                  or not (0)
c           TIDB - Boolean indicating if station has tides or not
c           DATE - The last forecast date (different from the date
c                  values to format the creation date)
c           U - File unit of correct SHEF-bulletin to print to
c           ITYP - Type of SHEF (1- mean; 2- 90% and 3- 10%)
c  Outputs: Shef messages that feed into ${COMOUT}/mdlsurge.HH.TWx.shef
c           files
c-----------------------------------------------------------------------

      SUBROUTINE MAKESHEF(SSARR,TIARR,ANARR,NWSLI,OBSB,TIDB,DATE,U,ITYP)

C     Data arrays. Length is 97 because it's a 96 hour
C     forecast plus the initial value
      REAL SSARR(103), TIARR(103), ANARR(103)
      CHARACTER(8) TWL(103)
      REAL NUM
      CHARACTER(8) SNUM

C     Station identifier and filename info
      CHARACTER(5) NWSLI

C     Date time info
      CHARACTER(12) DATE
      CHARACTER(8) DAY
      CHARACTER(4) TIME
      CHARACTER(10) FILL
      INTEGER, DIMENSION(8) :: VALUES
      INTEGER YR,MO,DA,HR,MI
      CHARACTER(2) MOSTR,DASTR,HRSTR,MISTR
      CHARACTER(4) YRSTR
      CHARACTER(14) DC

C     Shef parameters
      CHARACTER(55) INFOSTR
      CHARACTER(2) E
      CHARACTER(7) DATTYP
      INTEGER LNUM

C     Booleans for obs and tide
      CHARACTER OBSB, TIDB

C     Indices and units
      INTEGER II,JJ,D,U,ITYP

C     Formats
      CHARACTER(103) FMTSTR, FMTSTR1
      CHARACTER(2) IFMT
!----------------------------------------
!    define the forecast hour H.Liu
      integer :: FcstH
   
      FcstH = 102
!----------------------------------------      
C -------------------------------------------------
C Total water level is calculated differently here because these
C SHEF-encoded files have different requirements than the website
C .csv files; That's why combineAll.f doesn't pass a TWL array
C through to here.
C     Calculate TWL prediction 
      IF (TIDB.EQ.'1') THEN
        IF (OBSB.EQ.'0') THEN
C       No obs, no anomaly, just tide and surge
          DO II = 1,FcstH+1
             NUM = SSARR(II) + TIARR(II)
             WRITE(SNUM,'(F8.3)') NUM
             WRITE(TWL(II),'(A8)') SNUM
             IF ((SSARR(II).EQ.9999).OR.(TIARR(II).EQ.9999)) THEN
               TWL(II) = 'M'
             END IF
          END DO
        ELSE IF (OBSB.EQ.'1') THEN
C       Tide, surge, and anomaly are available
          DO II = 1,FcstH+1
             IF ((SSARR(II).EQ.9999).OR.(TIARR(II).EQ.9999)) THEN
               TWL(II) = 'M'
             ELSE
               IF (ANARR(II).EQ.9999) THEN
                 NUM = SSARR(II) + TIARR(II)
               ELSE
                 NUM = SSARR(II) + TIARR(II) + ANARR(II)
               END IF
               WRITE(SNUM,'(F8.3)') NUM
               WRITE(TWL(II),'(A8)') SNUM
             END IF
          END DO
        END IF
      ELSE
C     No tide, just surge; Likely an AHPS gauge
        DO II = 1,FcstH+1
          NUM = SSARR(II)
          WRITE(SNUM,'(F8.3)') NUM
          WRITE(TWL(II),'(A8)') SNUM
          IF (SSARR(II).EQ.9999) THEN
            TWL(II) = 'M'
          END IF
        END DO
      END IF

      IF (ITYP.EQ.1) THEN
         DATTYP = 'HMIFZD5' ! Surge + Tide + Anom; bias-adjusted for ensemble mean
      ELSE IF (ITYP.EQ.2) THEN
         DATTYP = 'HMIFZD9' ! Surge + Tide + Anom; bias-adjusted for 90% exceedance
      ELSE IF (ITYP.EQ.3) THEN
         DATTYP = 'HMIFZD1' ! Surge + Tide + Anom; bias-adjusted for 10% exceedance
      END IF

C     Get creation date and time (the date/time right now)
      CALL DATE_AND_TIME(FILL,FILL,FILL,VALUES)

      YR = VALUES(1)
      MO = VALUES(2)
      DA = VALUES(3)
      HR = VALUES(5)
      MI = VALUES(6)

C     Put '0' in front of month, day, hr, or min if necessary
      IF (MO.LT.10) THEN
        WRITE(MOSTR,'(I1)') MO
        MOSTR = "0" // MOSTR
      ELSE
        WRITE(MOSTR,'(I2)') MO
      END IF
      IF (DA.LT.10) THEN
        WRITE(DASTR,'(I1)') DA
        DASTR = "0" // DASTR
      ELSE
        WRITE(DASTR,'(I2)') DA
      END IF
      IF (HR.LT.10) THEN
        WRITE(HRSTR,'(I1)') HR
        HRSTR = "0" // HRSTR
      ELSE
        WRITE(HRSTR,'(I2)') HR
      END IF
      IF (MI.LT.10) THEN
        WRITE(MISTR,'(I1)') MI
        MISTR = "0" // MISTR
      ELSE
        WRITE(MISTR,'(I2)') MI
      END IF

      WRITE(YRSTR,'(I4)') YR
     
      ! Creation date (not the same as forecast date)
      DC = "DC" // YRSTR // MOSTR // DASTR // HRSTR // MISTR

C     Will have ".E ZZZZZ YYYYmmdd Z DCYYYYmmddHHMM/DHHHMM/HMIFW/DIH01/"
C     which is 54 places including the last slash
      DAY = DATE(1:8)
      TIME = DATE(9:12)
      E = ".E"

C     First line
      INFOSTR = E // " " // NWSLI // " " // DAY // " Z " // DC //
     &          "/DH" // TIME // "/" // DATTYP // "/DIH01"

!      WRITE(U,'(A54)'), INFOSTR
      WRITE(U,'(A55)'), INFOSTR

C     Loop through lines to print data into file
!      DO JJ = 1,8
C       "normal" lines of 12 data points 8*12=96hrs
      DO JJ = 1,9
!
!       "normal" lines of 12 data points 8*12+6=102hrs
        IFMT = 'I2'
        LNUM = JJ
        IF (LNUM.LT.10) THEN
          IFMT = 'I1'
        ELSE
          IFMT = 'I2'
        END IF

        FMTSTR = '(A2,' // IFMT // ',A2,A8,A1,A8,A1,A8,A1,'//
     &           'A8,A1,A8,A1,A8,A1,A8,A1,A8,A1,A8,A1,'//
     &           'A8,A1,A8,A1,A8)'

        FMTSTR1 = '(A2,' // IFMT // ',A2,A8,A1,A8,A1,A8,A1,'//
     &           'A8,A1,A8,A1,A8)'

        D = 1 + ((JJ - 1) * 12)

C ***** This is where you write to the appropriate output file based on
C ***** the file unit combineAll.f passed in 
!-----------------------------
! H.Liu Extra 6 hours results
        IF (JJ.EQ.9) THEN

        WRITE(U,FMTSTR), E,LNUM,'  ',TWL(D),'/',TWL(D+1),'/',TWL(D+2),
     &          '/',TWL(D+3),'/',TWL(D+4),'/',TWL(D+5),'/'
          
!-----------------------------
        ELSE        
!  H.Liu Original 96 hours results
!
        WRITE(U,FMTSTR), E,LNUM,'  ',TWL(D),'/',TWL(D+1),'/',TWL(D+2),
     &          '/',TWL(D+3),'/',TWL(D+4),'/',TWL(D+5),'/',TWL(D+6),
     &          '/',TWL(D+7),'/',TWL(D+8),'/',TWL(D+9),'/',TWL(D+10),
     &          '/',TWL(D+11)
        ENDIF
      END DO

      END
