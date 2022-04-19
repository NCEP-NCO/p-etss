       SUBROUTINE SETCP(IGEO)
C        J. CHEN    MAY 1992 MDL   IBM 360/195
C
C        PURPOSE
C           THIS SUBROUTINE  READS IN SURFACE WIND COMPONENTS AND
C           PRESSURE FIELD, FOR THE FIRST TWO TIME PROJECTIONS.
C           IT CALLS IN THE INTERPOLATION PROGRAM TO SET UP FORCINGS
C           ON SLOSH GRIDS.
C
C        DATA SET USE
C
C        VARIABLES
C             ITIME = NUMBER OF TIME STEPS; INITIALIZED TO ZERO
C            NDLTHR = NUMBER OF TIME STEPS IN ONE HOUR
C             HHRAD = ACCUMULATIVE TIME STEPS ON THE HOUR
C             IBGNT = TABULATED SUBSCRIPT FOR STORM INITIALIZATION
C                     C21 INITIALLY SET IN 'INTVAL'
C              DELT = UNIT TIME STEP
C               C22 = DRCTN OF STORM MOTION, 1ST HR, SET IN 'INTVAL'
C            ITMADV = COUNTER FOR HOURLY TIME, 1ST HOUR
C
C        GENERAL COMMENTS
C
!        HISTORY:
!           AWK      SEP 2010 MDL MODIFYING 09/2010 FOR NEW LARGER GULF OF MEX BASIN
!           H. LIU   MAY 2014 MDL MODIFYING TO READ 0.5 DEGREE GFS WIND
!                    WCOSS MACHINE
!           H. LIU   MAR 2019 MDL MODIFYING TO READ GEFS/CMCE/ECMWF wind data
!           H. LIU   ARP 2020 MDL USING DYNAMIC ALLOCATED ARRAY FOR WIND DATA
!                    XIU,XIV,XIP,XIUG,XIVG,IU,IV,IP,IUG,IVG

      use parm_gfs1
      use parm_gfs2

      include 'parm.for'
!     Increase to .5 by .5 Degree Huiqing.Liu /MDL April 2014
!      include 'parm_gfs.for'
!     Created for reading GEFS/CMC/ECMWF Huiqing.Liu /MDL March 2019
!     Modifed to read new GEFS with 0.25 Deg. Huiqing.Liu /MDL March 2020
!
!     GEFS 6-hr analysis and 3-hr forecast
!     CMCE 12-hr analysis and 3-hr forecast
!     new GEFS 6-hr analysis and 3-hr forecast with 0.25 Deg.
!     ECMWF 12-hr analysis and 6-hr forecast
!     new GEFS Retrospective data (00/12z) with 12-hr analysis and 3-hr forecast with 0.25 Deg.
!
      COMMON /WNDENS/ MGEFS_CMCE
! Huiqing Liu/MDL March. 2020
! COMM BLOCK for GEFS (MGEFS_CMCE = 1) or CMCE (2) or new GEFS (3) or ECMWF (4) or other
!
      COMMON /DATINC/ XLAT1,XLON1,DELLT,DELLN,IMXC,JMXC
C
      COMMON /FFTH/   ITIME,MHALT
      COMMON /DUMB3/  IMXB,JMXB,IMXB1,JMXB1,IMXB2,JMXB2
      COMMON /DELTH/  NDLTHR,NDLTH2,DLTHR,DLTH2
      COMMON /STIME2/  ISTM,JHR,ITMADV,NHRAD,IBGNT,ITEND
      COMMON /WTIME/  NHRW,INCW,CLAT,CLON
!      COMMON /STRMPS/ X(100),Y(100),PT(100),R(100),DIR(100),SP(100)
      COMMON /STRMPS/ X(999),Y(999),PT(999),R(999),DIR(999),SP(999)

      COMMON /SPLN/   ALT(15),ALN(15),AX(15),AY(15),RL(15),ANGD(15),
     1                PZ(15),RT(15),XLAT(100),YLONG(100),RLNGTH
      COMMON /FRST/   X1(50),X12(50)
      COMMON /EGTH/   DELS,DELT,G,COR
      COMMON /BSN/    PHI,ALTO,ALNO,PHI1,ALT1,ALN1,ALT1C
      COMMON /THRD/   C7,C17,C19,C25
      COMMON /INTV/   INTV(24),KINPUT
      COMMON /DUMM/   DUM1,DUM2,DUM3
      COMMON /MTH/    MONTH(12),AMON(12)
      CHARACTER*40    FILNAM
      CHARACTER*80    DUM,DUM1,DUM2,DUM3
      CHARACTER*4     AMON,AMON1
! Huiqing.Liu /MDL ETSS2.0 June 2014
      COMMON /DATEE/  IDATE1(4)
      COMMON /IDENT/  AIDENT(40),DACLOK(7)
      CHARACTER*4 AIDENT
! Huiqing.Liu /MDL March 2020 wind resolution 
      real :: res_wnd
!
C --------------------------------------------------------------------
      DATA RHOA/2.298E-3/,RHOW/4905./,RHOWG/29.89/
      REAL MTERFT
      DATA MTERFT/3.28084/
C-------------  TIME INTERVALS OF WIND INPUT ------
! GEFS/NewGEFS is 6 hourly, but cmc/ecmwf/newGEFS retrospective is 12 hourly for analysis wind
      if (MGEFS_CMCE.eq.1.or.MGEFS_CMCE.eq.3) then
         INCW=6
      else
         INCW=12
      endif

! NewGEFS is 0.25 deg.
      if (MGEFS_CMCE.eq.3.or.MGEFS_CMCE.eq.5) then
         res_wnd = 0.25
      else
         res_wnd = 0.5
      endif

      NHRW=0
      NHRAD=NDLTHR
      ITMADV=IBGNT+1
C ------------------------------------------------------
      CLAT=0.

      CLON=0.
C
C
C     INITIAL INPUT OF 2D WIND OF TWO TIME INTERVALS
C     There are total 60 hours analyzed wind,currently only use 48 hours
C     Therefore, codes need skip 12 hours wind
C     GEFS is 6 hourly and cmc/ecmwf is 12 Hourly 
C     JHR=61, IF IBGNT=JHR-48=13, KINPUT=3 for GEFS and KINPUTS=2 for cmc/ecmwf
C     Huiqing.Liu/AceInfo MDL March/2019

      if (MGEFS_CMCE.eq.1.or.MGEFS_CMCE.eq.3) then
         KINPUT=(IBGNT-1)/6+1
      else
         KINPUT=(IBGNT-1)/12+1
      endif
      KCNT=1
      DO
        IF (KCNT.GT.KINPUT) EXIT
        READ (33) IMXC,JMXC
        READ (33) DUM1,DUM2
        WRITE (*,'(A)') DUM1
! H.Liu Allocated dimension of wind arrays 04/2020        
        if (kcnt == 1) then
           allocate (IU(IMXC,JMXC))
           allocate (IV(IMXC,JMXC))
           allocate (IP(IMXC,JMXC))
           allocate (IUG(IMXC,JMXC))
           allocate (IVG(IMXC,JMXC))

           allocate (XIU(IMXC,JMXC,2))
           allocate (XIV(IMXC,JMXC,2))
           allocate (XIP(IMXC,JMXC,2))
           allocate (XIUG(IMXC,JMXC,2))
           allocate (XIVG(IMXC,JMXC,2))
        endif
!
        READ (33) ((IU(I,J),I=1,IMXC),J=1,JMXC)
        READ (33) ((IV(I,J),I=1,IMXC),J=1,JMXC)
        READ (33) ((IP(I,J),I=1,IMXC),J=1,JMXC)
        KCNT=KCNT+1
      ENDDO
!
! Huiqing.Liu /MDL June 2014 ETSS2.0 retrieve the date info
!
      DUM3=DUM1
      read(DUM3,8000)IDATE1(1),IDATE1(3),amon1,IDATE1(4)
8000  format(17x,I4,1x,I2,1x,A3,2x,I2)
      do n=1,12
         if (amon1.EQ.amon(n))then
            IDATE1(2)=n
            exit
         endif
      enddo
      call tflush
!
      CALL GEOWND(res_wnd)
      IF (IGEO.EQ.1) CALL GEO2BL(res_wnd)
C
C
      DO 200 J=1,JMXC
      DO 200 I=1,IMXC
         XIU(I,J,1)=IU(I,J)*.1*MTERFT
         XIV(I,J,1)=IV(I,J)*.1*MTERFT
         XIP(I,J,1)=-(IP(I,J)*.1-12.)/RHOWG
C        DENSITY RATIO BETWEEN WATER AND AIR IS ASUMMED TO BE 1000.
         XIUG(I,J,1)=IUG(I,J)*.1*MTERFT
         XIVG(I,J,1)=IVG(I,J)*.1*MTERFT
 200  CONTINUE
C
C
      KINPUT=KINPUT+1
C     WRITE (*,*)'   KINPUT = ',KINPUT
      READ (33) IDUM,JMXC
      READ (33) DUM1,DUM2
C     WRITE (*,'(A)') DUM1
      READ (33) ((IU(I,J),I=1,IMXC),J=1,JMXC)
      READ (33) ((IV(I,J),I=1,IMXC),J=1,JMXC)
      READ (33) ((IP(I,J),I=1,IMXC),J=1,JMXC)
      CALL GEOWND(res_wnd)
C
C       IF IGEO=1, FRICTIONALLY BALANCEED GEOSTRAPHIC WIND METHOD
C       IS USED.
         IF (IGEO.EQ.1) CALL GEO2BL(res_wnd)
C
      DO 210 J=1,JMXC
      DO 210 I=1,IMXC
         XIU(I,J,2)=IU(I,J)*.1*MTERFT
         XIV(I,J,2)=IV(I,J)*.1*MTERFT
         XIP(I,J,2)=-(IP(I,J)*.1-12.)/RHOWG
         XIUG(I,J,2)=IUG(I,J)*.1*MTERFT
         XIVG(I,J,2)=IVG(I,J)*.1*MTERFT
 210  CONTINUE
C
C       LINEAR WEIGHTING IN TIME FOR TWO-PROJECTIONS, INCREMENTED BY
C       BY INCW HOURS. (12 OR 6 HRS)
C
      ZZ=NDLTHR*INCW
      WGT=(ITIME+1-NHRW)/ZZ
C
      NBNDRY=0
      CALL WDPINT(WGT,CLAT,CLON,NBNDRY,IGEO,res_wnd)
C
      RETURN
      END

C
      SUBROUTINE STMVL(IGEO)
C        J. CHEN                  MAY 1992 MDL   IBM 360/195
C
C        PURPOSE
C           THIS SUBROUTINE (STMVAL) TESTS FOR INCREMENTAL HOURS AND
C           SETS HOURLY TRACK AND METEOROLOGY VALUES. IT TESTS FOR 1-MB
C           CHANGE IN PRESSURE DROP, OR 1 MILE CHANGE IN STORM SIZE,
C           FOR EACH ADVANCE IN TIME. PREPARATORY VALUES ARE SET FOR
C           LATER ADVANCING IN TIME.
C
C        DATA SET USE
C           NONE
C
C        VARIABLES
C             ITIME = CUMULATIVE TIME COUNTER
C            NDLTHR = NUMBER OF TIME STEPS IN ONE HOUR
C            ITMADV = COUNTER FOR HOURLY TIME, SET IN 'SETCMP'
C
C        GENERAL COMMENTS
C
C   AWK MODIFYING 09/2010 FOR NEW LARGER GULF OF MEX BASIN
!           H. LIU   ARP 2020 MDL USING DYNAMIC ALLOCATED ARRAY FOR WIND
!           DATA
!                    XIU,XIV,XIP,XIUG,XIVG,IU,IV,IP,IUG,IVG

      use parm_gfs1
      use parm_gfs2

      include 'parm.for'
!      include 'parm_gfs.for'
!------------------------------------------------------------------------
!     Increase to .5 by .5 Degreee Huiqing.Liu /MDL April 2014
!     Created for reading GEFS/CMC/ECMWF Huiqing.Liu /MDL March 2019
!     Modifed to read new GEFS with 0.25 Deg. Huiqing.Liu /MDL March
!     2020
!
!     GEFS 6-hr analysis and 3-hr forecast
!     CMCE 12-hr analysis and 3-hr forecast
!     new GEFS 6-hr analysis and 3-hr forecast with 0.25 Deg.
!     ECMWF 12-hr analysis and 6-hr forecast
!     new GEFS Retrospective data (00/12z) with 12-hr analysis and 3-hr
!     forecast with 0.25 Deg.
      COMMON /WNDENS/ MGEFS_CMCE
! Huiqing Liu/MDL March. 2020
! COMM BLOCK for GEFS (MGEFS_CMCE = 1) or CMCE (2) or new GEFS (3) or ECMWF (4) or new GEFS Restro (5)
!------------------------------------------------------------------------

      COMMON /DATINC/ XLAT1,XLON1,DELLT,DELLN,IMXC,JMXC
C
      COMMON /KEYS/   KEY1
      COMMON /FFTH/   ITIME,MHALT
      COMMON /BSN/    PHI,ALTO,ALNO,PHI1,ALT1,ALN1,ALT1C
      COMMON /DELTH/  NDLTHR,NDLTH2,DLTHR,DLTH2
      COMMON /EGTH/   DELS,DELT,G,COR
      COMMON /THRD/   C7,C17,C19,C25
!      COMMON /STRMPS/ X(100),Y(100),PT(100),R(100),DIR(100),SP(100)
      COMMON /STRMPS/ X(999),Y(999),PT(999),R(999),DIR(999),SP(999)

      COMMON /SPLN/   ALT(15),ALN(15),AX(15),AY(15),RL(15),ANGD(15),
     1                PZ(15),RT(15),XLAT(100),YLONG(100),RLNGTH
      COMMON /STIME2/  ISTM,JHR,ITMADV,NHRAD,IBGNT,ITEND
      COMMON /WTIME/  NHRW,INCW,CLAT,CLON
      COMMON /PRHSY/  IPRHR,JUMPR,IPRT
      COMMON /MAPTME/ ISTMAP,IADMAP
      COMMON /PRTSV/  IPRTSV,IPRTAD
      COMMON /SWTCH/  IOPERL(5)
      COMMON /DATE7/  IDATE(4)
      COMMON /INTV/   INTV(24),KINPUT
      COMMON /MTH/    MONTH(12),AMON(12)
      COMMON /DUMM/   DUM1,DUM2,DUM3
      COMMON /IDENT/  AIDENT(40),DACLOK(7)
      CHARACTER*4 AIDENT
      CHARACTER*40    FILNAM
      CHARACTER*80    DUM,DUM1,DUM2,DUM3
      CHARACTER*4     AMON,AMON1
!     
      integer interp
      real :: res_wnd
C
C --------------------------------------------------------------------
      DATA RHOA/2.298E-3/,RHOW/4905./,RHOWG/29.89/
      REAL MTERFT
      DATA MTERFT/3.28084/
C
! NewGEFS is 0.25 deg.
      if (MGEFS_CMCE.eq.3.or.MGEFS_CMCE.eq.5) then
         res_wnd = 0.25
      else
         res_wnd = 0.5
      endif
!
      ISKIP=0
      IF (ITIME.EQ.NHRAD) THEN
         NHRAD=NHRAD+NDLTHR
         ITMADV=ITMADV+1
      ENDIF
C        TEST FOR INCREMENTAL HOURLY CHANGES IN STORM TRACK
      IF (ITIME.GT.MHALT-2) RETURN
!------------------------------------------------------------
!     rewrite the following codes to remove the GOTO Statements
!     Huiqing.Liu /MDL March. 2015
!------------------------------------------------------------

!      IF(ITIME+1.NE.NHRW+NDLTHR*INCW) GO TO 100
      IF(ITIME+1.EQ.NHRW+NDLTHR*INCW) THEN

C       WRITE (*,*)'  ITIME MHALT NHRW+',ITIME,MHALT,NHRW+NDLTHR*INCW
      NHRW=NHRW+NDLTHR*INCW
      NBNDRY=0
C
      DO 500 J=1,JMXC
      DO 500 I=1,IMXC
         XIU(I,J,1)=IU(I,J)*.1*MTERFT
         XIV(I,J,1)=IV(I,J)*.1*MTERFT
         XIP(I,J,1)=-(IP(I,J)*.1-12.)/RHOWG
         XIUG(I,J,1)=IUG(I,J)*.1*MTERFT
         XIVG(I,J,1)=IVG(I,J)*.1*MTERFT
 500  CONTINUE
C
C------------  LAT/LONG HOURLY STORM POSITIONS OF CENTER  -----
C              COMPSITE-FIELD INTERPOLATIONS ARE USED.
C      CLAT= XLAT(ITMADV+INCW-1)-XLAT(ITMADV-1)
C      CLON=YLONG(ITMADV+INCW-1)-YLONG(ITMADV-1)
C           LOCAL INTERPOLATIONS IN TIME IS USED.
      CLAT=0.
      CLON=0.
C
C
      KINPUT=KINPUT+1
C
C        KINPUT=11, FINISHED READING ANALYZED DATA(60-hr) for GEFS 6 hourly
C        KINPUT=6, FINISHED READING ANALYZED DATA(60-hr) for CMC and ECMWF 12 hourly
      if (MGEFS_CMCE.eq.1.or.MGEFS_CMCE.eq.3) then
         interp = 11
      else
         interp = 6
      endif
!
      IF (KINPUT.GE.interp) THEN
C -------------------------------------------------------------------
         READ (34) IDUM,JDUM
         READ (34) DUM1,DUM2
         READ (34) ((IU(I,J),I=1,IMXC),J=1,JMXC)
         READ (34) ((IV(I,J),I=1,IMXC),J=1,JMXC)
         READ (34) ((IP(I,J),I=1,IMXC),J=1,JMXC)

         IF (KINPUT.EQ.interp) THEN
            DUM3=DUM1
            DO 344 K=1,20
               AIDENT(K)(1:4)=DUM3((K-1)*4+1:(K-1)*4+4)
 344        CONTINUE
C     WRITE (*,'(A)') DUM3
            DO 1001 L=1,4
               IDATE(L)=0
 1001       CONTINUE
            LB=18
            DO 1002 L=1,4
               LL=LB+L-1
               III=AMAX0(1,10**(4-L))
               IF (DUM3(LL:LL).EQ.' ') DUM3(LL:LL)='0'
C------------------------------------------------------------------
C        MAINFRAME   '0' = 240;   PC     '0' = 48.
C------------------------------------------------------------------
C         DECODE YEAR FROM ALPHAMERIC TO NUMERIC
               IDATE(1)=IDATE(1)+III*(ICHAR(DUM3(LL:LL))-240)
 1002       CONTINUE
C         DECODE DAY FROM ALPHAMERIC TO NUMERIC
            DO 1003 L=1,2
               LL=LB+5+L-1
               III=AMAX0(1,10*(2-L))
               IF (DUM3(LL:LL).EQ.' ') DUM3(LL:LL)='0'
               IDATE(3)=IDATE(3)+III*(ICHAR(DUM3(LL:LL))-240)
 1003          CONTINUE
C         DECODE HOUR FROM ALPHAMERIC TO NUMERIC
            DO 1004 L=1,2
               LL=LB+13+L-1
               III=AMAX0(1,10*(2-L))
               IF (DUM3(LL:LL).EQ.' ') DUM3(LL:LL)='0'
               IDATE(4)=IDATE(4)+III*(ICHAR(DUM3(LL:LL))-240)
 1004       CONTINUE
C       DECODE MONTH TO INTEGER
            AMON1=DUM3(LB+8:LB+10)
            DO 206 N=1,12
               IF (AMON1.EQ.AMON(N)) exit
 206        CONTINUE
 207        IDATE(2)=N
         ENDIF
C
C      FORECAST FIELDS, KINPUT > 11, ARE 3 HOURS APART IN GEFS.
C      FORECAST FIELDS, KINPUT > 6, ARE 3 HOURS APART IN CMCE.
C      FORECAST FIELDS, KINPUT > 6, ARE 6 HOURS APART in ECMWF.
        IF (KINPUT.GE.interp+1) THEN
           IF (MGEFS_CMCE.eq.4) THEN
              INCW=12.01/2.
           ELSE
              INCW=12.01/4.
           ENDIF           
        ENDIF
C-----------------------------------------------------------------------
      ELSE
C ----------------------------------------------------------------------
        IF (MGEFS_CMCE.eq.1.or.MGEFS_CMCE.eq.3) THEN
           INCW=6.01
        ELSE
           INCW=12.01
        ENDIF

         READ (33) IDUM,JMXC
         DUM=DUM1
         READ (33) DUM1,DUM2
         READ (33) ((IU(I,J),I=1,IMXC),J=1,JMXC)
         READ (33) ((IV(I,J),I=1,IMXC),J=1,JMXC)
         READ (33) ((IP(I,J),I=1,IMXC),J=1,JMXC)
      ENDIF
C-----------------------------------------------------------------------
C
C
      IF (DUM.EQ.DUM1) ISKIP=1
C
      CALL GEOWND(res_wnd)
      IF (IGEO.EQ.1) CALL GEO2BL(res_wnd)
C
C
      DO 200 J=1,JMXC
      DO 200 I=1,IMXC
         XIU(I,J,2)=IU(I,J)*.1*MTERFT
         XIV(I,J,2)=IV(I,J)*.1*MTERFT
         XIP(I,J,2)=-(IP(I,J)*.1-12.)/RHOWG
         XIUG(I,J,2)=IUG(I,J)*.1*MTERFT
         XIVG(I,J,2)=IVG(I,J)*.1*MTERFT
 200  CONTINUE
C
!
!
      END IF
!
!

 100  CONTINUE
C
C
      IF (ISKIP.EQ.0) THEN
C       UPDATE THE WIND FILED ONCE AN HOUR FOR SAVING COMPUTATIONS.
         IF (MOD(ITIME+1,NDLTHR).EQ.0) THEN
             ZZ=NDLTHR*INCW
             WGT=(ITIME+1-NHRW)/ZZ
             CALL WDPINT(WGT,CLAT,CLON,NBNDRY,IGEO,res_wnd)
         ENDIF
      ENDIF
C
      RETURN
      END


      SUBROUTINE WDPINT(WGT,CLAT,CLON,NBNDRY,IGEO,res_wnd)
C        J. CHEN               MAY 1992 MDL   IBM 360/195
C
C        PURPOSE
C           THIS SUBROUTINE INTERPOLATES DATA FIELDS FROM LAT/LON GRIDS
C           TO SLOSH GRIDS IN SPACE. FOR EACH TIME STEP BETWEEN HOURS
C           TWO DATA FIELDS ARE WEIGHTED THROUGH TRANSLATIONS AND
C           COMPSITING, BASING ON THE MOVEMENT VECTOR.
C
C        DATA SET USE
C           NONE
C
C        VARIABLES
C        YLT,YLG(,) = LAT/LON OF THE SLOSH GRID OF A BASIN
C         CLAT,CLON = TRANSLATION VECTOR FROM FIRST TO SECOND DATA FLD
C               WGT = THE WEIGHTING FACTOR FOR DATA FIELD AT TWO TIME
C                     LEVEL
C
C   AWK MODIFYING 09/2010 FOR NEW LARGER GULF OF MEX BASIN
!           H. LIU   ARP 2020 MDL USING DYNAMIC ALLOCATED ARRAY FOR WIND
!           DATA
!                    XIU,XIV,XIP,XIUG,XIVG,IU,IV,IP,IUG,IVG

      use parm_gfs1

      include 'parm.for'
C
!     Increase to .5 by .5 Degreee Huiqing.Liu /MDL April 2014
      PARAMETER (NBCPTS=12000)
      COMMON /DATINC/ XLAT1,XLON1,DELLT,DELLN,IMXC,JMXC
      COMMON /FORCE/ WX(M_,N_),WY(M_,N_),PP(M_,N_),PPX(M_,N_),PPY(M_,N_)
C
      COMMON /DUMB8/ IP(4),JP(4),IH(4),JH(4)
      COMMON /BSN/   PHI,ALTO,ALNO,PHI1,ALT1,ALN1,ALT1C
      COMMON /DUMB3/ IMXB,JMXB,IMXB1,JMXB1,IMXB2,JMXB2
      COMMON /DELTH/ NDLTHR,NDLTH2,DLTHR,DLTH2
      COMMON /BCPTS/  TIDESH(NBCPTS),BOUN_NEST(NBCPTS),
     *                NBCPT,ISH(NBCPTS),JSH(NBCPTS)
!
      real :: res_wnd
!
C
C
      IF (NBNDRY.EQ.1) THEN
      DO 355 N=1,NBCPT
      DO 355 K=1,4
      I=ISH(N)+IP(K)
      J=JSH(N)+JP(K)
      XLT=YLT(I,J)
      XLN=YLG(I,J)
      CALL GRDINT(XLT,XLN,CLAT,CLON,WGT,OUT,XIP,res_wnd)
      PP(I,J)=OUT
 355  CONTINUE
      ELSE
C
      DO 350 I=1,IMXB
      DO 350 J=1,JMXB
      XLT=YLT(I,J)
      XLN=YLG(I,J)
      CALL GRDINT(XLT,XLN,CLAT,CLON,WGT,OUT,XIP,res_wnd)
      PP(I,J)=OUT
 350  CONTINUE
      ENDIF
C
      DO 150 J=1,JMXB
      DO 150 I=1,IMXB
      IF (ITREE(I,J).EQ.'6') cycle
      XLT=YLT(I,J)
      XLN=YLG(I,J)
      CALL GRDINT(XLT,XLN,CLAT,CLON,WGT,OUT,XIU,res_wnd)
      IF (IGEO.EQ.1.AND.(ITREE(I,J).EQ.'3'.OR.ITREE(I,J).EQ.'1'))
     1       OUT=OUT*.8
      WX(I,J)=OUT
 150  CONTINUE
C
C
      DO 250 J=1,JMXB
      DO 250 I=1,IMXB
      IF (ITREE(I,J).EQ.'6') cycle
      XLT=YLT(I,J)
      XLN=YLG(I,J)
      CALL GRDINT(XLT,XLN,CLAT,CLON,WGT,OUT,XIV,res_wnd)
      IF (IGEO.EQ.1.AND.(ITREE(I,J).EQ.'3'.OR.ITREE(I,J).EQ.'1'))
     1       OUT=OUT*.8
      WY(I,J)=OUT
 250  CONTINUE
C
C
      DO 450 J=1,JMXB
      DO 450 I=1,IMXB
      IF (ITREE(I,J).EQ.'6') cycle
      XLT=YLT(I,J)
      CORR=2.*(7.292116E-5)*SIN(XLT*1.74532925E-2)
      XLN=YLG(I,J)
      CALL GRDINT(XLT,XLN,CLAT,CLON,WGT,OUT,XIUG,res_wnd)
      PPY(I,J)=-OUT*CORR/1000.
 450  CONTINUE
C
      DO 550 J=1,JMXB
      DO 550 I=1,IMXB
      IF (ITREE(I,J).EQ.'6') cycle
      XLT=YLT(I,J)
      CORR=2.*(7.292116E-5)*SIN(XLT*1.74532925E-2)
      XLN=YLG(I,J)
      CALL GRDINT(XLT,XLN,CLAT,CLON,WGT,OUT,XIVG,res_wnd)
      PPX(I,J)=OUT*CORR/1000.
 550  CONTINUE
C
C        CHANGE NORTH/SOUTH COORDINATES INTO X/Y COORDINATE BY
C        ROTATING THE GRIDS
      CS=COS(PHI1)
      SS=SIN(PHI1)
C
      DO 600 J=1,JMXB
      DO 600 I=1,IMXB
      IF (ITREE(I,J).EQ.'6') cycle
      ZA= CS*WX(I,J)-SS*WY(I,J)
      ZAA=SS*WX(I,J)+CS*WY(I,J)
      WX(I,J)=ZA
      WY(I,J)=ZAA
      ZA= CS*PPX(I,J)-SS*PPY(I,J)
      ZAA=SS*PPX(I,J)+CS*PPY(I,J)
      PPX(I,J)=ZA
      PPY(I,J)=ZAA
 600  CONTINUE
C
      RETURN
      END
C
      SUBROUTINE GRDINT(XLATIN,YLONIN,CLAT,CLON,WGT,FLDOUT,FLD,res_wnd)
C        J. CHEN                 MAY 1992 MDL   IBM 360/195
C
C        PURPOSE
C        THIS SUBROUTINE INTERPOLATES THE DATA BETWEEN TWO TIME LEVELS.
C        IT USES BI-LINEAR INTERPOLATIONS IN SPACE. IN TIME-INTERPOLA
C        TIONS, LINEAR WEIGHTING WITH THE COMPOSITE OF TWO FIELDS
C        FOLLOWING THE TRANSLATION VECTOR OF THE STORM CENTER AT TWO
C        TIME LEVELS. THE INPUT FIELDS ARE IN LAT/LONG GRID (1 DEGREE
C        RESOLUTION). TRANSLATES FORWARD FOR 1ST-FIELD, BACKWARD FOR
C        2ND-FIELD, THEN FORMS THE COMPOSITE WITH APPROPRIATE
C        WEIGHTINGS.
C
C        DATA SET USE
C           NONE
C
C        VARIABLES
C         IMXC,JMXC = DIMENSIONS OF INPUT FIELDS (1 DEG. INTERVAL)
C        FLD( , ,2) = DATA FIELDS OF TWO TIME LEVELS
!c--------------------------------------------------------------------------
!     Huiqing.Liu Added the following comments on March. 2014
!
!     XLATIN,YLONIN = SLOSH GRID LAT/LON ON WHICH INTERPOLATED VALUE IS
!     TO
!                     BE ASSIGNED, WHICH VALUE IS NEGATIVE (IF LON IS
!                     WEST)
!     XLAT1,XLON1   = LAT/LON OF NORTH/WEST CORNER OF THE DATA FIELD,
!                     WHICH VALUE IS POSITIVE (LON WITH W IF LON IS
!                     WEST) 
!c--------------------------------------------------------------------------
C            FLDOUT = INTERPOLATED FIELD VALUE AT XLATON,YLONIN
C               WGT = WEIGHTING FACTOR FOR TWO TIME LEVEL, = 0 REDUCED
C                     TO 1ST TIME LEVEL, = 1 2ND TIME LEVEL
C         CLAT,CLON = TRANSLATION VECTOR BETWEEN TWO TIME LEVEL OF THE
C                     STORM CENTER (DEEP LOW PRESSURE CENTER)
C
!     Increase to .5 by .5 Degree Huiqing.Liu /MDL April 2014
      COMMON /DATINC/ XLAT1,XLON1,DELLT,DELLN,IMXC,JMXC
       DIMENSION ZFLD(2)
!     GFS wind resolution
      real :: res_wnd
      integer :: ires
! H.Liu 04/2020 Declare FLD by using the exact dimension instead of a
! common big dimension number
      real :: FLD (IMXC,JMXC,2)

!-----------------------------------------------------------
!      res_wnd=0.5 !GFS wind resolution (unit degree)
      ires=1/res_wnd

!      

C
C       XLATIN,YLONIN(-) = TARGET LAT/LON
C         FLD( ,) = SOURCE DATA FIELD
C       CLAT,CLON = TRANSLATION VECTOR FROM FIRST TO SECOND DATA FLD
C       WGT       = DISTANCE ALONG TRANSLATION VECTOR (NORMALIZE TO 1)
C                   ALSO, THE WEIGHTING FACTOR FOR TWO DATA FIELD
C
      DO 100 K=1,2
      IF (K.EQ.1) THEN
          FCT=WGT
          ELSE
          FCT=-1.+WGT
      ENDIF
      XLAT=XLATIN-CLAT*FCT
      YLON=YLONIN+CLON*FCT
      if (YLON > 0) YLON = YLON - 360 !(H.Liu EAST longitud is positive in SLOSH)
!      PA=(XLAT1-XLAT)*2+1.
!      QA=(YLON+XLON1)*2+1.
      PA=(XLAT1-XLAT)*ires+1.
      QA=(YLON+XLON1)*ires+1.

      M=PA
      N=QA
C
      IF (M.LT.1.OR.N.LT.1.OR.M.GT.IMXC.OR.N.GT.JMXC) THEN
C      WRITE (*,*)'  LAT/LON',XLAT,YLON,XLATIN,YLONIN
C      WRITE (*,*) '  GRID POINT OUT OF BOUND ',PA,QA,M,N
       ELSE
      W1=PA-M
      W2=QA-N
      ZFLD(K)=(1.-W1)*(1.-W2)*FLD(M,N,K)+W1*(1.-W2)*FLD(M+1,N,K)
     1  +(1.-W1)*W2*FLD(M,N+1,K)+W1*W2*FLD(M+1,N+1,K)
C
      ENDIF
 100  CONTINUE
C
      FLDOUT=ZFLD(1)*(1.-WGT)+ZFLD(2)*WGT
C
      RETURN
      END
      SUBROUTINE FRCPNT1(I,J,FXB,FYB,NCATG,CSHLTR,NPLS)
C        J. CHEN               MAY 1992 MDL   IBM 360/195
C
C        PURPOSE
C           THIS SUBROUTINE (FRCPNT) COMPUTES COMPONENTS OF SURFACE
C           DRIVING FORCES AT A MOMENTUM GRID POINT. COMPUTATIONS ARE
C        DATA SET USE
C           NONE
C
C        VARIABLES
C            CSHLTR = EXTINCTION COEF OF STRESS FOR WATER LESS THAN 1 FT
C            X12(4) = RADIUS OR MAX WIND IN FEET
C           X12(15) = SQUARE OF X12(4)
C    X12(9) X12(10) = COMPONENTS OF FORWARD SPEED, 1ST HOUR
C               C19 = FRIC CFFCNT (3X10-6)
C           FXX FYY = COMPONENTS OF STRESS
C           PXX PYY = COMPONENTS OF SURFACE PRESSURE GRADIENT FORCE
C         FXBP FYBP = COMPONENTS OF SURFACE DRIVING FORCES IN (X,Y)
C           FXB FYB = COMPONENTS OF SURFACE FORCES ON IMAGE PLANE
C        WX,WY( , ) = SURFACE WIND COMPONENTS IN X/Y COORDINATE
C      PPX,PPY( , ) = PRESSURE GRADIENT FORCING IN X/Y COORDINATE
C
C        GENERAL COMMENTS
C
C   AWK MODIFYING 09/2010 FOR NEW LARGER GULF OF MEX BASIN
      include 'parm.for'
C
      parameter (ndp=600)
      COMMON /FORCE/ WX(M_,N_),WY(M_,N_),PP(M_,N_),PPX(M_,N_),PPY(M_,N_)
      COMMON /FRST/   X1(50),X12(50)
      COMMON /FRTH/   IC12,IC19,BT
      COMMON /THRD/   C7,C17,C19,C25
      COMMON /STRMSB/ C1,C2,C21,C22,AX,AY,PTENCY,RTENCY
      COMMON /SCND/   AR(ndp),AI(ndp),BR(ndp),BI(ndp),CR(ndp),CI(ndp)
      COMMON /MF/     DPH,HPD(4),ID
!-------------------------------------------------------
! Added Basin Name Variable by Huiqing.Liu/MDL June/2015
! for Modifying Cd wind drag coefficient parameter only
! in New Bering Sea extra-tropical basin (eno3)
!-------------------------------------------------------
      COMMON /GPRT/   STA
      CHARACTER*16  STA
C
C        COMPUTING FORCE AT A GRID POINT
C       STRESS =DRAG COEFF* SPEED * WIND VECTOR
C       STRESS REDUCTION FOR WATER LESS THAN 1 FT, AND SPIN-UP PERIOD
      A=WX(I,J)
      B=WY(I,J)
C      FCTT=C19*SQRT(A*A+B*B)
C         DRAG COEFFICIENT DEPENDING ON WIND SPEEDS
      FCTW=SQRT(A*A+B*B)
C     WU'S FORMULA
C      C19X=1.0236E-6+5.366E-8*FCTW/3.28084
C
C      DRAG COEFF. TO BEST-FIT ET-SURGE HINDCASTS (JAN.-FEB., 1998)
C
!------------------------------------------------
!Added by Huiqing.Liu in June-2015
! Using new modified Cd wnd Drag in eno3
!------------------------------------------------
      IF (STA(2:10) == 'ALASKA BE'.or.STA(2:10) == 'EXPANDED N') THEN
         ZZZ=1.02+0.1841*FCTW/3.28084  ! New modified Cd version
         ZZZ=MIN(3.9,ZZZ)
!         WRITE(*,*)"Using new Cd wnd Drag Coefficient in Basin ebbc and ny3"   
      ELSE
         ZZZ=1.38+0.081*FCTW/3.28084   ! Old ETSS version
         ZZZ=MIN(3.,ZZZ)
      ENDIF
      C19X=ZZZ*1.E-6

      FCTT=C19X*FCTW
C
      FCTT=FCTT*CSHLTR*BT
      FXX=FCTT*A
      FYY=FCTT*B
C
      FXB=CR(ID)*FXX-CI(ID)*FYY
      FYB=CR(ID)*FYY+CI(ID)*FXX
C        PRESSURE TERM IN THE FORCING FUNCTIONS
C
         IF (NPLS.NE.0) THEN
C
      PXX=-DPH*PPX(I,J)
      PYY=-DPH*PPY(I,J)
      FXB=FXB+BR(ID)*PXX-BI(ID)*PYY
      FYB=FYB+BR(ID)*PYY+BI(ID)*PXX
         ENDIF
C
      RETURN
      END
      SUBROUTINE GEOWND(res_wnd)
C        J. CHEN               MAY 1992 MDL   IBM 360/195
C
C        PURPOSE
C           THIS SUBROUTINE CALULATES THE PRESSURE GRADIENT FORCINGS, AN
C           CONVERTS ALL FORCINGS IN PROPER UNITS USED IN SLSOH EQUATION
C           WIND SPEEDS ARE CONVERTED FROM METER/SEC TO FT/SEC, PRESSURE
C           FROM MB TO HEIGHT OF WATER COLUMN, PRESSURE GRADIENTS FROM
C           MB/DEG TO GRADIENT CURRENT UNIT, FT/SEC INCORPERATING CORIOL
C           PARAMETERS AT DIFFERENT LATITUDES.
C
C        DATA SET USE
C           NONE
C
C        VARIABLES
C     IU,IV,XIU,XIV = U/V SURFACE WIND COMPONENTS IN LAT/LON GRID
C            IP,XIP = PRESSURE IN TERMS OF WEIGHT OF A WATER COLUMN
C IUG,IVG,XIUG,XIVG = CURRENT UNIT DUE TO ATM. PRESSURE GRADIENTS
C        YLT,YLG(,) = LAT/LON OF THE SLOSH GRID OF A BASIN
C             RHOWG = WATER DENSITY*GRAVITY (MB FT**-1)
C
C        GENERAL COMMENTS
C          IU,IV,IP,IUG,IVG ARE INPUT DATA IN A SPECIAL UNITS, VALUES
C          TIME 10 STORED AS IN INTEGERS. IT NEED NOT BE SO IN FINAL
C          OPERATIONAL PROGRAM.
C
!           H. LIU   ARP 2020 MDL USING DYNAMIC ALLOCATED ARRAY FOR WIND
!           DATA
!                    XIU,XIV,XIP,XIUG,XIVG,IU,IV,IP,IUG,IVG

      use parm_gfs1
      use parm_gfs2
!     Increase to .5 by .5 Degree Huiqing.Liu /MDL April 2014
      COMMON /DATINC/ XLAT1,XLON1,DELLT,DELLN,IMXC,JMXC
!----------------------------------------------------------
!     GFS wind resolution
      real :: res_wnd
!-----------------------------------------------------------

C
C     CNSTN REDUCES PRESSURE GRADIENT/CORIOLIS FROM MB/DEG*SEC TO
C         M/S(WIND SPEED UNIT ), GEOSTROPHIC WINDS.
C -------------------------------------------------------------------
      DATA CNSTN/4.769134/
!-----------------------------------------------------------
!      res_wnd=0.5 !GFS wind resolution (unit degree)
!
!

C
C         CALCULATE THE GEOSTROPHIC WIND FROM FINITE DIFFERENCE
C         PRESSURE GRADIENT IN LAT/LONG COORDINATES.
C
! H.Liu 04/2020 comment this 200 loop out to save running time as it only
! converts integer to real, which doesn't impact loop 201
!        DO 220 J=1,JMXC
!        DO 220 I=IMXC,1,-1
!        ZP(I,J)=IP(I,J)
! 220    CONTINUE
C
C
        DO 201 I=2,IMXC-1
!     Increase to .5 by .5 Degree Huiqing.Liu /MDL April 2014
!        ZZ=SIN((XLAT1-(I-1)*0.5)*1.74532925E-2)
!        Z1=COS((XLAT1-(I-1)*0.5)*1.74532925E-2)
        ZZ=SIN((XLAT1-(I-1)*res_wnd)*1.74532925E-2)
        Z1=COS((XLAT1-(I-1)*res_wnd)*1.74532925E-2)

        DO 201 J=2,JMXC-1
!        XVG=.5*(ZP(I+1,J+1)-ZP(I+1,J-1)+ZP(I-1,J+1)-ZP(I-1,J-1))
!     $       /(2*res_wnd)
!        XUG=.5*(ZP(I+1,J+1)-ZP(I-1,J+1)+ZP(I+1,J-1)-ZP(I-1,J-1))
!     $       /(2*res_wnd)
        XVG=.5*(IP(I+1,J+1)-IP(I+1,J-1)+IP(I-1,J+1)-IP(I-1,J-1))
     $       /(2*res_wnd)
        XUG=.5*(IP(I+1,J+1)-IP(I-1,J+1)+IP(I+1,J-1)-IP(I-1,J-1))
     $       /(2*res_wnd)
        IUG(I,J)=CNSTN*XUG/(ZZ*Z1)
        IVG(I,J)=CNSTN*XVG/ZZ
  201  CONTINUE
C
C     EXTENDED TO BOUNDARY VALURES
        K=-1
        DO 203 J=1,JMXC,JMXC-1
        K=-K
        DO 203 I=1,IMXC
        IUG(I,J)=IUG(I,J+K)
        IVG(I,J)=IVG(I,J+K)
 203    CONTINUE
        K=-1
        DO 204 I=1,IMXC,IMXC-1
        K=-K
        DO 204 J=1,JMXC
        IUG(I,J)=IUG(I+K,J)
        IVG(I,J)=IVG(I+K,J)
 204    CONTINUE
C
      RETURN
      END
C
      SUBROUTINE GEO2BL(res_wnd)

      use parm_gfs2

!      include 'parm_gfs.for'
!     Increase to .5 by .5 Degree Huiqing.Liu /MDL April 2014
      COMMON /DATINC/ XLAT1,XLON1,DELLT,DELLN,IMXC,JMXC
!      COMMON /LANDS/ LANDS(150,180)
      CHARACTER*1    LANDS (IMXC,JMXC)

!----------------------------------------------------------
!     GFS wind resolution
      real :: res_wnd
!-----------------------------------------------------------

      DATA CNSTN/4.769134/
      DATA XKS/5.E-1/,ROT/7.292/,XKSL/10.E-1/
C
!      res_wnd=0.5 !GFS wind resolution (unit degree)
       CNST=XKS/(2.*ROT)
       CNSTL=XKSL/(2.*ROT)
C        SIN AND COS OF CROSS-ISOBARIC ANGLE (SPECIFIED CONSTANT)
C       ANGC=COS(ANG*1.74532925E-2)
C       ANGS=SIN(ANG*1.74532925E-2)
C
C      REDUCE THE GEOSTROPHIC WIND TO SEA LEVEL. ACCOUNTING THE
C      BOUNDARY LAYER FRICTION BY ROTATING THE VECTOR TOWARD LOW
C      CENTER, WITH A SPECIFIED CROSS-ISOBARIC ANGLE AND REDUCING THE
C      SPEED BY A FACTOR.
C
C
CIGEO=1 MODE --- REPLACE INPUT WINDS IU,IV.
C
        DO 202 I=1,IMXC
!     Increase to .5 by .5 Degree Huiqing.Liu /MDL April 2014
!        CORF=CNST/SIN((XLAT1-(I-1)*0.5)*1.74532925E-2)
!        CORFL=CNSTL/SIN((XLAT1-(I-1)*0.5)*1.74532925E-2)
        CORF=CNST/SIN((XLAT1-(I-1)*res_wnd)*1.74532925E-2)
        CORFL=CNSTL/SIN((XLAT1-(I-1)*res_wnd)*1.74532925E-2)

        DO 202 J=1,JMXC
C          ZIUG,ZIVG IN METER/SEC * 10.
       ZIUG=IUG(I,J)
       ZIVG=IVG(I,J)
       SPD=SQRT(ZIUG**2+ZIVG**2)/10.
       IF (LANDS(I,J).EQ.'0') THEN
        CORFZ=CORFL
        ELSE
        CORFZ=CORF
        ENDIF
C
       IF (SPD.GT.2.5) THEN
C
       ALPHA=SPD*CORFZ
C       COSINE AND SINE  ANG.
       ANGC=SQRT(-.5+.5*SQRT(1.+4.*ALPHA**2))/ALPHA
       ANGS=SQRT(1.-ANGC*ANGC)
C
       IU(I,J)=ANGC*(ANGC*ZIUG-ANGS*ZIVG)
       IV(I,J)=ANGC*(ANGS*ZIUG+ANGC*ZIVG)
       ENDIF
 202    CONTINUE
      RETURN
      END
C
      SUBROUTINE INITLZ1(ITIDE)
C        J. CHEN    SEPTEMBER 1980MDL   IBM 360/195
C
C        PURPOSE
C           THIS SECOND OVERLAY 'INITLZ' USES SUBROUTINE 'INITLZ' AS A
C           CALLING SEQUENCE FOR 5 SUBROUTINES.
C           THE PURPOSE OF THE OVERLAY IS TO FIX SOME PROGRAM CONSTANTS,
C           SOME POLAR GRID CONSTANTS; TO SET BATHY/TOPO AND
C           BARRIER HEIGHTS, AND TO INITIALIZE A TIMING SEQUENCE.
C
C        DATA SET USE
C           NONE
C
C        VARIABLES
C         ALTO (DEG)= LATITUDE OF BASIN CENTER OR ORIGIN
C
C        GENERAL COMMENTS
C     THIS SUBROUTINE RESIDES IN OVERLAY 'INITLZ'. IT IS CALLED
C           BY THE ROOT OVERLAY AND NOT BY ANY OTHER SUBROUTINE.
C
CC      PARAMETER (IBGS=48, ITMS=48)
!
! Huiqing.Liu /MDL June 2014 ETSS2.0 wind control window info and
! station output information 
!
      COMMON /IFT11 / LATIN1,LATIN2,LONIN1,LONIN2
!
!Huiqing.Liu /MDL
!
      COMMON /WNDENS/ MGEFS_CMCE
! Huiqing Liu/MDL March. 2018
! Huiqing Liu/MDL March. 2019 adding ecmwf option
! COMM BLOCK for GEFS (MGEFS_CMCE = 1) or CMCE (2) or ECMWF (3)
! Huiqing Liu/MDL March. 2020 adding newGEFS option
! COMM BLOCK for GEFS (MGEFS_CMCE = 1) or CMCE (2) or newGEFS(3) or ECMWF (4) or retrospective newGEFS (5)


      COMMON /BSN/    PHI,ALTO,ALNO,PHI1,ALT1,ALN1,ALT1C
      COMMON /STIME2/  ISTM,JHR,ITMADV,NHRAD,IBGNT,ITEND
      COMMON /SWTCH/  IOPERL(5)
      COMMON /GPRT1/  DOLLAR,EBSN
      COMMON /INZSTM/ IBGS,ITMS
      COMMON /DATUM/  SEADTM,DTMLAK
      COMMON /OPT/  NOPT
      COMMON /CESAV/ IPN(250),IPL(250),KHSPT,IPN2(250),IPL2(250),
     $               KHSPT2
      COMMON /CESAV1/ STATS(250),STATS2(250)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C DTA OPTIONS FOR DTA VERSION AND ELLIPSOID TYPE IF EXIST
C IVER = 199201(ORIGINAL DTA FORMAT) / 201408(NEW DTA FORMAT)
C IPRJ = 0(CLARKE ELLIPSOID) / 1(GRS80 ELLIPSOID)
C D.Y 8/2014
      COMMON /DTAOPT/ IVER IPRJ
      INTEGER IVER, IPRJ
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

! Huiqing.Liu /MDL Jan. 2015 speeding model in T1 mode
!---------------------------------------------------------!
! Increase Model Time Step if Tide model is T1 (tide only)!
! ITIDE=-1                                                !
!---------------------------------------------------------!

      COMMON /DLTDTA/ DLTIN(3),DLTB
!
!

      COMMON /FLES/ FLE5,FLE9,FLE8,FLE91,FLE99,FLE10,FLE20,FLE30,FLE1
      CHARACTER*30  FLE5,FLE9,FLE8,FLE91,FLE99,FLE10,FLE20,FLE30,FLE1
      CHARACTER*2     DOLLAR
      CHARACTER*1     EBSN
      CHARACTER*30    STATS,FLESTA,dummyN
      CHARACTER*32    STATS2
! Added by H.Liu/MDL Aug. 2018 for initial water level 
      real :: dtmlak_N(2), seadtm_N(2),wl_mean,wl_bias,wl_an_old
      character*1 BsnIndx
      integer :: Iset (250),wl_count,Iopt ! index to show whether water level bias at 
                                          ! the station included in calculating the 
                                          ! average the whole basin bias
      logical :: ThereCSV
      integer :: SizeCSV
C
!
! Huiqing.Liu /MDL June 2014 Initialize the input and output files
!
      CHARACTER*255 FIL_96,FIL_11,FIL_14,FIL_19,FIL_21,FIL_25,FIL_33,
     1 FIL_34,FIL_81,FIL_22,FIL_52,FIL_53,FIL_84
     2,FIL_15,FIL_55,FIL_87,FIL_88,FIL_54,FIL_12,FIL_50

!
C     W3TAGB needs (Name, Julian day (see next line), seconds, org)
C         tclsh : puts [clock format [clock seconds] -format "%j"]
      CALL W3TAGB('EXT_6H',2012,0341,0000,'OST25')
C
C     OPEN UNIT NUMBERS
C         Log file for any errors in run
      CALL GETENV('FORT96',FIL_96)
      OPEN (96,FILE=FIL_96)
C         Open the temporary files (ht,spd,dir)(basin,whole domain?)      
      CALL GETENV('FORT81',FIL_81)
      OPEN (81,FILE=FIL_81,FORM='UNFORMATTED')
!      CALL GETENV('FORT82',FIL_82)
!      OPEN (82,FILE=FIL_82,FORM='UNFORMATTED')
!      CALL GETENV('FORT83',FIL_83)
!      OPEN (83,FILE=FIL_83,FORM='UNFORMATTED')
!------------------------------------------------------
! Temp for Water Level above model datum
! Huiqing. Liu March, 2016
!-----------------------------------------------------
      CALL GETENV('FORT84',FIL_84)
      OPEN (84,FILE=FIL_84,FORM='UNFORMATTED')

!      CALL GETENV('FORT85',FIL_85)
!      OPEN (85,FILE=FIL_85,FORM='UNFORMATTED')
!      CALL GETENV('FORT86',FIL_86)
!      OPEN (86,FILE=FIL_86,FORM='UNFORMATTED')
      CALL GETENV('FORT87',FIL_87)
      OPEN (87,FILE=FIL_87,FORM='UNFORMATTED')
!------------------------------------------------------
! Temp for Water Level above ground level
! Huiqing. Liu March, 2016
!-----------------------------------------------------
      CALL GETENV('FORT88',FIL_88)
      OPEN (88,FILE=FIL_88,FORM='UNFORMATTED')
C         Open control window info
      CALL GETENV('FORT11',FIL_11)
      OPEN (11,FILE=FIL_11)
!
      CALL GETENV('FORT12',FIL_12)
      OPEN (12,FILE=FIL_12)

C         Open tide gauge location info
      CALL GETENV('FORT14',FIL_14)
      OPEN (14,FILE=FIL_14)
      CALL GETENV('FORT15',FIL_15)
      OPEN (15,FILE=FIL_15)
C         Open basin bathy/topo info
!      CALL GETENV('FORT19',FIL_19)
!      OPEN (19,FILE=FIL_19)
C         Open model run time control file
      CALL GETENV('FORT21',FIL_21)
      OPEN (21,FILE=FIL_21)
!         open water initial condition control file and data files
      CALL GETENV('FORT19',FIL_19)
      OPEN (19,FILE=FIL_19)
      CALL GETENV('FORT22',FIL_22)
!         open last cycle water initial water condtion file
       CALL GETENV('FORT50',FIL_50)

C         Open llx (lat/lon of momentum points)
!      CALL GETENV('FORT25',FIL_25)
!      OPEN (25,FILE=FIL_25,FORM='UNFORMATTED')
C         Open cycles of AVN analyses
      CALL GETENV('FORT33',FIL_33)
      OPEN (33,FILE=FIL_33,FORM='UNFORMATTED')
C         Open cycles of AVN forecast
      CALL GETENV('FORT34',FIL_34)
      OPEN (34,FILE=FIL_34,FORM='UNFORMATTED')
C         Open surge archive (and whole domain)
      CALL GETENV('FORT52',FIL_52)
      OPEN (52,FILE=FIL_52,FORM='UNFORMATTED')
      CALL GETENV('FORT55',FIL_55)
      OPEN (55,FILE=FIL_55,FORM='UNFORMATTED')
!------------------------------------------------------
! File for Water Level above model datum
! Huiqing. Liu March, 2016
!-----------------------------------------------------

      CALL GETENV('FORT53',FIL_53)
      OPEN (53,FILE=FIL_53,FORM='UNFORMATTED')
!------------------------------------------------------
! File for Water Level above ground level
! Huiqing. Liu March, 2016
!-----------------------------------------------------
      CALL GETENV('FORT54',FIL_54)
      OPEN (54,FILE=FIL_54,FORM='UNFORMATTED')
C
      READ (96,'(A1)') AAA
      IF (AAA.EQ.'N') THEN
      CALL W3TAGE('EXT_6H')
      STOP
      ENDIF

!
! Huiqing.Liu /MDL June 2014 ETSS2.0 read wind control window info and
! station output information
!
       READ (11,*) LATIN1,LATIN2
       READ (11,'(A1)') ACHR
       READ (11,*) LONIN1,LONIN2
       READ (11,*)
! Huiqing Liu/MDL March. 2018
! COMM BLOCK for GEFS (MGEFS_CMCE = 1) or CMCE (2) or NewGEFS (3) or ECMWF (4) or NewGEFS Retro(5)
       MGEFS_CMCE = 1
       READ (12,*) MGEFS_CMCE
       CLOSE (12)
C ---------------- GAGE STATIONS USED FOR OUTPUT ------------
      READ (14,'(I3)') KHSPT
      DO N=1,KHSPT
         READ (14,'(A30,18X,2I4)') STATS(N),IPN(N),IPL(N)
!         write(*,*)'stat,i,j: ',stats(n),ipn(n),ipl(n)
      ENDDO
!----------------Second GAGE STATIONS USED FOR OUTPUT ------------
! Added by H.Liu/MDL Aug. 2018 for initial water condition
! Reading the basin name to detmine which water level value need (GOM/East)
      READ(19,*) BsnIndx,Iopt

      READ (15,'(I3)') KHSPT2
!      write(*,*) 'KHSPT2 =',KHSPT2
      wl_mean = 0.
      wl_count = 0
      DO N=1,KHSPT2
         READ (15,'(A32,16X,2I4,I2)') STATS2(N),IPN2(N),IPL2(N),Iset(N)
         if (Iset(N) == 1.and.Iopt ==2) then
            inquire(file=STATS2(N)(1:7)//'.csv',exist=ThereCSV)
! Added by Huiqing.Liu /MDL in June 2020 to make sure csv file is not empty
            inquire(file=STATS2(N)(1:7)//'.csv',size=SizeCSV)
            if (ThereCSV.and.SizeCSV.gt.0) then
               open(105,file=STATS2(N)(1:7)//'.csv',status='old')
!               do ii = 1, 2221
!               do ii = 1, 1201 ! 120 hrs re-analysis
!               do ii = 1, 1021 ! 102 hrs re-analysis
               do ii = 1, 1141 ! 114 hrs re-analysis
                  read(105,*)dummyN
               enddo
!               do ii = 1, 6 ! 6 hrs anomaly mean
!               do ii = 1, 24 ! 24 hrs anomaly mean
               do ii = 1, 12 ! 12 hrs anomaly mean
                  read(105,*)dummyN,dummyN,dummyN,dummyN,wl_bias,dummyN
     $                       ,dummyN,dummyN,dummyN,dummyN
                  do ij = 1, 9
                     read(105,*)dummyN
                  enddo
                  if (abs(wl_bias) < 2.0) then
                     wl_mean = wl_mean + wl_bias
                     wl_count = wl_count + 1
                  endif
               enddo
! Get the mean bias used in the post-processing of last run
!               do ii = 1,240
!                  read(105,*)dummyN,dummyN,dummyN,dummyN,wl_and_old,
!     $                       dummyN,dummyN,dummyN,dummyN,dummyN
!               enddo
               close(105)
            endif
         endif
      ENDDO

      if (wl_count > 0) wl_mean = wl_mean/wl_count
      
      inquire(file=FIL_50,exist=ThereCSV)
      if (ThereCSV.and.Iopt ==2) then
         OPEN (50,FILE=FIL_50)
         read(50,*)wl_an_old
         wl_mean = wl_mean + wl_an_old
         close(50)
      endif
      if (wl_mean > 2) wl_mean = 2.
      if (wl_mean < -2) wl_mean = -2.
!
! Huiqing.Liu /MDL
!
C          READ BASIN PROJECTION DATA
      IVER = 199201
      IPRJ = 0
      CALL CRDRD1(IVER,IPRJ)
!      CALL CRDRD1
! Huiqing.Liu /MDL Jan. 2015 speeding model in T1 mode
!---------------------------------------------------------!
! Increase Model Time Step if Tide model is T1 (tide only)!
! ITIDE=-1                                                !
!---------------------------------------------------------!
      IF (ITIDE.EQ.-1.AND.DLTIN(1).LT.100)THEN
         DLTIN(1)=100.
         DLTIN(2)=100.
         DLTIN(3)=100.
         DLTB=100.
      ENDIF
!

C
C        JHR=61, MAX 60 HOURS  SPIN-UP + HINCAST  TIME.
C        IBGS=48 MODEL SET 48 HOURS SPIN-UP ANALYSES FIELDS EVERY 6 HR. (DEFAULT)
C        ITMS=96 MODEL SET 96 HOURS FORECAST FIELDS EVERY 3 HR. (DEFAULT)
      IBGS=48
      ITMS=96
      JHR=61

C----------- OPTION TO OVERRIDE THE SPIN-UP TIME (# HOURS)  -------
C 
      READ(21,*) IBGS,ITMS
      close(21)
      close(19)
      if (Iopt == 1) then ! Using one anomaly for whole est,gom
! Reading the water-levlels.dat from NHC H.Liu/MDL Aug. 2018
         OPEN (22,FILE=FIL_22)
         do Ibsn = 1,2
            read(22,*) dummyN,dtmlak_N(Ibsn)
             read(22,*) dummyN,seadtm_N(Ibsn)
         enddo
         close(22)

         if (BsnIndx == 'e') then
            SEADTM = seadtm_N (1)
            DTMLAK = dtmlak_N (1)
         else if (BsnIndx == 'g') then
            SEADTM = seadtm_N (2)
            DTMLAK = dtmlak_N (2)
         else
            SEADTM = 0
            DTMLAK = 0
         endif
      else if (Iopt == 2) then  ! Using different anomaly each tropical basin    
            SEADTM = wl_mean
            DTMLAK = wl_mean
      else
            SEADTM = 0
            DTMLAK = 0
      endif
!
!     write the current used water level anomaly into file for next cycle run use.
!
      OPEN (50,FILE=FIL_50)
      write (50,*) SEADTM
      close(50)
!
!-------------------------------------------------------------------     
!
!-------------------------------------------------------------------      
 2000 CONTINUE
      WRITE (*,*) 'Spinup Time is (Hr):', IBGS
      WRITE (*,*) 'Forecast Time is (Hr):', ITMS
C       CLOSE (1)
       DTMLAK=SEADTM
       IBGNT=MAX0(JHR-IBGS,1)
      ITEND=JHR+ITMS

C ---------------- GAGE STATIONS USED FOR OUTPUT ------------
CC      READ (14,'(I3)') KHSPT
C     WRITE (*,'(A,I3)') ' NO. OF STA',KHSPT
CC      DO 2001 N=1,KHSPT
CC      READ (14,'(A30,18X,2I4)') STATS(N),IPN(N),IPL(N)
C     WRITE (*,'(A28,20X,2I4)') STATS(N),IPN(N),IPL(N)
 2001 CONTINUE
C      CLOSE (4)
C
      IF (IVER .EQ. 199201) THEN
        CALL CRDRD2
      ELSE
        CALL CRDRD2_N
      ENDIF

!       CALL CRDRD2
!
       IF (DOLLAR.EQ.'2$') CALL DEPMSY
!
       CALL DEPSFC
       if(idep.eq.1)call DEF_NESTING_BOUN
C
       CALL BTMSTR(ALTO)
       CALL SETDST
       CALL INTDLT
       CALL INTVAL
       CALL CNSTNT
      RETURN
      END
      SUBROUTINE HISTRY1
C        J. CHEN   OCTOBER        MDL   IBM 360/195
C
C        PURPOSE
C           THIS SUBROUTINE (HISTRY) SMOOTHS THE SURGE AT 10
C           PRE-SELECTED GRID POINTS AND SAVES THEM ON TAPE FOR FUTURE
C           PRINT OUT. ALSO, WIND SPEED AND DIRECTION ARE ALSO SAVED AT
C           THE SAME 10 POINTS.
C        DATA SET USE
C           FT 10, TEMPORARY SCRATCH TAPE FOR SURGE STORAGE
C           FT 20, TEMPORARY SCRATCH TAPE FOR WIND SPEED STORAGE
C           FT 30, TEMPORARY SCRATCH TAPE FOR STORAGE OF WIND DIRECTION
C
C        VARIABLES
C      IPN(), IPL() = SUBSCRIPTS FOR SELECTED POINTS
C         HB(  ,  ) = SURGE FIELD
C        ZB(  ,   ) = DEPTH FIELD
C       IP(4) JP(4) = SHIFT SUBSCRIPTS TO 4 ADJACENT MOMENTUM POINTS
C       ZBM(  ,   ) = MAXIMUM BARRIER HEIGHT AT A MOMENTUM POINT
C COST(  ) SINT(  ) = RAY PROJECTIONS THRU MOMNTM POINTS, ON (X,Y) AXIS
C        GRIDRF(  ) = RADIAL DISTANCES TOMOMENTUM POINTS
C             AX AY = CMPNENTS OF TOTAL STRM TRVRSE, ADVNCD IN 'STMVAL'
C             C1 C2 = COMPONENTS OF INITIAL STORM POSITION
C             NCATG = 1 FOR LAKE WINDS, 2 FOR OCEAN WINDS
C          W1218(2) = TWICE THE MAXIMUM WINDS, LAKE/OCEAN WINDS
C   CW(800) SW(800) = INFLOW ANGLE FOR LAKE WINDS
C     C(800) S(800) = INFLOW ANGLE FOR OCEAN WINDS
C           X12(50) = FIXED COEFFICIENTS
C    X12(9) X12(10) = COMPONENTS OF FORWARD SPEED, 1ST HOUR
C            WIND() = WIND SPEED AT SELECTED POINTS
C            TDIR() = WIND DIRECTION AT SELECTED POINTS
C               PHI = SLANT, Y-AXIS TO N/S DIRECTION, AZMTH+270
C               RAD = RADIAN MEASURE
C             IPRHR = 1,WHEN ON THE HOUR; 2, WHEN HALF HOUR IN BETWEEN;
C                     3, OTHERWISE
C
C        GENERAL COMMENTS
C           THIS SUBROUTINE RESIDES IN OVERLAY 'CMPUTE'. IT IS CALLED
C           IN BY SUBROUTINE 'CMPUTE' AT SELECTED TIME INTERVALS.
C
C   AWK MODIFYING 09/2010 FOR NEW LARGER GULF OF MEX BASIN
!      PARAMETER (M$=600,N$=600,L$=5000,NCT$=2500,LC$=L$)
      include 'parm.for'

      COMMON /DUMB3/  IMXB,JMXB,IMXB1,JMXB1,IMXB2,JMXB2
C
      COMMON /FORCE/ WX(M_,N_),WY(M_,N_),PP(M_,N_),PPX(M_,N_),PPY(M_,N_)
      COMMON /FRTH/   IC12,IC19,BT
      COMMON /PRHSY/  IPRHR,JUMPR,IPRT
      COMMON /CESAV/ IPN(250),IPL(250),KHSPT,IPN2(250),IPL2(250),
     $               KHSPT2
      COMMON /CESAV1/ STATS(250),STATS2(250)
      COMMON /STRMSB/ C1,C2,C21,C22,AX,AY,PTENCY,RTENCY
      COMMON /SSTME/  ISTM,JHR,ITMADV,NHRAD,IBGNT,ITEND
      COMMON /FRST/   X1(50),X12(50)
      COMMON /BSN/    PHI,ALTO,ALNO,PHI1,ALT1,ALN1,ALT1C
C---------------------------------------------------------
!      COMMON /SUBDCE/ ZSUBCE,JSUB,JSUB1,ZSUB(N$)
      COMMON /GPRT1/  DOLLAR,EBSN
!
! Huiqing.Liu prepare output Current Magnitude
!
      COMMON /CURRENT/ CurrentMag(M_,N_),U_Current(M_,N_),
     1                 V_Current(M_,N_)
!
!
!      DIMENSION       WIND(250),TDIR(250)
      DIMENSION       SAVEH(250),SAVEH2(250)
      DIMENSION       SH(M_,N_),SH_AGL(M_,N_)
      CHARACTER*2     DOLLAR
      CHARACTER*1     EBSN
C---------------------------------------------------------
      CHARACTER*30    STATS
      CHARACTER*32    STATS2
      DATA KHR,KHR2,KHR3/1,0,0/
C
C
C       KEEP TRACK TIME, ON THE HOUR,HALF HOUR BETWEEN, OR ELSE
C       FROM MOD(IME,100)=1,OR 2,OR 3
C
!------------------------------------------------------------
!     rewrite the following codes to remove the computed GOTO
!     Huiqing.Liu /MDL Feb. 2015
!------------------------------------------------------------
!--------------------------------------------------------------
! Old codes
!---------------------------------------------------------------

!       GO TO (10,20,30),IPRHR
!   10  KHR=KHR+1
!       IME=KHR*100+1
!       GO TO 40
!   20  KHR2=KHR2+1
!       IME=KHR2*100+2
!       GO TO 40
!   30  KHR3=KHR3+1
!      IME=KHR3*100+3
!   40  CONTINUE
!--------------------------------------------------------------
! New codes
!---------------------------------------------------------------
      IF (IPRHR == 1) THEN
         KHR=KHR+1
         IME=KHR*100+1
      ELSEIF (IPRHR ==2) THEN
         KHR2=KHR2+1
         IME=KHR2*100+2
      ELSEIF (IPRHR ==3) THEN
         KHR3=KHR3+1
         IME=KHR3*100+3
      ENDIF           
!
!---------------------------------------------------------------
C********SMOOTHING HISTORICAL SURGES AT 10 SELECTED GRID POINTS*********
      DO 1190 L=1,KHSPT
      I=IPN(L)
      J=IPL(L)
C
!-----------------------------------------------------------------------------
!Added by Huiqing.Liu Oct. 2014
!Assign the default surge value to the stations which are not in basin
!grids
!-----------------------------------------------------------------------------
       IF (I.GE.IMXB.OR.J.GE.JMXB.OR.(I*J).LE.0)THEN
          SAVEH(L) = 99.
       ELSE
          SAVEH(L)=HB(I,J)
          IF (ZB(I,J).EQ.-HB(I,J)) SAVEH(L) = 99.
       ENDIF
!---------------------------------------------------------------------------
!      SAVEH(L)=HB(I,J)
!      IF (ZB(I,J).EQ.-HB(I,J)) SAVEH(L) = 99.
C     
!      IF (I.GE.IMXB.OR.J.GE.JMXB.OR.(I*J).LE.0)THEN
!         A=99.
!         B=99.
!      ELSE
!         A=WX(I,J)
!         B=WY(I,J)
!      ENDIF
!      WIND(L)=SQRT(A*A+B*B)/3.28084
!      IF (WIND(L).LT.1.E-5) THEN
!       Z=180.
!       ELSE
!       Z =ATAN2(A,B)/1.74532925E-2+180.
!       ENDIF
!      TDIR(L)=AMOD(Z+PHI,360.)
 1190 CONTINUE

! Second output list
      DO L=1,KHSPT2
      I=IPN2(L)
      J=IPL2(L)
C
!-----------------------------------------------------------------------------
!Added by Huiqing.Liu Oct. 2014
!Assign the default surge value to the stations which are not in basin
!grids
!-----------------------------------------------------------------------------
       IF (I.GE.IMXB.OR.J.GE.JMXB.OR.(I*J).LE.0)THEN
          SAVEH2(L) = 99.
       ELSE
          SAVEH2(L)=HB(I,J)
          IF (ZB(I,J).EQ.-HB(I,J)) SAVEH2(L) = 99.
       ENDIF
!---------------------------------------------------------------------------
C
      ENDDO

!
C        WRITING HISTORICAL SURGES AT SELECTED GRID PTS ONTO TAPE
      WRITE (81)   IME,(SAVEH(N),N=1,KHSPT)
      WRITE (87)   IME,(SAVEH2(N),N=1,KHSPT2)
C      WRITE(*,'(I5,10F6.2,F10.2)')   IME,(SAVEH(N),N=1,10),BT
!      WRITE (82)   IME,(WIND(N),N=1,KHSPT)
!      WRITE (83)   IME,(TDIR(N),N=1,KHSPT)
C       WRITE(*,'(5X,10F6.2,F10.2)')   (WIND(N),N=1,10)
C      WRITE(*,'(5X,10F6.2,F10.2)')   (TDIR(N),N=1,10)
C  WRITE HISTORICAL SURGES, WIND SPEED & DIRECTION ON ALL GRIDS
      DO 1300 J=1,JMXB
        DO 1200 I=1,IMXB
          SH(I,J)=HB(I,J)
          IF(ZB(I,J).EQ.-HB(I,J)) SH(I,J) = 99.
!----------------------------------------------------------------
! Generate the water level above ground level at inundated grids
! Huiqing.Liu /MDL March,2016
!----------------------------------------------------------------
          SH_AGL(I,J) = SH(I,J)
          IF (ZB(I,J).LE.0.AND.HB(I,J).GE.0.AND.SH(I,J).NE.99) THEN
             IF (HB(I,J)> -ZB(I,J)) THEN
                SH_AGL(I,J) = HB(I,J) + ZB(I,J)
             ELSE
                SH_AGL(I,J) = 99
             ENDIF
          ENDIF
!
! Huiqing.Liu prepare output Current Magnitude
!          IF(ZB(I,J).EQ.-HB(I,J)) CurrentMag(I,J) = 99.
!
 1200   CONTINUE
 1300 CONTINUE
C      WRITE (84)   ((HB(I,J),I=1,IMXB),J=1,JMXB)
      WRITE (84)   ((SH(I,J),I=1,IMXB),J=1,JMXB)
      WRITE (88)   ((SH_AGL(I,J),I=1,IMXB),J=1,JMXB)
!
! Huiqing.Liu prepare output Current Magnitude
!      WRITE (85)   ((CurrentMag(I,J),I=1,IMXB),J=1,JMXB)
!
c      write(*,*) 'imxb=',imxb,'jmxb=',jmxb
      RETURN
      END

      SUBROUTINE FLEGEN
C   INPUT FILES:
C     FORT.11  - ETTGP  TIDE GAGE STATIONS TO BE MONITORED
C     FORT.81  - FLE21   SURGE HEIGHT (FLE10)
C     FORT.82  - FLE22   WIND SPEED   (FLE20)
C     FORT.83  - FLE23   WIND DIRECTION (FLE30)
C   OUTPUT FILE:
C     FORT.52   - SURGE.HH    SURGE ARCHIVE
C
C        IBGS - MODEL SPIN-UP HR. 
C        ITMS - MODEL FORECAST HR.

      COMMON /CESAV/ IPN(250),IPL(250),KHSPT,IPN2(250),IPL2(250),
     $               KHSPT2
      COMMON /CESAV1/ STATS(250),STATS2(250)
      COMMON /INZSTM/ IBGS,ITMS
      DIMENSION A(250)
      CHARACTER*30 STATS
      CHARACTER*32 STATS2

       MHOUR=IBGS+ITMS

       WRITE (52) KHSPT,MHOUR
       WRITE (55) KHSPT2,MHOUR
!       write(*,*) 'write into hist KHSPT2=',KHSPT2
       DO 10 N=1,KHSPT
       WRITE (52) STATS(N),IPN(N),IPL(N)
  10   CONTINUE

       DO N=1,KHSPT2
          WRITE (55) STATS2(N),IPN2(N),IPL2(N)
       ENDDO

!       NH2=MHOUR*2
!       DO 20 N=1,NH2
!       READ (81,END=999) IME,(A(L),L=1,KHSPT)
!       WRITE (52) IME,(A(L),L=1,KHSPT)
! 20    CONTINUE
! 999   CONTINUE
!       DO N=1,NH2
!       READ (87) IME,(A(L),L=1,KHSPT2)
!       WRITE (55) IME,(A(L),L=1,KHSPT2)
!       ENDDO
!       DO 30 N=1,NH2
!       READ (82) IME,(A(L),L=1,KHSPT)
!       WRITE (52) IME,(A(L),L=1,KHSPT)
! 30    CONTINUE
!       DO 40 N=1,NH2
!       READ (83) IME,(A(L),L=1,KHSPT)
!       WRITE (52) IME,(A(L),L=1,KHSPT)
! 40    CONTINUE
      RETURN
      END
C
      SUBROUTINE FLEGEN2
C
C        Y. HAO   OCTOBER 2007   MDL,OPC
C
C        PURPOSE
C           THIS PROGRAM CONCATENATE THE SURGE HEIGHT, WIND SPEED AND
c           WIND DIRECTION INTO ONE FILE FOR LATER PROCESSING. 
C
C        VARIABLES
C               A = TEMPORARY ARRAY TO READ DATA FROM TEMP FILE AND 
c                   WRITE DATA INTO OUTPUT FILE. 
C           MHOUR = TOTAL COMPUTATION HOURS
C
C        GENERAL COMMENTS
C
C        INPUT FILES:
C          FORT.84  - FLE40.TMP   SURGE HEIGHT 
C          FORT.85  - FLE50.TMP   WIND SPEED
C          FORT.86  - FLE60.TMP   WIND DIRECTION
C
C        OUTPUT FILE:
C          FORT.53   - SSGRID.CCB (CC = CYCLE, B=BASIN(E,G,W,A,Z))
C                      SURGE ARCHIVE
C
C   AWK MODIFYING 09/2010 FOR NEW LARGER GULF OF MEX BASIN
C        IBGS - MODEL SPIN-UP HR. 
C        ITMS - MODEL FORECAST HR.

      PARAMETER (M_=999,N_=999)
      COMMON /INZSTM/ IBGS,ITMS
      COMMON /DUMB3/  IMXB,JMXB,IMXB1,JMXB1,IMXB2,JMXB2
      DIMENSION A(M_,N_),A_AGL(M_,N_)

      MHOUR=IBGS+ITMS
      WRITE (53) MHOUR
      WRITE (54) MHOUR
!      NH2=MHOUR*2
C      write(*,*) 'imxb1=',imxb,'jmxb1=',jmxb
!      DO 20 N=1,NH2
!       READ (84,END=999)((A(I,J),I=1,IMXB),J=1,JMXB)
!       READ (88,END=999)((A_AGL(I,J),I=1,IMXB),J=1,JMXB)
C------------------------------------------------------------------------------
C Huiqing.Liu /MDL March 2017
C Write Gridded Field to files after spin up time (48/60 hr), 2 times/hour
C N.GE.94 ---- Spinup 48 hrs and N.GT.118 ----- Spinup 60 hrs
C------------------------------------------------------------------------------
!       IF (N.GT.94) THEN
!       IF (N.GT.IBGS*2-2) THEN
!        WRITE (53)((A(I,J),I=1,IMXB),J=1,JMXB)
!        WRITE (54)((A_AGL(I,J),I=1,IMXB),J=1,JMXB)
!       ENDIF
! 20   CONTINUE
 999  CONTINUE
      RETURN
      END

      SUBROUTINE BDRYHT_ETSS
C        J. CHEN               MAY 1992 MDL   IBM 360/195
C
C        PURPOSE
C           THIS SUBROUTINE (BDRYHT) COMPUTES STATIC HEIGHTS ON DEEP
C           WATER BOUNDARIES ONLY, SEE DIAGRAM BELOW:
C
C        DATA SET USE
C           NONE
C
C        VARIABLES
C             AX AY = COMPS OF TOTAL STRM MOTION, ADVANCED IN 'STMVAL'
C             C1 C2 = INITIAL COMPS OF STORM, SET IN 'INTVAL'
C             IMXB1 = MAX I-SUBSCRIPT FOR HEIGHT POINTS
C            SEADTM = INITIAL HEIGHT OF THE SEA (NO STATIC HEIGHTS)
C         DELP(800) = STATIC HEIGHTS AT MILE INTERVALS FROM STORM CENTER
C        HB(  ,   ) = SURGE HEIGHTS
C        ITREE( , ) = A,   ON AT LEAST ONE BOUNDARY CORNER AS STATIC
C                     HEIGHT IS USED
C
C        GENERAL COMMENTS
C           THIS SUBROUTINE RESIDES IN OVERLAY 'CMPUTE'. IT IS CALLED
C           IN BY SUBROUTINE 'CMPUTE'.
C      THIS SUBROUTINE IS CALLED IN BY SUBROUTINE 'CONTTY'
C      WHICH RESIDES IN OVERLAY 'CMPUTE'
C
C   AWK MODIFYING 09/2010 FOR NEW LARGER GULF OF MEX BASIN
!      PARAMETER (M$=600,N$=600,L$=5000,NCT$=2500,LC$=L$)
      include 'parm.for'
C
      parameter (NBCPTS=12000)
      COMMON /FORCE/ WX(M_,N_),WY(M_,N_),PP(M_,N_),PPX(M_,N_),PPY(M_,N_)
!      COMMON /BCPTS/  NBCPT,ISH(2000),JSH(2000)
      COMMON /BCPTS/  TIDESH(NBCPTS),BOUN_NEST(NBCPTS),
     *                NBCPT,ISH(NBCPTS),JSH(NBCPTS)

      COMMON /DATUM/  SEADTM,DTMLAK
C
C       STATIC HEIGHT BOUNDARY SQUARES DETERMINED IN SUBROUTINE DEPSFC
C
      DO 100 N=1,NBCPT
      I=ISH(N)
      J=JSH(N)
      HB(I,J)=.25*(PP(I,J)+PP(I+1,J)+PP(I,J+1)+PP(I+1,J+1))
     1          +SEADTM+TIDESH(N)
C     HB(I,J)=MAX(0.,HB(I,J)-SEADTM)+SEADTM
  100 CONTINUE
C
!      write(*,*) 'NBCPT=',NBCPT
!      call tflush
      RETURN
      END


