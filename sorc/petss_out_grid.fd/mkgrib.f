! Huiqing Liu /MDL Jan. 2016 
!
! Purpose:
!
! Created grib2 products
!  
!

      SUBROUTINE MKGRIB(LCGRIB,RYEAR,RMONTH,RDAY,RHOUR,RMIN,RSEC,
     1                  AREA,IFCSTHR,INCRH,STAP,DTM)
!
      USE Mask_Array
!
!
      REAL            FLD(NXNYMAX)
      CHARACTER*3     AREA,DTM
      INTEGER         LSEC0(2)
      INTEGER         LSEC1(13)
      INTEGER         IERR
      INTEGER         LCGRIB
      INTEGER         RYEAR,RMONTH,RDAY,RHOUR,RMIN,RSEC
      CHARACTER*1     CSEC2(1)
      INTEGER         LCSEC2
      INTEGER         IGDS(5)
      INTEGER         IGDSTMPL(NGDSTMPL)
      INTEGER         IDEFLIST(1)
      INTEGER         IDEFNUM
      REAL            COORDLIST(1)
      INTEGER         NUMCOORD
      INTEGER         IPDSNUM
      INTEGER         IPDSTMPL(IPDSTMPLEN)
      INTEGER         IFCSTHR
      INTEGER         IDRSNUM
      INTEGER         IDRSTMPL(IDRSTMPLEN)
      INTEGER         NGRDPTS
      INTEGER         IBMAP
      LOGICAL*1       BMAP(1)
      INTEGER         DSF
      INTEGER         ITEMP
      INTEGER         PERC_N
!------------------------------------------------------
! Get the time increment (hr) "INCRH"
!               type          "STAP"(2--incremental 
!                                    1--accumulative)
!scale factor for upper limit "SCALF"
!Type of fixed surface: FIXSURF 1--Ground or Water Surface (DTM==DAT)
!                             103--Specified Height Level Above Ground (DTM==AGL)
!------------------------------------------------------
      INTEGER         INCRH,STAP,SCALF,FIXSURF
!------------------------------------------------------
! Get the right end of forecast time 102 forecast hrs
!------------------------------------------------------
      INTEGER         YYE,MME,DDE,HHE

      HHE = RHOUR + IFCSTHR
      DDE = RDAY
      MME = RMONTH
      YYE = RYEAR

      IF (HHE >= 24 .AND. HHE < 48) THEN 
         DDE = DDE + 1
         HHE = HHE - 24
      ELSEIF (HHE >= 48 .AND. HHE < 72) THEN
         DDE = DDE + 2
         HHE = HHE - 48
      ELSEIF (HHE >= 72 .AND. HHE < 96) THEN
         DDE = DDE + 3
         HHE = HHE - 72
      ELSEIF (HHE >= 96 .AND. HHE < 120) THEN
         DDE = DDE + 4
         HHE = HHE - 96
      ELSEIF (HHE >= 120 .AND. HHE < 144) THEN
         DDE = DDE + 5
         HHE = HHE - 120
      ENDIF

      IF (RMONTH == 4 .OR. RMONTH == 6. OR. RMONTH == 9 .OR. 
     1    RMONTH == 11) THEN
         IF (DDE >= 31) THEN
             DDE = DDE - 30
             MME = MME + 1
         ENDIF
      ELSEIF (RMONTH == 2) THEN
         IF (MOD(YYE,4) == 0 .AND. MOD (YYE,100) /= 0 .OR. 
     1       MOD(YYE,400) == 0) THEN ! IF LEAP YEAR
            IF (DDE >= 30) THEN
               DDE = DDE - 29
               MME = MME + 1
            ENDIF
         ELSE                        ! IF NOT LEAP YEAR
            IF (DDE >= 29) THEN
               DDE = DDE - 28
               MME = MME + 1
            ENDIF 
         ENDIF
      ELSE
         IF (DDE >= 32) THEN
             DDE = DDE - 31
             MME = MME + 1
         ENDIF 
      ENDIF

      IF (MME >= 13) THEN
         MME = MME - 12
         YYE = YYE + 1
      ENDIF

      IF (DTM=='DAT') THEN
         FIXSURF = 1
      ELSE
         FIXSURF = 103
      ENDIF
!-----------------------------------------------------------
C 10 = OCEANOGRAPHIC PRODUCT, 2 = EDITION NUMBER (GRIB2)
      LSEC0(1) = 10
      LSEC0(2) = 2
C 7 = NCEP, 14 = MDL, 4 VERSION, 1 = VERSION OF THE LOCAL TABLES.
      LSEC1(1) = 7
      LSEC1(2) = 14
      LSEC1(3) = 3
      LSEC1(4) = 1
C 1 = START OF FORECAST, RYEAR, RMONTH, RDAY, RHOUR, RMIN, RSEC
      LSEC1(5) = 1
      LSEC1(6) = RYEAR
      LSEC1(7) = RMONTH
      LSEC1(8) = RDAY
      LSEC1(9) = RHOUR
      LSEC1(10) = RMIN
      LSEC1(11) = RSEC
C 1 = OPERATIONAL TEST PRODUCT (0 WOULD BE OPERATIONAL)
      LSEC1(12) = 1
C 1 = FORECAST PRODUCTS
      LSEC1(13) = 1
      CALL GRIBCREATE(CGRIB,NGRIBM,LSEC0,LSEC1,IERR)
C CHECK THE RESULTS OF IERR.

      LCSEC2 = 0
      CALL ADDLOCAL(CGRIB,NGRIBM,CSEC2,LCSEC2,IERR)
C CHECK THE RESULTS OF IERR.

C 0 = USING TEMPLATES, GRID SPECIFIED IN 3.1
      IGDS(1) = 0
      IGDS(2) = NX * NY
C 0 = MEANS NO IDEFLIST, 0 MEANS NO APPENDED LIST
      IGDS(3) = 0
      IGDS(4) = 0
C 30 = LAMBERT, 20 = POLAR STEREOGRAPHIC
      IF(AREA=='con') THEN
        IGDS(5) = 30
      ELSE IF(AREA=='ala') THEN
        IGDS(5) = 20
      END IF
      IGDSTMPL(1) = 1
      IGDSTMPL(2) = 0
      IGDSTMPL(3) = 6371200
      IGDSTMPL(4) = 0
      IGDSTMPL(5) = 0
      IGDSTMPL(6) = 0
      IGDSTMPL(7) = 0
      IGDSTMPL(8) = NX
      IGDSTMPL(9) = NY
      IF(AREA=='con') THEN
        IGDSTMPL(10) = 20191999
        IGDSTMPL(11) = 238445999
      ELSE IF(AREA=='ala') THEN
        IGDSTMPL(10) = 40530101
        IGDSTMPL(11) = 181429000
      END IF
C RESOLUTION FLAG IS 0.
      IGDSTMPL(12) = 0
      IF(AREA=='con') THEN
        IGDSTMPL(13) = 25000000
        IGDSTMPL(14) = 265000000
        IGDSTMPL(15) = Res_con !2.5km
        IGDSTMPL(16) = Res_con !2.5km
        IGDSTMPL(17) = 0
        IGDSTMPL(18) = 64
        IGDSTMPL(19) = 25000000
        IGDSTMPL(20) = 25000000
        IGDSTMPL(21) = -90000000
        IGDSTMPL(22) = 0
      ELSE IF(AREA=='ala') THEN
        IGDSTMPL(13) = 60000000
        IGDSTMPL(14) = 210000000
        IGDSTMPL(15) = Res_ala !3.0km
        IGDSTMPL(16) = Res_ala !3.0km
        IGDSTMPL(17) = 0
        IGDSTMPL(18) = 64
      END IF
      IDEFNUM = 0
      CALL ADDGRID(CGRIB,NGRIBM,IGDS,IGDSTMPL,NGDSTMPL,IDEFLIST,
     1             IDEFNUM,IERR)
C CHECK THE RESULTS OF IERR.

C 0 = FORECAST AT A HORIZONTAL LEVEL AT A POINT IN TIME
      IF (PROBP=='10p'.or.PROBP=='20p'.or.PROBP=='30p'.or.
     $    PROBP=='40p'.or.PROBP=='50p'.or.PROBP=='90p') THEN
         IPDSNUM = 10
      ELSE IF (PROBP=='max'.or.PROBP=='mean'.or.
     $    PROBP=='min') THEN
          IPDSNUM = 8
      ELSE
          IPDSNUM = 9
      ENDIF
      IPDSTMPL(1) = 3
! PRODUCT NAME 193--ETSRG 250--ETCWL
      IF (PROBP=='10p') THEN
         IPDSTMPL(2) = 192
      ELSEIF (PROBP=='20p') THEN
         IPDSTMPL(2) = 242
      ELSEIF (PROBP=='30p') THEN
         IPDSTMPL(2) = 243
      ELSEIF (PROBP=='40p') THEN
         IPDSTMPL(2) = 244
      ELSEIF (PROBP=='50p') THEN
         IPDSTMPL(2) = 245
      ELSEIF (PROBP=='90p') THEN
         IPDSTMPL(2) = 249
      ELSE
         IPDSTMPL(2) = 250
      ENDIF
c 5 = Probability FORECAST
      IPDSTMPL(3) = 5
      IPDSTMPL(4) = 0
! PETSS process ID
      IPDSTMPL(5) = 18
!      IPDSTMPL(5) = 12
      IPDSTMPL(6) = 65535
      IPDSTMPL(7) = 255
      IPDSTMPL(8) = 1
      IPDSTMPL(9) = IFCSTHR
c 1 = GROUND OR WATER SURFACE; 103 Above Ground Level
      IPDSTMPL(10) = FIXSURF
!      IPDSTMPL(10) = 1
      IPDSTMPL(11) = 0
      IPDSTMPL(12) = 0
C -1 is all 1's if we are dealing with signed integers.
C 13, and 14 only need 1 byte of all 1's (missing), so could use 255
      IPDSTMPL(13) = -1
      IPDSTMPL(14) = -1
      IPDSTMPL(15) = -1
      IF (PROBP=='10p'.or.PROBP=='20p'.or.PROBP=='30p'.or.
     $    PROBP=='40p'.or.PROBP=='50p'.or.PROBP=='90p') THEN
         READ(PROBP(1:2),'(I2)') PERC_N
         IPDSTMPL(16) = 100-PERC_N
         IPDSTMPL(17) = YYE
         IPDSTMPL(18) = MME
         IPDSTMPL(19) = DDE
         IPDSTMPL(20) = HHE
         IPDSTMPL(21) = 0 ! Minutes
         IPDSTMPL(22) = 0 ! Seconds
         IPDSTMPL(23) = 1 ! Number of time range specifications
         IPDSTMPL(24) = 0 ! Total number of data values missing in the statistical process
         IPDSTMPL(25) = 2 !maximum
         IPDSTMPL(26) = STAP !Successive times; same start time, forecast time incremented
         IPDSTMPL(27) = 1
         IPDSTMPL(28) = INCRH !Length of the time range over which statistical processing is d
         IPDSTMPL(29) = 1
         IPDSTMPL(30) = 0
         IPDSTMPL(31) = 0
         IPDSTMPL(32) = 0
      ELSE IF (PROBP=='max'.or.PROBP=='mean'.or.
     $    PROBP=='min') THEN
         IPDSTMPL(16) = YYE
         IPDSTMPL(17) = MME
         IPDSTMPL(18) = DDE
         IPDSTMPL(19) = HHE
         IPDSTMPL(20) = 0 ! Minutes
         IPDSTMPL(21) = 0 ! Seconds
         IPDSTMPL(22) = 1 ! Number of time range specifications 
         IPDSTMPL(23) = 0 ! Total number of data values missing in the statistical process
         IF (PROBP=='max') THEN
            IPDSTMPL(24) = 2
         ELSEIF (PROBP=='min') THEN
            IPDSTMPL(24) = 3
         ELSE
            IPDSTMPL(24) = 0
         ENDIF
         IPDSTMPL(25) = STAP !Successive times; same start time, forecast time incremented
         IPDSTMPL(26) = 1 ! Unit is hour
         IPDSTMPL(27) = INCRH ! Length of the time range over which statistical processing is do
         IPDSTMPL(28) = 1
         IPDSTMPL(29) = 0
         IPDSTMPL(30) = 0
         IPDSTMPL(31) = 0
      ELSE
         IPDSTMPL(16) = 0
         IPDSTMPL(17) = 1
         IPDSTMPL(18) = 1
         IPDSTMPL(19) = 0
         IPDSTMPL(20) = 0
         SCALF = 5
         IPDSTMPL(21) = SCALF
         IF (PROBP=='0ft.chance') THEN
            IPDSTMPL(22) = 0*0.3048*10**SCALF
         ELSEIF (PROBP=='1ft.chance') THEN
            IPDSTMPL(22) = 1*0.3048*10**SCALF
         ELSEIF (PROBP=='2ft.chance') THEN
            IPDSTMPL(22) = 2*0.3048*10**SCALF
         ELSEIF (PROBP=='3ft.chance') THEN
            IPDSTMPL(22) = 3*0.3048*10**SCALF 
         ELSEIF (PROBP=='4ft.chance') THEN
            IPDSTMPL(22) = 4*0.3048*10**SCALF
         ELSEIF (PROBP=='5ft.chance') THEN
            IPDSTMPL(22) = 5*0.3048*10**SCALF
         ELSEIF (PROBP=='6ft.chance') THEN
            IPDSTMPL(22) = 6*0.3048*10**SCALF 
         ELSEIF (PROBP=='7ft.chance') THEN
            IPDSTMPL(22) = 7*0.3048*10**SCALF
         ELSEIF (PROBP=='8ft.chance') THEN
            IPDSTMPL(22) = 8*0.3048*10**SCALF
         ELSEIF (PROBP=='9ft.chance') THEN
            IPDSTMPL(22) = 9*0.3048*10**SCALF
         ELSEIF (PROBP=='10ft.chance') THEN
            IPDSTMPL(22) = 10*0.3048*10**SCALF
         ELSEIF (PROBP=='13ft.chance') THEN
            IPDSTMPL(22) = 13*0.3048*10**SCALF
         ELSE
            IPDSTMPL(22) = 16*0.3048*10**SCALF
         ENDIF
         IPDSTMPL(23) = YYE
         IPDSTMPL(24) = MME
         IPDSTMPL(25) = DDE 
         IPDSTMPL(26) = HHE 
         IPDSTMPL(27) = 0 ! Mins
         IPDSTMPL(28) = 0 ! Seconds
         IPDSTMPL(29) = 1 ! Number of time range specifications
         IPDSTMPL(30) = 0 ! Total number of data values missing in the statistical process
         IPDSTMPL(31) = 2 ! Statistical process used to calculate -- "maximum"
         IPDSTMPL(32) = STAP !Successive times; same start time, forecast time incremented
         IPDSTMPL(33) = 1 ! 1 -- Unit is Hour
         IPDSTMPL(34) = INCRH !Length of the time range over which statistical processing
         IPDSTMPL(35) = 1
         IPDSTMPL(36) = 0
         IPDSTMPL(37) = 0
         IPDSTMPL(38) = 0

      ENDIF

      NUMCOORD = 0
      NGRDPTS = NX * NY
      IDRSNUM = 2
C REFERENCE VALUE IS SET TO 9999 FOR
      IDRSTMPL(1) = 9999
      IDRSTMPL(2) = 0
C 5 = DECIMAL SCALE FACTOR
!      DSF = 5
      DSF = 3
      IDRSTMPL(3) = DSF
      IDRSTMPL(4) = 9999
C 0 = FLOATING POINT (ORIGINAL DATA WAS A FLOATING POINT NUMBER)
      IDRSTMPL(5) = 0
      IDRSTMPL(6) = 9999
C 1 = MISSING VALUE MANAGEMENT (PRIMARY ONLY)
      IDRSTMPL(7) = 1
      call mkieee(9999.,IDRSTMPL(8),1)
      call mkieee(9999.,IDRSTMPL(9),1)
      IDRSTMPL(10) = 9999
      IDRSTMPL(11) = 9999
      IDRSTMPL(12) = 9999
      IDRSTMPL(13) = 9999
      IDRSTMPL(14) = 9999
      IDRSTMPL(15) = 9999
      IDRSTMPL(16) = 9999

C LOOP THROUGH THE DATA.
C Y (ft) * (12inch/ft) * (2.54cm/inch) * (m/100cm) = X (m)
C Y (ft) * M_FT = X (m)
C M_FT = 0.3048

      IF (PROBP=='10p'.or.PROBP=='max'.or.PROBP=='mean'.or.
     $    PROBP=='min'.or.PROBP=='20p'.or.PROBP=='30p'.or.
     $    PROBP=='40p'.or.PROBP=='50p'.or.PROBP=='90p') THEN 
! Storm tide height products converted from ft to meter
        DO 150 J=1,NY
        DO 140 I=1,NX
          IF (ABS(HA(I,J)) < 60) THEN
            ITEMP = HA(I,J) * 0.3048 * 10**DSF + 0.5
            FLD(I + (J - 1) * NX) = ITEMP / (10**DSF + 0.0)
          ELSE
            FLD(I + (J - 1) * NX) = 9999
          ENDIF
 140    CONTINUE
 150    CONTINUE
! Probablity products keep original (%)
      ELSE
        DO 151 J=1,NY 
        DO 141 I=1,NX 
          IF (HA(I,J)<=100.and.HA(I,J)>=0) THEN
            FLD(I + (J - 1) * NX) = HA(I,J)
          ELSE
            FLD(I + (J - 1) * NX) = 9999 
          ENDIF
 141    CONTINUE 
 151    CONTINUE

      ENDIF
C NO BIT MAP APPLIES FOR THE DATA.
      IBMAP = 255
!      write(*,*)maxval(FLD)
      CALL ADDFIELD(CGRIB,NGRIBM,IPDSNUM,IPDSTMPL,IPDSTMPLEN,
     1              COORDLIST,NUMCOORD,IDRSNUM,IDRSTMPL,
     1              IDRSTMPLEN,FLD,NGRDPTS,IBMAP,BMAP,IERR)
C CHECK THE RESULTS OF IERR.

C RENAME LCGRIB TO LENGRIB
C RENAME NGRIBM TO LCGRIB
      CALL GRIBEND(CGRIB,NGRIBM,LCGRIB,IERR)
C CHECK THE RESULTS OF IERR.
      RETURN
      END

