      SUBROUTINE MYUFDUMP(LUNIT,DATES)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    UFDUMP
C   PRGMMR: WOOLLEN          ORG: NP20       DATE: 2002-05-14
C   Adapted by: Ryan Schuster (ryan.schuster@noaa.gov) 2014
C   Updated to use Version 11.3.0 by: H.Liu (huiqing.liu@noaa.gov) 2020
C
C ABSTRACT: THIS SUBROUTINE DUMPS A DETAILED PRINT LISTING OF THE
C   CONTENTS OF THE UNPACKED DATA SUBSET CURRENTLY RESIDING IN THE
C   INTERNAL ARRAYS ASSOCIATED WITH THE BUFR FILE IN LOGICAL UNIT LUNIT.
C   LUNIT MUST HAVE BEEN OPENED FOR INPUT VIA A PREVIOUS CALL TO BUFR
C   ARCHIVE LIBRARY SUBROUTINE OPENBF.  THE DATA SUBSET MUST HAVE BEEN
C   SUBSEQUENTLY READ INTO THE INTERNAL BUFR ARCHIVE LIBRARY ARRAYS VIA
C   A CALL TO BUFR ARCHIVE LIBRARY SUBROUTINE READMG OR READERME,
C   FOLLOWED BY A CALL TO BUFR ARCHIVE LIBRARY SUBROUTINE READSB (OR VIA
C   A SINGLE CALL TO BUFR ARCHIVE LIBRARY SUBROUTINE READNS!).  FOR A
C   PARTICULAR SUBSET, THE PRINT LISTING CONTAINS EACH MNEMONIC
C   ACCOMPANIED BY ITS CORRESPONDING DATA VALUE (INCLUDING THE ACTUAL
C   BITS THAT WERE SET FOR FLAG TABLE VALUES!) AS WELL AS OTHER USEFUL
C   IDENTIFICATION INFORMATION.  THIS SUBROUTINE IS SIMILAR TO BUFR
C   ARCHIVE LIBRARY SUBROUTINE UFBDMP EXCEPT THAT IT DOES NOT PRINT
C   POINTERS, COUNTERS AND OTHER MORE ESOTERIC INFORMATION DESCRIBING
C   THE INTERNAL SUBSET STRUCTURES.  EACH SUBROUTINE, UFBDMP AND UFDUMP,
C   IS USEFUL FOR DIFFERENT DIAGNOSTIC PURPOSES, BUT IN GENERAL UFDUMP
C   IS MORE USEFUL FOR JUST LOOKING AT THE DATA ELEMENTS.
C
C   This program, once it finds a TLLW (total water level in MLLW) at a
C   location/time point, hands that information off to griddata.f so it
C   can be placed into one of six parsed BUFR grids. combineObs.f reads
C   in the grids later and combines them into one.
C
C PROGRAM HISTORY LOG:
C 2002-05-14  J. WOOLLEN -- ORIGINAL AUTHOR
C 2003-11-04  J. WOOLLEN -- MODIFIED TO HANDLE PRINT OF CHARACTER
C                           VALUES GREATER THAN EIGHT BYTES
C 2003-11-04  S. BENDER  -- ADDED REMARKS/BUFRLIB ROUTINE
C                           INTERDEPENDENCIES
C 2003-11-04  D. KEYSER  -- MAXJL (MAXIMUM NUMBER OF JUMP/LINK ENTRIES)
C                           INCREASED FROM 15000 TO 16000 (WAS IN
C                           VERIFICATION VERSION); UNIFIED/PORTABLE FOR
C                           WRF; ADDED DOCUMENTATION (INCLUDING
C                           HISTORY); OUTPUTS MORE COMPLETE DIAGNOSTIC
C                           INFO WHEN ROUTINE TERMINATES ABNORMALLY
C 2004-08-18  J. ATOR    -- ADDED FUZZINESS TEST AND THRESHOLD FOR
C                           MISSING VALUE; ADDED INTERACTIVE AND
C                           SCROLLING CAPABILITY SIMILAR TO UFBDMP
C 2006-04-14  J. ATOR    -- ADD CALL TO UPFTBV FOR FLAG TABLES TO GET
C                           ACTUAL BITS THAT WERE SET TO GENERATE VALUE
C 2007-01-19  J. ATOR    -- USE FUNCTION IBFMS
C 2009-03-23  J. ATOR    -- ADD LEVEL MARKERS TO OUTPUT FOR SEQUENCES
C                           WHERE THE REPLICATION COUNT IS > 1; OUTPUT
C                           ALL OCCURRENCES OF LONG CHARACTER STRINGS
C 2012-02-24  J. ATOR    -- FIX MISSING CHECK FOR LONG CHARACTER STRINGS
C 2012-03-02  J. ATOR    -- LABEL REDEFINED REFERENCE VALUES
C 2014-12-10  J. ATOR    -- USE MODULES INSTEAD OF COMMON BLOCKS
C 2015-09-24  J. WOOLLEN -- PRINT LEVEL IDENTIFIERS FOR EVENT STACKS
C 2014        R. Schuster - Removed most of the code. Only kept enough
C                           to grab water levels, their location, and
C                           their time of observation; also kept missing
C                           data filter.
C 2018        H. Liu      - Added TERC except TLLW as the water level paramter
C 2019        H. Liu      - Added RSHM except TLLW as the water level paramter
C 2021-09-20  H.Liu       - Convert to use WCOSS2 Bufr module              
C
C USAGE:    CALL UFDUMP (LUNIT, DATES)
C   INPUT ARGUMENT LIST:
C     LUNIT    - INTEGER: FORTRAN LOGICAL UNIT NUMBER FOR BUFR FILE
C     DATES    - CHARACTER ARRAY: List of time stamps at which to look
C                for data
C
C
C    THIS ROUTINE CALLS:        BORT     FSTAG    ICBFMS   IBFMS
C                               IREADMT  ISIZE    NEMTAB   NUMTBD
C                               READLC   RJUST    SRCHTBF  STATUS
C                               STRSUC   UPFTBV
C    THIS ROUTINE IS CALLED BY: fdebufr.f
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  PORTABLE TO ALL PLATFORMS
C
C$$$

      USE MODA_USRINT
      USE MODA_MSGCWD
      USE MODA_TABABD
      USE MODA_TABLES
      USE MODA_NRV203

      INCLUDE 'bufrlib.prm'

      COMMON /TABLEF/ CDMF

      CHARACTER*120 CFMEANG
      CHARACTER*80 FMT
      CHARACTER*64 DESC
      CHARACTER*24 UNIT
      CHARACTER*120 LCHR2
      CHARACTER*20  LCHR,PMISS
      CHARACTER*15 NEMO3
      CHARACTER*10 NEMO,NEMO2,TAGRFE
      CHARACTER*8  NEMOD
      CHARACTER*6  NUMB
      CHARACTER*7  FMTF
      CHARACTER*8  CVAL
      CHARACTER*3  TYPE
      CHARACTER*1  CDMF,TAB,YOU
      EQUIVALENCE  (RVAL,CVAL)
      REAL*8       RVAL
      LOGICAL      TRACK,FOUND,RDRV

      PARAMETER (MXCFDP=5)
      INTEGER   ICFDP(MXCFDP)

      PARAMETER (MXFV=31)
      INTEGER	IFV(MXFV)

      PARAMETER (MXSEQ=10)
      INTEGER   IDXREP(MXSEQ)
      INTEGER   NUMREP(MXSEQ)
      CHARACTER*10 SEQNAM(MXSEQ)

      PARAMETER (MXLS=10)
      CHARACTER*10 LSNEMO(MXLS)
      INTEGER   LSCT(MXLS)

      DATA PMISS /'             MISSING'/
      DATA YOU /'Y'/

C Ryan added:
      CHARACTER*4 YR
      CHARACTER*2 MO,DA,HR,MI
      CHARACTER*12 DATE
      CHARACTER*5 ST
      REAL*8 TL

      REAL*8 DIV
      PARAMETER (DIV=1000.0)
C-------------------------------

C----------------------------------------------------------------------
C----------------------------------------------------------------------

C Ryan Added:
      YR = '9999'
      MO = '99'
      DA = '99'
      HR = '99'
      MI = '99'
      TL = 9999
C-----------------

      NSEQ = 0
      NLS = 0
      LCFMEANG = LEN(CFMEANG)

C  CHECK THE FILE STATUS AND I-NODE
C  --------------------------------

      CALL STATUS(LUNIT,LUN,IL,IM)
      IF(IL.EQ.0) GOTO 900
      IF(IL.GT.0) GOTO 901
      IF(IM.EQ.0) GOTO 902
      IF(INODE(LUN).NE.INV(1,LUN)) GOTO 903

C  DUMP THE CONTENTS OF MODULE USRINT FOR UNIT LUNIT
C  -------------------------------------------------

C     If code/flag table details are being printed, and if this is the
C     first subset of a new message, then make sure the appropriate
C     master tables have been read in to memory for this message.

      IF(CDMF.EQ.'Y' .AND. NSUB(LUN).EQ.1) ITMP = IREADMT(LUN)

      DO NV=1,NVAL(LUN)
C  Ryan sez:
C  NV is where we are in the subset, LUN is the logical unit
C  number of the file we're looking at. INV is a list of
C  mnemonic numbers as they appear in the subset?
      NODE = INV (NV,LUN)
      NEMO = TAG (NODE)
      ITYP = ITP (NODE)
      TYPE = TYP (NODE)

      IF(ITYP.GE.1.AND.ITYP.LE.3) THEN
         CALL NEMTAB(LUN,NEMO,IDN,TAB,N)
         NUMB = TABB(N,LUN)(1:6)
         DESC = TABB(N,LUN)(16:70)
         UNIT = TABB(N,LUN)(71:94)
         RVAL = VAL(NV,LUN)
      ENDIF
      
      IF(ITYP.EQ.2) THEN

C        Other numeric value

C  Ryan sez:
C  This is where the value gets printed. First, we check to see
C  if it's missing...

C        Now print the value

         IF(IBFMS(RVAL).NE.0) THEN

C           The value is "missing".
C Rayn Added
             CALL STRSUC(NEMO,NEMO2,LNM2)
             IF((NEMO2(1:LNM2).EQ.'YEAR').AND.(YR.EQ.'9999')) THEN
                YR = '9999'
             ELSEIF((NEMO2(1:LNM2).EQ.'MNTH').AND.(MO.EQ.'99')) THEN
                MO = '99'
             ELSEIF((NEMO2(1:LNM2).EQ.'DAYS').AND.(DA.EQ.'99')) THEN
                DA = '99'
             ELSEIF((NEMO2(1:LNM2).EQ.'HOUR').AND.(HR.EQ.'99')) THEN
                HR = '99'
             ELSEIF((NEMO2(1:LNM2).EQ.'MINU').AND.(MI.EQ.'99')) THEN
                MI = '99'
             ELSEIF(NEMO2(1:LNM2).EQ.'TLLW') THEN
                DATE = YR // MO // DA // HR // MI
                TL = 9999
             ENDIF
C-------------------------------------------------------- 
         ELSE
C  Ryan sez:
C  This is where the data value is written if it's not missing...
C            WRITE(LUOUT,FMT) NUMB,NEMO,RVAL,UNIT,DESC
C H.Liu 2019:
C TERC and RSHM are added
C
             CALL STRSUC(NEMO,NEMO2,LNM2)
             IF(NEMO2(1:LNM2).EQ.'YEAR') THEN
                WRITE(YR,'(I4)') INT(RVAL)
             ELSEIF(NEMO2(1:LNM2).EQ.'MNTH') THEN
                IF (INT(RVAL).LT.10) THEN
                   WRITE(MO,'(I1)') INT(RVAL)
                   MO = '0' // MO
                ELSE
                   WRITE(MO,'(I2)') INT(RVAL)
                ENDIF
             ELSEIF(NEMO2(1:LNM2).EQ.'DAYS') THEN
                IF (INT(RVAL).LT.10) THEN
                   WRITE(DA,'(I1)') INT(RVAL)
                   DA = '0' // DA
                ELSE
                   WRITE(DA,'(I2)') INT(RVAL)
                ENDIF
             ELSEIF(NEMO2(1:LNM2).EQ.'HOUR') THEN
                IF (INT(RVAL).LT.10) THEN
                   WRITE(HR,'(I1)') INT(RVAL)
                   HR = '0' // HR
                ELSE
                   WRITE(HR,'(I2)') INT(RVAL)
                ENDIF
             ELSEIF(NEMO2(1:LNM2).EQ.'MINU') THEN
                IF (INT(RVAL).LT.10) THEN
                   WRITE(MI,'(I1)') INT(RVAL)
                   MI = '0' // MI
                ELSE
                   WRITE(MI,'(I2)') INT(RVAL)
                ENDIF
             ELSEIF(NEMO2(1:LNM2).EQ.'TLLW'.OR.
     &              NEMO2(1:LNM2).EQ.'TERC'.OR.
     &              NEMO2(1:LNM2).EQ.'RSHM') THEN
                TL = RVAL
                DATE = YR // MO // DA // HR // MI
             ENDIF
         ENDIF

C-------------------------------------------------------- 
      ELSEIF(ITYP.EQ.3) THEN

C        Character (CCITT IA5) value

         NCHR = IBT(NODE)/8

         IF(IBFMS(RVAL).NE.0) THEN
            LCHR = PMISS
         ELSE IF(NCHR.LE.8) THEN
            LCHR = CVAL
         ELSE

C           Track the number of occurrences of this long character string, so
C           that we can properly output each one.

            II = 1
            FOUND = .FALSE.
            DO WHILE((II.LE.NLS).AND.(.NOT.FOUND))
               IF(NEMO.EQ.LSNEMO(II)) THEN
                 FOUND = .TRUE.
               ELSE
                 II = II + 1
               ENDIF
            ENDDO

            IF(.NOT.FOUND) THEN
               NLS = NLS+1
               IF(NLS.GT.MXLS) GOTO 905
               LSNEMO(NLS) = NEMO
               LSCT(NLS) = 1
               NEMO3 = NEMO
            ELSE
               CALL STRSUC(NEMO,NEMO3,LNM3)
               LSCT(II) = LSCT(II) + 1
            ENDIF
C Ryan sez:
C This is what catches things like the station name...
C so instead of being RVAL it's CVAL

            CALL READLC(LUNIT,LCHR2,NEMO3)
            IF (ICBFMS(LCHR2,NCHR).NE.0) THEN
               LCHR = PMISS
            ELSE
               LCHR = LCHR2(1:20)
            ENDIF
         ENDIF

         IF ( NCHR.LE.20 .OR. LCHR.EQ.PMISS ) THEN
            IRET = RJUST(LCHR)
C----------------------------------------------------
             IF(NEMO.EQ.'RPID') THEN
                ST = LCHR(16:20)
             ENDIF
C----------------------------------------------------

         ELSE

C----------------------------------------------------
             IF(NEMO.EQ.'RPID') THEN
                ST = LCHR2(NCHR-5:NCHR)
             ENDIF
C----------------------------------------------------

         ENDIF
      ENDIF

      ENDDO

C----------------------------------------------------
C     Convert TL from meters to feet
      IF ((TL.LT.9999.001).AND.(TL.GT.9998.999)) THEN
C Missing detected...
      ELSE
C Convert to feet
        TL = TL * 3.28
        CALL GRIDDATA(DATE,ST,TL,DATES)
      ENDIF
C----------------------------------------------------

C  EXITS
C  -----

100   RETURN
900   CALL BORT('BUFRLIB: UFDUMP - INPUT BUFR FILE IS CLOSED, IT '//
     . 'MUST BE OPEN FOR INPUT')
901   CALL BORT('BUFRLIB: UFDUMP - INPUT BUFR FILE IS OPEN FOR '//
     . 'OUTPUT, IT MUST BE OPEN FOR INPUT')
902   CALL BORT('BUFRLIB: UFDUMP - A MESSAGE MUST BE OPEN IN INPUT '//
     . 'BUFR FILE, NONE ARE')
903   CALL BORT('BUFRLIB: UFDUMP - LOCATION OF INTERNAL TABLE FOR '//
     . 'INPUT BUFR FILE DOES NOT AGREE WITH EXPECTED LOCATION IN '//
     . 'INTERNAL SUBSET ARRAY')
904   CALL BORT('BUFRLIB: UFDUMP - MXSEQ OVERFLOW')
905   CALL BORT('BUFRLIB: UFDUMP - MXLS OVERFLOW')
      END
