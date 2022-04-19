        SUBROUTINE MYFDEBUFR (fn)

C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    fdebufr
C   PRGMMR: J. Ator          ORG: NP12       DATE: 2009-07-01
C   Adapted by: Ryan Schuster (ryan.schuster@noaa.gov)
c Updated by Huiqing Liu in March 2016
c-----------------------------------------------------------------------
C
C ABSTRACT: This subroutine reads every BUFR message from within the
C   input file that was specified on the command line.  Each such
C   message is decoded and the results are written as output to ofile.
C
C PROGRAM HISTORY LOG:
C 2009-07-01  J. Ator     Original author
C 2012-06-18  J. Ator     Added tblfil argument and options to decode
C                         files according to DX dictionary information 
C 2012-12-07  J. Ator     Added forcemt and lentd arguments
C 2014        R. Schuster Stripped out most original code to use this
C                         for BUFR water level obs only (no metadata,
C                         no tables)
C 2021-09-20  H.Liu       Convert to use WCOSS2 Bufr module                
C
C USAGE:    call fdebufr (fn)
C   INPUT ARGUMENT LIST:
C Ryan Added:
C     fn       - filename of input/output grid file
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  Portable to all platforms
C
C$$$

        USE DEFGRIDSTNS

        PARAMETER ( MXBF = 2500000 )
        PARAMETER ( MXBFD4 = MXBF/4 )
        PARAMETER ( MXDS3 = 500 )

        CHARACTER       opened
        CHARACTER*15    fn

        CHARACTER*8     cmgtag
        CHARACTER*6     cds3 ( MXDS3 )

        CHARACTER*1     bfmg ( MXBF )

        INTEGER         ibfmg ( MXBFD4 )

        EQUIVALENCE     ( bfmg (1), ibfmg (1) )

        CHARACTER(255) NULLFILE

C       Date array
!------------------------------------------------------------------------
!H.Liu
!5 days + 102 hrs = 13320 mins and divide by 6 mins step + 1 equals 2221
! lines in datelist files
!-------------------------------------------------------------------------
!        CHARACTER(12) DATES(2161)
!        CHARACTER(12) DATES(2221)

         INTEGER :: NDATES
         CHARACTER(12) ,  ALLOCATABLE :: DATES(:)
!
        CHARACTER(255) DATELIST

C       Get number of stations
        INTEGER NLINES
        CHARACTER(255) MASTER
        CHARACTER(13) FMTSTR

        CHARACTER(19) OBSOUT
        CHARACTER(5) NDBC
        CHARACTER(1) BOOL
        INTEGER HH
        CHARACTER(120) NOPE

C-----------------------------------------------------------------------
C Call filenames from environmental variables
      CALL GET_ENVIRONMENT_VARIABLE('FORT11',MASTER)
      CALL GET_ENVIRONMENT_VARIABLE('FORT12',DATELIST)
      CALL GET_ENVIRONMENT_VARIABLE('FORT13',NULLFILE)

C Get number of stations
      CALL NUMLINES(TRIM(MASTER),NLINES)
      CALL NUMLINES(TRIM(DATELIST),NDATES)

      ALLOCATE(DATES(NDATES))
!      ALLOCATE(GRID(2161,NLINES))
!      ALLOCATE(GRID(2221,NLINES))
      ALLOCATE(GRID(NDATES,NLINES))

      ALLOCATE(STNS(NLINES))

C     Open master file to grab station NWSLI's
      OPEN(11,FILE=TRIM(MASTER),ACTION="READ")
      HH = 0
      DO
        HH = HH + 1
        READ(11,*,END=20), NOPE,NOPE,NDBC,NOPE,NOPE,NOPE,
     &                    NOPE,NOPE,NOPE,BOOL,NOPE,NOPE,NOPE,
     &                    NOPE,NOPE

        STNS(HH) = NDBC
      END DO
 20   CLOSE(11)

C     Open list of timestamps for this forecast
      OPEN(12,FILE=TRIM(DATELIST),ACTION='READ')

C     Initialize the grid with 9999's
      DO II = 1,NDATES
        READ(12,'(A12)'), DATES(II)
        DO JJ = 1,NLINES
          GRID(II,JJ) = 9999
        ENDDO
      ENDDO
      CLOSE(12)

C     Note that in the below OPEN statement we just need to specify
C     a dummy placeholder file.

      lunit = 13
      OPEN ( UNIT = lunit, FILE = TRIM(NULLFILE) )

      CALL DATELEN ( 10 )
        
C     Initialize some other values.

      nmsg = 0
      nsubt = 0

      opened = 'N'

      DO WHILE ( .true. )

C         Get the next message from the input BUFR file.
          CALL CRBMG ( bfmg, MXBF, nbyt, ierr )

          IF ( ierr .ne. 0 )  THEN

              IF ( ierr .eq. -1 ) THEN             
C             Reached the end of this BUFR file

C                Write format string based on number of stns
                 WRITE (FMTSTR,'("(",I4,"(I6,1X))")') NLINES

                 OBSOUT = 'out/' // fn
                 OPEN(UNIT=51,FILE=OBSOUT)
!                 DO MM=1,2161
                 DO MM=1,NDATES
                    WRITE(51,FMTSTR) (GRID(MM,NN), NN=1,NLINES)
                 END DO
                 CLOSE(51)

              ELSE
                  WRITE  ( UNIT = 51, FMT = '( /, 2A, I4 )' )
     +              'Error while reading BUFR file; the return code ',
     +              'from CRBMG = ', IERR
              ENDIF

              DEALLOCATE(GRID)
              DEALLOCATE(STNS)

              RETURN
          ENDIF

          IF ( opened .eq. 'N' ) THEN
C             Decide how to process the file.
              IF (IDXMSG ( ibfmg ) .eq. 1) THEN
C                 The first message in the file is a DX dictionary
C                 message, so assume there's an embedded table at the
C                 front of the file and use this table to decode it.
                  CALL OPENBF ( lunit, 'INUL', lunit )
              ENDIF

              opened = 'Y'
          ENDIF

C         Pass the message to the decoder.
          CALL READERME ( ibfmg, lunit, cmgtag, imgdt, ierme )

          IF (IDXMSG ( ibfmg ) .ne. 1) THEN
              IF (ierme .ge. 0) THEN
C                 Decode and output the data from Section 4.
                  DO WHILE ( IREADSB ( lunit ) .eq. 0 )
                      CALL MYUFDUMP(lunit,DATES)
                  ENDDO
              ENDIF
          ENDIF
      ENDDO
       
      RETURN
      END

